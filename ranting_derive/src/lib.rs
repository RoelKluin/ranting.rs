// (c) RoelKluin 2022 GPL v3
#![feature(iter_intersperse)]

mod language;
mod ranting_impl;
mod str_lit;

use language::english_shared as lang;

use darling::{FromDeriveInput, ToTokens};
use itertools::join;
use lazy_static::lazy_static;
use proc_macro::{self, TokenStream as TokenStream1};
use proc_macro2::{Punct, Spacing, Span, TokenStream};
use ranting_impl::*;
use regex::{Captures, Regex};
use std::iter;
use str_lit::*;
use syn::{self, parse_quote, punctuated::Punctuated, Error, Expr, Token};

// TODO: replace Span::mixed_site() with more precise location.

#[proc_macro]
pub fn ack(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as Say);
    let tokens: TokenStream = parse_quote!(return Ok(#input));
    tokens.into()
}

#[proc_macro]
pub fn nay(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as Say);
    let tokens: TokenStream = parse_quote!(return Err(#input));
    tokens.into()
}

#[proc_macro]
pub fn say(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as Say);
    let tokens: TokenStream = parse_quote!(#input);
    tokens.into()
}

/// after parsing, Say basicly contains the format!() litteral string and its (optional) parameters
struct Say {
    lit_str: String,
    params: Vec<Expr>,
}

fn ref_expr_ranting_trait(ref_expr: TokenStream) -> TokenStream {
    parse_quote! {
        impl Ranting for #ref_expr {
            fn name(&self, uc: bool) -> String {
                (**self).name(uc)
            }
            fn subjective(&self) -> &str {
                (**self).subjective()
            }
            fn is_plural(&self) -> bool {
                (**self).is_plural()
            }
            fn inflect(&self, as_plural: bool, uc: bool) -> String {
                (**self).inflect(as_plural, uc)
            }
            fn skip_article(&self) -> bool {
                (**self).skip_article()
            }
        }
    }
}

#[proc_macro]
pub fn boxed_ranting_trait(input: TokenStream1) -> TokenStream1 {
    let trait_name = syn::parse_macro_input!(input as Expr);
    ref_expr_ranting_trait(parse_quote!(&'_ dyn #trait_name)).into()
}

#[proc_macro]
pub fn ref_ranting_trait(input: TokenStream1) -> TokenStream1 {
    let trait_name = syn::parse_macro_input!(input as Expr);
    ref_expr_ranting_trait(parse_quote!(Box<dyn #trait_name>)).into()
}

/// Implies `#[derive(Ranting)]` and includes `name` and `subject` in structs.
/// For an enum `"it"` and the variant's name are assumed.
#[proc_macro_attribute]
pub fn derive_ranting(_args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let mut ast = syn::parse_macro_input!(input as syn::DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(_) => {
            let tokens: TokenStream = parse_quote! {
                #[derive(ranting_derive::Ranting)]
                #ast
            };
            tokens.into()
        }
        syn::Data::Enum(_) => {
            let tokens: TokenStream = parse_quote! {
                #[derive(ranting::rant_strum_macros::Display, ranting_derive::Ranting)]
                #ast
            };
            tokens.into()
        }
        _ => panic!("`add_field` has to be used with structs or enums"),
    }
}

/// Above macros inflect Ranting elements within a placeholder. Structs require a `name` and `subject` String.
#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn inner_derive_ranting(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input);
    let mut is_enum = false;
    let options = RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    if let syn::Data::Enum(_) = &input.data {
        is_enum = true;
    }
    ranting_q(options, is_enum, &input.ident).into()
}

/// Split placeholders in multiple and extend params accordingly.
// currently via a regex; convenient, but suboptimal because pattern error indication is lacking.
impl syn::parse::Parse for Say {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        lazy_static! {
            static ref PH: Regex = Regex::new(lang::PH_START).unwrap();
            static ref PHE: Regex = Regex::new(lang::PH_EXT).unwrap();
        }
        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing format string"));
        }
        let lit = input.parse::<StrLit>()?;

        let params_in = if input.is_empty() {
            vec![]
        } else {
            input.parse::<Token![,]>()?;

            input
                .parse_terminated::<_, Token![,]>(Expr::parse)?
                .into_iter()
                .collect()
        };
        let src = lit.to_slice();
        let text = src.text();
        #[cfg(feature = "debug")]
        eprintln!("{}", text);

        let mut params = vec![];

        let mut err = None;

        let lit_str = PH
            .replace_all(text, |caps: &Captures| {
                let pre = caps.name("pre");
                let fmt = caps.name("fmt").map_or("", |s| s.as_str());
                if let Some(plain) = caps.name("plain") {
                    match get_opt_num_ph_expr(plain.as_str(), &params_in) {
                        Ok(expr) => {
                            let len = params.len().to_string();
                            params.push(expr);
                            pre.map_or("", |s| s.as_str()).to_string()
                                + "{"
                                + len.as_str()
                                + fmt
                                + "}"
                        }
                        Err(s) => {
                            err = Some((plain.start(), plain.end(), s));
                            String::new()
                        }
                    }
                } else {
                    let ranting = caps.name("ranting").unwrap();
                    if !PHE.is_match(ranting.as_str()) {
                        err = Some((
                            ranting.start(),
                            ranting.end(),
                            "Error in placeholder".to_string(),
                        ));
                        return String::new();
                    }
                    let at_sentence_start = pre
                        .filter(|m| m.start() == 0 || m.as_str().starts_with(['.', '?', '!']))
                        .is_some();
                    pre.map_or("", |s| s.as_str()).to_string()
                        + &PHE.replace(ranting.as_str(), |caps: &Captures| {
                            match handle_param(
                                caps,
                                &params_in,
                                &mut params,
                                at_sentence_start,
                                fmt,
                            ) {
                                Ok(s) => s,
                                Err((start, end, msg)) => {
                                    let offs = ranting.start();
                                    err = Some((start + offs, end + offs, msg));
                                    String::new()
                                }
                            }
                        })
                }
            })
            .to_string();
        match err {
            Some((start, end, msg)) => Err(src.slice(start..end).error(msg.as_str())),
            None => Ok(Say { lit_str, params }),
        }
    }
}

/// Construct the format!() macro call. Print result with `--features debug`
impl ToTokens for Say {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lit = self.lit_str.as_str();
        let macro_tokens = TokenStream::from_iter(
            iter::once(parse_quote!(#lit))
                .chain(self.params.iter().map(|e| e.into_token_stream()))
                .intersperse(Punct::new(',', Spacing::Alone).into_token_stream()),
        );
        *tokens = parse_quote!(format!(#macro_tokens));
        #[cfg(feature = "debug")]
        eprintln!("{}", tokens.to_string());
    }
}

/// construct a path expression, e.g. to an identifier or a call in a visible mod.
fn path_from<S: AsRef<str>>(path: S) -> Expr {
    Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: Punctuated::from_iter(path.as_ref().split("::").map(|s| syn::PathSegment {
                // XXX: Span::mixed_site() here gives errors.
                ident: syn::Ident::new(s, Span::call_site()),
                arguments: syn::PathArguments::None,
            })),
        },
    })
}

/// The expression for a match. if numeric, retreive the expression from the positionals
fn get_opt_num_ph_expr(p: &str, given: &[Expr]) -> Result<Expr, String> {
    match p.parse::<usize>() {
        Err(_) => Ok(path_from(p)),
        Ok(u) => match given.get(u) {
            Some(e) => Ok(e.clone()),
            None => Err(format!("positional {u} is out of bounds")),
        },
    }
}

fn split_at_find_start(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.rfind(fun).map(|u| s.split_at(u + 1))
}

// Placeholder parts are examined, added are replacements known at compile time,
// or placeholders are split in many and positionals are added.
fn handle_param(
    caps: &Captures,
    given: &[Expr],
    pos: &mut Vec<Expr>,
    at_sentence_start: bool,
    orig_fmt: &str,
) -> Result<String, (usize, usize, String)> {
    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let pre_cap = caps.name("pre");
    let uc = if let Some(m) = caps.name("uc") {
        m.as_str() == "^"
    } else {
        // or if article has uc or the noun is first or at new sentence
        at_sentence_start
            || pre_cap
                .filter(|s| {
                    s.as_str()
                        .trim_start_matches('?')
                        .starts_with(|c: char| c.is_uppercase())
                })
                .is_some()
    };

    let mut pre = pre_cap.map_or("", |m| m.as_str());
    let (mut nr, nr_s, nr_e) = caps
        .name("nr")
        .map_or(("", 0, 0), |m| (m.as_str(), m.start(), m.end()));

    let case = caps.name("case").map_or("", |m| m.as_str());
    let noun = caps.name("noun").unwrap();
    let post = caps.name("post").map_or("", |m| m.as_str());

    let plurality;
    // NB: if None, no alpha found => all are punct; occurs with '+' or '-'.
    (plurality, nr) = split_at_find_start(nr, |c| c.is_alphanumeric()).unwrap_or((nr, ""));
    let noun =
        get_opt_num_ph_expr(noun.as_str(), given).map_err(|s| (noun.start(), noun.end(), s))?;

    let (nr_fmt, fmt): (Vec<_>, Vec<_>) =
        orig_fmt
            .split(':')
            .filter(|&s| !s.is_empty())
            .partition(|&s| {
                match s {
                    "#x" | "-" | "+" | "x?" | "X?" => false,
                    x if x.starts_with('#')
                        && x.ends_with(&['x', 'X', 'o', 'p', 'b', 'e', 'E']) =>
                    {
                        false
                    }
                    x if x.ends_with(&['$', '*']) => true,
                    x if x.starts_with('.') => false,
                    x if x.ends_with('?') => true,
                    x if x.ends_with(|c: char| c.is_ascii_digit()) => !x.starts_with('0'), // width or fill
                    x => {
                        if !x.is_empty() {
                            eprintln!("Unhandled formatting '{x}'")
                        }
                        true
                    }
                }
            });
    let fmt = if fmt.is_empty() {
        String::new()
    } else {
        ":".to_string() + &join(&fmt, ":")
    };
    let len = pos.len().to_string();
    let noun_space;
    let nr_expr: Expr = if plurality.contains(&['#', '$']) {
        let nr_space;
        (pre, nr_space) = split_at_find_end(pre, |c: char| !c.is_whitespace()).unwrap_or((pre, ""));
        (nr, noun_space) = split_at_find_end(nr, |c: char| !c.is_whitespace()).unwrap_or((nr, ""));
        let nr_expr: TokenStream = match get_opt_num_ph_expr(nr, given) {
            Ok(n) if plurality != "$" => parse_quote!(#n),
            Ok(n) => parse_quote!(ranting::rant_convert_numbers(#n as i64)),
            Err(s) => return Err((nr_s, nr_e, s)),
        };
        let nr_fmt_strlit = if nr_fmt.is_empty() {
            format!("{nr_space}{{}}")
        } else {
            if plurality != "$" {
                return Err((
                    nr_s,
                    nr_e,
                    "number formatting not allowed for `{nr}' converted to words.".to_string(),
                ));
            }
            format!("{nr_space}{{:{}}}", join(&nr_fmt, ":"))
        };
        parse_quote!(format!(#nr_fmt_strlit, #nr_expr))
    } else if nr_fmt.is_empty() {
        if pre.is_empty() {
            noun_space = "";
        } else {
            (pre, noun_space) = split_at_find_end(pre, |c: char| !c.is_whitespace())
                .expect("pre without end space?");
        }
        parse_quote!("".to_string())
    } else {
        let m = caps.get(0).unwrap();
        return Err((
            m.start(),
            m.end(),
            "number formatting, for placeholder without a number.".to_string(),
        ));
    };
    pos.push(parse_quote!(ranting::handle_placeholder(&#noun, #nr_expr, #uc, [#pre, #plurality, #noun_space, #case, #post])));
    Ok(format!("{{{len}{fmt}}}"))
}

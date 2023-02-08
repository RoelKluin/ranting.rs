// (c) RoelKluin 2022 GPL v3
#![feature(iter_intersperse)]

mod language;
mod ranting_impl;
mod str_lit;

#[allow(dead_code)]
use language::english as lang;
use language::english_shared as lang_shared;

use darling::{FromDeriveInput, ToTokens};
use lazy_static::lazy_static;
use proc_macro::{self, TokenStream as TokenStream1};
use proc_macro2::{Punct, Spacing, Span, TokenStream};
use ranting_impl::*;
use regex::{Captures, Match, Regex};
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
                #[derive(ranting::strum_macros::Display, ranting_derive::Ranting)]
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
                        .filter(|m| m.start() == 0 || m.as_str().starts_with(&['.', '?', '!']))
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

/// Append to the lit_str placeholder part and extend params. The fmt is appended if Some().
fn res_pos_push(res: &mut String, pos: &mut Vec<Expr>, expr: Expr, fmt: &str) {
    res.push('{');
    res.push_str(pos.len().to_string().as_str());
    res.push_str(fmt);
    res.push('}');
    pos.push(expr);
}

fn split_at_find_start<'a>(s: &'a str, fun: fn(char) -> bool) -> Option<(&'a str, &'a str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end_space<'a>(s: &'a str) -> Option<(&'a str, &'a str)> {
    s.rfind(|c: char| !c.is_whitespace())
        .map(|u| s.split_at(u + 1))
}

fn cap_and_space<'a>(cap: Option<Match<'a>>, space_at_end: bool) -> (&'a str, &'a str) {
    cap.and_then(|s| {
        if space_at_end {
            split_at_find_end_space(s.as_str())
        } else {
            split_at_find_start(s.as_str(), |c: char| !c.is_whitespace())
        }
    })
    .unwrap_or(("", ""))
}

// Placeholder parts are examined, added are replacements known at compile time,
// or placeholders are split in many and positionals are added.
fn handle_param(
    caps: &Captures,
    given: &[Expr],
    pos: &mut Vec<Expr>,
    at_sentence_start: bool,
    nr_fmt: &str,
) -> Result<String, (usize, usize, String)> {
    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let pre_cap = caps.name("pre");
    let mut uc = if let Some(m) = caps.name("uc") {
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

    // could be a struct or enum with a Ranting trait or just be a regular
    //  placeholder if withou all other SayPlaceholder elements.
    let cap = caps.name("noun").unwrap();
    let noun = get_opt_num_ph_expr(cap.as_str(), given).map_err(|s| (cap.start(), cap.end(), s))?;
    let (mut pre, mut art_space) = cap_and_space(pre_cap, true);

    let mut etc1;
    (pre, etc1) = split_at_find_start(pre, |c| c.is_whitespace()).unwrap_or((pre, ""));

    let mut nr_space = "";
    if !etc1.is_empty() {
        nr_space = art_space;
        (art_space, etc1) =
            split_at_find_start(etc1, |c| c.is_alphanumeric()).unwrap_or(("", etc1));
    }

    let nr_cap = caps.name("nr");
    let (mut nr, mut noun_space) = cap_and_space(nr_cap, true);
    let plurality;
    // None, no alpha found => all are punct; occurs with '+' or '-'.
    (plurality, nr) = split_at_find_start(nr, |c| c.is_alphanumeric()).unwrap_or((nr, ""));
    let mut nr_expr = None;
    if !nr.is_empty() {
        nr_expr = Some(get_opt_num_ph_expr(nr, given).map_err(|s| {
            let cap = nr_cap.unwrap();
            (cap.start(), cap.end(), s)
        })?);
    }

    let (mut etc2_space, etc2) = cap_and_space(caps.name("etc2"), false);
    let (mut post_space, post) = cap_and_space(caps.name("post"), false);

    let opt_case = caps.name("case").map(|m| m.as_str());
    let visible_noun = opt_case.map(|s| s.starts_with('?')) != Some(true);
    if !visible_noun {
        if noun_space.is_empty() {
            if !etc2_space.is_empty() {
                etc2_space = "";
            } else if !post_space.is_empty() {
                post_space = "";
            } else if !nr_space.is_empty() {
                nr_space = "";
            } else if !art_space.is_empty() {
                art_space = "";
            }
        } else {
            noun_space = "";
        }
    }
    let mut opt_nr: Option<Expr> = None;
    let as_pl: Expr = match plurality {
        "" => parse_quote!(#noun.is_plural()),
        "+" => parse_quote!(true),
        "-" => parse_quote!(false),
        s => {
            let expr = nr_expr.unwrap();
            // "#", "#?" or "?#" are captured in RE but not "??" or "##".
            if s != "?#" {
                opt_nr = Some(expr.clone());
            } else if !nr_space.is_empty() {
                nr_space = "";
            } else if !art_space.is_empty() {
                art_space = "";
            } else if !noun_space.is_empty() {
                if !etc2_space.is_empty() {
                    noun_space = etc2_space;
                    etc2_space = "";
                } else {
                    noun_space = post_space;
                    post_space = "";
                }
            }
            parse_quote!(#expr != 1)
        }
    };

    let mut res = "".to_owned();
    let fmt = nr_fmt
        .split(':')
        .filter(|&s| {
            match s {
                "#x" | "-" | "+" | "x?" | "X?" => false,
                x if x.starts_with('#') && x.ends_with(&['x', 'X', 'o', 'p', 'b', 'e', 'E']) => {
                    false
                }
                // TODO: If we can ensure a single placeholder replacement, then make this true:
                x if x.ends_with(&['$', '*']) => {
                    panic!("parameter $ or multiple (*) not supported yet")
                }
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
        })
        .collect::<Vec<_>>()
        .join(":");

    // This may be an article or certain verbs that can occur before the noun:
    if !pre.is_empty() {
        let mut p = pre.to_lowercase();
        let mut optional_article = false;
        if let Some(s) = p.as_str().strip_prefix('?') {
            p = s.to_string();
            optional_article = true;
        }
        if lang::is_article_or_so(p.as_str()) {
            if !optional_article && (plurality == "-" || plurality == "+") {
                let a = lang_shared::adapt_article(
                    p.clone(),
                    p.as_str(),
                    art_space,
                    plurality == "+",
                    uc,
                );
                res.push_str(&a);
            } else {
                let call = parse_quote!(ranting::adapt_article(#noun.indefinite_article(#optional_article, #uc), #p, #art_space, #as_pl, #uc));
                res_pos_push(&mut res, pos, call, fmt.as_str());
            }
        } else {
            assert!(post.is_empty(), "verb before and after?");
            if plurality == "+" {
                let verb = lang::plural_verb(p.as_str(), uc);
                res.push_str(format!("{}", verb).as_str());
            } else {
                let call = parse_quote!(ranting::inflect_verb(#noun.subjective(), #p, #as_pl, #uc));
                res_pos_push(&mut res, pos, call, fmt.as_str());
            }
            res.push_str(art_space);
        }
        uc = false;
    }
    if !etc1.is_empty() {
        res.push_str(etc1);
        if opt_nr.is_none() {
            res.push_str(nr_space);
        }
    }
    if let Some(expr) = opt_nr {
        res.push_str(nr_space);
        res_pos_push(&mut res, pos, expr, nr_fmt);
    }
    // also if case is None, the noun should be printed.
    if visible_noun {
        res.push_str(noun_space);
        let expr = match opt_case.and_then(lang::get_case_from_str) {
            Some(case) if case.ends_with("ive") => {
                let path = path_from(format!("ranting::inflect_{case}"));
                parse_quote!(#path(#noun.subjective(), #as_pl, #uc))
            }
            Some(word) => {
                let w = word.trim_end_matches('>');
                parse_quote!(#noun.mutate_noun(#w, #uc))
            }
            None => {
                parse_quote!(#noun.inflect(#as_pl, #uc))
            }
        };
        res_pos_push(&mut res, pos, expr, fmt.as_str());
        uc = false;
    }
    res.push_str(etc2_space);
    res.push_str(etc2);
    if !post.is_empty() {
        res.push_str(post_space);
        match post {
            "'" | "'s" => {
                if plurality == "-" {
                    res.push_str("'s");
                } else {
                    let call: Expr = parse_quote!(ranting::adapt_possesive_s(&#noun, #as_pl));
                    res_pos_push(&mut res, pos, call, fmt.as_str());
                }
            }
            v => {
                if plurality == "+" {
                    let verb = lang::plural_verb(v, uc);
                    res.push_str(format!("{}", verb).as_str());
                } else {
                    let call =
                        parse_quote!(ranting::inflect_verb(#noun.subjective(), #v, #as_pl, #uc));
                    res_pos_push(&mut res, pos, call, fmt.as_str());
                }
            }
        }
    }
    Ok(res)
}

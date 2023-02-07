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
    let mut uc = if let Some(m) = caps.name("uc") {
        m.as_str() == "^"
    } else {
        // or if article has uc or the noun is first or at new sentence
        at_sentence_start
            || caps
                .name("pre")
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
    let pre;
    let mut art_space;
    (pre, art_space) = caps
        .name("pre")
        .and_then(|s| {
            s.as_str()
                .rfind(|c: char| !c.is_whitespace())
                .map(|u| s.as_str().split_at(u + 1))
        })
        .unwrap_or(("", ""));

    let etc1;
    let mut etc1_nr_space;
    (etc1, etc1_nr_space) = caps
        .name("etc1")
        .and_then(|s| {
            s.as_str()
                .rfind(|c: char| !c.is_whitespace())
                .map(|u| s.as_str().split_at(u + 1))
        })
        .unwrap_or(("", ""));

    let nr;
    let mut noun_space;
    let nr_cap = caps.name("nr");
    (nr, noun_space) = nr_cap
        .and_then(|s| {
            s.as_str()
                .rfind(|c: char| !c.is_whitespace())
                .map(|u| s.as_str().split_at(u + 1))
        })
        .unwrap_or(("", ""));
    let plurality = nr.chars().next();

    let mut post_space = caps.name("sp4").map_or("", |m| m.as_str());
    let opt_case = caps.name("case").map(|m| m.as_str());
    let visible_noun = opt_case.map(|s| s.starts_with('?')) != Some(true);
    if !visible_noun {
        if noun_space.is_empty() {
            if !post_space.is_empty() {
                post_space = "";
            } else if !etc1_nr_space.is_empty() {
                etc1_nr_space = "";
            } else if !art_space.is_empty() {
                art_space = "";
            }
        } else {
            noun_space = "";
        }
    }
    let mut opt_nr: Option<Expr> = None;
    let as_pl: Expr = if let Some(c) = plurality {
        match c {
            '+' => parse_quote!(true),
            '-' => parse_quote!(false),
            c => {
                let expr = get_opt_num_ph_expr(nr.trim_start_matches(&['?', '#']), given).map_err(
                    |s| {
                        let cap = nr_cap.unwrap();
                        (cap.start(), cap.end(), s)
                    },
                )?;
                // "#", "#?" or "?#" are captured in RE but not "??" or "##".
                if c != '?' {
                    opt_nr = Some(expr.clone());
                } else if !etc1.is_empty() {
                    etc1_nr_space = "";
                } else if !art_space.is_empty() {
                    art_space = "";
                } else if !noun_space.is_empty() {
                    noun_space = post_space;
                    post_space = "";
                }
                parse_quote!(#expr != 1)
            }
        }
    } else {
        parse_quote!(#noun.is_plural())
    };

    let mut res = caps.name("sentence").map_or("", |s| s.as_str()).to_owned();
    let post = caps.name("post1").or_else(|| caps.name("post2"));
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
            if let Some(c) = plurality.filter(|&c| !optional_article && (c == '-' || c == '+')) {
                let a = lang_shared::adapt_article(p.clone(), p.as_str(), art_space, c == '+', uc);
                res.push_str(&a);
            } else {
                let call = parse_quote!(ranting::adapt_article(#noun.indefinite_article(#optional_article, #uc), #p, #art_space, #as_pl, #uc));
                res_pos_push(&mut res, pos, call, fmt.as_str());
            }
        } else {
            assert!(post.is_none(), "verb before and after?");
            if let Some(verb) =
                plurality.and_then(|c| lang::inflect_verb_wo_subj(p.as_str(), c, uc))
            {
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
            res.push_str(etc1_nr_space);
        }
    }
    if let Some(expr) = opt_nr {
        res.push_str(etc1_nr_space);
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
    if let Some(etc2) = caps.name("etc2") {
        res.push_str(etc2.as_str());
    }
    if let Some(post) = post.map(|m| m.as_str()) {
        res.push_str(post_space);
        match post {
            "'" | "'s" => {
                if let Some(s) = plurality.and_then(lang::adapt_possesive_s_wo_subj) {
                    res.push_str(s);
                } else {
                    let call: Expr = parse_quote!(ranting::adapt_possesive_s(&#noun, #as_pl));
                    res_pos_push(&mut res, pos, call, fmt.as_str());
                }
            }
            v => {
                if let Some(verb) = plurality.and_then(|c| lang::inflect_verb_wo_subj(v, c, uc)) {
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

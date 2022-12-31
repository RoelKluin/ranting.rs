// (c) RoelKluin 2022 GPL v3
#![feature(iter_intersperse)]

mod ranting_impl;

#[path = "../language/english.rs"]
#[allow(dead_code)]
mod english;
use english as language;

use darling::{FromDeriveInput, ToTokens};
use lazy_static::lazy_static;
use proc_macro::{self, TokenStream as TokenStream1};
use proc_macro2::{Punct, Spacing, Span, TokenStream};
use quote::quote;
use ranting_impl::*;
use regex::{Captures, Regex};
use std::iter;
use syn::{self, parse::Parser, parse_quote, punctuated::Punctuated, Error, Expr, Token};

// TODO: replace Span::mixed_site() with more precise location.

#[proc_macro]
pub fn ack(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as Say);
    quote!(return Ok(#input)).into()
}

#[proc_macro]
pub fn nay(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as Say);
    quote!(return Err(#input)).into()
}

#[proc_macro]
pub fn say(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as Say);
    quote!(#input).into()
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
        syn::Data::Struct(ref mut struct_data) => {
            if let syn::Fields::Named(fields) = &mut struct_data.fields {
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { name: String })
                        .unwrap(),
                );
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { subject: String })
                        .unwrap(),
                );
            }

            quote! {
                #[derive(Ranting)]
                #ast
            }
            .into()
        }
        syn::Data::Enum(_) => quote! {
            #[derive(ranting::strum_macros::Display, Ranting)]
            #ast
        }
        .into(),
        _ => panic!("`add_field` has to be used with structs or enums"),
    }
}

/// Above macros inflect Ranting elements within a placeholder. Structs require a `name` and `subject` String.
#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn inner_derive_ranting(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input);
    let mut options =
        RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    if let syn::Data::Enum(_) = &input.data {
        options.is_enum = true;
    }
    ranting_q(options, &input.ident).into()
}

/// Split placeholders in multiple and extend params accordingly.
// currently via a regex; convenient, but suboptimal because pattern error indication is lacking.
impl syn::parse::Parse for Say {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing format string"));
        }
        let lit = input.parse::<syn::LitStr>()?;

        let params_in = if input.is_empty() {
            vec![]
        } else {
            input.parse::<Token![,]>()?;

            input
                .parse_terminated::<_, Token![,]>(Expr::parse)?
                .into_iter()
                .collect()
        };
        lazy_static! {
            static ref RE: Regex = Regex::new(language::RANTING_PLACEHOLDER).unwrap();
        }
        let mut err = None;
        #[cfg(feature = "debug")]
        eprintln!("{}", lit.value());
        let mut params = vec![];

        let lit_str = RE
            .replace_all(&lit.value(), |caps: &Captures| {
                match handle_param(caps, &params_in, &mut params, lit.span()) {
                    Ok(s) => s,
                    Err(e) => {
                        err = Some(e);
                        String::new()
                    }
                }
            })
            .to_string();

        match err {
            Some(e) => Err(e),
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
        *tokens = quote!(format!(#macro_tokens));
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
fn get_match_expr(m: Option<regex::Match>, given: &[Expr]) -> Result<Expr, String> {
    match m.map(|s| s.as_str()) {
        Some(part) => match part.parse::<usize>() {
            Err(_) => Ok(path_from(part)),
            Ok(u) => given
                .get(u)
                .cloned()
                .ok_or_else(|| format!("{part}, positional {u} is out of bounds")),
        },
        None => Err("missing in the placeholder.".to_string()),
    }
}

/// Append to the lit_str placeholder part and extend params. The fmt is appended if Some().
fn res_pos_push(res: &mut String, pos: &mut Vec<Expr>, expr: Expr, ofmt: Option<&str>) {
    res.push('{');
    res.push_str(pos.len().to_string().as_str());
    if let Some(fmt) = ofmt {
        res.push_str(fmt);
    }
    res.push('}');
    pos.push(expr);
}

// Placeholder parts are examined, added are replacements known at compile time,
// or placeholders are split in many and positionals are added.
fn handle_param(
    caps: &Captures,
    given: &[Expr],
    pos: &mut Vec<Expr>,
    span: Span,
) -> Result<String, Error> {
    let mut is_plain_placeholder = true;
    let mut nr: Option<Expr> = None;
    let mut noun_space = caps.name("sp1");
    let mut post_space = caps.name("sp3").map_or("", |m| m.as_str());
    let post = caps.name("post1").or_else(|| caps.name("post2"));
    let ofmt = caps.name("fmt").map(|s| s.as_str());

    // could be a struct or enum with a Ranting trait or just be a regular
    //  placeholder if withou all other SayPlaceholder elements.
    let noun = get_match_expr(caps.name("noun"), given)
        .map_err(|s| Error::new(Span::mixed_site(), format!("noun: {s}")))?;
    let plurality = caps
        .name("plurality")
        .and_then(|m| m.as_str().chars().next());

    let is_pl: Expr = if let Some(c) = plurality {
        is_plain_placeholder = false;
        match c {
            '+' => parse_quote!(true),
            '-' => parse_quote!(false),
            c => {
                noun_space = caps.name("sp2");
                let expr = get_match_expr(caps.name("nr"), given)
                    .map_err(|s| Error::new(Span::mixed_site(), format!("nr: {s}")))?;
                // "#", "#?" or "?#" are captured in RE but not "??" or "##".
                if c != '?' {
                    nr = Some(expr.clone());
                }
                parse_quote!(#expr != 1)
            }
        }
    } else {
        parse_quote!(#noun.is_plural())
    };
    let mut res = caps.name("sentence").map_or("", |s| s.as_str()).to_owned();

    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let mut uc = if let Some(m) = caps.name("uc") {
        is_plain_placeholder = false;
        m.as_str() == "^"
    } else {
        // or if article has uc or the noun is first or at new sentence
        caps.name("pre")
            .filter(|s| s.as_str().starts_with(|c: char| c.is_uppercase()))
            .is_some()
            || caps
                .name("sentence")
                .filter(|m| m.start() == 0 || m.as_str() != "")
                .is_some()
    };

    // This may be an article or certain verbs that can occur before the noun:
    if let Some(pre) = caps.name("pre") {
        is_plain_placeholder = false;
        let p = pre.as_str().to_lowercase();

        if language::is_article_or_so(p.as_str()) {
            if let Some(c) = plurality.filter(|&c| c == '-' || c == '+') {
                let a = language::adapt_article(p.as_str(), p.as_str(), c == '+', uc);
                res.push_str(format!("{}", a).as_str());
            } else {
                let call = parse_quote!(ranting::adapt_article(#noun.indefinite_article(#uc), #p, #is_pl, #uc));
                res_pos_push(&mut res, pos, call, None);
            }
        } else {
            assert!(post.is_none(), "verb before and after?");
            if let Some(verb) =
                plurality.and_then(|c| language::inflect_verb_wo_subj(p.as_str(), c, uc))
            {
                res.push_str(format!("{}", verb).as_str());
            } else {
                let call = parse_quote!(ranting::inflect_verb(#noun.subjective(), #p, #is_pl, #uc));
                res_pos_push(&mut res, pos, call, None);
            }
        }
        res.push_str(caps.name("s_pre").map_or("", |m| m.as_str()));
        uc = false;
    }

    if let Some(etc1) = caps.name("etc1") {
        is_plain_placeholder = false;
        res.push_str(etc1.as_str());
    }
    if let Some(expr) = nr {
        res.push_str(caps.name("sp1").map_or("", |m| m.as_str()));
        res_pos_push(&mut res, pos, expr, ofmt);
    }
    // also if case is None, the noun should be printed.
    let opt_case = caps.name("case").map(|m| m.as_str());
    if opt_case != Some("?") {
        res.push_str(noun_space.map_or("", |m| m.as_str()));
        let mut opt_format = None;
        let expr = match opt_case.and_then(language::get_case_from_str) {
            Some(case) if case.ends_with("ive") => {
                let path = path_from(format!("ranting::inflect_{case}"));
                parse_quote!(#path(#noun.subjective(), #is_pl, #uc))
            }
            Some(word) => {
                let w = word.trim_end_matches('>');
                parse_quote!(ranting::inflect_noun(#noun.mut_name(#w), #noun.is_plural(), #is_pl, #uc))
            }
            None if is_plain_placeholder && caps.name("etc2").is_none() && post.is_none() => {
                opt_format = ofmt;
                noun.clone()
            }
            None => {
                parse_quote!(ranting::inflect_noun(#noun.name(false), #noun.is_plural(), #is_pl, #uc))
            }
        };
        res_pos_push(&mut res, pos, expr, opt_format);
        uc = false;
    } else if noun_space.is_none() {
        post_space = "";
    }
    if let Some(etc2) = caps.name("etc2") {
        res.push_str(etc2.as_str());
    }
    if let Some(post) = post.map(|m| m.as_str()) {
        res.push_str(post_space);
        match post {
            "'" | "'s" => {
                if let Some(c) = plurality.and_then(language::adapt_possesive_s_wo_subj) {
                    res.push(c);
                } else {
                    let call = parse_quote!(ranting::adapt_possesive_s(#noun.name(false), #noun.is_plural(), #is_pl));
                    res_pos_push(&mut res, pos, call, None);
                }
            }
            v => {
                if let Some(verb) = plurality.and_then(|c| language::inflect_verb_wo_subj(v, c, uc))
                {
                    res.push_str(format!("{}", verb).as_str());
                } else {
                    let call =
                        parse_quote!(ranting::inflect_verb(#noun.subjective(), #v, #is_pl, #uc));
                    res_pos_push(&mut res, pos, call, None);
                }
            }
        }
    }
    Ok(res)
}

// (c) RoelKluin 2022 GPL v3

mod english;
mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use english as language;
use itertools::Itertools;
use lazy_static::lazy_static;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use ranting_impl::*;
use regex::{Captures, Regex};
use syn::parse::Parser;
use syn::{
    self, parse, parse_macro_input, DeriveInput, Error as SynError, Expr, ExprLit, ExprPath,
    Lit::Int, LitInt,
};

#[proc_macro]
pub fn ack(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("return Ok(format!({lit}))").parse().unwrap(),
        Err(e) => e,
    }
}

#[proc_macro]
pub fn nay(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("return Err(format!({lit}))").parse().unwrap(),
        Err(e) => e,
    }
}

#[proc_macro]
pub fn say(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("format!({lit})").parse().unwrap(),
        Err(e) => e,
    }
}

/// Implies `#[derive(Ranting)]` and includes `name` and `subject` in structs.
/// For an enum `"it"` and the variant's name are assumed.
#[proc_macro_attribute]
pub fn derive_ranting(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            match &mut struct_data.fields {
                syn::Fields::Named(fields) => {
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
                _ => (),
            }

            return quote! {
                #[derive(Ranting)]
                #ast
            }
            .into();
        }
        syn::Data::Enum(_) => {
            return quote! {
                #[derive(ranting::strum_macros::Display, Ranting)]
                #ast
            }
            .into();
        }
        _ => panic!("`add_field` has to be used with structs or enums"),
    }
}

/// Above macros inflect Ranting elements within a placeholder. Structs require a `name` and `subject` String.
#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn inner_derive_ranting(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let mut options =
        RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    if let syn::Data::Enum(_) = &input.data {
        options.is_enum = true;
    }
    ranting_q(options, &input.ident).into()
}

fn do_say(input: TokenStream) -> Result<String, TokenStream> {
    let mut token_it = input.into_iter().peekable();

    let mut lit = match lit_first(token_it.next()) {
        Ok(lit) => format!("{lit}"),
        Err(e) => return Err(e.into_compile_error().into()),
    };
    let mut given = vec![];
    while token_it.peek().is_some() {
        given.push(comma_next(&mut token_it).map_err(|e| {
            let ts: TokenStream = e.into_compile_error().into();
            ts
        })?);
    }

    // regex to capture the placholders or sentence ends
    // useful: https://regex101.com/r/Ly7O1x/3/
    lazy_static! {
        static ref RE: Regex = Regex::new(language::RANTING_PLACEHOLDER).unwrap();
    }
    //eprintln!("{:?}", RE.to_string());
    let mut err = None;
    let mut positional: Vec<String> = vec![];
    //let original = lit.to_string();

    lit = RE
        .replace_all(&lit, |caps: &Captures| {
            match handle_param(caps, &given, &mut positional) {
                Ok(s) => s,
                Err(e) => {
                    err = Some(e);
                    String::new()
                }
            }
        })
        .to_string();

    //eprintln!("{}\n {}", original, lit.as_str());
    if let Some(e) = err {
        return Err(e.into_compile_error().into());
    }

    if !positional.is_empty() {
        lit.push_str(", ");
        lit.push_str(&positional.iter().join(", "));
    }
    Ok(lit)
}

fn lit_first(o: Option<TokenTree>) -> Result<Literal, SynError> {
    match o {
        Some(TokenTree::Literal(l)) => Ok(l),
        Some(TokenTree::Ident(i)) => Err(SynError::new(
            i.span().into(),
            "expected Literal, not Identifier",
        )),
        Some(TokenTree::Punct(p)) => Err(SynError::new(
            p.span().into(),
            "expected Literal, not Punct",
        )),
        Some(TokenTree::Group(g)) => Err(SynError::new(
            g.span().into(),
            "expected Literal, not Group",
        )),
        None => Err(SynError::new(Span::call_site().into(), "expected Literal")),
    }
}

fn comma_next(it: &mut dyn Iterator<Item = TokenTree>) -> Result<String, SynError> {
    let t: TokenTree = it.next().unwrap();

    let res = match t {
        TokenTree::Punct(p) => (p.as_char() == ',')
            .then_some(it.next())
            .flatten()
            .ok_or_else(|| SynError::new(p.span().into(), "expected token after Punct")),
        TokenTree::Literal(l) => Err(SynError::new(
            l.span().into(),
            "expected Punct, not Literal",
        )),
        TokenTree::Ident(i) => Err(SynError::new(
            i.span().into(),
            "expected Punct, not Identifier",
        )),
        TokenTree::Group(g) => Err(SynError::new(g.span().into(), "expected Punct, not Group")),
    };

    match parse::<Expr>(res?.into())? {
        Expr::Path(ExprPath { path, .. }) => Ok(path.to_token_stream().to_string()),
        Expr::Lit(ExprLit {
            lit: Int(lit_int), ..
        }) => Ok(lit_int.to_token_stream().to_string()),
        e => Err(SynError::new(
            Span::call_site().into(),
            format!("unexpected expression: {e:?}"),
        )),
    }
}

// arguments become positionals
fn handle_param(
    caps: &Captures,
    given: &Vec<String>,
    pos: &mut Vec<String>,
) -> Result<String, SynError> {
    let mut is_plain_placeholder = true;
    let mut is_pl = String::new();
    let mut nr = String::new();
    let mut noun_space = caps.name("sp1");
    let mut post_space = caps.name("sp3").map_or("", |m| m.as_str());
    let post = caps.name("post1").or(caps.name("post2"));

    if let Some(plurality) = caps.name("plurality") {
        is_plain_placeholder = false;
        match plurality.as_str().trim().split_at(1) {
            ("+", "") => is_pl.push_str("true"),
            ("-", "") => is_pl.push_str("false"),
            ("#", x) | ("?", x) => {
                noun_space = caps.name("sp2");
                let mut x = x;
                // "#", "#?" or "?#" are captured in RE but not "??" or "##".
                if let Some(nr) = x.strip_prefix(&['?', '#']) {
                    x = nr; // nr not assigned!
                } else {
                    nr = x.to_string();
                }
                if let Ok(u) = x.trim_end().parse::<usize>() {
                    x = given
                        .get(u)
                        .ok_or(SynError::new(
                            Span::mixed_site().into(),
                            format!("A #var, positional {u}, is out of bounds"),
                        ))?
                        .as_str();
                    if !nr.is_empty() {
                        // keep leading whitespace if displayed.
                        nr = nr
                            .split_once(|c: char| c.is_ascii_digit())
                            .map_or("", |s| s.0)
                            .to_string()
                            + x;
                    }
                }
                is_pl = format!("{x} != 1");
            }
            (a, b) => panic!("Unrecognized plurality '{a}{b}'"),
        }
    }

    // could be a struct or enum with a Ranting trait or just be a regular
    //  placeholder if withou all other SayPlaceholder elements.
    let mut noun = caps.name("noun").map(|s| s.as_str()).ok_or(SynError::new(
        Span::mixed_site().into(),
        "The Noun is missing in a placeholder.",
    ))?;
    let fmt = caps.name("fmt").map_or("", |s| s.as_str());

    // a positional.
    if let Ok(u) = noun.parse::<usize>() {
        noun = given
            .get(u)
            .ok_or(SynError::new(
                Span::mixed_site().into(),
                format!("A Noun, positional {u}, is out of bounds"),
            ))?
            .as_str();
    }

    if is_pl.is_empty() {
        is_pl = format!("{noun}.is_plural()");
    }
    let mut res = caps.name("sentence").map_or("", |s| s.as_str()).to_owned();

    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let mut uc = if let Some(c) = caps.name("uc") {
        is_plain_placeholder = false;
        c.as_str() == "^"
    } else {
        // or if article has uc or the noun is first or at new sentence
        caps.name("pre")
            .filter(|s| s.as_str().starts_with(|c: char| c.is_uppercase()))
            .or(caps
                .name("sentence")
                .filter(|m| m.start() == 1 || m.as_str() != ""))
            .is_some()
    };

    // This may be an article or certain verbs that can occur before the noun:
    if let Some(pre) = caps.name("pre") {
        is_plain_placeholder = false;
        let p = pre.as_str().to_lowercase();
        res.push_str(&format!("{{{}}}", pos.len()));
        if language::is_article_or_so(p.as_str()) {
            pos.push(format!(
                "ranting::adapt_article({noun}.indefinite_article(false), \"{p}\", {is_pl}, {uc})"
            ));
        } else {
            assert!(post.is_none(), "verb before and after?");
            pos.push(format!(
                "ranting::inflect_verb({noun}.subjective(), \"{}\", {is_pl}, {uc})",
                p.as_str()
            ));
        }
        res.push_str(caps.name("s_pre").map_or("", |m| m.as_str()));
        uc = false;
    }

    if let Some(etc1) = caps.name("etc1") {
        is_plain_placeholder = false;
        res.push_str(&format!("{}", etc1.as_str()));
    }
    if !nr.is_empty() {
        res.push_str(caps.name("sp1").map_or("", |m| m.as_str()));
        res.push_str(&format!("{{{}{}}}", pos.len(), fmt));
        pos.push(format!("{nr}"));
    }
    // also if case is None, the noun should be printed.
    let opt_case = caps.name("case").map(|m| m.as_str());
    if opt_case != Some("?") {
        res.push_str(noun_space.map_or("", |m| m.as_str()));
        match opt_case.and_then(|s| language::get_case_from_str(s)) {
            Some(case) if case.ends_with("ive") => {
                res.push_str(&format!("{{{}}}", pos.len()));
                pos.push(format!(
                    "ranting::inflect_{case}({noun}.subjective(), {is_pl}, {uc})"
                ))
            }
            Some(word_angular) => {
                let word = word_angular.trim_end_matches('>');
                res.push_str(&format!("{{{}}}", pos.len()));
                pos.push(format!(
                    // {noun}.name would break working for Ranting trait generics
                    "ranting::inflect_noun({noun}.mut_name(\"{word}\").as_str(), {noun}.is_plural(), {is_pl}, {uc})"
                ))
            }
            None if is_plain_placeholder && caps.name("etc2").is_none() && post.is_none() => {
                res.push_str(&format!("{{{}{fmt}}}", pos.len()));
                pos.push(format!("{noun}")); // for non-Ranting variables
            }
            None => {
                res.push_str(&format!("{{{}}}", pos.len()));
                pos.push(format!(
                    // {noun}.name would break working for Ranting trait generics
                    "ranting::inflect_noun({noun}.name(false).as_str(), {noun}.is_plural(), {is_pl}, {uc})"
                ))
            }
        }
        uc = false;
    } else if noun_space.is_none() {
        post_space = "";
    }
    if let Some(etc2) = caps.name("etc2") {
        res.push_str(&format!("{}", etc2.as_str()));
    }
    if let Some(post) = post.map(|m| m.as_str()) {
        res.push_str(&format!("{post_space}{{{}}}", pos.len()));
        match post {
            "'" | "'s" => {
                pos.push(format!(
                    "ranting::adapt_possesive_s({noun}.name(false).as_str(), {noun}.is_plural(), {is_pl})"
                ));
            }
            verb => {
                pos.push(format!(
                    "ranting::inflect_verb({noun}.subjective(), \"{verb}\", {is_pl}, {uc})"
                ));
            }
        }
    }
    Ok(res)
}

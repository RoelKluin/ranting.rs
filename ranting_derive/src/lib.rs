// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use lazy_static::lazy_static;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use ranting_impl::*;
use regex::{Captures, Regex};
use std::default::Default;
use syn::parse::Parser;
use syn::{self, parse, parse_macro_input, DeriveInput, Error as SynError, Expr, ExprPath};

/// Generates the `Ranting` trait implementation
/// Structs that receive this trait require a name and subjective String.
/// and can be referenced in the say!() nay!() and ack!() macros.
#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn inner_derive_ranting(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let options = RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    ranting_q(options, &input.ident).into()
}

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

/// The say!() macro produces a String, a bit similar to format!(), but with extended
/// formatting options for Ranting trait objects provided as arguments to say!().
///
/// Ranting trait objects as arguments to say!()  are displayed as their name by
/// default, or by a pronoun with formatting markers:
///
/// `:` gives a subject, `@` an object, `'` a possesive and `~` an adjective form of the
/// pronoun.
///
/// when prepended with an article, this is adapted to the name if `a`. A capital is
/// preserved. Also `the`, `some`, `these` and `those` can occur before. Ranting mostly
/// uses the 1st plural form. `These` and `those` are converted to `this`
/// and `that` if the subjective is singular.
///
/// A verb after, also in 1st plural form, is also inflected to the subjective's case. The
/// Ranting object enclosed before a verb is assumed to be the subject in the sentence.
///
#[proc_macro]
pub fn say(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("format!({lit})").parse().unwrap(),
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
pub fn ack(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("return Ok(format!({lit}))").parse().unwrap(),
        Err(e) => e,
    }
}

#[derive(Default)]
struct SayPlaceHolder {
    uc: bool,
    pre: String,
    etc1: String,
    plurality: String,
    case: char,
    noun: String,
    etc2: String,
    post: String,
    format: String,
}

impl SayPlaceHolder {
    fn from_caps(
        caps: &Captures,
        given: &Vec<String>,
        sentence_start: usize,
    ) -> Result<Self, SynError> {
        // This may be an article or certain verbs that can occur before the noun:
        let pre = caps.name("pre").map(|s| s.as_str());

        // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
        // 2) uc if article or so is or 3) the noun is first or after start or `. '
        let uc = match caps.name("uc").and_then(|s| s.as_str().chars().next()) {
            Some('^') => true,
            Some(',') => false,
            _ => {
                // or if article has uc or the noun is first or at new sentence
                pre.filter(|s| s.starts_with(|c: char| c.is_uppercase()))
                    .is_some()
                    | (caps.get(0).unwrap().start() == sentence_start)
            }
        };
        // words that are not inflected but are retained.
        let etc1 = caps.name("etc1").map(|s| s.as_str());

        // A plurality character `+` or `-`, a number `#<var>` or hidden `#?<var>`.
        let plurality = caps.name("plurality").map(|s| s.as_str());

        // case as in subject, object, possesive or adjective. Also name specifier.
        let case = caps
            .name("case")
            .and_then(|s| s.as_str().chars().next())
            .unwrap_or_default();

        // could be a struct or enum with a Ranting trait or just be a regular
        //  placeholder if withou all other SayPlaceholder elements.
        let mut noun = caps
            .name("noun")
            .map(|s| s.as_str().to_owned())
            .ok_or(SynError::new(
                Span::mixed_site().into(),
                format!("'{}': missing noun", caps.get(0).unwrap().as_str()),
            ))?;
        if let Ok(u) = noun.parse::<usize>() {
            noun = given
                .get(u)
                .ok_or(SynError::new(
                    Span::call_site().into(),
                    "noun nr out of bounds",
                ))?
                .to_owned();
        }

        // words that are not inflected but are retained.
        let etc2 = caps.name("etc2").map(|s| s.as_str());

        // This is currently always a verb.
        let post = caps.name("post").map(|s| s.as_str());

        // default format!() formatters are allowed, preserved here
        let format = caps.name("format").map(|s| s.as_str());

        Ok(SayPlaceHolder {
            uc,
            pre: pre.unwrap_or_default().to_owned(),
            etc1: etc1.unwrap_or_default().to_owned(),
            plurality: plurality.unwrap_or_default().to_owned(),
            case,
            noun,
            etc2: etc2.unwrap_or_default().to_owned(),
            post: post.unwrap_or_default().to_owned(),
            format: format.unwrap_or_default().to_owned(),
        })
    }
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
    // can duplicated because the n't eats the n
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"(?x)(?:\.\s+|\{
                (?P<uc>[,^])?+
                (?:(?P<pre>
                    [aA]n?|[sS]ome|[tT]h(?:[eo]s)?e|
                    '[rv]e|'d|[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
                    (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|
                        (?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?)\s+)?
                (?P<etc1>\s+(?:[\w-]+\s+)+?)??
                (?P<plurality>[+-]|\#\??\w+\s+)?+
                (?P<case>[`:@~])?+
                (?P<noun>\??[\w-]+)
                (?P<etc2>(?:\s+[\w-]+)*?)??
                (?P<post>(?:(?:\s+\w+)?+'|\s+)[\w-]+)?
                (?P<format>:[^}]+)?+
            \})"
        )
        .unwrap();
    }
    //eprintln!("{:?}", RE.to_string());
    let mut err = None;
    let mut sentence_start = 1;

    let mut positional: Vec<String> = vec![];
    //let original = lit.to_string();

    lit = RE
        .replace_all(&lit, |caps: &Captures| {
            if let Some(new_sentence) = caps.get(0).filter(|m| m.as_str().starts_with('.')) {
                sentence_start = new_sentence.end();
                return new_sentence.as_str().to_string();
            }
            let sf = match SayPlaceHolder::from_caps(caps, &given, sentence_start) {
                Ok(sf) => sf,
                Err(e) => {
                    err = Some(e);
                    return String::new();
                }
            };
            handle_param(sf, &mut positional)
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
        e => Err(SynError::new(
            Span::call_site().into(),
            format!("unexpected expression: {e:?}"),
        )),
    }
}

fn get_case_from_char(c: char) -> Option<&'static str> {
    match c {
        ':' => Some("subjective"),
        '@' => Some("objective"),
        '`' => Some("possesive"),
        '~' => Some("adjective"),
        _ => None,
    }
}

// arguments become positionals
fn handle_param(sf: SayPlaceHolder, pos: &mut Vec<String>) -> String {
    let mut uc = sf.uc;
    let mut res = String::new();
    let mut nr = "";

    let (noun, do_display) = if let Some(n) = sf.noun.strip_prefix('?') {
        (n, false)
    } else {
        (sf.noun.as_str(), true)
    };
    let is_pl = if sf.plurality.is_empty() {
        format!("{noun}.is_plural()")
    } else {
        match sf.plurality.split_at(1) {
            ("+", "") => format!("true"),
            ("-", "") => format!("false"),
            ("#", s) => {
                if let Some(nr) = s.strip_prefix('?') {
                    format!("{nr} != 1") // Note: nr not assigned.
                } else {
                    nr = s;
                    format!("{nr} != 1")
                }
            }
            (a, b) => panic!("Unrecognized plurality '{a}{b}'"),
        }
    };

    if !sf.pre.is_empty() {
        let p = sf.pre.trim_end().to_lowercase();
        res.push_str(&format!("{{{}}}", pos.len()));
        match p.as_str() {
            "some" | "a" | "an" | "the" | "these" | "those" => pos.push(format!(
                "ranting::inflect_article({noun}.a_or_an({uc}), \"{p}\", {is_pl}, {uc})"
            )),
            verb => {
                assert!(sf.post.is_empty(), "verb before and after?");
                pos.push(format!(
                    "ranting::inflect_verb({noun}.subjective(), \"{verb}\", {is_pl}, {uc})"
                ))
            }
        }
        uc = false;
    }
    if !sf.etc1.is_empty() {
        if !res.is_empty() {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!("\"{}\"", sf.etc1));
    }
    if !nr.is_empty() {
        if !res.is_empty() {
            res.push(' ')
        }
        res.push_str(&format!("{{{}{}}}", pos.len(), sf.format));
        pos.push(format!("{nr}"));
    }
    if do_display {
        if !res.is_empty() {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        match get_case_from_char(sf.case) {
            Some(case) => pos.push(format!(
                "ranting::inflect_{case}({noun}.subjective(), {is_pl}, {uc})"
            )),
            None if sf.plurality.is_empty() && sf.pre.is_empty() && sf.post.is_empty() => {
                pos.push(format!("{noun}")); // for non-Ranting variables
            }
            None => pos.push(format!(
                // {noun}.name would break working for Ranting trait generics
                "ranting::inflect_noun({noun}.name(false).as_str(), {noun}.is_plural(), {is_pl}, {uc})"
            )),
        }
        uc = false;
    }
    if !sf.etc2.is_empty() {
        if !res.is_empty() {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!("\"{}\"", sf.etc2));
    }
    if !sf.post.is_empty() {
        if !res.is_empty() && !sf.post.starts_with('\'') {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!(
            "ranting::inflect_verb({noun}.subjective(), \"{}\", {is_pl}, {uc})",
            sf.post
        ));
    }
    res
}

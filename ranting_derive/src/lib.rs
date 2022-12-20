// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use lazy_static::lazy_static;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use ranting_impl::*;
use regex::{Captures, Regex};
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
            r"(?x)(?P<sentence>^|\.\s+|)\{
                (?P<uc>[,^])?+
                (?:(?P<pre>
                    [aA]n?|[sS]ome|[tT]h(?:[eo]s)?e|
                    '[rv]e|'d|[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
                    (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|
                        (?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?)(?P<s_pre>\s+))?
                (?P<etc1>\s+(?:[\w-]+\s+)+?)??
                (?:(?P<plurality>[+-]|\#\??\w+)(?P<s_nr>\s*))?+
                (?P<case>[`:@~])?+
                (?P<noun>\??[\w-]+)
                (?P<etc2>(?:\s+[\w-]+)*?)??
                (?:(?P<s_post>\s+)(?P<post>(?:\w+')?+[\w-]+))?
                (?P<fmt>:[^}]+)?+
            \}"
        )
        .unwrap();
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
        e => Err(SynError::new(
            Span::call_site().into(),
            format!("unexpected expression: {e:?}"),
        )),
    }
}

fn get_case_from_str(s: &str) -> Option<&'static str> {
    match s {
        ":" => Some("subjective"),
        "@" => Some("objective"),
        "`" => Some("possesive"),
        "~" => Some("adjective"),
        x => panic!("Unsupported case {x}"),
    }
}

// arguments become positionals
fn handle_param(
    caps: &Captures,
    given: &Vec<String>,
    pos: &mut Vec<String>,
) -> Result<String, SynError> {
    // could be a struct or enum with a Ranting trait or just be a regular
    //  placeholder if withou all other SayPlaceholder elements.
    let mut noun = caps.name("noun").map(|s| s.as_str()).ok_or(SynError::new(
        Span::mixed_site().into(),
        format!("'{}': missing noun", caps.get(0).unwrap().as_str()),
    ))?;

    let mut display_noun = true;
    let mut basic_placeholder = true;
    // a positional.
    if let Ok(u) = noun.parse::<usize>() {
        noun = given
            .get(u)
            .ok_or(SynError::new(
                Span::call_site().into(),
                "noun positional out of bounds",
            ))?
            .as_str();
    } else if let Some(n) = noun.strip_prefix('?') {
        basic_placeholder = false;
        display_noun = false;
        noun = n;
    }

    let mut nr = "";

    let is_pl = if let Some(plurality) = caps.name("plurality") {
        basic_placeholder = false;
        match plurality.as_str().split_at(1) {
            ("+", "") => format!("true"),
            ("-", "") => format!("false"),
            ("#", s) => {
                if let Some(nr) = s.strip_prefix('?') {
                    format!("{nr} != 1") // Note: nr not assigned.
                } else {
                    // TODO: allow positional here?
                    nr = s;
                    format!("{nr} != 1")
                }
            }
            (a, b) => panic!("Unrecognized plurality '{a}{b}'"),
        }
    } else {
        format!("{noun}.is_plural()")
    };
    let mut res = caps
        .name("sentence")
        .map(|s| s.as_str().to_owned())
        .unwrap_or_default();

    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let mut uc = if let Some(c) = caps.name("uc") {
        basic_placeholder = false;
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
        basic_placeholder = false;
        let p = pre.as_str().to_lowercase();
        res.push_str(&format!("{{{}}}", pos.len()));
        match p.as_str() {
            "some" | "a" | "an" | "the" | "these" | "those" => pos.push(format!(
                "ranting::inflect_article({noun}.a_or_an(false), \"{p}\", {is_pl}, {uc})"
            )),
            verb => {
                assert!(caps.name("post").is_none(), "verb before and after?");
                pos.push(format!(
                    "ranting::inflect_verb({noun}.subjective(), \"{verb}\", {is_pl}, {uc})"
                ))
            }
        }
        res.push_str(caps.name("s_pre").map(|m| m.as_str()).unwrap_or_default());
        uc = false;
    }

    if let Some(etc1) = caps.name("etc1") {
        basic_placeholder = false;
        res.push_str(&format!("{}", etc1.as_str()));
    }
    if !nr.is_empty() {
        let fmt = caps.name("fmt").map(|s| s.as_str()).unwrap_or_default();
        res.push_str(&format!("{{{}{}}}", pos.len(), fmt));
        res.push_str(caps.name("s_nr").map(|m| m.as_str()).unwrap_or_default());
        pos.push(format!("{nr}"));
    }
    if display_noun {
        res.push_str(&format!("{{{}}}", pos.len()));
        match caps
            .name("case")
            .and_then(|s| get_case_from_str(s.as_str()))
        {
            Some(case) => pos.push(format!(
                "ranting::inflect_{case}({noun}.subjective(), {is_pl}, {uc})"
            )),
            None if basic_placeholder && caps.name("etc2").is_none() && caps.name("post").is_none() => {
                pos.push(format!("{noun}")); // for non-Ranting variables
            }
            None => pos.push(format!(
                // {noun}.name would break working for Ranting trait generics
                "ranting::inflect_noun({noun}.name(false).as_str(), {noun}.is_plural(), {is_pl}, {uc})"
            )),
        }
        uc = false;
    }
    if let Some(etc2) = caps.name("etc2") {
        res.push_str(&format!("{}", etc2.as_str()));
    }
    if let Some(post) = caps.name("post") {
        res.push_str(caps.name("s_post").map(|m| m.as_str()).unwrap_or_default());
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!(
            "ranting::inflect_verb({noun}.subjective(), \"{}\", {is_pl}, {uc})",
            post.as_str()
        ));
    }
    Ok(res)
}

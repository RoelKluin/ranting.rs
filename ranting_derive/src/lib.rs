// (c) RoelKluin 2022 GPL v3
//! The `say!()` macro produces a String with for `Ranting` trait structs or enums
//! inflection within placeholders. Alongside the Ranting element, articles and verbs
//! can be included within the curly braces of a placeholder that are inflected
//! accordingly. Use a plural form of verbs if English.
//!
//! A placeholder to display a Ranting variable has this structure:
//! ```text
//! {[,^]?(article |verb )?([+-]|#var )?[':@~]?noun( verb):fmt}
//! ```
//!
//! With `,` and `^` lower- and uppercase are enforced, but a sentence start is assumed
//! to be uppercase or if an article or verb starts with uppercase.
//!
//! To pluralize use `+`, for a singular use `-`. If prependeded by `#var` where
//! `var` is an integer in scope, plurality is adapted to the count, singular if 1,
//! otherwise plural. A verb or article is inflected along with the specified or default
//! plurality. `These` or `those`, instead of an article, are converted to `this` or `that`
//! if the pronoun is singular.
//!
//! If a verb is included in the placeholder, the noun is assumed to be subject. By default
//! the name of a variable is displayed, but a pronoun with formatting markers:
//! * `:` - subject
//! * `@` - object
//! * `` ` `` - possesive
//! * `~` - adjective
//!
//! If a var or noun is prefixed with a question mark, e.g. `#?var`, inflection rules apply
//! accordingly, but the variable and the space following is not displayed.

mod ranting_impl;
mod english;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use lazy_static::lazy_static;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use ranting_impl::*;
use regex::{Captures, Regex};
use syn::parse::Parser;
use syn::{self, parse, parse_macro_input, DeriveInput, Error as SynError, Expr, ExprPath};
use english as language;


/// A wrapper for `return Ok(say!())`
///
/// # Examples
///
/// ```
/// #[derive(new)]
/// #[derive_ranting]
/// struct Named {}
///
/// fn question(harr: named, friends: Named, lad: Named) -> Result<String, String> {
///     ack!("{harr shall} {+:friends do} with {the drunken lad}?");
/// }
///
/// # fn main() {
/// let harr = Named::new("what", "it");
/// let friends = Named::new("crew", "we");
/// let lad = Named::new("sailor", "he");
///
/// assort_eq!(
///     question(harr, friends, lad),
///     Ok("What shall we do with the drunken sailor?".to_string())
/// );
/// # }
/// ```
#[proc_macro]
pub fn ack(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("return Ok(format!({lit}))").parse().unwrap(),
        Err(e) => e,
    }
}

/// A wrapper for `return Err(say!())`
///
/// # Examples
///
/// ```
/// #[derive(new)]
/// #[derive_ranting]
/// struct Named {}
///
/// fn sing(lad: Named) -> Result<String, String> {
///     // the verb part must be plural
///     nay!("Keel haul {@lad} till {:lad're} sober.");
/// }
///
/// # fn main() {
/// let lad = Named::new("sailor", "he");

/// assort_eq!(
///     sing(lad),
///     Err(" Keel haul him till he's sober.".to_string())
/// );
/// # }
/// ```
#[proc_macro]
pub fn nay(input: TokenStream) -> TokenStream {
    match do_say(input) {
        Ok(lit) => format!("return Err(format!({lit}))").parse().unwrap(),
        Err(e) => e,
    }
}

/// Like `format!()` but with inflection within placeholders for Ranting elements. Other elements
/// adhere to their Display or Debug traits.
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
    let options = RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
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
    // can duplicated because the n't eats the n
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
    let mut display_noun = true;
    let mut is_plain_placeholder = true;
    let mut is_pl = String::new();
    let mut nr = "";

    if let Some(plurality) = caps.name("plurality") {
        is_plain_placeholder = false;
        match plurality.as_str().split_at(1) {
            ("+", "") => is_pl.push_str("true"),
            ("-", "") => is_pl.push_str("false"),
            ("#", mut x) => {
                if let Some(nr) = x.strip_prefix('?') {
                    x = nr; // nr not assigned!
                } else {
                    nr = x;
                }
                if let Ok(u) = x.parse::<usize>() {
                    x = given
                        .get(u)
                        .ok_or(SynError::new(
                            Span::call_site().into(),
                            "#<nr> positional out of bounds",
                        ))?
                        .as_str();
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
        format!("'{}': missing noun", caps.get(0).unwrap().as_str()),
    ))?;

    if let Some(n) = noun.strip_prefix('?') {
        is_plain_placeholder = false;
        display_noun = false;
        noun = n;
    }

    // a positional.
    if let Ok(u) = noun.parse::<usize>() {
        noun = given
            .get(u)
            .ok_or(SynError::new(
                Span::call_site().into(),
                "<noun> positional out of bounds",
            ))?
            .as_str();
    }

    if is_pl.is_empty() {
        is_pl = format!("{noun}.is_plural()");
    }
    let mut res = caps
        .name("sentence")
        .map(|s| s.as_str().to_owned())
        .unwrap_or_default();

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
                "ranting::inflect_article({noun}.a_or_an(false), \"{p}\", {is_pl}, {uc})"
            ));
        } else {
            assert!(caps.name("post").is_none(), "verb before and after?");
            pos.push(format!(
                "ranting::inflect_verb({noun}.subjective(), \"{}\", {is_pl}, {uc})",
                p.as_str()
            ));
        }
        res.push_str(caps.name("s_pre").map(|m| m.as_str()).unwrap_or_default());
        uc = false;
    }

    if let Some(etc1) = caps.name("etc1") {
        is_plain_placeholder = false;
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
        res.push_str(caps.name("s_noun").map(|m| m.as_str()).unwrap_or_default());
        match caps
            .name("case")
            .and_then(|s| language::get_case_from_str(s.as_str()))
        {
            Some(case) => pos.push(format!(
                "ranting::inflect_{case}({noun}.subjective(), {is_pl}, {uc})"
            )),
            None if is_plain_placeholder && caps.name("etc2").is_none() && caps.name("post").is_none() => {
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
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!(
            "ranting::inflect_verb({noun}.subjective(), \"{}\", {is_pl}, {uc})",
            post.as_str()
        ));
    }
    Ok(res)
}

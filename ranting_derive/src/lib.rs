// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use lazy_static::lazy_static;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use ranting_impl::*;
use regex::{Captures, Match, Regex};
use std::collections::{hash_map::Entry, HashMap};
use std::default::Default;
use syn::{parse, parse_macro_input, Error as SynError, Expr, ExprPath};

/// Generates the `Ranting` trait implementation
/// Structs that receive this trait require a name and pronoun String.
/// and can be referenced in the say!() nay!() and ack!() macros.
#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn derive_ranting(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let options = RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    ranting_q(options, &input.ident).into()
}

/// The say!() macro produces a String, a bit similar to format!(), but with extended
/// formatting options for Ranting trait objects provided as arguments to say!().
///
/// Ranting trait objects as arguments to say!()  are displated as their name by
/// default, or by pronoun with the following formatting extensions:
///
/// `:s` gives a subject, `:o` an object, `:p` the possesive and `:a` the adjective
/// form of the pronoun. With a capital, e.g. `:S`, the pronoun form is capitalized.
///
/// There are also the `:m` or `:M` postfixes to display the plural form of the name.
///
/// when prepended with `a ` or `an `, this indefinite article is adapted to the name.
/// When capitalized this is preserved. Also `the`, `these` and `those` can occur before.
/// Ranting always uses the 1st plural form. `These` and `those` are converted to `this`
/// and `that` if the pronoun is singular.
///
/// A verb after, als o in 1st plural form, is also inflected to the pronoun's case. The
/// Ranting object enclosed before a verb is assumed to be the subject in the sentence.
///
/// Positional argument and numeric references are supported, but not named arguments,
/// currently.
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
struct SayFmt<'t> {
    article_or_so: Option<Match<'t>>,
    spaced_verb: Option<Match<'t>>,
    plurality: Option<Match<'t>>,
    case: char,
    named: bool,
    format: String,
    uc: bool,
}

impl<'t> SayFmt<'t> {
    fn from_caps(caps: &'t Captures, sentence_start: usize) -> Result<Self, SynError> {
        // case as in subject, object, possesive or adjective. Also name specifier.

        // default format!() formatters are allowed, preserved here
        let format = caps
            .name("format")
            .map(|s| s.as_str())
            .unwrap_or("")
            .to_string();

        let case = caps
            .name("case")
            .and_then(|s| s.as_str().chars().next())
            .unwrap_or_default();

        // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
        // 2) uc if article or so is or 3) the noun is first or after start or `. '
        let article_or_so = caps.name("article");

        let uc = match caps.name("uc").and_then(|s| s.as_str().chars().next()) {
            Some('^') => true,
            Some(',') => false,
            _ => {
                // or if article has uc or the noun is first or at new sentence
                article_or_so
                    .filter(|s| s.as_str().starts_with(|c: char| c.is_uppercase()))
                    .is_some()
                    | (caps.get(0).unwrap().start() == sentence_start)
            }
        };

        Ok(SayFmt {
            article_or_so,
            spaced_verb: caps.name("verb"),
            plurality: caps.name("plurality"),
            case,
            named: caps.name("named").is_some(),
            format,
            uc,
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
    lazy_static! {
        static ref RE: Regex = Regex::new(&format!(
            r"(?:[{{]{mode}{article}{noun}{verb}{fmt}[}}]|{period})",
            mode = r"(?P<uc>[,^])?(?P<plurality>[+-]|#\w+ )?(?P<named>\*)?(?P<case>[':@~]?)",
            article = r"(?:(?P<article>[Aa]n?|[Tt]h(?:e|[eo]se)) +)?",
            noun = r"(?P<noun>[\w-]+)",
            verb = r"(?P<verb>(?: [\w-]+)*?[' ][\w-]+)?",
            fmt = r"(?P<format>:[^}}]+)?",
            period = r"(?P<period>\. +)"
        ))
        .unwrap();
    }
    //eprintln!("{:?}", RE.to_string());
    let mut err = None;
    let mut sentence_start = 0;

    let mut positional: Vec<String> = vec![];

    lit = RE
        .replace_all(&lit, |caps: &Captures| {
            if let Some(new_sentence) = caps.name("period") {
                sentence_start = new_sentence.end();
                return new_sentence.as_str().to_string();
            }
            let mut expr = caps.name("noun").unwrap().as_str().to_string();
            if let Ok(u) = expr.parse::<usize>() {
                match given.get(u) {
                    Some(s) => expr = s.to_string(),
                    None => {
                        err = Some(SynError::new(
                            Span::call_site().into(),
                            "missing positional",
                        ));
                        return String::new();
                    }
                }
            }
            let sf = match SayFmt::from_caps(caps, sentence_start) {
                Ok(sf) => sf,
                Err(e) => {
                    err = Some(e);
                    return String::new();
                }
            };
            handle_param(sf, expr, &mut positional)
        })
        .to_string();

    if let Some(e) = err {
        return Err(e.into_compile_error().into());
    }

    if !positional.is_empty() {
        lit.push_str(", ");
        lit.push_str(&positional.iter().join(", "));
    }
    //eprintln!("{}", lit);
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
fn handle_param(sf: SayFmt, local: String, positional: &mut Vec<String>) -> String {
    let uc = sf.uc;
    let u = positional.len();
    let plurality = sf.plurality.map(|s| s.as_str());
    // numeric stay, only u increment if with article
    match sf.article_or_so {
        Some(article_or_so) => {
            let art = match article_or_so.as_str().to_lowercase().as_str() {
                "a" | "an" => match plurality {
                    Some("+") => format!("\"{}ome\"", if uc { 'S' } else { 's' }),
                    Some("-") => format!("{local}.a_or_an({uc})"),
                    Some(var) => {
                        let x = var.trim_start_matches('#');
                        format!(
                            r#"match {x} {{ 0 => "{}o", 1 => {local}.a_or_an({uc}), _ => "{}ome" }}"#,
                            if uc { 'N' } else { 'n' },
                            if uc { 'S' } else { 's' }
                        )
                    }
                    _ => format!(
                        r#"if {local}.is_plural() {{ "{}ome" }} else {{ {local}.a_or_an({uc}) }}"#,
                        if uc { 'S' } else { 's' }
                    ),
                },
                "these" => match plurality {
                    Some("+") => format!("\"{}hese\"", if uc { 'T' } else { 't' }),
                    Some("-") => format!("{}his", if uc { 'T' } else { 't' }),
                    Some(var) => {
                        let x = var.trim_start_matches('#');
                        format!(
                            r#"if {x} != 1 {{ "{0}hese" }} else {{ "{0}his" }}"#,
                            if uc { 'T' } else { 't' }
                        )
                    }
                    _ => format!(
                        r#"if {local}.is_plural() {{ "{0}hese" }} else {{ "{0}his" }}"#,
                        if uc { 'T' } else { 't' }
                    ),
                },
                "those" => match plurality {
                    Some("+") => format!("\"{}hose\"", if uc { 'T' } else { 't' }),
                    Some("-") => format!("{}hat", if uc { 'T' } else { 't' }),
                    Some(var) => {
                        let x = var.trim_start_matches('#');
                        format!(
                            r#"if {x} != 1 {{ "{0}hose" }} else {{ "{0}hat" }}"#,
                            if uc { 'T' } else { 't' }
                        )
                    }
                    _ => format!(
                        r#"if {local}.is_plural() {{ "{0}hose" }} else {{ "{0}hat" }}"#,
                        if uc { 'T' } else { 't' }
                    ),
                },
                "the" => format!("\"{}he\"", if uc { 'T' } else { 't' }),
                x => panic!("Unimplemented article {x}"),
            };
            positional.push(art);
            let reference = if sf.named {
                match plurality {
                    Some("+") => {
                        format!(
                            "if {local}.is_plural() {{ {local}.name({uc}) }} else {{ {local}.plural({uc}) }}"
                        )
                    }
                    Some("-") => {
                        format!(
                            "if {local}.is_plural() {{ {local}.singular({uc}) }} else {{ {local}.name({uc}) }}"
                        )
                    }
                    Some(var) => {
                        let x = var.trim_start_matches('#').to_string();
                        format!(
                            "if {x} != 1 {{ if {local}.is_plural() {{ {local}.singular({uc}) }} else {{ {local}.name({uc}) }} }} else {{ if {local}.is_plural() {{ {local}.name({uc}) }} else {{ {local}.plural({uc}) }} }}"
                        )
                    }
                    None => {
                        format!("{local}.name({uc})")
                    }
                }
            } else {
                match plurality {
                    Some("+") => {
                        format!(
                            r#"if {local}.is_plural() || {local}.subject(false) == "you" {{ {local}.subject({uc}) }} else if {local}.subject(true) == "I" {{ "{}e" }} else {{ "{}hey" }}"#,
                            if uc { 'W' } else { 'w' },
                            if uc { 'T' } else { 't' }
                        )
                    }
                    Some("-") => {
                        format!(
                            r#"if {local}.subject(false) == "you" || !{local}.is_plural() {{ {local}.subject({uc}) }} else if {local}.subject(true) == "We" {{ "I" }} else {{ "{}t" }}"#,
                            if uc { 'I' } else { 'i' } // assuming neutrum
                        )
                    }
                    Some(var) => {
                        let x = var.trim_start_matches('#').to_string();
                        format!(
                            r#"if {local}.subject(false) == "you" {{ {local}.subject({uc}) }} else if {x} != 1 {{ if {local}.is_plural() {{ {local}.subject({uc}) }} else if {local}.subject(true) == "I" {{ "{}e" }} else {{ "{}hey" }} }} else {{ if !{local}.is_plural() {{ {local}.subject({uc}) }} else if {local}.subject(true) == "We" {{ "I" }} else {{ "{}t" }} }}"#,
                            if uc { 'W' } else { 'w' },
                            if uc { 'T' } else { 't' },
                            if uc { 'I' } else { 'i' }
                        )
                    }
                    None => {
                        format!("{local}.name({uc})")
                    }
                }
            };
            positional.push(reference);

            if let Some(sv) = sf.spaced_verb.map(|s| s.as_str()) {
                positional.push(format!("{local}.verb(\"{sv}\")"));
                format!("{{{}}} {{{}{}}}{{{}}}", u, u + 1, sf.format, u + 2)
            } else {
                format!("{{{}}} {{{}{}}}", u, u + 1, sf.format)
            }
        }
        None => {
            if let Some(sv) = sf.spaced_verb.map(|s| s.as_str()) {
                match sf.case {
                    '<' => positional[u] = format!("\"{}here\"", if uc { 'T' } else { 't' }),
                    '*' => positional[u] = format!("{local}.name({uc})"),
                    /*'M' => {
                        positional[u] = format!("{local}.plural({uc})");
                        positional.push(format!("{local}.verb_for(\"{sv}\", \"they\")"));
                        return format!("{{{}{}}}{{{}}}", u, sf.format, u + 1);
                    },*/
                    '&' => panic!("Adjective before verb plurality? use {{&adj}} {{?obj {sv}}}"),
                    '!' | '\0' => positional[u] = format!("{local}.subject({uc})"),
                    x => panic!("{x}: Illegal format specifier before verb"),
                }
                positional.push(format!("{local}.verb(\"{sv}\")"));
                format!("{{{}{}}}{{{}}}", u, sf.format, u + 1)
            } else {
                positional[u] = match sf.case {
                    ':' => format!("{local}.subject({uc})"),
                    '@' => format!("{local}.object({uc})"),
                    '\'' => format!("{local}.possesive({uc})"),
                    '~' => format!("{local}.adjective({uc})"),
                    _ => local,
                };
                format!("{{{}{}}}", u, sf.format)
            }
        }
    }
}

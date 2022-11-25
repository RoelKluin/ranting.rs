// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use lazy_regex::regex;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use ranting_impl::*;
use regex::Captures;
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
struct SayFmt {
    article_or_so: Option<String>,
    spaced_verb: Option<String>,
    format: String,
    case: char,
    uc: bool,
}

impl SayFmt {
    fn from_caps(caps: &Captures, sentence_start: usize) -> Result<Self, SynError> {
        // uppercase if 1) an uc format is specified otherwise if not lc is specified
        // 2) uc if article or so is or 3) the noun is first or after start or `. '
        let mut uc = caps
            .get(1)
            .map(|a| a.as_str().starts_with(|c: char| c.is_uppercase()));

        // default format!() formatters are allowed, preserved here
        let mut format = caps.get(4).map(|s| s.as_str()).unwrap_or("");

        // case as in subject, object, possesive or adjective. Also name specifier.
        let mut case = Default::default();

        if let Some(fmt) = format.strip_prefix(':') {
            if fmt.is_empty() {
                return Err(SynError::new(
                    Span::call_site().into(),
                    "cannot end format with ':'",
                ));
            }
            let (c, fmt) = fmt.split_at(1);
            let c = c.chars().next().unwrap();
            if "SOPAMND".contains(c.to_ascii_uppercase()) {
                format = fmt;
                let _ = uc.insert(c.is_uppercase());
                case = c.to_ascii_uppercase();
            }
        }
        Ok(SayFmt {
            article_or_so: caps.get(1).map(|a| a.as_str().to_string()),
            spaced_verb: caps.get(3).map(|s| s.as_str().to_string()),
            format: format.to_string(),
            case,
            // sentence_start + 1 because noun is enclosed in curly braces, e.g. `{self are}'
            uc: uc.unwrap_or(caps.get(2).unwrap().start() == sentence_start + 2),
        })
    }
}

fn do_say(input: TokenStream) -> Result<String, TokenStream> {
    let mut token_it = input.into_iter();
    let mut lit = match lit_first(token_it.next()) {
        Ok(lit) => format!("{lit}"),
        Err(e) => return Err(e.into_compile_error().into()),
    };
    // Expressions can have side effects, those must be assigned to a local.
    let mut positional: Vec<String> = vec![];

    // regex to capture the placholders or sentence ends
    let re = regex!(
        r"(?:\{(?:([Aa]n?|[Tt]h(?:e|[eo]se)|#\w+) )?(\w+)([' ]\w+| \w+'\w+)?(:[^}]*)?\}|(\. +))"
    );
    let mut lookup: Vec<usize> = vec![];
    let mut err = None;
    let mut sentence_start = 0;

    let mut named_params: HashMap<String, Result<usize, String>> = HashMap::new();

    lit = re
        .replace_all(&lit, |caps: &Captures| {
            if let Some(new_sentence) = caps.get(5) {
                sentence_start = new_sentence.end() - 1;
                return new_sentence.as_str().to_string();
            }
            let noun_match = caps.get(2).unwrap();
            let noun_or_pos = noun_match.as_str().to_string();
            let sf = match SayFmt::from_caps(caps, sentence_start) {
                Ok(sf) => sf,
                Err(e) => {
                    err = Some(e);
                    return String::new();
                }
            };
            let u_offs = positional.len() + named_params.len();
            if let Ok(u) = noun_or_pos.parse::<usize>() {
                let expr = match lookup.get(u) {
                    Some(&u) => {
                        // reused may already be a function call
                        let clean = positional[u]
                            .split_once('.')
                            .map(|x| x.0)
                            .unwrap_or(positional[u].as_str())
                            .to_string();
                        positional.push(clean.to_string());
                        clean
                    }
                    None => match comma_next(&mut token_it, &mut positional) {
                        Ok(expr) => {
                            lookup.push(u_offs + u);
                            expr
                        }
                        Err(e) => {
                            err = Some(e);
                            return String::new();
                        }
                    },
                };
                handle_param(sf, expr, &mut positional, u_offs)
            } else {
                let expr;
                match named_params.entry(noun_or_pos.to_string()) {
                    Entry::Occupied(mut occ_ent) => {
                        if let Ok(n) = occ_ent.get() {
                            // expression already encountered.
                            expr = positional[*n].to_string();
                        } else {
                            // ok, but we need to set to the offsetted u and add positional
                            expr = occ_ent.insert(Ok(u_offs)).unwrap_err();
                            positional.push(expr.to_string());
                        }
                    }
                    Entry::Vacant(vac_ent) => {
                        // a local with this name, make it a positional
                        expr = vac_ent.key().to_string();
                        positional.push(expr.to_string());
                    }
                }
                handle_param(sf, expr, &mut positional, u_offs)
            }
        })
        .to_string();

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

fn comma_next(
    it: &mut dyn Iterator<Item = TokenTree>,
    positional: &mut Vec<String>,
) -> Result<String, SynError> {
    let t: TokenTree = it
        .next()
        .ok_or_else(|| SynError::new(Span::call_site().into(), "missing positional"))?;

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
        Expr::Path(ExprPath { path, .. }) => {
            let p = path.to_token_stream();
            positional.push(p.to_string());
            Ok(p.to_string())
        }
        e => Err(SynError::new(
            Span::call_site().into(),
            format!("unexpected expression: {e:?}"),
        )),
    }
}

// arguments become positionals
fn handle_param(sf: SayFmt, local: String, positional: &mut Vec<String>, u: usize) -> String {
    let uc = sf.uc;
    // numeric stay, only u increment if with article
    match sf.article_or_so {
        Some(article_or_so) => {
            if let Some(nr_var) = article_or_so.strip_prefix('#') {
                positional[u] = format!(
                    "if {nr_var} != 1 {{ {local}.plural({uc}) }} else {{ {local}.name({uc}) }}"
                );
                positional.push(nr_var.to_string());
                return format!("{{{}}} {{{}{}}}", u + 1, u, sf.format);
            }
            match article_or_so.to_ascii_lowercase().as_str() {
                "a" | "an" => positional.push(format!(
                    r#"if {local}.is_plural() {{ "some" }} else {{ {local}.a_or_an({uc}) }}"#
                )),
                "these" if uc => positional.push(format!(
                    r#"if {local}.is_plural() {{ "These" }} else {{ "This" }}"#
                )),
                "these" => positional.push(format!(
                    r#"if {local}.is_plural() {{ "these" }} else {{ "this" }}"#
                )),
                "those" if uc => positional.push(format!(
                    r#"if {local}.is_plural() {{ "Those" }} else {{ "That" }}"#
                )),
                "those" => positional.push(format!(
                    r#"if {local}.is_plural() {{ "those" }} else {{ "that" }}"#
                )),
                "the" if uc => positional.push("\"The\"".to_string()),
                "the" => positional.push("\"the\"".to_string()),
                x => panic!("Unexpected article {x}, this is a bug."),
            }
            if sf.case == 'M' {
                positional[u] = format!("{local}.plural(false)");
            }
            if let Some(sv) = sf.spaced_verb {
                positional.push(format!("{local}.verb(\"{sv}\")"));
                format!("{{{}}} {{{}{}}}{{{}}}", u + 1, u, sf.format, u + 2)
            } else {
                format!("{{{}}} {{{}{}}}", u + 1, u, sf.format)
            }
        }
        None => {
            if let Some(sv) = sf.spaced_verb {
                if sf.case == 'D' {
                    positional[u] = format!("\"{}here\"", if uc { 'T' } else { 't' });
                } else {
                    positional[u] = format!("{local}.subject({uc})");
                }
                positional.push(format!("{local}.verb(\"{sv}\")"));
                format!("{{{}{}}}{{{}}}", u, sf.format, u + 1)
            } else {
                match sf.case {
                    'S' => positional[u] = format!("{local}.subject({uc})"),
                    'O' => positional[u] = format!("{local}.object({uc})"),
                    'P' => positional[u] = format!("{local}.possesive({uc})"),
                    'A' => positional[u] = format!("{local}.adjective({uc})"),
                    'M' => positional[u] = format!("{local}.plural({uc})"),
                    'N' => positional[u] = format!("{local}.name({uc})"),
                    'D' => positional[u] = format!("\"{}here\"", if uc { 'T' } else { 't' }),
                    _ => positional[u] = local,
                }
                format!("{{{}{}}}", u, sf.format)
            }
        }
    }
}

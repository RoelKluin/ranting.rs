// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use lazy_static::lazy_static;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use ranting_impl::*;
use regex::{Captures, Match, Regex};
use std::default::Default;
use syn::{parse, parse_macro_input, Error as SynError, Expr, ExprPath};

/// Generates the `Ranting` trait implementation
/// Structs that receive this trait require a name and subjective String.
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
/// Ranting trait objects as arguments to say!()  are displayed as their name by
/// default, or by a pronoun with formatting markers:
///
/// `:` gives a subject, `@` an object, `'` a possesive and `~` an adjective form of the
/// pronoun.
///
/// when prepended with `a ` or `an `, this indefinite article is adapted to the name.
/// When capitalized this is preserved. Also `the`, `these` and `those` can occur before.
/// Ranting always uses the 1st plural form. `These` and `those` are converted to `this`
/// and `that` if the subjective is singular.
///
/// A verb after, als o in 1st plural form, is also inflected to the subjective's case. The
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
    pre: Option<Match<'t>>,
    post: Option<Match<'t>>,
    plurality: Option<Match<'t>>,
    case: char,
    uc: bool,
    hidden_noun: bool,
}

impl<'t> SayFmt<'t> {
    fn from_caps(
        caps: &'t Captures,
        sentence_start: usize,
        hidden_noun: bool,
    ) -> Result<Self, SynError> {
        // case as in subject, object, possesive or adjective. Also name specifier.

        let case = caps
            .name("case")
            .and_then(|s| s.as_str().chars().next())
            .unwrap_or_default();

        // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
        // 2) uc if article or so is or 3) the noun is first or after start or `. '
        let pre = caps.name("pre");

        let uc = match caps.name("uc").and_then(|s| s.as_str().chars().next()) {
            Some('^') => true,
            Some(',') => false,
            _ => {
                // or if article has uc or the noun is first or at new sentence
                pre.filter(|s| s.as_str().starts_with(|c: char| c.is_uppercase()))
                    .is_some()
                    | (caps.get(0).unwrap().start() == sentence_start)
            }
        };

        Ok(SayFmt {
            pre,
            post: caps.name("post"),
            plurality: caps.name("plurality"),
            case,
            uc,
            hidden_noun,
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
            r"(?:[{{]{ulc}(?P<pre>(?:{art}|{pre_verb}){etc})?{mode}{noun}{post}[}}]|{period})",
            ulc = r"(?P<uc>[,^])?",
            art = "[Aa]n |[Ss]ome |[Tt]h(?:e|[eo]se) ",
            pre_verb = "'re |may |(?:sha|wi)ll |(?:(?:a|we)re|do|ca|ha(?:d|ve)|(?:[cw]|sh)ould|must|might)(?:n't)? ",
            etc = r"(?:[\w-]+ )*?",
            mode = r"(?P<plurality>[+-]|#\??\w+ )?(?P<case>[':@~]?)",
            noun = r"(?P<noun>\??[\w-]+)",
            post = r"(?P<post>(?: [\w-]+)*?[' ][\w-]+)?",
            period = r"(?P<period>\. +)"
        ))
        .unwrap();
    }
    //eprintln!("{:?}", RE.to_string());
    let mut err = None;
    let mut sentence_start = 0;

    let mut positional: Vec<String> = vec![];
    let original = lit.to_string();

    lit = RE
        .replace_all(&lit, |caps: &Captures| {
            if let Some(new_sentence) = caps.name("period") {
                sentence_start = new_sentence.end();
                return new_sentence.as_str().to_string();
            }
            let mut hidden_noun = false;
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
            } else if let Some(var) = expr.strip_prefix('?').map(|s| s.to_owned()) {
                expr = var; // '?' indicates unnamed noun, so not pushed
                hidden_noun = true
            }
            let sf = match SayFmt::from_caps(caps, sentence_start, hidden_noun) {
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
    eprintln!("{}\n {}", original, lit);
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
        '\'' => Some("possesive"),
        '~' => Some("adjective"),
        _ => None,
    }
}

// arguments become positionals
fn handle_param(sf: SayFmt, var: String, pos: &mut Vec<String>) -> String {
    let mut nr = "";
    let mut res = String::new();
    let mut uc = sf.uc;
    let mut pl_it = sf.plurality.map(|s| s.as_str()).unwrap_or_default().chars();
    let plurality = pl_it.next();

    let is_pl = match plurality {
        Some('+') => format!("true"),
        Some('-') => format!("false"),
        Some('#') => {
            nr = pl_it.as_str();
            if nr.starts_with('?') {
                let test = format!("{} != 1", nr.trim_start_matches('?'));
                nr = "";
                test
            } else {
                format!("{nr} != 1")
            }
        }
        None => format!("{var}.is_plural()"),
        Some(a) => panic!("Unrecognized plurality '{a}{}'", pl_it.as_str()),
    };

    if let Some(p) = sf.pre.map(|a| a.as_str().trim_end().to_lowercase()) {
        res.push_str(&format!("{{{}}}", pos.len()));
        match p.as_str() {
            "some" | "a" | "an" | "the" | "these" | "those" => pos.push(format!(
                "ranting::inflect_article({var}.a_or_an({uc}), \"{p}\", {is_pl}, {uc})"
            )),
            verb => {
                assert!(sf.post.is_none(), "verb before and after?");
                pos.push(format!(
                    "ranting::inflect_verb({var}.subjective(), \"{verb}\", {is_pl}, {uc})"
                ))
            }
        }
        uc = false;
    }
    if !nr.is_empty() {
        if !res.is_empty() {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!("{nr}"));
    }
    if !sf.hidden_noun {
        if !res.is_empty() {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        match get_case_from_char(sf.case) {
            Some(case) => pos.push(format!("ranting::{case}({var}.subjective(), {uc})")),
            None if plurality.is_none() => pos.push(format!("{var}")), // for non-Ranting variables
            None => pos.push(format!(
                "ranting::inflect_noun({var}.name({uc}), {var}.is_plural(), {is_pl}, {uc})"
            )),
        }
        uc = false;
    }
    if let Some(sv) = sf.post.map(|s| s.as_str()) {
        if !res.is_empty() && !sv.starts_with('\'') {
            res.push(' ')
        }
        res.push_str(&format!("{{{}}}", pos.len()));
        pos.push(format!(
            "ranting::inflect_verb({var}.subjective(), \"{sv}\", {is_pl}, {uc})"
        ));
    }
    res
}

// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

use darling::{FromDeriveInput, ToTokens};
use itertools::Itertools;
use proc_macro::{self, Literal, Span, TokenStream, TokenTree};
use ranting_impl::*;

use lazy_regex::regex;
use regex::Captures;
use std::collections::{hash_map::Entry, HashMap};
use syn::{parse, parse_macro_input, Error as SynError, Expr, ExprPath};

#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn derive_ranting(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    let options = RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    ranting_q(options, &input.ident).into()
}

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

struct SayFmt {
    article_or_so: Option<String>,
    spaced_verb: Option<String>,
    formatting: String,
}

impl SayFmt {
    fn is_subject(&self) -> bool {
        self.spaced_verb.is_some()
            || self.formatting.starts_with(":s")
            || self.formatting.starts_with(":S")
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

    let re = regex!(
        r"(?:\{(?:([Aa]n?|[Tt]h(?:e|is|at)|(?:\w+[ :])?[Oo]ur) )?(\w+)([' ]\w+| \w+'\w+)?(:[^}]*)?\}|(\. ))"
    );
    let mut lookup: Vec<usize> = vec![];
    let mut err = None;
    let mut last_pos = 0;
    let mut subject = usize::MAX;

    let mut named_params: HashMap<String, Result<usize, String>> = HashMap::new();

    lit = re
        .replace_all(&lit, |caps: &Captures| {
            if let Some(new_sentence) = caps.get(5) {
                if subject != usize::MAX {
                    for i in last_pos..positional.len() {
                        if positional[i].contains("{subject}.possesive(") {
                            // the subject is usually var.subject(true/false)
                            positional[i] = positional[i].replace(
                                "{subject}",
                                positional[subject]
                                    .split_once('.')
                                    .map(|x| x.0)
                                    .unwrap_or(positional[subject].as_str()),
                            );
                        }
                    }
                }
                last_pos = positional.len();
                subject = usize::MAX;
                return new_sentence.as_str().to_string();
            }
            let sf = SayFmt {
                article_or_so: caps.get(1).map(|a| a.as_str().to_string()),
                spaced_verb: caps.get(3).map(|s| s.as_str().to_string()),
                formatting: caps.get(4).map(|s| s.as_str()).unwrap_or("").to_string(),
            };
            let noun_or_pos = caps.get(2).unwrap().as_str().to_string();
            let u_offs = positional.len() + named_params.len();
            if let Ok(u) = noun_or_pos.parse::<usize>() {
                let expr = match lookup.get(u) {
                    Some(&u) => positional[u].to_string(),
                    None => match comma_next(&mut token_it, &mut positional) {
                        Ok(expr) => {
                            lookup.push(u_offs + u);
                            if sf.is_subject() {
                                subject = u;
                            }
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
                            if sf.spaced_verb.is_some() {
                                subject = *n;
                            }
                        } else {
                            // ok, but we need to set to the offsetted u and add positional
                            expr = occ_ent.insert(Ok(u_offs)).unwrap_err();
                            if sf.is_subject() {
                                subject = positional.len();
                            }
                            positional.push(expr.to_string());
                        }
                    }
                    Entry::Vacant(vac_ent) => {
                        // a local with this name, make it a positional
                        expr = vac_ent.key().to_string();
                        if sf.spaced_verb.is_some() {
                            subject = positional.len();
                        }
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
        if subject != usize::MAX {
            for i in last_pos..positional.len() {
                if positional[i].contains("{subject}.possesive(") {
                    // the subject is usually var.subject(true/false)
                    positional[i] = positional[i].replace(
                        "{subject}",
                        positional[subject]
                            .split_once('.')
                            .map(|x| x.0)
                            .unwrap_or(positional[subject].as_str()),
                    );
                }
            }
        }
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
    // numeric stay, only u increment if with article
    match sf.article_or_so {
        Some(article_or_so) => {
            if article_or_so.starts_with('A') {
                let a = format!(r#"if {local}.a_or_an() == "a" {{"A"}} else {{"An"}}"#);
                positional.push(a);
            } else if article_or_so.starts_with('a') {
                positional.push(format!("{local}.a_or_an()"));
            } else if article_or_so.ends_with("Our") {
                if let Some((obj, _)) = article_or_so.split_once(' ') {
                    positional.push(format!("{obj}.object(true)"));
                    positional.push(format!("{obj}.possesive(false)"));
                    return format!("{{{}}} {{{}}} {{{}{}}}", u + 1, u + 2, u, sf.formatting);
                } else if let Some((obj, _)) = article_or_so.split_once(':') {
                    positional.push(format!("{obj}.possesive(true)"));
                } else {
                    positional.push("{subject}.possesive(true)".to_string());
                }
            } else if article_or_so.ends_with("our") {
                if let Some((obj, _)) = article_or_so.split_once(' ') {
                    positional.push(format!("{obj}.object(false)"));
                    positional.push(format!("{obj}.possesive(false)"));
                    return format!("{{{}}} {{{}}} {{{}{}}}", u + 1, u + 2, u, sf.formatting);
                } else if let Some((obj, _)) = article_or_so.split_once(':') {
                    positional.push(format!("{obj}.possesive(false)"));
                } else {
                    positional.push("{subject}.possesive(false)".to_string());
                }
            } else {
                positional.push(format!(r#""{article_or_so}""#));
            }
            if let Some(sv) = sf.spaced_verb {
                positional.push(format!("{local}.verb(\"{sv}\")"));
                format!("{{{}}} {{{}{}}}{{{}}}", u + 1, u, sf.formatting, u + 2)
            } else {
                format!("{{{}}} {{{}{}}}", u + 1, u, sf.formatting)
            }
        }
        None => {
            if let Some(sv) = sf.spaced_verb {
                let formatting = if let Some(formatting) = sf.formatting.strip_prefix(":S") {
                    positional[u] = format!("{local}.subject(true)");
                    formatting
                } else {
                    positional[u] = format!("{local}.subject(false)");
                    sf.formatting.trim_end_matches(":s")
                };
                positional.push(format!("{local}.verb(\"{sv}\")"));
                format!("{{{}{}}}{{{}}}", u, formatting, u + 1)
            } else if let Some(formatting) = sf.formatting.strip_prefix(":o") {
                positional[u] = format!("{local}.object(false)");
                format!("{{{}{}}}", u, formatting)
            } else if let Some(formatting) = sf.formatting.strip_prefix(":O") {
                positional[u] = format!("{local}.object(true)");
                format!("{{{}{}}}", u, formatting)
            } else if let Some(formatting) = sf.formatting.strip_prefix(":p") {
                positional[u] = format!("{local}.possesive(false)");
                format!("{{{}{}}}", u, formatting)
            } else if let Some(formatting) = sf.formatting.strip_prefix(":P") {
                positional[u] = format!("{local}.possesive(true)");
                format!("{{{}{}}}", u, formatting)
            } else if let Some(formatting) = sf.formatting.strip_prefix(":a") {
                positional[u] = format!("{local}.adjective(flase)");
                format!("{{{}{}}}", u, formatting)
            } else if let Some(formatting) = sf.formatting.strip_prefix(":A") {
                positional[u] = format!("{local}.adjective(true)");
                format!("{{{}{}}}", u, formatting)
            } else {
                format!("{{{}{}}}", u, sf.formatting)
            }
        }
    }
}

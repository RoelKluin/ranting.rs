// (c) RoelKluin 2022 GPL v3

mod heed;
mod language;
mod ranting_impl;
mod str_lit;

use language::adjective;
use ranting_core::grammar as lang;
use ranting_core::verb_conjugate;

use darling::{FromDeriveInput, ToTokens};
use itertools::join;
use proc_macro::{self, TokenStream as TokenStream1};
use proc_macro2::{Punct, Spacing, Span, TokenStream};
use quote::quote;
use ranting_impl::*;
use regex::{Captures, Regex};
use std::collections::HashMap;
use std::sync::LazyLock;
use str_lit::*;
use syn::{
    self, Error, Expr, Ident, Token,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
};

#[proc_macro]
pub fn ack(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as Say);
    let tokens: TokenStream = parse_quote!(Ok(#output));
    tokens.into()
}

#[proc_macro]
pub fn nay(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as Say);
    let tokens: TokenStream = parse_quote!(Err(#output));
    tokens.into()
}

#[proc_macro]
pub fn say(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as Say);
    let tokens: TokenStream = parse_quote!(#output);
    tokens.into()
}

/// say_with!(context, "fmt", args...) is like say!() but resolves `<`, `=`, `>`,
/// `<=`, `%`, `<%` tense markers against a runtime `NarrationContext` (falling
/// back to the marker's own default tense when the context doesn't override it).
/// The context also carries `narration_person`, `register`, and `dialect` — the
/// latter two are inert in this crate and only reach `Ranting::inflect_*_custom_with_context`
/// hooks for implementations that choose to read them (see `ranting::Ranting`).
#[proc_macro]
pub fn say_with(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as SayWith);
    let tokens: TokenStream = parse_quote!(#output);
    tokens.into()
}

/// ask!() is a macro that takes an audience and a question string.
#[proc_macro]
pub fn ask(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as Ask);
    let tokens: TokenStream = parse_quote!(#output);
    tokens.into()
}

/// heed!(template, input) matches `input` against `template` — literal
/// words plus `{name}` (single token), `{name...}` (greedy, until the next
/// literal or end of input), and `{$name}` (digits, parsed to u64) captures
/// — returning `None` on no match, or the captured value(s) on match: bare
/// for 0/1 captures, a tuple for 2+, matching say!()'s positional style.
#[proc_macro]
pub fn heed(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as heed::Heed);
    let tokens: TokenStream = parse_quote!(#output);
    tokens.into()
}

fn parse_str_params(
    lit: StrLit,
    params_in: HashMap<String, Expr>,
    runtime_tense: bool,
) -> syn::Result<(String, Vec<Expr>)> {
    static PH: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(lang::PH_START).expect("valid placeholder regex"));
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
                        pre.map_or("", |s| s.as_str()).to_string() + "{" + len.as_str() + fmt + "}"
                    }
                    Err(s) => {
                        err = Some((plain.start(), plain.end(), s));
                        String::new()
                    }
                }
            } else {
                let ranting = caps.name("ranting").unwrap();
                // Hand-written tokenizer (ROADMAP.md Phase 4 item 6) replaces the old
                // `PHE.is_match(...)` + `PHE.replace(...)` pair -- `PH_EXT` is fully
                // anchored (`^...$`), so it only ever produced one whole-string match
                // or none at all; `ranting_core::ph_ext::parse` mirrors that directly
                // instead of going through a regex replace-closure, and returns a
                // precise error span/message on failure instead of the old blanket
                // "Error in placeholder".
                let parsed = match ranting_core::ph_ext::parse(ranting.as_str()) {
                    Ok(parsed) => parsed,
                    Err(e) => {
                        let offs = ranting.start();
                        err = Some((e.start + offs, e.end + offs, e.message));
                        return String::new();
                    }
                };
                let at_sentence_start = pre
                    .filter(|m| m.start() == 0 || m.as_str().starts_with(['.', '?', '!']))
                    .is_some();
                let replaced = match handle_param(
                    &parsed,
                    &params_in,
                    &mut params,
                    at_sentence_start,
                    fmt,
                    runtime_tense,
                ) {
                    Ok(s) => s,
                    Err((start, end, msg)) => {
                        let offs = ranting.start();
                        err = Some((start + offs, end + offs, msg));
                        String::new()
                    }
                };
                pre.map_or("", |s| s.as_str()).to_string() + &replaced
            }
        })
        .to_string();
    match err {
        Some((start, end, msg)) => Err(src.slice(start..end).error(msg.as_str())),
        None => Ok((lit_str, params)),
    }
}

/// after parsing, Say basicly contains the format!() litteral string and its (optional) parameters
struct Say {
    lit_str: String,
    params: Vec<Expr>,
}

/// say_with!(context, "fmt", args...): like Say, but placeholders with tense
/// markers bake the base verb instead of a compile-time-conjugated form, so
/// they can be resolved at runtime against `context: NarrationContext`.
struct SayWith {
    context: Expr,
    say: Say,
}

impl Parse for SayWith {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing context expression"));
        }
        let context = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;

        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing format string"));
        }
        let lit = input.parse::<StrLit>()?;
        let params_in = parse_params(input)?;
        let (lit_str, params) = parse_str_params(lit, params_in, true)?;
        Ok(SayWith {
            context,
            say: Say { lit_str, params },
        })
    }
}

impl ToTokens for SayWith {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let context = &self.context;
        let say = &self.say;
        *tokens = parse_quote! {{
            let __ranting_narration_ctx: ranting::NarrationContext = #context;
            #say
        }};
    }
}

fn ref_expr_ranting_trait(ref_expr: TokenStream) -> TokenStream {
    parse_quote! {
        impl Ranting for #ref_expr {
            fn name(&self, uc: bool) -> String {
                (**self).name(uc)
            }
            fn subjective(&self) -> &str {
                (**self).subjective()
            }
            fn is_plural(&self) -> bool {
                (**self).is_plural()
            }
            fn inflect(&self, as_plural: bool, uc: bool) -> String {
                (**self).inflect(as_plural, uc)
            }
            fn skip_article(&self) -> bool {
                (**self).skip_article()
            }
        }
    }
}

#[proc_macro]
pub fn boxed_ranting_trait(input: TokenStream1) -> TokenStream1 {
    let trait_name = parse_macro_input!(input as Expr);
    ref_expr_ranting_trait(parse_quote!(&'_ dyn #trait_name)).into()
}

#[proc_macro]
pub fn ref_ranting_trait(input: TokenStream1) -> TokenStream1 {
    let trait_name = parse_macro_input!(input as Expr);
    ref_expr_ranting_trait(parse_quote!(Box<dyn #trait_name>)).into()
}

struct Ask {
    speaker: Expr,
    audience: Expr,
    question: String,
    params: Vec<Expr>,
}

impl syn::parse::Parse for Ask {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing format string"));
        }
        let speaker = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;

        let audience = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;

        let lit = input.parse::<StrLit>()?;
        let params_in = parse_params(input)?;
        let (question, params) = parse_str_params(lit, params_in, false)?;
        Ok(Ask {
            speaker,
            audience,
            question,
            params,
        })
    }
}

/// Construct the format!() macro call. Print result with `--features debug`
impl ToTokens for Ask {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lit = self.question.as_str();
        let lit: TokenStream = parse_quote!(#lit);

        let mut macro_tokens = vec![lit.into_token_stream()];

        // Iterate over parameters and separate them with commas.
        for param in self.params.iter() {
            macro_tokens.push(Punct::new(',', Spacing::Alone).into_token_stream());
            macro_tokens.push(param.into_token_stream());
        }

        let final_macro_tokens: TokenStream = macro_tokens
            .iter()
            .map(|t| {
                quote! {
                    #t
                }
            })
            .collect();
        let speaker = &self.speaker;
        let audience = &self.audience;

        *tokens = parse_quote!(#audience.answer(#speaker, format!(#final_macro_tokens)));
        #[cfg(feature = "debug")]
        eprintln!("{}", tokens.to_string());
    }
}

/// Derives `Ranting` trait implementation and enables inflection within `say!()` placeholders.
/// Implies `#[derive(Ranting)]` and includes `name` and `subject` in structs.
/// For an enum `"it"` and the variant's name are assumed.
///
/// # Subject Pronouns
///
/// The `subject` attribute specifies which pronoun to use: I, you, thou, he, she, it, we, ye, or they.
/// All pronouns are fully supported, including singular they for gender-neutral language.
///
/// # Examples
///
/// Using singular they for an individual with gender-neutral pronouns:
///
/// ```rust
/// # use ranting::say;
/// # use ranting_derive::derive_ranting;
/// #[derive_ranting]
/// #[ranting(subject = "they", name = "Alex")]
/// struct Person {}
///
/// # fn main() {
/// let alex = Person {};
/// assert_eq!(
///     say!("{=alex are} a wonderful colleague."),
///     "They are a wonderful colleague.".to_string()
/// );
/// # }
/// ```
#[proc_macro_attribute]
pub fn derive_ranting(_args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let mut ast = parse_macro_input!(input as syn::DeriveInput);
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
                #[derive(ranting::rant_strum_macros::Display, ranting_derive::Ranting)]
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
    let input = parse_macro_input!(input);
    let mut is_enum = false;
    let options = RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    if let syn::Data::Enum(_) = &input.data {
        is_enum = true;
    }
    ranting_q(options, is_enum, &input.ident).into()
}

fn parse_params(input: ParseStream) -> syn::Result<HashMap<String, Expr>> {
    let mut params_in = HashMap::new();
    let mut positional_index = 0;

    if input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
        while !input.is_empty() {
            if input.peek(Ident) && input.peek2(Token![=]) {
                // named argument
                let ident = input.parse::<Ident>()?;
                input.parse::<Token![=]>()?;
                let expr = input.parse::<Expr>()?;

                params_in.insert(ident.to_string(), expr);
            } else {
                // positional
                let expr = input.parse::<Expr>()?;

                params_in.insert(positional_index.to_string(), expr);
                positional_index += 1;
            }

            if input.is_empty() {
                break;
            }
            input.parse::<Token![,]>()?;
        }
    }
    Ok(params_in)
}

/// Split placeholders in multiple and extend params accordingly.
impl Parse for Say {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing format string"));
        }
        let lit = input.parse::<StrLit>()?;
        let params_in = parse_params(input)?;

        let (lit_str, params) = parse_str_params(lit, params_in, false)?;
        Ok(Say { lit_str, params })
    }
}

/// Construct the format!() macro call. Print result with `--features debug`
impl ToTokens for Say {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lit = self.lit_str.as_str();
        let lit: TokenStream = parse_quote!(#lit);

        let mut macro_tokens = vec![lit.into_token_stream()];

        // Iterate over parameters and separate them with commas.
        for param in self.params.iter() {
            macro_tokens.push(Punct::new(',', Spacing::Alone).into_token_stream());
            macro_tokens.push(param.into_token_stream());
        }

        let final_macro_tokens: TokenStream = macro_tokens
            .iter()
            .map(|t| {
                quote! {
                    #t
                }
            })
            .collect();

        let format_call: TokenStream = parse_quote!(format!(#final_macro_tokens));
        #[cfg(feature = "debug")]
        eprintln!("{}", format_call.to_string());
        tokens.extend(format_call);
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
fn get_opt_num_ph_expr(p: &str, given: &HashMap<String, Expr>) -> Result<Expr, String> {
    match p.parse::<String>() {
        Err(_) => Ok(path_from(p)),
        Ok(s) => match given.get(&s) {
            Some(e) => Ok(e.clone()),
            None => match s.parse::<usize>() {
                Ok(u) => Err(format!(
                    "positional argument at index {u} not provided (only {} argument(s) given)",
                    given
                        .iter()
                        .filter(|(k, _)| k.parse::<usize>().is_ok())
                        .count()
                )),
                Err(_) => {
                    // Not a number and not in given arguments - assume it's a variable from local scope
                    Ok(path_from(&s))
                }
            },
        },
    }
}

fn split_at_find_start(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.rfind(fun).map(|u| s.split_at(u + 1))
}

// Placeholder parts are examined, added are replacements known at compile time,
// or placeholders are split in many and positionals are added.
fn handle_param(
    caps: &ranting_core::ph_ext::PhExtMatch,
    given: &HashMap<String, Expr>,
    pos: &mut Vec<Expr>,
    at_sentence_start: bool,
    orig_fmt: &str,
    runtime_tense: bool,
) -> Result<String, (usize, usize, String)> {
    static POSS: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^((?:.*?\s+)?`)(\w+)\b(.*)$").unwrap());
    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let pre_cap = caps.name("pre");
    let pre_s = pre_cap.map_or(0, |m| m.start());
    let pre_e = pre_cap.map_or(0, |m| m.end());
    let mut uc = if let Some(m) = caps.name("uc") {
        m.as_str() == "^"
    } else {
        // or if article has uc or the noun is first or at new sentence
        at_sentence_start
            || pre_cap
                .filter(|s| {
                    s.as_str()
                        .trim_start_matches('?')
                        .starts_with(|c: char| c.is_uppercase())
                })
                .is_some()
    };

    let mut pre = pre_cap.map_or("", |m| m.as_str());
    let (mut nr, nr_s, nr_e) = caps
        .name("nr")
        .map_or(("", 0, 0), |m| (m.as_str(), m.start(), m.end()));

    let case = caps.name("case").map_or("", |m| m.as_str());
    let noun = caps.name("noun").unwrap();
    let post = caps.name("post").map_or("", |m| m.as_str());

    // Classify the post-noun word (if any) into a typed `ranting_core::placeholder::PostSpec`
    // variant here, at compile time -- see that module's docs for why: this is the part of
    // Phase 4 item 3 that replaces the old `~TENSE~MARKER:CONJUGATED` / `~DEGREE~WORD[:TRAILING]`
    // string sentinels (which used to be folded into a single `&str` and re-parsed at runtime
    // via strip_prefix/split_once) with an enum baked directly into the generated call.
    let post_span = caps.name("post");
    let post_expr: TokenStream = if post.is_empty() {
        quote!(ranting::placeholder::PostSpec::None)
    } else if post == "'" || post == "'s" {
        quote!(ranting::placeholder::PostSpec::PossessiveS)
    } else {
        let post_trimmed = post.trim_start();
        // Extract marker run: take all leading <, =, >, %, ! characters
        let marker_end = post_trimmed
            .chars()
            .take_while(|c| matches!(c, '<' | '=' | '>' | '%' | '!'))
            .count();

        if marker_end == 0 || marker_end >= post_trimmed.len() {
            // No tense/degree marker: a plain verb, possibly multi-word, exactly as
            // captured (the runtime still splits off the trailing word to conjugate and
            // passes leading words through verbatim -- unchanged from before this
            // refactor, see `PostSpec::Verb`'s doc comment).
            quote!(ranting::placeholder::PostSpec::Verb(#post))
        } else {
            let marker = &post_trimmed[..marker_end];
            let rest = post_trimmed[marker_end..].trim_start();
            // Split verb/adjective from any trailing content
            let (base_word, trailing) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
            let leading_space = &post[..post.len() - post_trimmed.len()];

            if marker.chars().all(|c| c == '!') {
                let degree_word = match marker.len() {
                    1 => adjective::to_comparative(base_word),
                    2 => adjective::to_superlative(base_word),
                    _ => {
                        let post_span = post_span.unwrap();
                        return Err((
                            post_span.start(),
                            post_span.end(),
                            "degree marker must be `!` (comparative) or `!!` (superlative)"
                                .to_string(),
                        ));
                    }
                };
                quote!(ranting::placeholder::PostSpec::Degree {
                    leading_space: #leading_space,
                    word: #degree_word,
                    trailing: #trailing,
                })
            } else if marker.contains('!') {
                let post_span = post_span.unwrap();
                return Err((
                    post_span.start(),
                    post_span.end(),
                    "degree marker `!`/`!!` cannot be combined with tense markers".to_string(),
                ));
            } else {
                // say!() bakes the fully-conjugated form (as before); say_with!()
                // bakes the uninflected base verb so it can be re-resolved at
                // runtime against a NarrationContext (see handle_placeholder_with_context).
                let conjugated = if runtime_tense {
                    base_word.to_string()
                } else {
                    match marker {
                        "<" => verb_conjugate::to_past(base_word),
                        "=" => verb_conjugate::to_continuous(base_word),
                        ">" => verb_conjugate::to_future(base_word),
                        "<=" => verb_conjugate::to_continuous(base_word),
                        "%" => verb_conjugate::to_past_participle(base_word),
                        "<%" => verb_conjugate::to_past_participle(base_word),
                        _ => base_word.to_string(),
                    }
                };
                // Guaranteed to match one of TenseMarker::from_marker's arms: `marker` was
                // extracted from the same `<=>%!` character class PH_EXT's `post` capture
                // group is built from, minus the all-`!` (degree) case handled above.
                let tense_variant = match marker {
                    "<" => quote!(Past),
                    "=" => quote!(Continuous),
                    ">" => quote!(Future),
                    "<=" => quote!(PastContinuous),
                    "%" => quote!(PresentPerfect),
                    "<%" => quote!(PastPerfect),
                    _ => {
                        let post_span = post_span.unwrap();
                        return Err((
                            post_span.start(),
                            post_span.end(),
                            format!("unrecognized tense marker `{marker}`"),
                        ));
                    }
                };
                quote!(ranting::placeholder::PostSpec::Tense {
                    leading_space: #leading_space,
                    marker: ranting::placeholder::TenseMarker::#tense_variant,
                    word: #conjugated,
                    trailing: #trailing,
                })
            }
        }
    };

    let plurality;
    // NB: if None, no alpha found => all are punct; occurs with '+' or '-'.
    (plurality, nr) = split_at_find_start(nr, |c| c.is_alphanumeric()).unwrap_or((nr, ""));
    let noun =
        get_opt_num_ph_expr(noun.as_str(), given).map_err(|s| (noun.start(), noun.end(), s))?;

    let (nr_fmt, fmt): (Vec<_>, Vec<_>) =
        orig_fmt
            .split(':')
            .filter(|&s| !s.is_empty())
            .partition(|&s| {
                match s {
                    "#x" | "-" | "+" | "x?" | "X?" => false,
                    x if x.starts_with('#')
                        && x.ends_with(&['x', 'X', 'o', 'p', 'b', 'e', 'E']) =>
                    {
                        false
                    }
                    x if x.ends_with(&['$', '*']) => true,
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
            });
    let fmt = if fmt.is_empty() {
        String::new()
    } else {
        ":".to_string() + &join(&fmt, ":")
    };
    let len = pos.len().to_string();
    let noun_space;
    let nr_expr: Expr = if plurality.contains(['#', '$']) {
        let nr_space;
        (pre, nr_space) = split_at_find_end(pre, |c: char| !c.is_whitespace()).unwrap_or((pre, ""));
        (nr, noun_space) = split_at_find_end(nr, |c: char| !c.is_whitespace()).unwrap_or((nr, ""));
        let nr_expr: TokenStream = match get_opt_num_ph_expr(nr, given) {
            Ok(n) if plurality != "#" => parse_quote!(#n),
            Ok(n) => parse_quote!(ranting::rant_convert_numbers(#n as i64)),
            Err(s) => return Err((nr_s, nr_e, s)),
        };
        let nr_fmt_strlit = if nr_fmt.is_empty() {
            format!("{nr_space}{{}}")
        } else {
            if plurality != "$" {
                return Err((
                    nr_s,
                    nr_e,
                    "number formatting not allowed for `{nr}' converted to words.".to_string(),
                ));
            }
            format!("{nr_space}{{:{}}}", join(&nr_fmt, ":"))
        };
        parse_quote!(format!(#nr_fmt_strlit, #nr_expr))
    } else if nr_fmt.is_empty() {
        if pre.is_empty() {
            noun_space = "";
        } else {
            (pre, noun_space) = split_at_find_end(pre, |c: char| !c.is_whitespace())
                .expect("pre without end space?");
        }
        parse_quote!("".to_string())
    } else {
        let m = caps.whole();
        return Err((
            m.start(),
            m.end(),
            "number formatting, for placeholder without a number.".to_string(),
        ));
    };
    let mut possesive = None;
    let mut possesive_uc = false;
    let pre_string = POSS
        .replace(pre, |caps: &Captures| {
            possesive = Some(caps.get(2).unwrap().as_str().to_string());
            match caps.get(1) {
                Some(m) => m.as_str().to_string() + caps.get(3).map_or("", |m| m.as_str()),
                None => {
                    possesive_uc = uc;
                    uc = false;
                    caps.get(3).map_or("", |m| m.as_str()).to_string()
                }
            }
        })
        .to_string();
    let mut poss: TokenStream = parse_quote!("".to_string());
    if let Some(p) = possesive {
        let expr = get_opt_num_ph_expr(&p, given).map_err(|s| (pre_s, pre_e, s))?;
        poss = parse_quote!(ranting::inflect_possessive(#expr.subjective(), false, #possesive_uc));
    }
    // Typed `case` -- see ranting_core::placeholder's module docs for why the pre-noun
    // subjective `=` and the post-noun continuous-tense `=` (folded into `post_expr` above)
    // can never be confused now that each has its own typed field.
    let case_variant = match case {
        "=" => quote!(Subjective),
        "@" => quote!(Objective),
        "`" => quote!(PossessiveDeterminer),
        "~" => quote!(PossessivePronoun),
        "%" => quote!(Reflexive),
        "?" => quote!(Hidden),
        _ => quote!(Name),
    };
    let case_expr = quote!(ranting::placeholder::CaseKind::#case_variant);
    let spec_expr = quote!(ranting::placeholder::PlaceholderSpec {
        pre: #pre_string,
        plurality: #plurality,
        noun_space: #noun_space,
        case: #case_expr,
        post: #post_expr,
    });
    if runtime_tense {
        pos.push(parse_quote!(ranting::handle_placeholder_with_context(&#noun, #poss, #nr_expr, #uc, #spec_expr, &__ranting_narration_ctx)));
    } else {
        pos.push(
            parse_quote!(ranting::handle_placeholder(&#noun, #poss, #nr_expr, #uc, #spec_expr)),
        );
    }
    Ok(format!("{{{len}{fmt}}}"))
}

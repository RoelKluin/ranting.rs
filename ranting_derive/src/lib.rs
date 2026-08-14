// (c) RoelKluin 2022 MIT

mod heed;
mod heed_derive;
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

/// ask!(speaker, audience, template, input) parses `input` against `template`
/// exactly like `heed!()`, then forwards the captures to `audience`'s
/// `Answerable::answer(&speaker, captures)`. Returns `Option<String>` —
/// `None` if `input` doesn't match `template`.
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
///
/// Whitespace is the only word boundary heed!() knows, permanently: every
/// literal/capture boundary must be whitespace in the input. This is
/// script-agnostic rather than ASCII-only — `heed!("取る {item}", "取る 剣")`
/// captures `"剣"` — but continuous-script input written without spaces
/// (`"剣を取る"`) simply returns `None` rather than being split by guesswork.
/// Capture such a run whole (`heed!("{clause}", "剣を取る")`) and segment it
/// with a real tokenizer. A punctuation-only literal is the one exemption: it
/// abuts what precedes it, in any script (`"{item}、 取る"` matches `"剣、 取る"`).
/// `ask!()` and `#[derive(Heed)]` share this compiler and behave identically.
/// See README.md and ROADMAP.md Phase 6 item 9.
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
    // The template literal's own span, handed to every identifier baked out of a placeholder --
    // see `path_from`. `subspan` (which would narrow this to the individual word) is nightly-only
    // and returns `None` here, so the literal is as precise as a stable build can get.
    let lit_span = lit.span_provider.span();
    #[cfg(feature = "debug")]
    eprintln!("{}", text);

    let mut params = vec![];

    let mut err = None;

    let lit_str = PH
        .replace_all(text, |caps: &Captures| {
            let pre = caps.name("pre");
            let fmt = caps.name("fmt").map_or("", |s| s.as_str());
            if let Some(plain) = caps.name("plain") {
                match get_opt_num_ph_expr(plain.as_str(), &params_in, lit_span) {
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
                // ROADMAP.md Phase 6 item 26: `pre` may now also match a plain literal word (the
                // `\w[\w'-]*\s+` branch PH_START just gained), not only sentence-start
                // punctuation/`^`/`{{`. `m.start() == 0` alone is no longer proof that nothing
                // precedes the placeholder -- a word can match at position 0 too -- so
                // sentence-start additionally requires the captured text be empty in that case.
                let at_sentence_start = pre
                    .filter(|m| {
                        (m.start() == 0 && m.as_str().is_empty())
                            || m.as_str().starts_with(lang::SENTENCE_TRIGGER_CHARS)
                    })
                    .is_some();
                // The preceding literal word, if `pre` matched the new word branch rather than a
                // sentence-start marker/`{{`/nothing -- forwarded as data (not collapsed to a
                // bool, unlike `at_sentence_start`) to `inflect_preposition_custom` via
                // `PlaceholderSpec::preposition`. `None` for every other `pre` shape, including
                // when there's no preceding text at all.
                let preposition = pre.filter(|m| {
                    !m.as_str().is_empty()
                        && !m.as_str().starts_with(lang::SENTENCE_TRIGGER_CHARS)
                        && m.as_str() != "{{"
                });
                let replaced = match handle_param(
                    &parsed,
                    &params_in,
                    &mut params,
                    at_sentence_start,
                    preposition.map(|m| m.as_str()),
                    fmt,
                    runtime_tense,
                    lit_span,
                ) {
                    Ok(s) => s,
                    Err((start, end, msg)) => {
                        let offs = ranting.start();
                        err = Some((start + offs, end + offs, msg));
                        String::new()
                    }
                };
                // The matched preceding word is now baked into the generated call
                // (`PlaceholderSpec::preposition`) and rendered at runtime by
                // `handle_placeholder_impl`, so it must *not* also be re-emitted here as
                // inert literal text -- that would render it twice. Every other `pre` shape
                // (sentence-start marker, `{{`, or nothing) is untouched: still plain literal
                // text, emitted exactly as before.
                if preposition.is_some() {
                    replaced
                } else {
                    pre.map_or("", |s| s.as_str()).to_string() + &replaced
                }
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
            fn inflect(&self, as_plural: bool, uc: bool, case: ranting::GrammaticalCase, count: Option<ranting::PlaceholderCount>) -> String {
                (**self).inflect(as_plural, uc, case, count)
            }
            fn skip_article(&self) -> bool {
                (**self).skip_article()
            }
            fn noun_class(&self) -> ranting::NounClass {
                (**self).noun_class()
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

/// `ask!(speaker, audience, template, input)`. Reuses `heed!()`'s template
/// compiler (Phase 5, ROADMAP.md): `template`/`input` follow exactly
/// `heed!()`'s own grammar and matching semantics, but the resulting
/// captures are forwarded into `audience`'s `Answerable::answer` instead of
/// being returned directly.
struct Ask {
    speaker: Expr,
    audience: Expr,
    template: StrLit,
    input: Expr,
}

impl syn::parse::Parse for Ask {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(Error::new(Span::mixed_site(), "missing speaker expression"));
        }
        let speaker = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;

        let audience = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;

        let template = input.parse::<StrLit>()?;
        input.parse::<Token![,]>()?;

        let input_expr = input.parse::<Expr>()?;
        Ok(Ask {
            speaker,
            audience,
            template,
            input: input_expr,
        })
    }
}

/// Parses `input` against `template` exactly like `heed!()`, then forwards
/// the captures to `audience.answer(&speaker, captures)` (`Answerable::answer`).
/// Print result with `--features debug`
impl ToTokens for Ask {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (pattern, captures) = match heed::compile_heed_template(&self.template) {
            Ok(result) => result,
            Err(e) => {
                *tokens = e.to_compile_error();
                return;
            }
        };

        // Defensive, mirrors heed!()'s own check: turn a template-compiler
        // bug into a compile error here rather than a runtime panic at
        // every ask!() call site.
        if let Err(e) = Regex::new(&pattern) {
            let msg = format!(
                "ask!() generated an invalid regex ({e}) — this is a bug in \
                 ask!()'s template compiler (shared with heed!()), please report it"
            );
            *tokens = Error::new_spanned(&self.template.span_provider, msg).to_compile_error();
            return;
        }

        let names: Vec<&str> = captures.iter().map(|c| c.name.as_str()).collect();
        let names_tokens = quote! { &[#(#names),*] };
        let input_expr = &self.input;
        let speaker = &self.speaker;
        let audience = &self.audience;

        // Unlike heed!(), captures stay plain `String`s here regardless of
        // capture kind (no `{$name}` -> u64 conversion): `Answerable::answer`
        // needs one fixed signature per implementor, so a caller that wants
        // a typed value parses the `String` itself inside `answer()`.
        let captures_expr = match captures.len() {
            0 => quote! { __ranting_ask_caps.map(|_| ()) },
            1 => quote! {
                __ranting_ask_caps.map(|mut __v| __v.pop().expect("ask!() matched capture count mismatch"))
            },
            n => {
                let element_exprs = (0..n).map(|_| {
                    quote! { __it.next().expect("ask!() matched capture count mismatch") }
                });
                quote! {
                    __ranting_ask_caps.map(|__v| {
                        let mut __it = __v.into_iter();
                        ( #(#element_exprs),* )
                    })
                }
            }
        };

        *tokens = quote! {{
            static __RANTING_ASK_MATCHER: ranting::HeedMatcher =
                ranting::HeedMatcher::new(#pattern, #names_tokens);
            let __ranting_ask_caps = __RANTING_ASK_MATCHER.match_input(#input_expr);
            #captures_expr.map(|__caps| ranting::Answerable::answer(&#audience, &#speaker, __caps))
        }};
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
/// ```rust,ignore
/// // `ignore`: this doctest can't compile standalone -- `ranting_derive` can't depend on
/// // `ranting` (that dependency runs the other way), so `use ranting::say` can never
/// // resolve here. See CLAUDE.md's "Doctests in proc-macro crate" note. The runnable
/// // copy of this exact example lives on `Ranting`'s trait doc in ranting/src/lib.rs.
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

/// `#[derive(Heed)]` — generates `fn heed(input: &str) -> Option<Self>` from a
/// `#[heed(template = "...")]` attribute, reusing `heed!()`'s own template
/// compiler. Every capture in the template must have a same-named field
/// (String for `{name}`/`{name...}`, `u64` for `{$name}`), and vice versa.
#[proc_macro_derive(Heed, attributes(heed))]
pub fn derive_heed(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input);
    heed_derive::derive_heed(input).into()
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
///
/// `span` is the template literal's own span, not `Span::call_site()`: an identifier baked here
/// names a variable the caller is expected to have in scope, and when they don't, the resulting
/// `E0425: cannot find value ...` is rustc's, not ours -- we cannot reword it, but we can decide
/// where it points. Pointing at the literal narrows the caret from the whole `say!(...)`
/// invocation to the template that actually contains the word.
///
/// Every segment must already be a valid Rust identifier -- see `check_ident_path`, which is the
/// guard that keeps `syn::Ident::new`'s panic from reaching the user as "proc macro panicked".
fn path_from<S: AsRef<str>>(path: S, span: Span) -> Expr {
    Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: Punctuated::from_iter(path.as_ref().split("::").map(|s| syn::PathSegment {
                // XXX: Span::mixed_site() here gives errors.
                ident: syn::Ident::new(s, span),
                arguments: syn::PathArguments::None,
            })),
        },
    })
}

/// Reject a word that cannot become a `syn::Ident` before `path_from` tries.
///
/// `syn::Ident::new` *panics* on a non-identifier, which rustc surfaces as a bare
/// "proc macro panicked / help: message: `\"gato-negro\"` is not a valid identifier" with the
/// caret on the whole macro -- no indication which placeholder, and nothing naming the rule that
/// was broken. The grammar reaches this: `ph_ext`'s word matcher admits `-` (and `'`), so a
/// hyphenated noun parses fine and only blows up here. Returning an `Err` instead routes the
/// message through `StrLitSlice::error`, which underlines the offending word in the template.
///
/// The check must mirror `Ident::new`'s rule exactly, which is "a keyword *or* a legal variable
/// name" -- not `syn::parse_str::<Ident>`, whose `Parse` impl rejects keywords. `{self}` is live
/// syntax (a `Ranting` method saying something about its own receiver, all over
/// `tests/ranting/male_female_and_object.rs`), so rejecting keywords here would break working
/// templates rather than catch a panic. Hence `Ident::parse_any`.
fn check_ident_path(p: &str) -> Result<(), String> {
    use syn::ext::IdentExt;
    for segment in p.split("::") {
        if syn::parse::Parser::parse_str(syn::Ident::parse_any, segment).is_err() {
            return Err(format!(
                "`{p}` is not a valid Rust identifier, so it cannot name a variable. A \
                 placeholder's noun must be a variable in scope, a positional index (`{{0}}`), or \
                 a named argument (`say!(\"{{x}}\", x = ..)`)"
            ));
        }
    }
    Ok(())
}

/// The expression for a match. if numeric, retreive the expression from the positionals
fn get_opt_num_ph_expr(p: &str, given: &HashMap<String, Expr>, span: Span) -> Result<Expr, String> {
    match p.parse::<String>() {
        Err(_) => {
            check_ident_path(p)?;
            Ok(path_from(p, span))
        }
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
                    // Not a number and not in given arguments - assume it's a variable from local
                    // scope. Whether it really is one is rustc's call, not ours: `{el gato}` and
                    // `{person walk}` are the same shape, so a word we don't recognise cannot be
                    // rejected here without rejecting live English syntax too.
                    check_ident_path(&s)?;
                    Ok(path_from(&s, span))
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
#[allow(clippy::too_many_arguments)]
fn handle_param(
    caps: &ranting_core::ph_ext::PhExtMatch,
    given: &HashMap<String, Expr>,
    pos: &mut Vec<Expr>,
    at_sentence_start: bool,
    preposition: Option<&str>,
    orig_fmt: &str,
    runtime_tense: bool,
    span: Span,
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
                let (degree_word, degree_kind) = match marker.len() {
                    1 => (
                        adjective::to_comparative(base_word),
                        quote!(ranting::placeholder::DegreeKind::Comparative),
                    ),
                    2 => (
                        adjective::to_superlative(base_word),
                        quote!(ranting::placeholder::DegreeKind::Superlative),
                    ),
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
                    base: #base_word,
                    degree: #degree_kind,
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
    let noun = get_opt_num_ph_expr(noun.as_str(), given, span)
        .map_err(|s| (noun.start(), noun.end(), s))?;

    let (nr_fmt, fmt): (Vec<_>, Vec<_>) =
        orig_fmt
            .split(':')
            .filter(|&s| !s.is_empty())
            .partition(|&s| {
                match s {
                    "#x" | "-" | "+" | "x?" | "X?" => false,
                    x if x.starts_with('#') && x.ends_with(['x', 'X', 'o', 'p', 'b', 'e', 'E']) => {
                        false
                    }
                    x if x.ends_with(['$', '*']) => true,
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
    // The numeral slot (ROADMAP.md Phase 6 item 8). `numeral_expr` is the baked
    // `Option<NumeralSpec>` for the placeholder spec; `count_expr` the `Option<i64>` count
    // handed to `handle_placeholder` alongside it -- `Some` only for `#var`, which is the
    // one form whose argument the macro already casts to `i64` anyway. `$var` may be given
    // any `Display` type (a float, a formatted width), so casting it here would fail to
    // compile code that works today; its count is recovered at runtime by parsing the
    // rendered digits instead, and is `None` when that parse fails.
    let mut numeral_expr: TokenStream = parse_quote!(None);
    let mut count_expr: TokenStream = parse_quote!(None);
    let nr_expr: Expr = if plurality.contains(['#', '$']) {
        let nr_space;
        (pre, nr_space) = split_at_find_end(pre, |c: char| !c.is_whitespace()).unwrap_or((pre, ""));
        (nr, noun_space) = split_at_find_end(nr, |c: char| !c.is_whitespace()).unwrap_or((nr, ""));
        // A hidden number (`{?$n noun}`) renders nothing, and neither numeral hook is called for
        // it -- same "nothing rendered, nothing to customize" gate as `elide_article_custom`'s
        // hidden-noun case. It still gets a `NumeralSpec`, with `hidden` set, because the
        // separator that would have followed it has to be suppressed too: ROADMAP.md Phase 7
        // item 13, where leaving the slot unrepresented is what produced the stray space.
        let hidden = plurality.contains('?');
        let nr_ph_expr = match get_opt_num_ph_expr(nr, given, span) {
            Ok(n) => n,
            Err(s) => return Err((nr_s, nr_e, s)),
        };
        let kind = if plurality.contains('#') {
            quote!(Words)
        } else {
            quote!(Digits)
        };
        numeral_expr = parse_quote!(Some(ranting::placeholder::NumeralSpec {
            kind: ranting::placeholder::NumeralKind::#kind,
            leading_space: #nr_space,
            hidden: #hidden,
        }));
        if plurality == "#" {
            if !nr_fmt.is_empty() {
                return Err((
                    nr_s,
                    nr_e,
                    "number formatting not allowed for `{nr}' converted to words.".to_string(),
                ));
            }
            // Words are spelled at *runtime* now, from this count, so that a numeral hook
            // can spell them in another language; `ranting::rant_convert_numbers` (the same
            // English speller as before) is the fallback there, so the output is unchanged.
            count_expr = parse_quote!(Some(#nr_ph_expr as i64));
            parse_quote!(String::new())
        } else {
            let nr_fmt_strlit = if nr_fmt.is_empty() {
                // The leading space moved into NumeralSpec; for a hidden number nothing is
                // rendered, so dropping it here is unobservable either way.
                "{}".to_string()
            } else {
                if plurality != "$" {
                    return Err((
                        nr_s,
                        nr_e,
                        "number formatting not allowed for `{nr}' converted to words.".to_string(),
                    ));
                }
                format!("{{:{}}}", join(&nr_fmt, ":"))
            };
            parse_quote!(format!(#nr_fmt_strlit, #nr_ph_expr))
        }
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
        let expr = get_opt_num_ph_expr(&p, given, span).map_err(|s| (pre_s, pre_e, s))?;
        poss = parse_quote!(ranting::inflect_possessive(
            #expr.subjective(),
            #expr.is_plural(),
            #possesive_uc
        ));
    }
    // Classify `pre`'s first word (and, when reachable, its second) at compile time --
    // see ranting_core::placeholder::ArticleKind's docs and ROADMAP.md's
    // `get_article_or_so` fixability note for the proof that this is always safe,
    // including when a `` ` `` possessive-substitution sentinel is present. Mirrors
    // `ArticleKind::classify` exactly (kept in sync manually, like `CaseKind`'s and
    // `TenseMarker`'s own local `match`es in this file) rather than calling it, since
    // this runs at proc-macro build time, producing `quote!` tokens naming the variant.
    fn article_kind_tokens(word: &str) -> TokenStream {
        let variant = if word.contains('`') {
            quote!(Other)
        } else {
            match word.trim_start_matches(['!', '?']) {
                "the" => quote!(The),
                "a" | "an" | "some" => quote!(AAnSome),
                "these" | "those" => quote!(TheseThose),
                _ => quote!(Other),
            }
        };
        quote!(ranting::placeholder::ArticleKind::#variant)
    }
    // Replicates handle_placeholder_impl's own `pre`/`etc1` splitting (same
    // split_at_find_start defined below in this file) on the same literal text, so the
    // baked classification matches exactly what the runtime would compute from it.
    let (pre_first_word, pre_rest) = split_at_find_start(pre_string.as_str(), char::is_whitespace)
        .unwrap_or((pre_string.as_str(), ""));
    let pre_kind_q = article_kind_tokens(&pre_first_word.to_lowercase());
    let pre_chained_kind_q = if pre_string.contains('`') {
        // has_possesive: the runtime's chained (second) get_article_or_so call is
        // never reached in this case (see ArticleKind's docs), so this value is
        // unobserved -- Other is the harmless default.
        quote!(ranting::placeholder::ArticleKind::Other)
    } else {
        let (_art_space, etc1_rest) =
            split_at_find_start(pre_rest, |c: char| !c.is_whitespace()).unwrap_or(("", pre_rest));
        let (s, _) = split_at_find_start(etc1_rest, char::is_whitespace).unwrap_or((etc1_rest, ""));
        // Not lowercased: matches the runtime's case-sensitive second call site.
        article_kind_tokens(s)
    };
    // Typed `case` -- see ranting_core::placeholder's module docs for why the pre-noun
    // subjective `=` and the post-noun continuous-tense `=` (folded into `post_expr` above)
    // can never be confused now that each has its own typed field.
    //
    // ROADMAP.md Phase 6 item 19: the fused `*=`/`*@`/`` *` ``/`*~`/`*%` forms (`*` immediately
    // followed by a real case marker, see `ph_ext::case_one_rep`) report the same `CaseKind` as
    // their bare counterpart -- the article/elision hooks must still see the real case -- plus
    // `display_as_name = true`, so `handle_placeholder_impl` renders the noun's name instead of
    // switching to a pronoun.
    let (case_variant, display_as_name) = match case {
        "*=" => (quote!(Subjective), true),
        "*@" => (quote!(Objective), true),
        "*`" => (quote!(PossessiveDeterminer), true),
        "*~" => (quote!(PossessivePronoun), true),
        "*%" => (quote!(Reflexive), true),
        "=" => (quote!(Subjective), false),
        "@" => (quote!(Objective), false),
        "`" => (quote!(PossessiveDeterminer), false),
        "~" => (quote!(PossessivePronoun), false),
        "%" => (quote!(Reflexive), false),
        "?" => (quote!(Hidden), false),
        _ => (quote!(Name), false),
    };
    let case_expr = quote!(ranting::placeholder::CaseKind::#case_variant);
    let preposition_expr: TokenStream = match preposition {
        Some(word) => quote!(Some(#word)),
        None => quote!(None),
    };
    let spec_expr = quote!(ranting::placeholder::PlaceholderSpec {
        pre: #pre_string,
        pre_kind: #pre_kind_q,
        pre_chained_kind: #pre_chained_kind_q,
        plurality: #plurality,
        numeral: #numeral_expr,
        noun_space: #noun_space,
        case: #case_expr,
        display_as_name: #display_as_name,
        post: #post_expr,
        sentence_start: #at_sentence_start,
        preposition: #preposition_expr,
    });
    if runtime_tense {
        pos.push(parse_quote!(ranting::handle_placeholder_with_context(&#noun, #poss, #nr_expr, #count_expr, #uc, #spec_expr, &__ranting_narration_ctx)));
    } else {
        pos.push(parse_quote!(ranting::handle_placeholder(
            &#noun,
            #poss,
            #nr_expr,
            #count_expr,
            #uc,
            #spec_expr
        )));
    }
    Ok(format!("{{{len}{fmt}}}"))
}

#[cfg(test)]
mod tests {
    use super::check_ident_path;

    /// The guard's whole point: these used to reach `syn::Ident::new` and *panic*, which rustc
    /// reports as "proc macro panicked" with the caret on the entire `say!(...)` invocation and no
    /// indication of which placeholder was at fault. `ph_ext`'s word matcher admits `-` and `'`,
    /// so the grammar really does hand these through -- `{gato-negro}` parses fine and only blows
    /// up at codegen.
    #[test]
    fn non_identifier_nouns_are_rejected_rather_than_panicking() {
        for word in ["gato-negro", "l'eau", "2x", "a b", "", "el-gato::x"] {
            let err = check_ident_path(word)
                .expect_err("a word that cannot become a Rust identifier must be an error");
            assert!(
                err.contains("not a valid Rust identifier"),
                "message should name the rule that was broken, got: {err}"
            );
            assert!(
                err.contains(word) || word.is_empty(),
                "message should quote the offending word, got: {err}"
            );
        }
    }

    /// Anything that *is* a plausible variable still passes -- including a raw identifier and a
    /// multi-segment path, both of which `path_from` supports. Whether the name is actually in
    /// scope is rustc's call, not ours: `{el gato}` and `{person walk}` are the same shape, so a
    /// word we don't recognise cannot be rejected here without rejecting live English syntax.
    ///
    /// `self` is the one that matters: it is a keyword, so `syn::parse_str::<Ident>` rejects it,
    /// but `Ident::new` accepts it and `{self}`/`{=self do}` are used throughout
    /// `tests/ranting/male_female_and_object.rs`. The guard must mirror `Ident::new`'s rule, not
    /// `syn`'s stricter `Parse` impl -- see `check_ident_path`.
    #[test]
    fn plausible_variable_names_still_pass() {
        for word in [
            "el", "gato", "person", "_x", "self", "r#fn", "módulo", "a::b::c",
        ] {
            assert!(
                check_ident_path(word).is_ok(),
                "{word} should be accepted as a possible variable name"
            );
        }
    }
}

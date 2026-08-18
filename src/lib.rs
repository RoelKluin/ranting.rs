// (c) Roel Kluin 2022 MIT
//!
//! Format sentences whose articles, verbs and pronouns agree with the nouns in them.
//!
//! [`say!`] works like `format!()`, but a placeholder may also carry an article, a verb, an
//! adjective or a pronoun case. Each is inflected to agree with the [`Ranting`] value the
//! placeholder names, so one template renders correctly for a singular or plural subject,
//! and for any set of pronouns.
//!
//! ## Gender-Neutral Pronouns
//!
//! This crate fully supports singular they/them for gender-neutral pronouns. Use `"they"` as the subject
//! when you want to refer to an individual using singular they:
//!
//! ```rust
//! # use ranting::{Noun, say};
//! let alex = Noun::new("Alex", "they");
//! assert_eq!(
//!     say!("{=alex} completed {`alex} assignment."),
//!     "They completed their assignment.".to_string()
//! );
//! ```
//!
//! Singular they is grammatically plural in form but semantically singular, so all verb forms are
//! plural (they are, they have, they do). All pronouns inflect correctly:
//!
//! - Subject: they
//! - Object: them
//! - Possessive determiner: their
//! - Possessive pronoun: theirs
//!
//! ## Verbatim verb marker
//!
//! A post-noun verb normally gets person/number agreement, which corrupts an
//! already-correct subjunctive: `` {=i were} `` renders "I was", not "I were" — mood is
//! a property of the surrounding clause (`if`, `wish`, ...), not of the verb, so a
//! smarter conjugator can't fix it without breaking the indicative reading, which is far
//! more common. Prefix the verb with `;` to render it exactly as written, with no
//! agreement applied at all:
//!
//! ```rust
//! # use ranting::{Noun, say};
//! let i = Noun::new("person", "I");
//! assert_eq!(say!("If {=i ;were} rich, I would travel.", i),
//!     "If I were rich, I would travel.".to_string());
//! ```
//!
//! ## Feature flags
#![doc = document_features::document_features!()]

extern crate self as ranting;

mod answerable;
mod collections;
mod heed;
mod language;
mod narration;
use regex::Regex;
use std::sync::LazyLock;

pub use answerable::Answerable;
pub use collections::{Many, Maybe};
pub use narration::{NarrationContext, Person, Register, Tense};

// Upstream's speller, re-exported raw: it spells a negative as one unhyphenated run
// ("negativeone"). `spell_count` below is what `#var` actually renders through.
#[doc(hidden)]
pub use english_numbers::convert_no_fmt as rant_convert_numbers;

/// Spell a `#var` count in English words, writing the sign as a separate word.
///
/// `english_numbers` renders a negative as a single run — `-1` comes back as the non-word
/// "negativeone" — so the magnitude is spelled and "minus " prefixed instead. The result is
/// still one string, which is what `inflect_numeral_custom` is contracted to replace wholesale.
fn spell_count(count: i64) -> String {
    match count.checked_neg() {
        // `i64::MIN` has no representable magnitude. Upstream panics on it (it takes `abs()`
        // internally), and did before this guard existed; leaving the call in place keeps that
        // pre-existing failure exactly as it was rather than inventing an output shape for it.
        Some(magnitude) if count < 0 => format!("minus {}", rant_convert_numbers(magnitude)),
        _ => rant_convert_numbers(count),
    }
}

/// Rewrite the last word of a spelled cardinal into its ordinal form ("three" -> "third",
/// "twenty" -> "twentieth"). Applied to the whole string when it has no interior space (the
/// common case, and also `english_numbers`' unhyphenated compounds like "twentyone", which this
/// sees as one token ending in "one" -- see [`spell_ordinal`]'s docs for that inherited quirk).
fn ordinalize_last_word(word: &str) -> String {
    // Suppletive/stem-change forms first (checked as suffixes, not whole-word equality, so a
    // compound like "twentyone" still rewrites to "twentyfirst"), then the `-y` -> `-ieth` rule,
    // then the regular `+th` fallback ("fourth", "sixth", "hundredth").
    if let Some(stem) = word.strip_suffix("one") {
        format!("{stem}first")
    } else if let Some(stem) = word.strip_suffix("two") {
        format!("{stem}second")
    } else if let Some(stem) = word.strip_suffix("three") {
        format!("{stem}third")
    } else if let Some(stem) = word.strip_suffix("five") {
        format!("{stem}fifth")
    } else if let Some(stem) = word.strip_suffix("eight") {
        format!("{stem}eighth")
    } else if let Some(stem) = word.strip_suffix("nine") {
        format!("{stem}ninth")
    } else if let Some(stem) = word.strip_suffix("twelve") {
        format!("{stem}twelfth")
    } else if let Some(stem) = word.strip_suffix('y') {
        format!("{stem}ieth")
    } else {
        format!("{word}th")
    }
}

/// Spell a `##var` count in English ordinal words ("three" -> "third").
///
/// Spells the cardinal via [`spell_count`], then rewrites its last word --
/// see `docs/superpowers/specs/2026-08-15-ordinal-numerals.md`'s "The English rules" section for
/// the full table. Inherits `spell_count`'s "minus " treatment of negatives verbatim ("minus
/// three" -> "minus third"), which is not English anyone writes but is deterministic and
/// non-panicking, same posture as the cardinal; a fork's `inflect_numeral_custom` hook may
/// replace the whole string. Also inherits `english_numbers`' unhyphenated compound spelling
/// ("twentyone", not "twenty-one"), which this function does not attempt to fix.
fn spell_ordinal(count: i64) -> String {
    let spelled = spell_count(count);
    match spelled.rsplit_once(' ') {
        Some((prefix, last)) => format!("{prefix} {}", ordinalize_last_word(last)),
        None => ordinalize_last_word(&spelled),
    }
}

/// The English ordinal suffix for `$$var`'s digit rendering ("3rd", "11th", "21st").
///
/// Checked against the *last two* digits, not the last one -- the teens exception: 11-13 (and
/// 111-113, 211-213, ...) all take "th" regardless of their last digit.
fn ordinal_suffix(n: i64) -> &'static str {
    let last_two = n.unsigned_abs() % 100;
    if (11..=13).contains(&last_two) {
        "th"
    } else {
        match n.unsigned_abs() % 10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        }
    }
}

// required for ranting_derive
#[doc(hidden)]
pub use strum_macros as rant_strum_macros;

use in_definite::get_a_or_an;
use language::english::{
    adapt_article, inflect_adjective, inflect_objective, inflect_subjective, inflect_verb,
};
pub use language::english::{
    inflect_noun_irregular, inflect_noun_regular, inflect_possessive, inflect_reflexive,
};
pub use ranting_core::grammar::{SubjectPronoun, is_subject, is_subjective_plural};
use ranting_core::placeholder::{
    ArticleKind, CaseKind, NumeralKind, NumeralSpec, PlaceholderSpec, Plurality, PostSpec,
};
use std::str::FromStr;

// Undocumented here on purpose: a `///` on the re-export renders above the module's own doc
// rather than replacing it, so the reader would read a summary twice. Deliberately not
// `#[doc(hidden)]` either — the module is the clearest description of the macro-to-runtime seam
// for anyone extending the grammar. See `.claude/rules/crate-layout.md`'s "Macro flow" section.
pub use ranting_core::placeholder;

pub use heed::{HeedMatcher, HeedTemplateError};

// TODO: make this a feature:
//pub(crate) use strum_macros;

/// Like `say!()`, but yields `Ok(String)` rather than a `String`.
///
/// It is an ordinary expression: bind it to a `let`, end a block with it, or write
/// `return ack!(...)` to return early.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, ack};
/// fn confirm(parcel: Noun) -> Result<String, String> {
///     ack!("{The parcel arrive} tomorrow.")
/// }
///
/// assert_eq!(
///     confirm(Noun::new("parcel", "it")),
///     Ok("The parcel arrives tomorrow.".to_string())
/// );
/// assert_eq!(
///     confirm(Noun::new("parcels", "they")),
///     Ok("The parcels arrive tomorrow.".to_string())
/// );
/// ```
pub use ranting_derive::ack;

/// Like `say!()`, but yields `Err(String)` rather than a `String`.
///
/// It is an ordinary expression: bind it to a `let`, end a block with it, or write
/// `return nay!(...)` to return early.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, nay};
/// fn check_out(reader: Noun, copies_left: u32) -> Result<String, String> {
///     if copies_left == 0 {
///         return nay!("{`reader} loan can't start: every copy is out.");
///     }
///     Ok("Enjoy the book!".to_string())
/// }
///
/// assert_eq!(
///     check_out(Noun::new("Jo", "she"), 0),
///     Err("Her loan can't start: every copy is out.".to_string())
/// );
/// assert_eq!(check_out(Noun::new("Jo", "she"), 3), Ok("Enjoy the book!".to_string()));
/// ```
pub use ranting_derive::nay;

/// Formats like `format!()`, but a placeholder may hold more than a value: an article, a verb,
/// an adjective or a numeral written beside the name is inflected to agree with it. Markers
/// written before the name — `+` and `-` for number, `` ` ``, `@` and `~` for pronoun case,
/// `<`, `=` and `>` for tense, among others — select which form is rendered.
///
/// # Examples
///
/// The article and the verb agree with the noun, so one template covers both numbers:
///
/// ```rust
/// # use ranting::{Noun, say};
/// let ship = Noun::new("ship", "she");
/// assert_eq!(say!("{The ship sail} at dawn."), "The ship sails at dawn.".to_string());
/// assert_eq!(say!("{The +ship sail} at dawn."), "The ships sail at dawn.".to_string());
/// ```
///
/// A marker before the name picks the pronoun case: `=` subject, `@` object, `` ` ``
/// possessive determiner, `~` possessive pronoun. All of them follow the noun's own subject:
///
/// ```rust
/// # use ranting::{Noun, say};
/// fn inflect(with: Noun) -> String {
///     let n = Noun::new("noun", "it");
///     say!("{some n} with {0} {?n inflect} as {=0}, {@0}, {`0} and {~0}.", with)
/// }
///
/// let sentences: Vec<String> = ["I", "you", "he", "she", "it", "we", "they"]
///     .iter()
///     .map(|s| inflect(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect();
///
/// assert_eq!(sentences, [
///     "A noun with subject I inflects as I, me, my and mine.",
///     "A noun with subject you inflects as you, you, your and yours.",
///     "A noun with subject he inflects as he, him, his and his.",
///     "A noun with subject she inflects as she, her, her and hers.",
///     "A noun with subject it inflects as it, it, its and its.",
///     "A noun with subject we inflects as we, us, our and ours.",
///     "A noun with subject they inflects as they, them, their and theirs.",
/// ]);
/// ```
///
/// # Gender-Neutral Pronouns (Singular They)
///
/// You can use singular they/them pronouns for individuals who prefer gender-neutral language:
///
/// ```rust
/// # use ranting::{Noun, say};
/// let jordan = Noun::new("Jordan", "they");
/// assert_eq!(
///     say!("{=jordan are} a wonderful friend."),
///     "They are a wonderful friend.".to_string()
/// );
/// assert_eq!(
///     say!("This is {`jordan} favorite book."),
///     "This is their favorite book.".to_string()
/// );
/// ```
pub use ranting_derive::say;

/// Like `say!()`, but takes a [`NarrationContext`] as its first argument: a placeholder's
/// tense marker becomes a default that the context can override at runtime, so one template
/// can be told in the past or the future. Without an override the output is identical to
/// `say!()`'s.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say_with, NarrationContext, Tense};
/// let jordan = Noun::new("Jordan", "they");
/// let ctx = NarrationContext {
///     tense: Some(Tense::Past),
///     ..Default::default()
/// };
/// assert_eq!(
///     say_with!(ctx, "{=jordan <arrive} here."),
///     "They arrived here.".to_string()
/// );
/// ```
pub use ranting_derive::say_with;

// The four re-exports below carry no doc of their own on purpose: rustdoc renders the
// re-export's `///` *above* the macro's own doc, so a summary here is read twice.
pub use ranting_derive::heed;

pub use ranting_derive::Heed;

pub use ranting_derive::ask;

pub use ranting_derive::derive_ranting;

/// Implements [`Ranting`] for a `Box<&dyn Trait>`, given a trait that requires [`Ranting`].
pub use ranting_derive::boxed_ranting_trait;

/// Implements [`Ranting`] for a `&'_ dyn Trait`, given a trait that requires [`Ranting`].
pub use ranting_derive::ref_ranting_trait;

/// How to render a noun's article at one call site — bundled to keep
/// `get_article_or_so` under clippy's argument-count limit now that
/// `case: GrammaticalCase` is threaded through alongside plurality/case/context.
struct ArticleRenderCtx<'a> {
    case: CaseKind,
    as_pl: bool,
    uc: bool,
    sentence_start: bool,
    ctx: Option<&'a NarrationContext>,
    count: Option<PlaceholderCount>,
}

fn get_article_or_so<R>(
    noun: &R,
    s: &str,
    kind: ArticleKind,
    space: &str,
    render: ArticleRenderCtx,
) -> Option<String>
where
    R: Ranting,
{
    let ArticleRenderCtx {
        case,
        as_pl,
        uc,
        sentence_start,
        ctx,
        count,
    } = render;
    if noun.skip_article() && !s.starts_with('!') && !matches!(kind, ArticleKind::TheseThose) {
        return Some("".to_string());
    }
    // `!` is the "render the article even though this entity skips articles" marker, checked
    // against the raw `s` just above; `?` is the "display depends on the entity" marker. Neither
    // is ever part of the word rendered, and both have to come off before the word reaches a
    // hook or `adapt_article` — see docs/architecture-review-2026-08-14.md §1.5.
    let article_form = s.trim_start_matches(['!', '?']);
    let case: GrammaticalCase = case.into();
    // Read off the noun rather than plumbed through `ArticleRenderCtx`: the class is a property
    // of the entity, and every article hook call in this function already has `noun` in hand.
    let class = noun.noun_class();
    match kind {
        ArticleKind::The => {
            // "the" needs no singular of its own; deriving one via inflect() would panic for
            // plural nouns whose name cannot be singularized (e.g. Noun::new("one", "they")).
            let singular = noun.name(false);
            if let Some(custom) = noun.inflect_article_custom_with_context(
                "the", &singular, case, class, as_pl, count, uc, ctx,
            ) {
                Some(custom + space)
            } else {
                Some(
                    noun.capitalize_with_context(
                        article_form,
                        OrthographyRole::Article,
                        uc,
                        sentence_start,
                        ctx,
                    ) + space,
                )
            }
        }
        ArticleKind::AAnSome => {
            // adapt_article() discards the a/an choice when as_pl, so only singularize when the
            // singular form is actually rendered; inflect() would panic on non-standard plurals.
            // `None`, not `count`: this asks what the noun's *singular* spelling is so a/an can be
            // picked from its first letter — it is not the site that renders the counted noun, and
            // it already forces `to_plural = false`, so a count of 2 here would contradict it. No
            // signal is lost: item 14 gave `inflect_article_custom_with_context` its own `count`,
            // which this placeholder's real numeral reaches just below.
            let singular = if as_pl {
                noun.name(false)
            } else {
                noun.inflect(false, false, case, None)
            };
            if let Some(custom) = noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                count,
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                // `uc` is applied once, by the hook, on the assembled article: adapt_article()
                // may pick either its own form or the a/an passed in, so capitalizing before it
                // ran would mean capitalizing a form that gets discarded.
                //
                // ROADMAP.md Phase 8 item 3: a mass noun's singular renders the unstressed
                // `some` instead of a guessed a/an -- `some` is already in the closed
                // vocabulary (`ArticleOrSo::A` covers `a`/`an`/`some`), so this substitutes it
                // for whichever of the three the template wrote; `adapt_article`'s `t ==
                // ArticleOrSo::A => s` arm then keeps it verbatim, exactly as it already does
                // for a written `some` on a non-mass noun. Elision was the other option the
                // design spike weighed and rejected: that story belongs to `skip_article`/
                // `no_article`, not to a mass-noun-only special case here.
                let a_or_an = if noun.is_mass() && !as_pl {
                    "some"
                } else {
                    get_a_or_an(&singular)
                };
                let article = ranting::adapt_article(a_or_an, article_form, space, as_pl, false);
                Some(noun.capitalize_with_context(
                    &article,
                    OrthographyRole::Article,
                    uc,
                    sentence_start,
                    ctx,
                ))
            }
        }
        ArticleKind::TheseThose => {
            // Demonstratives are chosen from as_pl alone; see the "the" arm for why the name is
            // used instead of an inflected singular.
            let singular = noun.name(false);
            if let Some(custom) = noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                count,
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                let article =
                    ranting::adapt_article(article_form, article_form, space, as_pl, false);
                Some(noun.capitalize_with_context(
                    &article,
                    OrthographyRole::Article,
                    uc,
                    sentence_start,
                    ctx,
                ))
            }
        }
        // ROADMAP.md Phase 8 item 3: `no` is number-transparent -- it renders itself
        // unchanged on both singular and plural agreement, so unlike every other arm here it
        // needs no `adapt_article`/`ArticleOrSo` table at all, only a real `ArticleKind` so it
        // stops reaching the pre-noun *verb* path (`{no $n item}` at `n = 1` used to render
        // "Noes 1 item"; see docs/superpowers/specs/2026-08-15-quantifier-determiners.md).
        // `each`/`either`/`neither` render the same way -- also invariant text, since their
        // number behavior (forcing the singular) is baked into `as_pl` at compile time by
        // `ranting_derive`'s `article_kind_tokens`, not expressed by swapping the word itself.
        ArticleKind::No | ArticleKind::Each | ArticleKind::EitherNeither => {
            let singular = noun.name(false);
            if let Some(custom) = noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                count,
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                Some(
                    noun.capitalize_with_context(
                        article_form,
                        OrthographyRole::Article,
                        uc,
                        sentence_start,
                        ctx,
                    ) + space,
                )
            }
        }
        // `every` swaps to the suppletive plural `all` on plural agreement -- the same
        // `these`/`those` -> `this`/`that` mechanism above, pointed at one more pair.
        ArticleKind::EveryAll => {
            let singular = noun.name(false);
            if let Some(custom) = noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                count,
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                let article =
                    ranting::adapt_article(article_form, article_form, space, as_pl, false);
                Some(noun.capitalize_with_context(
                    &article,
                    OrthographyRole::Article,
                    uc,
                    sentence_start,
                    ctx,
                ))
            }
        }
        // `much`/`many` and `less`/`fewer` select on `is_mass()`, not on `as_pl` -- the
        // concrete reason part (a) had to wait for part (b): without a mass/count flag the
        // only proxy is number agreement, which guesses wrong on exactly the nouns these
        // words exist for ("much items"/"many information"-class errors). Bypasses
        // `adapt_article`/`ArticleOrSo` entirely, since that table's only selection axis is
        // `as_pl`.
        ArticleKind::MuchMany | ArticleKind::LessFewer => {
            let singular = noun.name(false);
            if let Some(custom) = noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                count,
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                let word = match (kind, noun.is_mass()) {
                    (ArticleKind::MuchMany, true) => "much",
                    (ArticleKind::MuchMany, false) => "many",
                    (ArticleKind::LessFewer, true) => "less",
                    (ArticleKind::LessFewer, false) => "fewer",
                    _ => unreachable!("kind is MuchMany or LessFewer, checked by the outer match"),
                };
                Some(
                    noun.capitalize_with_context(
                        word,
                        OrthographyRole::Article,
                        uc,
                        sentence_start,
                        ctx,
                    ) + space,
                )
            }
        }
        // Not one of English's article keywords -- a possessive-substitution sentinel, a
        // pre-noun verb, or (since the language-modularity change) a word in some other
        // language entirely. Offer it to the entity before giving up: a fork's hook is the
        // only thing that can know `el`/`la`/`der` are articles, and without this call the
        // word would render as inert literal text with no agreement -- `{el +*=gato}` would
        // give "el gatos" rather than "los gatos". Returning `None` (every English impl's
        // default) leaves the word exactly as written, which is what makes English output
        // byte-identical. See docs/superpowers/specs/2026-08-14-language-modularity.md.
        ArticleKind::Other => {
            // `name`, not `inflect()`: this arm reaches words that are not articles at all,
            // and the "the" arm's reasoning applies a fortiori -- singularizing here would
            // panic for plural nouns whose name cannot be singularized.
            let singular = noun.name(false);
            noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                count,
                uc,
                ctx,
            )
            .map(|custom| custom + space)
        }
    }
}

/// Conjugate a verb, giving the noun's custom hook the first chance.
///
/// Falls back to the English `inflect_verb()` when the hook returns `None`. Every site that
/// conjugates a *base* verb must go through here so that ecosystem forks can override English
/// rules uniformly (pre-noun and post-noun alike).
///
/// Note: the `~TENSE~` marker path does *not* use this helper. There the verb arrives already
/// conjugated by the macro, so the English fallback would inflect it a second time; that site
/// tries the hook alone and keeps the conjugated form otherwise.
#[allow(clippy::too_many_arguments)]
fn conjugate_verb<R>(
    noun: &R,
    subjective: &str,
    verb: &str,
    as_pl: bool,
    count: Option<PlaceholderCount>,
    uc: bool,
    sentence_start: bool,
    ctx: Option<&NarrationContext>,
) -> String
where
    R: Ranting,
{
    if let Some(custom) =
        noun.inflect_verb_custom_with_context(subjective, verb, as_pl, count, uc, ctx)
    {
        custom
    } else {
        // Fallback path only: a custom form applies `uc` itself (see the hook's docs), so
        // capitalizing here too would be a second, unasked-for pass over it.
        let conjugated = inflect_verb(subjective, verb, as_pl, false);
        noun.capitalize_with_context(&conjugated, OrthographyRole::Verb, uc, sentence_start, ctx)
    }
}

/// Capitalize an English-fallback pronoun through the noun's own hook.
///
/// Only the fallback matters here: the five pronoun arms of `handle_placeholder_impl` all render
/// via `inflect_*(subjective, as_pl, false)` and hand the result to this, while a
/// `inflect_pronoun_custom` form applies `uc` itself and never passes through.
fn cap_pronoun<R>(
    noun: &R,
    pronoun: String,
    uc: bool,
    sentence_start: bool,
    ctx: Option<&NarrationContext>,
) -> String
where
    R: Ranting,
{
    noun.capitalize_with_context(&pronoun, OrthographyRole::Pronoun, uc, sentence_start, ctx)
}

/// The say macro parses placeholders and passes the compile-time-baked spec to this
/// function which returns a string.
#[doc(hidden)]
pub fn handle_placeholder<R>(
    noun: &R,
    poss: String,
    nr: String,
    count: Option<i64>,
    uc: bool,
    spec: PlaceholderSpec,
) -> String
where
    R: Ranting,
{
    handle_placeholder_impl(noun, poss, nr, count, uc, spec, None)
}

/// Like [`handle_placeholder`], but resolves tense markers against a runtime
/// [`NarrationContext`] instead of the compile-time marker alone. The say_with!()
/// macro parses placeholders and passes the compile-time-baked spec to this function.
#[doc(hidden)]
pub fn handle_placeholder_with_context<R>(
    noun: &R,
    poss: String,
    nr: String,
    count: Option<i64>,
    uc: bool,
    spec: PlaceholderSpec,
    ctx: &NarrationContext,
) -> String
where
    R: Ranting,
{
    handle_placeholder_impl(noun, poss, nr, count, uc, spec, Some(ctx))
}

#[allow(clippy::too_many_arguments)]
fn handle_placeholder_impl<R>(
    noun: &R,
    poss: String,
    nr: String,
    count: Option<i64>,
    mut uc: bool,
    spec: PlaceholderSpec,
    ctx: Option<&NarrationContext>,
) -> String
where
    R: Ranting,
{
    static OF: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\bof\s+$").expect("valid regex"));
    let PlaceholderSpec {
        pre: pre_raw,
        pre_kind,
        pre_chained_kind,
        plurality,
        numeral,
        noun_space,
        case,
        display_as_name,
        post: post_spec,
        sentence_start,
        preposition,
    } = spec;
    let mut pre = pre_raw;
    let has_possesive = pre.contains('`');
    let singular_post_verb = OF.is_match(pre); // e.g. "{a set of $ten are} still singular"

    let as_pl = match plurality {
        Plurality::Unmarked => noun.is_plural(),
        Plurality::Plus => true,
        Plurality::Minus => false,
        // `#var`'s count is baked by the macro, so number agreement is decided from the
        // number itself and never from the rendered word. That is what keeps a non-English
        // `inflect_numeral_custom` from flipping agreement -- the hook renders *after* this,
        // and its output is never sniffed -- and it is also why `spell_count`'s "minus one"
        // agrees plural ("minus one degrees"), which sniffing the spelled form would get
        // wrong now that the negative spelling contains the word "one".
        Plurality::CardinalWords => count != Some(1),
        // ROADMAP.md Phase 8 item 4: agreement decouples from the ordinal itself -- "the third
        // attempt" stays singular even though `count` is 3, an ordinal says *which* one, not
        // *how many* -- so this falls through to the same rule `Unmarked` takes rather than
        // reading `count`/`nr` at all.
        Plurality::OrdinalWords | Plurality::OrdinalDigits => noun.is_plural(),
        Plurality::CardinalDigits => {
            let s = nr.trim_start();
            s != "1" && s.split('.').next() != Some("1")
        }
    };
    // The count channel (ROADMAP.md Phase 6 item 14): mirrors the `as_pl` match immediately
    // above, but carries the numeral's value (and visible fraction digits) through to the five
    // `_custom` hooks that had no numeral signal at all, rather than collapsing it to a bool.
    // `None` for a bare `{noun}`/`{+noun}`/`{-noun}` -- there is no numeral to report.
    //
    // ROADMAP.md Phase 8 item 4: unlike `as_pl` above, an ordinal's `count` still flows through
    // here exactly like its cardinal sibling's -- this is what Spanish/Arabic ordinal gender
    // agreement needs from `inflect_numeral_custom`, even though the same count no longer
    // decides `as_pl`.
    let placeholder_count: Option<PlaceholderCount> = match plurality {
        Plurality::Unmarked | Plurality::Plus | Plurality::Minus => None,
        Plurality::CardinalWords | Plurality::OrdinalWords => count.map(|value| PlaceholderCount {
            value,
            fraction_digits: 0,
        }),
        Plurality::CardinalDigits | Plurality::OrdinalDigits => {
            let s = nr.trim_start();
            let mut parts = s.splitn(2, '.');
            let int_part = parts.next().unwrap_or("");
            let frac_part = parts.next();
            int_part.parse::<i64>().ok().map(|value| PlaceholderCount {
                value,
                fraction_digits: frac_part.map_or(0, |f| f.chars().count() as u32),
            })
        }
    };
    let pre_string = pre.replace('`', poss.as_str());

    let space;
    (pre, space) = split_at_find_end(&pre_string, |c: char| !c.is_whitespace())
        .unwrap_or((pre_string.as_str(), ""));

    let mut etc1;
    (pre, etc1) = split_at_find_start(pre, |c| c.is_whitespace()).unwrap_or((pre, ""));

    let declared_subjective = noun.subjective();
    // A `narration_person` override (say_with!() only) only ever affects
    // nouns declared first-person ("I"/"we") — the narrator. Other subjects
    // pass through unchanged, and the article/noun-name plurality (`as_pl`
    // above) is untouched either way: viewpoint governs which pronoun set
    // and verb agreement render, not how the noun's own name pluralizes.
    let viewpoint = ctx
        .and_then(|c| narration::resolve_viewpoint(noun, declared_subjective, c.narration_person));
    let subjective = viewpoint.map_or(declared_subjective, |(rendered, _)| rendered);
    let pronoun_as_pl = viewpoint.map_or(as_pl, |(_, forced_pl)| forced_pl);
    // The entity's own lexical gender / noun class, handed to the pronoun hooks below. Read off
    // the noun, not derived from anything in the placeholder — see `Ranting::noun_class`.
    let noun_class = noun.noun_class();
    let mut res = String::new();

    // Byte range of the last article pushed into `res`, for the post-assembly elision hook
    // below. Recorded here rather than returned by `get_article_or_so` so that function keeps
    // its signature — and so the English path stays provably untouched.
    let mut article_span: Option<(usize, usize)> = None;

    // The literal preposition word (with its trailing whitespace), when
    // `PlaceholderSpec::preposition` carried one -- rendered here, exactly as written, instead of
    // the macro baking it as inert literal format-string text, so `inflect_preposition_custom`
    // below can replace it together with the article that follows. Recorded the same way
    // `article_span` is, for the same reason. See ROADMAP.md Phase 6 item 26.
    let mut preposition_span: Option<(usize, usize)> = None;
    if let Some(word) = preposition {
        let start = res.len();
        res.push_str(word);
        preposition_span = Some((start, res.len()));
    }

    // This may be an article or certain verbs that can occur before the noun:
    if !pre.is_empty() {
        let p = pre.to_lowercase();
        if let Some(a) = get_article_or_so(
            noun,
            p.as_str(),
            pre_kind,
            space,
            ArticleRenderCtx {
                case,
                as_pl,
                uc,
                sentence_start,
                ctx,
                count: placeholder_count,
            },
        ) {
            let start = res.len();
            res.push_str(&a);
            article_span = Some((start, res.len()));
        } else if has_possesive {
            // A possessive noun phrase built from the noun's own name ("Jane's"), hence
            // OrthographyRole::Noun rather than a role of its own — and, as at the name site
            // below, pre-capitalized with `uc` already spent, so the hook is passed `false`.
            let poss_phrase = capitalize_if(pre, uc);
            res.push_str(&noun.capitalize_with_context(
                &poss_phrase,
                OrthographyRole::Noun,
                false,
                sentence_start,
                ctx,
            ));
        } else {
            assert!(
                matches!(post_spec, PostSpec::None),
                "verb before and after?"
            );
            let verb = conjugate_verb(
                noun,
                subjective,
                p.as_str(),
                pronoun_as_pl,
                placeholder_count,
                uc,
                sentence_start,
                ctx,
            );
            res.push_str(&verb);
            if !etc1.is_empty() {
                let art_space;
                (art_space, etc1) =
                    split_at_find_start(etc1, |c| !c.is_whitespace()).unwrap_or(("", etc1));
                res.push_str(art_space);
                let s;
                (s, etc1) = split_at_find_start(etc1, |c| c.is_whitespace()).unwrap_or((etc1, ""));
                if let Some(a) = get_article_or_so(
                    noun,
                    s,
                    pre_chained_kind,
                    space,
                    ArticleRenderCtx {
                        case,
                        as_pl,
                        uc: false,
                        sentence_start,
                        ctx,
                        count: placeholder_count,
                    },
                ) {
                    // Last one wins: a chained article ("isn't the homme") is the one adjacent
                    // to the noun, so it is the one elision applies to.
                    let start = res.len();
                    res.push_str(&a);
                    article_span = Some((start, res.len()));
                } else {
                    res.push_str(s);
                }
            }
        }
        res.push_str(etc1);
        res.push_str(space);
        uc = false;
    }
    // A zero-length article (`inflect_article_custom` returning `""` — German's articleless
    // indefinite plural, "Hunde bellen" — or `skip_article()` suppressing it entirely) is
    // recorded as an empty `article_span`. The separator that would normally follow the article
    // belongs to the article, not to whatever comes next, so it is swallowed once here — on
    // whichever of the numeral or the noun comes first — rather than left to render as a stray
    // leading or doubled space. See ROADMAP.md Phase 6 item 11 and
    // `tests/ranting/zero_length_article.rs`.
    let mut swallow_separator = article_span.is_some_and(|(start, end)| start == end);

    // The span of the rendered numeral in `res`, for the post-assembly splice below. `None` when
    // no numeral was rendered, exactly like `article_span`.
    let mut numeral_span: Option<(usize, usize)> = None;

    // The numeral slot. `numeral` is `None` when the placeholder has no `#var`/`$var` marker at
    // all; a hidden one (`{?$n noun}`) is `Some` with `hidden` set, and renders nothing. See
    // ROADMAP.md Phase 6 item 8 and Phase 7 item 13.
    // A hidden numeral renders nothing between two separators — the one before it and the noun's
    // own — so the pair has to collapse to one, the same way a zero-length article's does. Before
    // ROADMAP.md Phase 7 item 13 the slot was simply absent from the spec and both separators
    // rendered: `say!("I see {?$0 boot}", 2)` gave "I see  boots".
    //
    // Which one survives matters. Keeping the *leading* one and swallowing the noun's is what
    // leaves `{The ?$n noun}` rendering "The raven" — there the leading space is the article's
    // only separator, and the noun's is the spare. With nothing rendered before the numeral the
    // leading space is empty anyway, so the noun's is dropped and no stray space is left.
    if let Some(NumeralSpec {
        leading_space,
        hidden: true,
        ..
    }) = numeral
    {
        if !swallow_separator {
            res.push_str(leading_space);
        }
        swallow_separator = true;
    }
    if let Some(NumeralSpec {
        kind,
        leading_space,
        hidden: false,
    }) = numeral
    {
        // `#var`/`##var` are spelled here rather than by the macro, so the hook can replace the
        // speller wholesale; `$var`/`$$var` arrive already rendered as digits, `:fmt` spec
        // applied. ROADMAP.md Phase 8 item 4: `##var`/`$$var` are the ordinal siblings, sharing
        // the same real `count` the cardinal channel carries.
        let english = match kind {
            NumeralKind::Words => count.map_or_else(String::new, spell_count),
            NumeralKind::Ordinal => count.map_or_else(String::new, spell_ordinal),
            NumeralKind::Digits | NumeralKind::OrdinalDigits => nr.clone(),
        };
        // `$var`/`$$var`'s count is not baked (the argument needn't be an integer at all), so
        // it is recovered from the rendered digits — `None` for a float, a width-padded or
        // otherwise formatted number, or a non-numeric `Display`.
        let count = match kind {
            NumeralKind::Words | NumeralKind::Ordinal => count,
            NumeralKind::Digits | NumeralKind::OrdinalDigits => english.trim().parse::<i64>().ok(),
        };
        // The English ordinal suffix ("3rd", "11th") is appended from the parsed `count` above
        // rather than baked into `english` before that parse -- otherwise the parse would see
        // "3rd" instead of "3" and always fail. A failed parse (a float, a padded value) leaves
        // the digits unsuffixed, the same "count is None, agree from what we have" posture
        // `$var` already takes.
        let english = if kind == NumeralKind::OrdinalDigits {
            match count {
                Some(n) => format!("{english}{}", ordinal_suffix(n)),
                None => english,
            }
        } else {
            english
        };
        let rendered = noun
            .inflect_numeral_custom_with_context(
                &english,
                count,
                kind.into(),
                case.into(),
                noun_class,
                as_pl,
                ctx,
            )
            .unwrap_or(english);
        // A sentence-initial placeholder with no preceding article/verb still has `uc` to
        // spend, and the numeral is the next thing that renders — so it claims the capital
        // here instead of leaving it to fall through to the noun. The two channels differ:
        // `#var` is a spelled word and can take it; `$var` is digits and can't, so the
        // capital is simply dropped rather than carried on. Nothing claims it when the
        // numeral itself renders empty (a hidden numeral, or a fork returning "").
        let rendered = if uc && sentence_start && !rendered.is_empty() {
            uc = false;
            match kind {
                NumeralKind::Words | NumeralKind::Ordinal => capitalize_if(&rendered, true),
                NumeralKind::Digits | NumeralKind::OrdinalDigits => rendered,
            }
        } else {
            rendered
        };
        if swallow_separator {
            swallow_separator = false;
        } else {
            res.push_str(leading_space);
        }
        let start = res.len();
        res.push_str(&rendered);
        numeral_span = Some((start, res.len()));
    }

    // The leading whitespace preceding whatever comes after the noun. For
    // `PostSpec::Verb` this is still split off the raw captured text at
    // runtime (unchanged from before this refactor); for `Tense`/`Degree`
    // the macro already isolated it at compile time; `None`/`PossessiveS`
    // never have one.
    let post_leading_space = match post_spec {
        PostSpec::None | PostSpec::PossessiveS => "",
        PostSpec::Verb(raw) => {
            split_at_find_start(raw, |c: char| !c.is_whitespace()).map_or("", |(sp, _)| sp)
        }
        PostSpec::Tense { leading_space, .. }
        | PostSpec::Degree { leading_space, .. }
        | PostSpec::Verbatim { leading_space, .. } => leading_space,
    };

    if case != CaseKind::Hidden {
        if !swallow_separator {
            res.push_str(noun_space);
        }
        // ROADMAP.md Phase 6 item 19: the fused `*=`/`*@`/etc. marker case-marks the placeholder
        // (the article/elision hooks below still see `case`'s real `GrammaticalCase`, via
        // `case.into()`) but asks for the noun's own name here, exactly like `CaseKind::Name`,
        // instead of switching to `inflect_pronoun_custom`. `display_as_name` is only ever `true`
        // for a real case variant (never `Name`/`Hidden` -- see the field's own docs), so this
        // check has to run before the `case` match, not as one more arm of it.
        let s = if display_as_name {
            let name = noun.inflect(as_pl, uc, case.into(), placeholder_count);
            noun.capitalize_with_context(&name, OrthographyRole::Noun, false, sentence_start, ctx)
        } else {
            match case {
                CaseKind::Subjective => {
                    if let Some(custom) = noun.inflect_pronoun_custom_with_context(
                        subjective,
                        PronounCase::Subjective,
                        noun_class,
                        pronoun_as_pl,
                        placeholder_count,
                        uc,
                        ctx,
                    ) {
                        custom
                    } else {
                        cap_pronoun(
                            noun,
                            inflect_subjective(subjective, pronoun_as_pl, false),
                            uc,
                            sentence_start,
                            ctx,
                        )
                    }
                }
                CaseKind::Objective => {
                    if let Some(custom) = noun.inflect_pronoun_custom_with_context(
                        subjective,
                        PronounCase::Objective,
                        noun_class,
                        pronoun_as_pl,
                        placeholder_count,
                        uc,
                        ctx,
                    ) {
                        custom
                    } else {
                        cap_pronoun(
                            noun,
                            inflect_objective(subjective, pronoun_as_pl, false),
                            uc,
                            sentence_start,
                            ctx,
                        )
                    }
                }
                CaseKind::PossessiveDeterminer => {
                    if let Some(custom) = noun.inflect_pronoun_custom_with_context(
                        subjective,
                        PronounCase::PossessiveDeterminer,
                        noun_class,
                        pronoun_as_pl,
                        placeholder_count,
                        uc,
                        ctx,
                    ) {
                        custom
                    } else {
                        cap_pronoun(
                            noun,
                            inflect_possessive(subjective, pronoun_as_pl, false),
                            uc,
                            sentence_start,
                            ctx,
                        )
                    }
                }
                CaseKind::PossessivePronoun => {
                    if let Some(custom) = noun.inflect_pronoun_custom_with_context(
                        subjective,
                        PronounCase::PossessivePronoun,
                        noun_class,
                        pronoun_as_pl,
                        placeholder_count,
                        uc,
                        ctx,
                    ) {
                        custom
                    } else {
                        cap_pronoun(
                            noun,
                            inflect_adjective(subjective, pronoun_as_pl, false),
                            uc,
                            sentence_start,
                            ctx,
                        )
                    }
                }
                CaseKind::Reflexive => {
                    if let Some(custom) = noun.inflect_pronoun_custom_with_context(
                        subjective,
                        PronounCase::Reflexive,
                        noun_class,
                        pronoun_as_pl,
                        placeholder_count,
                        uc,
                        ctx,
                    ) {
                        custom
                    } else {
                        cap_pronoun(
                            noun,
                            inflect_reflexive(subjective, pronoun_as_pl, false),
                            uc,
                            sentence_start,
                            ctx,
                        )
                    }
                }
                // The only site that does *not* hand the hook an uncapitalized word: `inflect()`
                // takes `uc` itself and is user-implementable, so English capitalization is already
                // resolved here — and it is not the same as `capitalize_if`, since a derive-generated
                // `name()` for `#[ranting(name = "designer")]` reads `uc == true` as "as written",
                // not "force uppercase". Hence `false`: the hook must not apply it a second time.
                // A fork that capitalizes nouns unconditionally (German) ignores the flag anyway.
                CaseKind::Name | CaseKind::Hidden => {
                    let name = noun.inflect(as_pl, uc, case.into(), placeholder_count);
                    noun.capitalize_with_context(
                        &name,
                        OrthographyRole::Noun,
                        false,
                        sentence_start,
                        ctx,
                    )
                }
            }
        };
        res.push_str(&s);
        // The numeral-noun boundary, spliced the same post-assembly way the article-noun boundary
        // is below (ROADMAP.md Phase 7 item 12). It runs **first of the three**, before
        // preposition fusion and article elision, because it edits the innermost region:
        // `[preposition][article][numeral][noun]`. Every byte it rewrites is at or after
        // `article_span`'s end, so both spans the two later splices depend on stay valid, and
        // they in turn see the already-fused numeral+noun as their trailing text.
        //
        // It has to be this way round. When this ran *after* preposition fusion — as it did
        // between Phase 7 item 12 and 2026-08-15 — a successful fusion had already truncated at
        // `p_start` and rebuilt, shifting every later byte, while `numeral_span` still held
        // pre-fusion offsets. The slice below then read a displaced window (`"> g"` out of
        // `"<2> gato"`) and `res.truncate(start)` cut at a displaced index: silently wrong text,
        // or a panic off a `char` boundary with multibyte input. See
        // `docs/architecture-review-2026-08-15.md` §1.1.
        //
        // Japanese needs it: 「一匹の猫」 is written with no space anywhere, and until this
        // existed the separator was pushed by this function and offered to no hook, so
        // `一匹の 猫` was the best a fork could do. See `Ranting::elide_numeral_custom`.
        if let Some((start, end)) = numeral_span.filter(|(start, end)| start != end) {
            let (numeral_text, num_ws) =
                split_at_find_end(&res[start..end], |c: char| !c.is_whitespace())
                    .unwrap_or((&res[start..end], ""));
            let (noun_ws, following) =
                split_at_find_start(&res[end..], |c: char| !c.is_whitespace())
                    .unwrap_or(("", &res[end..]));
            let separator = format!("{num_ws}{noun_ws}");
            let fused = noun.elide_numeral_custom_with_context(
                numeral_text,
                &separator,
                following,
                case.into(),
                noun_class,
                as_pl,
                placeholder_count,
                ctx,
            );
            if let Some(fused) = fused {
                res.truncate(start);
                res.push_str(&fused);
            }
        }
        // Preposition-article fusion (ROADMAP.md Phase 6 item 26): tried before *article*
        // elision, at the same post-assembly point, since it needs the same rendered article text
        // elision does. The numeral splice above has already run and may have rewritten
        // everything after the article, which is fine: `tail` below re-reads it. On success it consumes both the preposition and the article,
        // so the elision splice below is skipped -- the article it would have elided against no
        // longer exists. See `Ranting::inflect_preposition_custom`.
        let mut prep_fused = false;
        if let (Some((p_start, p_end)), Some((a_start, a_end))) = (preposition_span, article_span) {
            // Only the simple, adjacent case is offered to the hook -- nothing rendered between
            // the preposition and the article (e.g. no pre-noun verb in between). `following`
            // isn't part of the hook's contract the way it is for `elide_article_custom`, so
            // there is nothing sensible to splice a mid-section into.
            if p_start != p_end && a_start != a_end && p_end == a_start {
                let prep_word = res[p_start..p_end].trim_end();
                let (article, _art_ws) =
                    split_at_find_end(&res[a_start..a_end], |c: char| !c.is_whitespace())
                        .unwrap_or((&res[a_start..a_end], ""));
                let fused = noun.inflect_preposition_custom_with_context(
                    prep_word,
                    article,
                    case.into(),
                    noun_class,
                    as_pl,
                    placeholder_count,
                    uc,
                    ctx,
                );
                if let Some(fused) = fused {
                    // Everything after the article (separator, numeral, noun/pronoun) must
                    // survive the splice -- unlike `elide_article_custom`, whose `following`
                    // parameter lets the hook rebuild it, this hook only replaces the
                    // preposition+article pair itself.
                    let tail = res[a_end..].to_string();
                    res.truncate(p_start);
                    res.push_str(&fused);
                    res.push_str(&tail);
                    prep_fused = true;
                }
            }
        }
        // Post-assembly elision: the article and everything the placeholder renders after it are
        // both in `res` now, which is the whole point — `inflect_article_custom` ran before this
        // text existed. Inside the `case != Hidden` block on purpose: `{?the noun}` renders
        // nothing to elide against. See `Ranting::elide_article_custom`.
        if !prep_fused && let Some((start, end)) = article_span.filter(|(start, end)| start != end)
        {
            // The separator is not necessarily the article string's own trailing whitespace:
            // the noun's leading space is pushed later (`noun_space`), so collect whitespace
            // from both sides of the boundary rather than assuming which side carries it.
            let (article, art_ws) =
                split_at_find_end(&res[start..end], |c: char| !c.is_whitespace())
                    .unwrap_or((&res[start..end], ""));
            let (noun_ws, following) =
                split_at_find_start(&res[end..], |c: char| !c.is_whitespace())
                    .unwrap_or(("", &res[end..]));
            let separator = format!("{art_ws}{noun_ws}");
            let fused = noun.elide_article_custom_with_context(
                article,
                &separator,
                following,
                case.into(),
                noun_class,
                as_pl,
                placeholder_count,
                ctx,
            );
            if let Some(fused) = fused {
                res.truncate(start);
                res.push_str(&fused);
            }
        }
        res.push_str(post_leading_space);
        uc = false;
    } else if !swallow_separator && !post_leading_space.is_empty() {
        // The noun itself renders nothing, so `post_leading_space` (the gap between the noun
        // and the post-noun text) is dropped -- there is nothing rendered yet for it to
        // separate from. `noun_space` (the gap between a preceding article/pre-word and the
        // noun) is different: the article itself already rendered into `res` before this
        // point regardless of `case`, so if `noun_space` is non-empty, something real needs
        // separating from what follows (`{the ?jane !!good}` used to render "Thebest";
        // pushing `noun_space` here gives "the best"). When there's no preceding article,
        // `noun_space` is empty and this is a no-op, matching the existing, correct
        // `{?w !!good}` -> "Best in class" behavior.
        //
        // Gated on `post_leading_space` being non-empty (i.e. `post_spec` is not
        // `None`/`PossessiveS`) so a hidden noun with nothing after it at all -- `{can ?w}`,
        // `PostSpec::None` -- doesn't gain a stray trailing space with nothing to separate
        // from what follows in the *next* placeholder or literal text.
        //
        // `uc` is deliberately left untouched here (unlike the visible branch above) -- the
        // hidden noun contributes no capitalizable text, so a sentence-initial placeholder
        // still needs `uc` to reach the post-noun verb/degree text that renders next.
        res.push_str(noun_space);
    }

    match post_spec {
        PostSpec::None => {}
        PostSpec::PossessiveS => {
            res.push_str(adapt_possesive_s(as_pl));
        }
        PostSpec::Verb(raw) => {
            // A phrasal or compound verb ("pick up") conjugates on its head word;
            // everything after the first word — including the separating whitespace
            // — is carried through unchanged. (Until 2026-08-15 this split off the
            // *last* word instead, which conjugated the particle rather than the verb —
            // "pick up" -> "pick ups"; see docs/architecture-review-2026-08-15.md §1.6.)
            let rest =
                split_at_find_start(raw, |c: char| !c.is_whitespace()).map_or(raw, |(_, r)| r);
            let (word, trailing) =
                split_at_find_start(rest, |c: char| c.is_whitespace()).unwrap_or((rest, ""));
            if !word.is_empty() {
                match word {
                    "'" | "'s" => {
                        res.push_str(adapt_possesive_s(as_pl));
                    }
                    v => {
                        let verb = conjugate_verb(
                            noun,
                            subjective,
                            v,
                            !singular_post_verb && pronoun_as_pl,
                            placeholder_count,
                            uc,
                            sentence_start,
                            ctx,
                        );
                        res.push_str(&verb);
                    }
                }
                res.push_str(trailing);
            }
        }
        PostSpec::Tense {
            marker,
            word,
            trailing,
            ..
        } => {
            let tense_result = match ctx {
                None => {
                    // say!(): word is already the compile-time-conjugated form. The
                    // English fallback would inflect it twice ("will walks"), so offer
                    // the hook the conjugated form and keep it verbatim when it declines.
                    let main_verb = noun
                        .inflect_verb_custom_with_context(
                            subjective,
                            word,
                            !singular_post_verb && pronoun_as_pl,
                            placeholder_count,
                            false,
                            None,
                        )
                        .unwrap_or_else(|| word.to_string());
                    handle_tense_marker(subjective, marker.as_marker_str(), &main_verb)
                }
                Some(narration_ctx) => {
                    // say_with!(): word is the uninflected base verb; resolve per the
                    // runtime context, falling back to the compile-time marker's
                    // default tense.
                    let (marker_str, base_form) = match narration_ctx.tense {
                        Some(t) => narration::marker_and_form_for_tense(t, word, marker),
                        None => (
                            marker.as_marker_str(),
                            narration::form_for_marker(marker.as_marker_str(), word),
                        ),
                    };
                    if marker_str.is_empty() {
                        // Present tense: plain subject-verb agreement, no auxiliary.
                        conjugate_verb(
                            noun,
                            subjective,
                            &base_form,
                            !singular_post_verb && pronoun_as_pl,
                            placeholder_count,
                            false,
                            sentence_start,
                            ctx,
                        )
                    } else {
                        let main_verb = noun
                            .inflect_verb_custom_with_context(
                                subjective,
                                &base_form,
                                !singular_post_verb && pronoun_as_pl,
                                placeholder_count,
                                false,
                                ctx,
                            )
                            .unwrap_or(base_form);
                        handle_tense_marker(subjective, marker_str, &main_verb)
                    }
                }
            };
            res.push_str(&noun.capitalize_with_context(
                &tense_result,
                OrthographyRole::Verb,
                uc,
                sentence_start,
                ctx,
            ));
            if !trailing.is_empty() {
                res.push(' ');
                res.push_str(trailing);
            }
        }
        PostSpec::Degree {
            base,
            degree,
            word,
            trailing,
            ..
        } => {
            // The hook gets the adjective as written plus the agreement inputs (case, class,
            // number); only when it declines do we emit the compile-time English degree form,
            // which is why say!()'s English output is unchanged.
            let custom = noun.inflect_adjective_custom_with_context(
                base,
                degree.into(),
                case.into(),
                noun_class,
                as_pl,
                placeholder_count,
                uc,
                ctx,
            );
            if let Some(custom) = custom {
                res.push_str(&custom);
            } else {
                res.push_str(&noun.capitalize_with_context(
                    word,
                    OrthographyRole::Adjective,
                    uc,
                    sentence_start,
                    ctx,
                ));
            }
            if !trailing.is_empty() {
                res.push(' ');
                res.push_str(trailing);
            }
        }
        PostSpec::Verbatim { word, trailing, .. } => {
            // ROADMAP.md Phase 8 item 2: no `inflect_verb_custom_with_context` call at
            // all -- the whole point of the marker is that nothing re-derives this word.
            res.push_str(&noun.capitalize_with_context(
                word,
                OrthographyRole::Verb,
                uc,
                sentence_start,
                ctx,
            ));
            if !trailing.is_empty() {
                res.push(' ');
                res.push_str(trailing);
            }
        }
    }
    res
}

/// Handle verb conjugation with auxiliary insertion for tense markers.
///
/// This function is called by the macro when tense markers (`<`, `=`, `>`) are detected.
/// It conjugates both the auxiliary verb and the main verb based on the subject pronoun.
///
/// # Arguments
///
/// * `subject` - The subject pronoun (e.g., "I", "he", "she", "they")
/// * `marker` - The tense marker: `<` for past, `=` for continuous, `>` for future
/// * `verb` - The conjugated verb form from the macro
///
/// # Returns
///
/// A string containing the auxiliary verb + main verb combination (e.g., "will walk", "is running")
#[doc(hidden)]
pub fn handle_tense_marker(subject: &str, marker: &str, verb: &str) -> String {
    use language::auxiliary::{AuxiliaryVerb, conjugate_auxiliary};

    match marker {
        "<" => {
            // Past tense: subject + past verb (no auxiliary needed)
            verb.to_string()
        }
        "=" => {
            // Continuous present: subject + is/are + gerund
            let aux = conjugate_auxiliary(AuxiliaryVerb::IsAre, subject);
            format!("{} {}", aux, verb)
        }
        ">" => {
            // Future: subject + will + base verb
            let aux = conjugate_auxiliary(AuxiliaryVerb::Will, subject);
            format!("{} {}", aux, verb)
        }
        "<=" => {
            // Past continuous: subject + was/were + gerund
            let aux = conjugate_auxiliary(AuxiliaryVerb::WasWere, subject);
            format!("{} {}", aux, verb)
        }
        "%" => {
            // Present perfect: subject + has/have + past participle
            let aux = conjugate_auxiliary(AuxiliaryVerb::HaveHas, subject);
            format!("{} {}", aux, verb)
        }
        "<%" => {
            // Past perfect: subject + had + past participle
            let aux = conjugate_auxiliary(AuxiliaryVerb::Had, subject);
            format!("{} {}", aux, verb)
        }
        // ROADMAP.md Phase 8 item 1: the participle channel -- passive voice, future
        // perfect, perfect progressive. Auxiliary agreement reuses IsAre/WasWere/HaveHas
        // unchanged (docs/superpowers/specs/2026-08-15-participle-channel.md); "will
        // have"/"had been"/"will be"/"will have been" are invariant across every person.
        "=%" => {
            // Present passive: subject + is/are/am + past participle
            let aux = conjugate_auxiliary(AuxiliaryVerb::IsAre, subject);
            format!("{} {}", aux, verb)
        }
        "<=%" => {
            // Past passive: subject + was/were + past participle
            let aux = conjugate_auxiliary(AuxiliaryVerb::WasWere, subject);
            format!("{} {}", aux, verb)
        }
        ">%" => {
            // Future perfect: subject + will have + past participle
            format!("will have {}", verb)
        }
        "%=" => {
            // Present perfect progressive: subject + has/have + been + gerund
            let aux = conjugate_auxiliary(AuxiliaryVerb::HaveHas, subject);
            format!("{} been {}", aux, verb)
        }
        "<%=" => {
            // Past perfect progressive: subject + had been + gerund
            format!("had been {}", verb)
        }
        // Internal-only marker strings: never baked by `handle_param` (`>=%`/`>%=` are not
        // enumerated `tense_variant` spellings), only ever synthesized by
        // `narration::marker_and_form_for_tense` when a `ctx.tense` override moves a
        // passive/perfect-progressive placeholder to the future while preserving its voice.
        ">=%" => {
            // Future passive: subject + will be + past participle
            format!("will be {}", verb)
        }
        ">%=" => {
            // Future perfect progressive: subject + will have been + gerund
            format!("will have been {}", verb)
        }
        _ => verb.to_string(),
    }
}

/// Uppercase a word's first character when `uc` is set, and return it unchanged otherwise.
///
/// A leading apostrophe is skipped, so a contraction capitalizes the letter a reader expects:
/// `'tis` becomes `'Tis`, not `'tis`. This is what [`Ranting::capitalize`] does by default, and
/// what a custom hook should call when it wants English's own behavior for a form it built
/// itself.
///
/// ```rust
/// # use ranting::capitalize_if;
/// assert_eq!(capitalize_if("dog", true), "Dog");
/// assert_eq!(capitalize_if("dog", false), "dog");
/// assert_eq!(capitalize_if("'tis", true), "'Tis");
/// ```
pub fn capitalize_if(s: &str, uc: bool) -> String {
    if uc {
        let mut c = s.chars();
        c.next()
            .map(|t| match t {
                '\'' => {
                    t.to_string()
                        + &c.next()
                            .map(|c| c.to_uppercase().collect::<String>())
                            .unwrap_or_default()
                }
                _ => t.to_uppercase().collect::<String>(),
            })
            .unwrap_or_default()
            + c.as_str()
    } else {
        s.to_string()
    }
}

/// Renamed to [`capitalize_if`].
#[deprecated(since = "1.4.0", note = "renamed to `capitalize_if`")]
pub fn uc_1st_if(s: &str, uc: bool) -> String {
    capitalize_if(s, uc)
}

fn split_at_find_start(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    // `rfind` yields the *start* byte of the matched char, so the split point is one whole
    // character further on, not one byte. `u + 1` panicked on any multibyte match — every Arabic,
    // Greek, Cyrillic or CJK placeholder reached it, since the predicate here is "the last
    // non-whitespace character". Found by `ranting_ar`; pinned by
    // `tests/ranting/property_based.rs::split_at_find_end_handles_multibyte_text`.
    s.rfind(fun).map(|u| {
        let width = s[u..].chars().next().map_or(1, char::len_utf8);
        s.split_at(u + width)
    })
}

/// A name together with the subject pronoun that goes with it — the ready-made [`Ranting`]
/// value, for text that isn't backed by a type of your own.
///
/// ```rust
/// # use ranting::{Noun, say};
/// let cat = Noun::new("cat", "she");
/// assert_eq!(
///     say!("{The cat} lost {`cat} collar."),
///     "The cat lost her collar.".to_string()
/// );
/// assert_eq!(
///     say!("{The +cat} lost {+`cat} collars."),
///     "The cats lost their collars.".to_string()
/// );
/// ```
///
/// For a type of your own, `#[derive_ranting]` gives it the same placeholder support directly,
/// and lets you override individual [`Ranting`] methods.
#[derive(ranting_derive::Ranting)]
// By setting name and subject to "$", these must come from the struct.
#[ranting(
    name = "$",
    subject = "$",
    gender = "$",
    singular_end = "$",
    plural_end = "$",
    mass = "$",
    no_article = "$"
)]
pub struct Noun {
    pub(crate) name: String,
    // Typed, not `String`: an invalid subject pronoun is now unrepresentable
    // in a constructed `Noun` — `try_new` is the only way in, and it rejects
    // anything `SubjectPronoun::from_str` rejects. `ranting_derive`'s generic
    // `subject = "$"` codegen only ever calls `self.subject.as_str()` /
    // passes that to `is_subjective_plural(&str)`, so `SubjectPronoun::as_str`
    // (added alongside this) keeps that generated code compiling unchanged —
    // it also serves user structs that still declare a plain `subject: String`
    // field, which this type change does not affect.
    pub(crate) subject: SubjectPronoun,
    // The lexical gender / noun class, `NounClass::UNSET` unless set with
    // `with_noun_class()`. Named to match the `gender = "$"` attribute above, following the
    // same attribute-name-is-field-name rule as `name`/`subject`; the accessor is the
    // `Ranting::noun_class` trait method, since that is what the inflection hooks are given.
    pub(crate) gender: NounClass,
    // The `singular_end`/`plural_end` attributes, as runtime values. `Option`, and typed as
    // such rather than as the `String` a user struct declares for `= "$"`, because for `Noun`
    // "unset" has to be expressible at *runtime*: a bare `Noun` must keep getting English's
    // regular plural rules, while `with_plural_end("s")` must be able to ask for literal
    // append-`s` and get no English orthography at all. `DeclaredEnding` is what lets one
    // attribute accept both field shapes.
    pub(crate) singular_end: Option<String>,
    pub(crate) plural_end: Option<String>,
    // ROADMAP.md Phase 8 item 3. Plain `bool`, `false` unless `with_mass()` is called: `Noun`
    // has no attributes to declare, the same reason `gender`/`singular_end`/`plural_end` are
    // runtime fields here, and mass is a bare flag with no "unset" state to represent (unlike
    // `singular_end`/`plural_end`, which need `Option` to distinguish "declared empty" from
    // "no rule declared" -- mass has only two states to begin with).
    pub(crate) mass: bool,
    // Plain `bool`, `false` unless `with_skip_article()` is called -- same shape and same reason
    // as `mass` above: `Noun` has no attributes to declare, so this is the runtime field
    // `#[ranting(no_article = "$")]` reads. Distinct from the type-level `#[ranting(no_article)]`
    // a derived struct writes for a noun class that is *always* article-less; here it's a plain
    // per-instance flag because two `Noun`s can differ (a proper name vs. a common noun).
    pub(crate) no_article: bool,
}

/// How the derive macro reads a `#[ranting(singular_end = "$")]` / `#[ranting(plural_end =
/// "$")]` field, so that both field shapes work.
///
/// A `String` field (what [the attribute docs](crate#attributes) describe, and what user structs
/// declare) always counts as *declared* — the struct stated a rule, so [`inflect_noun_regular`]
/// takes the literal strip-and-append path. An `Option<String>` field can additionally say
/// "unset" at runtime, i.e. fall back to English's regular rules; [`Noun`] uses that to offer
/// [`Noun::with_plural_end`] without giving up the rules for every noun that doesn't call it.
///
/// You only implement this if you want a third field shape; the blanket cases below cover the
/// documented ones.
pub trait DeclaredEnding {
    /// The declared suffix, or `None` for "no rule declared — use the language's own".
    fn declared(&self) -> Option<&str>;
}

impl DeclaredEnding for String {
    fn declared(&self) -> Option<&str> {
        Some(self.as_str())
    }
}

impl DeclaredEnding for &str {
    fn declared(&self) -> Option<&str> {
        Some(self)
    }
}

impl DeclaredEnding for Option<String> {
    fn declared(&self) -> Option<&str> {
        self.as_deref()
    }
}

impl DeclaredEnding for Option<&str> {
    fn declared(&self) -> Option<&str> {
        *self
    }
}

/// Error returned by [`Noun::try_new`] when `subject` isn't one of the
/// recognized subject pronouns ("I", "you", "thou", "he", "she", "it", "we",
/// "ye", "they").
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvalidSubjectError(pub String);

impl std::fmt::Display for InvalidSubjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} is not a valid subject pronoun", self.0)
    }
}

impl std::error::Error for InvalidSubjectError {}

impl Noun {
    /// Construct a noun from a name and one of the recognized subject pronouns: "I", "you",
    /// "thou", "he", "she", "it", "we", "ye" or "they".
    ///
    /// Panics on any other subject. Use [`Noun::try_new`] when the subject comes from input or
    /// configuration rather than from a literal you wrote.
    ///
    /// ```rust
    /// # use ranting::{Noun, say};
    /// let alex = Noun::new("Alex", "they");
    /// assert_eq!(say!("{=alex are} here."), "They are here.".to_string());
    /// ```
    //
    // Kept alongside `try_new`, which arrived later, for backward compatibility with existing
    // call sites.
    pub fn new(name: &str, subject: &str) -> Self {
        Self::try_new(name, subject).expect("not a subject")
    }

    /// Construct a noun, reporting an unrecognized subject pronoun as an
    /// [`InvalidSubjectError`] rather than panicking the way [`Noun::new`] does.
    ///
    /// ```rust
    /// # use ranting::Noun;
    /// assert!(Noun::try_new("Alex", "they").is_ok());
    ///
    /// let refused = Noun::try_new("Alex", "xe").err().unwrap();
    /// assert_eq!(refused.to_string(), "\"xe\" is not a valid subject pronoun");
    /// ```
    pub fn try_new(name: &str, subject: &str) -> Result<Self, InvalidSubjectError> {
        let subject = SubjectPronoun::from_str(subject)
            .map_err(|_| InvalidSubjectError(subject.to_string()))?;
        Ok(Noun {
            name: name.to_string(),
            subject,
            gender: NounClass::UNSET,
            singular_end: None,
            plural_end: None,
            mass: false,
            no_article: false,
        })
    }

    /// Declare this noun's lexical gender / noun class, consuming and returning it so it chains
    /// off [`new`](Self::new)/[`try_new`](Self::try_new). Both constructors leave it
    /// [`NounClass::UNSET`], which is what every noun that never calls this keeps.
    ///
    /// The class is handed to [`Ranting::inflect_article_custom`],
    /// [`Ranting::inflect_pronoun_custom`] and [`Ranting::inflect_adjective_custom`]; `ranting`
    /// itself never reads it, so setting one on a
    /// noun rendered by plain English rules changes nothing.
    ///
    /// # Examples
    /// ```rust
    /// # use ranting::*;
    /// let katze = Noun::new("Katze", "she").with_noun_class(NounClass::new("feminine"));
    /// assert_eq!(katze.noun_class(), NounClass::new("feminine"));
    /// ```
    pub fn with_noun_class(mut self, class: NounClass) -> Self {
        self.gender = class;
        self
    }

    /// Declare this noun's own plural suffix, opting out of English's regular spelling rules.
    ///
    /// The suffix is appended literally, with no orthography applied — the same contract
    /// `#[ranting(plural_end = "...")]` gives a derived struct. Both constructors leave it
    /// undeclared, which is what keeps `{+fly}` rendering `"flies"`.
    ///
    /// Declaring `"s"` is meaningful and is not the same as leaving it alone: it asks for a bare
    /// append, which is what a German or Dutch loanword plural wants. Consonant + `y` is the
    /// class where the two actually differ.
    ///
    /// # Examples
    /// ```rust
    /// # use ranting::*;
    /// # use ranting_derive::say;
    /// let party = Noun::new("Party", "it");
    /// assert_eq!(say!("{+party}"), "Parties".to_string());
    ///
    /// let party = Noun::new("Party", "it").with_plural_end("s");
    /// assert_eq!(say!("{+party}"), "Partys".to_string());
    /// ```
    pub fn with_plural_end(mut self, plural_end: &str) -> Self {
        self.plural_end = Some(plural_end.to_string());
        self
    }

    /// Declare the suffix stripped before [`with_plural_end`](Self::with_plural_end)'s is
    /// appended, and appended when singularizing. Opts out of English's regular rules on its
    /// own, exactly as `#[ranting(singular_end = "...")]` does.
    ///
    /// # Examples
    /// ```rust
    /// # use ranting::*;
    /// # use ranting_derive::say;
    /// let fuchs = Noun::new("Fuchs", "it")
    ///     .with_singular_end("s")
    ///     .with_plural_end("se");
    /// assert_eq!(say!("{+fuchs}"), "Fuchse".to_string());
    /// ```
    pub fn with_singular_end(mut self, singular_end: &str) -> Self {
        self.singular_end = Some(singular_end.to_string());
        self
    }

    /// Declare this noun a mass noun ("information", "water") rather than a count noun,
    /// consuming and returning it so it chains off [`new`](Self::new)/[`try_new`](Self::try_new).
    /// Both constructors leave it `false`, which is what every noun that never calls this keeps.
    ///
    /// See [`Ranting::is_mass`] for what changes once it's set.
    ///
    /// # Examples
    /// ```rust
    /// # use ranting::*;
    /// # use ranting_derive::say;
    /// let info = Noun::new("information", "it").with_mass();
    /// assert_eq!(say!("{a info}"), "Some information".to_string());
    /// ```
    pub fn with_mass(mut self) -> Self {
        self.mass = true;
        self
    }

    /// Declare whether this noun's article should be suppressed — for a proper name ("Alice"
    /// walked in, not "An Alice"), a sport, or a meal, where an article never belongs.
    /// Consuming and returning, so it chains off [`new`](Self::new)/[`try_new`](Self::try_new).
    /// Both constructors leave it `false`, which is what every noun that never calls this keeps.
    ///
    /// A `bool` parameter rather than a bare `with_skip_article()` (unlike [`with_mass`]) because
    /// per-instance suppression is expected to be set conditionally — e.g. only for nouns a
    /// caller has classified as proper-named — so a caller building `Noun`s from data typically
    /// wants `with_skip_article(is_proper_name)` rather than a branch that does or doesn't call
    /// it.
    ///
    /// # Examples
    /// ```rust
    /// # use ranting::*;
    /// # use ranting_derive::say;
    /// let alice = Noun::new("Alice", "she").with_skip_article(true);
    /// assert_eq!(say!("{a alice} walked in."), "Alice walked in.".to_string());
    /// ```
    pub fn with_skip_article(mut self, skip: bool) -> Self {
        self.no_article = skip;
        self
    }
}

/// convert to `'s` or `'` as appropriate for singular or plural of a noun.
///
/// # Examples
///
/// ```rust
/// # use ranting::*;
/// # use ranting_derive::say;
/// # fn main() {
///
/// let school = Noun::new("school", "it");
/// let principal = Noun::new("principal", "she");
/// let myles = Noun::new("Myles", "he");
///
/// assert_eq!(say!("{the school'} {principal are} also {myles'}, but only one of all {the +school's} {+principal} in town."),
///     "The school's principal is also Myles's, but only one of all the schools' principals in town.".to_string());
/// assert_eq!(say!("Are {the school'} {principal} also {myles'}?"),
///     "Are the school's principal also Myles's?".to_string());
/// # }
/// ```
// a combined plural may require some tricks: "The star and cross' design was pattented by Bob."
fn adapt_possesive_s(asked_plural: bool) -> &'static str {
    if asked_plural { "'" } else { "'s" }
}

/// Pronoun grammatical case for customization via inflect_pronoun_custom()
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PronounCase {
    /// Subject pronouns: I, you, he, she, it, we, they
    Subjective,
    /// Object pronouns: me, you, him, her, it, us, them
    Objective,
    /// Possessive determiners: my, your, his, her, its, our, their
    PossessiveDeterminer,
    /// Possessive pronouns: mine, yours, his, hers, its, ours, theirs
    PossessivePronoun,
    /// Reflexive pronouns: myself, yourself, thyself, himself, herself, itself,
    /// ourselves, yourselves, themselves
    Reflexive,
}

/// The grammatical role of the *noun an article or adjective attaches to*, for customization via
/// [`Ranting::inflect_article_custom`] and [`Ranting::inflect_adjective_custom`]. Mirrors the case
/// marker written directly on that
/// placeholder's noun (`` {the =noun} `` is `Subjective`, `` {the @noun} `` is `Objective`,
/// etc.) — English doesn't inflect articles by case, so this carries no information English
/// itself needs, but a case-declining language's fork (e.g. German `der`/`den`/`dem`) does.
///
/// A placeholder with no case marker on the noun at all (the common case: `` {the noun} ``)
/// reports [`GrammaticalCase::Name`] — there is nothing in the source to distinguish subject
/// from object in that form, the same way English's own "the" doesn't. Getting a
/// case-correct article for such a placeholder requires the template to add an explicit
/// marker, e.g. `` {the =noun} ``; `GrammaticalCase` can only report what was written, not
/// infer sentence role from surrounding words.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GrammaticalCase {
    /// No case marker on the noun (bare `` {the noun} ``) — English's own default; treat as
    /// nominative unless the fork's grammar says otherwise.
    Name,
    /// `` {the =noun} `` — the noun is also displayed as a subject pronoun; nominative case.
    Subjective,
    /// `` {the @noun} `` — the noun is also displayed as an object pronoun; objective/accusative
    /// case (English doesn't distinguish accusative from dative; neither does this).
    Objective,
    /// `` {the `noun} `` — the noun is also displayed as a possessive determiner; genitive-ish.
    PossessiveDeterminer,
    /// `` {the ~noun} `` — the noun is also displayed as a possessive pronoun; genitive-ish.
    PossessivePronoun,
    /// `` {the %noun} `` — the noun is also displayed reflexively.
    Reflexive,
    /// `` {?the noun} `` — the noun itself is hidden from output, but the article still renders
    /// and still needs a grammatically correct form.
    Hidden,
}

/// Which part of a rendered placeholder a piece of text is, handed to [`Ranting::capitalize`]
/// so an implementation can decide *per role* whether sentence-position capitalization applies.
///
/// Unlike [`GrammaticalCase`] this is **not** mirrored from a `ranting_core` type: a case marker
/// is written in the placeholder, so there is something at the macro↔runtime seam to mirror, but
/// a call-site role is never written in a placeholder — it is a property of where `ranting`'s own
/// renderer is in assembling the output. Like [`NounClass`], it is defined in `ranting` alone.
///
/// The roles are the fallback capitalization sites in `handle_placeholder`, in output order.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OrthographyRole {
    /// The article or demonstrative in front of the noun (`a`/`an`/`the`/`some`/`these`/`those`).
    Article,
    /// A verb, whether it precedes the noun (`` {are =noun} ``) or follows it
    /// (`` {=noun are} ``), including a tense marker's auxiliary+verb phrase.
    Verb,
    /// A pronoun rendered for a case marker (`` {=noun} ``, `` {@noun} ``, ...).
    Pronoun,
    /// The noun's own name, and a possessive noun phrase built from it (`` {`noun} `` in the
    /// pre-noun position).
    Noun,
    /// A `!`/`!!` degree adjective following the noun.
    Adjective,
}

/// An open-ended lexical-gender / noun-class label carried *by the entity*, for customization
/// via [`Ranting::inflect_article_custom`], [`Ranting::inflect_pronoun_custom`] and
/// [`Ranting::inflect_adjective_custom`].
///
/// Deliberately a newtype over a `&'static str` rather than a closed
/// `enum { Masculine, Feminine, Neuter }`: Bantu languages have a dozen-plus noun classes and
/// Danish has common/neuter, so an English-adjacent closed enum would be wrong on arrival.
/// `ranting` never interprets the label — it only carries it from the noun to the hook, exactly
/// like [`NarrationContext::dialect`]. What the classes *are* is the fork's business.
///
/// This is the channel that lets a fork stop keying gender off the display string. Before it, a
/// `ranting-german` had to keep an external `HashMap<&str, Gender>` looked up by
/// `noun_singular`, which breaks on homographs (`der Band`/`das Band`), on names, and on nouns
/// built at runtime. Gender is a property of the entity, like `subject`, and now lives there.
///
/// Note what `&'static str` does and does not make static: the *set of labels* a program uses
/// must be known at compile time (or leaked), but which label a given entity carries is ordinary
/// per-value data — a `Noun` built at runtime picks its class at runtime. That is what fixes the
/// homograph problem; it is not a promise of runtime-*computed* label strings.
///
/// # Examples
/// ```
/// # use ranting::*;
/// let hund = Noun::new("Hund", "he").with_noun_class(NounClass::new("masculine"));
/// assert_eq!(hund.noun_class().as_str(), "masculine");
///
/// let plain = Noun::new("dog", "it");
/// assert!(plain.noun_class().is_unset());
/// ```
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct NounClass(&'static str);

impl NounClass {
    /// No class declared — the default for every noun that doesn't set one, and what
    /// [`Ranting::noun_class`] returns unless overridden. Equal to `NounClass::new("")`: the
    /// empty label is *defined* to mean unset, so a fork matching on [`as_str`](Self::as_str)
    /// can treat `""` and "absent" as one case rather than two.
    pub const UNSET: NounClass = NounClass("");

    /// Label a noun's class. Any string is accepted; `ranting` attaches no meaning to it.
    /// `NounClass::new("")` is [`UNSET`](Self::UNSET).
    pub const fn new(label: &'static str) -> Self {
        NounClass(label)
    }

    /// The label as written, `""` when unset.
    pub const fn as_str(&self) -> &'static str {
        self.0
    }

    /// Whether no class was declared (the label is empty).
    pub const fn is_unset(&self) -> bool {
        self.0.is_empty()
    }
}

impl std::fmt::Display for NounClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

/// Which degree marker a post-noun adjective was written with, for customization via
/// [`Ranting::inflect_adjective_custom`]. Mirrors
/// [`placeholder::DegreeKind`], the compile-time type the
/// macro bakes, the way [`GrammaticalCase`] mirrors `CaseKind`.
///
/// There are only two variants because `!`/`!!` are the only two markers the placeholder grammar
/// has: an adjective written *without* a degree marker isn't a placeholder word at all (it's
/// literal template text, which no hook can reach), and a post-noun word with no marker is parsed
/// as a verb. A fork whose adjectives agree with their noun therefore writes `!` for the plain
/// (positive-degree) case too, and ignores this parameter — see `docs/EXTENSIBILITY.md` §2.5.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AdjectiveDegree {
    /// `` {noun !adj} `` — comparative ("better", "more careful").
    Comparative,
    /// `` {noun !!adj} `` — superlative ("best", "most careful").
    Superlative,
}

impl From<placeholder::DegreeKind> for AdjectiveDegree {
    fn from(degree: placeholder::DegreeKind) -> Self {
        match degree {
            placeholder::DegreeKind::Comparative => AdjectiveDegree::Comparative,
            placeholder::DegreeKind::Superlative => AdjectiveDegree::Superlative,
        }
    }
}

/// Which numeral notation a placeholder asked for, for customization via
/// [`Ranting::inflect_numeral_custom`]. Mirrors [`placeholder::NumeralKind`], the compile-time
/// type the macro bakes, the way [`GrammaticalCase`] mirrors `CaseKind` and [`AdjectiveDegree`]
/// mirrors `DegreeKind` — the `#`/`$` marker is written in the placeholder, so there is something
/// at the macro↔runtime seam to mirror.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumeralStyle {
    /// `` {#n nouns} `` — the number spelled out. English: "two".
    Words,
    /// `` {$n nouns} `` — the number in digits, the argument's own `Display` output with any
    /// `:fmt` spec applied. English: "2".
    Digits,
    /// `` {##n attempt} `` — ROADMAP.md Phase 8 item 4: the ordinal, spelled out. English:
    /// "third". Carries the same real `count` [`NumeralStyle::Words`] does, but — unlike
    /// `Words` — does not itself decide the noun's number agreement: an ordinal says *which*
    /// one, not *how many*, so `` {the ##n attempt} `` renders "the third attempt", never
    /// "attempts".
    Ordinal,
    /// `` {$$n attempt} `` — the ordinal, as digits with an English suffix. English: "3rd".
    /// Same agreement decoupling as [`NumeralStyle::Ordinal`].
    OrdinalDigits,
}

impl From<placeholder::NumeralKind> for NumeralStyle {
    fn from(kind: placeholder::NumeralKind) -> Self {
        match kind {
            placeholder::NumeralKind::Words => NumeralStyle::Words,
            placeholder::NumeralKind::Digits => NumeralStyle::Digits,
            placeholder::NumeralKind::Ordinal => NumeralStyle::Ordinal,
            placeholder::NumeralKind::OrdinalDigits => NumeralStyle::OrdinalDigits,
        }
    }
}

/// The numeral value backing a placeholder occurrence, when it has one. `None` for a placeholder
/// with no numeral at all (`` {noun} ``, `` {+noun} ``, `` {-noun} ``).
///
/// Passed to most `_custom`/`_with_context` hooks (and to [`Ranting::inflect`]) so a fork can
/// agree in number/case without re-parsing the template. The exception is
/// `inflect_numeral_custom`/`_with_context`, which already receives its own `count: Option<i64>`
/// plus the rendered numeral string, so a second count parameter there would be redundant.
///
/// # Why a struct and not a bare `i64`
/// English marks `1.0 inches` as plural but `1 inch` as singular, so whether a visible fraction
/// was written is load-bearing (CLDR's `one`/`other` split cares about this too). `fraction_digits`
/// carries exactly that: the count of digits actually rendered after a decimal point, `0` for a
/// plain integer.
// History: the count channel owed by ROADMAP.md Phase 6 item 4, closed by item 14 for five hook
// pairs, then extended by Phase 7 items 11, 12 and 26 to `Ranting::inflect`,
// `elide_numeral_custom` and `inflect_preposition_custom`. See
// `docs/superpowers/specs/2026-08-13-number-categories.md` Open Questions 1 and 2 for the design
// rationale behind the struct shape and the `inflect_numeral_custom` exception.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct PlaceholderCount {
    /// The integer value of the placeholder's numeral (`{$n noun}`/`{#n noun}`).
    pub value: i64,
    /// How many digits were actually rendered after a decimal point — `0` for a plain integer,
    /// e.g. `1` from `{$n noun}` with `n = 1` and no `:fmt`, or `1` from the same placeholder with
    /// `n = 1.0` and `:.1`.
    pub fraction_digits: u32,
}

impl From<placeholder::CaseKind> for GrammaticalCase {
    fn from(case: placeholder::CaseKind) -> Self {
        match case {
            placeholder::CaseKind::Name => GrammaticalCase::Name,
            placeholder::CaseKind::Subjective => GrammaticalCase::Subjective,
            placeholder::CaseKind::Objective => GrammaticalCase::Objective,
            placeholder::CaseKind::PossessiveDeterminer => GrammaticalCase::PossessiveDeterminer,
            placeholder::CaseKind::PossessivePronoun => GrammaticalCase::PossessivePronoun,
            placeholder::CaseKind::Reflexive => GrammaticalCase::Reflexive,
            placeholder::CaseKind::Hidden => GrammaticalCase::Hidden,
        }
    }
}

/// What a struct or enum must provide to be usable as a noun in a placeholder.
///
/// `#[derive_ranting]` implements it from the `#[ranting(...)]` attributes, which is the usual
/// way in. Implement it by hand, or override individual methods, when the entity's name or
/// pronouns are decided at runtime — or when the text isn't English, in which case the
/// `_custom` hooks below are where a language's own rules go.
///
/// # Examples
///
/// ```
/// # use std::str::FromStr;
/// # use ranting::*;
/// # use ranting_derive::*;
/// // Core attributes: subject, name, singular_end, plural_end
/// #[derive_ranting]
/// #[ranting(subject = "you", plural_you = true)]  // plural_you is a cosmetic attribute
/// struct OpponentTeam {}
///
/// #[derive_ranting]
/// #[ranting(subject = "he")]
/// struct ChessPlayer {}
///
/// fn big_words_to<T: Ranting>(who: T) -> String {
///     say!("I will grant {@0} {`0} fight, but {=0 are} going to lose today.", who)
/// }
///
/// # fn main() {
/// let team = OpponentTeam {};
/// assert_eq!(big_words_to(team),
///     "I will grant you your fight, but you are going to lose today.");
///
/// let magnus = ChessPlayer {};
/// assert_eq!(big_words_to(magnus),
///     "I will grant him his fight, but he is going to lose today.");
/// # }
/// ```
///
/// Using singular they for an individual with gender-neutral pronouns:
//
// The runnable copy of `derive_ranting`'s own doc example, which can't compile standalone in a
// proc-macro crate -- see CLAUDE.md's "Testing conventions".
///
/// ```
/// # use ranting::*;
/// # use ranting_derive::*;
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
///
/// # Parameters the hooks share
///
/// The `_custom` hooks below take the same few arguments, meaning the same thing in each. Their
/// own docs note only what is particular to them.
///
/// | Parameter | What it is |
/// |---|---|
/// | `class` | The entity's own noun class, or [`NounClass::UNSET`] when it declares none. Lets an implementation choose a form from the entity rather than guess from its spelling. |
/// | `case` | The grammatical role the placeholder's case marker wrote, as a [`GrammaticalCase`]. A bare `` {the noun} `` reports [`Name`](GrammaticalCase::Name), English having nothing more specific to report. |
/// | `count` | The placeholder's own numeral, as a [`PlaceholderCount`], and `None` when it wrote none — which is not the same as a count of one. |
/// | `as_plural` | Whether to render the plural *agreement* form; see [`is_plural`](Self::is_plural) for what that does and does not promise. |
/// | `uc` | Whether English would uppercase the first character. The caller applies it on the fallback path only, so a custom form must apply it itself — [`capitalize_if`] does that. |
// By overriding functions one can adapt default behavior, which affects placeholder rendering.
//
// ## Derive Attributes
//
// **Core attributes** (required for full functionality):
// - `subject`: The pronoun (I, you, he, she, it, we, ye, they)
// - `name`: The display name
// - `singular_end`: Suffix to strip when singularizing
// - `plural_end`: Suffix to add when pluralizing (default: "s")
// - `gender`: Lexical gender / noun class label, surfaced as `noun_class()` (default: "",
//   unset); "$" reads a `gender: ranting::NounClass` field
//
// **Cosmetic attributes** (optional, affect formatting):
// - `plural_you`: If subject is "you", whether it refers to plural (default: false)
// - `uc`: Whether name should always start uppercase (default: false)
// - `no_article`: Whether to skip articles (default: false)
pub trait Ranting: std::fmt::Display {
    /// The entity's display name, with its first character uppercased when `uc` is set.
    ///
    /// Derived from `#[ranting(name = "...")]`, or from the struct's own name when that
    /// attribute is absent.
    fn name(&self, uc: bool) -> String;

    /// The entity's subject pronoun: "I", "you", "thou", "he", "she", "it", "we", "ye" or
    /// "they". Every other pronoun a placeholder renders is inflected from this one.
    ///
    /// Derived from `#[ranting(subject = "...")]`, defaulting to "it".
    fn subjective(&self) -> &str;

    /// Whether the entity is plural, which decides the agreement form of every verb and
    /// pronoun rendered beside it.
    ///
    /// The subject pronoun answers this on its own, except for "you" — write
    /// `#[ranting(plural_you = true)]` for a "you" that addresses several.
    fn is_plural(&self) -> bool;

    /// The name in the number the placeholder asked for, uppercased when `uc` is set.
    ///
    /// English spelling rules apply unless the entity declares its own suffixes with
    /// `#[ranting(singular_end = "...", plural_end = "...")]`.
    // if name can change this should be overridden to lookup each singular_end and plural_end:
    ///
    /// English implementations can ignore `case` and `count`; both are here for languages that
    /// decline or count the noun itself.
    ///
    /// `case` is the placeholder's own grammatical role — the same [`GrammaticalCase`] handed to
    /// [`inflect_article_custom`](Self::inflect_article_custom) — so a declining language can
    /// honor the case marker the template wrote (`` {the =noun} `` vs. `` {the @noun} ``) on the
    /// noun as well as on its article: German's dative plural `den Hunden`, genitive `des Hauses`.
    ///
    /// `count` is the placeholder's own numeral, when it wrote one, and `None` when it wrote
    /// none — which is *not* the same as a count of one. A language with a third morphological
    /// number needs it: Arabic `{$n kitab}` with `n = 2` must render the dual `kitābān`, and
    /// `to_plural` alone can only ask for the plural `kutub`. Plural categories are deliberately
    /// left to the implementation; this hands over the raw count rather than a bucketed one.
    //
    // `case` arrived with ROADMAP.md Phase 6 item 14, `count` with Phase 7 item 11 — item 14 had
    // widened the five agreeing hook pairs but not `inflect`, the one call that renders the
    // counted noun, so a fork could agree in a third number everywhere except on the noun.
    // `to_plural` was not widened to an enum because that breaks every existing `match`. CLDR
    // categories stay out of the crate: docs/superpowers/specs/2026-08-13-number-categories.md.
    // `ranting_i18n`'s `GermanNoun::inflect` is the worked `case` example, declining by `self`'s
    // own entity-carried override first and this parameter second.
    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        case: GrammaticalCase,
        count: Option<PlaceholderCount>,
    ) -> String;
    /// Whether a placeholder should render no article for this entity — true for names,
    /// languages, meals and sports, which take one only for emphasis.
    ///
    /// Set it with `#[ranting(no_article = true)]`. A `!` in the placeholder's article slot
    /// overrides it and renders the article anyway.
    // examples: Names, languages, elements, food grains, meals (unless particular), sports.
    // if name can change and sometimes goes without article (e.g. a sport) lookup & override:
    fn skip_article(&self) -> bool;

    /// The noun's lexical gender / noun class, or [`NounClass::UNSET`] (the default) when it
    /// declares none. Set it with `#[ranting(gender = "...")]`, or on a [`Noun`] with
    /// [`Noun::with_noun_class`].
    ///
    /// `ranting` itself never reads the value — English has no lexical gender. It exists to be
    /// handed to [`inflect_article_custom`](Self::inflect_article_custom),
    /// [`inflect_pronoun_custom`](Self::inflect_pronoun_custom) and
    /// [`inflect_adjective_custom`](Self::inflect_adjective_custom) so a non-English implementation
    /// can select `der`/`die`/`das` from the entity rather than from an external table keyed by
    /// the display string. See [`NounClass`] for why the label is open-ended.
    fn noun_class(&self) -> NounClass {
        NounClass::UNSET
    }

    /// Whether the entity is a mass noun ("information", "water") rather than a count noun
    /// ("item", "boot") -- orthogonal to [`NounClass`]: a word can be both, since many languages
    /// have a lexical gender for their mass nouns too (German *das Wasser* is neuter and mass).
    ///
    /// `false` by default, so no existing entity's rendering changes. Set it with
    /// `#[ranting(mass)]`, or on a [`Noun`] with [`Noun::with_mass`]. `ranting` reads it in
    /// exactly two places: the `a`/`an`/`some` article slot renders `some` on a mass noun's
    /// singular instead of guessing `a`/`an` (`` {a 0} `` on "information" would otherwise render
    /// "An information"), and the `much`/`many` and `less`/`fewer` quantifier pairs pick their
    /// mass-noun member.
    fn is_mass(&self) -> bool {
        false
    }

    /// Whether `subject` counts as first-person (the narrator) for
    /// [`NarrationContext::narration_person`](crate::NarrationContext) viewpoint overrides —
    /// consulted by `say_with!()`'s viewpoint resolution before anything else, so overriding
    /// this is enough to make a non-English first-person label (`ich`, `wir`, …) participate
    /// in viewpoint retelling. The default is English's own rule, `subject == "I" || subject ==
    /// "we"`.
    ///
    /// `subject` is a parameter rather than read off `self.subjective()` for the same reason
    /// `inflect_verb_custom` takes an explicit `subject`: wrapper types ([`Many`], [`Maybe`],
    /// `Box`) delegate a hook call to an inner value, and the caller — not the callee — decides
    /// which entity's declared subject is in play.
    fn is_first_person_subject_custom(&self, subject: &str) -> bool {
        ranting_core::grammar::is_first_person_subject(subject)
    }

    /// Customize verb conjugation (tense, plurality, person).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// Called for every verb in a placeholder, whether it precedes or follows the noun.
    ///
    /// One caveat at tense-marker sites (`{=0 >walk}`): the verb arrives *already conjugated*
    /// by the macro, so `verb` may be "walked" or "walking" rather than the form written in the
    /// placeholder — match accordingly if you customize those. The auxiliary ("will", "is", …)
    /// is composed around whatever you return, and capitalization is applied by the caller, so
    /// `uc` is false there.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool, count: Option<PlaceholderCount>, uc: bool) -> Option<String> {
    ///     match verb {
    ///         "be" => Some("be".to_string()),  // Pirate: use "be" for all forms
    ///         _ => None,  // Fall back to English for other verbs
    ///     }
    /// }
    /// ```
    fn inflect_verb_custom(
        &self,
        _subject: &str,
        _verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_verb_custom`](Self::inflect_verb_custom), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one.
    ///
    /// Every call site that conjugates a verb calls this instead of `inflect_verb_custom`
    /// directly (`say!()` calls it with `ctx: None`, `say_with!()` with `ctx: Some(_)`), so
    /// overriding this hook alone is enough — you don't need both. The default implementation
    /// ignores `ctx` and delegates to `inflect_verb_custom`, so existing implementations that
    /// only override the non-context hook keep working unchanged.
    ///
    /// `ctx` is a parameter, not something read off `self` — an entity's own `subject` stays a
    /// property of the entity, while tense/viewpoint/register/dialect are settings of the
    /// telling, which may differ per `say_with!()` call rather than per noun.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_verb_custom_with_context(
    ///     &self,
    ///     subject: &str,
    ///     verb: &str,
    ///     as_plural: bool,
    ///     count: Option<PlaceholderCount>,
    ///     uc: bool,
    ///     ctx: Option<&NarrationContext>,
    /// ) -> Option<String> {
    ///     match (verb, ctx.and_then(|c| c.register)) {
    ///         ("be", Some(Register::Formal)) => Some(capitalize_if("shall be", uc)),
    ///         _ => self.inflect_verb_custom(subject, verb, as_plural, count, uc),
    ///     }
    /// }
    /// ```
    fn inflect_verb_custom_with_context(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_verb_custom(subject, verb, as_plural, count, uc)
    }

    /// Customize pronoun inflection (subject/object/possessive forms).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// # Arguments
    /// * `subject` - Subject pronoun (e.g., "I", "he", "they")
    /// * `case` - Which pronoun form: Subjective, Objective, PossessiveDeterminer, or PossessivePronoun
    /// * `class` - Lets a gendered pronoun be picked from the entity rather than guessed from
    ///   its display string.
    /// * `as_plural`, `count`, `uc` - As for every hook; pronoun agreement in Arabic and Slavic
    ///   is number-sensitive beyond singular/plural, which is why this one carries `count` too.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase, class: NounClass, as_plural: bool, count: Option<PlaceholderCount>, uc: bool) -> Option<String> {
    ///     match (case, class.as_str()) {
    ///         // German: a neuter noun is "es", whatever English pronoun it was declared with.
    ///         (PronounCase::Subjective, "neuter") => Some(capitalize_if("es", uc)),
    ///         (PronounCase::Subjective, "feminine") => Some(capitalize_if("sie", uc)),
    ///         _ => None,  // Fall back to English
    ///     }
    /// }
    /// ```
    #[allow(clippy::too_many_arguments)]
    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        _case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_pronoun_custom`](Self::inflect_pronoun_custom), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: every pronoun call site calls this one, and the default delegates to
    /// `inflect_pronoun_custom` with `ctx` ignored.
    #[allow(clippy::too_many_arguments)]
    fn inflect_pronoun_custom_with_context(
        &self,
        subject: &str,
        case: PronounCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_pronoun_custom(subject, case, class, as_plural, count, uc)
    }

    /// Customize article inflection (a/an/the/some, demonstratives, etc.).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// # Arguments
    /// * `article` - Requested article form (e.g., "a", "the", "some", "these", "those")
    /// * `noun_singular` - Singular form of noun (for vowel detection, gender agreement, etc.)
    /// * `case` - The noun's own grammatical role, from its case marker if it has one (see
    ///   [`GrammaticalCase`] — a bare `` {the noun} `` reports `GrammaticalCase::Name`, since
    ///   English gives nothing more specific to report). Lets a case-declining language's fork
    ///   pick e.g. German `der`/`den`/`dem` correctly when the template annotates the noun's
    ///   role explicitly (`` {the =noun} `` vs `` {the @noun} ``).
    /// * `class` - Together with `case`, what makes `der`/`die`/`das` reachable from the entity
    ///   alone. A gender table keyed by `noun_singular` instead would break on homographs,
    ///   names, and nouns built at runtime.
    /// * `case`, `as_plural`, `count`, `uc` - As for every hook.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_article_custom(&self, article: &str, noun_singular: &str, case: GrammaticalCase, class: NounClass, as_plural: bool, count: Option<PlaceholderCount>, uc: bool) -> Option<String> {
    ///     match article {
    ///         "the" => {
    ///             // German definite article, declined by class and case.
    ///             let form = match (class.as_str(), case) {
    ///                 _ if as_plural => "die",
    ///                 ("masculine", GrammaticalCase::Objective) => "den",
    ///                 ("masculine", _) => "der",
    ///                 ("feminine", _) => "die",
    ///                 _ => "das",
    ///             };
    ///             Some(capitalize_if(form, uc))
    ///         }
    ///         _ => None,  // Fall back to English for a/an/some
    ///     }
    /// }
    /// ```
    #[allow(clippy::too_many_arguments)]
    fn inflect_article_custom(
        &self,
        _article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_article_custom`](Self::inflect_article_custom), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: every article call site calls this one, and the default delegates to
    /// `inflect_article_custom` with `ctx` ignored.
    #[allow(clippy::too_many_arguments)]
    fn inflect_article_custom_with_context(
        &self,
        article: &str,
        noun_singular: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_article_custom(article, noun_singular, case, class, as_plural, count, uc)
    }

    /// Elide or fuse a rendered article with the word that follows it.
    ///
    /// This is the one hook that runs *after* assembly rather than instead of it: by the time it
    /// is called the article and the noun (or number, or pronoun) have both been rendered, so a
    /// fork can see them together. That is what English `a`/`an` needs and what
    /// [`inflect_article_custom`](Self::inflect_article_custom) structurally cannot give — that
    /// hook returns its string before the following text exists.
    ///
    /// Return `Some(String)` to replace the article, the separator *and* the following text with
    /// one fused string; return `None` (the default) to keep them exactly as rendered, which is
    /// what keeps English output byte-identical. English needs nothing here: `a`/`an` is chosen
    /// from the singular noun inside `get_article_or_so` and never reaches this hook.
    ///
    /// # Arguments
    /// * `article` - The article exactly as it was rendered, capitalization included — whether
    ///   that came from `inflect_article_custom` or the English fallback. There is deliberately
    ///   no `uc` parameter: the word is already capitalized, so `uc` would have nothing left to
    ///   decide. A fork that needs to re-case its fused form can inspect the first character or
    ///   call [`capitalize`](Self::capitalize) itself.
    /// * `separator` - The whitespace rendered between article and following text (usually `" "`).
    ///   French `l'homme` returns a form that drops it; `le chien` returns `None` and keeps it.
    /// * `following` - Everything the placeholder rendered between that separator and the
    ///   post-noun slot, i.e. whatever is actually adjacent to the article: any words the
    ///   placeholder's own pre-text carried after it (`` {a set of $n chiens} `` gives
    ///   `"set of 2 chiens"`), then the number when there is one, then the noun name or the
    ///   case-selected pronoun. It is the rendered text, not the dictionary form — that is the
    ///   point of running after assembly.
    /// * `case` / `class` / `as_plural` / `count` - As for
    ///   [`inflect_article_custom`](Self::inflect_article_custom). No `uc` parameter here either,
    ///   for the same reason as `article`: `uc` has already been reset to `false` by the splice
    ///   point.
    ///
    /// # Not this hook's job
    /// Preposition-article fusion across a placeholder boundary (French `de` + `le` → `du`,
    /// Italian `di` + `il` → `del`) is not expressible here: the preposition lives in the
    /// template's literal text, outside the placeholder, and this hook's span starts at the
    /// article. Use [`inflect_preposition_custom`](Self::inflect_preposition_custom) instead —
    /// it runs first, at the same post-assembly point, and when it fires this hook is skipped,
    /// the article it would have elided against being gone. Neither hook is called for a hidden
    /// noun (`` {?the noun} ``), which renders nothing to elide against.
    ///
    /// # Examples
    /// ```ignore
    /// fn elide_article_custom(&self, article: &str, _separator: &str, following: &str, _case: GrammaticalCase, _class: NounClass, _as_plural: bool, _count: Option<PlaceholderCount>) -> Option<String> {
    ///     let elides = matches!(following.chars().next(), Some(c) if "aeiouhâàéèêîôûAEIOUH".contains(c));
    ///     match article {
    ///         "le" | "la" if elides => Some(format!("l'{following}")),
    ///         _ => None, // keep article, separator and following exactly as rendered
    ///     }
    /// }
    /// ```
    #[allow(clippy::too_many_arguments)]
    fn elide_article_custom(
        &self,
        _article: &str,
        _separator: &str,
        _following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
    ) -> Option<String> {
        None
    }

    /// Like [`elide_article_custom`](Self::elide_article_custom), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: the elision call site calls this one, and the default delegates to
    /// `elide_article_custom` with `ctx` ignored.
    #[allow(clippy::too_many_arguments)]
    fn elide_article_custom_with_context(
        &self,
        article: &str,
        separator: &str,
        following: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.elide_article_custom(article, separator, following, case, class, as_plural, count)
    }

    /// Fuse a rendered numeral with the noun that follows it — the numeral-side twin of
    /// [`elide_article_custom`](Self::elide_article_custom).
    ///
    /// `numeral` is what the numeral slot rendered (a fork's own
    /// [`inflect_numeral_custom`](Self::inflect_numeral_custom) output, or English's), `separator`
    /// the whitespace between it and the noun, and `following` the rest of the placeholder's own
    /// output. Returning `Some` replaces **all three**; returning `None` — the default, and every
    /// case English needs — leaves them exactly as rendered.
    ///
    /// A language that writes a counter phrase and its noun as one run needs this: Japanese
    /// 「一匹の猫」 has no space anywhere, and without the hook the separator is in the output
    /// with no way to remove it.
    ///
    /// Called first of the three post-assembly splices — ahead of preposition fusion and article
    /// elision — because `[preposition][article][numeral][noun]` makes the numeral-noun boundary
    /// the innermost of the three. It is **not** called for a hidden numeral (`` {?$n noun} ``),
    /// which renders nothing to fuse — the same gate a hidden noun gives the article hook.
    ///
    /// ```ignore
    /// // Japanese: the counter phrase and its noun are written as one run.
    /// fn elide_numeral_custom(
    ///     &self, numeral: &str, _separator: &str, following: &str,
    ///     _case: GrammaticalCase, _class: NounClass, _as_plural: bool,
    ///     _count: Option<PlaceholderCount>,
    /// ) -> Option<String> {
    ///     Some(format!("{numeral}{following}"))   // 一匹の + 猫 -> 一匹の猫
    /// }
    /// ```
    #[allow(clippy::too_many_arguments)]
    fn elide_numeral_custom(
        &self,
        _numeral: &str,
        _separator: &str,
        _following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
    ) -> Option<String> {
        None
    }

    /// Like [`elide_numeral_custom`](Self::elide_numeral_custom), but also receives the
    /// [`NarrationContext`] when the call came from `say_with!()`. Defaults to delegating, so
    /// overriding either one alone is enough.
    #[allow(clippy::too_many_arguments)]
    fn elide_numeral_custom_with_context(
        &self,
        numeral: &str,
        separator: &str,
        following: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.elide_numeral_custom(numeral, separator, following, case, class, as_plural, count)
    }

    /// Fuse a literal preposition, written in the template immediately before a placeholder, with
    /// the article that placeholder renders.
    ///
    /// German `zu` + `dem` → `zum`, Spanish `de` + `el` → `del`. The preposition is literal
    /// template text outside the placeholder's `{...}`, which is why
    /// [`elide_article_custom`](Self::elide_article_custom) cannot reach it: the macro captures
    /// that word and forwards it here as data rather than baking it as inert text.
    ///
    /// Called at the same post-assembly point as `elide_article_custom`, and *before* it: if this
    /// hook returns `Some`, the preposition and the article it consumed are both replaced and
    /// `elide_article_custom` is not called at all (the article no longer exists to elide). If it
    /// returns `None` — the default, and every case English needs, since English never fuses a
    /// preposition with an article — the preposition is rendered exactly as written and
    /// `elide_article_custom` still gets its normal chance at the (untouched) article, so English
    /// output is byte-identical either way.
    ///
    /// # Arguments
    /// * `preposition` - The literal word exactly as written in the template (capitalization
    ///   included), with no trailing whitespace.
    /// * `article` - The article exactly as it was rendered — whether that came from
    ///   `inflect_article_custom` or the English fallback — with no leading/trailing whitespace.
    /// * `case` / `class` / `as_plural` / `count` - As for
    ///   [`inflect_article_custom`](Self::inflect_article_custom).
    /// * `uc` - Whether the placeholder itself would force an uppercase first character (a `^`/`,`
    ///   marker, or sentence position) — the preposition's own capitalization is not this hook's
    ///   concern (it is rendered exactly as the caller typed it when this hook declines), but a
    ///   fused replacement starting a sentence may need it.
    ///
    /// # Not reachable from here
    /// Only the single literal word immediately adjacent to the placeholder (whitespace-separated,
    /// nothing else between it and `{`) is ever captured — a multi-word preposition, or one
    /// separated from the placeholder by punctuation or an adverb, is not. A hidden noun
    /// (`` {?the noun} ``) renders no article to fuse against, so the hook is not called there
    /// either, same as `elide_article_custom`.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_preposition_custom(&self, preposition: &str, article: &str, _case: GrammaticalCase, _class: NounClass, _as_plural: bool, _count: Option<PlaceholderCount>, _uc: bool) -> Option<String> {
    ///     match (preposition, article) {
    ///         ("de", "el") => Some("del".to_string()),
    ///         ("a", "el") => Some("al".to_string()),
    ///         _ => None, // de la / de los / de las / a la / ... are already correct unfused
    ///     }
    /// }
    /// ```
    #[allow(clippy::too_many_arguments)]
    fn inflect_preposition_custom(
        &self,
        _preposition: &str,
        _article: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_preposition_custom`](Self::inflect_preposition_custom), but also receives
    /// the [`NarrationContext`] in effect for this call, when there is one. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: the preposition-fusion call site calls this one, and the default delegates
    /// to `inflect_preposition_custom` with `ctx` ignored.
    #[allow(clippy::too_many_arguments)]
    fn inflect_preposition_custom_with_context(
        &self,
        preposition: &str,
        article: &str,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_preposition_custom(preposition, article, case, class, as_plural, count, uc)
    }

    /// Customize a post-noun adjective (the `!`/`!!` degree slot).
    /// Return Some(String) to use a custom form, None to keep the compile-time-resolved English
    /// comparative/superlative.
    ///
    /// English resolves degree entirely at compile time and needs no agreement, so this hook has
    /// no English use and returning `None` — the default — leaves `say!()`'s output byte-identical.
    /// It exists for Romance and Germanic adjectives, which agree with their noun in gender,
    /// number and (German) case: `un chat noir` / `une robe noire` / `des chats noirs`, none of
    /// which is knowable when the macro bakes the degree form.
    ///
    /// # Arguments
    /// * `adjective` - The adjective exactly as written in the placeholder, *before* the English
    ///   degree table touched it (`` {a chat !noir} `` gives `"noir"`, not `"more noir"`). This is
    ///   what a fork inflects; the English form it would otherwise get is not reversible back into
    ///   the base.
    /// * `degree` - Which marker was written, `!` or `!!` (see [`AdjectiveDegree`], including why
    ///   there is no positive-degree variant).
    /// * `case`, `class`, `as_plural`, `count`, `uc` - As for every hook; `class` and
    ///   `as_plural` together are the agreement input an adjective usually needs.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_adjective_custom(
    ///     &self,
    ///     adjective: &str,
    ///     _degree: AdjectiveDegree,
    ///     _case: GrammaticalCase,
    ///     class: NounClass,
    ///     as_plural: bool,
    ///     _count: Option<PlaceholderCount>,
    ///     uc: bool,
    /// ) -> Option<String> {
    ///     // French: noir / noire / noirs / noires
    ///     let mut form = adjective.to_string();
    ///     if class.as_str() == "feminine" {
    ///         form.push('e');
    ///     }
    ///     if as_plural {
    ///         form.push('s');
    ///     }
    ///     Some(capitalize_if(&form, uc))
    /// }
    /// ```
    #[allow(clippy::too_many_arguments)]
    fn inflect_adjective_custom(
        &self,
        _adjective: &str,
        _degree: AdjectiveDegree,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_adjective_custom`](Self::inflect_adjective_custom), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: the adjective call site calls this one, and the default delegates to
    /// `inflect_adjective_custom` with `ctx` ignored.
    #[allow(clippy::too_many_arguments)]
    fn inflect_adjective_custom_with_context(
        &self,
        adjective: &str,
        degree: AdjectiveDegree,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        count: Option<PlaceholderCount>,
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_adjective_custom(adjective, degree, case, class, as_plural, count, uc)
    }

    /// Customize how a placeholder's number is written (the `#var`/`$var` slot).
    /// Return `Some(String)` to use a custom numeral, `None` to keep the English rendering.
    ///
    /// English needs nothing here: `#var` is spelled by `english-numbers` and `$var` is the
    /// argument's own `Display` output, and returning `None` — the default — leaves `say!()`'s
    /// output byte-identical. It exists because both of those are hard-coded English/ASCII
    /// choices. Every other language needs its own speller (`zwei`, `deux`, `два`), several
    /// agree the numeral itself with the noun's gender and case (Russian `два стола` vs. `две
    /// книги`), and several scripts have their own digits (Devanagari `२`, Arabic-Indic `٢`).
    ///
    /// # Arguments
    /// * `numeral` - The number as English renders it, i.e. what is used if this returns `None`:
    ///   the spelled-out word for [`NumeralStyle::Words`], or the already-formatted digits
    ///   (`:fmt` spec applied) for [`NumeralStyle::Digits`]. A digit-mapping fork can transcribe
    ///   this directly; a fork that spells numbers wants `count` instead. A negative count
    ///   arrives with its sign spelled as a word — `-1` is `"minus one"` — in this one string,
    ///   which a returned `Some` replaces whole, sign included.
    /// * `count` - The number itself, when it is available. Always `Some` for
    ///   [`NumeralStyle::Words`], where the macro bakes the same `as i64` cast it always applied
    ///   before spelling. For [`NumeralStyle::Digits`] it is recovered by parsing `numeral`, so
    ///   it is `None` whenever that isn't a plain integer — a float, a width-padded or otherwise
    ///   formatted number, or a non-numeric `Display` argument. This count is *local to the
    ///   numeral*: agreement in a number category (dual, paucal) on the noun, article and verb
    ///   is what the other hooks' own `count` parameter is for.
    /// * `style` - Which of `#var`/`$var` was written (see [`NumeralStyle`]).
    /// * `case`, `class` - As for every hook. Russian declines the numeral by case, and picks
    ///   `два` or `две` by class.
    /// * `as_plural` - As for every hook, but decided *before* this hook runs, from the count
    ///   rather than from the rendered word, so a custom numeral can never flip it.
    ///
    /// There is deliberately no `uc` parameter: capitalization stays entirely on the crate side
    /// of this hook, applied to whatever it returns (or to the English fallback) rather than
    /// delegated to it. A sentence-initial placeholder with no preceding article or verb spends
    /// its capital on the numeral — [`NumeralStyle::Words`] gets it capitalized
    /// (`"Two items fell."`), [`NumeralStyle::Digits`] simply drops it, since a digit can't be
    /// capitalized (`"2 items fell."`, not `"2 Items fell."`). A fork's returned string is
    /// capitalized the same way an unmodified English one would be, so it never needs its own
    /// case logic for this. Note also that a returned string replaces the rendered numeral
    /// outright, so a `:fmt` width/fill spec on `$var` is *not* re-applied to it — a fork that
    /// wants padding pads its own output.
    ///
    /// Not called at all when nothing numeric renders: a placeholder without a `#var`/`$var`
    /// marker, or with a hidden one (`` {?$n nouns} ``, where the number governs agreement but is
    /// not written). `heed!()`/`ask!()`'s `{$name}` is input parsing, the inverse direction, and
    /// is not routed here either.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_numeral_custom(
    ///     &self,
    ///     numeral: &str,
    ///     count: Option<i64>,
    ///     style: NumeralStyle,
    ///     _case: GrammaticalCase,
    ///     class: NounClass,
    ///     _as_plural: bool,
    /// ) -> Option<String> {
    ///     match style {
    ///         // Russian: the numeral "two" agrees with its noun's gender.
    ///         NumeralStyle::Words => Some(match (count?, class.as_str()) {
    ///             (1, "feminine") => "одна".to_string(),
    ///             (1, _) => "один".to_string(),
    ///             (2, "feminine") => "две".to_string(),
    ///             (2, _) => "два".to_string(),
    ///             (n, _) => n.to_string(),
    ///         }),
    ///         // Devanagari digits: a transcription of what English rendered.
    ///         NumeralStyle::Digits => Some(numeral.chars().map(|c| match c {
    ///             '0'..='9' => char::from_u32(c as u32 - '0' as u32 + 0x966).unwrap_or(c),
    ///             other => other,
    ///         }).collect()),
    ///         // Ordinals: keep the English rendering for this sketch.
    ///         NumeralStyle::Ordinal | NumeralStyle::OrdinalDigits => None,
    ///     }
    /// }
    /// ```
    fn inflect_numeral_custom(
        &self,
        _numeral: &str,
        _count: Option<i64>,
        _style: NumeralStyle,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_numeral_custom`](Self::inflect_numeral_custom), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one — which is
    /// where a locale (`NarrationContext::dialect`) selecting a digit system would live. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: the numeral call site calls this one, and the default delegates to
    /// `inflect_numeral_custom` with `ctx` ignored.
    #[allow(clippy::too_many_arguments)]
    fn inflect_numeral_custom_with_context(
        &self,
        numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_numeral_custom(numeral, count, style, case, class, as_plural)
    }

    /// Apply orthographic capitalization to one rendered piece of a placeholder.
    ///
    /// Unlike the `inflect_*_custom` hooks this returns a `String`, not an `Option`: it *is* the
    /// fallback, not a chance to decline one. It is called on every piece a placeholder renders
    /// through English's own rules, and its default is exactly [`capitalize_if`], so overriding
    /// nothing leaves `say!()`'s output unchanged.
    ///
    /// It exists because sentence-start uppercasing is an English orthographic assumption baked
    /// into the crate. German capitalizes every noun regardless of sentence position; Japanese,
    /// Chinese, Arabic and Hebrew have no letter case at all, so `uc` is meaningless and the
    /// honest implementation returns `word` unchanged; Turkish needs `i`→`İ` and `ı`→`I`, which
    /// [`char::to_uppercase`] gets wrong for a Turkish locale.
    ///
    /// The hook decides what is *done* with `uc`; it does not decide `uc` itself. Whether a
    /// placeholder is at a sentence start, and the `,`/`^` markers that force lower/uppercase,
    /// are resolved by the macro at compile time and arrive here as the `uc` bool.
    ///
    /// Note this is capitalization by *sentence position*, not case preservation of a word's own
    /// spelling: `apply_case` in `src/language/plurals.rs`, which keeps an irregular plural's
    /// ALL-CAPS/Title/lowercase pattern, is reached through the `self`-less free function
    /// [`inflect_noun_irregular`] and is not routed here.
    ///
    /// # Arguments
    /// * `word` - The rendered text, uncapitalized. For [`OrthographyRole::Article`] it may carry
    ///   the trailing space that separates it from the noun.
    /// * `role` - Which part of the placeholder this is (see [`OrthographyRole`]), so a fork can
    ///   capitalize nouns always and everything else only sentence-initially.
    /// * `uc` - What English would do: uppercase the first character.
    /// * `sentence_start` - Whether the placeholder is sentence-initial, independent of `uc`.
    ///   The two differ because `uc` also means "forced uppercase by a `^`/`,` marker or an
    ///   uppercase pre-text word": mid-sentence, `` {The noun} `` has `uc == true` and
    ///   `sentence_start == false`. Ignore it if you only care about letter case; it is here for
    ///   a caseless-script fork that still needs sentence boundaries for its own punctuation.
    ///
    /// One exception to "`word` arrives uncapitalized": at [`OrthographyRole::Noun`] the name has
    /// already been through [`inflect`](Self::inflect), which takes `uc` itself and is
    /// user-implementable, so English capitalization is spent by then and `uc` is reported as
    /// `false`. (It is also not simply [`capitalize_if`]: a derive-generated `name()` for
    /// `#[ranting(name = "designer")]` reads `uc == true` as "as written", not "force
    /// uppercase".) An always-capitalize fork ignores `uc` and is unaffected; a fork that needs
    /// *position-sensitive* noun casing overrides `name`/`inflect` instead. `sentence_start` is
    /// unaffected by this exception — it still reports the placeholder's real sentence position.
    ///
    /// # Examples
    /// ```ignore
    /// fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool, sentence_start: bool) -> String {
    ///     // German: nouns are capitalized wherever they stand, everything else only
    ///     // sentence-initially.
    ///     match role {
    ///         OrthographyRole::Noun => capitalize_if(word, true),
    ///         _ => capitalize_if(word, uc),
    ///     }
    /// }
    /// ```
    fn capitalize(
        &self,
        word: &str,
        _role: OrthographyRole,
        uc: bool,
        _sentence_start: bool,
    ) -> String {
        capitalize_if(word, uc)
    }

    /// Like [`capitalize`](Self::capitalize), but also receives the
    /// [`NarrationContext`] in effect for this call, when there is one — which is where a
    /// locale (`NarrationContext::dialect`, e.g. `"tr"`) would live for a fork that needs
    /// Turkish dotted/dotless `i`. See
    /// [`inflect_verb_custom_with_context`](Self::inflect_verb_custom_with_context) for the
    /// general shape: every call site calls this one, and the default delegates to `capitalize`
    /// with `ctx` ignored.
    fn capitalize_with_context(
        &self,
        word: &str,
        role: OrthographyRole,
        uc: bool,
        sentence_start: bool,
        _ctx: Option<&NarrationContext>,
    ) -> String {
        self.capitalize(word, role, uc, sentence_start)
    }
}

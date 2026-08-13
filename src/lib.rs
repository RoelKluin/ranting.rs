// (c) Roel Kluin 2022 MIT
//!
//! Functions to handle [Ranting](https://docs.rs/ranting_derive/0.2.1/ranting_derive/) trait placeholders.
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

#[doc(hidden)]
pub use english_numbers::convert_no_fmt as rant_convert_numbers;

// required for ranting_derive
#[doc(hidden)]
pub use strum_macros as rant_strum_macros;

use in_definite::get_a_or_an;
use language::english::{
    adapt_article, inflect_adjective, inflect_objective, inflect_subjective, inflect_verb,
};
pub use language::english::{inflect_noun_irregular, inflect_possessive, inflect_reflexive};
pub use ranting_core::grammar::{SubjectPronoun, is_subject, is_subjective_plural};
use ranting_core::placeholder::{ArticleKind, CaseKind, PlaceholderSpec, PostSpec};
use std::str::FromStr;

/// Typed placeholder-spec types (`PlaceholderSpec`, `CaseKind`, `PostSpec`,
/// `TenseMarker`) that `ranting_derive` bakes at compile time and
/// `handle_placeholder`/`handle_placeholder_with_context` consume at
/// runtime. Not part of the stable public API surface in the usual sense --
/// exposed so macro-generated code (which only has a `ranting::` path to
/// work with) can name these types -- but not `#[doc(hidden)]` either,
/// since understanding this module explains the `say!()`/`say_with!()`
/// macro <-> runtime seam for anyone extending the grammar (see CLAUDE.md's
/// "Macro flow" section).
pub use ranting_core::placeholder;

#[doc(hidden)]
pub use heed::HeedMatcher;

// TODO: make this a feature:
//pub(crate) use strum_macros;

/// Expands to `Ok(say!(...))` — a plain expression, not a hidden `return`.
/// Callers that want early-return behavior write `return ack!(...)` themselves;
/// the macro can also be used anywhere an expression is valid (e.g. bound to a
/// `let`, or as the tail expression of a block).
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, ack, Ranting};
/// fn question(harr: Noun, friends: Noun, lad: Noun) -> Result<String, String> {
///     return ack!("{harr shall} {+=friends do} with {the drunken *lad}?");
/// }
///
/// # fn main() {
/// let harr = Noun::new("what", "it");
/// let friends = Noun::new("crew", "we");
/// let lad = Noun::new("sailor", "he");
///
/// assert_eq!(
///     question(harr, friends, lad),
///     Ok("What shall we do with the drunken sailor?".to_string())
/// );
/// # }
/// ```
pub use ranting_derive::ack;

/// Expands to `Err(say!(...))` — a plain expression, not a hidden `return`.
/// Callers that want early-return behavior write `return nay!(...)` themselves;
/// the macro can also be used anywhere an expression is valid (e.g. bound to a
/// `let`, or as the tail expression of a block).
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, nay, Ranting};
/// fn home(p: Noun) -> Result<String, String> {
///     return nay!("{=p can't} get in {`p} house.");
/// }
///
/// # fn main() {
/// assert_eq!(
///     home(Noun::new("Jo", "she")),
///     Err("She can't get in her house.".to_string())
/// );
/// # }
/// ```
pub use ranting_derive::nay;

/// Functions like `format!()` for normal placeholders, but allows extended placeholders including
/// e.g. articles or verbs beside a Noun or a variable with the Ranting trait. These are inflected
/// accordingly, and adjustable by punctuation prefixes.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn inflect(with: Noun) -> String {
///     let n = Noun::new("noun", "it");
///     say!("{some n} with {0} {?n inflect} as {=0}, {@0}, {`0} and {~0}.", with)
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "he", "she", "it", "we", "they"]
///     .iter()
///     .map(|s| inflect(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "A noun with subject I inflects as I, me, my and mine.\
///     A noun with subject you inflects as you, you, your and yours.\
///     A noun with subject he inflects as he, him, his and his.\
///     A noun with subject she inflects as she, her, her and hers.\
///     A noun with subject it inflects as it, it, its and its.\
///     A noun with subject we inflects as we, us, our and ours.\
///     A noun with subject they inflects as they, them, their and theirs."
///     .to_string());
/// # }
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

/// heed!(template, input) — scanf-like input parsing; see `ranting_derive::heed`.
pub use ranting_derive::heed;

/// `#[derive(Heed)]` — v2 of `heed!()`: put `#[heed(template = "...")]` on a
/// struct with named fields (`String` for `{name}`/`{name...}`, `u64` for
/// `{$name}`) and it gains `fn heed(input: &str) -> Option<Self>`, built on
/// the same template compiler as `heed!()`. See `ranting_derive::Heed`.
pub use ranting_derive::Heed;

/// ask!(speaker, audience, template, input) — parses `input` against `template`
/// like `heed!()`, then forwards the captures to `audience`'s [`Answerable::answer`],
/// returning `Option<String>` (`None` on no match). See `ranting_derive::ask`.
pub use ranting_derive::ask;

/// If you want to implement Ranting on a `Box<&dyn Trait>` where Trait has Ranting
pub use ranting_derive::boxed_ranting_trait;

/// If you want to implement Ranting on a `&'_ dyn Trait` where Trait has Ranting
pub use ranting_derive::ref_ranting_trait;

/// How to render a noun's article at one call site — bundled to keep
/// `get_article_or_so` under clippy's argument-count limit now that
/// `case: GrammaticalCase` is threaded through alongside plurality/case/context.
struct ArticleRenderCtx<'a> {
    case: CaseKind,
    as_pl: bool,
    uc: bool,
    ctx: Option<&'a NarrationContext>,
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
        ctx,
    } = render;
    if noun.skip_article() && !s.starts_with('!') && !matches!(kind, ArticleKind::TheseThose) {
        return Some("".to_string());
    }
    let article_form = s.trim_start_matches('!');
    let case: GrammaticalCase = case.into();
    // Read off the noun rather than plumbed through `ArticleRenderCtx`: the class is a property
    // of the entity, and every article hook call in this function already has `noun` in hand.
    let class = noun.noun_class();
    match kind {
        ArticleKind::The => {
            // "the" needs no singular of its own; deriving one via inflect() would panic for
            // plural nouns whose name cannot be singularized (e.g. Noun::new("one", "they")).
            let singular = noun.name(false);
            if let Some(custom) = noun
                .inflect_article_custom_with_context("the", &singular, case, class, as_pl, uc, ctx)
            {
                Some(custom + space)
            } else {
                Some(
                    noun.capitalize_with_context(article_form, OrthographyRole::Article, uc, ctx)
                        + space,
                )
            }
        }
        ArticleKind::AAnSome => {
            // adapt_article() discards the a/an choice when as_pl, so only singularize when the
            // singular form is actually rendered; inflect() would panic on non-standard plurals.
            let singular = if as_pl {
                noun.name(false)
            } else {
                noun.inflect(false, false)
            };
            if let Some(custom) = noun.inflect_article_custom_with_context(
                article_form,
                &singular,
                case,
                class,
                as_pl,
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                // `uc` is applied once, by the hook, on the assembled article: adapt_article()
                // may pick either its own form or the a/an passed in, so capitalizing before it
                // ran would mean capitalizing a form that gets discarded.
                let a_or_an = get_a_or_an(&singular);
                let article = ranting::adapt_article(a_or_an, s, space, as_pl, false);
                Some(noun.capitalize_with_context(&article, OrthographyRole::Article, uc, ctx))
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
                uc,
                ctx,
            ) {
                Some(custom + space)
            } else {
                let article = ranting::adapt_article(s, s, space, as_pl, false);
                Some(noun.capitalize_with_context(&article, OrthographyRole::Article, uc, ctx))
            }
        }
        ArticleKind::Other => None,
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
fn conjugate_verb<R>(
    noun: &R,
    subjective: &str,
    verb: &str,
    as_pl: bool,
    uc: bool,
    ctx: Option<&NarrationContext>,
) -> String
where
    R: Ranting,
{
    if let Some(custom) = noun.inflect_verb_custom_with_context(subjective, verb, as_pl, uc, ctx) {
        custom
    } else {
        // Fallback path only: a custom form applies `uc` itself (see the hook's docs), so
        // capitalizing here too would be a second, unasked-for pass over it.
        let conjugated = inflect_verb(subjective, verb, as_pl, false);
        noun.capitalize_with_context(&conjugated, OrthographyRole::Verb, uc, ctx)
    }
}

/// Capitalize an English-fallback pronoun through the noun's own hook.
///
/// Only the fallback matters here: the five pronoun arms of `handle_placeholder_impl` all render
/// via `inflect_*(subjective, as_pl, false)` and hand the result to this, while a
/// `inflect_pronoun_custom` form applies `uc` itself and never passes through.
fn cap_pronoun<R>(noun: &R, pronoun: String, uc: bool, ctx: Option<&NarrationContext>) -> String
where
    R: Ranting,
{
    noun.capitalize_with_context(&pronoun, OrthographyRole::Pronoun, uc, ctx)
}

/// The say macro parses placeholders and passes the compile-time-baked spec to this
/// function which returns a string.
#[doc(hidden)]
pub fn handle_placeholder<R>(
    noun: &R,
    poss: String,
    nr: String,
    uc: bool,
    spec: PlaceholderSpec,
) -> String
where
    R: Ranting,
{
    handle_placeholder_impl(noun, poss, nr, uc, spec, None)
}

/// Like [`handle_placeholder`], but resolves tense markers against a runtime
/// [`NarrationContext`] instead of the compile-time marker alone. The say_with!()
/// macro parses placeholders and passes the compile-time-baked spec to this function.
#[doc(hidden)]
pub fn handle_placeholder_with_context<R>(
    noun: &R,
    poss: String,
    nr: String,
    uc: bool,
    spec: PlaceholderSpec,
    ctx: &NarrationContext,
) -> String
where
    R: Ranting,
{
    handle_placeholder_impl(noun, poss, nr, uc, spec, Some(ctx))
}

fn handle_placeholder_impl<R>(
    noun: &R,
    poss: String,
    nr: String,
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
        noun_space,
        case,
        post: post_spec,
    } = spec;
    let mut pre = pre_raw;
    let has_possesive = pre.contains('`');
    let singular_post_verb = OF.is_match(pre); // e.g. "{a set of $ten are} still singular"

    let as_pl = match plurality {
        "" => noun.is_plural(),
        "+" => true,
        "-" => false,
        // A bit hackish but should work also for e.g. 1.0%
        "#" => nr.trim_start() != "one",
        _ => {
            let s = nr.trim_start();
            s != "1" && s.split('.').next() != Some("1")
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
    let viewpoint =
        ctx.and_then(|c| narration::resolve_viewpoint(declared_subjective, c.narration_person));
    let subjective = viewpoint.map_or(declared_subjective, |(rendered, _)| rendered);
    let pronoun_as_pl = viewpoint.map_or(as_pl, |(_, forced_pl)| forced_pl);
    // The entity's own lexical gender / noun class, handed to the pronoun hooks below. Read off
    // the noun, not derived from anything in the placeholder — see `Ranting::noun_class`.
    let noun_class = noun.noun_class();
    let mut res = String::new();

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
                ctx,
            },
        ) {
            res.push_str(&a);
        } else if has_possesive {
            // A possessive noun phrase built from the noun's own name ("Jane's"), hence
            // OrthographyRole::Noun rather than a role of its own — and, as at the name site
            // below, pre-capitalized with `uc` already spent, so the hook is passed `false`.
            let poss_phrase = uc_1st_if(pre, uc);
            res.push_str(&noun.capitalize_with_context(
                &poss_phrase,
                OrthographyRole::Noun,
                false,
                ctx,
            ));
        } else {
            assert!(
                matches!(post_spec, PostSpec::None),
                "verb before and after?"
            );
            let verb = conjugate_verb(noun, subjective, p.as_str(), pronoun_as_pl, uc, ctx);
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
                        ctx,
                    },
                ) {
                    res.push_str(&a);
                } else {
                    res.push_str(s);
                }
            }
        }
        res.push_str(etc1);
        res.push_str(space);
        uc = false;
    }
    if !plurality.contains('?') {
        res.push_str(&nr);
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
        PostSpec::Tense { leading_space, .. } | PostSpec::Degree { leading_space, .. } => {
            leading_space
        }
    };

    if case != CaseKind::Hidden {
        res.push_str(noun_space);
        let s = match case {
            CaseKind::Subjective => {
                if let Some(custom) = noun.inflect_pronoun_custom_with_context(
                    subjective,
                    PronounCase::Subjective,
                    noun_class,
                    pronoun_as_pl,
                    uc,
                    ctx,
                ) {
                    custom
                } else {
                    cap_pronoun(
                        noun,
                        inflect_subjective(subjective, pronoun_as_pl, false),
                        uc,
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
                    uc,
                    ctx,
                ) {
                    custom
                } else {
                    cap_pronoun(
                        noun,
                        inflect_objective(subjective, pronoun_as_pl, false),
                        uc,
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
                    uc,
                    ctx,
                ) {
                    custom
                } else {
                    cap_pronoun(
                        noun,
                        inflect_possessive(subjective, pronoun_as_pl, false),
                        uc,
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
                    uc,
                    ctx,
                ) {
                    custom
                } else {
                    cap_pronoun(
                        noun,
                        inflect_adjective(subjective, pronoun_as_pl, false),
                        uc,
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
                    uc,
                    ctx,
                ) {
                    custom
                } else {
                    cap_pronoun(
                        noun,
                        inflect_reflexive(subjective, pronoun_as_pl, false),
                        uc,
                        ctx,
                    )
                }
            }
            // The only site that does *not* hand the hook an uncapitalized word: `inflect()`
            // takes `uc` itself and is user-implementable, so English capitalization is already
            // resolved here — and it is not the same as `uc_1st_if`, since a derive-generated
            // `name()` for `#[ranting(name = "designer")]` reads `uc == true` as "as written",
            // not "force uppercase". Hence `false`: the hook must not apply it a second time.
            // A fork that capitalizes nouns unconditionally (German) ignores the flag anyway.
            CaseKind::Name | CaseKind::Hidden => {
                let name = noun.inflect(as_pl, uc);
                noun.capitalize_with_context(&name, OrthographyRole::Noun, false, ctx)
            }
        };
        res.push_str(&s);
        res.push_str(post_leading_space);
        uc = false;
    }

    match post_spec {
        PostSpec::None => {}
        PostSpec::PossessiveS => {
            res.push_str(adapt_possesive_s(noun, as_pl));
        }
        PostSpec::Verb(raw) => {
            // Same last-word-conjugated / leading-words-verbatim split as before this
            // refactor: `PostSpec::Verb` still carries free multi-word text (see its
            // doc comment), the macro never had a marker to split it on.
            let rest =
                split_at_find_start(raw, |c: char| !c.is_whitespace()).map_or(raw, |(_, r)| r);
            let (etc2, word) =
                split_at_find_end(rest, |c: char| c.is_whitespace()).unwrap_or(("", rest));
            res.push_str(etc2);
            if !word.is_empty() {
                match word {
                    "'" | "'s" => {
                        res.push_str(adapt_possesive_s(noun, as_pl));
                    }
                    v => {
                        let verb = conjugate_verb(
                            noun,
                            subjective,
                            v,
                            !singular_post_verb && pronoun_as_pl,
                            uc,
                            ctx,
                        );
                        res.push_str(&verb);
                    }
                }
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
                        Some(t) => narration::marker_and_form_for_tense(t, word),
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
                            false,
                            ctx,
                        )
                    } else {
                        let main_verb = noun
                            .inflect_verb_custom_with_context(
                                subjective,
                                &base_form,
                                !singular_post_verb && pronoun_as_pl,
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
                    ctx,
                ));
            }
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
        _ => verb.to_string(),
    }
}

/// upper cases first character if uc is true, or second in a contraction.
pub fn uc_1st_if(s: &str, uc: bool) -> String {
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

fn split_at_find_start(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.rfind(fun).map(|u| s.split_at(u + 1))
}

/// Has the Ranting trait. Often you may want to `#[derive(Ranting)]` and sometimes override some
/// of the trait functions.
#[derive(ranting_derive::Ranting)]
// By setting name and subject to "$", these must come from the struct.
#[ranting(name = "$", subject = "$", gender = "$")]
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
    /// Construct a `Noun`, panicking if `subject` isn't a recognized subject
    /// pronoun. Kept for backward compatibility with existing call sites;
    /// prefer [`Noun::try_new`] when `subject` isn't a compile-time literal
    /// you already know is valid.
    pub fn new(name: &str, subject: &str) -> Self {
        Self::try_new(name, subject).expect("not a subject")
    }

    /// Fallible constructor: returns `Err(InvalidSubjectError)` instead of
    /// panicking when `subject` isn't one of the recognized subject pronouns,
    /// so invalid input can be handled instead of aborting the program.
    pub fn try_new(name: &str, subject: &str) -> Result<Self, InvalidSubjectError> {
        let subject = SubjectPronoun::from_str(subject)
            .map_err(|_| InvalidSubjectError(subject.to_string()))?;
        Ok(Noun {
            name: name.to_string(),
            subject,
            gender: NounClass::UNSET,
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
fn adapt_possesive_s(noun: &dyn Ranting, asked_plural: bool) -> &str {
    if asked_plural && !is_name(noun) {
        "'"
    } else {
        "'s"
    }
}
fn is_name(noun: &dyn Ranting) -> bool {
    noun.name(false)
        .trim_start_matches('\'')
        .starts_with(|c: char| c.is_uppercase())
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

/// The trait required for a struct or enum to function as a noun in a placeholder, derived with `#[derive_ranting]`.
/// Functions are used in `say!()` placeholders replacements.
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
/// Using singular they for an individual with gender-neutral pronouns (the runnable copy of
/// `ranting_derive::derive_ranting`'s own doc example, which can't compile standalone there --
/// see CLAUDE.md's "Doctests in proc-macro crate" note):
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
// By overriding functions one can adapt default behavior, which affects the
// [placeholder](https://docs.rs/ranting_derive/0.2.1/ranting_derive/) behavior.
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
    /// return the name, which is struct name or the `#{ranting(name = "..")]` value, or self.name
    /// if the name attribute was set to "$"
    fn name(&self, uc: bool) -> String;
    /// return the subject: "it" or the `#{ranting(subject = "..")]` value; self.subject if "$".
    fn subjective(&self) -> &str;
    /// return if plural (the subject, or if you, the `#{ranting(plural_you = "true/false")]` value,
    /// default false
    // if the subject can be "you" in both forms, you may want to override the function.
    fn is_plural(&self) -> bool;
    /// return the singular or plural form as configured, starting with capital if uc is set.
    /// use `#{ranting(singular_end = "..", plural_end = "..")]` if not plural = singular + "s"
    // if name can change this should be overridden to lookup each singular_end and plural_end:
    fn inflect(&self, to_plural: bool, uc: bool) -> String;
    /// If an article is only required when emphasizing, set `#{ranting(no_article = "true")]`,
    /// and this function will return accordingly (used by placeholders).
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
    /// fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool, uc: bool) -> Option<String> {
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
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_verb_custom`](Self::inflect_verb_custom), but also receives the
    /// story-wide [`NarrationContext`] in effect for this call, when there is one.
    ///
    /// Every call site that conjugates a verb calls this instead of `inflect_verb_custom`
    /// directly (`say!()` calls it with `ctx: None`, `say_with!()` with `ctx: Some(_)`), so
    /// overriding this hook alone is enough — you don't need both. The default implementation
    /// ignores `ctx` and delegates to `inflect_verb_custom`, so existing implementations that
    /// only override the non-context hook keep working unchanged.
    ///
    /// `ctx` is a parameter, not something read off `self` — an entity's own `subject` stays a
    /// property of the entity, while tense/viewpoint/register/dialect are story-wide settings
    /// that vary per `say_with!()` call, not per noun.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_verb_custom_with_context(
    ///     &self,
    ///     subject: &str,
    ///     verb: &str,
    ///     as_plural: bool,
    ///     uc: bool,
    ///     ctx: Option<&NarrationContext>,
    /// ) -> Option<String> {
    ///     match (verb, ctx.and_then(|c| c.register)) {
    ///         ("be", Some(Register::Formal)) => Some(uc_1st_if("shall be", uc)),
    ///         _ => self.inflect_verb_custom(subject, verb, as_plural, uc),
    ///     }
    /// }
    /// ```
    fn inflect_verb_custom_with_context(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_verb_custom(subject, verb, as_plural, uc)
    }

    /// Customize pronoun inflection (subject/object/possessive forms).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// # Arguments
    /// * `subject` - Subject pronoun (e.g., "I", "he", "they")
    /// * `case` - Which pronoun form: Subjective, Objective, PossessiveDeterminer, or PossessivePronoun
    /// * `class` - The noun's own lexical gender / noun class (see [`NounClass`]), or
    ///   [`NounClass::UNSET`] when it declares none. Lets a fork pick a gendered pronoun from the
    ///   entity instead of guessing from its display string.
    /// * `as_plural` - Whether to pluralize
    /// * `uc` - Whether to uppercase first character
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase, class: NounClass, as_plural: bool, uc: bool) -> Option<String> {
    ///     match (case, class.as_str()) {
    ///         // German: a neuter noun is "es", whatever English pronoun it was declared with.
    ///         (PronounCase::Subjective, "neuter") => Some(uc_1st_if("es", uc)),
    ///         (PronounCase::Subjective, "feminine") => Some(uc_1st_if("sie", uc)),
    ///         _ => None,  // Fall back to English
    ///     }
    /// }
    /// ```
    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        _case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_pronoun_custom`](Self::inflect_pronoun_custom), but also receives the
    /// story-wide [`NarrationContext`] in effect for this call, when there is one. See
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
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_pronoun_custom(subject, case, class, as_plural, uc)
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
    /// * `class` - The noun's own lexical gender / noun class (see [`NounClass`]), or
    ///   [`NounClass::UNSET`] when it declares none. Together with `case` this is what makes
    ///   `der`/`die`/`das` reachable from the entity alone, with no gender table keyed by
    ///   `noun_singular` — which would break on homographs, names, and runtime-built nouns.
    /// * `as_plural` - Whether the noun is plural
    /// * `uc` - Whether to uppercase first character
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_article_custom(&self, article: &str, noun_singular: &str, case: GrammaticalCase, class: NounClass, as_plural: bool, uc: bool) -> Option<String> {
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
    ///             Some(uc_1st_if(form, uc))
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
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_article_custom`](Self::inflect_article_custom), but also receives the
    /// story-wide [`NarrationContext`] in effect for this call, when there is one. See
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
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_article_custom(article, noun_singular, case, class, as_plural, uc)
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
    /// * `case` - The noun's own grammatical role from its case marker, as for
    ///   [`inflect_article_custom`](Self::inflect_article_custom); a bare `` {a chat !noir} ``
    ///   reports [`GrammaticalCase::Name`].
    /// * `class` - The noun's lexical gender / noun class (see [`NounClass`]), or
    ///   [`NounClass::UNSET`]. Together with `as_plural` this is the agreement input.
    /// * `as_plural` - Whether the noun renders plural here (see [`Ranting::is_plural`] for what
    ///   this bool does and does not promise: plural *agreement*, not a referent count).
    /// * `uc` - Whether to uppercase the first character. Applied by the caller only on the
    ///   fallback path, so a custom form must apply it itself — [`uc_1st_if`] does that.
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
    ///     Some(uc_1st_if(&form, uc))
    /// }
    /// ```
    fn inflect_adjective_custom(
        &self,
        _adjective: &str,
        _degree: AdjectiveDegree,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _uc: bool,
    ) -> Option<String> {
        None
    }

    /// Like [`inflect_adjective_custom`](Self::inflect_adjective_custom), but also receives the
    /// story-wide [`NarrationContext`] in effect for this call, when there is one. See
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
        uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        self.inflect_adjective_custom(adjective, degree, case, class, as_plural, uc)
    }

    /// Apply orthographic capitalization to one rendered piece of a placeholder.
    ///
    /// Unlike the `inflect_*_custom` hooks this returns a `String`, not an `Option`: it *is* the
    /// fallback, not a chance to decline one. It is called on every fallback path in
    /// `handle_placeholder` that used to call [`uc_1st_if`] (or an inline uppercase-first-char
    /// block) directly, and its default is exactly that call — so overriding nothing leaves
    /// `say!()`'s output byte-identical.
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
    ///
    /// One exception to "`word` arrives uncapitalized": at [`OrthographyRole::Noun`] the name has
    /// already been through [`inflect`](Self::inflect), which takes `uc` itself and is
    /// user-implementable, so English capitalization is spent by then and `uc` is reported as
    /// `false`. (It is also not simply [`uc_1st_if`]: a derive-generated `name()` for
    /// `#[ranting(name = "designer")]` reads `uc == true` as "as written", not "force
    /// uppercase".) An always-capitalize fork ignores `uc` and is unaffected; a fork that needs
    /// *position-sensitive* noun casing overrides `name`/`inflect` instead.
    ///
    /// # Examples
    /// ```ignore
    /// fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool) -> String {
    ///     // German: nouns are capitalized wherever they stand, everything else only
    ///     // sentence-initially.
    ///     match role {
    ///         OrthographyRole::Noun => uc_1st_if(word, true),
    ///         _ => uc_1st_if(word, uc),
    ///     }
    /// }
    /// ```
    fn capitalize(&self, word: &str, _role: OrthographyRole, uc: bool) -> String {
        uc_1st_if(word, uc)
    }

    /// Like [`capitalize`](Self::capitalize), but also receives the story-wide
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
        _ctx: Option<&NarrationContext>,
    ) -> String {
        self.capitalize(word, role, uc)
    }
}

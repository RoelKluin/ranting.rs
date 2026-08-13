// Tests for story-wide narration context threading (ROADMAP.md v1.1 item 4).
//
// Item 3 already threads `tense` and `narration_person` through `say_with!()`,
// resolved internally by this crate. Item 4 adds `register` and `dialect` to
// `NarrationContext` and threads the context itself (not just tense/person)
// into three new `Ranting` hooks — `inflect_verb_custom_with_context`,
// `inflect_pronoun_custom_with_context`, `inflect_article_custom_with_context`
// — so ecosystem forks can branch on story-wide settings without the crate
// itself interpreting `register`/`dialect`. `subject` stays an entity
// property throughout; `register`/`dialect`/`narration_person` only ever
// arrive as a `ctx` parameter, never read off the noun.

use ranting::*;
use ranting_derive::say_with;
use std::fmt;

#[derive(Clone, Copy)]
struct Courtier;

impl fmt::Display for Courtier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "courtier")
    }
}

impl Ranting for Courtier {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("courtier", uc)
    }

    fn subjective(&self) -> &str {
        "he"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("courtiers", uc)
        } else {
            uc_1st_if("courtier", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    // Only the context-aware hook is overridden — the plain
    // `inflect_verb_custom` is left at its default (`None`), which proves
    // `say_with!()` doesn't require both to be implemented.
    fn inflect_verb_custom_with_context(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        match (verb, ctx.and_then(|c| c.register)) {
            ("greet", Some(Register::Formal)) => Some(uc_1st_if("bows before", uc)),
            ("greet", Some(Register::Casual)) => Some(uc_1st_if("waves at", uc)),
            _ => None,
        }
    }
}

#[test]
fn register_selects_between_custom_verb_forms() {
    let courtier = Courtier;

    let formal = NarrationContext::new().register(Register::Formal);
    assert_eq!(
        say_with!(formal, "{=0 greet} the queen.", courtier),
        "He bows before the queen."
    );

    let casual = NarrationContext::new().register(Register::Casual);
    assert_eq!(
        say_with!(casual, "{=0 greet} the queen.", courtier),
        "He waves at the queen."
    );
}

#[test]
fn no_register_falls_back_to_english() {
    let courtier = Courtier;
    let ctx = NarrationContext::new();
    // No register override: the hook declines (returns None) and English
    // conjugation runs, exactly as it would for say!().
    assert_eq!(
        say_with!(ctx, "{=0 greet} the queen.", courtier),
        say!("{=0 greet} the queen.", courtier)
    );
}

#[derive(Clone, Copy)]
struct Innkeeper;

impl fmt::Display for Innkeeper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "innkeeper")
    }
}

impl Ranting for Innkeeper {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("innkeeper", uc)
    }

    fn subjective(&self) -> &str {
        "she"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("innkeepers", uc)
        } else {
            uc_1st_if("innkeeper", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_pronoun_custom_with_context(
        &self,
        subject: &str,
        case: PronounCase,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        if case == PronounCase::Objective && ctx.and_then(|c| c.dialect) == Some("pirate") {
            return Some(uc_1st_if("her ladyship", uc));
        }
        // Falls back to the plain (non-context) hook for every other case,
        // proving the default `_with_context` delegation still runs when a
        // fork overrides both hooks.
        self.inflect_pronoun_custom(subject, case, as_plural, uc)
    }
}

#[test]
fn dialect_is_available_to_the_pronoun_hook() {
    let innkeeper = Innkeeper;

    let pirate = NarrationContext::new().dialect("pirate");
    assert_eq!(
        say_with!(pirate, "I greet {@0}.", innkeeper),
        "I greet her ladyship."
    );

    let plain = NarrationContext::new();
    assert_eq!(say_with!(plain, "I greet {@0}.", innkeeper), "I greet her.");
}

#[test]
fn context_builder_sets_register_and_dialect_independently() {
    let ctx = NarrationContext::new()
        .register(Register::Formal)
        .dialect("en-GB");
    assert_eq!(ctx.register, Some(Register::Formal));
    assert_eq!(ctx.dialect, Some("en-GB"));
    assert_eq!(ctx.tense, None);
    assert_eq!(ctx.narration_person, None);
}

#[derive(Clone, Copy)]
struct Merchant;

impl fmt::Display for Merchant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "merchant")
    }
}

impl Ranting for Merchant {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("merchant", uc)
    }

    fn subjective(&self) -> &str {
        "it"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("merchants", uc)
        } else {
            uc_1st_if("merchant", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_article_custom_with_context(
        &self,
        article: &str,
        _noun_singular: &str,
        as_plural: bool,
        uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        if article == "the" && ctx.and_then(|c| c.register) == Some(Register::Formal) {
            let form = if as_plural {
                "the honored"
            } else {
                "the honorable"
            };
            return Some(uc_1st_if(form, uc));
        }
        None
    }
}

#[test]
fn register_reaches_the_article_hook() {
    let merchant = Merchant;

    let formal = NarrationContext::new().register(Register::Formal);
    assert_eq!(
        say_with!(formal, "{the 0} arrives.", merchant),
        "The honorable merchant arrives."
    );

    let plain = NarrationContext::new();
    assert_eq!(
        say_with!(plain, "{the 0} arrives.", merchant),
        "The merchant arrives."
    );
}

#[test]
fn say_macro_still_passes_none_to_context_hooks() {
    // say!() has no NarrationContext at all, so it must call the
    // `_with_context` hooks with `ctx: None` — proving `say!()` output stays
    // unaffected by register/dialect (there's nothing to read).
    let courtier = Courtier;
    assert_eq!(
        say!("{=0 greet} the queen.", courtier),
        "He greets the queen."
    );
}

/// Negative control, mirroring `custom_inflection.rs`'s `Sentinel`: the
/// context-aware hook returns a sentinel for every verb, ignoring `ctx`
/// entirely, and the plain `inflect_verb_custom` is left at its default
/// (`None`). If `say!()` were ever changed to call the plain hook instead of
/// `inflect_verb_custom_with_context`, this sentinel would never surface and
/// the test would fail.
#[derive(Clone, Copy)]
struct ContextHookSentinel;

impl fmt::Display for ContextHookSentinel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sentinel")
    }
}

impl Ranting for ContextHookSentinel {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("sentinel", uc)
    }

    fn subjective(&self) -> &str {
        "he"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, _to_plural: bool, uc: bool) -> String {
        uc_1st_if("sentinel", uc)
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom_with_context(
        &self,
        _subject: &str,
        _verb: &str,
        _as_plural: bool,
        _uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        Some("XXCONTEXTHOOKCALLEDXX".to_string())
    }
}

#[test]
fn say_macro_calls_the_context_aware_hook_not_just_the_plain_one() {
    let sentinel = ContextHookSentinel;
    // Only inflect_verb_custom_with_context is overridden (inflect_verb_custom
    // stays at its default None); say!() must still surface the sentinel.
    let result = say!("{=0 are} here.", sentinel);
    assert_eq!(result, "He XXCONTEXTHOOKCALLEDXX here.".to_string());
}

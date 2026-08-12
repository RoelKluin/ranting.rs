// (c) Roel Kluin 2026 GPL v3
//! Runtime narration context: lets `say_with!()` pick a tense at runtime,
//! from e.g. a `StoryState`, instead of the tense being fixed at compile
//! time by the `<`, `=`, `>`, `<=`, `%`, `<%` placeholder markers.
//!
//! `say!()` is unaffected: without a context, placeholder markers keep
//! meaning exactly what they meant in v1.0.

use crate::language::english_shared::is_first_person_subject;
use crate::language::verb_conjugate;
use crate::is_subjective_plural;

/// One of the 7 tenses `say!()` supports via placeholder markers.
///
/// | Tense | Marker | Example |
/// |---|---|---|
/// | `Present` | *(none)* | "walks" |
/// | `Past` | `<` | "walked" |
/// | `Future` | `>` | "will walk" |
/// | `PresentContinuous` | `=` | "is walking" |
/// | `PastContinuous` | `<=` | "was walking" |
/// | `PresentPerfect` | `%` | "has walked" |
/// | `PastPerfect` | `<%` | "had walked" |
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tense {
    Present,
    Past,
    Future,
    PresentContinuous,
    PastContinuous,
    PresentPerfect,
    PastPerfect,
}

/// The grammatical person a story is narrated from — a story-wide setting,
/// distinct from an entity's own declared `subject`.
///
/// Only applies to nouns declared first-person (`I`/`we`): the narrator or
/// narrator-group. Nouns declared second- or third-person are never in
/// scope, so a scene with a first-person narrator and other, already-
/// third-person characters retells only the narrator under person changes —
/// side characters keep their own declared pronouns. See
/// `resolve_viewpoint` for the exact mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Person {
    First,
    Second,
    Third,
}

/// Story-wide narration settings, threaded through `say_with!()`.
///
/// Currently carries a tense override and a narration-person (viewpoint)
/// override.
#[derive(Debug, Clone, Copy, Default)]
pub struct NarrationContext {
    pub tense: Option<Tense>,
    pub narration_person: Option<Person>,
}

impl NarrationContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tense(mut self, tense: Tense) -> Self {
        self.tense = Some(tense);
        self
    }

    pub fn narration_person(mut self, person: Person) -> Self {
        self.narration_person = Some(person);
        self
    }
}

/// Resolve a first-person-declared noun's rendered subject pronoun, plus the
/// plurality flag pronoun inflection must use with it, under a runtime
/// `narration_person` override.
///
/// Returns `None` when no override applies — either `narration_person` is
/// `None`/`Person::First` (no-op), or `declared_subject` isn't first-person
/// (`I`/`we`), in which case callers keep the noun's own declared subject
/// and plurality unchanged.
///
/// The returned plurality flag is `is_subjective_plural` of the *rendered*
/// pronoun, not the noun's own declared plurality — this mirrors how the
/// crate already treats a noun declared `subject = "they"` (structurally
/// plural, `is_subjective_plural("they") == true`, which is what makes
/// singular "they" conjugate as plural in form). Forcing the same
/// convention here is what keeps `pluralize_pronoun` from corrupting the
/// override (e.g. "they" silently downgrading to "it" if handed a stale
/// `as_pl = false` from a singular `I`-declared noun).
///
/// "they" is the fixed third-person fallback — this crate has no gender
/// data for a first-person-declared noun to render a gendered third-person
/// pronoun instead. Nouns that want gendered third-person output should
/// declare that `subject` directly rather than relying on this override.
pub(crate) fn resolve_viewpoint(
    declared_subject: &str,
    narration_person: Option<Person>,
) -> Option<(&'static str, bool)> {
    let person = narration_person?;
    if person == Person::First || !is_first_person_subject(declared_subject) {
        return None;
    }
    let rendered = match person {
        Person::First => unreachable!(),
        Person::Second => "you",
        Person::Third => "they",
    };
    Some((rendered, is_subjective_plural(rendered)))
}

/// Map a `Tense` to the `~TENSE~` marker `handle_tense_marker` understands,
/// plus the base verb conjugated for that tense (empty marker = present,
/// handled by plain subject-verb agreement, no auxiliary).
pub(crate) fn marker_and_form_for_tense(tense: Tense, base_verb: &str) -> (&'static str, String) {
    match tense {
        Tense::Present => ("", base_verb.to_string()),
        Tense::Past => ("<", verb_conjugate::to_past(base_verb)),
        Tense::Future => (">", verb_conjugate::to_future(base_verb)),
        Tense::PresentContinuous => ("=", verb_conjugate::to_continuous(base_verb)),
        Tense::PastContinuous => ("<=", verb_conjugate::to_continuous(base_verb)),
        Tense::PresentPerfect => ("%", verb_conjugate::to_past_participle(base_verb)),
        Tense::PastPerfect => ("<%", verb_conjugate::to_past_participle(base_verb)),
    }
}

/// Conjugate `base_verb` per a `~TENSE~` marker exactly as the compile-time
/// `say!()` path would have (used by `say_with!()` when no context override
/// applies, to stay output-identical to say!() for that placeholder).
pub(crate) fn form_for_marker(marker: &str, base_verb: &str) -> String {
    match marker {
        "<" => verb_conjugate::to_past(base_verb),
        "=" | "<=" => verb_conjugate::to_continuous(base_verb),
        ">" => verb_conjugate::to_future(base_verb),
        "%" | "<%" => verb_conjugate::to_past_participle(base_verb),
        _ => base_verb.to_string(),
    }
}

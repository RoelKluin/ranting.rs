// (c) Roel Kluin 2026 GPL v3
//! Runtime narration context: lets `say_with!()` pick a tense at runtime,
//! from e.g. a `StoryState`, instead of the tense being fixed at compile
//! time by the `<`, `=`, `>`, `<=`, `%`, `<%` placeholder markers.
//!
//! `say!()` is unaffected: without a context, placeholder markers keep
//! meaning exactly what they meant in v1.0.

use crate::language::verb_conjugate;

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

/// Story-wide narration settings, threaded through `say_with!()`.
///
/// Currently carries a tense override; see ROADMAP.md item 4 for planned
/// viewpoint (narration person) and other settings.
#[derive(Debug, Clone, Copy, Default)]
pub struct NarrationContext {
    pub tense: Option<Tense>,
}

impl NarrationContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tense(mut self, tense: Tense) -> Self {
        self.tense = Some(tense);
        self
    }
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

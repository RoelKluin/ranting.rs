// (c) Roel Kluin 2026 MIT
//! Runtime narration context: lets `say_with!()` pick a tense at runtime,
//! from e.g. a `StoryState`, instead of the tense being fixed at compile
//! time by the `<`, `=`, `>`, `<=`, `%`, `<%` placeholder markers.
//!
//! `say!()` is unaffected: without a context, placeholder markers keep
//! meaning exactly what they meant in v1.0.

use crate::{Ranting, is_subjective_plural};
use ranting_core::placeholder::TenseMarker;
use ranting_core::verb_conjugate;

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

/// The grammatical person a story is narrated from — a setting of the telling,
/// distinct from an entity's own declared `subject`. Like the rest of
/// [`NarrationContext`], it is per call rather than fixed for a whole story.
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

/// A formality setting, for forks that want to vary word choice
/// (contractions, honorifics, slang) by register.
///
/// Unlike `tense` and `narration_person`, the crate itself has no built-in
/// English behavior for this — it is inert until a `Ranting` implementation
/// reads `NarrationContext.register` from one of the `*_with_context` hooks
/// (see the `Ranting` trait in `src/lib.rs`) and acts on it.
///
/// `Register::Neutral` is an explicit middle value, distinct from
/// `NarrationContext.register: None`: `None` means "no register override in
/// effect" (a hook should treat it exactly like never having a context, e.g.
/// falling through to whatever the plain, non-context hook would have done),
/// while `Some(Register::Neutral)` means a story has actively opted into
/// "neither formal nor casual" as its register, as opposed to `Formal` or
/// `Casual`. Implementations that don't need the distinction can freely
/// treat both the same way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Formal,
    Neutral,
    Casual,
}

/// Narration settings, threaded through `say_with!()`.
///
/// **Per call, not per story.** These are usually set once for a story, but a context is cheap
/// (`Copy`) and construct-per-utterance is normal too — e.g. `register` varies freely per
/// *addressee* within one scene for Japanese keigo. See
/// `ranting_ja/tests/japanese.rs::register_can_vary_per_utterance_within_one_scene` for two
/// contexts in one scene.
//
// Earlier revisions of this doc said "story-wide," which read as a constraint the type imposes
// rather than a description of the common case (ROADMAP.md Phase 7 items 3 and 13).
///
/// Carries a tense override and a narration-person (viewpoint) override,
/// both resolved internally by this crate (see `resolve_viewpoint` and
/// `marker_and_form_for_tense`/`form_for_marker`), plus a `register` and
/// `dialect` that the crate never interprets itself — those two exist purely
/// so a `Ranting` implementation's `inflect_verb_custom_with_context` /
/// `inflect_pronoun_custom_with_context` / `inflect_article_custom_with_context`
/// hooks can branch on narration settings without the entity itself owning
/// them (keeping `subject`, an entity property, separate from these, which
/// are properties of the telling — see `.claude/rules/extension-hooks.md`).
///
/// `dialect` is a plain `&'static str` (e.g. "en-GB", "pirate") rather than
/// an enum, since the crate places no constraints on it; it is entirely
/// fork-defined. Both new fields are `Copy`, like the rest of the struct, so
/// a single context can still be reused across multiple `say_with!()` calls.
#[derive(Debug, Clone, Copy, Default)]
pub struct NarrationContext {
    pub tense: Option<Tense>,
    pub narration_person: Option<Person>,
    pub register: Option<Register>,
    pub dialect: Option<&'static str>,
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

    pub fn register(mut self, register: Register) -> Self {
        self.register = Some(register);
        self
    }

    pub fn dialect(mut self, dialect: &'static str) -> Self {
        self.dialect = Some(dialect);
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
///
/// Whether `declared_subject` counts as first-person is delegated to
/// `noun.is_first_person_subject_custom` (ROADMAP.md Phase 6 item 16) rather
/// than hard-coded, so a fork whose first-person labels aren't `I`/`we` can
/// still get viewpoint retelling by overriding that hook — English behavior
/// is unchanged, since the hook's default is exactly the old hard-coded check.
pub(crate) fn resolve_viewpoint<R: Ranting + ?Sized>(
    noun: &R,
    declared_subject: &str,
    narration_person: Option<Person>,
) -> Option<(&'static str, bool)> {
    let person = narration_person?;
    if person == Person::First || !noun.is_first_person_subject_custom(declared_subject) {
        return None;
    }
    let rendered = match person {
        Person::First => unreachable!(),
        Person::Second => "you",
        Person::Third => "they",
    };
    Some((rendered, is_subjective_plural(rendered)))
}

/// Which point on the tense axis a `Tense` value picks -- present, past or
/// future -- independent of aspect (continuous/perfect). ROADMAP.md Phase 8
/// item 1's `ctx.tense` override moves a passive/perfect-progressive
/// placeholder along exactly this axis while keeping its voice/aspect fixed,
/// so this collapses the 7-variant `Tense` down to the 3 buckets that
/// classification needs; aspect information in the requested `Tense` (e.g.
/// `PresentPerfect`'s "perfect") is discarded for those markers -- the
/// compile-time marker's own aspect wins, not the override's. A bare
/// `{=%take}` under `Tense::PresentPerfect` still renders "is taken", not
/// "has been taken": the override only ever moves present/past/future.
enum TenseAxis {
    Present,
    Past,
    Future,
}

fn tense_axis(tense: Tense) -> TenseAxis {
    match tense {
        Tense::Present | Tense::PresentContinuous | Tense::PresentPerfect => TenseAxis::Present,
        Tense::Past | Tense::PastContinuous | Tense::PastPerfect => TenseAxis::Past,
        Tense::Future => TenseAxis::Future,
    }
}

/// Map a `Tense` to the marker string `handle_tense_marker` understands, plus
/// the base verb conjugated for that tense (empty marker = present, handled
/// by plain subject-verb agreement, no auxiliary).
///
/// `compile_time_marker` is the marker the placeholder was actually written
/// with. For the six markers `say!()` has always supported (`Past` through
/// `PastPerfect`), a `ctx.tense` override fully replaces it -- unchanged
/// behavior, since none of those six carry voice. For the three
/// participle-channel families added by ROADMAP.md Phase 8 item 1 (passive,
/// future perfect, perfect progressive), the override moves only the tense
/// axis (`tense_axis`, above); voice/aspect is preserved from
/// `compile_time_marker`, per that item's DECIDED ruling -- a full-table
/// override would silently strip voice (`{=%take}` + `Tense::Past` must
/// render "was taken", not "took"). The future member of the passive and
/// perfect-progressive families (`>=%`/`>%=`) has no enumerated placeholder
/// spelling -- `handle_param` never bakes one -- but is a legitimate runtime
/// destination for this override, so `handle_tense_marker` still carries an
/// arm for it (see docs/superpowers/specs/2026-08-15-participle-channel.md).
pub(crate) fn marker_and_form_for_tense(
    tense: Tense,
    base_verb: &str,
    compile_time_marker: TenseMarker,
) -> (&'static str, String) {
    match compile_time_marker {
        TenseMarker::Past
        | TenseMarker::Continuous
        | TenseMarker::Future
        | TenseMarker::PastContinuous
        | TenseMarker::PresentPerfect
        | TenseMarker::PastPerfect => match tense {
            Tense::Present => ("", base_verb.to_string()),
            Tense::Past => ("<", verb_conjugate::to_past(base_verb)),
            Tense::Future => (">", verb_conjugate::to_future(base_verb)),
            Tense::PresentContinuous => ("=", verb_conjugate::to_continuous(base_verb)),
            Tense::PastContinuous => ("<=", verb_conjugate::to_continuous(base_verb)),
            Tense::PresentPerfect => ("%", verb_conjugate::to_past_participle(base_verb)),
            Tense::PastPerfect => ("<%", verb_conjugate::to_past_participle(base_verb)),
        },
        TenseMarker::PresentPassive | TenseMarker::PastPassive => {
            let form = verb_conjugate::to_past_participle(base_verb);
            match tense_axis(tense) {
                TenseAxis::Present => ("=%", form),
                TenseAxis::Past => ("<=%", form),
                TenseAxis::Future => (">=%", form),
            }
        }
        TenseMarker::FuturePerfect => {
            // The perfect family's present/past members are the pre-existing `%`/`<%`
            // spellings -- reused here rather than duplicated, so a `>%` placeholder
            // overridden to `Tense::Past` renders through the exact same marker string
            // (and `handle_tense_marker` arm) a `%`/`<%` placeholder would.
            let form = verb_conjugate::to_past_participle(base_verb);
            match tense_axis(tense) {
                TenseAxis::Present => ("%", form),
                TenseAxis::Past => ("<%", form),
                TenseAxis::Future => (">%", form),
            }
        }
        TenseMarker::PresentPerfectProgressive | TenseMarker::PastPerfectProgressive => {
            let form = verb_conjugate::to_continuous(base_verb);
            match tense_axis(tense) {
                TenseAxis::Present => ("%=", form),
                TenseAxis::Past => ("<%=", form),
                TenseAxis::Future => (">%=", form),
            }
        }
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
        // ROADMAP.md Phase 8 item 1.
        "=%" | "<=%" | ">%" => verb_conjugate::to_past_participle(base_verb),
        "%=" | "<%=" => verb_conjugate::to_continuous(base_verb),
        _ => base_verb.to_string(),
    }
}

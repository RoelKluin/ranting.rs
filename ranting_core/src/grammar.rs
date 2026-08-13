// (c) Roel Kluin 2022 GPL v3
//! Placeholder grammar (the `{...}` sigil syntax `say!()` parses) and
//! subject-pronoun logic, shared between `ranting` (runtime) and
//! `ranting_derive` (compile-time `say!()`/`say_with!()` parsing).
//!
//! Formerly `src/language/english_shared.rs` at the repo root, copied into
//! `ranting_derive`'s `OUT_DIR` at build time and required to compile under
//! two different `strum` major versions simultaneously (`ranting`'s 0.27 and
//! `ranting_derive`'s 0.24) since it was included verbatim into both crates.
//! Now that this code lives in exactly one crate, that constraint is gone —
//! `ranting_core` depends on a single `strum` version (0.27) and both
//! `ranting` and `ranting_derive` get it transitively.
use std::str::FromStr;
use strum_macros::EnumString;

// sentence always captures: to obtain the placeholder offset.
#[allow(dead_code)]
pub static PH_START: &str =
    r"(?P<pre>(?:^|[.?!]\s+|\{\{)?+)\{(?:(?P<plain>\w*+)|(?P<ranting>[^{}:]*+))(?P<fmt>:.*?)?\}";

// Currently unused in both crates (verified: ranting_derive's article handling
// works via string literals, not these enums — see docs/architecture-review-2026-08-13.md).
// Candidate for removal or for backing Phase 4 item 3's typed placeholder spec.
// Kept as dead code here, in ranting_core, rather than dropped or wired up —
// see ROADMAP.md Phase 4 item 1 implementation notes for the reasoning.
#[allow(dead_code)]
#[derive(EnumString, PartialEq, Eq, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum Article {
    The,
    #[strum(serialize = "a", serialize = "an")]
    A,
    #[strum(serialize = "some")]
    Som,
}

#[allow(dead_code)]
#[derive(EnumString, PartialEq, Eq, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum DemonstrativePronoun {
    These,
    Those,
    Such,
    No,
    Neither,
}

// TODO: do not capture space separate but split off from parts.
// regex to capture the placholders or sentence ends
// useful: https://regex101.com/r/Ly7O1x/3/
/// The components captured in a Ranting trait placeholder are defined here.
#[allow(dead_code)]
pub static PH_EXT: &str = r"^(?x)
    (?P<uc>[,^])?+
    (?P<pre>(?:
        \??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se|`[\w-]+|
        (?:[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
        (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
        (?:\s+(?:\??an?|\??some|\??the|th[eo]se|`[\w-]+))?
    )(?:\s+[\w-]+)*?\s+)?+
    (?P<nr>[+-]|(?:\#|\??\$)\w+\s+)?+
    (?P<case>[`=@~*?%])?+
    (?P<noun>[\w-]+)
    (?P<post>\s+[<=>%!]*(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$";

/// An enum with pronouns in subjective form.
#[derive(EnumString, Copy, Clone, Debug, strum_macros::EnumIter)]
#[strum(serialize_all = "lowercase")]
pub enum SubjectPronoun {
    #[strum(serialize = "I")]
    I,
    You,
    Thou,
    He,
    She,
    It,
    We,
    Ye,
    They,
}

/// return whether the given `&str` is a valid subject
pub fn is_subject(subject: &str) -> bool {
    SubjectPronoun::from_str(subject).is_ok()
}

/// Returns whether the subjective is plural. You is assumed singular; a plural_you
/// ranting attribute should already be considered before this call.
pub fn is_subjective_plural(subject: &str) -> bool {
    (SubjectPronoun::from_str(subject).expect("subject should be a valid pronoun") as usize) >= 6
}

/// Returns whether the subjective is first-person ("I" or "we"). Used to scope
/// runtime viewpoint overrides (see `narration::resolve_viewpoint` in the
/// `ranting` crate) to nouns declaring themselves as the narrator — other
/// subjects are left untouched.
pub fn is_first_person_subject(subject: &str) -> bool {
    matches!(subject, "I" | "we")
}

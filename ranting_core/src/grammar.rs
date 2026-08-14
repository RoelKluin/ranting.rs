// (c) Roel Kluin 2022 MIT
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
//
// ROADMAP.md Phase 6 item 17: widened beyond ASCII `.`/`?`/`!` + trailing whitespace, which
// silently missed every non-Latin sentence boundary. Three separate widenings, because a single
// "add more punctuation" pass would still be wrong for two of them:
//   - Greek question mark (U+037E, visually a semicolon) and Urdu full stop (U+06D4) are
//     additional *terminators*, but both scripts still space-separate words, so they keep the
//     existing "punctuation, then required whitespace" shape.
//   - CJK full-width terminators (U+3002 `。`, U+FF01 `！`, U+FF1F `？`) take no following space at
//     all, so they get their own alternative with no `\s+` requirement.
//   - Spanish opening `¿`/`¡` (U+00BF/U+00A1) mark sentence-initial from the *other* side: the
//     punctuation is the trigger and the placeholder can immediately follow it (`¿{noun}...`), so
//     it is `\s*+` (optional, not required) rather than `\s+`.
// `SENTENCE_TRIGGER_CHARS` below is the single source of truth for "does this pre-capture start
// a sentence" so the regex here and `ranting_derive`'s `at_sentence_start` check can't drift.
//
// ROADMAP.md Phase 6 item 26 (docs/superpowers/specs/2026-08-13-preposition-fusion.md, option
// (b)): `pre`'s alternation gained one more branch, `\w[\w'-]*\s+` -- a single literal word (plus
// its trailing whitespace) immediately before the placeholder, e.g. the `"de "`/`"zu "` in `"de
// {the *=0}"`/`"zu {the *=0}"`. It is the *last* alternative (a fallback, tried only once `^`,
// the sentence-terminator branches, and `{{` have failed) because it is the only branch with no
// fixed leading character -- ordering it last means it can never shadow the punctuation/`{{`
// branches, which start with a disjoint character class. `ranting_derive`'s `parse_str_params`
// is what actually turns this into data (the matched word forwarded to a new
// `inflect_preposition_custom` hook instead of being emitted as inert literal text); this regex
// only widens what `pre` is *allowed* to capture. Consequently `pre.start() == 0` is no longer
// sufficient on its own to mean "nothing precedes this placeholder" -- a word can now match at
// position 0 too -- so `at_sentence_start` also has to check the captured text is empty; see
// `ranting_derive::parse_str_params`.
#[allow(dead_code)]
pub static PH_START: &str = concat!(
    // The outer group is a plain `?` (0-or-1), not `?+`: this `regex` crate's `X?+` is not a
    // possessive single-optional the way PCRE's is -- empirically it behaves as `(X?)+` (see
    // ROADMAP.md Phase 6 item 26's implementation notes), which is harmless for the pre-existing
    // branches (each can match at most once in practice: `^` is zero-width, and the punctuation
    // branches require a placeholder to immediately follow, so two can never abut) but would
    // silently chain the new word branch across every preceding word ("Vengo de " instead of just
    // "de ") were it left as `?+`. `\s*+` just below is a different, unrelated possessive
    // quantifier -- it bounds *whitespace repetition inside one branch*, not the branch
    // alternation itself, and is unaffected by this.
    r"(?P<pre>(?:^|[.?!\u{37E}\u{6D4}]\s+|[\u{3002}\u{FF01}\u{FF1F}]|[\u{BF}\u{A1}]\s*+|\{\{|\w[\w'-]*\s+)?)",
    r"\{(?:(?P<plain>\w*+)|(?P<ranting>[^{}:]*+))(?P<fmt>:.*?)?\}"
);

/// The characters that make a `PH_START` `pre` capture count as sentence-initial: ASCII
/// terminators, Greek's question mark, Urdu's full stop, CJK full-width terminators, and
/// Spanish's opening `¿`/`¡`. `ranting_derive`'s `at_sentence_start` check matches a captured
/// `pre`'s first character against this list, and is its only reader.
///
/// **This list is NOT kept in sync with `PH_START` by construction — it is a hand-maintained
/// duplicate.** `PH_START` is a `concat!` of string literals with the same characters hard-coded
/// into its own character class above, and `concat!` cannot interpolate a `&[char]` const, so no
/// mechanism couples the two. Adding a terminator here without also editing `PH_START`'s class
/// (or vice versa) silently desynchronizes compile-time and regex-side sentence detection.
/// `tests/ranting/sentence_detection.rs` is the only thing that would catch it.
pub const SENTENCE_TRIGGER_CHARS: &[char] = &[
    '.', '?', '!', '\u{37E}', '\u{6D4}', '\u{3002}', '\u{FF01}', '\u{FF1F}', '\u{BF}', '\u{A1}',
];

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
///
/// As of ROADMAP.md Phase 4 item 6, this is no longer used to actually parse
/// placeholder internals -- `crate::ph_ext::parse` is a hand-written
/// tokenizer that replaced it at the one call site (`ranting_derive`'s
/// `parse_str_params`), for precise per-error spans instead of one blanket
/// "Error in placeholder" message. This string is kept as the *reference
/// grammar*: `ph_ext`'s own test suite differentially fuzzes `parse` against
/// `Regex::new(PH_EXT)` to confirm they agree, so this stays the
/// authoritative spec `ph_ext::parse`'s doc comments and code structure are
/// implementing by hand. The outer sigil grammar, `PH_START` below, is
/// unaffected by item 6 and still does the real work of finding `{...}`
/// placeholders in a template string.
///
/// ROADMAP.md Phase 6 item 19: `case`'s alternation now tries a fused two-character form first
/// -- `*` immediately followed by a real case marker (`` ` ``/`=`/`@`/`~`/`%`, deliberately not
/// `?`, which already means "hidden", or a second `*`) -- before falling back to the original
/// single-character class. This reuses `*` (already a case-marker-position character, previously
/// synonymous with no marker at all -- see `CaseKind::Name`'s docs) rather than adding a new
/// marker: `{the *=noun}` case-marks the placeholder exactly as `{the =noun}` would (the article
/// hook still sees `GrammaticalCase::Subjective`) but keeps rendering the noun's own name instead
/// of switching to a pronoun. See `ranting_core::placeholder::PlaceholderSpec::display_as_name`.
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
    (?P<case>\*[`=@~%]|[`=@~*?%])?+
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

impl SubjectPronoun {
    /// The canonical spelling accepted by [`FromStr`] for this pronoun — the
    /// inverse of `SubjectPronoun::from_str`. Used to store a typed pronoun on
    /// `Noun` while keeping the string-based `Ranting::subjective(&self) -> &str`
    /// contract (and `ranting_derive`'s generated `self.subject.as_str()` call
    /// sites, which predate this type and are shared with user-defined structs
    /// that still declare a plain `subject: String` field) unchanged.
    pub fn as_str(&self) -> &'static str {
        match self {
            SubjectPronoun::I => "I",
            SubjectPronoun::You => "you",
            SubjectPronoun::Thou => "thou",
            SubjectPronoun::He => "he",
            SubjectPronoun::She => "she",
            SubjectPronoun::It => "it",
            SubjectPronoun::We => "we",
            SubjectPronoun::Ye => "ye",
            SubjectPronoun::They => "they",
        }
    }
}

/// return whether the given `&str` is a valid subject
pub fn is_subject(subject: &str) -> bool {
    SubjectPronoun::from_str(subject).is_ok()
}

/// Returns whether the subjective is plural. You is assumed singular; a plural_you
/// ranting attribute should already be considered before this call.
///
/// Degrades gracefully on invalid input instead of panicking: a `subject` that
/// isn't a recognized pronoun is treated as singular (`false`) rather than
/// aborting a formatting call — a formatting library should never panic at
/// runtime on data. Explicit match, no discriminant arithmetic and no
/// wildcard, so adding a new `SubjectPronoun` variant forces a decision here
/// instead of silently falling on one side of a numeric cutoff.
pub fn is_subjective_plural(subject: &str) -> bool {
    match SubjectPronoun::from_str(subject) {
        Ok(pronoun) => match pronoun {
            SubjectPronoun::I
            | SubjectPronoun::You
            | SubjectPronoun::Thou
            | SubjectPronoun::He
            | SubjectPronoun::She
            | SubjectPronoun::It => false,
            SubjectPronoun::We | SubjectPronoun::Ye | SubjectPronoun::They => true,
        },
        Err(_) => false,
    }
}

/// Returns whether the subjective is first-person ("I" or "we"). Used to scope
/// runtime viewpoint overrides (see `narration::resolve_viewpoint` in the
/// `ranting` crate) to nouns declaring themselves as the narrator — other
/// subjects are left untouched.
pub fn is_first_person_subject(subject: &str) -> bool {
    matches!(subject, "I" | "we")
}

#[cfg(test)]
mod tests {
    use super::*;
    use strum::IntoEnumIterator;

    #[test]
    fn as_str_round_trips_through_from_str() {
        // Every variant's as_str() must parse back to itself via FromStr —
        // as_str() is the field-storage encoding Noun::subject relies on
        // (see ROADMAP.md Phase 4 item 4), so a mismatch here would silently
        // corrupt a stored pronoun.
        for pronoun in SubjectPronoun::iter() {
            let s = pronoun.as_str();
            let round_tripped = SubjectPronoun::from_str(s).expect("as_str() output must parse");
            assert_eq!(round_tripped.as_str(), s, "round-trip mismatch for {s:?}");
        }
    }

    #[test]
    fn is_subjective_plural_covers_every_variant() {
        // Cross-check the explicit match in is_subjective_plural against
        // every current SubjectPronoun variant, so this test breaks (rather
        // than silently doing the wrong thing) if a new variant is ever
        // added without updating the match.
        let plural =
            [SubjectPronoun::We, SubjectPronoun::Ye, SubjectPronoun::They].map(|p| p.as_str());
        for pronoun in SubjectPronoun::iter() {
            let s = pronoun.as_str();
            assert_eq!(
                is_subjective_plural(s),
                plural.contains(&s),
                "unexpected plurality for {s:?}"
            );
        }
    }

    #[test]
    fn is_subjective_plural_degrades_to_false_on_invalid_input() {
        // Previously `.expect(...)`-panicked on invalid input; now degrades
        // gracefully to `false` (see ROADMAP.md Phase 4 item 4).
        assert!(!is_subjective_plural("not-a-pronoun"));
        assert!(!is_subjective_plural(""));
    }

    #[test]
    fn pre_captures_only_the_single_word_immediately_before_a_placeholder() {
        // Regression guard for the repetition quirk this crate's regex engine gave `?+`
        // (documented on `PH_START` above): a plain `?` on the outer group is what keeps a
        // *second* preceding word ("Vengo") out of `pre` when the placeholder is only meant to
        // capture the one immediately adjacent to it ("de").
        let re = regex::Regex::new(PH_START).unwrap();
        let caps = re.captures("Vengo de {the gato}.").unwrap();
        assert_eq!(caps.name("pre").map(|m| m.as_str()), Some("de "));
    }
}

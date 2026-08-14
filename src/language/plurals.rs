// (c) Roel Kluin 2024 MIT
//! Noun pluralization: the irregular table, plus the regular orthographic rules.
//! The irregular table is generated from data/irregular_plurals.txt via build.rs.
//!
//! Until ROADMAP.md Phase 7 item 9 there were no regular rules at all — `Ranting::inflect()`
//! looked the noun up in the table and otherwise appended the `plural_end` attribute (default
//! `"s"`) verbatim, so `{+fly}` rendered `"flys"` and `{+box}` rendered `"boxs"`. Everything
//! English does beyond append-`s` had to be a row in a 63-line table. [`regular_plural`] and
//! [`compound_plural`] are those missing rules; `ranting_gaps` found the gap and specified them.
//!
//! Two scope decisions, both deliberate:
//!
//! **Orthography only.** These rules are a function of the spelling alone, so they need no
//! lexicon. The classes that require knowing what a word *means* or where it was borrowed from
//! (`-us` → `-i` for Latin nouns but not for `bus`; `-o` → `-oes` for `hero` but `-os` for
//! `piano`; `quiz` → `quizzes`, whose consonant doubling is conditioned by stress rather than by
//! letters) stay table entries. A half-right rule for those would be worse than an honest
//! omission.
//!
//! **Pluralization only — the inverse is deliberately unchanged.** Singularization still strips
//! `plural_end`, because every inverse rule has a counterexample class that spelling cannot
//! separate from its positive class: `-ies` → `-y` fixes `cities` but breaks `movies` →
//! `"movy"` (today's naive `-s` strip gets that one right), and a `-ves` → `-f` suffix rule
//! turns `olives` into `"olife"`. So singularization keeps whatever it does today rather than
//! trading one wrong class for another; see `singularization_is_deliberately_unchanged` below.
//!
//! **`ranting_gaps/src/english.rs` keeps its own copy of these rules on purpose.** It is the tool
//! that found this gap, and it re-finds it by comparing `ranting`'s real output against an
//! independently written expectation. Calling into this module from there would make the probe
//! agree with the implementation by construction and report zero findings forever. The
//! duplication is a differential oracle — the same arrangement `CLAUDE.md` records for `PH_EXT`
//! versus `ph_ext` — not the hand-kept duplication this repo warns about elsewhere. Do not
//! deduplicate it.

/// Stems whose `-fe` becomes `-ves`. A suffix list rather than a bare `-fe` test because the
/// rule is lexical, not phonological (`safe` → `safes`).
///
/// These three are also rows in `data/irregular_plurals.txt`, which is consulted *first*, so the
/// entries here only ever fire for **compounds** — `jackknife` → `jackknives`, `housewife` →
/// `housewives` — which the table's exact-match lookup misses. They are not redundant with it.
const FE_VES_STEMS: &[&str] = &["knife", "wife", "life"];

/// Stems whose `-f` becomes `-ves`. English has plenty of counterexamples (`roof`/`roofs`,
/// `chief`/`chiefs`, `belief`/`beliefs`), which is why this is a list and not a bare `-f` test.
///
/// **Order matters**: each of `shelf`, `self`, `elf` is a suffix of the one before it, so
/// `bookshelf` must meet `shelf` before it can match `elf`.
const F_VES_STEMS: &[&str] = &[
    "leaf", "loaf", "thief", "wolf", "calf", "half", "shelf", "self", "elf",
];

/// The regular English plural of `singular`, by spelling alone.
///
/// Reached from [`crate::inflect_noun_regular`] only when the noun missed the irregular table and
/// the `singular_end`/`plural_end` attributes are at their defaults — a struct that sets them has
/// stated its own rule, and that statement wins.
pub(crate) fn regular_plural(singular: &str) -> String {
    let lower = singular.to_lowercase();
    let plural = regular_plural_lowercase(&lower);

    // Restore the caller's case. A rule that only *appends* keeps the name exactly as written,
    // so `iPhone` → `iPhones` rather than the `iphones` that `apply_case`'s lowercase arm would
    // give; anything that rewrote the stem (`city` → `cities`) has no original text left to
    // preserve and goes through `apply_case`, the same function — and therefore the same
    // all-caps/title-case behavior — as the irregular table's own path.
    if !is_all_caps(singular)
        && let Some(added) = plural.strip_prefix(lower.as_str())
    {
        return singular.to_string() + added;
    }
    apply_case(singular, &plural)
}

/// The rules themselves, applied to an already-lowercased noun.
///
/// Working on the lowercased form throughout is what makes the slicing sound: every index below
/// comes from a match against *this* string, so it always lands on a character boundary. Matching
/// on the lowercase form and then slicing the original is not equivalent — lowercasing can change
/// a string's byte length (`\u{212A}nife` is 7 bytes and lowercases to the 5-byte `knife`), so
/// that arrangement panics at formatting time on real data.
fn regular_plural_lowercase(lower: &str) -> String {
    for stem in FE_VES_STEMS {
        if let Some(head) = lower.strip_suffix(stem) {
            return format!("{head}{}ves", &stem[..stem.len() - 2]);
        }
    }
    for stem in F_VES_STEMS {
        if let Some(head) = lower.strip_suffix(stem) {
            return format!("{head}{}ves", &stem[..stem.len() - 1]);
        }
    }

    // consonant + y -> -ies. A vowel before the y keeps it (`day`/`days`, `boy`/`boys`).
    if let Some(head) = lower.strip_suffix('y')
        && head.chars().next_back().is_some_and(|c| !is_vowel(c))
    {
        return format!("{head}ies");
    }

    // Sibilants -> -es. `-ch` is only a sibilant when it sounds like /tʃ/; `stomach` and `epoch`
    // are /k/ and take a bare -s. Spelling cannot tell them apart, so those stay table entries.
    for suffix in ["s", "x", "z", "ch", "sh", "ss"] {
        if lower.ends_with(suffix) {
            return format!("{lower}es");
        }
    }

    format!("{lower}s")
}

/// The plural of a hyphenated compound, which pluralizes its **head** rather than its last
/// element: `mother-in-law` → `mothers-in-law`, `passer-by` → `passers-by`, `court-martial` →
/// `courts-martial`.
///
/// `None` when the compound has no identifiable head, which is the common case (`t-shirt`,
/// `merry-go-round`): those pluralize on the tail like any other word, so the caller falls
/// through to [`regular_plural`]. Note this is the one failure the `plural_end` attribute cannot
/// work around even in principle — the `-s` belongs in the middle of the word, and `plural_end`
/// can only append.
pub(crate) fn compound_plural(word: &str) -> Option<String> {
    const PREPOSITIONS: &[&str] = &["in", "of", "by", "at", "on", "to", "up"];
    // `court-martial` / `attorney-general`: a postposed adjective, head still first.
    const POSTPOSED_ADJECTIVES: &[&str] = &["martial", "general", "apparent", "designate"];

    let parts: Vec<&str> = word.split('-').collect();
    if parts.len() < 2 {
        return None;
    }
    let second = parts[1].to_lowercase();
    if !PREPOSITIONS.contains(&second.as_str()) && !POSTPOSED_ADJECTIVES.contains(&second.as_str())
    {
        return None;
    }
    let mut out = regular_plural(parts[0]);
    for part in &parts[1..] {
        out.push('-');
        out.push_str(part);
    }
    Some(out)
}

fn is_vowel(c: char) -> bool {
    matches!(
        c.to_ascii_lowercase(),
        'a' | 'e' | 'i' | 'o' | 'u' | 'y' | 'à' | 'é' | 'è'
    )
}

// Include the generated irregular plurals table at compile time
include!(concat!(env!("OUT_DIR"), "/irregular_plurals_generated.rs"));

/// Look up the plural form of a noun, preserving the case of the original.
/// Returns Some(plural_form) if found in irregular table, None otherwise (fallback to regular rules).
/// Called by `english::inflect_noun_irregular`, which backs `Ranting::inflect()`'s irregular path.
pub(crate) fn get_plural(singular: &str) -> Option<String> {
    IRREGULAR_PLURALS
        .iter()
        .find(|(s, _)| s.eq_ignore_ascii_case(singular))
        .map(|(_, p)| apply_case(singular, p))
}

/// Look up the singular form of a noun, preserving the case of the original.
/// Returns Some(singular_form) if found in irregular table, None otherwise (fallback to regular rules).
/// Called by `english::inflect_noun_irregular`, which backs `Ranting::inflect()`'s irregular path.
pub(crate) fn get_singular(plural: &str) -> Option<String> {
    IRREGULAR_PLURALS
        .iter()
        .find(|(_, p)| p.eq_ignore_ascii_case(plural))
        .map(|(s, _)| apply_case(plural, s))
}

/// Whether `s` carries no lowercase letter — all-caps, or no letters at all.
fn is_all_caps(s: &str) -> bool {
    s.chars().all(|c| !c.is_alphabetic() || c.is_uppercase())
}

/// Apply the case pattern from original to target string.
/// If original is all uppercase, return target uppercase.
/// If original starts with uppercase, return target with first char uppercase.
/// Otherwise return target lowercase.
fn apply_case(original: &str, target: &str) -> String {
    if is_all_caps(original) {
        // All uppercase or no letters
        target.to_uppercase()
    } else if original.chars().next().is_some_and(|c| c.is_uppercase()) {
        // First character is uppercase
        let mut chars = target.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        }
    } else {
        // Lowercase
        target.to_lowercase()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_irregular_plurals_lookup() {
        assert_eq!(get_plural("child"), Some("children".to_string()));
        assert_eq!(get_plural("person"), Some("people".to_string()));
        assert_eq!(get_plural("mouse"), Some("mice".to_string()));
    }

    #[test]
    fn test_irregular_singulars_lookup() {
        assert_eq!(get_singular("children"), Some("child".to_string()));
        assert_eq!(get_singular("people"), Some("person".to_string()));
        assert_eq!(get_singular("mice"), Some("mouse".to_string()));
    }

    #[test]
    fn test_not_found_returns_none() {
        assert_eq!(get_plural("dog"), None);
        assert_eq!(get_singular("dogs"), None);
    }

    #[test]
    fn test_case_preservation_uppercase() {
        assert_eq!(get_plural("CHILD"), Some("CHILDREN".to_string()));
    }

    #[test]
    fn test_case_preservation_title() {
        assert_eq!(get_plural("Child"), Some("Children".to_string()));
    }

    /// The classes append-`s` got wrong. The left column is what `{+noun}` used to render with an
    /// `s` stuck on the end.
    #[test]
    fn regular_rules_cover_what_append_s_got_wrong() {
        for (singular, expected) in [
            ("fly", "flies"),
            ("city", "cities"),
            ("baby", "babies"),
            ("entity", "entities"),
            ("box", "boxes"),
            ("church", "churches"),
            ("bush", "bushes"),
            ("bus", "buses"),
            ("glass", "glasses"),
            ("match", "matches"),
            ("regex", "regexes"),
            // The -ves stems fire only for compounds; the bare words hit the irregular table
            // first, which is why these are the interesting cases and `knife` is not.
            ("bookshelf", "bookshelves"),
            ("housewife", "housewives"),
            ("jackknife", "jackknives"),
            ("calf", "calves"),
            ("half", "halves"),
            ("shelf", "shelves"),
        ] {
            assert_eq!(
                regular_plural(singular),
                expected,
                "regular plural of {singular}"
            );
        }
    }

    /// Words a naive rule breaks. Pinned as hard as the positive cases: a rule that "fixes"
    /// `fly` by also turning `day` into `dies` is not an improvement.
    #[test]
    fn vowel_y_and_plain_stems_keep_the_bare_s() {
        for (singular, expected) in [
            ("day", "days"),
            ("boy", "boys"),
            ("key", "keys"),
            ("dog", "dogs"),
            ("cat", "cats"),
            ("roof", "roofs"),
            ("chief", "chiefs"),
            ("belief", "beliefs"),
            ("safe", "safes"),
        ] {
            assert_eq!(regular_plural(singular), expected);
        }
    }

    /// The regular path must preserve case exactly as the irregular one does — the crate already
    /// promises all-caps preservation for table words (`test_case_preservation_uppercase`), so a
    /// rule word must not come out as `"CITIes"`.
    #[test]
    fn case_is_preserved_the_same_way_the_irregular_table_does_it() {
        assert_eq!(regular_plural("Bookshelf"), "Bookshelves");
        assert_eq!(regular_plural("City"), "Cities");
        assert_eq!(regular_plural("CITY"), "CITIES");
        assert_eq!(regular_plural("BOX"), "BOXES");
        // A rule that only appends leaves the name exactly as written, so interior capitals
        // survive; `apply_case` would have lowercased them.
        assert_eq!(regular_plural("iPhone"), "iPhones");
    }

    /// This runs at formatting time, on data, so it may not panic on any input. The Kelvin sign
    /// is the sharp case: it is 3 bytes, lowercases to a 1-byte `k`, and so makes the lowercased
    /// form shorter than the original — an index taken from one and applied to the other lands
    /// mid-codepoint.
    #[test]
    fn a_stem_match_that_changes_byte_length_when_lowercased_does_not_panic() {
        assert_eq!(regular_plural("\u{212A}nife"), "Knives");
        assert_eq!(regular_plural("café"), "cafés");
        assert_eq!(regular_plural("café-shelf"), "café-shelves");
        for word in ["Ärzte", "日本", "", "-", "İ", "ß"] {
            let _ = regular_plural(word);
            let _ = compound_plural(word);
        }
    }

    #[test]
    fn compounds_pluralize_their_head_only_when_they_have_one() {
        assert_eq!(
            compound_plural("mother-in-law").as_deref(),
            Some("mothers-in-law")
        );
        assert_eq!(compound_plural("passer-by").as_deref(), Some("passers-by"));
        assert_eq!(
            compound_plural("court-martial").as_deref(),
            Some("courts-martial")
        );
        // No head structure: tail pluralization is right, so the caller must fall through.
        assert_eq!(compound_plural("t-shirt"), None);
        assert_eq!(compound_plural("merry-go-round"), None);
        assert_eq!(compound_plural("single"), None);
    }

    /// Singularization was left alone, and this pins *why* rather than pinning it as correct:
    /// the inverse rules that would fix the left column break the right one, and spelling cannot
    /// tell the two apart. `cities` → `"citie"` is wrong and stays wrong; adding `-ies` → `-y`
    /// would make `movies` → `"movy"`, which is wrong in a class that is currently right.
    #[test]
    fn singularization_is_deliberately_unchanged() {
        use crate::inflect_noun_regular;
        for (plural, singular) in [("cities", "citie"), ("boxes", "boxe")] {
            assert_eq!(inflect_noun_regular(plural, false, None, None), singular);
        }
        for (plural, singular) in [("movies", "movie"), ("olives", "olive")] {
            assert_eq!(inflect_noun_regular(plural, false, None, None), singular);
        }
    }
}

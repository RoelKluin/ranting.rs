//! An independent implementation of regular English noun pluralization.
//!
//! It was written because `ranting` had none: `Ranting::inflect()` fell back to appending the
//! `plural_end` attribute, so `{+fly}` rendered `"flys"`. It served as the specification for the
//! fix, and `src/language/plurals.rs` now implements the same rules (ROADMAP.md Phase 7 item 10).
//!
//! **This copy must stay independent -- do not make it call `ranting`.** The probes compare
//! `ranting`'s real output against these rules; routing both through one implementation would
//! make them agree by construction and report zero findings forever, whatever `ranting` does.
//! The duplication is a differential oracle, the same arrangement `CLAUDE.md` records for
//! `PH_EXT` versus `ph_ext`, and not the hand-kept duplication this repo warns about elsewhere.
//! `src/language/plurals.rs` carries the matching note.
//!
//! Scope is deliberate. These are the *orthographic* rules -- the ones that are a function of the
//! spelling alone, and therefore implementable without a lexicon. The classes that need to know
//! what a word *means* or where it was borrowed from (`-us` → `-i` for Latin nouns but not for
//! `bus`; `-o` → `-oes` for `hero` but `-os` for `piano`) are not here, and are exactly what an
//! irregular table is for. Distinguishing the two is the point: it says how much of the 63-line
//! table would stop being necessary.
//!
//! One more class is knowingly out: single-syllable `-z`/`-s` stems that double the consonant
//! (`quiz` → `quizzes`). The rule below gives `quizes`, which is wrong. It is left wrong rather
//! than special-cased because the doubling condition is phonological (stress and syllable count),
//! not orthographic, and a half-right rule here would be a worse specification than an honest
//! omission — `quiz` belongs in the table.

/// The regular English plural of `singular`, by spelling alone.
///
/// Every branch is a rule `ranting` does not have. Ordered most-specific first.
pub fn regular_plural(singular: &str) -> String {
    let lower = singular.to_lowercase();

    // -f / -fe -> -ves. Restricted to the stems where it is genuinely regular; English has
    // plenty of counterexamples (`roof`/`roofs`, `chief`/`chiefs`, `belief`/`beliefs`), which is
    // why this is a suffix list rather than a bare "-f" test.
    for stem in ["knife", "wife", "life"] {
        if lower.ends_with(stem) {
            return replace_tail(
                singular,
                stem.len(),
                &format!("{}ves", &stem[..stem.len() - 2]),
            );
        }
    }
    for stem in [
        "leaf", "loaf", "thief", "wolf", "calf", "half", "shelf", "self", "elf",
    ] {
        if lower.ends_with(stem) {
            return replace_tail(
                singular,
                stem.len(),
                &format!("{}ves", &stem[..stem.len() - 1]),
            );
        }
    }

    // consonant + y -> -ies. A vowel before the y keeps it (`day`/`days`, `boy`/`boys`).
    if let Some(before) = lower.strip_suffix('y')
        && before.chars().next_back().is_some_and(|c| !is_vowel(c))
    {
        return replace_tail(singular, 1, "ies");
    }

    // Sibilants -> -es. `-ch` is only a sibilant when it sounds like /tʃ/; `stomach`/`epoch` are
    // /k/ and take a bare -s. Spelling cannot tell them apart, so those stay table entries.
    for suffix in ["s", "x", "z", "ch", "sh", "ss"] {
        if lower.ends_with(suffix) {
            return format!("{singular}es");
        }
    }

    format!("{singular}s")
}

/// The plural of a hyphenated compound, which pluralizes its **head**, not its last element:
/// `mother-in-law` → `mothers-in-law`, `passer-by` → `passers-by`, `court-martial` →
/// `courts-martial`.
///
/// Returns `None` when the compound has no identifiable head -- the head is the first element
/// only when a preposition follows it, which is the shape that makes `-s` on the tail wrong.
/// `merry-go-round` and `t-shirt` have no such structure and pluralize on the tail, exactly as
/// `ranting` already does, so they are not findings.
pub fn compound_plural(word: &str) -> Option<String> {
    const PREPOSITIONS: &[&str] = &["in", "of", "by", "at", "on", "to", "up"];
    let parts: Vec<&str> = word.split('-').collect();
    if parts.len() < 2 {
        return None;
    }
    // `court-martial` / `attorney-general`: a postposed adjective, head still first.
    const POSTPOSED_ADJECTIVES: &[&str] = &["martial", "general", "apparent", "designate"];
    let head_is_first = PREPOSITIONS.contains(&parts[1].to_lowercase().as_str())
        || POSTPOSED_ADJECTIVES.contains(&parts[1].to_lowercase().as_str());
    if !head_is_first {
        return None;
    }
    let mut out = regular_plural(parts[0]);
    for part in &parts[1..] {
        out.push('-');
        out.push_str(part);
    }
    Some(out)
}

/// Replace the last `tail_len` characters of `word` with `replacement`, preserving whatever case
/// the caller wrote the rest of the word in.
fn replace_tail(word: &str, tail_len: usize, replacement: &str) -> String {
    let keep = word.len().saturating_sub(tail_len);
    let mut out = word[..keep].to_string();
    // Match the case of the character that was replaced, so `Knife` → `Knives` not `Knifeves`.
    let replaced_upper = word[keep..].chars().next().is_some_and(char::is_uppercase);
    if replaced_upper {
        let mut chars = replacement.chars();
        if let Some(first) = chars.next() {
            out.extend(first.to_uppercase());
            out.push_str(chars.as_str());
        }
    } else {
        out.push_str(replacement);
    }
    out
}

fn is_vowel(c: char) -> bool {
    matches!(
        c.to_ascii_lowercase(),
        'a' | 'e' | 'i' | 'o' | 'u' | 'y' | 'à' | 'é' | 'è'
    )
}

/// A crude adjective test, used only by the word-order probe to count how often the permanent
/// prenominal-adjective boundary actually bites in real text. Suffix-based and knowingly
/// incomplete -- it under-counts, which is the safe direction for a boundary we are measuring
/// rather than fixing.
pub fn looks_like_adjective(word: &str) -> bool {
    const SUFFIXES: &[&str] = &[
        "ous", "ful", "ive", "able", "ible", "al", "ic", "ish", "less", "ary",
    ];
    const COMMON: &[&str] = &[
        "small", "big", "large", "little", "good", "bad", "new", "old", "young", "long", "short",
        "high", "low", "great", "same", "other", "own", "last", "next", "first", "best", "red",
        "black", "white", "green", "blue", "whole", "real", "full", "free", "main", "single",
    ];
    let lower = word.to_lowercase();
    COMMON.contains(&lower.as_str()) || SUFFIXES.iter().any(|s| lower.ends_with(s))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The rules `ranting` is missing, each paired with what it renders today. The right-hand
    /// column of this table is the content of `failures/regular-plural-rules/README.md`.
    #[test]
    fn regular_rules_cover_the_classes_append_s_gets_wrong() {
        for (singular, expected) in [
            ("fly", "flies"),
            ("city", "cities"),
            ("baby", "babies"),
            ("box", "boxes"),
            ("church", "churches"),
            ("bush", "bushes"),
            ("bus", "buses"),
            ("glass", "glasses"),
            ("knife", "knives"),
            ("wife", "wives"),
            ("wolf", "wolves"),
            ("shelf", "shelves"),
            ("half", "halves"),
        ] {
            assert_eq!(
                regular_plural(singular),
                expected,
                "regular plural of {singular}"
            );
        }
    }

    /// Words a naive rule would break. A tool that reports these as failures would be worse than
    /// useless, so they are pinned as hard as the positive cases.
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
        ] {
            assert_eq!(regular_plural(singular), expected);
        }
    }

    #[test]
    fn case_is_preserved_across_a_stem_rewrite() {
        assert_eq!(regular_plural("Knife"), "Knives");
        assert_eq!(regular_plural("City"), "Cities");
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
        // No head structure: tail pluralization is correct, so these must not be findings.
        assert_eq!(compound_plural("t-shirt"), None);
        assert_eq!(compound_plural("merry-go-round"), None);
        assert_eq!(compound_plural("single"), None);
    }
}

// (c) Roel Kluin 2024 GPL v3
//! Verb tense classification and detection.
//! Supports detection of past, continuous, and present tenses.
//! Built-in for Phase 2 grammar depth — to avoid introducing new trait methods,
//! these functions live as free functions in src/language/, consistent with
//! inflect_verb, inflect_possessive, and other existing inflection functions.

// This module only needs IRREGULAR_PAST, from ranting_core::verb_conjugate
// (the single generated copy of the table — see CLAUDE.md's Architecture
// section for why there's no longer a per-crate copy of this codegen).
use ranting_core::verb_conjugate::IRREGULAR_PAST;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tense {
    Present,
    Past,
    Continuous,
}

/// Detect the tense of a verb by checking for irregular forms, -ed suffix, or -ing suffix.
pub(crate) fn detect_tense(verb: &str) -> Tense {
    let verb_lower = verb.to_lowercase();

    // Check irregular past table
    if IRREGULAR_PAST.iter().any(|(_, past)| verb_lower == *past) {
        return Tense::Past;
    }

    // Check regular past: -ed suffix
    if verb_lower.ends_with("ed") && verb.len() > 2 {
        return Tense::Past;
    }

    // Check continuous: -ing suffix
    if verb_lower.ends_with("ing") && verb.len() > 3 {
        return Tense::Continuous;
    }

    // Default to present
    Tense::Present
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn detect_past_regular() {
        let test_cases = vec!["walked", "talked", "wanted", "played", "watched"];
        for verb in test_cases {
            assert_eq!(detect_tense(verb), Tense::Past, "Failed for: {}", verb);
            // Case insensitive
            assert_eq!(
                detect_tense(&verb.to_uppercase()),
                Tense::Past,
                "Failed for uppercase: {}",
                verb
            );
        }
    }

    #[test]
    fn detect_past_irregular() {
        let test_cases = vec![
            "went", "saw", "took", "made", "got", "came", "gave", "knew", "thought", "found",
            "was", "were", "had", "did", "said",
        ];
        for verb in test_cases {
            assert_eq!(detect_tense(verb), Tense::Past, "Failed for: {}", verb);
        }
    }

    #[test]
    fn detect_continuous() {
        let test_cases = vec!["walking", "running", "going", "talking", "playing"];
        for verb in test_cases {
            assert_eq!(
                detect_tense(verb),
                Tense::Continuous,
                "Failed for: {}",
                verb
            );
            // Case insensitive
            assert_eq!(
                detect_tense(&verb.to_uppercase()),
                Tense::Continuous,
                "Failed for uppercase: {}",
                verb
            );
        }
    }

    #[test]
    fn detect_present() {
        let test_cases = vec!["walk", "run", "go", "is", "have", "do", "say", "see"];
        for verb in test_cases {
            assert_eq!(detect_tense(verb), Tense::Present, "Failed for: {}", verb);
        }
    }

    #[test]
    fn irregular_table_coverage() {
        // Ensure all entries in the irregular table are correctly classified as Past
        for (_, past) in IRREGULAR_PAST {
            assert_eq!(
                detect_tense(past),
                Tense::Past,
                "Irregular verb table entry '{}' not detected as Past",
                past
            );
        }
    }

    proptest! {
        #[test]
        fn prop_detect_tense_no_panic(verb in any::<String>()) {
            let _ = detect_tense(&verb);
        }

        #[test]
        fn prop_ed_suffix_detects_as_past(base in r"[a-z]{1,10}") {
            // Any word ending in "ed" with length > 2 should detect as Past
            let verb = format!("{}ed", base);
            if verb.len() > 2 {
                prop_assert_eq!(detect_tense(&verb), Tense::Past,
                    "Word '{}' (base + 'ed') should detect as Past", verb);
            }
        }

        #[test]
        fn prop_ing_suffix_detects_as_continuous(base in r"[a-z]{1,10}") {
            // Any word ending in "ing" with length > 3 should detect as Continuous
            let verb = format!("{}ing", base);
            if verb.len() > 3 {
                prop_assert_eq!(detect_tense(&verb), Tense::Continuous,
                    "Word '{}' (base + 'ing') should detect as Continuous", verb);
            }
        }
    }
}

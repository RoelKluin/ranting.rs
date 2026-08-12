// Property-based tests for public inflection API using proptest.
// Tests the three publicly exported functions:
// - inflect_possesive (requires valid pronoun input, so only tested on known pronouns)
// - is_subject (safe on all input — returns bool)
// - is_subjective_plural (requires valid pronoun input, so only tested on known pronouns)

use proptest::prelude::*;
use ranting::{inflect_possesive, is_subject, is_subjective_plural};

// Known valid subject pronouns (strum serialization is uppercase for all except you/ye/they)
const VALID_PRONOUNS: &[&str] = &["I", "you", "thou", "he", "she", "it", "we", "ye", "they"];

proptest! {
    #[test]
    fn prop_is_subject_no_panic(subject in any::<String>()) {
        let _ = is_subject(&subject);
    }

    #[test]
    fn prop_is_subject_on_valid_pronouns(subject in "I|you|thou|he|she|it|we|ye|they") {
        // The is_subject function recognizes these pronouns (I is uppercase, rest lowercase)
        prop_assert!(is_subject(&subject), "'{}' should be a valid subject", subject);
    }

    #[test]
    fn prop_is_subject_rejects_garbage(s in r"[a-z]{1,10}") {
        // Most random lowercase strings are not valid pronouns
        if !VALID_PRONOUNS.contains(&s.as_str()) {
            prop_assert!(!is_subject(&s), "'{}' should not be a valid subject", s);
        }
    }
}

#[test]
fn inflect_possesive_known_pronouns() {
    // Known pronouns should have correct possessives (second param to_plural=false keeps singular form)
    assert_eq!(inflect_possesive("I", false, false), "my");
    assert_eq!(inflect_possesive("he", false, false), "his");
    assert_eq!(inflect_possesive("she", false, false), "her");
    assert_eq!(inflect_possesive("it", false, false), "its");
    // "they" is already plural; to_plural=false converts it to singular "it" -> "its"
    assert_eq!(inflect_possesive("they", false, false), "its");
    // "I" is singular; to_plural=true converts it to plural "we" -> "our"
    assert_eq!(inflect_possesive("I", true, false), "our");
}

#[test]
fn inflect_possesive_with_case_flag() {
    // Uppercase flag should capitalize the first letter
    assert_eq!(inflect_possesive("I", false, true), "My");
    assert_eq!(inflect_possesive("he", false, true), "His");
}

#[test]
fn is_subject_known_pronouns() {
    // Known subject pronouns
    assert!(is_subject("I"));
    assert!(is_subject("you"));
    assert!(is_subject("he"));
    assert!(is_subject("she"));
    assert!(is_subject("it"));
    assert!(is_subject("we"));
    assert!(is_subject("they"));
    assert!(is_subject("thou"));
    assert!(is_subject("ye"));

    // Non-pronouns should not be subjects
    assert!(!is_subject(""));
    assert!(!is_subject("apple"));
    assert!(!is_subject("123"));
}

#[test]
fn is_subjective_plural_known_pronouns() {
    // Plural subject pronouns (we, ye, they are >= index 6 in the enum)
    assert!(is_subjective_plural("we"));
    assert!(is_subjective_plural("they"));
    assert!(is_subjective_plural("ye"));

    // Singular pronouns (indices 0-5)
    assert!(!is_subjective_plural("I"));
    assert!(!is_subjective_plural("he"));
    assert!(!is_subjective_plural("she"));
    assert!(!is_subjective_plural("it"));
    assert!(!is_subjective_plural("thou"));
    assert!(!is_subjective_plural("you"));
}

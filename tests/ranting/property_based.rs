// Property-based tests for public inflection API using proptest.
// Tests the three publicly exported functions:
// - inflect_possesive (requires valid pronoun input, so only tested on known pronouns)
// - is_subject (safe on all input — returns bool)
// - is_subjective_plural (safe on all input — degrades to `false` on an
//   unrecognized pronoun instead of panicking; see ROADMAP.md Phase 4 item 4)

use proptest::prelude::*;
use ranting::{Noun, inflect_possesive, is_subject, is_subjective_plural};

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

    #[test]
    fn prop_is_subjective_plural_no_panic(subject in any::<String>()) {
        // Degrades gracefully (see ROADMAP.md Phase 4 item 4): any input,
        // valid pronoun or not, must return a bool rather than panic.
        let _ = is_subjective_plural(&subject);
    }

    #[test]
    fn prop_noun_try_new_no_panic(name in any::<String>(), subject in any::<String>()) {
        // Noun::try_new must never panic, regardless of input: it either
        // returns a valid Noun or an InvalidSubjectError.
        let _ = Noun::try_new(&name, &subject);
    }
}

#[test]
fn is_subjective_plural_invalid_subject_degrades_to_false() {
    // Previously this path called `.expect(...)` and panicked on an invalid
    // subject. It now degrades gracefully to `false` instead (see
    // ROADMAP.md Phase 4 item 4).
    assert!(!is_subjective_plural("not-a-pronoun"));
    assert!(!is_subjective_plural(""));
}

#[test]
fn noun_try_new_rejects_invalid_subject_without_panicking() {
    // Noun::new panics via assert!/expect on an invalid subject (kept for
    // backward compatibility); Noun::try_new is the non-panicking API that
    // makes an invalid subject something you can handle instead of crash on.
    let err = match Noun::try_new("thing", "not-a-pronoun") {
        Err(e) => e,
        Ok(_) => panic!("expected an InvalidSubjectError"),
    };
    assert_eq!(err.0, "not-a-pronoun");
    assert_eq!(
        err.to_string(),
        "\"not-a-pronoun\" is not a valid subject pronoun"
    );

    assert!(Noun::try_new("thing", "it").is_ok());
}

#[test]
#[should_panic(expected = "not a subject")]
fn noun_new_still_panics_on_invalid_subject() {
    // Documents that Noun::new's existing (panicking) behavior for invalid
    // input is intentionally unchanged — Noun::try_new is the new escape
    // hatch, not a replacement for Noun::new's contract.
    Noun::new("thing", "not-a-pronoun");
}

#[test]
fn inflect_degrades_gracefully_on_suffix_mismatch() {
    use ranting::Ranting;
    use ranting_derive::derive_ranting;

    // "Sheep" is declared plural (subject = "they") but its name doesn't end
    // in the default plural_end "s", so singularizing it can't strip a
    // matching suffix. This path used to call `.expect(...)` in the
    // generated `inflect()` and panic; it now degrades gracefully by
    // returning the name unchanged instead (see ROADMAP.md Phase 4 item 4).
    #[derive_ranting]
    #[ranting(subject = "they")]
    struct Sheep;

    let sheep = Sheep;
    assert_eq!(Ranting::inflect(&sheep, false, false), "sheep");
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
    // Plural subject pronouns (we, ye, they — matched explicitly in
    // is_subjective_plural, not via discriminant arithmetic; see
    // ROADMAP.md Phase 4 item 4)
    assert!(is_subjective_plural("we"));
    assert!(is_subjective_plural("they"));
    assert!(is_subjective_plural("ye"));

    // Singular pronouns
    assert!(!is_subjective_plural("I"));
    assert!(!is_subjective_plural("he"));
    assert!(!is_subjective_plural("she"));
    assert!(!is_subjective_plural("it"));
    assert!(!is_subjective_plural("thou"));
    assert!(!is_subjective_plural("you"));
}

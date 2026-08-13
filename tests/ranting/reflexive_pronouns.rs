// (c) Roel Kluin 2022 MIT
//! Integration tests for reflexive pronouns (ROADMAP Phase 3 item 5).
//!
//! The `%` case marker (see README.md's placeholder grammar list) selects the
//! reflexive form: myself, yourself, thyself, himself, herself, itself,
//! ourselves, yourselves, themselves.

use ranting::*;
use std::fmt;

#[test]
fn all_pronouns_reflexive_form() {
    let cases = [
        ("I", "myself"),
        ("you", "yourself"),
        ("thou", "thyself"),
        ("he", "himself"),
        ("she", "herself"),
        ("it", "itself"),
        ("we", "ourselves"),
        ("ye", "yourselves"),
        ("they", "themselves"),
    ];
    for (subject, expected) in cases {
        let w = Noun::new("", subject);
        let result = say!("Look, {=w hurt} {%w}.", w);
        assert_eq!(
            result,
            format!("Look, {} hurt {}.", w.subjective(), expected),
            "reflexive form mismatch for subject {subject}"
        );
    }
}

#[test]
fn reflexive_singular_they() {
    // Singular "they" always conjugates and reflexes as grammatically plural.
    let alex = Noun::new("Alex", "they");
    assert_eq!(
        say!("{=alex hurt} {%alex}.", alex),
        "They hurt themselves.".to_string()
    );
}

#[test]
fn reflexive_at_sentence_start_is_capitalized() {
    let alex = Noun::new("Alex", "he");
    assert_eq!(say!("{%alex} did it.", alex), "Himself did it.".to_string());
}

#[test]
fn reflexive_uppercase_marker() {
    // The `,`/`^` uc-control prefix forces capitalization mid-sentence.
    let alex = Noun::new("Alex", "she");
    assert_eq!(
        say!("Only {^%alex} can decide that.", alex),
        "Only Herself can decide that.".to_string()
    );
}

#[test]
fn reflexive_forced_plural_and_singular() {
    // Forced plurality (`+`) on a normally-singular subject renders as "they"
    // internally, so the reflexive form follows suit.
    let bob = Noun::new("Bob", "he");
    assert_eq!(say!("{+%bob}.", bob), "Themselves.".to_string());

    // Forced singular (`-`) on "we" collapses to "I" and reflexes accordingly.
    let us = Noun::new("", "we");
    assert_eq!(say!("{-%us}.", us), "Myself.".to_string());
}

#[test]
fn reflexive_you_singular_vs_plural() {
    let you = Noun::new("", "you");
    assert_eq!(say!("{%you}.", you), "Yourself.".to_string());
    assert_eq!(say!("{+%you}.", you), "Yourselves.".to_string());
}

#[test]
fn reflexive_combines_with_possessive_in_sentence() {
    let jordan = Noun::new("Jordan", "they");
    assert_eq!(
        say!(
            "{=jordan have} shared {`jordan} pronouns with {%jordan}.",
            jordan
        ),
        "They have shared their pronouns with themselves.".to_string()
    );
}

#[test]
fn reflexive_positional_and_named_args() {
    let noun = Noun::new("Casey", "she");
    assert_eq!(say!("{%0}", noun), "Herself".to_string());
    assert_eq!(say!("{%x}", x = noun), "Herself".to_string());
}

// ============================================================================
// Custom hook routing: PronounCase::Reflexive
// ============================================================================

#[derive(Clone, Copy)]
struct Dignitary;

impl fmt::Display for Dignitary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dignitary")
    }
}

impl Ranting for Dignitary {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("dignitary", uc)
    }

    fn subjective(&self) -> &str {
        "you"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool, _case: GrammaticalCase) -> String {
        if to_plural {
            uc_1st_if("dignitaries", uc)
        } else {
            uc_1st_if("dignitary", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if subject == "you" && case == PronounCase::Reflexive {
            return Some(uc_1st_if("their own royal person", uc));
        }
        None
    }
}

#[test]
fn reflexive_custom_hook_overrides_english() {
    let dignitary = Dignitary;
    let result = say!("{0 see} {%0} in the mirror.", dignitary);
    assert_eq!(
        result,
        "Dignitary see their own royal person in the mirror.".to_string()
    );
}

#[test]
fn reflexive_custom_hook_falls_back_for_other_cases() {
    // The hook only special-cases PronounCase::Reflexive; other cases fall back to English.
    let dignitary = Dignitary;
    let result = say!("{=0 are} welcome.", dignitary);
    assert_eq!(result, "You are welcome.".to_string());
}

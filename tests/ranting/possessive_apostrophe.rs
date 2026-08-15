// Regression tests for architecture-review-2026-08-15.md §1.7 / ROADMAP.md Phase 8 item 6:
// a plural proper name used to take "'s" instead of the bare apostrophe.
use ranting::*;
use ranting_derive::say;

#[test]
fn plural_proper_name_takes_bare_apostrophe() {
    let joneses = Noun::new("Joneses", "they");
    assert_eq!(
        say!("I visited {the 0's} house.", joneses),
        "I visited the Joneses' house.".to_string()
    );
}

#[test]
fn singular_name_ending_in_s_keeps_apostrophe_s() {
    let myles = Noun::new("Myles", "he");
    assert_eq!(
        say!("I visited {0's} house.", myles),
        "I visited Myles's house.".to_string()
    );
}

#[test]
fn plural_common_noun_takes_bare_apostrophe() {
    let schools = Noun::new("schools", "they");
    assert_eq!(
        say!("I visited {the 0's} playground.", schools),
        "I visited the schools' playground.".to_string()
    );
}

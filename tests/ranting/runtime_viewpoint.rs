// Tests for say_with!() runtime viewpoint (narration person) selection
// (ROADMAP.md v1.1 item 3). Only first-person-declared nouns ("I"/"we") are
// in scope for a narration_person override — other subjects, and other
// nouns in the same call, pass through unchanged.

use ranting::*;
use ranting_derive::{derive_ranting, say, say_with};

#[test]
fn context_overrides_first_person_subject() {
    let hero = Noun::new("Hero", "I");

    let third = NarrationContext::new().narration_person(Person::Third);
    assert_eq!(say_with!(third, "{=0 <walk}", hero), "They walked");

    let second = NarrationContext::new().narration_person(Person::Second);
    assert_eq!(say_with!(second, "{=0 <walk}", hero), "You walked");

    let first = NarrationContext::new().narration_person(Person::First);
    assert_eq!(say_with!(first, "{=0 <walk}", hero), "I walked");
}

#[test]
fn third_person_override_uses_plural_verb_agreement() {
    // Singular they takes plural verb morphology: "they are"/"have", never
    // "they is"/"has" — this is the case most likely to regress silently.
    let hero = Noun::new("Hero", "I");

    let pres_cont = NarrationContext::new()
        .tense(Tense::PresentContinuous)
        .narration_person(Person::Third);
    assert_eq!(say_with!(pres_cont, "{=0 <walk}", hero), "They are walking");

    let past_cont = NarrationContext::new()
        .tense(Tense::PastContinuous)
        .narration_person(Person::Third);
    assert_eq!(say_with!(past_cont, "{=0 <walk}", hero), "They were walking");

    let pres_perf = NarrationContext::new()
        .tense(Tense::PresentPerfect)
        .narration_person(Person::Third);
    assert_eq!(say_with!(pres_perf, "{=0 <walk}", hero), "They have walked");
}

#[test]
fn context_leaves_non_first_person_nouns_unaffected() {
    // A first-person-declared narrator alongside an unrelated third-person
    // noun in the same call: only the narrator's rendering changes.
    let hero = Noun::new("Hero", "I");
    let bystander = Noun::new("Bystander", "she");
    let third = NarrationContext::new().narration_person(Person::Third);

    assert_eq!(
        say_with!(third, "{=0 <walk} past {=1}", hero, bystander),
        "They walked past she"
    );
}

#[test]
fn no_narration_person_falls_back_to_declared_subject() {
    let hero = Noun::new("Hero", "I");
    let ctx = NarrationContext::new();
    assert_eq!(say_with!(ctx, "{=0 <walk}", hero), say!("{=0 <walk}", hero));

    let tense_only = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(tense_only, "{=0 <walk}", hero), "I walked");
}

#[derive_ranting]
#[ranting(name = "$", subject = "$")]
struct Narrator {
    name: String,
    subject: String,
}

#[test]
fn context_overrides_derived_struct_first_person_subject() {
    let narrator = Narrator {
        name: String::from("Kay"),
        subject: String::from("I"),
    };
    let third = NarrationContext::new().narration_person(Person::Third);
    assert_eq!(say_with!(third, "{=0 <arrive}", narrator), "They arrived");
}

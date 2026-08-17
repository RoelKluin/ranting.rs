// Tests for the `;` verbatim-verb marker (ROADMAP.md Phase 8 item 2), the
// subjunctive escape hatch closing docs/architecture-review-2026-08-15.md
// §1.5: `{=i were}` renders "I was" (correct indicative agreement, and
// unchanged by this feature), while `{=i ;were}` renders "I were" (the
// caller's word, exactly as written, no person/number agreement).

use ranting::*;
use ranting_derive::say;

#[test]
fn indicative_were_still_agrees() {
    let i = Noun::new("person", "I");
    assert_eq!(say!("{=i were}", i), "I was");
}

#[test]
fn verbatim_were_bypasses_agreement() {
    let i = Noun::new("person", "I");
    assert_eq!(say!("{=i ;were}", i), "I were");
}

#[test]
fn verbatim_subjunctive_in_a_full_clause() {
    let i = Noun::new("person", "I");
    assert_eq!(
        say!("If {=i ;were} rich, I would travel.", i),
        "If I were rich, I would travel."
    );
}

#[test]
fn verbatim_bypasses_third_person_agreement_too() {
    // `were` would normally become "was" for he/she/it (third_person()); verbatim
    // suppresses that the same way it does for first person.
    let he = Noun::new("person", "he");
    assert_eq!(say!("{=0 ;were}", he), "He were");
}

#[test]
fn verbatim_other_verb_stays_uninflected() {
    // Not just `were` -- any post-noun verb marked `;` is rendered exactly as
    // captured, with no runtime conjugation at all.
    let he = Noun::new("person", "he");
    assert_eq!(say!("{=0 ;have}", he), "He have");
}

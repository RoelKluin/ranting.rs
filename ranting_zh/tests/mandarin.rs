//! What Mandarin *does* reach through `ranting`'s public hooks.
//!
//! Every assertion here is real Mandarin (or, where marked, the honest English degradation of
//! an unknown word/out-of-range numeral). The tense/aspect gap is in `holes.rs`. Mirrors
//! `ranting_fr/tests/french.rs`'s structure.

use ranting::say;
use ranting_zh::{MandarinNoun, MandarinPerson};

// ------------------------------------------------------------------- nouns --

#[test]
fn inflect_is_an_identity_function_regardless_of_the_plus_or_minus_marker() {
    // Not a new finding -- matches `ranting_ja::JapaneseNoun::inflect` exactly. Mandarin common
    // nouns do not inflect for number at all.
    assert_eq!(say!("{+0}", MandarinNoun::mao()), "猫");
    assert_eq!(say!("{-0}", MandarinNoun::mao()), "猫");
}

#[test]
fn a_bare_placeholder_pronoun_falls_through_to_english_since_no_pronoun_hook_is_overridden() {
    // `subjective()` is an uninterpreted channel with no hook reading it back: with no
    // `inflect_pronoun_custom` override, `{=0}` renders `ranting`'s own English default rather
    // than the Chinese string `subjective()` returns. `MandarinPerson` (below) overrides the
    // hook and gets real Mandarin; a bare noun, like a bare `ranting_ja::JapaneseNoun`, does not.
    assert_eq!(say!("{=0}", MandarinNoun::mao()), "It");
}

// -------------------------------------------------------------------verbs --

#[test]
fn a_bare_verb_placeholder_substitutes_the_invariant_mandarin_word() {
    assert_eq!(say!("{0 eat}.", MandarinNoun::mao()), "猫 吃.");
}

#[test]
fn an_unknown_verb_falls_through_to_english_rather_than_being_guessed() {
    assert_eq!(say!("{0 run}.", MandarinNoun::mao()), "猫 run.");
}

// ---------------------------------------------------------- classifiers/numerals --

#[test]
fn numeral_and_classifier_fuse_directly_against_the_noun() {
    // Not a new finding -- the same post-assembly splice `ranting_ja`'s counters use, minus the
    // attributive particle Japanese needs. `只`/`本`/`个` are each read off the noun, never off
    // `NounClass`.
    assert_eq!(say!("{#0 1}", 3, MandarinNoun::mao()), "三只猫");
    assert_eq!(say!("{#0 1}", 0, MandarinNoun::ren()), "零个人");
}

#[test]
fn a_numeral_outside_the_closed_set_falls_through_to_english_while_the_noun_stays_chinese() {
    assert_eq!(say!("{#0 1}.", 6, MandarinNoun::ren()), "Six人.");
}

// ------------------------------------------------------------ MandarinPerson --

#[test]
fn subject_pronouns_across_persons() {
    assert_eq!(say!("{=0}", MandarinPerson::WO), "我");
    assert_eq!(say!("{=0}", MandarinPerson::NI), "你");
    assert_eq!(say!("{=0}", MandarinPerson::NIN), "您");
    assert_eq!(say!("{=0}", MandarinPerson::TA), "他");
    assert_eq!(say!("{=0}", MandarinPerson::WOMEN), "我们");
    assert_eq!(say!("{=0}", MandarinPerson::NIMEN), "你们");
    assert_eq!(say!("{=0}", MandarinPerson::TAMEN), "他们");
}

#[test]
fn subject_and_object_position_use_the_identical_word() {
    // No case distinction at all -- unlike every other fork's person entity, not even a
    // two-way split.
    assert_eq!(say!("{=0}", MandarinPerson::WO), "我");
    assert_eq!(say!("Vi {@0}.", MandarinPerson::WO), "Vi 我.");
}

#[test]
fn possessive_is_the_regular_particle_not_a_separate_word() {
    assert_eq!(say!("{`0} book.", MandarinPerson::WO), "我的 book.");
}

#[test]
fn reflexive_is_invariant_across_every_person() {
    assert_eq!(say!("{%0}", MandarinPerson::WO), "自己");
    assert_eq!(say!("{%0}", MandarinPerson::TAMEN), "自己");
}

#[test]
fn person_verb_substitution_matches_the_noun_hook() {
    assert_eq!(say!("{=0 eat}.", MandarinPerson::WO), "我 吃.");
    assert_eq!(say!("{=0 eat}.", MandarinPerson::WOMEN), "我们 吃.");
}

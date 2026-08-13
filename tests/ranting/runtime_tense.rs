// Tests for say_with!() runtime tense selection (ROADMAP.md v1.1 item 3).
// The same placeholder, with the same compile-time marker, must render in
// whatever tense a runtime NarrationContext specifies.

use ranting::*;
use ranting_derive::say_with;

#[test]
fn context_overrides_compile_time_marker() {
    let he = Noun::new("person", "he");

    // Baseline marker in the placeholder ('<' = past); every override below
    // must win over it.
    let past = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(past, "{=0 <walk}", he), "He walked");

    let future = NarrationContext::new().tense(Tense::Future);
    assert_eq!(say_with!(future, "{=0 <walk}", he), "He will walk");

    let present = NarrationContext::new().tense(Tense::Present);
    assert_eq!(say_with!(present, "{=0 <walk}", he), "He walks");

    let pres_cont = NarrationContext::new().tense(Tense::PresentContinuous);
    assert_eq!(say_with!(pres_cont, "{=0 <walk}", he), "He is walking");

    let past_cont = NarrationContext::new().tense(Tense::PastContinuous);
    assert_eq!(say_with!(past_cont, "{=0 <walk}", he), "He was walking");

    let pres_perf = NarrationContext::new().tense(Tense::PresentPerfect);
    assert_eq!(say_with!(pres_perf, "{=0 <walk}", he), "He has walked");

    let past_perf = NarrationContext::new().tense(Tense::PastPerfect);
    assert_eq!(say_with!(past_perf, "{=0 <walk}", he), "He had walked");
}

#[test]
fn context_overrides_irregular_verb() {
    let she = Noun::new("person", "she");
    let past = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(past, "{=0 <go}", she), "She went");

    let pres_perf = NarrationContext::new().tense(Tense::PresentPerfect);
    assert_eq!(say_with!(pres_perf, "{=0 <go}", she), "She has gone");
}

#[test]
fn no_tense_override_falls_back_to_placeholder_marker() {
    // Without a tense override, say_with!() must reproduce say!()'s output
    // for the same placeholder — the compile-time marker still wins.
    let he = Noun::new("person", "he");
    let ctx = NarrationContext::new();
    assert_eq!(say_with!(ctx, "{=0 <walk}", he), say!("{=0 <walk}", he));
    assert_eq!(say_with!(ctx, "{=0 >walk}", he), say!("{=0 >walk}", he));
    assert_eq!(say_with!(ctx, "{=0 <=dance}", he), say!("{=0 <=dance}", he));
    assert_eq!(say_with!(ctx, "{=0 <%leave}", he), say!("{=0 <%leave}", he));
}

#[test]
fn tense_marker_with_trailing_words_runtime_tense() {
    // say_with!() counterpart of verb_tense::tense_marker_with_trailing_words:
    // `runtime_tense = true` bakes `PostSpec::Tense { word: base_word, trailing,
    // .. }` (the uninflected base verb) instead of a compile-time-conjugated
    // form, but trailing-word handling goes through the same typed fields.
    let he = Noun::new("person", "he");
    let ctx = NarrationContext::new();
    assert_eq!(say_with!(ctx, "{=0 <go home}", he), "He went home");

    let future = NarrationContext::new().tense(Tense::Future);
    assert_eq!(say_with!(future, "{=0 <go home}", he), "He will go home");
}

#[test]
fn context_overrides_plural_subject() {
    let they = Noun::new("friends", "they");
    let past = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(past, "{=0 <walk}", they), "They walked");

    let pres_cont = NarrationContext::new().tense(Tense::PresentContinuous);
    assert_eq!(say_with!(pres_cont, "{=0 <walk}", they), "They are walking");
}

// ROADMAP.md Phase 8 item 1: the participle channel -- passive voice, future
// perfect, perfect progressive. Five composed TenseMarker spellings:
// `=%` (present passive), `<=%` (past passive), `>%` (future perfect),
// `%=` (present perfect progressive), `<%=` (past perfect progressive).
// See docs/superpowers/specs/2026-08-15-participle-channel.md.

use ranting::*;
use ranting_derive::{say, say_with};

#[test]
fn present_passive_basic() {
    let sword = Noun::new("sword", "it");
    assert_eq!(say!("{=0 =%take}", sword), "It is taken");
    let swords = Noun::new("swords", "they");
    assert_eq!(say!("{=0 =%take}", swords), "They are taken");
}

#[test]
fn present_passive_irregular() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 =%write}", person), "She is written");
    assert_eq!(say!("{=0 =%see}", person), "She is seen");
}

#[test]
fn present_passive_all_pronouns() {
    let test_cases = vec![
        ("I", "I am taken"),
        ("you", "You are taken"),
        ("he", "He is taken"),
        ("she", "She is taken"),
        ("it", "It is taken"),
        ("we", "We are taken"),
        ("they", "They are taken"),
    ];
    for (pronoun, expected) in test_cases {
        let person = Noun::new("person", pronoun);
        assert_eq!(say!("{=0 =%take}", person), expected, "pronoun: {pronoun}");
    }
}

#[test]
fn past_passive_basic() {
    let sword = Noun::new("sword", "it");
    assert_eq!(say!("{=0 <=%take}", sword), "It was taken");
    let swords = Noun::new("swords", "they");
    assert_eq!(say!("{=0 <=%take}", swords), "They were taken");
}

#[test]
fn past_passive_irregular() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 <=%write}", person), "He was written");
    assert_eq!(say!("{=0 <=%eat}", person), "He was eaten");
}

#[test]
fn past_passive_all_pronouns() {
    let test_cases = vec![
        ("I", "I was taken"),
        ("you", "You were taken"),
        ("he", "He was taken"),
        ("she", "She was taken"),
        ("it", "It was taken"),
        ("we", "We were taken"),
        ("they", "They were taken"),
    ];
    for (pronoun, expected) in test_cases {
        let person = Noun::new("person", pronoun);
        assert_eq!(say!("{=0 <=%take}", person), expected, "pronoun: {pronoun}");
    }
}

#[test]
fn future_perfect_basic() {
    let person = Noun::new("Alex", "they");
    assert_eq!(say!("{=0 >%take}", person), "They will have taken");
}

#[test]
fn future_perfect_is_invariant_across_pronouns() {
    let test_cases = vec!["I", "you", "he", "she", "it", "we", "they"];
    for pronoun in test_cases {
        let person = Noun::new("person", pronoun);
        let result = say!("{=0 >%pick}", person);
        assert!(
            result.ends_with("will have picked"),
            "pronoun {pronoun}: got {result}"
        );
    }
}

#[test]
fn present_perfect_progressive_basic() {
    let person = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 %=pick}", person), "He has been picking");
    let they = Noun::new("them", "they");
    assert_eq!(say!("{=0 %=pick}", they), "They have been picking");
}

#[test]
fn present_perfect_progressive_all_pronouns() {
    let test_cases = vec![
        ("I", "I have been picking"),
        ("you", "You have been picking"),
        ("he", "He has been picking"),
        ("she", "She has been picking"),
        ("it", "It has been picking"),
        ("we", "We have been picking"),
        ("they", "They have been picking"),
    ];
    for (pronoun, expected) in test_cases {
        let person = Noun::new("person", pronoun);
        assert_eq!(say!("{=0 %=pick}", person), expected, "pronoun: {pronoun}");
    }
}

#[test]
fn past_perfect_progressive_basic() {
    let person = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 <%=pick}", person), "She had been picking");
}

#[test]
fn past_perfect_progressive_is_invariant_across_pronouns() {
    for pronoun in ["I", "you", "he", "she", "it", "we", "they"] {
        let person = Noun::new("person", pronoun);
        let result = say!("{=0 <%=pick}", person);
        assert!(
            result.ends_with("had been picking"),
            "pronoun {pronoun}: got {result}"
        );
    }
}

#[test]
fn say_with_no_override_matches_say_for_all_five_markers() {
    // say_with!() with no ctx.tense override must reproduce say!()'s output
    // byte-for-byte for every one of the five new markers, exactly like the
    // pre-existing six (tests/ranting/runtime_tense.rs).
    let he = Noun::new("person", "he");
    let ctx = NarrationContext::new();
    assert_eq!(say_with!(ctx, "{=0 =%take}", he), say!("{=0 =%take}", he));
    assert_eq!(say_with!(ctx, "{=0 <=%take}", he), say!("{=0 <=%take}", he));
    assert_eq!(say_with!(ctx, "{=0 >%take}", he), say!("{=0 >%take}", he));
    assert_eq!(say_with!(ctx, "{=0 %=pick}", he), say!("{=0 %=pick}", he));
    assert_eq!(say_with!(ctx, "{=0 <%=pick}", he), say!("{=0 <%=pick}", he));
}

#[test]
fn ctx_tense_override_preserves_passive_voice() {
    // The naive-extension trap the design spike calls out: a `ctx.tense`
    // override must move only the tense axis, never silently strip the
    // marker's voice. `{=%take}` is written passive; every override below
    // must still render passive.
    let it = Noun::new("sword", "it");

    let present = NarrationContext::new().tense(Tense::Present);
    assert_eq!(say_with!(present, "{=0 =%take}", it), "It is taken");

    let past = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(past, "{=0 =%take}", it), "It was taken");

    let future = NarrationContext::new().tense(Tense::Future);
    assert_eq!(say_with!(future, "{=0 =%take}", it), "It will be taken");

    // Starting from the past-passive spelling, an override to the present
    // axis must move it back to present passive, not to plain active past.
    let present_again = NarrationContext::new().tense(Tense::Present);
    assert_eq!(say_with!(present_again, "{=0 <=%take}", it), "It is taken");
}

#[test]
fn ctx_tense_override_preserves_perfect_progressive_voice() {
    let he = Noun::new("person", "he");

    let past = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(past, "{=0 %=pick}", he), "He had been picking");

    let future = NarrationContext::new().tense(Tense::Future);
    assert_eq!(
        say_with!(future, "{=0 %=pick}", he),
        "He will have been picking"
    );

    let present = NarrationContext::new().tense(Tense::Present);
    assert_eq!(
        say_with!(present, "{=0 <%=pick}", he),
        "He has been picking"
    );
}

#[test]
fn ctx_tense_override_moves_future_perfect_through_the_shared_perfect_spellings() {
    // `>%`'s present/past members reuse the pre-existing `%`/`<%` spellings
    // (docs/superpowers/specs/2026-08-15-participle-channel.md), so an
    // override away from the future axis renders exactly what a `%`/`<%`
    // placeholder would.
    let he = Noun::new("person", "he");

    let present = NarrationContext::new().tense(Tense::Present);
    assert_eq!(say_with!(present, "{=0 >%take}", he), "He has taken");

    let past = NarrationContext::new().tense(Tense::Past);
    assert_eq!(say_with!(past, "{=0 >%take}", he), "He had taken");
}

#[test]
fn ctx_tense_override_leaves_existing_six_markers_unaffected() {
    // The six pre-existing markers keep today's full-table override
    // (tests/ranting/runtime_tense.rs::context_overrides_compile_time_marker):
    // this item must not change that behavior.
    let he = Noun::new("person", "he");
    let future = NarrationContext::new().tense(Tense::Future);
    assert_eq!(say_with!(future, "{=0 %take}", he), "He will take");
}

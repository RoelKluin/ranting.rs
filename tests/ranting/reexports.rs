// Tests for ROADMAP.md Phase 6 item 12: `say_with!` and `derive_ranting` are
// re-exported from `ranting` itself, so a crate depending only on `ranting`'s
// public API (not `ranting_derive` directly) can reach both. Deliberately
// does NOT `use ranting_derive::*;` like other integration tests do — the
// whole point is proving these two are reachable through `ranting` alone.
use ranting::*;

#[test]
fn say_with_reachable_without_ranting_derive() {
    let jordan = Noun::new("Jordan", "they");
    let ctx = NarrationContext {
        tense: Some(Tense::Past),
        ..Default::default()
    };
    assert_eq!(
        say_with!(ctx, "{=jordan <arrive} here."),
        "They arrived here.".to_string()
    );
    // No override present: matches say!()'s own output.
    assert_eq!(
        say_with!(NarrationContext::default(), "{=jordan <arrive} here."),
        say!("{=jordan <arrive} here.")
    );
}

#[derive_ranting]
#[ranting(subject = "they", name = "Alex")]
struct Person {}

#[test]
fn derive_ranting_reachable_without_ranting_derive() {
    let alex = Person {};
    assert_eq!(
        say!("{=alex are} a wonderful colleague."),
        "They are a wonderful colleague.".to_string()
    );
}

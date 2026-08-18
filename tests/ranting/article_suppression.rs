// (c) Roel Kluin 2026 MIT
// Per-instance article suppression on `Noun`: `skip_article()` used to be baked as a compile-time
// literal per type by `#[ranting(no_article)]`, so `Noun` -- which derives without it -- always
// returned `false` and gave no way to mark one instance (a proper name, "Alice") as article-less
// without also making every other `Noun` article-less. `no_article` now takes the same `"$"`
// sentinel `mass`/`gender`/`singular_end`/`plural_end` already use, and `Noun` reads it from a
// runtime field via `Noun::with_skip_article(bool)`. Mirrors `mass_count.rs`'s test shape.
use ranting::*;

#[test]
fn derived_no_article_attribute_still_bakes_a_literal() {
    // The pre-existing, type-level shape is unaffected: `#[ranting(no_article)]` still bakes one
    // literal for the whole type, not a per-instance flag.
    #[derive_ranting]
    #[ranting(subject = "it", name = "chess", no_article)]
    struct Chess {}

    #[derive_ranting]
    #[ranting(subject = "it", name = "item")]
    struct Item {}

    assert!(Chess {}.skip_article());
    assert!(!Item {}.skip_article());
}

#[test]
fn noun_carries_skip_article_via_with_skip_article() {
    let alice = Noun::new("Alice", "she").with_skip_article(true);
    assert!(alice.skip_article());

    // Unset by default.
    let dog = Noun::new("dog", "it");
    assert!(!dog.skip_article());

    // Additivity, from the rendering side: a non-suppressed noun renders byte-identically to
    // before this channel existed.
    assert_eq!(say!("{a dog}"), "A dog".to_string());
}

#[test]
fn a_proper_name_renders_without_its_article() {
    let alice = Noun::new("Alice", "she").with_skip_article(true);
    assert_eq!(say!("{a alice} walked in."), "Alice walked in.".to_string());
    assert_eq!(
        say!("{the alice} walked in."),
        "Alice walked in.".to_string()
    );
}

#[test]
fn with_skip_article_false_is_explicitly_a_no_op() {
    let dog = Noun::new("dog", "it").with_skip_article(false);
    assert_eq!(say!("{a dog}"), "A dog".to_string());
}

#[test]
fn wrappers_delegate_skip_article_of_their_inner_value() {
    let alice = || Noun::new("Alice", "she").with_skip_article(true);

    assert!(Box::new(alice()).skip_article());
    assert!(Maybe(Some(alice())).skip_article());
    assert!(Many(vec![alice()]).skip_article());

    // `Many`'s own rule (not delegation): a 2+ item collection never suppresses regardless of
    // its members, but an empty one always does -- nothing to put an article in front of.
    assert!(!Many(vec![alice(), Noun::new("dog", "it")]).skip_article());
    assert!(Many::<Noun>(vec![]).skip_article());

    // `Maybe`'s own rule: `None` renders nothing, so it suppresses the article too.
    assert!(Maybe::<Noun>(None).skip_article());
}

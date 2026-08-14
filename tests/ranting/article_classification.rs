//! Pins `get_article_or_so`'s two classification call sites in
//! `handle_placeholder_impl` (`src/lib.rs`): the first word of the `pre`
//! capture (call site 1, lowercased before classification), and -- when
//! that first word isn't itself an article/`` ` ``-possessive -- the
//! second, "chained" word split out of the remainder (call site 2, *not*
//! lowercased; see ROADMAP.md's `get_article_or_so` fixability note for why
//! that asymmetry matters). Written before moving the classification to
//! compile time (`ArticleKind`), so these keep passing unchanged across
//! that refactor.

use ranting::*;

/// Call site 2: a verb-like first word ("can", not an article) followed by
/// a genuine nested article ("the") that only classifies correctly if the
/// second word is checked too, not just the first.
#[test]
fn chained_article_after_modal() {
    let friends = Noun::new("friend", "they");
    let result = say!("{can the #0 friends} here?", 2);
    assert_eq!(result, "Can the two friend here?".to_string());
}

/// Call site 1 with an embedded backtick-possessive combined with leading
/// verb text and trailing extra words, all in the same `pre` capture --
/// the case ROADMAP.md's fixability note traces through in detail.
/// `has_possesive` being true here routes away from call site 2 entirely,
/// so the "pair of" extra words are never re-classified, just appended
/// verbatim.
#[test]
fn combined_verb_and_backtick_possessive() {
    let boots = Noun::new("shoe", "it");
    let man = Noun::new("Old man", "he");
    let result = say!("{can `man pair of #0 boots remain} singular?", 2);
    assert_eq!(
        result,
        "Can his pair of two shoes remain singular?".to_string()
    );
}

/// A `?`-marked article ("display depends on the entity", README's `{?the 0}` syntax) must
/// classify as the article it is.
///
/// It didn't: `ArticleKind::classify` stripped a leading `!` and nothing else, so `?the` fell
/// through to `ArticleKind::Other` -- which is the *pre-noun verb* path -- and the article was
/// conjugated as a verb. `{?the dog}` rendered "?thes dog" and `{?a dog}` rendered "?as dog",
/// silently, at both compile time and run time. See
/// docs/architecture-review-2026-08-14.md §1.5.
///
/// The marker is only ever *consumed*: on an entity that renders articles normally, `?the` is
/// exactly `the` -- the entity is consulted for every article kind regardless (the
/// `skip_article()` early return in `get_article_or_so` runs before the classification match),
/// so `?` asks for the behavior that is already the default.
#[test]
fn question_marked_article_is_still_an_article() {
    let dog = Noun::new("dog", "it");
    assert_eq!(say!("{?the dog}"), "The dog".to_string());
    assert_eq!(say!("{?a dog}"), "A dog".to_string());
    assert_eq!(say!("I saw {?the dog}"), "I saw the dog".to_string());
    assert_eq!(say!("I saw {?a dog}"), "I saw a dog".to_string());

    // The marker is consumed, not rendered, in the plural/`an` forms too.
    assert_eq!(say!("I saw {?some +dog}"), "I saw some dogs".to_string());
    let apple = Noun::new("apple", "it");
    assert_eq!(say!("I ate {?an apple}"), "I ate an apple".to_string());
}

/// The other half of the same syntax, which is what every existing test and doc example
/// exercised and is why the bug above survived: an entity that skips articles omits it, marker
/// or no marker.
#[test]
fn question_marked_article_still_omitted_by_no_article() {
    #[derive_ranting]
    #[ranting(subject = "it", no_article = true)]
    struct Breakfast {}

    assert_eq!(
        say!("{?the 0} was great!", Breakfast {}),
        // Lowercase: a derive-generated `name()` reads `uc == true` as "as written", which is
        // the `OrthographyRole::Noun` rule -- this is README.md's own worked example.
        "breakfast was great!".to_string()
    );
}

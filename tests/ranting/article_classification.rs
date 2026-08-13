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

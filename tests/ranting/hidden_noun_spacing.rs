// (c) Roel Kluin 2026 MIT
//
// A hidden noun (`?`) preceded by an explicit article/pre-word within the same placeholder used
// to drop *both* the separator before it and the separator after it, since both were pushed only
// when `case != CaseKind::Hidden` -- `say!("{the ?jane !!good} in class", jane)` rendered
// "Thebest in class" instead of "The best in class". Fixed in src/lib.rs's
// handle_placeholder_impl: when hidden, push `noun_space` (the gap between the article and the
// noun) whenever something actually follows within the placeholder (`post_leading_space`
// non-empty) -- `noun_space` alone already encodes "did an article render before this point",
// and gating on `post_leading_space` keeps a hidden noun with nothing after it at all
// (`PostSpec::None`, e.g. `{can ?w}`) from gaining a stray trailing space it has nothing to
// separate from.

use ranting::*;

#[test]
fn article_then_hidden_noun_then_degree_gets_exactly_one_space() {
    let jane = Noun::new("Jane", "she");
    assert_eq!(
        say!("{the ?jane !!good} in class.", jane),
        "The best in class.".to_string()
    );
}

#[test]
fn article_then_hidden_noun_then_verb_gets_exactly_one_space() {
    let jane = Noun::new("Jane", "she");
    assert_eq!(
        say!("{the ?jane <%receive} a bad mark.", jane),
        "The had received a bad mark.".to_string()
    );
}

#[test]
fn a_full_sentence_links_a_hidden_noun_across_two_later_placeholders() {
    // The motivating case: refer to `jane` by name once, then drive a superlative adjective and
    // a past-perfect verb from two further placeholders that never repeat her name.
    let jane = Noun::new("Jane", "she");
    assert_eq!(
        say!(
            "{jane}, {the ?jane !!good} in class, {?jane <%receive} a bad mark.",
            jane
        ),
        "Jane, the best in class, had received a bad mark.".to_string()
    );
}

#[test]
fn hidden_noun_with_no_article_and_no_post_spec_is_unaffected() {
    // Regression guard: a hidden noun with nothing before or after it within its own
    // placeholder must not gain a stray trailing space just because a later placeholder in the
    // template pushes something -- `{can ?w}` renders "can" with no trailing space; the
    // template's own literal " " between placeholders supplies the separator to what follows.
    let w = Noun::new("", "he");
    assert_eq!(
        say!("{can ?w} {?w see} {?w may} {do =w}.", w),
        "Can sees may does he.".to_string()
    );
}

#[test]
fn hidden_noun_with_no_preceding_article_is_still_unaffected() {
    // The pre-existing, already-correct case (docs/CHEATSHEET.md's degree-marker example):
    // no article precedes the hidden noun, so there is nothing to separate from what follows.
    let w = Noun::new("student", "she");
    assert_eq!(say!("{?w !!good} in class", w), "Best in class".to_string());
}

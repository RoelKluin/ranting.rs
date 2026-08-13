// (c) Roel Kluin 2026 MIT
//
// ROADMAP.md Phase 6 item 17: `PH_START` (`ranting_core::grammar`) decides a
// placeholder is sentence-initial -- and so should auto-capitalize -- only
// after an ASCII `.`/`?`/`!` followed by whitespace, before this change. That
// silently never fired after a Greek question mark (a character that looks
// like an ASCII semicolon but is a distinct Unicode codepoint, U+037E), after
// Japanese/Chinese full-width punctuation (which takes no following space),
// or before a Spanish opening inverted question mark.
//
// These tests pin the three widened cases the ROADMAP item names explicitly.
// `SENTENCE_TRIGGER_CHARS` in `ranting_core::grammar` is the single source of
// truth both `PH_START` and `ranting_derive`'s `at_sentence_start` check read
// from, so there is exactly one place that can drift.
//
// Note: these templates use the literal Unicode characters rather than
// `\u{...}` escapes -- `say!()` matches its placeholder grammar against the
// literal's raw source text (to keep compile-error spans accurate), and a
// `\u{XXXX}` escape's own braces would otherwise be mistaken for a
// placeholder.
use ranting::*;

#[test]
fn greek_question_mark_triggers_capitalization() {
    // Greek question mark, then required whitespace -- same shape as ASCII
    // `.`/`?`/`!`, since Greek still space-separates words.
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("Τι είναι αυτό; {=thing} είναι εδώ.", thing),
        "Τι είναι αυτό; It είναι εδώ.".to_string()
    );
}

#[test]
fn greek_question_mark_mid_sentence_does_not_capitalize() {
    // No terminator directly precedes the placeholder here -- only ordinary
    // text -- so it stays lowercase, exactly like the ASCII case.
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("Λέει ότι {=thing} είναι εδώ;", thing),
        "Λέει ότι it είναι εδώ;".to_string()
    );
}

#[test]
fn japanese_full_width_period_triggers_capitalization_with_no_space() {
    // The full-width Japanese period takes no following whitespace at all --
    // Japanese doesn't space-separate words -- so this is its own
    // alternative in `PH_START`, not the "terminator + required \s+" shape
    // ASCII/Greek/Urdu use.
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("これは何ですか。{=thing}はここにあります。", thing),
        "これは何ですか。Itはここにあります。".to_string()
    );
}

#[test]
fn japanese_mid_sentence_does_not_capitalize() {
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("彼は{=thing}を見た。", thing),
        "彼はitを見た。".to_string()
    );
}

#[test]
fn spanish_opening_inverted_question_mark_triggers_capitalization() {
    // The Spanish opening inverted question mark marks sentence-initial from
    // the *opening* side, and directly abuts the following word (no
    // whitespace required, though it tolerates some) -- the mirror image of
    // the Japanese case above.
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("Dijo: ¿{=thing} es correcto?", thing),
        "Dijo: ¿It es correcto?".to_string()
    );
}

#[test]
fn spanish_opening_mark_with_following_space_also_triggers() {
    // The opening mark followed by a space before the placeholder is also
    // accepted -- `\s*+` in `PH_START`, not `\s+` -- since some Spanish
    // house styles put one there.
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("Dijo: ¿ {=thing} es correcto?", thing),
        "Dijo: ¿ It es correcto?".to_string()
    );
}

#[test]
fn spanish_mid_sentence_without_opening_mark_does_not_capitalize() {
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("Ella pregunta si {=thing} es correcto.", thing),
        "Ella pregunta si it es correcto.".to_string()
    );
}

#[test]
fn ascii_terminators_are_unaffected_by_the_widening() {
    // Regression guard: the pre-existing ASCII `.`/`?`/`!` + whitespace path
    // is untouched by the new alternatives.
    let thing = Noun::new("thing", "it");
    assert_eq!(
        say!("First. {=thing} second.", thing),
        "First. It second.".to_string()
    );
    assert_eq!(
        say!("Mid-sentence {=thing} stays lowercase.", thing),
        "Mid-sentence it stays lowercase.".to_string()
    );
}

// (c) Roel Kluin 2026 MIT
//! Integration tests for comparative & superlative adjectives (ROADMAP Phase 3 item 6).
//!
//! `!` and `!!` are post-noun degree markers (see README.md's placeholder grammar
//! list): `{noun !word}` bakes the comparative form of `word`, `{noun !!word}`
//! bakes the superlative form. Degree needs no subject/number/tense agreement,
//! so unlike the `~TENSE~` verb markers it's resolved once, entirely at compile
//! time, from `ranting_derive/src/language/adjective.rs`'s irregular table
//! (generated from `data/irregular_adjectives.txt`) and regular `-er`/`-est`
//! (or `more`/`most`) fallback rules.

use ranting::*;

#[test]
fn comparative_irregular_good() {
    let alex = Noun::new("Alex", "they");
    assert_eq!(
        say!(
            "{=alex are} {?alex !good} than {@bob}.",
            alex,
            bob = Noun::new("Bob", "he")
        ),
        "They are better than him.".to_string()
    );
}

#[test]
fn superlative_irregular_good() {
    let alex = Noun::new("Alex", "they");
    assert_eq!(
        say!("the {?alex !!good} player", alex),
        "the best player".to_string()
    );
}

#[test]
fn comparative_and_superlative_irregular_bad() {
    let w = Noun::new("it", "it");
    assert_eq!(
        say!("This is {?w !bad} than that.", w),
        "This is worse than that.".to_string()
    );
    assert_eq!(say!("This is {?w !!bad}.", w), "This is worst.".to_string());
}

#[test]
fn comparative_regular_monosyllabic() {
    let w = Noun::new("it", "it");
    // "fast" -> no doubling (two trailing consonants)
    assert_eq!(say!("{,?w !fast}", w), "faster".to_string());
    // "big" -> doubles final consonant (CVC)
    assert_eq!(say!("{,?w !!big}", w), "biggest".to_string());
    // "large" -> ends in e, just append r/st
    assert_eq!(say!("{,?w !large}", w), "larger".to_string());
}

#[test]
fn comparative_regular_two_syllable_y() {
    let w = Noun::new("it", "it");
    // "happy" -> two syllables ending in consonant+y: y -> i, add er/est
    assert_eq!(say!("{,?w !happy}", w), "happier".to_string());
    assert_eq!(say!("{,?w !!happy}", w), "happiest".to_string());
}

#[test]
fn comparative_periphrastic_multisyllable() {
    let w = Noun::new("it", "it");
    // "beautiful" -> 3 syllables, not a suffix-eligible ending: more/most
    assert_eq!(say!("{,?w !beautiful}", w), "more beautiful".to_string());
    assert_eq!(say!("{,?w !!beautiful}", w), "most beautiful".to_string());
}

#[test]
fn degree_marker_capitalizes_at_sentence_start() {
    let w = Noun::new("it", "it");
    assert_eq!(
        say!("{^?w !good} things are ahead.", w),
        "Better things are ahead.".to_string()
    );
}

#[test]
fn degree_marker_with_trailing_words() {
    let w = Noun::new("it", "it");
    assert_eq!(
        say!("{,?w !good at chess} than yesterday.", w),
        "better at chess than yesterday.".to_string()
    );
}

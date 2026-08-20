//! The falsification half of this crate: what French cannot get through `ranting`'s public API,
//! plus the two confirmation findings worth pinning the same way. Mirrors
//! `ranting_es/tests/holes.rs`'s and `ranting_ar/tests/holes.rs`'s structure and naming.
//!
//! Each test asserts what the crate *actually* produces, not what French needs, and is named
//! after the numbered hole (or confirmation) in this crate's README.md. They are pins: if a
//! later change closes the hole, the test fails and the entry gets struck from the README
//! rather than quietly rotting.

use ranting::say;
use ranting_fr::FrenchNoun;

// ------------------------------------------ hole 1: prenominal adjectives --

#[test]
fn hole_1_prenominal_adjectives_agree_correctly_in_the_wrong_position() {
    // `grand` is real French, and `inflect_adjective_custom` agrees it correctly (`grand` /
    // `grande` / `grands` / `grandes`, verified below) -- but the only position the `!`/`!!`
    // degree slot can render an adjective in is post-noun, and correct French puts this word
    // *before* the noun ("grand chat", not "chat grand"). The mechanism works; the word order
    // is wrong in every template, exactly the shape German's adjectives take for the *whole*
    // language, here narrowed to this one closed set of eight words
    // (`lexicon::is_prenominal`).
    assert_eq!(
        say!("{the *=0 !grand}", FrenchNoun::chat()),
        "Le chat grand"
    );
    // Confirmed this is a position hole, not a missing-agreement-data one: the feminine and
    // plural forms are exactly right, just in the wrong place.
    assert_eq!(
        say!("{the *=0 !grand}", FrenchNoun::maison()),
        "La maison grande"
    );
    assert_eq!(
        say!("{the +*=0 !grand}", FrenchNoun::chat()),
        "Les chats grands"
    );
    assert!(ranting_fr::lexicon::is_prenominal("grand"));
    // The only way to get correct French word order is to write the adjective as literal
    // template text, where no hook can inflect it -- the same escape hatch (and the same
    // limitation) `ranting_i18n`'s hole 4a documents for German.
    assert_eq!(
        say!("{the ?0} grand {*=0}", FrenchNoun::chat()),
        "Le grand chat"
    );
}

// --------------------------- confirmation: is_mass()/partitive article --

#[test]
fn confirmation_is_mass_is_sufficient_for_the_partitive_article() {
    // Not a hole: the first fork to override `is_mass()` at all finds the hook signature
    // already sufficient. `get_article_or_so` resolves a mass noun's indefinite slot before
    // handing the word to `inflect_article_custom`, so this crate can answer `du`/`de la`
    // directly for every written form of that slot (`a`, `an`, or a literal `some`).
    assert_eq!(say!("{a *=0}", FrenchNoun::eau()), "De l'eau");
    assert_eq!(say!("{some *=0}", FrenchNoun::eau()), "De l'eau");
}

// ------------------------------- confirmation: h aspiré blocks elision --

#[test]
fn confirmation_elide_article_custom_supports_a_negative_case() {
    // Not a hole either: `elide_article_custom` returning `None` was always documented as "keep
    // article, separator and following exactly as rendered" -- this is simply the first fork to
    // need that path for a word that *looks* elidable and isn't. `héros`'s `h` is spelled
    // identically to `homme`'s; only the entity-carried `h_aspire` flag tells them apart.
    assert_eq!(say!("{the *=0}", FrenchNoun::homme()), "L'homme");
    assert_eq!(say!("{the *=0}", FrenchNoun::heros()), "Le héros");
}

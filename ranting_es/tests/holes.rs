//! The falsification half of ROADMAP.md Phase 6 item 23: what Spanish cannot get through
//! `ranting`'s public API. Mirrors `ranting_i18n/tests/holes.rs`'s structure and naming.
//!
//! Each test asserts what the crate *actually* produces, not what Spanish needs, and is named
//! after the numbered hole in this crate's README.md. They are pins: if a later change closes
//! one of these, the test fails and the hole gets struck from the README rather than quietly
//! rotting.

use ranting::say;
use ranting_es::SpanishNoun;

// ------------------------------- hole 1: preposition-article fusion + slot --

#[test]
fn hole_1_de_el_does_not_fuse_to_del() {
    // Spanish contracts "de" + "el" to "del" and "a" + "el" to "al" — its only two article
    // fusions. `elide_article_custom` runs *after* the article inside the placeholder has been
    // rendered, but "de"/"a" here are template literal text *before* the placeholder even
    // starts, so the hook never receives them at all — the exact structural gap
    // `ranting_i18n`'s README records as hole 7 (ROADMAP.md Phase 6 item 7's own "not reachable"
    // note: "the preposition lives in the template's literal text, outside the placeholder").
    // Overriding `elide_article_custom` here would be dead code, so this crate doesn't.
    assert_eq!(
        say!("Vengo de {the *=0}.", SpanishNoun::gato()),
        "Vengo de el gato." // Spanish wants "Vengo del gato."
    );
    assert_eq!(
        say!("Voy a {the *=0}.", SpanishNoun::gato()),
        "Voy a el gato." // Spanish wants "Voy al gato."
    );
    // Confirmed not a per-noun-gender quirk: "de la" is already correct Spanish (no fusion for
    // the feminine article), so this crate isn't missing anything for `casa`.
    assert_eq!(
        say!("Vengo de {the *=0}.", SpanishNoun::casa()),
        "Vengo de la casa."
    );
    // As `ranting_i18n` also records for German: the escape hatch of writing the preposition
    // *inside* the placeholder, where a hook could see it, does not exist. The pre-noun slot
    // accepts only an article (`a`/`an`/`some`/`the`/`these`/`those`) or one of `ranting`'s
    // hard-coded English modal words — `say!("{de the *=0}", gato)` is a *compile* error
    // ("expected article or verb"), not something this test can execute without breaking the
    // whole crate's build, so it's recorded here in prose rather than as a second runtime
    // assertion (the same choice `ranting_i18n`'s hole 7 test makes).
}

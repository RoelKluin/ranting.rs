//! The falsification half of ROADMAP.md Phase 6 item 23: what Spanish cannot get through
//! `ranting`'s public API. Mirrors `ranting_i18n/tests/holes.rs`'s structure and naming.
//!
//! Each test asserts what the crate *actually* produces, not what Spanish needs, and is named
//! after the numbered hole in this crate's README.md. They are pins: if a later change closes
//! one of these, the test fails and the hole gets struck from the README rather than quietly
//! rotting.

use ranting::say;
use ranting_es::SpanishNoun;

// --------------------------------- hole 1: preposition-article fusion (closed) --

// ROADMAP.md Phase 6 item 26 added `Ranting::inflect_preposition_custom`, fed the literal word
// immediately before a placeholder (`ranting_derive::parse_str_params` now forwards it instead of
// discarding it) and the rendered article, called at the same post-assembly point
// `elide_article_custom` runs at. `SpanishNoun::inflect_preposition_custom` answers it for
// Spanish's two obligatory fusions. Kept as `hole_1_*` (not renamed) so it stays findable from the
// README/ROADMAP cross-references; the assertions below now show the fused forms instead of the
// previous `"Vengo de el gato."`/`"Voy a el gato."`.

#[test]
fn hole_1_de_el_now_fuses_to_del() {
    // Spanish contracts "de" + "el" to "del" and "a" + "el" to "al" — its only two article
    // fusions.
    assert_eq!(
        say!("Vengo de {the *=0}.", SpanishNoun::gato()),
        "Vengo del gato."
    );
    assert_eq!(
        say!("Voy a {the *=0}.", SpanishNoun::gato()),
        "Voy al gato."
    );
    // Confirmed not a per-noun-gender quirk: "de la" is already correct Spanish (no fusion for
    // the feminine article) and stays unfused, since `inflect_preposition_custom` only answers
    // ("de"|"a", "el").
    assert_eq!(
        say!("Vengo de {the *=0}.", SpanishNoun::casa()),
        "Vengo de la casa."
    );
    // The pre-noun placeholder slot itself is still a closed English word list --
    // `say!("{de the *=0}", gato)` is still a *compile* error ("expected article or verb") -- the
    // same pre-noun-slot restriction `ranting_i18n`'s hole 7 documents. Item 26 never needed that
    // to change: the preposition reaches the hook as template literal text, not from inside the
    // placeholder.
}

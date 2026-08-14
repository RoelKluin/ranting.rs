//! The falsification half of ROADMAP.md Phase 7 item 5: what Arabic cannot get through
//! `ranting`'s public API. Mirrors `ranting_i18n/tests/holes.rs` and `ranting_es/tests/holes.rs`
//! in structure and naming.
//!
//! Each test asserts what the crate *actually* produces, not what Arabic needs, and is named
//! after the numbered hole in this crate's README.md. They are pins: if a later change closes one
//! of these, the test fails and the hole gets struck from the README rather than quietly rotting.

use ranting::say;
use ranting_ar::ArabicNoun;

// ------------------------------------------- hole 1: the dual needs a numeral in the placeholder --

#[test]
fn hole_1_a_bare_dual_is_unreachable() {
    // Arabic marks the dual on the noun with no numeral written at all — `كتابان` is "two books",
    // spoken and written without a digit. `+`/`-` are the only number markers the placeholder
    // grammar accepts, and `+` means "plural", so there is no marker a bare dual could use.
    //
    // This is a *grammar* change, not a hook signature, and the number-categories spec puts it
    // deliberately out of scope. Phase 7 item 11 closed the numbered half of the gap and left
    // this half open on purpose.
    let kitab = ArabicNoun::kitab();
    assert_eq!(say!("{+0}", kitab), "كتب"); // the plural; Arabic wants `كتابان`
    assert_eq!(say!("{the +0}", kitab), "الكتب");

    // The workaround `ranting_i18n` uses for definiteness — carry it as entity state — is
    // available in principle, and this crate deliberately does **not** take it: an `ArabicNoun`
    // has no `dual` field, so this test pins the gap instead of hiding it.
}

// ---------------------------------------------------- hole 2: no genitive in `GrammaticalCase` --

#[test]
fn hole_2_the_genitive_case_has_no_marker() {
    // Arabic nouns decline for three cases — nominative, accusative, genitive — and the genitive
    // is obligatory after any preposition (`في الكتابِ`). `GrammaticalCase` carries English's
    // five-marker inventory, so a placeholder can say "subjective" or "objective" and nothing
    // else; there is no marker for the case a preposition governs.
    //
    // This is the same shape as `ranting_i18n`'s hole 3, reached by a different route: German
    // needs a dative, Arabic a genitive, and neither exists. Reconfirmation from an unrelated
    // family is worth recording — the gap is not a German quirk.
    let kitab = ArabicNoun::kitab();
    // Unvocalized script does not write the case ending, so nothing here is *visibly* wrong —
    // which is the trap. A vocalized fork would need `الكتابِ` and could not ask for it.
    assert_eq!(say!("في {the 0}", kitab), "في الكتاب");
}

// ------------------------------------------- hole 3: object and possessive pronouns are suffixes --

#[test]
fn hole_3_bound_pronouns_cannot_attach_to_their_host() {
    // Arabic's object and possessive pronouns are **suffixes on the host word** — `كتابه` "his
    // book", `رأيته` "I saw him" — not free words. `inflect_pronoun_custom` returns a string that
    // is rendered as its own placeholder, so the closest this crate can do is emit the bare
    // suffix as if it were a word.
    //
    // Not a signature gap: it is the word-order boundary in a different costume. The suffix's
    // *position* is inside another word, and no inflection hook will ever change position.
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{@0}", muallim), "ه"); // a bare suffix standing alone
    assert_eq!(say!("{`0}", muallim), "ه"); // possessive determiner, same problem
}

// ------------------------------------------------------- hole 4: construct state (الإضافة) --

#[test]
fn hole_4_the_construct_state_is_not_expressible() {
    // In a possessive chain (إضافة) the *first* noun takes no article even when definite, and the
    // second takes the genitive: `كتاب المعلم` "the teacher's book" — not `الكتاب المعلم`.
    //
    // Definiteness here is a property of the noun's *position in the phrase*, and `skip_article`
    // is a property of the entity. A template writing `{the 0} {the 1}` gets an article on both.
    let kitab = ArabicNoun::kitab();
    let muallim = ArabicNoun::muallim();
    assert_eq!(say!("{the 0} {the 1}", kitab, muallim), "الكتاب المعلم");
    // Omitting the first article is expressible only by writing a different template, which is
    // the caller's job and not a gap — but nothing lets one *entity* know it is in construct.
    assert_eq!(say!("{0} {the 1}", kitab, muallim), "كتاب المعلم");
}

// ---------------------------------------------------------- hole 5: VSO word order (a boundary) --

#[test]
fn hole_5_verb_initial_word_order_is_a_boundary_not_a_gap() {
    // Classical Arabic's neutral order is verb-subject-object (`كتب المعلم الكتاب`). `ranting`
    // inflects words within a template; their order is the template's. This is recorded the way
    // `ranting_i18n`'s hole 8 is — as a permanent boundary that this crate reconfirms from a
    // non-Indo-European direction, not as an unclosed gap.
    //
    // The template simply writes VSO itself, and every word still inflects correctly:
    let muallim = ArabicNoun::muallim();
    let kitab = ArabicNoun::kitab();
    assert_eq!(
        say!("{0 كتب} {1} {the 2}", muallim, muallim, kitab),
        "معلم كتب معلم الكتاب"
    );
    // What is unreachable is a *hook* that reorders — and none will ever exist. See
    // `docs/EXTENSIBILITY.md` §2.12.
}

// ------------------------------------- hole 6: the numeral-noun separator (shared with Japanese) --

#[test]
fn hole_6_the_numeral_is_always_separated_from_its_noun_by_a_space() {
    // `handle_placeholder_impl` pushes a hard-coded space between the rendered numeral and the
    // noun, and no hook is offered it — unlike the article separator, which
    // `elide_article_custom` receives and may drop. Arabic writes the space, so this costs
    // nothing *here*; it is recorded because this crate proves the asymmetry is real rather than
    // Japanese-specific (ROADMAP.md Phase 7 item 12, found by the item 3 spike).
    let kitab = ArabicNoun::kitab();
    assert_eq!(say!("{$0 1}", 2, kitab), "٢ كتابان");
    // The article, by contrast, binds — same placeholder, opposite treatment.
    assert_eq!(say!("{the 0}", kitab), "الكتاب");
}

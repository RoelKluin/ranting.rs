//! What Spanish *does* reach through `ranting`'s public hooks.
//!
//! Every assertion here is real Spanish. The things that come out wrong — or cannot be written
//! at all — are in `holes.rs`, asserted exactly as the crate renders them. Mirrors
//! `ranting_i18n/tests/german.rs`'s structure.

use ranting::say;
use ranting_es::{SpanishNoun, SpanishPerson};

// ---------------------------------------------------------------- articles --

#[test]
fn definite_article_by_gender() {
    assert_eq!(say!("{the *=0}", SpanishNoun::gato()), "El gato");
    assert_eq!(say!("{the *=0}", SpanishNoun::casa()), "La casa");
}

#[test]
fn definite_article_ignores_the_noun_ending_and_uses_noun_class_instead() {
    // `problema` ends in `-a` like `casa` but is masculine — the classic trap of guessing
    // gender from a noun's last letter (`docs/EXTENSIBILITY.md` §4.3's own pre-`NounClass`
    // example gets this wrong). `NounClass` makes it a non-issue: the entity declares its own
    // class, the hook never inspects `noun_singular`'s spelling.
    assert_eq!(say!("{the *=0}", SpanishNoun::problema()), "El problema");
}

#[test]
fn definite_article_plural_by_gender() {
    assert_eq!(say!("{the +*=0}", SpanishNoun::gato()), "Los gatos");
    assert_eq!(say!("{the +*=0}", SpanishNoun::casa()), "Las casas");
}

#[test]
fn indefinite_article_by_gender_and_number() {
    assert_eq!(say!("{a *=0}", SpanishNoun::gato()), "Un gato");
    assert_eq!(say!("{a *=0}", SpanishNoun::casa()), "Una casa");
    assert_eq!(say!("{a +*=0}", SpanishNoun::gato()), "Unos gatos");
    assert_eq!(say!("{a +*=0}", SpanishNoun::casa()), "Unas casas");
}

#[test]
fn el_agua_euphonic_singular_article() {
    // `agua` is feminine (adjectives agree feminine, see `adjective_agreement_on_agua_stays_
    // feminine_despite_the_el_article` below) but takes the masculine-*looking* singular
    // article to avoid the vowel clash `la agua` would cause — a phonological rule, not a
    // gender change. The plural reverts to the expected feminine `las`/`unas`.
    assert_eq!(say!("{the *=0}", SpanishNoun::agua()), "El agua");
    assert_eq!(say!("{a *=0}", SpanishNoun::agua()), "Un agua");
    assert_eq!(say!("{the +*=0}", SpanishNoun::agua()), "Las aguas");
    assert_eq!(say!("{a +*=0}", SpanishNoun::agua()), "Unas aguas");
}

// ------------------------------------------------------------------- verbs --

#[test]
fn verb_agreement_across_all_six_persons() {
    assert_eq!(say!("{=0 hablar}.", SpanishPerson::YO), "Yo hablo.");
    assert_eq!(say!("{=0 hablar}.", SpanishPerson::TU), "Tú hablas.");
    assert_eq!(
        say!("{the *=0 hablar}.", SpanishNoun::gato()),
        "El gato habla."
    );
    assert_eq!(
        say!("{=0 hablar}.", SpanishPerson::NOSOTROS),
        "Nosotros hablamos."
    );
    assert_eq!(
        say!("{=0 hablar}.", SpanishPerson::VOSOTROS),
        "Vosotros habláis."
    );
    assert_eq!(
        say!("{=0 hablar}.", SpanishPerson::USTEDES),
        "Ustedes hablan."
    );
}

#[test]
fn tu_and_usted_take_different_verb_agreement() {
    // The pair item 23 specifically asks this crate to exercise: `tú` (informal "you") takes
    // second-person-singular agreement, `usted` (formal "you") takes third-person-**singular**
    // agreement — the same person index as `él`/`ella`. That is the sharp contrast with German
    // `Sie` (`ranting_i18n::person::GermanPerson::SIE`), which borrows third-person-**plural**
    // agreement instead: two languages solving formal address the same grammatical way
    // (borrowing another person's slot) but picking a *different* slot to borrow.
    assert_eq!(say!("{=0 hablar}.", SpanishPerson::TU), "Tú hablas.");
    assert_eq!(say!("{=0 hablar}.", SpanishPerson::USTED), "Usted habla.");
    // Proof that "habla" is really third-person-singular agreement, not a form special to
    // "usted": a noun (always third-person-singular here) takes the identical verb form.
    assert_eq!(
        say!("{the *=0 hablar}.", SpanishNoun::gato()),
        "El gato habla."
    );
}

#[test]
fn irregular_ser_across_persons() {
    assert_eq!(say!("{=0 ser} feliz.", SpanishPerson::YO), "Yo soy feliz.");
    assert_eq!(say!("{=0 ser} feliz.", SpanishPerson::TU), "Tú eres feliz.");
    assert_eq!(
        say!("{the *=0 ser} negro.", SpanishNoun::gato()),
        "El gato es negro."
    );
}

#[test]
fn an_unknown_verb_falls_through_to_english_rather_than_being_guessed() {
    // Same decline-rather-than-guess contract `ranting_i18n` documents: `inflect_verb_custom`
    // returns `None` for a verb outside the closed set, and `ranting`'s English rules render
    // the bare form — "correr", not a conjugated Spanish form.
    assert_eq!(
        say!("{the *=0 correr}.", SpanishNoun::gato()),
        "El gato correr."
    );
}

// -------------------------------------------------------------- adjectives --

#[test]
fn postnominal_adjective_agreement_in_a_real_sentence() {
    // The exact examples ROADMAP.md item 23 names, and the reason Spanish is the second
    // acceptance test: unlike `ranting_i18n`'s hole 4a, there is no position mismatch here to
    // record — Spanish attributive adjectives *are* post-nominal, exactly where the `!` degree
    // slot renders, so this is correct, complete Spanish, not merely a mechanism demonstration.
    assert_eq!(
        say!("{the *=0 !negro}", SpanishNoun::gato()),
        "El gato negro"
    );
    assert_eq!(
        say!("{the *=0 !negro}", SpanishNoun::casa()),
        "La casa negra"
    );
    assert_eq!(
        say!("{the +*=0 !negro}", SpanishNoun::gato()),
        "Los gatos negros"
    );
    assert_eq!(
        say!("{the +*=0 !negro}", SpanishNoun::casa()),
        "Las casas negras"
    );
}

#[test]
fn adjective_agreement_on_agua_stays_feminine_despite_the_el_article() {
    // `agua`'s article looks masculine (`el`), but the noun's actual `NounClass` is still
    // "feminine" — the euphonic article is a display-only exception (see
    // `NounEntry::euphonic_el`'s doc comment), not a change of grammatical gender, and the
    // adjective agreement proves it: "fría", not "frío".
    assert_eq!(
        say!("{the *=0 !pequeño}", SpanishNoun::agua()),
        "El agua pequeña"
    );
}

#[test]
fn gender_invariant_adjective_still_takes_number_agreement() {
    // `azul` doesn't end in `-o`, so it doesn't change for gender — only for number, and with
    // the regular `-es` (not `-s`) ending consonant-final Spanish adjectives take.
    assert_eq!(say!("{the *=0 !azul}", SpanishNoun::gato()), "El gato azul");
    assert_eq!(say!("{the *=0 !azul}", SpanishNoun::casa()), "La casa azul");
    assert_eq!(
        say!("{the +*=0 !azul}", SpanishNoun::gato()),
        "Los gatos azules"
    );
}

#[test]
fn an_unknown_adjective_falls_through_to_the_english_degree_table() {
    assert_eq!(
        say!("{the *=0 !good}", SpanishNoun::gato()),
        "El gato better"
    );
}

// ---------------------------------------------------------------- numerals --

#[test]
fn spelled_numerals_agree_like_the_indefinite_article_at_one() {
    assert_eq!(say!("Veo {#0 1}.", 1, SpanishNoun::gato()), "Veo un gato.");
    assert_eq!(say!("Veo {#0 1}.", 1, SpanishNoun::casa()), "Veo una casa.");
    assert_eq!(say!("Veo {#0 1}.", 1, SpanishNoun::agua()), "Veo un agua.");
    assert_eq!(
        say!("Veo {#0 1}.", 2, SpanishNoun::gato()),
        "Veo dos gatos."
    );
    assert_eq!(
        say!("Veo {#0 1}.", 12, SpanishNoun::casa()),
        "Veo doce casas."
    );
}

#[test]
fn sentence_initial_numeral_takes_the_capital_not_the_noun() {
    // The main crate's engine spends a sentence-initial placeholder's capital on the
    // spelled numeral rather than the noun (docs/architecture-review-2026-08-15.md
    // §1.11); the same fix German's `spelled_numerals_agree_like_an_article_at_one`
    // pins, exercised here against a hook that returns `Some` rather than falling
    // through to English.
    assert_eq!(say!("{#0 1}.", 1, SpanishNoun::gato()), "Un gato.");
}

#[test]
fn a_numeral_outside_the_closed_range_falls_through_to_english() {
    assert_eq!(
        say!("Veo {#0 1}.", 40, SpanishNoun::gato()),
        "Veo forty gatos."
    );
}

#[test]
fn digit_numerals_are_left_alone_because_spanish_writes_the_same_digits() {
    assert_eq!(say!("Veo {$0 1}.", 3, SpanishNoun::casa()), "Veo 3 casas.");
}

// ------------------------------------------------------------- pronouns ----

#[test]
fn subject_pronouns_by_gender_and_number() {
    assert_eq!(say!("{=0 hablar}.", SpanishNoun::gato()), "Él habla.");
    assert_eq!(say!("{=0 hablar}.", SpanishNoun::casa()), "Ella habla.");
    assert_eq!(say!("{+=0 hablar}.", SpanishNoun::gato()), "Ellos hablan.");
    assert_eq!(say!("{+=0 hablar}.", SpanishNoun::casa()), "Ellas hablan.");
}

#[test]
fn object_pronouns_by_gender_and_number() {
    assert_eq!(say!("Vi {@0}.", SpanishNoun::gato()), "Vi lo.");
    assert_eq!(say!("Vi {@0}.", SpanishNoun::casa()), "Vi la.");
    // `agua`'s object pronoun is `la`, not `lo` — the euphonic exception belongs to the
    // article alone (see `el_agua_euphonic_singular_article` above), and doesn't leak into the
    // pronoun hook, which never even receives the `euphonic_el` flag as a parameter.
    assert_eq!(say!("Vi {@0}.", SpanishNoun::agua()), "Vi la.");
    assert_eq!(say!("Vi {+@0}.", SpanishNoun::gato()), "Vi los.");
}

#[test]
fn person_pronouns_decline_too() {
    // Spanish object clitics attach before the verb, so a natural sentence puts the
    // placeholder there — a word-order choice made in this template, not by a hook.
    assert_eq!(say!("{@0} ves.", SpanishPerson::YO), "Me ves.");
    assert_eq!(say!("{@0} veo.", SpanishPerson::TU), "Te veo.");
    assert_eq!(say!("{`0} gato.", SpanishPerson::TU), "Tu gato.");
    assert_eq!(
        say!("{=0 hablar} {%0}.", SpanishPerson::NOSOTROS),
        "Nosotros hablamos nos."
    );
}

// ------------------------------------------------- orthography / word order --

#[test]
fn inverted_question_mark_triggers_sentence_start_capitalization() {
    // `¿` marks sentence-initial from the *opening* side and abuts the placeholder directly —
    // ROADMAP.md Phase 6 item 17, already closed in `ranting_core` before this crate existed.
    // Nothing here had to be added for it; this test exists to show a real Spanish sentence
    // exercising it, the way `ranting_i18n` (German has no equivalent construction) could not.
    assert_eq!(
        say!("¿{the *=0 ser} negro?", SpanishNoun::gato()),
        "¿El gato es negro?"
    );
}

#[test]
fn plain_sentence_mid_text_does_not_capitalize() {
    assert_eq!(
        say!("Creo que {the *=0} es bonito.", SpanishNoun::gato()),
        "Creo que el gato es bonito."
    );
}

/// A Spanish template written *in Spanish*, not with English keywords.
///
/// Before 2026-08-14 the pre-noun slot accepted only a closed English vocabulary, so every
/// template in this file had to say `{the *=0}` and rely on `inflect_article_custom` to turn
/// the English keyword into Spanish output. An unrecognized pre-noun word is now handed to
/// that hook instead of being rendered as literal text, so the template can say what it means.
///
/// `ranting` still knows no Spanish: `el`/`la`/`los`/`las`/`un`/`una` are matched in
/// `SpanishNoun::inflect_article_custom`, in this crate. That is the whole point — the
/// vocabulary belongs to the language module, which is what makes languages modular.
#[test]
fn native_spanish_article_keywords() {
    // The written form selects the paradigm; gender and number still pick the form.
    assert_eq!(say!("Veo {el *=0}.", SpanishNoun::gato()), "Veo el gato.");
    assert_eq!(
        say!("Veo {el +*=0}.", SpanishNoun::gato()),
        "Veo los gatos."
    );
    assert_eq!(say!("Veo {la *=0}.", SpanishNoun::casa()), "Veo la casa.");
    assert_eq!(
        say!("Veo {la +*=0}.", SpanishNoun::casa()),
        "Veo las casas."
    );
    assert_eq!(say!("Veo {un *=0}.", SpanishNoun::gato()), "Veo un gato.");
    assert_eq!(say!("Veo {una *=0}.", SpanishNoun::casa()), "Veo una casa.");

    // Writing a form that disagrees with the entity is corrected, exactly as the English
    // keyword would be: `los` on a singular noun still renders `el`.
    assert_eq!(say!("Veo {los *=0}.", SpanishNoun::gato()), "Veo el gato.");

    // The English keyword still works, unchanged -- this is additive.
    assert_eq!(say!("Veo {the *=0}.", SpanishNoun::gato()), "Veo el gato.");

    // `el agua`: the euphonic-article rule is entity-carried, so it applies to the native
    // keyword exactly as it does to the English one.
    assert_eq!(say!("Veo {la *=0}.", SpanishNoun::agua()), "Veo el agua.");
}

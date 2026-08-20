//! What French *does* reach through `ranting`'s public hooks.
//!
//! Every assertion here is real French. The things that come out wrong — or cannot be written
//! at all — are in `holes.rs`, asserted exactly as the crate renders them. Mirrors
//! `ranting_es/tests/spanish.rs`'s structure.

use ranting::say;
use ranting_fr::{FrenchNoun, FrenchPerson};

// ---------------------------------------------------------------- articles --

#[test]
fn definite_article_by_gender() {
    assert_eq!(say!("{the *=0}", FrenchNoun::chat()), "Le chat");
    assert_eq!(say!("{the *=0}", FrenchNoun::maison()), "La maison");
}

#[test]
fn definite_article_plural_by_gender() {
    assert_eq!(say!("{the +*=0}", FrenchNoun::chat()), "Les chats");
    assert_eq!(say!("{the +*=0}", FrenchNoun::maison()), "Les maisons");
}

#[test]
fn indefinite_article_by_gender_and_number() {
    assert_eq!(say!("{a *=0}", FrenchNoun::chat()), "Un chat");
    assert_eq!(say!("{a *=0}", FrenchNoun::maison()), "Une maison");
    assert_eq!(say!("{a +*=0}", FrenchNoun::chat()), "Des chats");
    assert_eq!(say!("{a +*=0}", FrenchNoun::maison()), "Des maisons");
}

// --------------------------------------------------------- partitive/mass --

#[test]
fn a_mass_noun_takes_the_partitive_article_instead_of_the_indefinite() {
    // The first exercise of `is_mass()` by any falsifier — "some water" is "de l'eau", never
    // "une eau". Vowel-initial, so this also proves the partitive elides exactly like the bare
    // definite article does.
    assert_eq!(say!("{a *=0}", FrenchNoun::eau()), "De l'eau");
}

#[test]
fn a_mass_noun_still_takes_the_definite_article_normally() {
    // `is_mass()` only changes the *indefinite* slot -- "the water" is ordinary "l'eau", not
    // some special mass-noun definite form.
    assert_eq!(say!("{the *=0}", FrenchNoun::eau()), "L'eau");
}

// ------------------------------------------------------------------ elision --

#[test]
fn a_vowel_initial_noun_elides_the_article() {
    assert_eq!(say!("{the *=0}", FrenchNoun::arbre()), "L'arbre");
    assert_eq!(say!("{the *=0}", FrenchNoun::ecole()), "L'école");
}

#[test]
fn h_muet_elides_like_a_vowel() {
    assert_eq!(say!("{the *=0}", FrenchNoun::homme()), "L'homme");
}

#[test]
fn h_aspire_blocks_elision_despite_looking_identical_to_h_muet() {
    // The negative case: `héros` spells its first letter exactly like `homme`'s, but is
    // `h aspiré` and does not elide.
    assert_eq!(say!("{the *=0}", FrenchNoun::heros()), "Le héros");
}

#[test]
fn a_consonant_initial_noun_never_elides() {
    assert_eq!(say!("{the *=0}", FrenchNoun::chat()), "Le chat");
}

// ------------------------------------------------------------------- verbs --

#[test]
fn verb_agreement_across_persons() {
    assert_eq!(say!("{=0 parler}.", FrenchPerson::JE), "Je parle.");
    assert_eq!(say!("{=0 parler}.", FrenchPerson::TU), "Tu parles.");
    assert_eq!(
        say!("{the *=0 parler}.", FrenchNoun::chat()),
        "Le chat parle."
    );
    assert_eq!(say!("{=0 parler}.", FrenchPerson::NOUS), "Nous parlons.");
    assert_eq!(say!("{=0 parler}.", FrenchPerson::VOUS), "Vous parlez.");
}

#[test]
fn tu_and_vous_formal_take_the_same_verb_agreement_as_vous_plural() {
    // The pair this crate exists to exercise: `tu` (informal "you") takes second-person-
    // singular agreement, `vous` (formal "you") takes the *same word and the same conjugation
    // row* as plural `vous` -- not merely the same person-slot the way Spanish `usted` borrows
    // third-singular (a distinct word from `ustedes`), or German `Sie` borrows third-plural (a
    // distinct word from `sie`).
    assert_eq!(say!("{=0 parler}.", FrenchPerson::TU), "Tu parles.");
    assert_eq!(
        say!("{=0 parler}.", FrenchPerson::VOUS_FORMAL),
        "Vous parlez."
    );
    assert_eq!(say!("{=0 parler}.", FrenchPerson::VOUS), "Vous parlez.");
}

#[test]
fn irregular_etre_and_avoir_across_persons() {
    assert_eq!(
        say!("{=0 être} content.", FrenchPerson::JE),
        "Je suis content."
    );
    assert_eq!(
        say!("{the *=0 avoir} faim.", FrenchNoun::chat()),
        "Le chat a faim."
    );
}

#[test]
fn an_unknown_verb_falls_through_to_english_rather_than_being_guessed() {
    assert_eq!(
        say!("{the *=0 courir}.", FrenchNoun::chat()),
        "Le chat courir."
    );
}

// -------------------------------------------------------------- adjectives --

#[test]
fn postnominal_adjective_agreement_in_a_real_sentence() {
    assert_eq!(say!("{the *=0 !noir}", FrenchNoun::chat()), "Le chat noir");
    assert_eq!(
        say!("{the *=0 !intelligent}", FrenchNoun::maison()),
        "La maison intelligente"
    );
    assert_eq!(
        say!("{the +*=0 !noir}", FrenchNoun::chat()),
        "Les chats noirs"
    );
}

#[test]
fn an_invariant_postnominal_adjective_does_not_change_for_gender() {
    assert_eq!(
        say!("{the *=0 !rouge}", FrenchNoun::chat()),
        "Le chat rouge"
    );
    assert_eq!(
        say!("{the *=0 !rouge}", FrenchNoun::maison()),
        "La maison rouge"
    );
}

#[test]
fn an_unknown_adjective_falls_through_to_the_english_degree_table() {
    // Same as `ranting_es`'s equivalent test: `!` bakes the *comparative* English form at
    // compile time when the hook declines, not the bare word — "good" -> "better", the
    // irregular table's entry, not "gooder".
    assert_eq!(
        say!("{the *=0 !good}", FrenchNoun::chat()),
        "Le chat better"
    );
}

// -------------------------------------------------------------- pronouns --

#[test]
fn subject_pronouns_by_gender_and_number() {
    assert_eq!(say!("{=0}", FrenchNoun::chat()), "Il");
    assert_eq!(say!("{=0}", FrenchNoun::maison()), "Elle");
    assert_eq!(say!("{=0}", FrenchNoun::chat().plural()), "Ils");
    assert_eq!(say!("{=0}", FrenchNoun::maison().plural()), "Elles");
}

// -------------------------------------------------------------- numerals --

#[test]
fn cardinal_numerals_agree_at_one() {
    assert_eq!(say!("{#0 1}", 1, FrenchNoun::chat()), "Un chat");
    assert_eq!(say!("{#0 1}", 1, FrenchNoun::maison()), "Une maison");
    assert_eq!(say!("{#0 1}", 5, FrenchNoun::chat()), "Cinq chats");
}

#[test]
fn vigesimal_numerals_are_genuinely_irregular_not_just_different_words() {
    assert_eq!(say!("{#0 1}", 70, FrenchNoun::chat()), "Soixante-dix chats");
    assert_eq!(
        say!("{#0 1}", 80, FrenchNoun::chat()),
        "Quatre-vingts chats"
    );
    assert_eq!(
        say!("{#0 1}", 90, FrenchNoun::chat()),
        "Quatre-vingt-dix chats"
    );
}

#[test]
fn a_numeral_outside_the_closed_set_falls_through_to_english() {
    assert_eq!(say!("{#0 1}", 42, FrenchNoun::chat()), "Fortytwo chats");
}

#[test]
fn ordinals_agree_in_gender_only_at_one() {
    assert_eq!(say!("{##0 1}", 1, FrenchNoun::chat()), "Premier chat");
    assert_eq!(say!("{##0 1}", 1, FrenchNoun::maison()), "Première maison");
    assert_eq!(say!("{##0 1}", 2, FrenchNoun::maison()), "Deuxième maison");
}

// -------------------------------------------------- preposition fusion --

#[test]
fn de_le_and_a_le_fuse_to_du_and_au() {
    assert_eq!(
        say!("Je viens de {the *=0}.", FrenchNoun::chat()),
        "Je viens du chat."
    );
    assert_eq!(
        say!("Je vais à {the *=0}.", FrenchNoun::chat()),
        "Je vais au chat."
    );
    // Feminine "la" doesn't fuse -- confirms this isn't a blanket rewrite of every fused word.
    assert_eq!(
        say!("Je viens de {the *=0}.", FrenchNoun::maison()),
        "Je viens de la maison."
    );
}

//! What German *does* reach through `ranting`'s public hooks.
//!
//! Every assertion here is real German. The things that come out wrong — or cannot be written at
//! all — are in `holes.rs`, asserted exactly as the crate renders them.

use ranting::say;
use ranting_i18n::{Case, Definiteness, GermanNoun, GermanPerson};

// ---------------------------------------------------------------- articles --

#[test]
fn definite_article_by_gender_nominative() {
    assert_eq!(
        say!("{the =0 bellen}.", GermanNoun::hund()),
        "Der Hund bellt."
    );
    assert_eq!(
        say!("{the =0 schlafen}.", GermanNoun::katze()),
        "Die Katze schläft."
    );
    assert_eq!(
        say!("{the =0 sein} alt.", GermanNoun::haus()),
        "Das Haus ist alt."
    );
}

#[test]
fn definite_article_accusative_from_the_case_marker() {
    // `@` is the only case distinction the placeholder grammar can express; it reaches
    // accusative, and only for masculine is the German form visibly different.
    assert_eq!(
        say!("Ich sehe {the @0}.", GermanNoun::hund()),
        "Ich sehe den Hund."
    );
    assert_eq!(
        say!("Ich sehe {the @0}.", GermanNoun::katze()),
        "Ich sehe die Katze."
    );
    assert_eq!(
        say!("Ich sehe {the @0}.", GermanNoun::haus()),
        "Ich sehe das Haus."
    );
}

#[test]
fn definite_article_dative_and_genitive_from_the_entity() {
    // Not from the marker: `GrammaticalCase` has no dative. See README hole 3.
    assert_eq!(
        say!(
            "Ich gebe {the =0} etwas.",
            GermanNoun::hund().in_case(Case::Dative)
        ),
        "Ich gebe dem Hund etwas."
    );
    assert_eq!(
        say!(
            "Ich gebe {the =0} etwas.",
            GermanNoun::katze().in_case(Case::Dative)
        ),
        "Ich gebe der Katze etwas."
    );
    assert_eq!(
        say!(
            "Das Dach {the 0}.",
            GermanNoun::haus().in_case(Case::Genitive)
        ),
        "Das Dach des Hauses."
    );
}

#[test]
fn definite_article_plural_is_die_for_every_gender() {
    assert_eq!(
        say!("{the +=0 bellen}.", GermanNoun::hund()),
        "Die Hunde bellen."
    );
    assert_eq!(
        say!("{the +=0 schlafen}.", GermanNoun::katze()),
        "Die Katzen schlafen."
    );
    assert_eq!(
        say!("{the +=0 sein} alt.", GermanNoun::haus()),
        "Die Häuser sind alt."
    );
}

#[test]
fn dative_plural_takes_the_n_ending_on_the_noun_itself() {
    // "den Hunden", "den Häusern" — a case-driven change to the *noun*, not the article.
    assert_eq!(
        say!(
            "Ich gebe {the +0} etwas.",
            GermanNoun::hund().in_case(Case::Dative)
        ),
        "Ich gebe den Hunden etwas."
    );
    assert_eq!(
        say!(
            "Ich gebe {the +0} etwas.",
            GermanNoun::haus().in_case(Case::Dative)
        ),
        "Ich gebe den Häusern etwas."
    );
}

#[test]
fn indefinite_article_by_gender_and_case() {
    assert_eq!(
        say!("{a =0 bellen}.", GermanNoun::hund()),
        "Ein Hund bellt."
    );
    assert_eq!(
        say!("{a =0 schlafen}.", GermanNoun::katze()),
        "Eine Katze schläft."
    );
    assert_eq!(
        say!("{a =0 sein} alt.", GermanNoun::haus()),
        "Ein Haus ist alt."
    );
    assert_eq!(
        say!("Ich sehe {a @0}.", GermanNoun::hund()),
        "Ich sehe einen Hund."
    );
    assert_eq!(
        say!(
            "Ich gebe {a =0} etwas.",
            GermanNoun::hund().in_case(Case::Dative)
        ),
        "Ich gebe einem Hund etwas."
    );
}

// ------------------------------------------------------------------- verbs --

#[test]
fn verb_agreement_across_all_six_persons() {
    assert_eq!(
        say!("{=0 sehen} den Hund.", GermanPerson::ICH),
        "Ich sehe den Hund."
    );
    assert_eq!(
        say!("{=0 sehen} den Hund.", GermanPerson::DU),
        "Du siehst den Hund."
    );
    assert_eq!(
        say!("{the =0 sehen} den Hund.", GermanNoun::katze()),
        "Die Katze sieht den Hund."
    );
    assert_eq!(
        say!("{=0 sehen} den Hund.", GermanPerson::WIR),
        "Wir sehen den Hund."
    );
    assert_eq!(
        say!("{=0 sehen} den Hund.", GermanPerson::IHR),
        "Ihr seht den Hund."
    );
    assert_eq!(
        say!("{=0 sehen} den Hund.", GermanPerson::SIE),
        "Sie sehen den Hund."
    );
}

#[test]
fn irregular_and_stem_changing_verbs() {
    assert_eq!(say!("{=0 sein} alt.", GermanPerson::ICH), "Ich bin alt.");
    assert_eq!(say!("{=0 sein} alt.", GermanPerson::DU), "Du bist alt.");
    assert_eq!(say!("{=0 sein} alt.", GermanPerson::WIR), "Wir sind alt.");
    assert_eq!(
        say!("{the =0 schlafen}.", GermanNoun::hund()),
        "Der Hund schläft."
    );
    assert_eq!(
        say!("{the +=0 schlafen}.", GermanNoun::hund()),
        "Die Hunde schlafen."
    );
}

#[test]
fn an_unknown_verb_falls_through_to_english_rather_than_being_guessed() {
    // A partial lexicon should decline, not invent. `inflect_verb_custom` returns None and
    // `ranting`'s English rules render the word — as "walk", not "walks", because the declared
    // subject "er" is not an English pronoun and so falls to the catch-all arm of
    // `english::inflect_verb`'s match, which emits the bare form. That silent degradation is the
    // documented cost of `subjective()` being an uninterpreted channel (ROADMAP "SubjectPronoun
    // is a closed English enum"); it is only ever visible for a word the fork's hook declined.
    assert_eq!(say!("{the =0 walk}.", GermanNoun::hund()), "Der Hund walk.");
}

// -------------------------------------------------------------- adjectives --

#[test]
fn weak_declension_after_a_definite_article() {
    // der kleine Hund / den kleinen Hund / dem kleinen Hund — the endings are right even though
    // the position is not (see holes.rs).
    let hund = GermanNoun::hund();
    assert!(say!("{the =0 !klein}", hund).ends_with("kleine"));
    assert!(say!("{the @0 !klein}", hund).ends_with("kleinen"));
    assert!(say!("{the =0 !klein}", hund.in_case(Case::Dative)).ends_with("kleinen"));
    assert!(say!("{the +=0 !klein}", hund).ends_with("kleinen"));
}

#[test]
fn mixed_declension_after_an_indefinite_article() {
    // ein kleiner Hund / ein kleines Haus / eine kleine Katze — the mixed endings, which differ
    // from the weak ones exactly where `ein` itself carries no ending.
    let indef = Definiteness::Indefinite;
    assert!(say!("{a =0 !klein}", GermanNoun::hund().with_article(indef)).ends_with("kleiner"));
    assert!(say!("{a =0 !klein}", GermanNoun::haus().with_article(indef)).ends_with("kleines"));
    assert!(say!("{a =0 !klein}", GermanNoun::katze().with_article(indef)).ends_with("kleine"));
    assert!(say!("{a @0 !klein}", GermanNoun::hund().with_article(indef)).ends_with("kleinen"));
}

#[test]
fn strong_declension_with_no_article() {
    let bare = Definiteness::Bare;
    assert!(say!("{=0 !klein}", GermanNoun::hund().with_article(bare)).ends_with("kleiner"));
    assert!(say!("{=0 !klein}", GermanNoun::haus().with_article(bare)).ends_with("kleines"));
    assert!(
        say!(
            "{=0 !klein}",
            GermanNoun::hund().with_article(bare).in_case(Case::Dative)
        )
        .ends_with("kleinem")
    );
}

#[test]
fn an_unknown_adjective_falls_through_to_the_english_degree_table() {
    // Same contract as the verb hook: outside the closed vocabulary the lexicon declines, and
    // `ranting`'s compile-time comparative is emitted instead of an invented German ending.
    assert_eq!(
        say!("{the =0 !good}", GermanNoun::hund()),
        "Der Hund better"
    );
}

// ---------------------------------------------------------------- numerals --

#[test]
fn spelled_numerals_agree_like_an_article_at_one() {
    let one = 1;
    let two = 2;
    let zwolf = 12;
    assert_eq!(say!("{#0 1}", one, GermanNoun::hund()), "ein Hund");
    assert_eq!(say!("{#0 1}", one, GermanNoun::katze()), "eine Katze");
    assert_eq!(say!("{#0 1}", two, GermanNoun::hund()), "zwei Hunde");
    assert_eq!(say!("{#0 1}", zwolf, GermanNoun::haus()), "zwölf Häuser");
}

#[test]
fn a_numeral_outside_the_closed_range_falls_through_to_english() {
    let many = 40;
    assert_eq!(say!("{#0 1}", many, GermanNoun::hund()), "forty Hunde");
}

#[test]
fn digit_numerals_are_left_alone_because_german_writes_the_same_digits() {
    let n = 3;
    assert_eq!(say!("{$0 1}", n, GermanNoun::haus()), "3 Häuser");
}

// ----------------------------------------------------------- orthography ---

#[test]
fn nouns_stay_capitalized_mid_sentence() {
    assert_eq!(
        say!(
            "Ich sehe {the @0} und {the @1}.",
            GermanNoun::hund(),
            GermanNoun::katze()
        ),
        "Ich sehe den Hund und die Katze."
    );
}

#[test]
fn a_lowercase_marker_does_not_lowercase_a_german_noun() {
    // `,` forces lowercase on the placeholder; the article obeys, the noun does not — which is
    // what `capitalize(_, OrthographyRole::Noun, _)` is for.
    assert_eq!(say!("{,the =0}", GermanNoun::hund()), "der Hund");
}

// ------------------------------------------------------------- pronouns ----

#[test]
fn real_pronouns_when_the_entity_asks_for_them() {
    let hund = GermanNoun::hund().as_pronoun();
    assert_eq!(say!("{=0 bellen}.", hund), "Er bellt.");
    assert_eq!(say!("Ich sehe {@0}.", hund), "Ich sehe ihn.");
    assert_eq!(
        say!("Ich gebe {=0} etwas.", hund.in_case(Case::Dative)),
        "Ich gebe ihm etwas."
    );
    assert_eq!(
        say!("{=0 bellen}.", GermanNoun::katze().as_pronoun()),
        "Sie bellt."
    );
    assert_eq!(
        say!("{=0 sein} alt.", GermanNoun::haus().as_pronoun()),
        "Es ist alt."
    );
}

#[test]
fn person_pronouns_decline_too() {
    assert_eq!(say!("Er sieht {@0}.", GermanPerson::ICH), "Er sieht mich.");
    assert_eq!(say!("Er sieht {@0}.", GermanPerson::IHR), "Er sieht euch.");
    assert_eq!(
        say!("Das ist {`0} Hund.", GermanPerson::DU),
        "Das ist dein Hund."
    );
    assert_eq!(
        say!("{=0 sehen} {%0}.", GermanPerson::WIR),
        "Wir sehen uns."
    );
}

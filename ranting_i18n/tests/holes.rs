//! The falsification half of ROADMAP.md Phase 6 item 10: what German cannot get through
//! `ranting`'s public API.
//!
//! Each test asserts what the crate *actually* produces, not what German needs, and is named
//! after the numbered hole in this crate's README.md. They are pins: if a later change closes one
//! of these, the test fails and the hole gets struck from the README rather than quietly rotting.

use ranting::{Ranting, say};
use ranting_i18n::{Case, Definiteness, GermanNoun, GermanPerson};

// ------------------------------------------------------- hole 1: say_with! --

// `say_with!()` and `#[derive_ranting]` are not re-exported from `ranting` (only `say`, `ack`,
// `nay`, `heed`, `Heed`, `ask`, `boxed_ranting_trait` and `ref_ranting_trait` are), so a crate
// depending on `ranting` alone cannot write either. There is no test to write for a macro that
// does not resolve: the evidence is that this crate's Cargo.toml has no `ranting_derive`
// dependency and every hook it overrides is the non-`_with_context` one. The consequence is
// asserted below instead.

#[test]
fn hole_1_the_with_context_hooks_are_unreachable_so_dialect_never_arrives() {
    // `NarrationContext` is public and its `dialect`/`register` fields are the documented home
    // for a locale, but the only macro that can deliver one is `say_with!()`. Overriding a
    // `_with_context` hook is therefore pointless in this crate: `say!()` always passes `None`.
    struct DialectProbe(GermanNoun);
    impl std::fmt::Display for DialectProbe {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }
    impl Ranting for DialectProbe {
        fn name(&self, uc: bool) -> String {
            self.0.name(uc)
        }
        fn subjective(&self) -> &str {
            self.0.subjective()
        }
        fn is_plural(&self) -> bool {
            self.0.is_plural()
        }
        fn inflect(&self, to_plural: bool, uc: bool) -> String {
            self.0.inflect(to_plural, uc)
        }
        fn skip_article(&self) -> bool {
            self.0.skip_article()
        }
        fn inflect_pronoun_custom(
            &self,
            subject: &str,
            case: ranting::PronounCase,
            class: ranting::NounClass,
            as_plural: bool,
            uc: bool,
        ) -> Option<String> {
            self.0
                .inflect_pronoun_custom(subject, case, class, as_plural, uc)
        }
        fn inflect_article_custom_with_context(
            &self,
            _article: &str,
            _noun_singular: &str,
            _case: ranting::GrammaticalCase,
            _class: ranting::NounClass,
            _as_plural: bool,
            _uc: bool,
            ctx: Option<&ranting::NarrationContext>,
        ) -> Option<String> {
            // Records what arrived; there is no way from this crate to make it anything else.
            Some(match ctx.and_then(|c| c.dialect) {
                Some(d) => format!("<dialect={d}>"),
                None => "<no-context>".to_string(),
            })
        }
    }

    assert_eq!(
        say!("{the =0}", DialectProbe(GermanNoun::hund())),
        "<no-context> Hund"
    );
}

// -------------------------------------------- hole 2: inflect() has no case --

#[test]
fn hole_2_the_noun_form_cannot_follow_the_placeholders_case_marker() {
    // `Ranting::inflect(to_plural, uc)` takes number but not case, so the *noun's own* declension
    // has to be carried on the entity. Here the same entity is rendered under two different case
    // markers and produces the same dative-plural form both ways — the marker does not reach
    // `inflect`, only the article hook.
    let dative = GermanNoun::hund().in_case(Case::Dative).plural();
    assert_eq!(say!("{the 0}", dative), "Den Hunden");
    let nominative = GermanNoun::hund().plural();
    // Correct German for a dative here would still be "Hunden"; the marker cannot say so.
    assert_eq!(
        say!("Ich gebe {the @0} etwas.", nominative),
        "Ich gebe die Hunde etwas."
    );
}

// ------------------------------------------------------- hole 3: no dative --

#[test]
fn hole_3_grammatical_case_cannot_express_dative_so_the_marker_is_ignored() {
    // Once the entity carries the case (the only way to reach dative), `GrammaticalCase` becomes
    // ignorable: `=` and `@` produce identical output. That is the precise sense in which the
    // v1.3 `GrammaticalCase` addition does not, on its own, close the German article gap — five
    // markers collapse onto German's four cases, with `@` meaning accusative-or-dative.
    let dativ = GermanNoun::hund().in_case(Case::Dative);
    assert_eq!(say!("{the =0}", dativ), "Dem Hund");
    assert_eq!(say!("{the @0}", dativ), "Dem Hund");
}

// --------------------------------------- holes 4a/4b: adjective position ----

#[test]
fn hole_4a_an_attributive_adjective_can_only_follow_the_noun() {
    // German is "der kleine Hund". The `!` slot is post-noun only (`PostSpec::Degree`), and
    // Phase 6 item 1 settled that `ranting` will not move text it does not own — so the ending is
    // right and the position is wrong, in every template.
    assert_eq!(
        say!("{the =0 !klein}", GermanNoun::hund()),
        "Der Hund kleine"
    );
    // The only way to get German word order is to write the adjective as literal template text,
    // where no hook can inflect it — i.e. a per-language template with a per-case wording.
    assert_eq!(
        say!("{the ?0} kleine {=0}", GermanNoun::hund()),
        "Der kleine Hund"
    );
}

#[test]
fn hole_4b_declension_class_is_not_reported_so_it_must_be_carried_on_the_entity() {
    // Weak after "der", mixed after "ein" — but `inflect_adjective_custom` receives case, class
    // and number, never the article that was rendered, and `self` cannot know it either since the
    // article is template text. Without `with_article` the lexicon has to guess, and guesses
    // "definite": the mixed ending is unreachable from the placeholder alone.
    let guessed = say!("{a =0 !klein}", GermanNoun::hund());
    assert_eq!(guessed, "Ein Hund kleine"); // German wants "kleiner"
    let told = say!(
        "{a =0 !klein}",
        GermanNoun::hund().with_article(Definiteness::Indefinite)
    );
    assert_eq!(told, "Ein Hund kleiner");
}

// ------------------------------------------ hole 5: pronoun/name collision --

#[test]
fn hole_5_one_hook_serves_both_case_marking_and_pronoun_display() {
    // A case marker does two jobs at once: it tells `inflect_article_custom` the role *and*
    // switches the noun slot to a pronoun. A fork that wants "Der Hund bellt" must therefore make
    // `inflect_pronoun_custom` return the name — and then it returns the name everywhere, so
    // genuine pronouns are unreachable for that same entity.
    let named = GermanNoun::hund();
    assert_eq!(say!("Ich sehe {@0}.", named), "Ich sehe Hund."); // wanted: "ihn"
    // Reachable only by carrying the choice on the entity, which is state the placeholder should
    // have been able to express.
    assert_eq!(say!("Ich sehe {@0}.", named.as_pronoun()), "Ich sehe ihn.");
}

// ------------------------------------------ hole 6: no zero-article signal --

#[test]
fn hole_6_a_missing_article_leaves_a_separator_no_hook_can_remove() {
    // German has no indefinite plural article ("Hunde bellen"), and the only way
    // `inflect_article_custom` can say so is to return `""`. The separator is emitted anyway, so
    // the placeholder renders with a leading space — visible as a doubled space mid-sentence.
    assert_eq!(
        say!("{a +=0 bellen}.", GermanNoun::hund()),
        " Hunde bellen."
    );
    assert_eq!(
        say!("Dort {a +=0 bellen}.", GermanNoun::hund()),
        "Dort  Hunde bellen."
    );

    // `elide_article_custom` cannot repair it: the post-assembly splice is skipped when the
    // recorded article span is empty, so the hook is never called for a zero-length article.
    // This one is checked with a probe that would panic if it were reached.
    struct ElisionProbe(GermanNoun);
    impl std::fmt::Display for ElisionProbe {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }
    impl Ranting for ElisionProbe {
        fn name(&self, uc: bool) -> String {
            self.0.name(uc)
        }
        fn subjective(&self) -> &str {
            self.0.subjective()
        }
        fn is_plural(&self) -> bool {
            self.0.is_plural()
        }
        fn inflect(&self, to_plural: bool, uc: bool) -> String {
            self.0.inflect(to_plural, uc)
        }
        fn skip_article(&self) -> bool {
            self.0.skip_article()
        }
        fn noun_class(&self) -> ranting::NounClass {
            self.0.noun_class()
        }
        fn inflect_article_custom(
            &self,
            article: &str,
            noun_singular: &str,
            case: ranting::GrammaticalCase,
            class: ranting::NounClass,
            as_plural: bool,
            uc: bool,
        ) -> Option<String> {
            self.0
                .inflect_article_custom(article, noun_singular, case, class, as_plural, uc)
        }
        fn inflect_pronoun_custom(
            &self,
            subject: &str,
            case: ranting::PronounCase,
            class: ranting::NounClass,
            as_plural: bool,
            uc: bool,
        ) -> Option<String> {
            self.0
                .inflect_pronoun_custom(subject, case, class, as_plural, uc)
        }
        fn elide_article_custom(
            &self,
            _article: &str,
            _separator: &str,
            following: &str,
            _case: ranting::GrammaticalCase,
            _class: ranting::NounClass,
            _as_plural: bool,
        ) -> Option<String> {
            // Would drop the stray separator — if it ran.
            Some(following.to_string())
        }
    }
    assert_eq!(say!("{a +=0}", ElisionProbe(GermanNoun::hund())), " Hunde");

    // `Ranting::skip_article` does suppress it, but it is per-entity and unconditional: it
    // cannot mean "no article in the plural only", and it would also swallow `der`/`die`/`das`.
    assert_eq!(say!("{the +=0}", GermanNoun::hund()), "Die Hunde");
}

// ------------------------------- hole 7: preposition-article fusion + slot --

#[test]
fn hole_7_the_pre_noun_slot_is_a_closed_english_word_list() {
    // German needs "im Haus" (in + dem). `elide_article_custom` runs after assembly and could
    // fuse them — but only if the preposition were inside the placeholder, and the pre-noun slot
    // accepts an article or one of `ranting`'s hard-coded English modal words, nothing else.
    // `say!("{in the =0}", haus)` is a *compile* error ("expected article or verb"), so the
    // preposition can only be literal template text, outside every hook's reach.
    let haus = GermanNoun::haus().in_case(Case::Dative);
    assert_eq!(say!("in {the =0}", haus), "in dem Haus"); // German wants "im Haus"
}

// ----------------------------------------------------------- hole 8: order --

#[test]
fn hole_8_word_order_lives_in_this_crates_own_templates() {
    // Settled permanently by Phase 6 item 1: `ranting` inflects within a template and never
    // reorders. German verb-second is reachable only because the caller writes the German
    // template; the English one would need different literal text, not a different hook.
    assert_eq!(
        say!("Heute {the =0 schlafen}.", GermanNoun::katze()),
        "Heute die Katze schläft." // German V2 wants "Heute schläft die Katze."
    );
    // Written as a German template instead, with the order in the literal text:
    assert_eq!(
        say!("Heute {?0 schlafen} {the =0}.", GermanNoun::katze()),
        "Heute schläft die Katze."
    );
}

#[test]
fn hole_8b_a_verb_split_across_two_positions_is_not_expressible_at_all() {
    // "Der Hund macht die Tür auf" — one verb, two positions. A placeholder cannot carry both a
    // pre-noun and a post-noun verb (`handle_placeholder_impl` asserts it), and no hook can emit
    // text at a position it does not own. The separable prefix must be literal template text.
    assert_eq!(
        say!("{=0 sehen} den Hund auf.", GermanPerson::WIR),
        "Wir sehen den Hund auf."
    );
}

//! `MandarinNoun` — the `Ranting` implementation, and the only file besides
//! [`crate::person`] that touches the trait seam.
//!
//! Two of the eight hook pairs are live here: the verb (invariant-word substitution) and the
//! numeral (classifiers) — the same count `ranting_ja` lands on, for largely the same reason:
//! Mandarin, like Japanese, inflects almost nothing English does. The other six are deliberately
//! not overridden — see the README, where that is recorded as further confirmation of an
//! already-established finding, not a new one. What *is* new about this crate lives entirely in
//! `tests/holes.rs`: the tense-marker pipeline itself, not any one hook.

use crate::lexicon::{self, NounEntry};
use ranting::*;
use std::fmt;

/// One Mandarin noun from the closed vocabulary.
///
/// It carries no number state and no class/gender field at all — matching `ranting_ja`'s
/// `JapaneseNoun`, not `ranting_es`/`ranting_ar`/`ranting_fr`'s gendered nouns. Mandarin common
/// nouns do not inflect for number; see `inflect` below and [`crate::person::MandarinPerson`]
/// for the one place plural marking (们) does apply.
#[derive(Copy, Clone)]
pub struct MandarinNoun {
    entry: &'static NounEntry,
}

impl MandarinNoun {
    const fn new(entry: &'static NounEntry) -> Self {
        MandarinNoun { entry }
    }

    /// 猫 (māo) "cat" — counted with 只.
    pub const fn mao() -> Self {
        Self::new(&lexicon::MAO)
    }

    /// 书 (shū) "book" — counted with 本.
    pub const fn shu() -> Self {
        Self::new(&lexicon::SHU)
    }

    /// 人 (rén) "person" — counted with 个.
    pub const fn ren() -> Self {
        Self::new(&lexicon::REN)
    }
}

impl fmt::Display for MandarinNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.entry.word)
    }
}

impl Ranting for MandarinNoun {
    fn name(&self, _uc: bool) -> String {
        // `uc` is ignored: Chinese script is caseless. `capitalize` is not overridden for the
        // same reason — already pre-documented in `docs/EXTENSIBILITY.md` §2.6 for Chinese
        // specifically, before this crate existed.
        self.entry.word.to_string()
    }

    fn subjective(&self) -> &str {
        // An uninterpreted channel, same discipline every fork in this repo uses — the actual
        // word choice would live in `inflect_pronoun_custom`, which this crate doesn't override
        // (see the README: Mandarin pronouns are ordinary closed-class words with no agreement
        // to speak of, so there is nothing for the hook to compute that a template can't just
        // write directly).
        "它"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(
        &self,
        _to_plural: bool,
        _uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        // An identity function, matching `ranting_ja::JapaneseNoun::inflect` exactly (not a new
        // finding — see the README). Mandarin common nouns do not inflect for number, case or
        // definiteness at all; every parameter here, including the count channel Arabic needed,
        // is inert for this language.
        self.entry.word.to_string()
    }

    fn skip_article(&self) -> bool {
        // Mandarin has no articles at all.
        true
    }

    fn inflect_verb_custom_with_context(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
        _ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        // This is the hook the crate's headline finding turns on: it substitutes the correct,
        // invariant Mandarin verb for whatever English spelling `ranting`'s compile-time tense
        // baking hands it (see `lexicon::VERBS`'s doc comment) — proving the hook itself works
        // perfectly. What happens to its return value next is the hole; see
        // `tests/holes.rs::hole_1_*`. `subject`/`as_plural`/`count` are all ignored: Mandarin
        // verbs never agree with anything.
        lexicon::verb_form(verb).map(str::to_string)
    }

    fn inflect_numeral_custom(
        &self,
        _numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        // Not a new finding: the same shape `ranting_ja`'s counters use — a classifier read off
        // `self.entry`, not off `class`, because which classifier a noun takes is a property of
        // the noun, not of the placeholder. Implemented here for a complete, correct showcase,
        // exactly the role `ranting_fr` gave already-closed hooks like preposition fusion.
        match style {
            NumeralStyle::Words | NumeralStyle::Digits => {
                lexicon::counted(count?, self.entry.classifier)
            }
            // Mandarin ordinals (第n) are a different construction, not modeled by this closed
            // lexicon — falls through to English, the same honest degradation `ranting_ja`'s
            // counters use for the same reason.
            NumeralStyle::Ordinal | NumeralStyle::OrdinalDigits => None,
        }
    }

    fn elide_numeral_custom(
        &self,
        numeral: &str,
        _separator: &str,
        following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
    ) -> Option<String> {
        // 一只猫 is written as one run with no space anywhere — the same post-assembly splice
        // `ranting_ja` needed for 一匹の猫, minus the attributive particle Japanese inserts.
        Some(format!("{numeral}{following}"))
    }

    // Not overridden, deliberately, and recorded in the README as confirmation of an
    // already-established finding rather than as new holes:
    //
    // - `noun_class` — Mandarin has no grammatical gender or noun class at all, matching
    //   `ranting_ja`, not the four gendered forks.
    // - `inflect_article_custom` / `elide_article_custom` — no articles.
    // - `inflect_pronoun_custom` — Mandarin pronouns are ordinary closed-class words with no
    //   agreement; a template that needs one writes it (`subjective()` is a placeholder value
    //   like `ranting_ja`'s "それ").
    // - `inflect_adjective_custom` — Mandarin's attributive words are prenominal stative verbs,
    //   a *third* restatement of `ranting_i18n`'s hole 4a (German, then `ranting_fr`'s lexically
    //   split version); no new signal here, so this crate declines to implement it at all, the
    //   same call `ranting_ar` made for a postnominal proof it didn't need either.
    // - `inflect_preposition_custom` — Mandarin coverbs behave more like Japanese's
    //   postpositional particles than prepositions, and are template literal text under the
    //   word-order boundary either way.
    // - `capitalize` — the script is caseless.
    // - `is_first_person_subject_custom` — nothing in this vocabulary declares first person.
}

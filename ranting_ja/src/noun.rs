//! `JapaneseNoun` — the `Ranting` implementation, and the only file that touches the trait seam.
//!
//! Two of the eight hook pairs are live here: the verb (register-driven politeness) and the
//! numeral (classifiers). The other six are deliberately not overridden — see the README, where
//! that is recorded as a *finding* rather than as holes.

use crate::lexicon::{self, NounEntry, Politeness};
use ranting::*;
use std::fmt;

/// One Japanese noun from the closed vocabulary.
///
/// It carries no number state at all: Japanese nouns do not inflect for number, so there is
/// nothing for `plural` to select between.
#[derive(Copy, Clone)]
pub struct JapaneseNoun {
    entry: &'static NounEntry,
}

impl JapaneseNoun {
    const fn new(entry: &'static NounEntry) -> Self {
        JapaneseNoun { entry }
    }

    /// 猫 "cat" — counted with 匹.
    pub const fn neko() -> Self {
        Self::new(&lexicon::NEKO)
    }

    /// 人 "person" — counted with 人.
    pub const fn hito() -> Self {
        Self::new(&lexicon::HITO)
    }

    /// 本 "book" — counted with 本.
    pub const fn hon() -> Self {
        Self::new(&lexicon::HON)
    }

    /// 先生 "teacher" — honored, so verbs about them take sonkeigo forms when formal.
    pub const fn sensei() -> Self {
        Self::new(&lexicon::SENSEI)
    }

    /// Map the crate's three-value [`Register`] onto this lexicon's two-value politeness.
    ///
    /// `None` — no context, or `say!()` — is [`Politeness::Plain`], which is what makes
    /// `say_with!()` with a default context byte-identical to `say!()`.
    fn politeness(ctx: Option<&NarrationContext>) -> Politeness {
        match ctx.and_then(|c| c.register) {
            Some(Register::Formal) => Politeness::Polite,
            // `Neutral` and `Casual` both map to the plain form: Japanese's politeness axis has
            // more than three levels, but the *cut* this lexicon needs falls between formal and
            // everything else. See the README on why three values is not the limitation it looks
            // like.
            Some(Register::Neutral) | Some(Register::Casual) | None => Politeness::Plain,
        }
    }
}

impl fmt::Display for JapaneseNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.entry.word)
    }
}

impl Ranting for JapaneseNoun {
    fn name(&self, _uc: bool) -> String {
        // `uc` is ignored: the Japanese scripts are caseless. `capitalize` is not overridden for
        // the same reason.
        self.entry.word.to_string()
    }

    fn subjective(&self) -> &str {
        // Japanese is pro-drop and its "pronouns" are ordinary nouns; this is an uninterpreted
        // channel, as in all three other forks.
        "それ"
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
        // An identity function, and that is the whole story: Japanese nouns do not inflect for
        // number, case or definiteness. Every parameter is ignored — including Phase 7 item 11's
        // `count`, which Arabic needed and this language has no use for. A hook surface sized for
        // maximally-inflected languages degrading to nothing here is the intended shape.
        self.entry.word.to_string()
    }

    fn skip_article(&self) -> bool {
        // Japanese has no articles at all.
        true
    }

    fn inflect_verb_custom_with_context(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        // `NarrationContext::register`'s first real consumer in this repo. Two things it proves,
        // both of which the Phase 7 item 3 spike scoped and the item 4 decision bought this crate
        // for:
        //
        // 1. Politeness rides `register`, with **no pronoun anywhere in the template** and no
        //    entity state involved — unlike German/Spanish T-V politeness, which is a pronoun
        //    slot and so rides the addressee's declared subject label instead.
        // 2. Sonkeigo (honorific *substitution*, 食べる → 召し上がる) needs no additional signal:
        //    it is a lookup keyed by verb, register and `self`, and this hook has all three.
        //
        // Only the `_with_context` twin is overridden, which is the documented sufficient shape —
        // the non-context hook defaults to delegating here.
        lexicon::conjugate(verb, Self::politeness(ctx), self.entry.honorific).map(str::to_string)
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
        // The counter is read off `self.entry`, **not** off the `class` parameter. That answers
        // the item 3 spike's stated question — "can `NounClass` carry a classifier?" — by
        // dissolving it: which counter a noun takes is a property of that noun, so the hook's
        // `&self` is already enough and `NounClass` never has to stretch. It stays `UNSET` here.
        let n = count?;
        let counted = self.entry.counter.count(n)?;
        match style {
            // 一匹の猫 — the counter phrase, then the attributive particle の. The particle is
            // returned as part of the numeral because there is nowhere else to put it.
            NumeralStyle::Words => Some(format!("{counted}の")),
            // `$n` asks for digits; Japanese writes the same digits, but the *counter* is still
            // obligatory, so this channel gets the same treatment.
            NumeralStyle::Digits => Some(format!("{counted}の")),
        }
    }

    // Not overridden, deliberately, and recorded in the README as a finding rather than as holes:
    //
    // - `inflect_article_custom` / `elide_article_custom` — Japanese has no articles.
    // - `inflect_pronoun_custom` — pro-drop; its "pronouns" are ordinary nouns, and a template
    //   that needs one writes it.
    // - `inflect_adjective_custom` — i-adjectives conjugate for tense, negation and politeness,
    //   which the `!` slot's *degree* axis does not express; and they are prenominal, so the slot
    //   is in the wrong position anyway (`ranting_i18n`'s hole 4a, from a different direction).
    // - `inflect_preposition_custom` — Japanese has postpositional particles, not prepositions,
    //   and they are template literal text under the word-order boundary.
    // - `capitalize` — the scripts are caseless.
    // - `is_first_person_subject_custom` — nothing in this vocabulary declares first person.
}

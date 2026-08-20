//! `MandarinPerson` — a speaker/addressee entity, present so verb substitution and the 们-plural
//! can be shown across all persons rather than only the third, which is all a noun ever
//! exercises. Mirrors `ranting_fr::person`/`ranting_es::person`'s reason for existing, though the
//! grammar it demonstrates is much flatter: Mandarin pronouns don't decline for case at all.

use crate::lexicon;
use ranting::*;
use std::fmt;

/// 我 / 你 / 您 / 他 / 我们 / 你们 / 他们 — a personal pronoun as a `Ranting` entity.
///
/// Unlike every other fork's person entity, this one needs no separate objective/possessive
/// pronoun table beyond a fixed suffix: Mandarin pronouns are identical in subject and object
/// position (我 is both "I" and "me" — there is no case distinction to make at all, not even
/// the two- or three-way splits `ranting_es`/`ranting_fr`'s person entities carry), and the
/// possessive is formed by the regular particle 的 rather than a separate word
/// (我的 "my"/"mine"). See [`Ranting::inflect_pronoun_custom`] below.
#[derive(Copy, Clone)]
pub struct MandarinPerson {
    subject: &'static str,
    plural: bool,
}

impl MandarinPerson {
    /// 我 (wǒ) — first person.
    pub const WO: MandarinPerson = MandarinPerson {
        subject: "我",
        plural: false,
    };
    /// 你 (nǐ) — second person, plain.
    pub const NI: MandarinPerson = MandarinPerson {
        subject: "你",
        plural: false,
    };
    /// 您 (nín) — second person, formal. Singular only: standard Mandarin has **no** plural
    /// formal-you — unlike French `vous`, which covers both formal-singular and plain-plural
    /// with the identical word, 您们 is nonstandard and not modeled here.
    pub const NIN: MandarinPerson = MandarinPerson {
        subject: "您",
        plural: false,
    };
    /// 他 (tā) — third person. Spoken Mandarin does not distinguish gender here at all: 他/她/它
    /// ("he"/"she"/"it") are homophones, spelled differently only in writing — a fact about the
    /// script, not the grammar, so this lexicon picks one spelling rather than modeling a
    /// distinction the spoken language doesn't make.
    pub const TA: MandarinPerson = MandarinPerson {
        subject: "他",
        plural: false,
    };
    /// 我们 (wǒmen) — first person plural.
    pub const WOMEN: MandarinPerson = MandarinPerson {
        subject: "我们",
        plural: true,
    };
    /// 你们 (nǐmen) — second person plural.
    pub const NIMEN: MandarinPerson = MandarinPerson {
        subject: "你们",
        plural: true,
    };
    /// 他们 (tāmen) — third person plural.
    pub const TAMEN: MandarinPerson = MandarinPerson {
        subject: "他们",
        plural: true,
    };
}

impl fmt::Display for MandarinPerson {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.subject)
    }
}

impl Ranting for MandarinPerson {
    fn name(&self, _uc: bool) -> String {
        self.subject.to_string()
    }

    fn subjective(&self) -> &str {
        self.subject
    }

    fn is_plural(&self) -> bool {
        self.plural
    }

    fn inflect(
        &self,
        _to_plural: bool,
        _uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        self.subject.to_string()
    }

    fn skip_article(&self) -> bool {
        true
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        Some(match case {
            // No case distinction at all: subject and object position use the identical word.
            PronounCase::Subjective | PronounCase::Objective => self.subject.to_string(),
            PronounCase::PossessiveDeterminer | PronounCase::PossessivePronoun => {
                format!("{}的", self.subject)
            }
            PronounCase::Reflexive => "自己".to_string(),
        })
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
        // Same invariant-word substitution as `MandarinNoun` — see its doc comment and the
        // crate's headline finding in `tests/holes.rs`.
        lexicon::verb_form(verb).map(str::to_string)
    }
}

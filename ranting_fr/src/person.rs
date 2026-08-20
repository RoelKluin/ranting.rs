//! `FrenchPerson` — a speaker/addressee entity, present so that verb agreement (and
//! specifically `tu` vs `vous`) can be shown across all persons rather than only the third,
//! which is all a noun ever exercises. Mirrors `ranting_es::person`.

use crate::lexicon::{self, Person};
use ranting::*;
use std::fmt;

/// `je` / `tu` / `vous` (formal) / `nous` / `vous` (plural) — a personal pronoun as a `Ranting`
/// entity.
///
/// French has one fewer *distinct word* here than Spanish's six-way system: Spanish's informal
/// second-person plural `vosotros` has no French equivalent — `vous` alone covers formal-you and
/// plural-you, sharing not just the same conjugation slot (as Spanish `usted`/third-singular
/// does) but the identical written word. See [`VOUS_FORMAL`] and [`VOUS`], and the crate
/// README's "`tu` vs `vous`" section.
///
/// It carries no noun class: French personal pronouns take no article and no attributive
/// adjective, so there is nothing for a class to select — same as `ranting_es::SpanishPerson`.
#[derive(Copy, Clone)]
pub struct FrenchPerson {
    subject: &'static str,
    plural: bool,
}

impl FrenchPerson {
    /// `je` — first person singular.
    pub const JE: FrenchPerson = FrenchPerson {
        subject: "je",
        plural: false,
    };
    /// `tu` — second person singular, informal.
    pub const TU: FrenchPerson = FrenchPerson {
        subject: "tu",
        plural: false,
    };
    /// `vous` — second person singular, formal. Grammatically the same word and the same
    /// conjugation row as [`VOUS`] (plural) — not merely the same slot, unlike Spanish `usted`
    /// (a distinct word from `ustedes`) or German `Sie` (a distinct word from `sie`). See
    /// [`lexicon::Person::Vous`].
    pub const VOUS_FORMAL: FrenchPerson = FrenchPerson {
        subject: "vous",
        plural: false,
    };
    /// `nous` — first person plural.
    pub const NOUS: FrenchPerson = FrenchPerson {
        subject: "nous",
        plural: true,
    };
    /// `vous` — second person plural. Same word, same conjugation row as [`VOUS_FORMAL`] —
    /// `is_plural()` differs, the rendered form doesn't.
    pub const VOUS: FrenchPerson = FrenchPerson {
        subject: "vous",
        plural: true,
    };

    fn objective(&self) -> &'static str {
        match self.subject {
            "je" => "me",
            "tu" => "te",
            "nous" => "nous",
            _ => "vous", // vous (formal or plural)
        }
    }

    fn possessive(&self) -> &'static str {
        match self.subject {
            "je" => "mon",
            "tu" => "ton",
            "nous" => "notre",
            _ => "votre", // vous (formal or plural)
        }
    }

    fn reflexive(&self) -> &'static str {
        match self.subject {
            "je" => "me",
            "tu" => "te",
            "nous" => "nous",
            _ => "vous", // vous (formal or plural)
        }
    }
}

impl fmt::Display for FrenchPerson {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.subject)
    }
}

impl Ranting for FrenchPerson {
    fn name(&self, uc: bool) -> String {
        capitalize_if(self.subject, uc)
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
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        // Personal pronouns don't decline by case here — `subjective()`/`objective()`/etc.
        // already cover case via the pronoun hook, same as `ranting_es::SpanishPerson`.
        capitalize_if(self.subject, uc)
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
        uc: bool,
    ) -> Option<String> {
        let word = match case {
            PronounCase::Subjective => self.subject,
            PronounCase::Objective => self.objective(),
            PronounCase::PossessiveDeterminer | PronounCase::PossessivePronoun => self.possessive(),
            PronounCase::Reflexive => self.reflexive(),
        };
        Some(capitalize_if(word, uc))
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        let person = Person::from_subject(subject, as_plural);
        lexicon::conjugate(verb, person).map(|form| capitalize_if(form, uc))
    }
}

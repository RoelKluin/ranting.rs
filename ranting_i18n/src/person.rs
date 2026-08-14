//! `GermanPerson` — a speaker/addressee entity, present so that verb agreement can be shown
//! across all six persons rather than only the third, which is all a noun ever exercises.

use crate::lexicon::{self, Person};
use ranting::*;
use std::fmt;

/// `ich` / `du` / `wir` / `ihr` / `Sie` — a personal pronoun as a `Ranting` entity.
///
/// It carries no noun class: German personal pronouns take no article and no attributive
/// adjective, so there is nothing for a class to select.
#[derive(Copy, Clone)]
pub struct GermanPerson {
    subject: &'static str,
    plural: bool,
}

impl GermanPerson {
    /// `ich` — first person singular.
    pub const ICH: GermanPerson = GermanPerson {
        subject: "ich",
        plural: false,
    };
    /// `du` — second person singular, familiar.
    pub const DU: GermanPerson = GermanPerson {
        subject: "du",
        plural: false,
    };
    /// `wir` — first person plural.
    pub const WIR: GermanPerson = GermanPerson {
        subject: "wir",
        plural: true,
    };
    /// `ihr` — second person plural, familiar.
    pub const IHR: GermanPerson = GermanPerson {
        subject: "ihr",
        plural: true,
    };
    /// `Sie` — the formal address, which is a pronoun slot of its own rather than a register
    /// setting (see the ROADMAP's `SubjectPronoun` decision and
    /// `docs/superpowers/specs/2026-08-13-pronoun-inventory.md`).
    pub const SIE: GermanPerson = GermanPerson {
        subject: "Sie",
        plural: true,
    };

    fn objective(&self) -> &'static str {
        match self.subject {
            "ich" => "mich",
            "du" => "dich",
            "wir" => "uns",
            "ihr" => "euch",
            _ => "Sie",
        }
    }

    fn possessive(&self) -> &'static str {
        match self.subject {
            "ich" => "mein",
            "du" => "dein",
            "wir" => "unser",
            "ihr" => "euer",
            _ => "Ihr",
        }
    }

    fn reflexive(&self) -> &'static str {
        match self.subject {
            "ich" => "mich",
            "du" => "dich",
            "wir" => "uns",
            "ihr" => "euch",
            _ => "sich",
        }
    }
}

impl fmt::Display for GermanPerson {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.subject)
    }
}

impl Ranting for GermanPerson {
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.subject, uc)
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
        // Personal pronouns (ich/du/wir/...) don't decline by case here — `subjective()`,
        // `objective()` etc. already cover case via the pronoun hook.
        uc_1st_if(self.subject, uc)
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
        Some(uc_1st_if(word, uc))
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
        lexicon::conjugate(verb, person).map(|form| uc_1st_if(form, uc))
    }
}

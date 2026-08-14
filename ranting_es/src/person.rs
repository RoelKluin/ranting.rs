//! `SpanishPerson` — a speaker/addressee entity, present so that verb agreement (and
//! specifically `tú` vs `usted`) can be shown across all six persons rather than only the
//! third, which is all a noun ever exercises. Mirrors `ranting_i18n::person`.

use crate::lexicon::{self, Person};
use ranting::*;
use std::fmt;

/// `yo` / `tú` / `usted` / `nosotros` / `vosotros` / `ustedes` — a personal pronoun as a
/// `Ranting` entity.
///
/// It carries no noun class: Spanish personal pronouns take no article and no attributive
/// adjective, so there is nothing for a class to select — same as `ranting_i18n::GermanPerson`.
#[derive(Copy, Clone)]
pub struct SpanishPerson {
    subject: &'static str,
    plural: bool,
}

impl SpanishPerson {
    /// `yo` — first person singular.
    pub const YO: SpanishPerson = SpanishPerson {
        subject: "yo",
        plural: false,
    };
    /// `tú` — second person singular, informal.
    pub const TU: SpanishPerson = SpanishPerson {
        subject: "tú",
        plural: false,
    };
    /// `usted` — second person singular, formal. Grammatically third-person-**singular**: this
    /// is the pair `tú`/`usted` item 23 asks this crate to exercise, and the sharpest contrast
    /// with `ranting_i18n::GermanPerson::SIE` (which borrows the third-person-**plural** slot
    /// instead). See [`Person`] and the crate README.
    pub const USTED: SpanishPerson = SpanishPerson {
        subject: "usted",
        plural: false,
    };
    /// `nosotros` — first person plural.
    pub const NOSOTROS: SpanishPerson = SpanishPerson {
        subject: "nosotros",
        plural: true,
    };
    /// `vosotros` — second person plural, informal (Spain).
    pub const VOSOTROS: SpanishPerson = SpanishPerson {
        subject: "vosotros",
        plural: true,
    };
    /// `ustedes` — second person plural, formal in Spain and the *only* second-person plural in
    /// most of Latin America. Grammatically third-person plural, like `ellos`.
    pub const USTEDES: SpanishPerson = SpanishPerson {
        subject: "ustedes",
        plural: true,
    };

    fn objective(&self) -> &'static str {
        match self.subject {
            "yo" => "me",
            "tú" => "te",
            "nosotros" => "nos",
            "vosotros" => "os",
            "ustedes" => "los",
            _ => "lo", // usted
        }
    }

    fn possessive(&self) -> &'static str {
        match self.subject {
            "yo" => "mi",
            "tú" => "tu",
            "nosotros" => "nuestro",
            "vosotros" => "vuestro",
            _ => "su", // usted, ustedes
        }
    }

    fn reflexive(&self) -> &'static str {
        match self.subject {
            "yo" => "me",
            "tú" => "te",
            "nosotros" => "nos",
            "vosotros" => "os",
            _ => "se", // usted, ustedes
        }
    }
}

impl fmt::Display for SpanishPerson {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.subject)
    }
}

impl Ranting for SpanishPerson {
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
        // Personal pronouns don't decline by case here — `subjective()`/`objective()`/etc.
        // already cover case via the pronoun hook, same as `ranting_i18n::GermanPerson`.
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

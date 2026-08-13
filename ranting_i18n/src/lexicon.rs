//! The closed German vocabulary: three nouns, four verbs, three adjectives and the numerals
//! `0..=12`, plus the article/adjective/numeral inflection tables they need.
//!
//! Nothing in this module knows about `ranting` — it is plain German morphology, so that
//! [`crate::noun`] and [`crate::person`] contain only the trait plumbing and it is obvious which
//! of the two is doing the work. That separation is deliberate: the point of this crate is to
//! show what the trait seam can and cannot carry, and mixing the two would blur it.

/// The four cases of German. `ranting`'s [`ranting::GrammaticalCase`] has no dative (see the
/// crate README, hole 3), so a `GermanNoun` carries this itself.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Case {
    Nominative,
    Accusative,
    Dative,
    Genitive,
}

impl Case {
    const fn index(self) -> usize {
        match self {
            Case::Nominative => 0,
            Case::Accusative => 1,
            Case::Dative => 2,
            Case::Genitive => 3,
        }
    }
}

/// Which article stands in front of the noun. German adjective endings depend on it (weak after
/// `der`, mixed after `ein`, strong after nothing), and no hook reports it — see README hole 4.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Definiteness {
    /// `der`/`die`/`das` — the adjective declines weak.
    Definite,
    /// `ein`/`eine` — the adjective declines mixed.
    Indefinite,
    /// No article at all — the adjective declines strong.
    Bare,
}

/// The three noun classes this lexicon uses, as the labels handed to
/// [`ranting::NounClass`].
pub const MASCULINE: &str = "masculine";
pub const FEMININE: &str = "feminine";
pub const NEUTER: &str = "neuter";

/// One closed-vocabulary noun, fully declined. Four singular then four plural forms, indexed by
/// [`Case::index`].
pub struct NounEntry {
    pub class: &'static str,
    pub singular: [&'static str; 4],
    pub plural: [&'static str; 4],
}

impl NounEntry {
    /// The noun's own form for a case and number — the thing
    /// [`ranting::Ranting::inflect`] cannot ask for, since it takes only `to_plural`.
    pub fn form(&self, case: Case, plural: bool) -> &'static str {
        if plural {
            self.plural[case.index()]
        } else {
            self.singular[case.index()]
        }
    }
}

/// `der Hund` — masculine, dative plural `den Hunden`.
pub static HUND: NounEntry = NounEntry {
    class: MASCULINE,
    singular: ["Hund", "Hund", "Hund", "Hundes"],
    plural: ["Hunde", "Hunde", "Hunden", "Hunde"],
};

/// `die Katze` — feminine, invariant apart from the plural stem.
pub static KATZE: NounEntry = NounEntry {
    class: FEMININE,
    singular: ["Katze", "Katze", "Katze", "Katze"],
    plural: ["Katzen", "Katzen", "Katzen", "Katzen"],
};

/// `das Haus` — neuter, umlauting plural `Häuser`, dative plural `Häusern`.
pub static HAUS: NounEntry = NounEntry {
    class: NEUTER,
    singular: ["Haus", "Haus", "Haus", "Hauses"],
    plural: ["Häuser", "Häuser", "Häusern", "Häuser"],
};

/// The definite article, declined by case, class and number: `der`/`die`/`das`/`den`/`dem`/`des`.
pub fn definite_article(case: Case, class: &str, plural: bool) -> &'static str {
    if plural {
        return match case {
            Case::Nominative | Case::Accusative => "die",
            Case::Dative => "den",
            Case::Genitive => "der",
        };
    }
    match (case, class) {
        (Case::Nominative, MASCULINE) => "der",
        (Case::Nominative, FEMININE) => "die",
        (Case::Nominative, _) => "das",
        (Case::Accusative, MASCULINE) => "den",
        (Case::Accusative, FEMININE) => "die",
        (Case::Accusative, _) => "das",
        (Case::Dative, FEMININE) => "der",
        (Case::Dative, _) => "dem",
        (Case::Genitive, FEMININE) => "der",
        (Case::Genitive, _) => "des",
    }
}

/// The indefinite article. German has none in the plural, which is why this returns `""` there —
/// see README hole 6 for what that empty string costs at the placeholder seam.
pub fn indefinite_article(case: Case, class: &str, plural: bool) -> &'static str {
    if plural {
        return "";
    }
    match (case, class) {
        (Case::Nominative, MASCULINE) | (Case::Nominative, NEUTER) => "ein",
        (Case::Nominative, _) => "eine",
        (Case::Accusative, MASCULINE) => "einen",
        (Case::Accusative, FEMININE) => "eine",
        (Case::Accusative, _) => "ein",
        (Case::Dative, FEMININE) => "einer",
        (Case::Dative, _) => "einem",
        (Case::Genitive, FEMININE) => "einer",
        (Case::Genitive, _) => "eines",
    }
}

/// The attributive adjective ending, by declension class (weak/mixed/strong, chosen from
/// `definiteness`), case, gender and number. This is the full German table, not a simplification;
/// what the crate cannot do is put the result in front of the noun (README hole 4).
pub fn adjective_ending(
    definiteness: Definiteness,
    case: Case,
    class: &str,
    plural: bool,
) -> &'static str {
    match definiteness {
        // After `der`/`die`/`das`: only -e and -en occur.
        Definiteness::Definite => {
            if plural {
                return "en";
            }
            match (case, class) {
                (Case::Nominative, _) => "e",
                (Case::Accusative, MASCULINE) => "en",
                (Case::Accusative, _) => "e",
                _ => "en",
            }
        }
        // After `ein`: strong endings where `ein` itself carries none, weak elsewhere. There is
        // no indefinite plural article, so the plural falls back to strong.
        Definiteness::Indefinite => {
            if plural {
                return adjective_ending(Definiteness::Bare, case, class, true);
            }
            match (case, class) {
                (Case::Nominative, MASCULINE) => "er",
                (Case::Nominative, NEUTER) => "es",
                (Case::Nominative, _) => "e",
                (Case::Accusative, MASCULINE) => "en",
                (Case::Accusative, NEUTER) => "es",
                (Case::Accusative, _) => "e",
                _ => "en",
            }
        }
        // No article: the adjective carries the article's own endings.
        Definiteness::Bare => {
            if plural {
                return match case {
                    Case::Nominative | Case::Accusative => "e",
                    Case::Dative => "en",
                    Case::Genitive => "er",
                };
            }
            match (case, class) {
                (Case::Nominative, MASCULINE) => "er",
                (Case::Nominative, NEUTER) => "es",
                (Case::Nominative, _) => "e",
                (Case::Accusative, MASCULINE) => "en",
                (Case::Accusative, NEUTER) => "es",
                (Case::Accusative, _) => "e",
                (Case::Dative, FEMININE) => "er",
                (Case::Dative, _) => "em",
                (Case::Genitive, FEMININE) => "er",
                (Case::Genitive, _) => "en",
            }
        }
    }
}

/// The three adjectives this lexicon knows, as bare stems. Anything else is passed through
/// unchanged with the ending appended — the endings are regular, the stems are the closed part.
pub const ADJECTIVES: [&str; 3] = ["klein", "alt", "schön"];

/// Grammatical person, recovered from the German pronoun a `Ranting` entity declares as its
/// `subjective()`. `sie` is 3rd singular or 3rd plural depending on the placeholder's own number
/// agreement, which is exactly what `as_plural` reports.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Person {
    Ich,
    Du,
    ErSieEs,
    Wir,
    Ihr,
    SiePlural,
}

impl Person {
    /// Map a declared `subjective()` string plus the placeholder's number agreement onto a
    /// person. Unknown labels are treated as 3rd person, which is what a noun is.
    pub fn from_subject(subject: &str, plural: bool) -> Person {
        match subject {
            "ich" => Person::Ich,
            "du" => Person::Du,
            "wir" => Person::Wir,
            "ihr" => Person::Ihr,
            "Sie" => Person::SiePlural,
            _ if plural => Person::SiePlural,
            _ => Person::ErSieEs,
        }
    }

    const fn index(self) -> usize {
        match self {
            Person::Ich => 0,
            Person::Du => 1,
            Person::ErSieEs => 2,
            Person::Wir => 3,
            Person::Ihr => 4,
            Person::SiePlural => 5,
        }
    }
}

/// Present-tense conjugations for the closed verb set: one regular verb, two stem-changing
/// strong verbs and the irregular `sein`. Order is [`Person::index`].
static VERBS: [(&str, [&str; 6]); 4] = [
    (
        "bellen",
        ["belle", "bellst", "bellt", "bellen", "bellt", "bellen"],
    ),
    (
        "sehen",
        ["sehe", "siehst", "sieht", "sehen", "seht", "sehen"],
    ),
    (
        "schlafen",
        [
            "schlafe",
            "schläfst",
            "schläft",
            "schlafen",
            "schlaft",
            "schlafen",
        ],
    ),
    ("sein", ["bin", "bist", "ist", "sind", "seid", "sind"]),
];

/// Conjugate an infinitive in the present tense, or `None` when it is not in the closed set.
///
/// Returning `None` rather than guessing is what lets the `Ranting` hook fall through to English
/// for anything this lexicon does not know, which is how a partial fork is supposed to behave.
pub fn conjugate(infinitive: &str, person: Person) -> Option<&'static str> {
    VERBS
        .iter()
        .find(|(inf, _)| *inf == infinitive)
        .map(|(_, forms)| forms[person.index()])
}

/// `zwei`, `drei`, … — the numerals `0..=12` spelled out. `1` is handled by
/// [`numeral_one`] instead, since it agrees like an article.
static NUMERALS: [&str; 13] = [
    "null", "eins", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn",
    "elf", "zwölf",
];

/// The numeral `1` agrees with its noun exactly like the indefinite article: `ein Hund`,
/// `eine Katze`, `einen Hund`.
pub fn numeral_one(case: Case, class: &str) -> &'static str {
    indefinite_article(case, class, false)
}

/// Spell a count in German, or `None` outside the closed range — again, falling through rather
/// than guessing.
pub fn spell(count: i64, case: Case, class: &str) -> Option<String> {
    match count {
        1 => Some(numeral_one(case, class).to_string()),
        0..=12 => Some(NUMERALS[count as usize].to_string()),
        _ => None,
    }
}

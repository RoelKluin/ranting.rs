//! The closed French vocabulary: seven nouns, three verbs, eight prenominal and three
//! postnominal adjectives, and the numerals `0..=20` plus the vigesimal irregulars `70/71/80/
//! 81/90/91`, plus the article/adjective/numeral inflection tables they need.
//!
//! Nothing in this module knows about `ranting` — it is plain French morphology, so that
//! [`crate::noun`] and [`crate::person`] contain only the trait plumbing. Same separation
//! `ranting_es`/`ranting_ar` use, for the same reason.

/// The two noun classes this lexicon uses, as the labels handed to [`ranting::NounClass`].
/// French has no third class, unlike German, and no grammatical case at all, like Spanish.
pub const MASCULINE: &str = "masculine";
pub const FEMININE: &str = "feminine";

/// One closed-vocabulary noun.
pub struct NounEntry {
    pub class: &'static str,
    pub singular: &'static str,
    pub plural: &'static str,
    /// Mass (uncountable) nouns take a partitive article (`du`/`de la`/`des`) instead of the
    /// indefinite `un`/`une` — see [`crate::noun::FrenchNoun::is_mass`] and the crate README.
    pub mass: bool,
    /// `h aspiré`: an initial `h` that, unlike an ordinary `h muet`, blocks elision even though
    /// both spell identically as a plain `h`. `héros` sets this; `homme` doesn't. See
    /// [`crate::noun::FrenchNoun::elide_article_custom`].
    pub h_aspire: bool,
}

/// `le chat` — masculine, regular, consonant-initial (no elision).
pub static CHAT: NounEntry = NounEntry {
    class: MASCULINE,
    singular: "chat",
    plural: "chats",
    mass: false,
    h_aspire: false,
};

/// `la maison` — feminine, regular, consonant-initial.
pub static MAISON: NounEntry = NounEntry {
    class: FEMININE,
    singular: "maison",
    plural: "maisons",
    mass: false,
    h_aspire: false,
};

/// `l'arbre` — masculine, vowel-initial: elides.
pub static ARBRE: NounEntry = NounEntry {
    class: MASCULINE,
    singular: "arbre",
    plural: "arbres",
    mass: false,
    h_aspire: false,
};

/// `l'école` — feminine, vowel-initial (accented): elides.
pub static ECOLE: NounEntry = NounEntry {
    class: FEMININE,
    singular: "école",
    plural: "écoles",
    mass: false,
    h_aspire: false,
};

/// `l'homme` — masculine, `h muet`: elides despite the spelled `h`.
pub static HOMME: NounEntry = NounEntry {
    class: MASCULINE,
    singular: "homme",
    plural: "hommes",
    mass: false,
    h_aspire: false,
};

/// `le héros` — masculine, `h aspiré`: does **not** elide, despite spelling identically to
/// `homme`'s initial letter. The negative elision case — see the crate README.
pub static HEROS: NounEntry = NounEntry {
    class: MASCULINE,
    singular: "héros",
    plural: "héros",
    mass: false,
    h_aspire: true,
};

/// `l'eau` / `de l'eau` — feminine, mass, vowel-initial: exercises the partitive article and
/// elision together.
pub static EAU: NounEntry = NounEntry {
    class: FEMININE,
    singular: "eau",
    plural: "eaux",
    mass: true,
    h_aspire: false,
};

/// The definite article: `le`/`la`/`les`. Elision (`l'`) is a separate, post-assembly step —
/// see [`crate::noun::FrenchNoun::elide_article_custom`] — because it depends on what follows
/// the article, not on the noun's class or number alone.
pub fn definite_article(class: &str, plural: bool) -> &'static str {
    if plural {
        return "les";
    }
    if class == FEMININE { "la" } else { "le" }
}

/// The indefinite article: `un`/`une`/`des`.
pub fn indefinite_article(class: &str, plural: bool) -> &'static str {
    if plural {
        return "des";
    }
    if class == FEMININE { "une" } else { "un" }
}

/// The partitive article a mass noun takes instead of the indefinite: `du`/`de la`. There is no
/// singular/plural distinction to make here — a mass noun's "some" is always this, never `des`
/// (which this lexicon reserves for the ordinary count-noun plural indefinite).
pub fn partitive_article(class: &str) -> &'static str {
    if class == FEMININE { "de la" } else { "du" }
}

/// A vowel, or an accented vowel, or an `h` that isn't `h aspiré` — the elision trigger. See
/// [`crate::noun::FrenchNoun::elide_article_custom`] for how `h_aspire` is threaded in.
pub fn starts_elidable(word: &str, h_aspire: bool) -> bool {
    match word.chars().next() {
        Some(c) if "aeiouyAEIOUYâàéèêëîïôöûüÂÀÉÈÊËÎÏÔÖÛÜ".contains(c) => {
            true
        }
        Some('h') | Some('H') => !h_aspire,
        _ => false,
    }
}

/// One adjective's four agreement forms, plus whether real French wants it *before* the noun
/// (`grand chat`) or *after* it (`chat noir`). `inflect_adjective_custom` agrees every entry
/// here correctly regardless of `prenominal` — the `!`/`!!` degree slot it is called from only
/// ever renders post-noun, so a `prenominal` entry's correctly agreed form still comes out in
/// the wrong position. This is the hole: not missing agreement data, but nowhere correct to put
/// output that *is* agreed right. See [`crate::noun::FrenchNoun::inflect_adjective_custom`] and
/// the crate README.
struct AdjectiveForms {
    stem: &'static str,
    masc_sg: &'static str,
    fem_sg: &'static str,
    masc_pl: &'static str,
    fem_pl: &'static str,
    prenominal: bool,
}

/// The eleven adjectives this lexicon knows: eight prenominal (real French word order puts
/// these *before* the noun) and three postnominal (the position the `!`/`!!` slot actually
/// renders). No `beau`-style liaison irregularity (`beau` → `bel` before a vowel-initial
/// masculine noun) is modeled — out of scope for a closed lexicon whose prenominal set can
/// never reach a grammatically correct sentence through this hook anyway.
static ADJECTIVES: [AdjectiveForms; 11] = [
    AdjectiveForms {
        stem: "grand",
        masc_sg: "grand",
        fem_sg: "grande",
        masc_pl: "grands",
        fem_pl: "grandes",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "petit",
        masc_sg: "petit",
        fem_sg: "petite",
        masc_pl: "petits",
        fem_pl: "petites",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "beau",
        masc_sg: "beau",
        fem_sg: "belle",
        masc_pl: "beaux",
        fem_pl: "belles",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "bon",
        masc_sg: "bon",
        fem_sg: "bonne",
        masc_pl: "bons",
        fem_pl: "bonnes",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "vieux",
        masc_sg: "vieux",
        fem_sg: "vieille",
        masc_pl: "vieux",
        fem_pl: "vieilles",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "nouveau",
        masc_sg: "nouveau",
        fem_sg: "nouvelle",
        masc_pl: "nouveaux",
        fem_pl: "nouvelles",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "jeune",
        masc_sg: "jeune",
        fem_sg: "jeune",
        masc_pl: "jeunes",
        fem_pl: "jeunes",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "joli",
        masc_sg: "joli",
        fem_sg: "jolie",
        masc_pl: "jolis",
        fem_pl: "jolies",
        prenominal: true,
    },
    AdjectiveForms {
        stem: "noir",
        masc_sg: "noir",
        fem_sg: "noire",
        masc_pl: "noirs",
        fem_pl: "noires",
        prenominal: false,
    },
    AdjectiveForms {
        stem: "rouge",
        masc_sg: "rouge",
        fem_sg: "rouge",
        masc_pl: "rouges",
        fem_pl: "rouges",
        prenominal: false,
    },
    AdjectiveForms {
        stem: "intelligent",
        masc_sg: "intelligent",
        fem_sg: "intelligente",
        masc_pl: "intelligents",
        fem_pl: "intelligentes",
        prenominal: false,
    },
];

/// Agree an adjective from this lexicon's closed set with a noun's class and number, or `None`
/// if it isn't one of [`ADJECTIVES`] — the same decline-rather-than-guess contract as the verb
/// and numeral tables. Agreement is correct for *every* entry, prenominal or not; see
/// [`AdjectiveForms`]'s doc comment for why a prenominal entry is still the crate's hole.
pub fn adjective_form(adjective: &str, feminine: bool, plural: bool) -> Option<String> {
    let entry = ADJECTIVES.iter().find(|e| e.stem == adjective)?;
    let form = match (feminine, plural) {
        (false, false) => entry.masc_sg,
        (true, false) => entry.fem_sg,
        (false, true) => entry.masc_pl,
        (true, true) => entry.fem_pl,
    };
    Some(form.to_string())
}

/// Whether real French wants this adjective *before* its noun. Not consulted by
/// [`adjective_form`] or the hook — `inflect_adjective_custom` has no way to act on it, which is
/// the hole. Exposed only so `tests/holes.rs` can assert the position mismatch against the same
/// data the crate's own agreement table uses, rather than a second hard-coded word list.
pub fn is_prenominal(adjective: &str) -> bool {
    ADJECTIVES
        .iter()
        .any(|e| e.stem == adjective && e.prenominal)
}

/// Grammatical person, recovered from the French pronoun a `Ranting` entity declares as its
/// `subjective()`, the same uninterpreted-channel discipline `ranting_es`/`ranting_i18n` use.
///
/// `vous`-formal (singular formal "you") and `vous`-plural share **the same word and the same
/// conjugation row** — unlike Spanish's `usted` (borrows third-person-singular, a distinct word
/// from `ustedes`) or German's `Sie` (borrows third-person-plural, a distinct word from `sie`).
/// So `Vous` is reached by `"vous"` regardless of number, rather than by two different subject
/// strings. See the crate README's "`tu` vs `vous`" section.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Person {
    Je,
    Tu,
    IlElle,
    Nous,
    Vous,
    IlsElles,
}

impl Person {
    /// Map a declared `subjective()` string plus the placeholder's number agreement onto a
    /// person. Unknown labels (a noun's own `il`/`elle`/`ils`/`elles`) fall through on
    /// `as_plural` alone, same as Spanish's `_ => Person::ElEllaUsted`.
    pub fn from_subject(subject: &str, plural: bool) -> Person {
        match subject {
            "je" | "j'" => Person::Je,
            "tu" => Person::Tu,
            "vous" => Person::Vous,
            "nous" => Person::Nous,
            "ils" | "elles" => Person::IlsElles,
            "il" | "elle" => Person::IlElle,
            _ if plural => Person::IlsElles,
            _ => Person::IlElle,
        }
    }

    const fn index(self) -> usize {
        match self {
            Person::Je => 0,
            Person::Tu => 1,
            Person::IlElle => 2,
            Person::Nous => 3,
            Person::Vous => 4,
            Person::IlsElles => 5,
        }
    }
}

/// Present-tense conjugations for the closed verb set: the two irregular auxiliaries and one
/// regular `-er` verb. Order is [`Person::index`]: je, tu, il/elle, nous, vous, ils/elles.
static VERBS: [(&str, [&str; 6]); 3] = [
    ("être", ["suis", "es", "est", "sommes", "êtes", "sont"]),
    ("avoir", ["ai", "as", "a", "avons", "avez", "ont"]),
    (
        "parler",
        ["parle", "parles", "parle", "parlons", "parlez", "parlent"],
    ),
];

/// Conjugate an infinitive in the present tense, or `None` when it is not in the closed set —
/// the same fall-through-to-English contract `ranting_es::lexicon::conjugate` documents.
pub fn conjugate(infinitive: &str, person: Person) -> Option<&'static str> {
    VERBS
        .iter()
        .find(|(inf, _)| *inf == infinitive)
        .map(|(_, forms)| forms[person.index()])
}

/// `zéro`, `un`, `deux`, … `vingt` — spelled out, `0..=20`. `un` is also handled by
/// [`numeral_one`] when it needs to agree with its noun's gender, exactly like Spanish `uno`.
static NUMERALS: [&str; 21] = [
    "zéro", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix", "onze",
    "douze", "treize", "quatorze", "quinze", "seize", "dix-sept", "dix-huit", "dix-neuf", "vingt",
];

/// The numeral `1` agrees with its noun exactly like the indefinite article: `un chat`,
/// `une maison`.
pub fn numeral_one(class: &str) -> &'static str {
    indefinite_article(class, false)
}

/// Spell a count in French, or `None` outside the closed set — falling through to English rather
/// than guessing, same as `ranting_es::lexicon::spell`.
///
/// `0..=20` is the regular contiguous range; `70`/`71`/`80`/`81`/`90`/`91` are called out
/// individually to demonstrate the vigesimal irregularity a full `0..=99` table would need
/// throughout (`soixante-dix` "sixty-ten", `quatre-vingts` "four-twenties",
/// `quatre-vingt-dix` "four-twenty-ten") — genuinely different algorithm shape from
/// `ranting_es`/`ranting_ar`'s numeral tables, not merely different spellings of the same one.
pub fn spell(count: i64, class: &str) -> Option<String> {
    match count {
        1 => Some(numeral_one(class).to_string()),
        0..=20 => Some(NUMERALS[count as usize].to_string()),
        70 => Some("soixante-dix".to_string()),
        71 => Some("soixante et onze".to_string()),
        80 => Some("quatre-vingts".to_string()),
        81 => Some("quatre-vingt-un".to_string()),
        90 => Some("quatre-vingt-dix".to_string()),
        91 => Some("quatre-vingt-onze".to_string()),
        _ => None,
    }
}

/// `premier`/`première`, `deuxième`, `troisième`, `quatrième`, `cinquième` — the ordinals
/// `1..=5`. Unlike Spanish, where every ordinal agrees in gender, only `premier` does: `deuxième`
/// onward is already `-ième`-invariant across both genders — a different agreement shape from
/// `ranting_es::lexicon::ordinal`, not the same rule with different words.
static ORDINALS: [&str; 6] = [
    "zéro",
    "premier",
    "deuxième",
    "troisième",
    "quatrième",
    "cinquième",
];

/// Spell an ordinal in French, or `None` outside the closed `0..=5` range. Agrees in gender only
/// at `1` (`premier` → `première`); every other ordinal in this range is invariant.
pub fn ordinal(count: i64, class: &str) -> Option<String> {
    let word = *ORDINALS.get(usize::try_from(count).ok()?)?;
    if word == "premier" && class == FEMININE {
        return Some("première".to_string());
    }
    Some(word.to_string())
}

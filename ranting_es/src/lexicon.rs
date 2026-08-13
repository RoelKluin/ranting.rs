//! The closed Spanish vocabulary: four nouns, four verbs, three adjectives and the numerals
//! `0..=12`, plus the article/adjective/numeral inflection tables they need.
//!
//! Nothing in this module knows about `ranting` — it is plain Spanish morphology, so that
//! [`crate::noun`] and [`crate::person`] contain only the trait plumbing and it is obvious which
//! of the two is doing the work. Same separation `ranting_i18n` uses, for the same reason.

/// The two noun classes this lexicon uses, as the labels handed to [`ranting::NounClass`].
/// Spanish has no third class and no grammatical case, unlike German — see the crate README.
pub const MASCULINE: &str = "masculine";
pub const FEMININE: &str = "feminine";

/// One closed-vocabulary noun.
pub struct NounEntry {
    pub class: &'static str,
    pub singular: &'static str,
    pub plural: &'static str,
    /// Feminine nouns that begin with a stressed `a`/`ha` sound take the masculine-*looking*
    /// singular article (`el agua`, `un agua`) to avoid the vowel clash `la agua` would cause —
    /// a phonological euphony rule, not a change of grammatical gender: `el agua fría` still
    /// takes feminine adjective agreement, and the plural reverts to `las aguas`. This flag is
    /// what `agua` sets and `gato`/`casa`/`problema` don't.
    pub euphonic_el: bool,
}

/// `el gato` — masculine, fully regular.
pub static GATO: NounEntry = NounEntry {
    class: MASCULINE,
    singular: "gato",
    plural: "gatos",
    euphonic_el: false,
};

/// `la casa` — feminine, fully regular.
pub static CASA: NounEntry = NounEntry {
    class: FEMININE,
    singular: "casa",
    plural: "casas",
    euphonic_el: false,
};

/// `el problema` — masculine despite the `-a` ending, the classic counterexample to guessing
/// gender from a noun's last letter (Greek-derived `-ma` nouns: `el problema`, `el sistema`,
/// `el idioma`). Exactly the trap `docs/EXTENSIBILITY.md` §4.3 calls out its own pre-`NounClass`
/// example for falling into.
pub static PROBLEMA: NounEntry = NounEntry {
    class: MASCULINE,
    singular: "problema",
    plural: "problemas",
    euphonic_el: false,
};

/// `el agua` / `las aguas` — feminine, but singular takes the euphonic article. See
/// [`NounEntry::euphonic_el`].
pub static AGUA: NounEntry = NounEntry {
    class: FEMININE,
    singular: "agua",
    plural: "aguas",
    euphonic_el: true,
};

/// The definite article: `el`/`la`/`los`/`las`.
pub fn definite_article(class: &str, plural: bool, euphonic_el: bool) -> &'static str {
    match (plural, class == FEMININE) {
        (true, true) => "las",
        (true, false) => "los",
        (false, true) if euphonic_el => "el",
        (false, true) => "la",
        (false, false) => "el",
    }
}

/// The indefinite article: `un`/`una`/`unos`/`unas`. Spanish has an indefinite plural, unlike
/// German (see `ranting_i18n`'s hole 6) — there is no zero-length-article case here at all.
pub fn indefinite_article(class: &str, plural: bool, euphonic_el: bool) -> &'static str {
    match (plural, class == FEMININE) {
        (true, true) => "unas",
        (true, false) => "unos",
        (false, true) if euphonic_el => "un",
        (false, true) => "una",
        (false, false) => "un",
    }
}

/// The three adjectives this lexicon knows, as bare stems, with their agreement kind.
///
/// `negro` and `pequeño` end in `-o` and take the regular four-way `-o`/`-a`/`-os`/`-as` pattern.
/// `azul` ends in a consonant and is gender-invariant — Spanish adjectives that don't end in `-o`
/// (colors like `azul`, `verde`; most adjectives ending in a consonant or `-e`) don't change for
/// gender at all, only for number (`azul` → `azules`). Deliberately regular: no apocope (`bueno`
/// → `buen`, `grande` → `gran`) is modeled, because apocope is a *prenominal*-only phenomenon —
/// see the crate README's "why this crate has no adjective-position hole" note — so this lexicon
/// never has to compute it in the one (post-nominal) position it can render an adjective at all.
pub const ADJECTIVES: [&str; 3] = ["negro", "pequeño", "azul"];

/// Agree an adjective from this lexicon's closed set with a noun's class and number, or `None`
/// if it isn't one of [`ADJECTIVES`] — the same decline-rather-than-guess contract as the verb
/// and numeral tables.
pub fn adjective_form(adjective: &str, feminine: bool, plural: bool) -> Option<String> {
    if !ADJECTIVES.contains(&adjective) {
        return None;
    }
    let gendered = match adjective.strip_suffix('o') {
        Some(stem) if feminine => format!("{stem}a"),
        Some(_) => adjective.to_string(),
        None => adjective.to_string(),
    };
    if !plural {
        return Some(gendered);
    }
    Some(match gendered.chars().last() {
        Some('a') | Some('e') | Some('o') | Some('i') | Some('u') => format!("{gendered}s"),
        _ => format!("{gendered}es"),
    })
}

/// Grammatical person, recovered from the Spanish pronoun a `Ranting` entity declares as its
/// `subjective()`, exactly like `ranting_i18n::lexicon::Person` — an uninterpreted channel
/// (ROADMAP "`SubjectPronoun` is a closed English enum" decision).
///
/// `usted` (formal "you") maps onto the *same* index as `él`/`ella` — Spanish formal address
/// borrows third-person-**singular** verb agreement, unlike German `Sie`, which borrows
/// third-person-**plural**. `ustedes` (formal "you-all", and in most of Latin America the
/// *only* second-person plural, `vosotros` being Spain-specific) borrows third-person plural.
/// See the crate README's "`tú` vs `usted`" section.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Person {
    Yo,
    Tu,
    ElEllaUsted,
    Nosotros,
    Vosotros,
    EllosUstedes,
}

impl Person {
    /// Map a declared `subjective()` string plus the placeholder's number agreement onto a
    /// person. Unknown labels (a noun's own `él`/`ella`/`ellos`/`ellas`) fall through on
    /// `as_plural` alone, same as German's `_ => Person::ErSieEs`.
    pub fn from_subject(subject: &str, plural: bool) -> Person {
        match subject {
            "yo" => Person::Yo,
            "tú" => Person::Tu,
            "usted" => Person::ElEllaUsted,
            "nosotros" | "nosotras" => Person::Nosotros,
            "vosotros" | "vosotras" => Person::Vosotros,
            "ustedes" => Person::EllosUstedes,
            _ if plural => Person::EllosUstedes,
            _ => Person::ElEllaUsted,
        }
    }

    const fn index(self) -> usize {
        match self {
            Person::Yo => 0,
            Person::Tu => 1,
            Person::ElEllaUsted => 2,
            Person::Nosotros => 3,
            Person::Vosotros => 4,
            Person::EllosUstedes => 5,
        }
    }
}

/// Present-tense conjugations for the closed verb set: two regular `-ar`/`-er`/`-ir` verbs, one
/// more of the third regular class, and the irregular `ser`. Order is [`Person::index`]: yo, tú,
/// él/ella/usted, nosotros, vosotros, ellos/ellas/ustedes.
static VERBS: [(&str, [&str; 6]); 4] = [
    (
        "hablar",
        ["hablo", "hablas", "habla", "hablamos", "habláis", "hablan"],
    ),
    (
        "comer",
        ["como", "comes", "come", "comemos", "coméis", "comen"],
    ),
    (
        "vivir",
        ["vivo", "vives", "vive", "vivimos", "vivís", "viven"],
    ),
    ("ser", ["soy", "eres", "es", "somos", "sois", "son"]),
];

/// Conjugate an infinitive in the present tense, or `None` when it is not in the closed set —
/// the same fall-through-to-English contract `ranting_i18n::lexicon::conjugate` documents.
pub fn conjugate(infinitive: &str, person: Person) -> Option<&'static str> {
    VERBS
        .iter()
        .find(|(inf, _)| *inf == infinitive)
        .map(|(_, forms)| forms[person.index()])
}

/// `cero`, `uno`, `dos`, … — the numerals `0..=12` spelled out. `1` is handled by
/// [`numeral_one`] instead, since — like German `eins`/`ein`/`eine` — it agrees with its noun.
static NUMERALS: [&str; 13] = [
    "cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
    "once", "doce",
];

/// The numeral `1` agrees with its noun exactly like the indefinite article: `un gato`,
/// `una casa`, `un agua` (the same euphonic exception the article takes).
pub fn numeral_one(class: &str, euphonic_el: bool) -> &'static str {
    indefinite_article(class, false, euphonic_el)
}

/// Spell a count in Spanish, or `None` outside the closed range — falling through to English
/// rather than guessing, same as `ranting_i18n::lexicon::spell`.
pub fn spell(count: i64, class: &str, euphonic_el: bool) -> Option<String> {
    match count {
        1 => Some(numeral_one(class, euphonic_el).to_string()),
        0..=12 => Some(NUMERALS[count as usize].to_string()),
        _ => None,
    }
}

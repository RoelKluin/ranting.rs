//! The grammar tables: a closed noun vocabulary with three numbers each, the sun/moon letter
//! split, past-tense verb conjugation, and Arabic numerals.
//!
//! Nothing here touches `ranting` — it is a plain lexicon, so [`crate::noun`] is the only file
//! that has to be read to see what the public trait seam does and does not offer.

/// Grammatical gender, used as the [`ranting::NounClass`](ranting::NounClass) label.
pub const MASCULINE: &str = "masculine";
/// Grammatical gender, used as the [`ranting::NounClass`](ranting::NounClass) label.
pub const FEMININE: &str = "feminine";

/// The fourteen **sun letters** (الحروف الشمسية). The definite article's `ل` assimilates to
/// them; before the fourteen moon letters it does not.
///
/// This is the closed consonant set `elide_article_custom` has to recognize, and the reason the
/// Arabic spike picked it: the hook receives the following text as a rendered string, so the
/// trigger is `following.chars().next()` and the question was whether that is enough signal.
/// It is.
const SUN_LETTERS: [char; 14] = [
    'ت', 'ث', 'د', 'ذ', 'ر', 'ز', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ل', 'ن',
];

/// The shadda (U+0651), which marks the doubled consonant a sun letter assimilation produces.
pub const SHADDA: char = '\u{0651}';

/// Is this word's first letter a sun letter?
pub fn is_sun_initial(word: &str) -> bool {
    word.chars()
        .next()
        .is_some_and(|c| SUN_LETTERS.contains(&c))
}

/// One noun, with all three of Arabic's morphological numbers.
///
/// The dual is a *separate stored form*, not a rule: `كتابان` is regular here but `شمسان` and
/// the sound plurals below are not all derivable from the singular by suffixing, and this crate
/// deliberately stores forms rather than deriving them — the same choice `ranting_i18n` and
/// `ranting_es` make.
pub struct NounEntry {
    pub singular: &'static str,
    /// Exactly two. Reachable only through the placeholder's own numeral — see README hole 1.
    pub dual: &'static str,
    /// Three or more.
    pub plural: &'static str,
    pub class: &'static str,
    /// A *sound* (suffixed) plural rather than a broken one. Recorded because the distinction is
    /// what makes the plural column a table instead of a rule, and the README cites it.
    pub sound_plural: bool,
}

/// `كتاب` "book" — masculine, **broken** plural `كتب`, moon-letter initial.
pub const KITAB: NounEntry = NounEntry {
    singular: "كتاب",
    dual: "كتابان",
    plural: "كتب",
    class: MASCULINE,
    sound_plural: false,
};

/// `شمس` "sun" — feminine, broken plural `شموس`, and a **sun letter** initial, so
/// `ال` + `شمس` assimilates.
pub const SHAMS: NounEntry = NounEntry {
    singular: "شمس",
    dual: "شمسان",
    plural: "شموس",
    class: FEMININE,
    sound_plural: false,
};

/// `قمر` "moon" — masculine, broken plural `أقمار`, **moon letter** initial (the class is named
/// after this word).
pub const QAMAR: NounEntry = NounEntry {
    singular: "قمر",
    dual: "قمران",
    plural: "أقمار",
    class: MASCULINE,
    sound_plural: false,
};

/// `معلم` "teacher" (m.) — masculine **sound** plural `معلمون`.
pub const MUALLIM: NounEntry = NounEntry {
    singular: "معلم",
    dual: "معلمان",
    plural: "معلمون",
    class: MASCULINE,
    sound_plural: true,
};

/// `طالبة` "student" (f.) — feminine **sound** plural `طالبات`, and a sun-letter initial.
pub const TALIBA: NounEntry = NounEntry {
    singular: "طالبة",
    dual: "طالبتان",
    plural: "طالبات",
    class: FEMININE,
    sound_plural: true,
};

/// Person/number/gender of a verb's subject. Arabic conjugates for all three, including a dual
/// in the second and third persons — the case that motivated this crate.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Person {
    FirstSingular,
    FirstPlural,
    SecondMasculine,
    SecondFeminine,
    SecondDual,
    SecondPlural,
    ThirdMasculine,
    ThirdFeminine,
    ThirdDualMasculine,
    ThirdDualFeminine,
    ThirdPluralMasculine,
    ThirdPluralFeminine,
}

impl Person {
    /// Read the subject label the entity declared, plus the placeholder's number agreement, into
    /// a person. The **dual** cannot be recovered from `as_plural` alone — a dual subject is
    /// "plural" as far as `ranting` is concerned — which is why [`from_subject_and_count`] takes
    /// the count and this one does not.
    pub fn from_subject(subject: &str, as_plural: bool) -> Person {
        match (subject, as_plural) {
            ("أنا", _) | ("I", _) => Person::FirstSingular,
            ("نحن", _) | ("we", _) => Person::FirstPlural,
            ("أنت", false) | ("you", false) => Person::SecondMasculine,
            ("أنتِ", _) => Person::SecondFeminine,
            ("أنت", true) | ("you", true) => Person::SecondPlural,
            ("هي", true) => Person::ThirdPluralFeminine,
            ("هي", false) => Person::ThirdFeminine,
            (_, true) => Person::ThirdPluralMasculine,
            (_, false) => Person::ThirdMasculine,
        }
    }

    /// The same, but honoring a placeholder numeral of exactly two: `كتب` → `كتبا`.
    pub fn from_subject_and_count(subject: &str, as_plural: bool, count: Option<i64>) -> Person {
        let person = Person::from_subject(subject, as_plural);
        if count != Some(2) {
            return person;
        }
        match person {
            Person::SecondPlural | Person::SecondMasculine | Person::SecondFeminine => {
                Person::SecondDual
            }
            Person::ThirdPluralFeminine | Person::ThirdFeminine => Person::ThirdDualFeminine,
            Person::ThirdPluralMasculine | Person::ThirdMasculine => Person::ThirdDualMasculine,
            other => other,
        }
    }
}

/// Past-tense (الماضي) conjugation for the two verbs in this vocabulary.
///
/// Returns `None` for anything else, which is the decline-rather-than-guess contract every hook
/// in this crate follows: an unknown verb falls back to `ranting`'s English conjugation rather
/// than being silently mangled.
pub fn conjugate(verb: &str, person: Person) -> Option<&'static str> {
    let forms: [&'static str; 12] = match verb {
        // كتب "to write"
        "كتب" | "write" => [
            "كتبت",
            "كتبنا",
            "كتبت",
            "كتبتِ",
            "كتبتما",
            "كتبتم",
            "كتب",
            "كتبت",
            "كتبا",
            "كتبتا",
            "كتبوا",
            "كتبن",
        ],
        // ذهب "to go"
        "ذهب" | "go" => [
            "ذهبت",
            "ذهبنا",
            "ذهبت",
            "ذهبتِ",
            "ذهبتما",
            "ذهبتم",
            "ذهب",
            "ذهبت",
            "ذهبا",
            "ذهبتا",
            "ذهبوا",
            "ذهبن",
        ],
        _ => return None,
    };
    let index = match person {
        Person::FirstSingular => 0,
        Person::FirstPlural => 1,
        Person::SecondMasculine => 2,
        Person::SecondFeminine => 3,
        Person::SecondDual => 4,
        Person::SecondPlural => 5,
        Person::ThirdMasculine => 6,
        Person::ThirdFeminine => 7,
        Person::ThirdDualMasculine => 8,
        Person::ThirdDualFeminine => 9,
        Person::ThirdPluralMasculine => 10,
        Person::ThirdPluralFeminine => 11,
    };
    Some(forms[index])
}

/// Arabic-Indic digits: `12` → `١٢`.
pub fn to_arabic_digits(rendered: &str) -> String {
    rendered
        .chars()
        .map(|c| match c {
            '0'..='9' => char::from_u32(c as u32 - '0' as u32 + 0x0660).unwrap_or(c),
            other => other,
        })
        .collect()
}

/// Spell a numeral `0..=10`, with Arabic's **gender polarity**: `1` and `2` agree with the
/// counted noun, while `3..=10` take the *opposite* gender — `ثلاثة كتب` (feminine-marked numeral,
/// masculine noun) but `ثلاث طالبات`.
///
/// This is the sharpest thing in the crate that `count` + `NounClass` together have to be
/// sufficient for, and they are: both arrive at `inflect_numeral_custom`.
pub fn spell(count: i64, class: &str) -> Option<String> {
    let feminine_noun = class == FEMININE;
    let word = match count {
        0 => "صفر",
        1 => {
            if feminine_noun {
                "واحدة"
            } else {
                "واحد"
            }
        }
        2 => {
            if feminine_noun {
                "اثنتان"
            } else {
                "اثنان"
            }
        }
        // Polarity: the numeral takes the gender *opposite* to the noun it counts.
        3..=10 => {
            let masculine_numeral = ["ثلاث", "أربع", "خمس", "ست", "سبع", "ثمان", "تسع", "عشر"];
            let feminine_numeral = [
                "ثلاثة",
                "أربعة",
                "خمسة",
                "ستة",
                "سبعة",
                "ثمانية",
                "تسعة",
                "عشرة",
            ];
            let index = (count - 3) as usize;
            if feminine_noun {
                masculine_numeral[index]
            } else {
                feminine_numeral[index]
            }
        }
        _ => return None,
    };
    Some(word.to_string())
}

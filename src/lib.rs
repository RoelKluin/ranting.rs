// (c) Roel Kluin 2022 GPL v3

pub use in_definite;
pub use inflector;
pub use inflector::cases::sentencecase::to_sentence_case;
pub use inflector::string::pluralize::to_plural;
pub use inflector::string::singularize::to_singular;

/// upper cases first character
pub fn uc_1st(s: &str) -> String {
    let mut c = s.chars();
    c.next()
        .map(|f| f.to_uppercase().collect::<String>())
        .unwrap_or_default()
        + c.as_str()
}

/// Returns Ok(true) if the subjective is plural, an Error if unrecognized or indiscernible.
/// ```
///     use ranting::is_subjective_plural;
///     let subjective: &str = "we";
///     assert!(is_subjective_plural(subjective)?);
///
/// ```
///
pub fn is_subjective_plural(subjective: &str) -> Result<bool, String> {
    match subjective {
        "we" | "they" | "ye" => Ok(true),
        "you" => Err("'you' could be either singular or plural".to_string()),
        "I" | "she" | "he" | "it" | "thou" => Ok(false),
        x => Err(format!("'{x}' is not recognized as subjective")),
    }
}

/// Return the subjective teh same for subjective except the case can differ.
pub fn subjective(subjective: &str, uc: bool) -> &str {
    match subjective {
        "you" if uc => "You",
        "thou" if uc => "Thou",
        "he" if uc => "He",
        "she" if uc => "She",
        "it" if uc => "It",
        "we" if uc => "We",
        "ye" if uc => "Ye",
        "they" if uc => "They",
        alt => alt,
    }
}

/// inflect subjective to singular / pluralize according to nr
/// pancis for invalid subjective. valid are:
/// "I", "you", "thou", "he", "she", "it", "we", "ye", "they".
/// all must be lowercase except "I".
/// If the subjective is "they", the assumption is neutrum, which is a guess.
///
pub fn inflect_subjective(subject: &str, to_plural: bool, uc: bool) -> &str {
    if to_plural == is_subjective_plural(subject).unwrap_or(false) {
        subjective(subject, uc)
    } else if to_plural {
        match subject {
            "I" if uc => "We",
            "you" if uc => "You",
            "thou" if uc => "Ye",
            "he" if uc => "They",
            "she" if uc => "They",
            "it" if uc => "They",
            "we" if uc => "We",
            "ye" if uc => "Ye",
            "they" if uc => "They",
            "you" if uc => "You",
            "I" | "we" => "we",
            "you" => "you",
            "thou" | "ye" => "ye",
            "he" | "she" | "it" | "they" => "they",
            x => panic!("'{x}' is not recognized as subjective"),
        }
    } else {
        match subject {
            "I" | "we" => "I",
            "you" if uc => "You",
            "thou" if uc => "Thou",
            "he" if uc => "He",
            "she" if uc => "She",
            "it" if uc => "It",
            "they" if uc => "It",
            "ye" if uc => "Thou",
            "you" => "you",
            "thou" | "ye" => "thou",
            "he" => "he",
            "she" => "she",
            "it" | "they" => "it",
            x => panic!("'{x}' is not recognized as subjective"),
        }
    }
}

/// Return the objective for a subjective. Can panic. see inflect_subjective_as_nr().
pub fn objective(subject: &str, uc: bool) -> &str {
    match subject {
        "I" if uc => "Me",
        "you" if uc => "You",
        "he" if uc => "Him",
        "she" if uc => "Her",
        "it" if uc => "It",
        "we" if uc => "Us",
        "ye" if uc => "You",
        "they" if uc => "Them",
        "thou" if uc => "Thee",
        "I" => "me",
        "thou" => "thee",
        "he" => "him",
        "she" => "her",
        "it" => "it",
        "we" => "us",
        "you" | "ye" => "you",
        "they" => "them",
        x => panic!("'{x}' is not recognized as subjective"),
    }
}

/// singular-/pluralize subjective according to nr
pub fn inflect_objective(subject: &str, to_plural: bool, uc: bool) -> &str {
    objective(inflect_subjective(subject, to_plural, false), uc)
}

/// Return the objective for a subjective. Can panic. see inflect_subjective_as_nr().
pub fn possesive(subject: &str, uc: bool) -> &str {
    match subject {
        "I" if uc => "My",
        "you" if uc => "Your",
        "he" if uc => "His",
        "she" if uc => "Her",
        "it" if uc => "Its",
        "we" if uc => "Our",
        "ye" if uc => "Your",
        "they" if uc => "Their",
        "thou" if uc => "Thy",
        "I" => "my",
        "thou" => "thy",
        "he" => "his",
        "she" => "her",
        "it" => "its",
        "we" => "our",
        "you" | "ye" => "your",
        "they" => "their",
        x => panic!("'{x}' is not recognized as subjective"),
    }
}

/// singular-/pluralize subjective according to nr
pub fn inflect_possesive(subject: &str, to_plural: bool, uc: bool) -> &str {
    possesive(inflect_subjective(subject, to_plural, false), uc)
}

/// Return the adjective for a subjective. Can panic. see inflect_subjective_as_nr().
pub fn adjective(subject: &str, uc: bool) -> &str {
    match subject {
        "I" if uc => "Mine",
        "you" if uc => "Yours",
        "ye" if uc => "Yours",
        "he" if uc => "His",
        "she" if uc => "Hers",
        "it" if uc => "Its",
        "we" if uc => "Ours",
        "they" if uc => "Theirs",
        "I" => "mine",
        "you" | "ye" => "yours",
        "he" => "his",
        "she" => "hers",
        "it" => "its",
        "we" => "ours",
        "they" => "theirs",
        "thou" => "thine",
        x => panic!("'{x}' is not recognized as subjective"),
    }
}

/// singular-/pluralize subjective according to nr
pub fn inflect_adjective(subject: &str, to_plural: bool, uc: bool) -> &str {
    adjective(inflect_subjective(subject, to_plural, false), uc)
}

pub fn inflect_verb(subject: &str, verb: &str, to_plural: bool, uc: bool) -> String {
    let res = match inflect_subjective(subject, to_plural, false) {
        "I" => match verb.trim() {
            "'re" => "'m".to_string(),
            "are" => "am".to_string(),
            "were" => "was".to_string(),
            "aren't" => "am not".to_string(),
            "weren't" => "wasn't".to_string(),
            v => v.to_string(),
        },
        "you" | "we" | "they" | "ye" | "thou" => verb.to_string(),
        _ => match verb.trim() {
            "'re" | "'ve" => "'s".to_string(),
            "are" => "is".to_string(),
            "have" => "has".to_string(),
            "were" => "was".to_string(),
            "do" => "does".to_string(),
            "aren't" => "isn't".to_string(),
            "weren't" => "wasn't".to_string(),
            "don't" => "doesn't".to_string(),
            "could" | "would" | "can" | "may" | "might" | "must" | "should" | "shall" | "will"
            | "had" | "couldn't" | "wouldn't" | "can't" | "mightn't" | "mustn't" | "shouldn't"
            | "hadn't" => verb.to_string(),
            v => {
                if v.ends_with(&['s', 'o', 'x']) || v.ends_with("ch") || v.ends_with("sh") {
                    format!("{}es", v)
                } else if let Some(p) = v
                    .strip_suffix('y')
                    .filter(|p| !p.ends_with(&['a', 'e', 'u', 'o']))
                {
                    format!("{}ies", p)
                } else {
                    format!("{}s", v)
                }
            }
        },
    };
    if uc {
        uc_1st(res.as_str())
    } else {
        res
    }
}

/// singular-/pluralize noun name according to nr
pub fn inflect_noun(name: String, is_default_plural: bool, as_plural: bool, uc: bool) -> String {
    let res = if as_plural == is_default_plural {
        name
    } else if as_plural {
        to_plural(name.as_str())
    } else {
        to_singular(name.as_str())
    };
    if uc {
        uc_1st(res.as_str())
    } else {
        res
    }
}

pub fn match_article_to_nr(nr: i64, default: &str, lc_art: &str, uc: bool) -> String {
    match (nr, lc_art) {
        (0, "some") | (0, "a") | (0, "an") => format!("{}one", if uc { 'N' } else { 'n' }),
        (0, "these") => format!("{}ero", if uc { 'Z' } else { 'z' }),
        (0, "those") => format!("{}o", if uc { 'N' } else { 'n' }),
        (1, "some") | (1, "a") | (1, "an") => {
            if uc {
                uc_1st(default)
            } else {
                default.to_string()
            }
        }
        (1, "these") => format!("{}his", if uc { 'T' } else { 't' }),
        (1, "those") => format!("{}hat", if uc { 'T' } else { 't' }),
        (_, "some") | (_, "a") | (_, "an") => format!("{}ome", if uc { 'S' } else { 's' }),
        (_, a) if a == "the" => format!("{}he", if uc { 'T' } else { 't' }),
        (_, a) if a == "those" => format!("{}hose", if uc { 'T' } else { 't' }),
        (_, a) if a == "these" => format!("{}hese", if uc { 'T' } else { 't' }),
        _ => panic!("Unimplemented article {lc_art}"),
    }
}

#[rustfmt::skip]
pub trait Ranting: std::fmt::Display {
    fn subjective(&self) -> &str;
    fn is_plural(&self) -> bool;
    fn name(&self, uc: bool) -> String;
    fn a_or_an(&self, uc: bool) -> &str;
}

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

/// Return the plural of a (subject) subjective; unchanged if already plural.
/// pancis for invalid subjective. valid are:
/// "I", "you", "thou", "he", "she", "it", "we", "ye", "they".
/// all must be lowercase except "I".
///
pub fn pluralize_subjective(subjective: &str, uc: bool) -> &str {
    match subjective {
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
}

/// Return the singular of a (subject) subjective; unchanged if already singular.
/// pancis for invalid subjective, see pluralize_subjective().
/// If the subjective is "they", the assumption is neutrum, which may be incorrect.
///
pub fn singularize_subjective(subjective: &str, uc: bool) -> &str {
    match subjective {
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

/// Return the objective for a subjective. Can panic. see pluralize_subjective().
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

pub fn pluralize_objective(subject: &str, uc: bool) -> &str {
    objective(pluralize_subjective(subject, false), uc)
}

pub fn singularize_objective(subject: &str, uc: bool) -> &str {
    objective(singularize_subjective(subject, false), uc)
}

/// Return the objective for a subjective. Can panic. see pluralize_subjective().
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

pub fn pluralize_possesive(subject: &str, uc: bool) -> &str {
    possesive(pluralize_subjective(subject, false), uc)
}

pub fn singularize_possessive(subject: &str, uc: bool) -> &str {
    possesive(singularize_subjective(subject, false), uc)
}

/// Return the adjective for a subjective. Can panic. see pluralize_subjective().
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

pub fn pluralize_adjective(subject: &str, uc: bool) -> &str {
    adjective(pluralize_subjective(subject, false), uc)
}

pub fn singularize_adjective(subject: &str, uc: bool) -> &str {
    adjective(singularize_subjective(subject, false), uc)
}

pub fn inflect_verb(subject: &str, verb: &str, trim_and_uc: Option<bool>) -> String {
    let res = match subject {
        "I" => match verb {
            "'re" => "'m".to_string(),
            " are" => " am".to_string(),
            " were" => " was".to_string(),
            " aren't" => " am not".to_string(),
            " weren't" => " wasn't".to_string(),
            v => v.to_string(),
        },
        "you" | "we" | "they" | "ye" | "thou" => verb.to_string(),
        _ => match verb {
            "'re" | "'ve" => "'s".to_string(),
            " are" => " is".to_string(),
            " have" => " has".to_string(),
            " were" => " was".to_string(),
            " do" => " does".to_string(),
            " aren't" => " isn't".to_string(),
            " weren't" => " wasn't".to_string(),
            " don't" => " doesn't".to_string(),
            " could" | " would" | " can" | " may" | " might" | " must" | " should" | " shall"
            | " will" | " had" | " couldn't" | " wouldn't" | " can't" | " mightn't"
            | " mustn't" | " shouldn't" | " hadn't" => verb.to_string(),
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
    if let Some(uc) = trim_and_uc {
        if uc {
            uc_1st(res.trim())
        } else {
            res.trim().to_string()
        }
    } else {
        res
    }
}

pub fn pluralize_verb(subject: &str, verb: &str, trim_and_uc: Option<bool>) -> String {
    inflect_verb(pluralize_subjective(subject, false), verb, trim_and_uc)
}

pub fn singularize_verb(subject: &str, verb: &str, trim_and_uc: Option<bool>) -> String {
    inflect_verb(pluralize_subjective(subject, false), verb, trim_and_uc)
}

/// pluralize name and of noun
pub fn if_pluralize_name(is_plural_by_default: bool, name: String) -> String {
    if is_plural_by_default {
        name
    } else {
        to_plural(name.as_str())
    }
}

/// singularize name and of noun
pub fn if_singularize_name(is_plural_by_default: bool, name: String) -> String {
    if is_plural_by_default {
        to_singular(name.as_str())
    } else {
        name
    }
}

/// singular-/pluralize noun name according to nr
pub fn pluralize_noun_as_nr(nr: i64, is_plural_by_default: bool, name: String) -> String {
    let is_multiple = nr != 1;
    if is_multiple == is_plural_by_default {
        name
    } else if is_multiple {
        to_plural(name.as_str())
    } else {
        to_singular(name.as_str())
    }
}

/// singular-/pluralize noun name according to nr
pub fn pluralize_subjective_as_nr(nr: i64, subject: &str, uc: bool) -> &str {
    let is_multiple = nr != 1;
    if is_multiple == is_subjective_plural(subject).unwrap_or(false) {
        subjective(subject, uc)
    } else if is_multiple {
        pluralize_subjective(subject, uc)
    } else {
        singularize_subjective(subject, uc)
    }
}

/// singular-/pluralize verb according to nr
pub fn pluralize_verb_as_nr(
    nr: i64,
    mut subject: &str,
    verb: &str,
    trim_and_uc: Option<bool>,
) -> String {
    let is_multiple = nr != 1;
    if is_subjective_plural(subject).unwrap_or(false) != is_multiple {
        if is_multiple {
            subject = pluralize_subjective(subject, false);
        } else {
            subject = singularize_subjective(subject, false);
        }
    }
    inflect_verb(subject, verb, trim_and_uc)
}

pub fn match_article_to_nr(x: i64, default: &str, lc_art: &str, uc: bool) -> String {
    match (x, lc_art) {
        (0, "some") => format!("{}one", if uc { 'N' } else { 'n' }),
        (0, "those") => format!("{}o", if uc { 'N' } else { 'n' }),
        (1, "some") => default.to_string(),
        (1, "these") => format!("{}his", if uc { 'T' } else { 't' }),
        (1, "those") => format!("{}hat", if uc { 'T' } else { 't' }),
        (_, y) if y == "the" => format!("{}he", if uc { 'T' } else { 't' }),
        (_, y) if y == "those" => format!("{}hose", if uc { 'T' } else { 't' }),
        (_, y) if y == "these" => format!("{}hese", if uc { 'T' } else { 't' }),
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

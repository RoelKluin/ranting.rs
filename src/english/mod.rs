// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.

use crate::uc_1st;

/// Return the adjective for a subject or panic.
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

/// Returns Ok(true) if the subjective is plural, an Error if unrecognized or indiscernible.
/// ```
///     use ranting::is_subjective_plural;
///     let subjective: &str = "we";
///     assert!(is_subjective_plural(subjective).expect("unsure"));
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

/// Return the subjective the same for subjective except the case can differ.
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

/// Inflect subjective to singular / pluralize according to nr
pub fn inflect_subjective(subject: &str, as_plural: bool, uc: bool) -> &str {
    if as_plural == is_subjective_plural(subject).unwrap_or(false) {
        subjective(subject, uc)
    } else if as_plural {
        if uc {
            match subject {
                "I" | "we" => "We",
                "he" | "she" | "it" | "they" => "They",
                "you" => "You",
                "thou" | "ye" => "Ye",
                x => panic!("'{x}' is not recognized as subjective"),
            }
        } else {
            match subject {
                "I" => "we",
                "he" | "she" | "it" => "they",
                "thou" => "ye",
                alt => alt,
            }
        }
    } else {
        match subject {
            "you" if uc => "You",
            "thou" if uc => "Thou",
            "he" if uc => "He",
            "she" if uc => "She",
            "it" if uc => "It",
            "they" if uc => "It",
            "ye" if uc => "Thou",
            "we" => "I",
            "they" => "it",
            "ye" => "thou",
            alt => alt,
        }
    }
}

/// Return the objective for a subjective. Can panic. see inflect_subjective().
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
        "we" => "us",
        "ye" => "you",
        "they" => "them",
        alt => alt,
    }
}

/// Return the objective for a subjective. Can panic. see inflect_subjective().
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

/// Given a subject and a verb, inflect it and to_upper() it as specified.
pub fn inflect_verb(subject: &str, verb: &str, as_plural: bool, uc: bool) -> String {
    let trimmed = verb.trim();
    let res = match inflect_subjective(subject, as_plural, false) {
        "I" => match trimmed {
            "'re" => "'m".to_string(),
            "are" => "am".to_string(),
            "were" => "was".to_string(),
            "aren't" => "am not".to_string(),
            "weren't" => "wasn't".to_string(),
            v => v.to_string(),
        },
        "you" | "we" | "they" | "ye" | "thou" => trimmed.to_string(),
        _ => match trimmed {
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
            | "hadn't" => trimmed.to_string(),
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

/// Given an article, the default, a requested one, inflect and to_upper() it as specified.
pub fn inflect_article(default: &str, requested: &str, as_plural: bool, uc: bool) -> String {
    match (as_plural, requested) {
        (_, "the") => format!("{}he", if uc { 'T' } else { 't' }),
        (false, "some") | (false, "a") | (false, "an") => {
            if uc {
                uc_1st(default)
            } else {
                default.to_string()
            }
        }
        (false, "these") => format!("{}his", if uc { 'T' } else { 't' }),
        (false, "those") => format!("{}hat", if uc { 'T' } else { 't' }),
        (true, "some") | (_, "a") | (_, "an") => format!("{}ome", if uc { 'S' } else { 's' }),
        (true, "those") => format!("{}hose", if uc { 'T' } else { 't' }),
        (true, "these") => format!("{}hese", if uc { 'T' } else { 't' }),
        _ => panic!("Unimplemented article {requested}"),
    }
}

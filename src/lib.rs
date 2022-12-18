// (c) Roel Kluin 2022 GPL v3

pub use in_definite;
pub use inflector;
pub use inflector::cases::sentencecase::to_sentence_case;
pub use inflector::string::pluralize::to_plural;
pub use inflector::string::singularize::to_singular;

/// upper cases first character if uc is true, otherwise lowers it.
pub fn uc_1st_if(s: &str, uc: bool) -> String {
    let mut c = s.chars();
    c.next()
        .map(|f| {
            if uc {
                f.to_uppercase().collect::<String>()
            } else {
                f.to_lowercase().collect::<String>()
            }
        })
        .unwrap_or_default()
        + c.as_str()
}

/// Returns Ok(true) if the pronoun is plural, an Error if unrecognized or indiscernible.
/// ```
///     use ranting::is_pronoun_plural;
///     let pronoun: &str = "we";
///     assert!(is_pronoun_plural(pronoun)?);
///
/// ```
///
pub fn is_pronoun_plural(pronoun: &str) -> Result<bool, String> {
    match pronoun {
        "we" | "they" | "ye" => Ok(true),
        "you" => Err("'you' could be either singular or plural".to_string()),
        "I" | "she" | "he" | "it" | "thou" => Ok(false),
        x => Err(format!("'{x}' is not recognized as pronoun")),
    }
}

/// Return the plural of a (subject) pronoun; unchanged if already plural.
/// pancis for invalid pronouns. valid are:
/// "I", "you", "thou", "he", "she", "it", "we", "ye", "they".
/// all must be lowercase except "I".
///
pub fn pluralize_pronoun(pronoun: &str, uc: bool) -> &str {
    match pronoun {
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
        x => panic!("'{x}' is not recognized as pronoun"),
    }
}

/// Return the singular of a (subject) pronoun; unchanged if already singular.
/// pancis for invalid pronouns, see pluralize_pronoun().
/// If the pronoun is "they", the assumption is neutrum, which may be incorrect.
///
pub fn singularize_pronoun(pronoun: &str, uc: bool) -> &str {
    match pronoun {
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
        x => panic!("'{x}' is not recognized as pronoun"),
    }
}

/// Return the subjective teh same for pronoun except the case can differ.
pub fn subjective(pronoun: &str, uc: bool) -> &str {
    match pronoun {
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

/// Return the objective for a subjective pronoun. Can panic. see pluralize_pronoun().
pub fn objective(pronoun: &str, uc: bool) -> &str {
    match pronoun {
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
        p => panic!("Unimplemented: objective for '{}'", p),
    }
}

/// Return the objective for a subjective pronoun. Can panic. see pluralize_pronoun().
pub fn possesive(pronoun: &str, uc: bool) -> &str {
    match pronoun {
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
        p => panic!("Unimplemented: possesive for '{}'", p),
    }
}

/// Return the adjective for a subjective pronoun. Can panic. see pluralize_pronoun().
pub fn adjective(pronoun: &str, uc: bool) -> &str {
    match pronoun {
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
        p => panic!("Unimplemented adjective for '{}'", p),
    }
}

pub fn inflect_verb(pronoun: &str, verb: &str) -> String {
    match pronoun {
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
    }
}

/// pluralize name and of noun and use the specified upper/lower case
pub fn pluralize_case(is_plural: bool, name: String) -> String {
    if is_plural {
        name
    } else {
        to_plural(name.as_str())
    }
}

/// singularize name and of noun and use the specified case
pub fn singularize_case(is_plural: bool, name: String) -> String {
    if is_plural {
        to_singular(name.as_str())
    } else {
        name
    }
}

/// singular-/pluralize noun name according to nr and use the specified case
pub fn pluralize_noun_as_nr(nr: i64, is_plural: bool, name: String) -> String {
    let is_multiple = nr != 1;
    if is_multiple == is_plural {
        name
    } else if is_plural {
        to_singular(name.as_str())
    } else {
        to_plural(name.as_str())
    }
}

/// singular-/pluralize verb according to nr and use the specified case
pub fn pluralize_verb_as_nr(nr: i64, mut pronoun: &str, verb: &str) -> String {
    let is_multiple = nr != 1;
    if is_pronoun_plural(pronoun).unwrap_or(false) != is_multiple {
        if is_multiple {
            pronoun = singularize_pronoun(pronoun, false);
        } else {
            pronoun = pluralize_pronoun(pronoun, false);
        }
    }
    inflect_verb(pronoun, verb)
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
    fn pronoun(&self) -> &str;
    fn is_plural(&self) -> bool;
    fn name(&self, uc: bool) -> String;
    fn a_or_an(&self, uc: bool) -> &str;
}

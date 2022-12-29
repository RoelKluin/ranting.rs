// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.

// TODO: make this a feature.
use crate::uc_1st_if;

#[cfg(feature = "inflector")]
use inflector::string::{pluralize::to_plural, singularize::to_singular};

/// can convert a `'s` or `'` after a noun to `'` or `'s` as appropriate for singular or plural.
pub fn adapt_possesive_s<'a, S: AsRef<str> + 'a>(
    name: S,
    is_default_plural: bool,
    asked_plural: bool,
) -> &'a str {
    let name = name.as_ref();
    if !asked_plural
        || name.contains(|c: char| c.is_ascii_uppercase())
        || (if is_default_plural == asked_plural {
            !name.ends_with('s')
        } else {
            !inflect_name(name, true).ends_with('s')
        })
    {
        "'s"
    } else {
        "'"
    }
}

/// Given an article, the default, a requested one, inflect and to_upper() it as specified.
pub fn adapt_article(default: &str, requested: &str, as_plural: bool, uc: bool) -> String {
    match (as_plural, requested) {
        (_, "the") => format!("{}he", if uc { 'T' } else { 't' }),
        (false, "some") | (false, "a") | (false, "an") => uc_1st_if(default, uc),
        (false, "these") => format!("{}his", if uc { 'T' } else { 't' }),
        (false, "those") => format!("{}hat", if uc { 'T' } else { 't' }),
        (true, "some") | (_, "a") | (_, "an") => format!("{}ome", if uc { 'S' } else { 's' }),
        (true, "those") => format!("{}hose", if uc { 'T' } else { 't' }),
        (true, "these") => format!("{}hese", if uc { 'T' } else { 't' }),
        _ => panic!("Unimplemented article {requested}"),
    }
}

/// Return the adjective for a subject or panic.
pub(crate) fn adjective(subject: &str, uc: bool) -> &str {
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

/// Inflect a subject pronoun to singular or plural and uppercase first character as indicated
///
/// # Example
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn get_subject(word: Noun) -> String {
///     say!("{:word are} - for {word}.")
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "she", "he", "it", "we", "they"]
///     .iter()
///     .map(|s| get_subject(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "I am - for subject I.\
///     You are - for subject you.\
///     She is - for subject she.\
///     He is - for subject he.\
///     It is - for subject it.\
///     We are - for subject we.\
///     They are - for subject they."
///     .to_string());
/// # }
/// ```
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
pub(crate) fn objective(subject: &str, uc: bool) -> &str {
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
pub(crate) fn possesive(subject: &str, uc: bool) -> &str {
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
/// Inflect a subject pronoun to singular or plural and uppercase first character as indicated
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn deny(w: Noun) -> String {
///     say!("{:w don't}, {:w can't}, {:?w won't} {:?w mustn't} {:?w haven't} {:?w weren't}. ")
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "it", "we", "they"]
///     .iter()
///     .map(|s| deny(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "I don't, I can't, won't mustn't haven't wasn't. \
///     You don't, you can't, won't mustn't haven't weren't. \
///     It doesn't, it can't, won't mustn't hasn't wasn't. \
///     We don't, we can't, won't mustn't haven't weren't. \
///     They don't, they can't, won't mustn't haven't weren't. "
///     .to_string());
/// # }
/// ```
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn allow(w: Noun) -> String {
///     say!("{:w're} {can :?w} {:?w see} {:?w may} {do :w}, {:w've}, {:w were}. ")
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "he", "we", "they"]
///     .iter()
///     .map(|s| allow(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "I'm can see may do I, I've, I was. \
///     You're can see may do you, you've, you were. \
///     He's can sees may does he, he's, he was. \
///     We're can see may do we, we've, we were. \
///     They're can see may do they, they've, they were. "
///     .to_string());
/// # }
/// ```
///
pub fn inflect_verb(subject: &str, verb: &str, as_plural: bool, uc: bool) -> String {
    let verb = verb.trim();

    let (part, post) = verb
        .strip_suffix("n't")
        .map_or((verb, ""), |start| (start, "n't"));

    match inflect_subjective(subject, as_plural, false) {
        "I" => match part {
            "'re" => format!("'{}", if uc { 'M' } else { 'm' }),
            "are" => match post {
                "n't" => format!("{}in't", if uc { 'A' } else { 'a' }),
                _ => format!("{}m", if uc { 'A' } else { 'a' }),
            },
            "were" => format!("{}as{post}", if uc { 'W' } else { 'w' }),
            _ => uc_1st_if(verb, uc),
        },
        "you" | "we" | "they" | "ye" | "thou" => uc_1st_if(verb, uc),
        _ => match part {
            "'re" | "'ve" => format!("'{}", if uc { 'S' } else { 's' }),
            "are" => format!("{}s{post}", if uc { 'I' } else { 'i' }),
            "have" => format!("{}as{post}", if uc { 'H' } else { 'h' }),
            "were" => format!("{}as{post}", if uc { 'W' } else { 'w' }),
            "do" => format!("{}oes{post}", if uc { 'D' } else { 'd' }),
            "ca" | "wo" | "had" | "could" | "would" | "should" | "might" | "must" | "can"
            | "may" | "shall" | "will" | "'d" => uc_1st_if(part, uc) + post,
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

/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn pluralize(w: &Noun) -> String {
///     let ct = 2;
///     say!("{+w do} or {^don't #0 w}? More of {Some w}.", ct)
/// }
///
/// fn singularize(w: &Noun) -> String {
///     say!("{,-:w do} or {don't #0 w}? Less of {Some w}.", 1)
/// }
/// # #[cfg(feature = "inflector")]
/// # fn main() {
/// let one = Noun::new("ox", "it");
///
/// assert_eq!(
///     pluralize(&one),
///     "Oxen do or Don't 2 oxen? More of An ox.".to_string()
/// );
///
/// let two = Noun::new("foo_bars", "they");
/// assert_eq!(
///     singularize(&two),
///     "it does or doesn't 1 foo_bar? Less of Some foo_bars.".to_string()
/// );
/// # }
/// ```
#[cfg(feature = "inflector")]
pub(crate) fn inflect_name(name: &str, as_plural: bool) -> String {
    if as_plural {
        to_plural(name)
    } else {
        to_singular(name)
    }
}

#[cfg(not(feature = "inflector"))]
pub(crate) fn inflect_name(_name: &str, _as_plural: bool) -> String {
    panic!("Inflection requires the \"inflector\" feature.");
}

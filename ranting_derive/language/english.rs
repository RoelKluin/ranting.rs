// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.

#[cfg(feature = "inflector")]
use inflector::string::{pluralize::to_plural, singularize::to_singular};

// regex to capture the placholders or sentence ends
// useful: https://regex101.com/r/Ly7O1x/3/
/// The components captured in a Ranting trait placeholder are defined here.
pub(crate) static RANTING_PLACEHOLDER: &str = r"(?x)
(?P<sentence>(?:[.?!]\s+)?+)  # sentence always captures: to obtain the placeholder offset.
\{
    (?P<uc>[,^])?+
    (?:
        (?P<pre>[aA]n?|[sS]ome|[tT]h(?:[eo]s)?e|
        '[rv]e|[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
        (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)?+
        (?P<etc1>(?:\s+[\w-]+)+?)??
        (?P<sp1>\s+)
    )?+
    (?:(?P<plurality>[+-]|\??\#(?P<nr>\w+)(?P<sp2>\s+)))?+
    (?P<case>(?:[`:@~*?]|<[^>]*>))?+
    (?P<noun>[\w-]+)
    (?:
        (?P<sp3>\s+)(?P<etc2>(?:[\w-]+\s+)+?)??(?P<post1>(?:[\w-]+')?[\w-]+)?|
        (?P<post2>'\w*)
    )?
    (?P<fmt>:[^}]+)?+
\}";

/// upper cases first character if uc is true, or second in a contraction.
///
/// # Example
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn upper(w: Noun) -> String {
///     say!("{:?w're} {the w}? {:?w'd} say for {`w}self! {:?w've} got here all of {@w}. ")
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "she", "they"]
///     .iter()
///     .map(|s| upper(Noun::new("one", s)))
///     .collect::<String>(),
///     "'M the one? 'D say for myself! 'Ve got here all of me. \
///     'Re the one? 'D say for yourself! 'Ve got here all of you. \
///     'S the one? 'D say for herself! 'S got here all of her. \
///     'Re the one? 'D say for theirself! 'Ve got here all of them. "
///     .to_string());
/// # }
/// ```
pub fn uc_1st_if(s: &str, uc: bool) -> String {
    if uc {
        let mut c = s.chars();
        c.next()
            .map(|t| match t {
                '\'' => {
                    t.to_string()
                        + &c.next()
                            .map(|c| c.to_uppercase().collect::<String>())
                            .unwrap_or_default()
                }
                _ => t.to_uppercase().collect::<String>(),
            })
            .unwrap_or_default()
            + c.as_str()
    } else {
        s.to_string()
    }
}

/// Return the case for a character.
pub(crate) fn get_case_from_str(s: &str) -> Option<String> {
    match s {
        ":" => Some("subjective".to_string()),
        "@" => Some("objective".to_string()),
        "`" => Some("possesive".to_string()),
        "~" => Some("adjective".to_string()),
        "*" => None,
        _ => Some(s.trim_start_matches('<').to_string()),
    }
}

/// Return whether a word is one of `some` `a` `an` `the` `these` `those`
pub fn is_article_or_so(word: &str) -> bool {
    matches!(word, "some" | "a" | "an" | "the" | "these" | "those")
}

/// can convert a `'s` or `'` after a noun as appropriate for singular or plural.
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

// In English singular possesive s i always the same.
pub(crate) fn adapt_possesive_s_wo_subj(c: char) -> Option<char> {
    if c == '-' {
        Some('\'')
    } else {
        None
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
        "he" | "she" | "it" => match part {
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
        _ => uc_1st_if(verb, uc),
    }
}

// In English verbs are the same if 1st, 2nd or 3rd person plural.
pub(crate) fn inflect_verb_wo_subj(verb: &str, c: char, uc: bool) -> Option<String> {
    if c == '+' {
        Some(inflect_verb("we", verb, true, uc))
    } else {
        None
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

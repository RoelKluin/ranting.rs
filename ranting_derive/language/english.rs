// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.
use strum_macros::EnumString;

#[cfg(any(feature = "inflector", feature = "debug"))]
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

#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum SubjectPronoun {
    #[strum(serialize = "I")]
    I,
    You,
    Thou,
    He,
    She,
    It,
    We,
    Ye,
    They,
}

static SUBJECTIVE_PRONOUN: [[&str; 9]; 2] = [
    ["I", "you", "thou", "he", "she", "it", "we", "ye", "they"],
    ["I", "You", "Thou", "He", "She", "It", "We", "Ye", "They"],
];

static OBJECTIVE_PRONOUN: [[&str; 9]; 2] = [
    ["me", "you", "thee", "him", "her", "it", "us", "you", "them"],
    ["Me", "You", "Thee", "Him", "Her", "It", "Us", "You", "Them"],
];

static POSSESIVE_PRONOUN: [[&str; 9]; 2] = [
    [
        "my", "your", "thy", "his", "her", "its", "our", "your", "their",
    ],
    [
        "My", "Your", "Thy", "His", "Her", "Its", "Our", "Your", "Their",
    ],
];

static ADJECTIVE_PRONOUN: [[&str; 9]; 2] = [
    [
        "mine", "yours", "thine", "his", "hers", "its", "ours", "yours", "theirs",
    ],
    [
        "Mine", "Yours", "Thine", "His", "Hers", "Its", "Ours", "Yours", "Theirs",
    ],
];

static IRREGULAR_VERBS_1ST: [&str; 4] = ["am", "aint", "was", "'m"];
static IRREGULAR_VERBS_3RD: [&str; 5] = ["is", "was", "'s", "has", "does"];

#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum IrregularPluralVerb {
    Are,
    Were,
    #[strum(serialize = "'re")]
    Re,
    #[strum(serialize = "'ve")]
    Ve,
    #[strum(serialize = "'d")]
    D,
    Have,
    Do,
    Had,
    Could,
    Would,
    Should,
    Might,
    Must,
    Can,
    May,
    Shall,
    Will,
}

#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum ArticleOrSo {
    The,
    #[strum(serialize = "a", serialize = "an", serialize = "some")]
    A,
    These,
    Those,
}static ARTICLE_OR_SO: [[&str; 8]; 2] = [
    ["a", "an", "this", "that", "the", "some", "these", "those"],
    ["A", "An", "This", "That", "The", "Some", "These", "Those"],
];/// upper cases first character if uc is true, or second in a contraction.
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
    (c == '-').then_some('\'')
}

pub fn is_indefinite_article(article_or_so: &str) -> bool {
    match article_or_so {
        "some" | "a" | "an" => true,
        _ => false,
    }
}

/// Given an article, the default, a requested one, inflect and to_upper() it as specified.
pub fn adapt_article(default: &str, requested: &str, as_plural: bool, uc: bool) -> String {
    match (as_plural, requested) {
        (_, "the") => format!("{}he", if uc { 'T' } else { 't' }),
        (false, "some") | (false, "a") | (false, "an") => uc_1st_if(default, uc),
        (false, "these") => format!("{}his", if uc { 'T' } else { 't' }),
        (false, "those") => format!("{}hat", if uc { 'T' } else { 't' }),
        (true, "some") | (true, "a") | (true, "an") => format!("{}ome", if uc { 'S' } else { 's' }),
        (true, "those") => format!("{}hose", if uc { 'T' } else { 't' }),
        (true, "these") => format!("{}hese", if uc { 'T' } else { 't' }),
        _ => panic!("Unimplemented article {requested}"),
    }
}

/// Return the adjective for a subject or panic.
pub(crate) fn adjective(subject: SubjectPronoun, uc: bool) -> &'static str {
    ADJECTIVE_PRONOUN[uc as usize][subject as usize]
}

/// Returns Ok(true) if the subjective is plural, an Error if unrecognized or indiscernible.
/// ```
/// # use std::str::FromStr;
/// # use ranting::*;
///
/// # fn main() {
/// for subject in ["I", "you", "thou", "she", "he", "it"]
///     .into_iter()
///     .map(SubjectPronoun::from_str)
/// {
///     assert!(!is_subjective_plural(subject.unwrap()));
/// }
/// for subject in ["we", "ye", "they"]
///     .into_iter()
///     .map(SubjectPronoun::from_str)
/// {
///     assert!(is_subjective_plural(subject.unwrap()));
/// }
/// # }
/// ```
pub fn is_subjective_plural(subjective: SubjectPronoun) -> bool {
    (subjective as usize) >= 6
}

/// Return the subjective the same for subjective except the case can differ.
pub fn subjective(subject: SubjectPronoun, uc: bool) -> &'static str {
    SUBJECTIVE_PRONOUN[uc as usize][subject as usize]
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
pub fn pluralize_pronoun(subject: SubjectPronoun, as_plural: bool) -> SubjectPronoun {
    if as_plural == is_subjective_plural(subject) {
        subject
    } else if as_plural {
        match subject {
            SubjectPronoun::I => SubjectPronoun::We,
            SubjectPronoun::Thou => SubjectPronoun::Ye,
            SubjectPronoun::He | SubjectPronoun::She | SubjectPronoun::It => SubjectPronoun::They,
            x => x,
        }
    } else {
        match subject {
            SubjectPronoun::We => SubjectPronoun::I,
            SubjectPronoun::Ye => SubjectPronoun::Thou,
            SubjectPronoun::They => SubjectPronoun::It,
            x => x,
        }
    }
}

/// singular-/pluralize subjective according to nr
pub fn inflect_subjective(subject: SubjectPronoun, as_plural: bool, uc: bool) -> &'static str {
    let subject = pluralize_pronoun(subject, as_plural);
    SUBJECTIVE_PRONOUN[uc as usize][subject as usize]
}

/// Return the objective for a subjective. Can panic. see pluralize_pronoun().
pub(crate) fn objective(subject: SubjectPronoun, uc: bool) -> &'static str {
    OBJECTIVE_PRONOUN[uc as usize][subject as usize]
}

/// Return the objective for a subjective. Can panic. see pluralize_pronoun().
pub(crate) fn possesive(subject: SubjectPronoun, uc: bool) -> &'static str {
    POSSESIVE_PRONOUN[uc as usize][subject as usize]
}

/// Given a subject and a verb, inflect it and to_upper() it as specified.
/// Inflect a subject pronoun to singular or plural and uppercase first character as indicated
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn deny(w: Noun) -> String {
///     say!("{:w don't}, {:w can't}, {:?w won't}, {:?w mustn't}, {:?w haven't}, {:?w weren't}, or {weren't :w}? ")
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "it", "we", "they"]
///     .iter()
///     .map(|s| deny(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "I don't, I can't, won't, mustn't, haven't, wasn't, or wasn't I? \
///     You don't, you can't, won't, mustn't, haven't, weren't, or weren't you? \
///     It doesn't, it can't, won't, mustn't, hasn't, wasn't, or wasn't it? \
///     We don't, we can't, won't, mustn't, haven't, weren't, or weren't we? \
///     They don't, they can't, won't, mustn't, haven't, weren't, or weren't they? "
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
pub fn inflect_verb(subject: SubjectPronoun, verb: &str, as_plural: bool, uc: bool) -> String {
    let verb = verb.trim();

    let (part, post) = verb
        .strip_suffix("n't")
        .map_or((verb, ""), |start| (start, "n't"));

    match pluralize_pronoun(subject, as_plural) {
        SubjectPronoun::I => match part {
            "'re" => format!("'{}", if uc { 'M' } else { 'm' }),
            "are" => match post {
                "n't" => format!("{}in't", if uc { 'A' } else { 'a' }),
                _ => format!("{}m", if uc { 'A' } else { 'a' }),
            },
            "were" => format!("{}as{post}", if uc { 'W' } else { 'w' }),
            _ => uc_1st_if(verb, uc),
        },
        SubjectPronoun::He | SubjectPronoun::She | SubjectPronoun::It => match part {
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
    (c == '+').then_some(inflect_verb(SubjectPronoun::We, verb, true, uc))
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
/// # #[cfg(any(feature = "inflector", feature = "debug"))]
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
#[cfg(any(feature = "inflector", feature = "debug"))]
pub(crate) fn inflect_name(name: &str, as_plural: bool) -> String {
    if as_plural {
        to_plural(name)
    } else {
        to_singular(name)
    }
}

#[cfg(not(any(feature = "inflector", feature = "debug")))]
pub(crate) fn inflect_name(_name: &str, _as_plural: bool) -> String {
    panic!("Inflection requires the \"inflector\" feature.");
}

// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.
use std::fmt;
use std::str::FromStr;
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

static SUBJECTIVE_PRONOUN: [&str; 9] = ["I", "you", "thou", "he", "she", "it", "we", "ye", "they"];

static OBJECTIVE_PRONOUN: [&str; 9] =
    ["me", "you", "thee", "him", "her", "it", "us", "you", "them"];

static POSSESIVE_PRONOUN: [&str; 9] = [
    "my", "your", "thy", "his", "her", "its", "our", "your", "their",
];

static ADJECTIVE_PRONOUN: [&str; 9] = [
    "mine", "yours", "thine", "his", "hers", "its", "ours", "yours", "theirs",
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
    Have,
    Do,
    #[strum(serialize = "'d")]
    D,
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
    Ca, // for can't
    Wo, // for won't
}

#[derive(EnumString, PartialEq, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum ArticleOrSo {
    The,
    #[strum(serialize = "a", serialize = "an", serialize = "some")]
    A,
    These,
    Those,
}
static ARTICLE_OR_SO: [&str; 8] = ["the", "some", "these", "those", "a", "an", "this", "that"];

/// A word that may be uppercased and has a base and separate extension. (e.g. plural for a verb)
pub struct ExtCased<'a> {
    s: &'a str,
    uc: bool,
    ext: &'a str,
}

impl<'a> fmt::Display for ExtCased<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cased = Cased {
            s: self.s,
            uc: self.uc,
        };
        write!(f, "{}{}", cased, self.ext)
    }
}

/// A word that may be uppercased - without a string copy.
pub struct Cased<'a> {
    s: &'a str,
    uc: bool,
}

impl<'a> fmt::Display for Cased<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.uc {
            let mut it = self.s.chars();
            let fst = it.next().ok_or(fmt::Error)?;
            if fst.is_ascii_punctuation() {
                // in case of a contraction, lowercase the next character.
                let snd = it.next().ok_or(fmt::Error)?;
                let offs = fst.len_utf8() + snd.len_utf8();
                let etc = self.s.get(offs..).unwrap_or("");
                write!(f, "{fst}{}{etc}", snd.to_uppercase())
            } else {
                let etc = self.s.get(fst.len_utf8()..).unwrap_or("");
                write!(f, "{}{etc}", fst.to_uppercase())
            }
        } else {
            write!(f, "{}", self.s)
        }
    }
}

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
pub(crate) fn get_case_from_str(s: &str) -> Option<&str> {
    match s {
        ":" => Some("subjective"),
        "@" => Some("objective"),
        "`" => Some("possesive"),
        "~" => Some("adjective"),
        "*" => None,
        _ => Some(s.trim_start_matches('<')),
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
    matches!(article_or_so, "some" | "a" | "an")
}

/// Given an article, the default, a requested one, inflect and to_upper() it as specified.
pub fn adapt_article<'a>(
    mut s: &'a str,
    requested: &'a str,
    as_plural: bool,
    uc: bool,
) -> Cased<'a> {
    match ArticleOrSo::from_str(requested).expect("Not an article") {
        t if t == ArticleOrSo::The || as_plural => s = ARTICLE_OR_SO[t as usize],
        ArticleOrSo::A => {}
        t => s = ARTICLE_OR_SO[(t as usize) + 4],
    }
    Cased { s, uc }
}

/// Return the adjective for a subject or panic.
pub(crate) fn adjective<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = ADJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
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
pub fn subjective<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = SUBJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
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
pub fn inflect_subjective<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    let subject = pluralize_pronoun(subject, as_plural);
    let s = SUBJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
}

/// Return the objective for a subjective. Can panic. see pluralize_pronoun().
pub(crate) fn objective<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = OBJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
}

/// Return the objective for a subjective. Can panic. see pluralize_pronoun().
pub(crate) fn possesive<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = POSSESIVE_PRONOUN[subject as usize];
    Cased { s, uc }
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
pub fn inflect_verb(subject: SubjectPronoun, verb: &str, as_plural: bool, uc: bool) -> ExtCased {
    let verb = verb.trim();

    let (mut s, mut ext) = verb
        .strip_suffix("n't")
        .map_or((verb, ""), |start| (start, "n't"));

    match pluralize_pronoun(subject, as_plural) {
        SubjectPronoun::I => match IrregularPluralVerb::from_str(s) {
            Ok(IrregularPluralVerb::Are) if ext != "n't" => {
                s = IRREGULAR_VERBS_1ST[0];
            }

            Ok(e) => {
                s = IRREGULAR_VERBS_1ST.get((e as usize) + 1).unwrap_or(&s);
            }
            _ => {}
        },
        SubjectPronoun::He | SubjectPronoun::She | SubjectPronoun::It => {
            if let Ok(mut val) = IrregularPluralVerb::from_str(s).map(|e| e as usize) {
                if val >= IrregularPluralVerb::Ve as usize {
                    val -= 1;
                }
                s = IRREGULAR_VERBS_3RD.get(val).unwrap_or(&s);
            } else if s.ends_with(&['s', 'o', 'x']) || s.ends_with("ch") || s.ends_with("sh") {
                ext = "es";
            } else if let Some(p) = s
                .strip_suffix('y')
                .filter(|p| !p.ends_with(&['a', 'e', 'u', 'o']))
            {
                s = p;
                ext = "ies";
            } else {
                ext = "s";
            }
        }
        _ => {}
    }
    ExtCased { s, ext, uc }
}

// In English verbs are the same if 1st, 2nd or 3rd person plural.
pub(crate) fn inflect_verb_wo_subj(verb: &str, c: char, uc: bool) -> Option<ExtCased> {
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

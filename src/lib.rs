// (c) Roel Kluin 2022 GPL v3
//!
//! Functions are used by [Ranting](https://docs.rs/ranting_derive/0.2.0/ranting_derive/) trait placeholders.
//!
//! ## Feature flags
#![doc = document_features::document_features!()]

extern crate self as ranting;

pub(crate) mod language;

pub use language::english_shared::{is_subjective_plural, SubjectPronoun};
pub use language::roman_shared::uc_1st_if;

use in_definite::get_a_or_an;
use language::english::{
    adapt_article, adjective, inflect_subjective, inflect_verb, objective, pluralize_pronoun,
    possesive,
};
use language::roman_shared::{Cased, ExtCased};
use std::str::FromStr;

// TODO: make this a feature:
//pub(crate) use strum_macros;

/// A wrapper for `return Ok(say!())`
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, ack, Ranting};
/// fn question(harr: Noun, friends: Noun, lad: Noun) -> Result<String, String> {
///     ack!("{harr shall} {+=friends do} with {the drunken *lad}?");
/// }
///
/// # fn main() {
/// let harr = Noun::new("what", "it");
/// let friends = Noun::new("crew", "we");
/// let lad = Noun::new("sailor", "he");
///
/// assert_eq!(
///     question(harr, friends, lad),
///     Ok("What shall we do with the drunken sailor?".to_string())
/// );
/// # }
/// ```
pub use ranting_derive::ack;

/// A wrapper for `return Err(say!())`
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, nay, Ranting};
/// fn home(p: Noun) -> Result<String, String> {
///     nay!("{=p can't} get in {`p} house.")
/// }
///
/// # fn main() {
/// assert_eq!(
///     home(Noun::new("Jo", "she")),
///     Err("She can't get in her house.".to_string())
/// );
/// # }
/// ```
pub use ranting_derive::nay;

/// As `format!()` but allows inflection of Ranting trait elements in the placeholder. If not
/// to normal Display or Debug trait behavior.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn inflect(with: Noun) -> String {
///     let n = Noun::new("noun", "it");
///     say!("{Some n} with {0} {?n inflect} as {=0}, {@0}, {`0} and {~0}.", with)
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "he", "she", "it", "we", "they"]
///     .iter()
///     .map(|s| inflect(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "A noun with subject I inflects as I, me, my and mine.\
///     A noun with subject you inflects as you, you, your and yours.\
///     A noun with subject he inflects as he, him, his and his.\
///     A noun with subject she inflects as she, her, her and hers.\
///     A noun with subject it inflects as it, it, its and its.\
///     A noun with subject we inflects as we, us, our and ours.\
///     A noun with subject they inflects as they, them, their and theirs."
///     .to_string());
/// # }
/// ```
pub use ranting_derive::say;

/// The say macro parses placeholders and passes captures to here which returns a string.
pub fn handle_placeholder<R>(noun: &R, nr: String, mut uc: bool, caps: [&str; 5]) -> String
where
    R: Ranting,
{
    let [mut pre, plurality, noun_space, case, mut post] = caps;
    let mut res = String::new();
    let as_pl = match plurality {
        "" => noun.is_plural(),
        "+" => true,
        "-" => false,
        // FIXME this is hackish. What is we want to say("{a 1.0% increase are} not a lot")?
        _ => nr.trim() != "1",
    };

    let art_space;
    (pre, art_space) = split_at_find_end(pre, |c: char| !c.is_whitespace()).unwrap_or((pre, ""));

    let etc1;
    (pre, etc1) = split_at_find_start(pre, |c| c.is_whitespace()).unwrap_or((pre, ""));

    let post_space;
    (post_space, post) =
        split_at_find_start(post, |c: char| !c.is_whitespace()).unwrap_or(("", post));

    let etc2;
    (etc2, post) = split_at_find_end(post, |c| c.is_whitespace()).unwrap_or(("", post));

    let subjective = noun.subjective();

    // This may be an article or certain verbs that can occur before the noun:
    if !pre.is_empty() {
        let mut p = pre.to_lowercase();
        let mut optional_article = false;
        if let Some(s) = p.as_str().strip_prefix('?') {
            p = s.to_string();
            optional_article = true;
        }
        if is_article_or_so(p.as_str()) {
            let art = match p.as_str() {
                "a" | "an" | "some" | "the" if optional_article && noun.skip_article() => {
                    String::new()
                }
                "a" | "an" | "some" => {
                    let singular = noun.inflect(false, false);
                    get_a_or_an(singular.as_str()).to_string()
                }
                art => art.to_string(),
            };
            let a = ranting::adapt_article(art, p.as_str(), art_space, as_pl, uc);
            res.push_str(&a);
        } else {
            assert!(post.is_empty(), "verb before and after?");
            let verb = inflect_verb(subjective, p.as_str(), as_pl, uc);
            res.push_str(&format!("{verb}{art_space}"));
        }
        uc = false;
    }
    if !etc1.is_empty() {
        res.push_str(etc1);
    }
    if !plurality.contains('?') {
        res.push_str(&nr);
    }

    if case != "?" {
        res.push_str(noun_space);
        let s = match case {
            "=" => format!("{}", inflect_subjective(subjective, as_pl, uc)),
            "@" => format!("{}", inflect_objective(subjective, as_pl, uc)),
            "`" => format!("{}", inflect_possesive(subjective, as_pl, uc)),
            "~" => format!("{}", inflect_adjective(subjective, as_pl, uc)),
            _ => noun.inflect(as_pl, uc),
        };
        res.push_str(&s);
        res.push_str(post_space);
        uc = false;
    }
    res.push_str(etc2);
    if !post.is_empty() {
        match post {
            "'" | "'s" => {
                res.push_str(adapt_possesive_s(noun, as_pl));
            }
            v => {
                let verb = inflect_verb(subjective, v, as_pl, uc);
                res.push_str(&format!("{verb}"));
            }
        }
    }
    res
}

fn split_at_find_start(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.rfind(fun).map(|u| s.split_at(u + 1))
}

/// Return whether a word is one of `some` `a` `an` `the` `these` `those`
fn is_article_or_so(word: &str) -> bool {
    matches!(word, "some" | "a" | "an" | "the" | "these" | "those")
}

/// Has the Ranting trait. Instead you may want to `#[derive(Ranting)]` and maybe override a few
/// derived default functions. By setting name and subject to "$", these must come from the struct.
#[derive(ranting_derive::Ranting)]
#[ranting(name = "$", subject = "$")]
pub struct Noun {
    name: String,
    subject: SubjectPronoun,
}
impl Noun {
    pub fn new(name: &str, subject: &str) -> Self {
        Noun {
            name: name.to_string(),
            subject: SubjectPronoun::from_str(subject)
                .unwrap_or_else(|_| panic!("invalid subject pronoun: '{subject}'")),
        }
    }
}

/// convert to `'s` or `'` as appropriate for singular or plural of a noun.
///
/// # Examples
///
/// ```rust
/// # use ranting::*;
/// # fn main() {
///
/// let school = Noun::new("school", "it");
/// let principal = Noun::new("principal", "she");
/// let myles = Noun::new("Myles", "he");
///
/// assert_eq!(say!("{the school'} {principal are} also {myles'}, but only one of all {the +school's} {+principal} in town."),
///     "The school's principal is also Myles's, but only one of all the schools' principals in town.".to_string());
/// # }
/// ```
// a combined plural may require some tricks: "The star and cross' design was pattented by Bob."
fn adapt_possesive_s(noun: &dyn Ranting, asked_plural: bool) -> &str {
    if asked_plural && !is_name(noun) {
        "'"
    } else {
        "'s"
    }
}

/// Inflect adjective pronoun as to_plural indicates. The first character is a capital if uc is set.
fn inflect_adjective<'a>(subject: SubjectPronoun, to_plural: bool, uc: bool) -> Cased<'a> {
    adjective(pluralize_pronoun(subject, to_plural), uc)
}

fn is_name(noun: &dyn Ranting) -> bool {
    noun.name(false)
        .trim_start_matches('\'')
        .starts_with(|c: char| c.is_uppercase())
}

/// Inflect objective pronoun as to_plural indicates. The first character is a capital if uc is set.
fn inflect_objective<'a>(subject: SubjectPronoun, to_plural: bool, uc: bool) -> Cased<'a> {
    objective(pluralize_pronoun(subject, to_plural), uc)
}

/// Inflect possesive pronoun as to_plural indicates. The first character is a capital if uc is set.
fn inflect_possesive<'a>(subject: SubjectPronoun, to_plural: bool, uc: bool) -> Cased<'a> {
    possesive(pluralize_pronoun(subject, to_plural), uc)
}

/// ```
/// # use std::str::FromStr;
/// # use ranting::*;
/// # use ranting_derive::*;
///
/// #[derive_ranting]
/// #[ranting(subject = "you", is_plural = true)]
/// struct OpponentTeam {}
///
/// #[derive_ranting]
/// #[ranting(subject = "he")]
/// struct ChessPlayer {}
///
/// fn big_words_to<T: Ranting>(who: T) -> String {
///     say!("I will grant {@0} {`0} fight, but {=0 are} going to loose today.", who)
/// }
///
/// # fn main() {
///     let team = OpponentTeam {};
///     assert_eq!(big_words_to(team),
///         "I will grant you your fight, but you are going to loose today.");
///
///     let magnus = ChessPlayer {};
///
///     assert_eq!(big_words_to(magnus),
///         "I will grant him his fight, but he is going to loose today.");
/// # }
/// ```

/// By overriding functions one can adapt default behavior, which affects the
/// [placeholder](https://docs.rs/ranting_derive/0.2.0/ranting_derive/) behavior.
// TODO: add function for 'the': some words require an article to be printed.
// E.g. names, languages, elements, food grains, meals (unless particular), sports.
// Space after should then also be omitted.
#[rustfmt::skip]
pub trait Ranting: std::fmt::Display {
    fn subjective(&self) -> SubjectPronoun;
    fn is_plural(&self) -> bool;
    fn name(&self, uc: bool) -> String;
    fn skip_article(&self) -> bool;
    fn inflect(&self, to_plural: bool, uc: bool) -> String;
}

// (c) Roel Kluin 2022 GPL v3
//!
//! Functions are used by [Ranting](https://docs.rs/ranting_derive/0.2.0/ranting_derive/) trait placeholders.
//!
//! ## Feature flags
#![doc = document_features::document_features!()]

extern crate self as ranting;

#[path = "../ranting_derive/language/english.rs"]
#[allow(dead_code)]
mod english;
use english as language;
pub use language::{Cased, SubjectPronoun};

pub use in_definite;
//pub(crate) use ranting_derive::derive_ranting;
use std::str::FromStr;

// TODO: make this a feature:
//pub(crate) use strum_macros;

/// A wrapper for `return Ok(say!())`
///
/// # Examples
///
/// ```rust
/// # use ranting::{Object, Noun, ack, Ranting};
/// fn question(harr: Object, friends: Noun, lad: Noun) -> Result<String, String> {
///     ack!("{harr shall} {+=friends do} with {the drunken *lad}?");
/// }
///
/// # fn main() {
/// let harr = Object::new("what");
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
/// # use ranting::{Noun, Object, say, Ranting};
/// fn inflect(with: Noun) -> String {
///     let n = Object::new("noun");
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

/// Similar, but always neutrum.
#[derive(ranting_derive::Ranting)]
#[ranting(name = "$")]
pub struct Object {
    name: String,
}
impl Object {
    pub fn new(name: &str) -> Self {
        Object {
            name: name.to_string(),
        }
    }
}

pub use language::adapt_article;

/// convert to `'s` or `'` as appropriate for singular or plural of a noun.
///
/// # Examples
///
/// ```rust
/// # use ranting::*;
/// # fn main() {
///
/// let school = Object::new("school");
/// let principal = Noun::new("principal", "she");
/// let myles = Noun::new("Myles", "he");
///
/// assert_eq!(say!("{the school'} {principal are} also {myles'}, but only one of all {the +school's} {+principal} in town."),
///     "The school's principal is also Myles's, but only one of all the schools' principals in town.".to_string());
/// # }
/// ```
// a combined plural may require some tricks: "The star and cross' design was pattented by Bob."
pub fn adapt_possesive_s(noun: &dyn Ranting, asked_plural: bool) -> &str {
    if asked_plural && !is_name(noun) {
        "'"
    } else {
        "'s"
    }
}

/// singular-/pluralize adjective with as_plural and set uc to capitalize first character
pub fn inflect_adjective<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    adjective(pluralize_pronoun(subject, as_plural), uc)
}

/// retrieve singular-/pluralize noun name with as_plural and uc to capitalize first
pub fn inflect_noun(noun: &dyn Ranting, as_plural: bool, uc: bool) -> String {
    if noun.is_plural() == as_plural {
        noun.name(uc)
    } else {
        noun.inflect(as_plural, uc)
    }
}

fn is_name(noun: &dyn Ranting) -> bool {
    noun.name(false)
        .trim_start_matches('\'')
        .starts_with(|c: char| c.is_uppercase())
}

/// singular-/pluralize objective with as_plural and uc apitalizes first
pub fn inflect_objective<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    objective(pluralize_pronoun(subject, as_plural), uc)
}

/// singular-/pluralize possesive with as_plural and you can uc first
pub fn inflect_possesive<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    possesive(pluralize_pronoun(subject, as_plural), uc)
}

pub use language::inflect_subjective;
pub(crate) use language::pluralize_pronoun;

pub use language::inflect_verb;
pub use language::is_subjective_plural;
pub use language::uc_1st_if;

use language::adjective;
use language::objective;
use language::possesive;

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
    fn mut_name(&mut self, _word: &str) -> String;
    fn indefinite_article(&self, optional_article: bool, uc: bool) -> String;
    fn requires_article(&self) -> bool;
    fn inflect(&self, as_plural: bool, uc: bool) -> String;
}

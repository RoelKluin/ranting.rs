// (c) Roel Kluin 2022 GPL v3
//!
//! Functions are used by [Ranting](https://docs.rs/ranting_derive/0.1.2/ranting_derive/) trait placeholders.
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
/// # use ranting::{Noun, ack, Ranting};
/// fn question(harr: Noun, friends: Noun, lad: Noun) -> Result<String, String> {
///     ack!("{harr shall} {+:friends do} with {the drunken *lad}?");
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
///     nay!("{:p can't} get in {`p} house.")
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

/// As `format!()` but with inflection within placeholders for Ranting elements. Other elements
/// adhere to their Display or Debug traits.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn inflect(with: Noun) -> String {
///     let n = Noun::new("noun", "it");
///     say!("{Some n} with {0} {?n inflect} as {:0}, {@0}, {`0} and {~0}.", with)
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

/// A struct with the Ranting trait. Instead you may want to `#[derive(Ranting)]` and override a
/// few derived default functions.
// with all trait functions, derive works everywhere but here due to Ranting collision
#[derive(ranting_derive::Ranting)]
#[ranting(name = "", subject = "", lc = "true")]
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

pub use language::adapt_article;

/// can convert a `'s` or `'` after a noun as appropriate for singular or plural.
pub fn adapt_possesive_s(noun: &dyn Ranting, asked_plural: bool) -> &str {
    if !asked_plural || language::is_name_or_plural(noun.name(false).as_str(), noun.is_plural()) {
        "'s"
    } else {
        "'"
    }
}

/// singular-/pluralize adjective
pub fn inflect_adjective<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    adjective(pluralize_pronoun(subject, as_plural), uc)
}

/// retrieve singular-/pluralize noun name
pub fn inflect_noun(noun: &dyn Ranting, as_plural: bool, uc: bool) -> String {
    let name = noun.name(false);
    let is_default_plural = noun.is_plural();
    if is_default_plural == as_plural {
        uc_1st_if(name.as_ref(), uc)
    } else {
        let plural = language::inflect_name(name.as_ref(), as_plural);
        uc_1st_if(plural.as_str(), uc)
    }
}

/// retrieve singular-/plural of noun name, a command is passed along and its state may change.
pub fn mutate_noun(noun: &mut dyn Ranting, command: &str, as_plural: bool, uc: bool) -> String {
    let name = noun.mut_name(command);
    let is_default_plural = noun.is_plural();
    if is_default_plural == as_plural {
        uc_1st_if(name.as_ref(), uc)
    } else {
        let plural = language::inflect_name(name.as_ref(), as_plural);
        uc_1st_if(plural.as_str(), uc)
    }
}

/// singular-/pluralize objective
pub fn inflect_objective<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    objective(pluralize_pronoun(subject, as_plural), uc)
}

/// singular-/pluralize possesive
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

/// By overriding these one can adapt default behavior, which affects the
/// [placeholder](https://docs.rs/ranting_derive/0.1.2/ranting_derive/) interpretation.
// TODO: add function for 'the': some words require an article to be printed.
// E.g. names, languages, elements, food grains, meals (unless particular), sports.
// Space after should then also be omitted.
#[rustfmt::skip]
pub trait Ranting: std::fmt::Display {
    fn subjective(&self) -> SubjectPronoun;
    fn is_plural(&self) -> bool;
    fn name(&self, uc: bool) -> String;
    fn mut_name(&mut self, _word: &str) -> String;
    fn indefinite_article(&self, uc: bool) -> String;
    fn requires_article(&self) -> bool;
    fn inflect_noun(&self, as_plural: bool, uc: bool) -> String;
}

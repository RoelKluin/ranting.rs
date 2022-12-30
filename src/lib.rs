// (c) Roel Kluin 2022 GPL v3
//!
//! Functions are used by [Ranting](https://docs.rs/ranting_derive/0.1.2/ranting_derive/) trait placeholders.
//!
//! ## Feature flags
#![doc = document_features::document_features!()]

#[path = "../ranting_derive/language/english.rs"]
#[allow(dead_code)]
mod english;
use english as language;
pub use language::SubjectPronoun;

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

/// Basicly a struct with the Ranting trait. In general you would want to `#[derive(Ranting)]` instead
/// and override a few derived default functions.
// with all trait functions, derive works everywhere but here due to Ranting collision
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

impl Ranting for Noun {
    fn subjective(&self) -> SubjectPronoun {
        self.subject
    }
    fn is_plural(&self) -> bool {
        is_subjective_plural(self.subjective())
    }
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.name.as_str(), uc)
    }
    fn mut_name(&mut self, _word: &str) -> String {
        self.name(false)
    }
    fn requires_article(&self) -> bool {
        true
    }
    fn indefinite_article(&self, uc: bool) -> &str {
        if self.is_plural() {
            return if uc { "Some" } else { "some" };
        }
        match in_definite::get_a_or_an(self.name.as_str()) {
            "a" if uc => "A",
            "an" if uc => "An",
            lc => lc,
        }
    }
}
impl std::fmt::Display for Noun {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "{}", self.name)
    }
}

pub use language::adapt_article;
pub use language::adapt_possesive_s;

/// singular-/pluralize subjective according to nr
pub fn inflect_adjective(subject: SubjectPronoun, as_plural: bool, uc: bool) -> &'static str {
    adjective(pluralize_pronoun(subject, as_plural), uc)
}

/// singular-/pluralize noun name according to nr
pub fn inflect_noun<S: AsRef<str>>(
    name: S,
    is_default_plural: bool,
    as_plural: bool,
    uc: bool,
) -> String {
    if is_default_plural == as_plural {
        uc_1st_if(name.as_ref(), uc)
    } else {
        let plural = language::inflect_name(name.as_ref(), as_plural);
        uc_1st_if(plural.as_str(), uc)
    }
}

/// singular-/pluralize subjective according to nr
pub fn inflect_objective(subject: SubjectPronoun, as_plural: bool, uc: bool) -> &'static str {
    objective(pluralize_pronoun(subject, as_plural), uc)
}

/// singular-/pluralize subjective according to nr
pub fn inflect_possesive(subject: SubjectPronoun, as_plural: bool, uc: bool) -> &'static str {
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
    fn indefinite_article(&self, uc: bool) -> &str;
    fn requires_article(&self) -> bool;
}

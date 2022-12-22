// (c) Roel Kluin 2022 GPL v3
//!
//! Functions are used by [Ranting](https://docs.rs/ranting_derive/0.1.0/ranting_derive/) trait placeholders.
//!
//! ```
//! use ranting::*;
//!
//! #[derive_ranting]
//! struct X {}
//! impl X {
//!     fn new(n: &str, s: &str) -> Self {
//!         X {
//!             name: n.to_string(),
//!             subject: s.to_string()
//!         }
//!     }
//! }
//!
//! # fn main() {
//! let mry = X::new("Mary", "she");
//! let lmb = X::new("lamb", "it");
//! let fl = X::new("fleece", "it");
//!
//! assert_eq!(say!("{mry had} {some little *lmb}. {`lmb} {fl were} white as snow."),
//!     "Mary had a little lamb. Its fleece was white as snow.".to_string());
//! # }
//! ```
//!
//! Inflection occurs according to a subjective pronoun; one of: `I`, `you`, `he`, `she`,
//! `it`, `we`, `they`, `thou` or `ye`. If the subjective is "they", the assumed gender is
//! neutrum. Verbs and articles in placeholders should be plural.

mod english;
use english as language;

pub use ranting_derive::*;

pub use in_definite;

// TODO: make this a feature.
pub use inflector;
pub use inflector::cases::sentencecase::to_sentence_case;
pub use inflector::string::pluralize::to_plural;
pub use inflector::string::singularize::to_singular;

// TODO: make this a feature:
pub use strum_macros;

pub use language::inflect_article;
pub use language::inflect_possesive_s;
pub use language::inflect_subjective;
pub use language::inflect_verb;
pub use language::is_subjective_plural;
pub use language::subjective;

use language::adjective;
use language::objective;
use language::possesive;

/// By overriding these one can adapt default behavior, which affects the
/// [placeholder](https://docs.rs/ranting_derive/0.1.0/ranting_derive/) interpretation.
// TODO: add function for 'the': some words require an article to be printed.
// E.g. names, languages, elements, food grains, meals (unless particular), sports.
// Space after should then also be omitted.
#[rustfmt::skip]
pub trait Ranting: std::fmt::Display {
    fn subjective(&self) -> &str;
    fn is_plural(&self) -> bool;
    fn name(&self, uc: bool) -> String;
    fn a_or_an(&self, uc: bool) -> &str;
    fn requires_article(&self) -> bool;
}

/// singular-/pluralize subjective according to nr
pub fn inflect_objective(subject: &str, as_plural: bool, uc: bool) -> &str {
    objective(inflect_subjective(subject, as_plural, false), uc)
}

/// singular-/pluralize subjective according to nr
pub fn inflect_possesive(subject: &str, as_plural: bool, uc: bool) -> &str {
    possesive(inflect_subjective(subject, as_plural, false), uc)
}

/// singular-/pluralize subjective according to nr
pub fn inflect_adjective(subject: &str, as_plural: bool, uc: bool) -> &str {
    adjective(inflect_subjective(subject, as_plural, false), uc)
}

/// singular-/pluralize noun name according to nr
pub fn inflect_noun(name: &str, is_default_plural: bool, as_plural: bool, uc: bool) -> String {
    let res = if is_default_plural == as_plural {
        name.to_string()
    } else if as_plural {
        to_plural(name)
    } else {
        to_singular(name)
    };
    if uc {
        uc_1st(res.as_str())
    } else {
        res
    }
}

/// upper cases first character
pub fn uc_1st(s: &str) -> String {
    let mut c = s.chars();
    c.next()
        .map(|f| f.to_uppercase().collect::<String>())
        .unwrap_or_default()
        + c.as_str()
}

pub struct Noun {
    name: String,
    subject: String,
}
impl Noun {
    pub fn new(name: &str, subject: &str) -> Self {
        Noun {
            name: name.to_string(),
            subject: subject.to_string(),
        }
    }
}

impl Ranting for Noun {
    fn subjective(&self) -> &str {
        self.subject.as_str()
    }
    fn is_plural(&self) -> bool {
        is_subjective_plural(self.subjective()).unwrap_or(false)
    }
    fn name(&self, uc: bool) -> String {
        let subject = self.subjective();
        match subject {
            "he" | "she" | "it" | "they" => inflect_noun(self.name.as_str(), true, true, uc),
            "I" => format!("I, {},", self.name),
            "you" | "we" => {
                format!("{}, {},", subjective(subject, uc), self.name)
            }
            p => panic!("Unimplemented: subject for '{}'", p),
        }
    }
    fn requires_article(&self) -> bool {
        true
    }
    fn a_or_an(&self, uc: bool) -> &str {
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

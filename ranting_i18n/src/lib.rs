//! A German reference lexicon for [`ranting`], built **only** on `ranting`'s public API.
//!
//! This crate is ROADMAP.md Phase 6 item 10: the acceptance test for items 1–9. Its job is not
//! to be a usable German library — the vocabulary is three nouns, four verbs, three adjectives
//! and the numerals `0..=12` — but to find out, concretely, which signals a non-English
//! `Ranting` implementation still cannot obtain through the trait seam.
//!
//! What it *does* reach through the hooks alone:
//!
//! - `der`/`die`/`das`/`den`/`dem`/`des` and `ein`/`eine`/`einen`/`einem`, selected from the
//!   noun's own [`NounClass`](ranting::NounClass) and case
//!   ([`Ranting::inflect_article_custom`](ranting::Ranting::inflect_article_custom));
//! - present-tense verb agreement over all six persons
//!   ([`inflect_verb_custom`](ranting::Ranting::inflect_verb_custom));
//! - the full weak/mixed/strong attributive adjective ending table
//!   ([`inflect_adjective_custom`](ranting::Ranting::inflect_adjective_custom));
//! - German numerals, with `1` agreeing like an article
//!   ([`inflect_numeral_custom`](ranting::Ranting::inflect_numeral_custom));
//! - noun capitalization ([`capitalize`](ranting::Ranting::capitalize)).
//!
//! What it does **not** reach is written down in this crate's `README.md` as eight numbered
//! holes, each cross-referenced from the ROADMAP Phase 6 item it belongs to. Every one of them
//! is recorded rather than worked around; where the lexicon carries state on the entity instead
//! (grammatical case, article definiteness), the README says so and says which hook would
//! otherwise have carried it.
//!
//! Word order, in particular, is **not** something `ranting` will do — Phase 6 item 1 settled
//! that as a permanent boundary. German verb-second order lives in this crate's own template
//! strings, and constructions that split one verb across two positions are out of reach
//! entirely. See README §"Word order".
//!
//! ```
//! use ranting::say;
//! use ranting_i18n::{Case, GermanNoun};
//!
//! let hund = GermanNoun::hund();
//! assert_eq!(say!("{the *=0 bellen}.", hund), "Der Hund bellt.");
//! assert_eq!(say!("Ich sehe {the *@0}.", hund), "Ich sehe den Hund.");
//! assert_eq!(
//!     say!("Ich gebe {the *=0} etwas.", hund.in_case(Case::Dative)),
//!     "Ich gebe dem Hund etwas."
//! );
//! ```

pub mod lexicon;
pub mod noun;
pub mod person;

pub use lexicon::{Case, Definiteness, Person};
pub use noun::GermanNoun;
pub use person::GermanPerson;

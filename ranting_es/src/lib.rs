//! A Spanish reference lexicon for [`ranting`], built **only** on `ranting`'s public API.
//!
//! This crate is ROADMAP.md Phase 6 item 23: the *second* acceptance test, after
//! `ranting_i18n`'s German (item 10). Its job is the same falsification exercise, on a language
//! chosen for a specific reason: item 10 found that German **structurally cannot use the
//! adjective hook at all** in a correct sentence (`ranting_i18n`'s hole 4a) — German attributive
//! adjectives are prenominal (`der kleine Hund`) while the `!`/`!!` degree slot
//! ([`Ranting::inflect_adjective_custom`]) is post-noun only, and German's post-verbal
//! predicative adjectives are uninflected. Spanish attributive adjectives are post-nominal
//! (`el gato negro`), so this crate can exercise [`Ranting::inflect_adjective_custom`]
//! end-to-end, in real Spanish sentences, where German could only exercise the *mechanism*.
//!
//! What it *does* reach through the hooks alone:
//!
//! - `el`/`la`/`los`/`las` and `un`/`una`/`unos`/`unas`, selected from the noun's own
//!   [`NounClass`](ranting::NounClass) — including the `el agua` euphonic exception
//!   ([`Ranting::inflect_article_custom`](ranting::Ranting::inflect_article_custom));
//! - post-nominal adjective agreement in gender and number, in a real Spanish sentence
//!   (`el gato negro`, `la casa negra`, `los gatos negros`)
//!   ([`Ranting::inflect_adjective_custom`](ranting::Ranting::inflect_adjective_custom));
//! - present-tense verb agreement over all six persons, including the `tú`/`usted` contrast
//!   ([`Ranting::inflect_verb_custom`](ranting::Ranting::inflect_verb_custom));
//! - Spanish numerals `0..=12`, with `1` agreeing like the indefinite article
//!   ([`Ranting::inflect_numeral_custom`](ranting::Ranting::inflect_numeral_custom));
//! - inverted `¿`/`¡` sentence-start capitalization, via the widening ROADMAP.md Phase 6 item 17
//!   already landed in `ranting_core` (nothing new this crate had to add).
//!
//! What it does **not** reach is written down in this crate's `README.md` as numbered holes,
//! each cross-referenced to the ROADMAP Phase 6 item it belongs to — the same falsification
//! contract `ranting_i18n` uses: every hole is recorded, none is worked around.
//!
//! Unlike German, Spanish nouns don't decline by grammatical case at all, so `GrammaticalCase`
//! having no dative variant (`ranting_i18n`'s hole 3) simply never comes up here — there is
//! nothing for the entity to carry that the hook signature doesn't already offer.
//!
//! ```
//! use ranting::say;
//! use ranting_es::SpanishNoun;
//!
//! let gato = SpanishNoun::gato();
//! assert_eq!(say!("{the *=0 !negro}", gato), "El gato negro");
//! assert_eq!(say!("{the *=0 ser} negro.", gato), "El gato es negro.");
//! assert_eq!(say!("{the +*=0 !negro}", gato), "Los gatos negros");
//! ```

pub mod lexicon;
pub mod noun;
pub mod person;

pub use noun::SpanishNoun;
pub use person::SpanishPerson;

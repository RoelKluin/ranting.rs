//! An Arabic reference lexicon for [`ranting`], built **only** on `ranting`'s public API.
//!
//! This crate is ROADMAP.md Phase 7 item 5: the *third* acceptance test, after `ranting_i18n`'s
//! German (Phase 6 item 10) and `ranting_es`'s Spanish (item 23). Both of those are Indo-European
//! and fusional — the shape the hook surface was designed against. Arabic was chosen by the
//! Phase 7 item 2 spike and confirmed by the item 4 build decision for two things neither of
//! them can reach:
//!
//! - **A third morphological number.** `كتاب` / `كتابان` / `كتب` — singular, dual, plural. German
//!   and Spanish have nothing to ask for beyond `to_plural: bool`, so nothing in this repo had
//!   ever pushed on it. The spike found that Phase 6 item 14's count channel reached every hook
//!   that *agrees* with the noun but not [`Ranting::inflect`], which renders the noun itself;
//!   Phase 7 item 11 closed that, and this crate is what it was closed for.
//! - **[`Ranting::elide_article_custom`]'s first real user.** The Phase 7 item 1 audit found that
//!   hook overridden by neither existing fork — German's and Spanish's real fusions are all
//!   preposition-side. Arabic's `ال` is written *bound* to its noun and assimilates to the
//!   fourteen sun letters (`ال` + `شمس` → `الشّمس`), which is exactly the post-assembly,
//!   may-drop-the-separator shape that hook was built for.
//!
//! What it reaches through the hooks alone:
//!
//! - singular/dual/plural on the noun, selected by the placeholder's own numeral
//!   ([`Ranting::inflect`]'s `count`), across both sound and broken plurals;
//! - the definite article `ال` bound to its noun, with sun-letter assimilation
//!   ([`Ranting::elide_article_custom`]), and the *absence* of an indefinite article;
//! - past-tense verb agreement across person, gender and number **including the dual**
//!   ([`Ranting::inflect_verb_custom`]);
//! - Arabic-Indic digits and spelled numerals with gender polarity — `ثلاثة كتب` but
//!   `ثلاث طالبات` ([`Ranting::inflect_numeral_custom`]).
//!
//! What it does **not** reach is written down in this crate's `README.md` as numbered holes, each
//! pinned by a test in `tests/holes.rs` — the same falsification contract the other two use:
//! every hole is recorded, none is worked around.
//!
//! ```
//! use ranting::say;
//! use ranting_ar::ArabicNoun;
//!
//! let kitab = ArabicNoun::kitab();
//! // The dual, on the noun itself — the reason this crate exists.
//! assert_eq!(say!("{$0 1}", 1, kitab), "١ كتاب");
//! assert_eq!(say!("{$0 1}", 2, kitab), "٢ كتابان");
//! assert_eq!(say!("{$0 1}", 3, kitab), "٣ كتب");
//!
//! // The article binds to its noun, and assimilates before a sun letter.
//! assert_eq!(say!("{the 0}", ArabicNoun::qamar()), "القمر");
//! assert_eq!(say!("{the 0}", ArabicNoun::shams()), "الشّمس");
//! ```

pub mod lexicon;
pub mod noun;

pub use noun::ArabicNoun;

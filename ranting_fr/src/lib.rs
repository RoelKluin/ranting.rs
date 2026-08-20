//! A French reference lexicon for [`ranting`], built **only** on `ranting`'s public API.
//!
//! This crate is the fifth falsifier, after German (`ranting_i18n`), Spanish (`ranting_es`),
//! Arabic (`ranting_ar`) and Japanese (`ranting_ja`) — and the first chosen for adoption reach
//! (French is one of the most widely used languages a `ranting` fork might target) rather than
//! purely to close a documented gap. It still earns its place the same way the other four did:
//! French's two most obvious candidate gaps turn out to already be claimed —
//! [`Ranting::elide_article_custom`](ranting::Ranting::elide_article_custom) is documented,
//! repo-wide, as built for exactly French's `le`+`homme`→`l'homme`, but `ranting_ar` is already
//! its first real user (sun/moon-letter assimilation); `de`+`le`→`du`/`à`+`le`→`au` fusion is
//! structurally identical to `ranting_es`'s already-closed hole 1. See this crate's README for
//! what's genuinely new here instead:
//!
//! - **Adjective position is lexically split, not categorical.** German is prenominal-only, so
//!   [`Ranting::inflect_adjective_custom`](ranting::Ranting::inflect_adjective_custom) is
//!   structurally unreachable for the *whole language*; Spanish and Arabic are postnominal-only,
//!   so the hook always fires correctly. French has a small closed set of common adjectives
//!   (`grand`, `petit`, `beau`, ...) that go *before* the noun, while most go after — a
//!   per-word, not per-language, reachability split against the hook's single post-noun slot.
//!   This is the crate's one real hole.
//! - **`is_mass()`/the partitive article (`du`/`de la`) have zero prior exercise** by any
//!   falsifier — a confirmation finding, not a hole.
//! - **`h aspiré` vs. `h muet`** gives `elide_article_custom` its first *negative* case: an
//!   entity-carried flag correctly declining to elide even though the surface string looks
//!   elidable. Also a confirmation.
//!
//! What it *does* reach through the hooks alone, for a real showcase of correct French:
//!
//! - `le`/`la`/`les` and `un`/`une`/`des`, selected from the noun's own
//!   [`NounClass`](ranting::NounClass)
//!   ([`Ranting::inflect_article_custom`](ranting::Ranting::inflect_article_custom));
//! - `du`/`de la` partitive articles on a mass noun
//!   ([`Ranting::is_mass`](ranting::Ranting::is_mass) +
//!   [`Ranting::inflect_article_custom`](ranting::Ranting::inflect_article_custom));
//! - `l'` elision before a vowel or `h muet`, correctly withheld before `h aspiré`
//!   ([`Ranting::elide_article_custom`](ranting::Ranting::elide_article_custom));
//! - post-nominal adjective agreement in gender and number
//!   ([`Ranting::inflect_adjective_custom`](ranting::Ranting::inflect_adjective_custom));
//! - present-tense verb agreement over `je`/`tu`/`il`/`elle`/`nous`/`vous`/`ils`/`elles`,
//!   including the `tu`/`vous` contrast
//!   ([`Ranting::inflect_verb_custom`](ranting::Ranting::inflect_verb_custom));
//! - French numerals `0..=20` plus the vigesimal irregulars `70`/`71`/`80`/`81`/`90`/`91`
//!   ([`Ranting::inflect_numeral_custom`](ranting::Ranting::inflect_numeral_custom));
//! - `du`/`au` preposition-article fusion, not a new finding — see the README
//!   ([`Ranting::inflect_preposition_custom`](ranting::Ranting::inflect_preposition_custom)).
//!
//! What it does **not** reach is written down in this crate's `README.md` as a numbered hole,
//! cross-referenced to `tests/holes.rs` — the same falsification contract every other falsifier
//! uses: every hole is recorded, none is worked around.
//!
//! ```
//! use ranting::say;
//! use ranting_fr::FrenchNoun;
//!
//! let chat = FrenchNoun::chat();
//! assert_eq!(say!("{the *=0 !noir}", chat), "Le chat noir");
//! let arbre = FrenchNoun::arbre();
//! assert_eq!(say!("{the *=0}", arbre), "L'arbre");
//! let eau = FrenchNoun::eau();
//! assert_eq!(say!("{a *=0}", eau), "De l'eau");
//! ```

pub mod lexicon;
pub mod noun;
pub mod person;

pub use noun::FrenchNoun;
pub use person::FrenchPerson;

//! A Mandarin Chinese reference lexicon for [`ranting`], built **only** on `ranting`'s public
//! API.
//!
//! This is the sixth falsifier, after German (`ranting_i18n`), Spanish (`ranting_es`), Arabic
//! (`ranting_ar`), Japanese (`ranting_ja`) and French (`ranting_fr`). Two obvious Mandarin
//! angles were considered and rejected as non-novel before this crate was scoped:
//!
//! - **Classifiers/measure words** (一只猫, 一本书) would, implemented the way `ranting_ja`'s
//!   counters are, be a second working example of the same mechanism — a lexical property read
//!   off `&self` inside [`Ranting::inflect_numeral_custom`], spliced against the noun via
//!   [`Ranting::elide_numeral_custom`] — adding no falsification. Implemented here anyway, for
//!   showcase completeness, but not claimed as a finding.
//! - **Prenominal adjective position** (Mandarin's attributive words are prenominal stative
//!   verbs) would be a *third* restatement of `ranting_i18n`'s hole 4a, no more novel than
//!   `ranting_fr`'s lexically-split version already made it the second time. Not implemented at
//!   all.
//!
//! What's genuinely new: **the tense-marker pipeline is architecturally committed to composing
//! an English auxiliary+verb pair, and no hook can turn that off.**
//! [`Ranting::inflect_verb_custom_with_context`] never receives which tense marker fired — only
//! `subject`, an already-partially-conjugated `verb` string, `as_plural`, `count`, `uc`, `ctx` —
//! and its return value is unconditionally piped through English auxiliary composition with no
//! trait method involved at all. Mandarin doesn't mark tense with auxiliary words; it marks
//! **aspect** with particles (了/过/着) suffixed directly onto the verb — a different
//! grammatical category the tense-marker system has no seam for. A fork can substitute the
//! correct Mandarin verb perfectly and the output is still an English/Mandarin hybrid:
//! `say!("{0 >eat}", MandarinNoun::mao())` renders `"Will 吃"`, never a bare 了/过/着-suffixed
//! verb with no auxiliary. See `tests/holes.rs` and the README.
//!
//! ```
//! use ranting::say;
//! use ranting_zh::{MandarinNoun, MandarinPerson};
//!
//! let mao = MandarinNoun::mao();
//! // A bare, untensed verb placeholder substitutes correctly, with no auxiliary to fight.
//! assert_eq!(say!("{0 eat}.", mao), "猫 吃.");
//! // The moment a tense marker is involved, an English auxiliary appears no matter what the
//! // verb hook returns — the crate's headline finding.
//! assert_eq!(say!("{0 >eat}", mao), "猫 will 吃");
//! // Classifiers work the same way `ranting_ja`'s counters do (not a new finding).
//! assert_eq!(say!("{#0 1}", 3, mao), "三只猫");
//! // 们-pluralization is a person-only exception; MandarinPerson takes it, common nouns don't.
//! assert_eq!(say!("{=0}", MandarinPerson::WOMEN), "我们");
//! ```

pub mod lexicon;
pub mod noun;
pub mod person;

pub use noun::MandarinNoun;
pub use person::MandarinPerson;

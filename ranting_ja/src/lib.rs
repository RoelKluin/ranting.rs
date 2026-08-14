//! A Japanese reference lexicon for [`ranting`], built **only** on `ranting`'s public API.
//!
//! This crate is ROADMAP.md Phase 7 item 6: the *fourth* acceptance test, after German
//! (Phase 6 item 10), Spanish (item 23) and Arabic (Phase 7 item 5). The Phase 7 item 3 spike
//! scoped it and the item 4 build decision bought it for two things, one of which is unusual:
//!
//! - **[`NarrationContext::register`]'s first real consumer.** It has been inert since Phase 3 —
//!   the crate never interprets it, and neither German nor Spanish ever read it, because in both
//!   of those politeness *is* a pronoun slot and rides the addressee's declared subject label
//!   instead. Japanese keigo operates on the verb with no pronoun present at all. This crate is
//!   the evidence that `register`'s design is right, which the item 1 audit could not settle from
//!   inside the repo — and the item 4 decision turns on it, since publishing freezes the trait.
//! - **A defect no other language reaches**: the numeral and its noun were joined by a hard-coded
//!   space that no hook could remove, so 「一匹の猫」 was unreachable and 「一匹の 猫」 was what
//!   rendered. This crate shipped the wrong output rather than working around it, since unlike
//!   Arabic's dual the gap had no workaround to encode. ROADMAP.md Phase 7 item 12 then closed it
//!   with [`Ranting::elide_numeral_custom`], of which this crate is the first and only user.
//!
//! Only **three of the eight hook pairs are live** here — verb, numeral and numeral elision. That
//! is a finding, not a gap: a surface sized for maximally-inflected languages degrading to near-nothing for a
//! low-inflection one is the intended shape, and Japanese is the proof that it degrades cleanly
//! rather than forcing a fork to fight it. See `README.md`.
//!
//! ```
//! use ranting::{Register, NarrationContext, say, say_with};
//! use ranting_ja::JapaneseNoun;
//!
//! let neko = JapaneseNoun::neko();
//!
//! // Politeness comes from the context, with no pronoun in the template and no entity state.
//! let formal = NarrationContext::new().register(Register::Formal);
//! let casual = NarrationContext::new().register(Register::Casual);
//! assert_eq!(say_with!(formal, "{0 are}", neko), "猫 です");
//! assert_eq!(say_with!(casual, "{0 are}", neko), "猫 だ");
//!
//! // Sonkeigo is a lexical substitution, keyed by verb + register + who the subject is.
//! let sensei = JapaneseNoun::sensei();
//! assert_eq!(say_with!(formal, "{0 eat}", sensei), "先生 召し上がります");
//!
//! // `say!()` is the plain form, so a context-free call is unchanged.
//! assert_eq!(say!("{0 are}", neko), "猫 だ");
//! ```

pub mod lexicon;
pub mod noun;
pub mod shopkeeper;

pub use noun::JapaneseNoun;
pub use shopkeeper::Shopkeeper;

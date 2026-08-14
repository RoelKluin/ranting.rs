//! An [`Answerable`] audience, for the one thing `ask!()` is genuinely useful for in Japanese.
//!
//! The Phase 7 item 3 spike's §3 finding: against unspaced Japanese *prose* every template
//! collapses to a single `{clause}` capture, so `ask!()` degenerates into "call `answer()` with
//! the input string". Against **command-style** input — written with spaces, as game and CLI
//! input often are — it does what it does for English. This type is that case, and
//! `tests/holes.rs` pins the other one.

use ranting::{Answerable, Ranting};

/// A shopkeeper who responds to `取る <item>`-shaped commands.
pub struct Shopkeeper;

impl Answerable for Shopkeeper {
    type Captures = String;

    fn answer(&self, _speaker: &dyn Ranting, item: String) -> String {
        match item.as_str() {
            "剣" => "剣を売ります。".to_string(),
            "本" => "本を売ります。".to_string(),
            _ => "それはありません。".to_string(),
        }
    }
}

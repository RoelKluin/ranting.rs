// (c) Roel Kluin 2026 MIT
//! Backs `ask!()`: lets an object respond differently depending on what it's
//! asked, with captures parsed out of free-form input the same way
//! `heed!()` parses them — see the `ask!()` macro docs for the full picture.

use crate::Ranting;

/// Implemented by anything that can be the audience of an `ask!()` call.
///
/// `Captures` mirrors `heed!()`'s own capture-count convention: `()` for a
/// zero-capture template, a bare `String` for one capture, or a tuple of
/// `String`s for two or more. Unlike `heed!()`, a `{$name}` numeric capture
/// is *not* auto-converted to `u64` here — `Captures` is always `String`
/// (or a tuple of `String`s), since this is a trait method with one fixed
/// signature regardless of which template a particular `ask!()` call site
/// used to reach it. Parse what you need inside `answer()`.
///
/// `Captures` being an associated type means a given implementor supports
/// exactly one capture arity everywhere it's used as an `ask!()` audience —
/// a type that needs to answer differently-shaped questions needs a
/// different approach (e.g. `Captures = Vec<String>`, giving up compile-time
/// arity checking).
pub trait Answerable {
    /// The capture shape every `ask!()` call site targeting this type must
    /// use — see the trait-level docs for why this is fixed per type.
    type Captures;

    /// Produce a response to `speaker`'s question, given the captures
    /// `ask!()` parsed out of the input text.
    fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String;
}

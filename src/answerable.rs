// (c) Roel Kluin 2026 MIT
//! Backs `ask!()`: lets an object respond differently depending on what it's
//! asked, with captures parsed out of free-form input the same way
//! `heed!()` parses them — see the `ask!()` macro docs for the full picture.

use crate::Ranting;

/// Implemented by anything that can be the audience of an `ask!()` call.
///
/// The associated type [`Captures`](Self::Captures) mirrors what `heed!()` returns: the unit
/// type for a template that captures nothing, a bare string for one capture, a tuple for two or
/// more. Numeric captures are the one difference — a `{$name}` arrives here as a string rather
/// than a `u64`, because the trait has one signature for every template that reaches it. Parse
/// what you need inside answer.
///
/// Being an associated type, that shape is fixed per implementor: one audience answers
/// questions of one arity. An audience that must field differently shaped questions needs a
/// shape wide enough for all of them — `Vec<String>`, say — giving up the arity check the
/// compiler would otherwise do.
pub trait Answerable {
    /// The capture shape every `ask!()` call site targeting this audience must use — see the
    /// trait docs for why it is fixed per type.
    type Captures;

    /// Produce a response to the speaker's question, given the captures `ask!()` parsed out of
    /// the input text.
    fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String;
}

use std::fmt::Write;

use proc_macro2::Literal;
use syn::{
    parse::{Parse, ParseStream},
    Error,
};
use unicode_width::UnicodeWidthStr;

use crate::*;

/// A wrapper around a string literal
pub struct StrLit {
    text: String,
    span_provider: Literal,
}

impl StrLit {
    pub fn new(input: syn::LitStr) -> Self {
        let text = input.to_token_stream().to_string();

        // input has to be parsed as `syn::LitStr` to access the content as a string. But in order to
        // call subspan, we need it as a `proc_macro2::Literal`. So: parse it as `LitStr` first and
        // convert that to a `Literal` with the same content and span.
        let mut span_provider = Literal::string(&text);
        span_provider.set_span(input.span()); // input is a single Token so span() works even on stable

        Self {
            text,
            span_provider,
        }
    }

    pub fn to_slice(&self) -> StrLitSlice {
        // find the position of the opening quote.
        let quote_position = self.text.find('"').unwrap();
        let prefix_length = quote_position + '"'.len_utf8();

        let start = prefix_length;
        let end = self.text.len() - '"'.len_utf8();

        StrLitSlice {
            src: self,
            range: start..end,
        }
    }
}

/// A slice into StrLit
#[derive(Clone)]
pub struct StrLitSlice<'a> {
    src: &'a StrLit,
    range: std::ops::Range<usize>,
}

impl<'a> StrLitSlice<'a> {
    pub fn text(&self) -> &str {
        &self.src.text[self.range.clone()]
    }

    #[track_caller]
    pub fn slice<R>(&'_ self, range: R) -> StrLitSlice<'a>
    where
        R: std::ops::RangeBounds<usize>
            + std::slice::SliceIndex<str, Output = str>
            + Clone
            + std::fmt::Debug,
    {
        use std::ops::Bound::*;
        let start = match range.start_bound() {
            Included(&n) => n,
            Excluded(_) => unimplemented!("StrLitSlice::slice: excluded start"),
            Unbounded => 0,
        };
        let end = match range.end_bound() {
            Included(&n) => n + self.text()[n..].chars().next().unwrap().len_utf8(),
            Excluded(&n) => n,
            Unbounded => self.range.len(),
        };

        let text = self.text().get(start..end).unwrap_or_else(|| {
            panic!(
                "StrLitSlice::slice: invalid range {range:?} for {:?}",
                self.text()
            );
        });
        if text.is_empty() {
            panic!("StrLitSlice::slice: empty slice");
        }

        StrLitSlice {
            src: self.src,
            range: self.range.start + start..self.range.start + end,
        }
    }

    /// Generates a crate::Error with the given message for the slice.
    pub fn error(&self, message: &str) -> Error {
        // subspan allows pointing at a span that is not the whole string, but it only works in nightly
        if let Some(span) = self.src.span_provider.subspan(self.range.clone()) {
            Error::new(span, message)
        } else {
            // Workaround for stable: print a copy of the entire format string into the error message
            // and manually underline the desired section.
            let mut m = String::new();
            writeln!(m, "{message}:").unwrap();

            let text_prefix = "At ";
            let text_prefix_len = 3; // length of "At "

            writeln!(m, "{text_prefix}{}", self.src.text).unwrap();

            let squiggle_start = UnicodeWidthStr::width(&self.src.text[..self.range.start]);
            let squiggle_len = UnicodeWidthStr::width(self.text());

            // Add the line with the error squiggles
            // start already includes the string prefix
            for _ in 0..(text_prefix_len + squiggle_start) {
                m.push(' ');
            }
            for _ in 0..squiggle_len {
                m.push('^');
            }
            Error::new_spanned(&self.src.span_provider, m)
        }
    }
}

impl Parse for StrLit {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse().map(Self::new)
    }
}

use std::ops::*;

impl Deref for StrLit {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

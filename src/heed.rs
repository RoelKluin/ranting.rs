// (c) Roel Kluin 2026 MIT
// Runtime support for heed!() — matches input text against a compiled
// template pattern and extracts the ordered set of named captures as raw
// strings. Per-capture typing ({name} -> String, {$name} -> u64, etc.) is
// resolved by the generated call-site code in ranting_derive, not here —
// this type only knows about strings and regex, so ranting_derive's regex
// dependency never needs to match this crate's. (Both declare 1.11 today, so
// the decoupling is unobservable in the manifests; it is a property of the
// generated code, which names only `ranting::HeedMatcher`.)

use regex::Regex;
use std::sync::OnceLock;

pub use ranting_core::heed_template::HeedTemplateError;

/// Backs `heed!()`'s generated code. One `HeedMatcher` is emitted as a
/// `static` at each `heed!()` call site; its regex is compiled once (via
/// `OnceLock`) and reused across every call through that call site.
///
/// [`HeedMatcher::from_template`] builds the same type from a template that
/// only exists at runtime — read from a file, typed by a user, or otherwise
/// unavailable as a `heed!()` call's own string literal.
#[derive(Debug)]
pub struct HeedMatcher {
    pattern: &'static str,
    names: &'static [&'static str],
    re: OnceLock<Regex>,
}

impl HeedMatcher {
    #[doc(hidden)]
    pub const fn new(pattern: &'static str, names: &'static [&'static str]) -> Self {
        Self {
            pattern,
            names,
            re: OnceLock::new(),
        }
    }

    /// Compiles a `heed!()`-shaped template that is only known at runtime — from a config file,
    /// user input, or anywhere else `heed!()`'s own compile-time string literal can't reach —
    /// into a matcher with the same behavior as one `heed!()` itself would generate.
    ///
    /// `heed!()` cannot accept a runtime template directly: its return type (`Option<T>` for zero
    /// or one capture, `Option<(T1, T2, ...)>` for more, with `{$name}` captures parsed to `u64`)
    /// is derived from the template's own text at compile time, which a `String` only known at
    /// runtime cannot supply. `from_template` is the honest alternative — every capture comes back
    /// as a plain `String` in [`HeedMatcher::match_input`]'s existing, always-untyped order (see
    /// [`HeedMatcher::capture_names`] to recover which name is which), the same shape `ask!()`'s
    /// `Answerable::Captures` already uses for the same reason (`.claude/rules/heed-input-parsing.md`).
    ///
    /// The pattern and capture names are leaked once (`Box::leak`) to fit `HeedMatcher`'s existing
    /// `'static` shape, the same shape a `heed!()` call site's own `static` gets for free from its
    /// literal argument. This is a deliberate, one-time cost: build a `HeedMatcher` once — at
    /// startup, from a vocabulary file — and reuse it, rather than calling `from_template` in a
    /// hot loop.
    ///
    /// ```
    /// # use ranting::HeedMatcher;
    /// let template = String::from("take {item}"); // e.g. read from a file at runtime
    /// let matcher = HeedMatcher::from_template(&template).expect("valid template");
    /// assert_eq!(matcher.match_input("take sword"), Some(vec!["sword".to_string()]));
    /// assert_eq!(matcher.capture_names(), &["item"]);
    /// ```
    pub fn from_template(template: &str) -> Result<Self, HeedTemplateError> {
        let (pattern, captures) = ranting_core::heed_template::compile_heed_template(template)?;
        let names: Vec<&'static str> = captures
            .into_iter()
            .map(|c| &*Box::leak(c.name.into_boxed_str()))
            .collect();
        Ok(Self::new(
            Box::leak(pattern.into_boxed_str()),
            Box::leak(names.into_boxed_slice()),
        ))
    }

    /// The capture names this matcher was built with, in the same order
    /// [`HeedMatcher::match_input`] returns their values.
    pub fn capture_names(&self) -> &[&str] {
        self.names
    }

    /// Matches `input` against this matcher's pattern, returning the captured values in
    /// [`HeedMatcher::capture_names`]'s order, or `None` on no match.
    pub fn match_input(&self, input: impl AsRef<str>) -> Option<Vec<String>> {
        let re = self
            .re
            .get_or_init(|| Regex::new(self.pattern).expect("heed!() pattern is valid regex"));
        let caps = re.captures(input.as_ref())?;
        Some(
            self.names
                .iter()
                .map(|name| {
                    caps.name(name)
                        .expect("heed!() capture name present in compiled pattern")
                        .as_str()
                        .to_string()
                })
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_single_word_capture() {
        static M: HeedMatcher = HeedMatcher::new(r"^take\s+(?P<item>\S+)$", &["item"]);
        assert_eq!(M.match_input("take sword"), Some(vec!["sword".to_string()]));
    }

    #[test]
    fn no_match_returns_none() {
        static M: HeedMatcher = HeedMatcher::new(r"^take\s+(?P<item>\S+)$", &["item"]);
        assert_eq!(M.match_input("drop sword"), None);
    }

    #[test]
    fn returns_captures_in_names_order() {
        static M: HeedMatcher = HeedMatcher::new(r"^(?P<b>\S+)\s+(?P<a>\S+)$", &["a", "b"]);
        // names is ["a", "b"] but the pattern's capture groups appear in the
        // order b, a — match_input must follow `names`, not group position.
        assert_eq!(
            M.match_input("first second"),
            Some(vec!["second".to_string(), "first".to_string()])
        );
    }

    #[test]
    fn caches_compiled_regex_across_repeated_calls() {
        static M: HeedMatcher = HeedMatcher::new(r"^(?P<a>\S+)$", &["a"]);
        assert_eq!(M.match_input("one"), Some(vec!["one".to_string()]));
        assert_eq!(M.match_input("two"), Some(vec!["two".to_string()]));
    }

    #[test]
    fn zero_names_matches_literal_only_pattern() {
        static M: HeedMatcher = HeedMatcher::new(r"^look around$", &[]);
        assert_eq!(M.match_input("look around"), Some(vec![]));
        assert_eq!(M.match_input("look elsewhere"), None);
    }
}

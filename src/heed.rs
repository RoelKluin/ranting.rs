// (c) Roel Kluin 2026 GPL v3
// Runtime support for heed!() — matches input text against a compiled
// template pattern and extracts the ordered set of named captures as raw
// strings. Per-capture typing ({name} -> String, {$name} -> u64, etc.) is
// resolved by the generated call-site code in ranting_derive, not here —
// this type only knows about strings and regex, so ranting_derive's own
// (older) regex dependency never needs to match this crate's.

use regex::Regex;
use std::sync::OnceLock;

/// Backs `heed!()`'s generated code. One `HeedMatcher` is emitted as a
/// `static` at each `heed!()` call site; its regex is compiled once (via
/// `OnceLock`) and reused across every call through that call site.
#[doc(hidden)]
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

    #[doc(hidden)]
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

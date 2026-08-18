// (c) Roel Kluin 2026 MIT
//! Shared, span-agnostic core of `heed!()`'s template compiler.
//!
//! This is the same algorithm `ranting_derive::heed` used to own outright, moved here so it can
//! be reached from both sides of the macro/runtime seam: `ranting_derive` (a `proc-macro = true`
//! crate, which `ranting` cannot depend on) still calls it at compile time and turns a
//! [`HeedTemplateError`] into a spanned `syn::Error`; `ranting::HeedMatcher::from_template` calls
//! the exact same function at runtime, for a template that only exists as a `String` (read from a
//! file, typed by a user, ...) rather than as a `heed!()` call's own literal argument. Neither side
//! duplicates the grammar; `ranting_derive → ranting` would be an illegal dependency cycle, so
//! `ranting_core` — already the shared rlib both sides depend on for exactly this role — is where
//! it has to live.
//!
//! Deliberately a much smaller grammar than `say!()`'s `PH_START`/`PH_EXT`: literal words plus
//! three capture forms (`{name}`, `{name...}`, `{$name}`) — no article/verb/pronoun-case markers,
//! since `heed!()` matches input text, it doesn't inflect output.

use std::ops::Range;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum HeedCaptureKind {
    /// `{name}` — a single whitespace-delimited token: `(?P<name>\S+)`.
    Word,
    /// `{name...}` — one or more tokens, lazily up to the next literal or
    /// end of input: `(?P<name>.+?)`.
    Greedy,
    /// `{$name}` — one or more digits, parsed to `u64` by the generated
    /// call-site code: `(?P<name>\d+)`.
    Number,
}

#[derive(Clone, Debug)]
pub struct HeedCapture {
    pub name: String,
    pub kind: HeedCaptureKind,
}

enum HeedSegment {
    /// An already-`regex::escape`d literal word.
    Literal(String),
    Capture(HeedCapture),
}

/// A malformed `heed!()` template, carrying the byte range (into the template text passed to
/// [`compile_heed_template`]) that the problem occupies. `Display` renders exactly the message
/// text `ranting_derive`'s compile errors have always used for each case — the derive side wraps
/// `to_string()` verbatim into a spanned `syn::Error`, so this text is pinned, not just informal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HeedTemplateError {
    NestedCapture { range: Range<usize> },
    UnterminatedBrace { range: Range<usize> },
    InvalidIdentifier { range: Range<usize> },
    AmbiguousAdjacentCaptures { range: Range<usize> },
}

impl HeedTemplateError {
    /// The byte range within the template text this error occupies.
    pub fn range(&self) -> Range<usize> {
        match self {
            Self::NestedCapture { range }
            | Self::UnterminatedBrace { range }
            | Self::InvalidIdentifier { range }
            | Self::AmbiguousAdjacentCaptures { range } => range.clone(),
        }
    }
}

impl std::fmt::Display for HeedTemplateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::NestedCapture { .. } => "heed!() captures cannot be nested",
            Self::UnterminatedBrace { .. } => "unterminated '{' in heed!() template",
            Self::InvalidIdentifier { .. } => {
                "invalid heed!() identifier: must start with a letter or '_' \
                 and contain only letters, digits, or '_'"
            }
            Self::AmbiguousAdjacentCaptures { .. } => {
                "ambiguous: two heed!() captures with no literal text between them"
            }
        })
    }
}

impl std::error::Error for HeedTemplateError {}

fn heed_capture_pattern(kind: HeedCaptureKind, name: &str) -> String {
    match kind {
        HeedCaptureKind::Word => format!(r"(?P<{name}>\S+)"),
        HeedCaptureKind::Greedy => format!(r"(?P<{name}>.+?)"),
        HeedCaptureKind::Number => format!(r"(?P<{name}>\d+)"),
    }
}

fn is_valid_capture_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Scans the template for `{...}` placeholders, splitting literal runs on
/// whitespace into individually-escaped word segments so `build_heed_pattern`
/// can insert `\s+`/`\s*` separators uniformly rather than baking whitespace
/// handling into this scan.
fn parse_heed_segments(text: &str) -> Result<Vec<HeedSegment>, HeedTemplateError> {
    let mut segments = Vec::new();
    let mut literal_start = 0usize;
    let mut chars = text.char_indices();

    while let Some((i, c)) = chars.next() {
        if c != '{' {
            continue;
        }
        for word in text[literal_start..i].split_whitespace() {
            segments.push(HeedSegment::Literal(regex::escape(word)));
        }

        let body_start = i + 1;
        let mut body_end = None;
        for (j, cc) in chars.by_ref() {
            if cc == '}' {
                body_end = Some(j);
                break;
            }
            if cc == '{' {
                return Err(HeedTemplateError::NestedCapture { range: i..j });
            }
        }
        let body_end = body_end.ok_or(HeedTemplateError::UnterminatedBrace {
            range: i..text.len(),
        })?;
        let body = &text[body_start..body_end];

        let (kind, name) = if let Some(rest) = body.strip_prefix('$') {
            (HeedCaptureKind::Number, rest)
        } else if let Some(rest) = body.strip_suffix("...") {
            (HeedCaptureKind::Greedy, rest)
        } else {
            (HeedCaptureKind::Word, body)
        };

        if !is_valid_capture_name(name) {
            return Err(HeedTemplateError::InvalidIdentifier {
                range: body_start..body_end.max(body_start + 1),
            });
        }

        // Only reject if the last segment is a Capture AND the gap between the
        // two captures has zero length (truly ambiguous). Whitespace-only gaps
        // are fine — build_heed_pattern will insert \s+ between the captures.
        let raw_gap = &text[literal_start..i];
        if matches!(segments.last(), Some(HeedSegment::Capture(_))) && raw_gap.is_empty() {
            return Err(HeedTemplateError::AmbiguousAdjacentCaptures {
                range: i..body_end + 1,
            });
        }

        segments.push(HeedSegment::Capture(HeedCapture {
            name: name.to_string(),
            kind,
        }));
        literal_start = body_end + 1;
    }

    for word in text[literal_start..].split_whitespace() {
        segments.push(HeedSegment::Literal(regex::escape(word)));
    }

    Ok(segments)
}

/// Joins parsed segments into the final anchored regex pattern, plus the
/// ordered capture list the runtime matcher needs. Whitespace between segments is
/// mandatory (`\s+`); leading/trailing whitespace in the input is tolerated
/// (`\s*` at both ends). Punctuation-only literals (no alphanumeric chars)
/// attach to the preceding segment without whitespace.
///
/// The mandatory `\s+` is a **decided, permanent restriction**, not a gap:
/// whitespace is the only word boundary `heed!()`/`ask!()` know, and no
/// tokenizer hook exists here (ROADMAP.md Phase 6 item 9). Note this is not an
/// ASCII/Latin restriction — the pattern is script-agnostic, so `"取る {item}"`
/// against `"取る 剣"` captures fine; what will not match is a template whose
/// segments abut, e.g. `"{item}を取る"` against `"剣を取る"`, which returns
/// `None` rather than letting backtracking invent a split. Relaxing these
/// joins to `\s*` would trade that honest `None` for a silently wrong capture.
fn build_heed_pattern(segments: &[HeedSegment]) -> (String, Vec<HeedCapture>) {
    let mut pattern = String::from(r"^\s*");
    let mut captures = Vec::new();

    for (i, segment) in segments.iter().enumerate() {
        let is_punctuation_only = match segment {
            HeedSegment::Literal(escaped) => !escaped.chars().any(|c| c.is_alphanumeric()),
            HeedSegment::Capture(_) => false,
        };

        if i > 0 && !is_punctuation_only {
            pattern.push_str(r"\s+");
        }
        match segment {
            HeedSegment::Literal(escaped) => pattern.push_str(escaped),
            HeedSegment::Capture(cap) => {
                pattern.push_str(&heed_capture_pattern(cap.kind, &cap.name));
                captures.push(cap.clone());
            }
        }
    }
    pattern.push_str(r"\s*$");
    (pattern, captures)
}

/// Compiles a `heed!()` template string into a runtime regex pattern plus
/// the ordered list of captures it contains. Surfaces malformed syntax and
/// ambiguous adjacent captures as byte-range-carrying [`HeedTemplateError`]s.
pub fn compile_heed_template(text: &str) -> Result<(String, Vec<HeedCapture>), HeedTemplateError> {
    let segments = parse_heed_segments(text)?;
    Ok(build_heed_pattern(&segments))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_word_capture_pattern() {
        let (pattern, captures) = compile_heed_template("take {item}").expect("should compile");
        assert_eq!(pattern, r"^\s*take\s+(?P<item>\S+)\s*$");
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].name, "item");
        assert_eq!(captures[0].kind, HeedCaptureKind::Word);
    }

    #[test]
    fn greedy_and_numeric_capture_kinds() {
        let (pattern, captures) =
            compile_heed_template("give {item...} to {target}, {$count} gold")
                .expect("should compile");
        assert_eq!(
            pattern,
            r"^\s*give\s+(?P<item>.+?)\s+to\s+(?P<target>\S+),\s+(?P<count>\d+)\s+gold\s*$"
        );
        assert_eq!(captures[0].kind, HeedCaptureKind::Greedy);
        assert_eq!(captures[1].kind, HeedCaptureKind::Word);
        assert_eq!(captures[2].kind, HeedCaptureKind::Number);
    }

    #[test]
    fn zero_captures_is_a_pure_literal_pattern() {
        let (pattern, captures) = compile_heed_template("look around").expect("should compile");
        assert_eq!(pattern, r"^\s*look\s+around\s*$");
        assert!(captures.is_empty());
    }

    #[test]
    fn adjacent_captures_are_rejected() {
        let err =
            compile_heed_template("{a}{b}").expect_err("should reject ambiguous adjacent captures");
        assert!(matches!(
            err,
            HeedTemplateError::AmbiguousAdjacentCaptures { .. }
        ));
        assert_eq!(err.range(), 3..6);
        assert!(err.to_string().contains("ambiguous"));
    }

    #[test]
    fn nested_capture_is_rejected() {
        let err = compile_heed_template("take {a{b}}").expect_err("should reject nesting");
        assert!(matches!(err, HeedTemplateError::NestedCapture { .. }));
        assert!(err.to_string().contains("nested"));
    }

    #[test]
    fn unterminated_brace_is_rejected() {
        let err =
            compile_heed_template("take {item").expect_err("should reject unterminated brace");
        assert!(matches!(err, HeedTemplateError::UnterminatedBrace { .. }));
        assert!(err.to_string().contains("unterminated"));
    }

    #[test]
    fn invalid_capture_name_is_rejected() {
        let err =
            compile_heed_template("take {2item}").expect_err("should reject invalid identifier");
        assert!(matches!(err, HeedTemplateError::InvalidIdentifier { .. }));
        assert!(err.to_string().contains("identifier"));
    }

    #[test]
    fn empty_capture_name_is_rejected() {
        let err = compile_heed_template("take {}").expect_err("should reject empty capture name");
        assert!(matches!(err, HeedTemplateError::InvalidIdentifier { .. }));
        assert!(err.to_string().contains("identifier"));
    }

    #[test]
    fn whitespace_separated_captures_are_allowed() {
        let (pattern, captures) =
            compile_heed_template("{a} {b}").expect("should compile whitespace-separated captures");
        assert!(
            pattern.contains(r"\s+"),
            "pattern should contain separator: {pattern}"
        );
        assert_eq!(captures.len(), 2);
        assert_eq!(captures[0].name, "a");
        assert_eq!(captures[1].name, "b");
    }
}

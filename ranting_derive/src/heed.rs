// (c) Roel Kluin 2026 MIT
// Compile-time template parsing for heed!(). Deliberately a much smaller
// grammar than say!()'s PH_START/PH_EXT: literal words plus three capture
// forms ({name}, {name...}, {$name}) — no article/verb/pronoun-case markers,
// since heed!() matches input text, it doesn't inflect output.

use crate::str_lit::{StrLit, StrLitSlice};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum HeedCaptureKind {
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
pub(crate) struct HeedCapture {
    pub name: String,
    pub kind: HeedCaptureKind,
}

enum HeedSegment {
    /// An already-`regex::escape`d literal word.
    Literal(String),
    Capture(HeedCapture),
}

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
fn parse_heed_segments(slice: &StrLitSlice) -> syn::Result<Vec<HeedSegment>> {
    let text = slice.text();
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
                return Err(slice.slice(i..j).error("heed!() captures cannot be nested"));
            }
        }
        let body_end = body_end.ok_or_else(|| {
            slice
                .slice(i..text.len())
                .error("unterminated '{' in heed!() template")
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
            return Err(slice.slice(body_start..body_end.max(body_start + 1)).error(
                "invalid heed!() identifier: must start with a letter or '_' \
                     and contain only letters, digits, or '_'",
            ));
        }

        // Only reject if the last segment is a Capture AND the gap between the
        // two captures has zero length (truly ambiguous). Whitespace-only gaps
        // are fine — build_heed_pattern will insert \s+ between the captures.
        let raw_gap = &text[literal_start..i];
        if matches!(segments.last(), Some(HeedSegment::Capture(_))) && raw_gap.is_empty() {
            return Err(slice
                .slice(i..body_end + 1)
                .error("ambiguous: two heed!() captures with no literal text between them"));
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
/// ordered capture list `HeedMatcher` needs. Whitespace between segments is
/// mandatory (`\s+`); leading/trailing whitespace in the input is tolerated
/// (`\s*` at both ends). Punctuation-only literals (no alphanumeric chars)
/// attach to the preceding segment without whitespace.
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
/// ambiguous adjacent captures as span-accurate `syn::Error`s.
pub(crate) fn compile_heed_template(lit: &StrLit) -> syn::Result<(String, Vec<HeedCapture>)> {
    let slice = lit.to_slice();
    let segments = parse_heed_segments(&slice)?;
    Ok(build_heed_pattern(&segments))
}

use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Error, Expr, Token,
    parse::{Parse, ParseStream},
};

/// Converts a matched capture's raw `String` into its typed form. `Word`
/// and `Greedy` captures are always valid (any non-empty string), but a
/// `Number` capture's digits — guaranteed by its regex, not by size — can
/// overflow `u64`, so it converts via `.ok()?` and relies on the enclosing
/// closure returning `Option<_>` to short-circuit the whole `heed!()` call
/// to `None` on overflow instead of panicking on untrusted input.
fn heed_convert_captured(kind: HeedCaptureKind, value: TokenStream) -> TokenStream {
    match kind {
        HeedCaptureKind::Word | HeedCaptureKind::Greedy => value,
        HeedCaptureKind::Number => quote! { #value.parse::<u64>().ok()? },
    }
}

/// `heed!(template_str, input_expr)`.
pub(crate) struct Heed {
    template: StrLit,
    input: Expr,
}

impl Parse for Heed {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(Error::new(
                proc_macro2::Span::mixed_site(),
                "missing template string",
            ));
        }
        let template = input.parse::<StrLit>()?;
        input.parse::<Token![,]>()?;
        if input.is_empty() {
            return Err(Error::new(
                proc_macro2::Span::mixed_site(),
                "missing input expression",
            ));
        }
        let input_expr = input.parse::<Expr>()?;
        Ok(Heed {
            template,
            input: input_expr,
        })
    }
}

impl ToTokens for Heed {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (pattern, captures) = match compile_heed_template(&self.template) {
            Ok(result) => result,
            Err(e) => {
                tokens.extend(e.to_compile_error());
                return;
            }
        };

        // Defensive: build_heed_pattern is expected to always emit a valid
        // regex. Verifying it here, at compile time, turns any bug in this
        // module into a heed!() compile error instead of a runtime panic at
        // every call site — matches the crate's "catch it early" stance.
        if let Err(e) = regex::Regex::new(&pattern) {
            let msg = format!(
                "heed!() generated an invalid regex ({e}) — this is a bug in \
                 heed!()'s template compiler, please report it"
            );
            tokens.extend(Error::new_spanned(&self.template.span_provider, msg).to_compile_error());
            return;
        }

        let names: Vec<&str> = captures.iter().map(|c| c.name.as_str()).collect();
        let names_tokens = quote! { &[#(#names),*] };
        let input_expr = &self.input;

        let value_expr = match captures.len() {
            0 => quote! { __ranting_heed_caps.map(|_| ()) },
            1 => {
                // `Number` converts straight to `Option<u64>` (`.ok()`, no
                // `?`) and plugs into `and_then` as-is — writing `Some(expr?)`
                // here would be clippy::needless_question_mark, since `expr?`
                // and the `Some(..)` cancel out to just `expr`. `Word`/`Greedy`
                // convert to a plain value, so `map` (not `and_then`) suits them.
                if captures[0].kind == HeedCaptureKind::Number {
                    quote! {
                        __ranting_heed_caps.and_then(|mut __v| {
                            let __s = __v.pop().expect("heed!() matched capture count mismatch");
                            __s.parse::<u64>().ok()
                        })
                    }
                } else {
                    let converted = heed_convert_captured(captures[0].kind, quote! { __s });
                    quote! {
                        __ranting_heed_caps.map(|mut __v| {
                            let __s = __v.pop().expect("heed!() matched capture count mismatch");
                            #converted
                        })
                    }
                }
            }
            _ => {
                let element_exprs: Vec<TokenStream> = captures
                    .iter()
                    .map(|cap| {
                        heed_convert_captured(
                            cap.kind,
                            quote! { __it.next().expect("heed!() matched capture count mismatch") },
                        )
                    })
                    .collect();
                quote! {
                    __ranting_heed_caps.and_then(|__v| {
                        let mut __it = __v.into_iter();
                        Some(( #(#element_exprs),* ))
                    })
                }
            }
        };

        *tokens = quote! {{
            static __RANTING_HEED_MATCHER: ranting::HeedMatcher =
                ranting::HeedMatcher::new(#pattern, #names_tokens);
            let __ranting_heed_caps = __RANTING_HEED_MATCHER.match_input(#input_expr);
            #value_expr
        }};
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile(text: &str) -> syn::Result<(String, Vec<HeedCapture>)> {
        let lit_str: syn::LitStr =
            syn::parse_str(&format!("{text:?}")).expect("valid string literal source");
        let lit = StrLit::new(lit_str);
        compile_heed_template(&lit)
    }

    #[test]
    fn single_word_capture_pattern() {
        let (pattern, captures) = compile("take {item}").expect("should compile");
        assert_eq!(pattern, r"^\s*take\s+(?P<item>\S+)\s*$");
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].name, "item");
        assert_eq!(captures[0].kind, HeedCaptureKind::Word);
    }

    #[test]
    fn greedy_and_numeric_capture_kinds() {
        let (pattern, captures) =
            compile("give {item...} to {target}, {$count} gold").expect("should compile");
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
        let (pattern, captures) = compile("look around").expect("should compile");
        assert_eq!(pattern, r"^\s*look\s+around\s*$");
        assert!(captures.is_empty());
    }

    #[test]
    fn adjacent_captures_are_rejected() {
        let err = compile("{a}{b}").expect_err("should reject ambiguous adjacent captures");
        assert!(err.to_string().contains("ambiguous"));
    }

    #[test]
    fn unterminated_brace_is_rejected() {
        let err = compile("take {item").expect_err("should reject unterminated brace");
        assert!(err.to_string().contains("unterminated"));
    }

    #[test]
    fn invalid_capture_name_is_rejected() {
        let err = compile("take {2item}").expect_err("should reject invalid identifier");
        assert!(err.to_string().contains("identifier"));
    }

    #[test]
    fn empty_capture_name_is_rejected() {
        let err = compile("take {}").expect_err("should reject empty capture name");
        assert!(err.to_string().contains("identifier"));
    }

    #[test]
    fn whitespace_separated_captures_are_allowed() {
        let (pattern, captures) =
            compile("{a} {b}").expect("should compile whitespace-separated captures");
        // Pattern should have \s+ between the two captures
        assert!(
            pattern.contains(r"\s+"),
            "pattern should contain separator: {}",
            pattern
        );
        assert_eq!(captures.len(), 2);
        assert_eq!(captures[0].name, "a");
        assert_eq!(captures[1].name, "b");
    }

    #[test]
    fn zero_width_adjacent_captures_are_still_rejected() {
        let err = compile("{a}{b}").expect_err("should still reject truly adjacent captures");
        assert!(err.to_string().contains("ambiguous"));
    }
}

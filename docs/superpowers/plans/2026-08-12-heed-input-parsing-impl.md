# heed!() Input Parsing (v1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `heed!(template, input)`, a new expression macro that matches free-form input text against a small template grammar (literal words + named captures) and returns the captured values — the command-parser-style half of the feasibility brainstorm in `docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md`.

**Architecture:** Two halves, same split as every other macro in this crate: `ranting_derive` parses the template at compile time into an anchored regex pattern plus an ordered capture list, and generates code referencing a small runtime type, `ranting::HeedMatcher`, that owns the actual `regex::Regex` (compiled once, cached via `std::sync::OnceLock`) and returns raw string captures. Per-capture typing (`{name}` → `String`, `{$name}` → `u64`) is resolved entirely in the generated call-site code — `HeedMatcher` itself is kind-agnostic, it only knows named-capture-by-string.

**Tech Stack:** Rust, `syn`/`quote`/`proc-macro2` (already `ranting_derive` dependencies), `regex` (already a dependency of both crates — no new dependency for v1).

## Global Constraints

- v1 scope only, per the approved spec: `{name}` (single-token capture), `{name...}` (greedy, until next literal or end of input), `{$name}` (digits, parsed to `u64`). No `{#name}` word-number captures.
- Return shape: positional, matching `say!()`'s existing positional-only style — bare `Option<T>` for 0 or 1 captures, `Option<(T1, T2, ...)>` for 2+.
- Two adjacent captures with no literal text between them is a **compile-time error**, not a runtime ambiguity.
- `HeedMatcher`'s internals (regex compilation/caching) must be fully owned by the `ranting` crate — generated code must never reference `regex::` types directly, so `ranting_derive`'s regex version (1.6.0) and `ranting`'s (1.11) never need to match.
- Only ever edit `ranting_derive/src/heed.rs` and `src/heed.rs` for this feature's logic — do not touch `english_shared.rs`, `verb_conjugate.rs`, or `narration.rs`, which are unrelated shared-codegen files with their own build.rs mechanisms (see CLAUDE.md).
- Revert incidental `Cargo.lock`/`ranting_derive/Cargo.lock` drift after every `cargo build`/`test`/`clippy` run, per this repo's established pattern (`git checkout -- Cargo.lock ranting_derive/Cargo.lock`).

---

### Task 1: Runtime `HeedMatcher` (in `ranting`)

**Files:**
- Create: `src/heed.rs`
- Modify: `src/lib.rs` (add `mod heed;` and re-exports)

**Interfaces:**
- Produces: `pub struct HeedMatcher` with `pub const fn new(pattern: &'static str, names: &'static [&'static str]) -> Self` and `pub fn match_input(&self, input: &str) -> Option<Vec<String>>`. `match_input` returns `None` on no match; on match, returns one `String` per entry in `names`, in the same order, each holding the raw (unconverted) matched text for that named capture group.

- [ ] **Step 1: Write the failing tests**

Create `src/heed.rs`:

```rust
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
    pub fn match_input(&self, input: &str) -> Option<Vec<String>> {
        let re = self
            .re
            .get_or_init(|| Regex::new(self.pattern).expect("heed!() pattern is valid regex"));
        let caps = re.captures(input)?;
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
        static M: HeedMatcher =
            HeedMatcher::new(r"^(?P<b>\S+)\s+(?P<a>\S+)$", &["a", "b"]);
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
```

- [ ] **Step 2: Wire the module in, then run to verify tests pass**

Add to `src/lib.rs` near the other `mod`/`pub use` declarations (see existing
`mod narration;` at line 33 and `pub use narration::{...}` at line 37 for the
pattern to match):

```rust
mod heed;
```

and, near the other `#[doc(hidden)]`-style re-exports (e.g. next to
`pub use language::english_shared::{is_subject, is_subjective_plural};`):

```rust
#[doc(hidden)]
pub use heed::HeedMatcher;
```

Run: `cargo test --offline heed`
Expected: 5 tests pass (`matches_single_word_capture`,
`no_match_returns_none`, `returns_captures_in_names_order`,
`caches_compiled_regex_across_repeated_calls`,
`zero_names_matches_literal_only_pattern`).

- [ ] **Step 3: Commit**

```bash
git add src/heed.rs src/lib.rs
git commit -m "feat: add HeedMatcher runtime support for heed!()"
```

---

### Task 2: Compile-time template parser (in `ranting_derive`)

**Files:**
- Create: `ranting_derive/src/heed.rs`
- Modify: `ranting_derive/src/lib.rs` (add `mod heed;`)

**Interfaces:**
- Consumes: `str_lit::StrLit`/`StrLitSlice` (existing, see `ranting_derive/src/str_lit.rs` — `StrLit::to_slice()`, `StrLitSlice::text()`, `StrLitSlice::slice(range)`, `StrLitSlice::error(msg) -> syn::Error`).
- Produces: `pub(crate) fn compile_heed_template(lit: &StrLit) -> syn::Result<(String, Vec<HeedCapture>)>`, `pub(crate) struct HeedCapture { pub name: String, pub kind: HeedCaptureKind }`, `pub(crate) enum HeedCaptureKind { Word, Greedy, Number }` (all `Clone, Copy` where applicable) — consumed by Task 3's `Heed::to_tokens`.

- [ ] **Step 1: Write the failing tests**

Create `ranting_derive/src/heed.rs`:

```rust
// (c) Roel Kluin 2026 GPL v3
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
                return Err(slice
                    .slice(i..j)
                    .error("heed!() captures cannot be nested"));
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
            return Err(slice
                .slice(body_start..body_end.max(body_start + 1))
                .error(
                    "heed!() capture name must start with a letter or '_' \
                     and contain only letters, digits, or '_'",
                ));
        }

        if matches!(segments.last(), Some(HeedSegment::Capture(_))) {
            return Err(slice.slice(i..body_end + 1).error(
                "ambiguous: two heed!() captures with no literal text between them",
            ));
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
/// (`\s*` at both ends).
fn build_heed_pattern(segments: &[HeedSegment]) -> (String, Vec<HeedCapture>) {
    let mut pattern = String::from(r"^\s*");
    let mut captures = Vec::new();

    for (i, segment) in segments.iter().enumerate() {
        if i > 0 {
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
            r"^\s*give\s+(?P<item>.+?)\s+to\s+(?P<target>\S+)\,\s+(?P<count>\d+)\s+gold\s*$"
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
}
```

- [ ] **Step 2: Wire the module in, then run to verify tests pass**

Add to `ranting_derive/src/lib.rs`, alongside the existing `mod language;`,
`mod ranting_impl;`, `mod str_lit;` (line 3-5):

```rust
mod heed;
```

Run: `cd ranting_derive && cargo test --offline --lib heed`
Expected: 9 tests pass (5 in `ranting_derive/src/heed.rs`'s own `tests`
module — note: 7 listed above, recount: `single_word_capture_pattern`,
`greedy_and_numeric_capture_kinds`, `zero_captures_is_a_pure_literal_pattern`,
`adjacent_captures_are_rejected`, `unterminated_brace_is_rejected`,
`invalid_capture_name_is_rejected`, `empty_capture_name_is_rejected` — 7
tests).

- [ ] **Step 3: Commit**

```bash
git add ranting_derive/src/heed.rs ranting_derive/src/lib.rs
git commit -m "feat: add heed!() compile-time template parser"
```

---

### Task 3: `heed!()` macro entry point

**Files:**
- Modify: `ranting_derive/src/heed.rs` (add `Heed` struct + `Parse`/`ToTokens`)
- Modify: `ranting_derive/src/lib.rs` (add `#[proc_macro] pub fn heed`)
- Modify: `src/lib.rs` (re-export the macro)

**Interfaces:**
- Consumes: Task 1's `ranting::HeedMatcher` (`new`, `match_input`), Task 2's `compile_heed_template`, `HeedCapture`, `HeedCaptureKind`.
- Produces: the `heed!(template_str_literal, input_expr)` macro, callable from any crate depending on `ranting`.

- [ ] **Step 1: Append the macro codegen to `ranting_derive/src/heed.rs`**

Append to `ranting_derive/src/heed.rs` (after `compile_heed_template`, before
the `#[cfg(test)]` module):

```rust
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Error, Expr, Token,
};

fn heed_convert_captured(kind: HeedCaptureKind, value: TokenStream) -> TokenStream {
    match kind {
        HeedCaptureKind::Word | HeedCaptureKind::Greedy => value,
        HeedCaptureKind::Number => quote! {
            #value.parse::<u64>().expect(
                "heed!() $ capture is guaranteed all-digits by its regex, but overflowed u64"
            )
        },
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
                let converted = heed_convert_captured(captures[0].kind, quote! { __s });
                quote! {
                    __ranting_heed_caps.map(|mut __v| {
                        let __s = __v.pop().expect("heed!() matched capture count mismatch");
                        #converted
                    })
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
                    __ranting_heed_caps.map(|__v| {
                        let mut __it = __v.into_iter();
                        ( #(#element_exprs),* )
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
```

- [ ] **Step 2: Add the proc-macro entry point**

In `ranting_derive/src/lib.rs`, add near the other `#[proc_macro]` functions
(e.g. right after `pub fn ask`, around line 65):

```rust
/// heed!(template, input) matches `input` against `template` — literal
/// words plus `{name}` (single token), `{name...}` (greedy, until the next
/// literal or end of input), and `{$name}` (digits, parsed to u64) captures
/// — returning `None` on no match, or the captured value(s) on match: bare
/// for 0/1 captures, a tuple for 2+, matching say!()'s positional style.
#[proc_macro]
pub fn heed(input: TokenStream1) -> TokenStream1 {
    let output = parse_macro_input!(input as heed::Heed);
    let tokens: TokenStream = parse_quote!(#output);
    tokens.into()
}
```

- [ ] **Step 3: Re-export from `ranting`**

In `src/lib.rs`, add next to the other macro re-exports (e.g. next to
`pub use ranting_derive::say;`):

```rust
pub use ranting_derive::heed;
```

- [ ] **Step 4: Build both crates and run the existing test suites**

Run: `cargo build --offline && cd ranting_derive && cargo build --offline`
Expected: both build clean (Task 4 adds the first real integration test of
the macro itself — this step only confirms it compiles and wires together).

Run: `cargo test --offline --lib` (from repo root) and
`cd ranting_derive && cargo test --offline --lib`
Expected: all existing tests still pass — this task must not regress
anything (`heed!()` is additive).

Revert lockfile drift: `git checkout -- Cargo.lock ranting_derive/Cargo.lock`

- [ ] **Step 5: Commit**

```bash
git add ranting_derive/src/heed.rs ranting_derive/src/lib.rs src/lib.rs
git commit -m "feat: add heed!() macro entry point"
```

---

### Task 4: Integration tests

**Files:**
- Create: `tests/ranting/heed.rs`
- Modify: `tests/ranting/main.rs` (add `mod heed;`)

**Interfaces:**
- Consumes: `ranting::heed` (the macro re-export from Task 3).

- [ ] **Step 1: Write the tests**

Create `tests/ranting/heed.rs`:

```rust
// (c) Roel Kluin 2026 GPL v3
use ranting::heed;

#[test]
fn single_word_capture() {
    assert_eq!(heed!("take {item}", "take sword"), Some("sword".to_string()));
}

#[test]
fn no_match_returns_none() {
    assert_eq!(heed!("take {item}", "drop sword"), None);
}

#[test]
fn multi_capture_returns_positional_tuple() {
    assert_eq!(
        heed!("give {item} to {target}", "give sword to guard"),
        Some(("sword".to_string(), "guard".to_string()))
    );
}

#[test]
fn greedy_capture_spans_multiple_words() {
    assert_eq!(
        heed!("take {item...}", "take rusty old sword"),
        Some("rusty old sword".to_string())
    );
}

#[test]
fn greedy_capture_before_trailing_literal_and_capture() {
    assert_eq!(
        heed!(
            "give {item...} to {target}",
            "give rusty old sword to guard"
        ),
        Some(("rusty old sword".to_string(), "guard".to_string()))
    );
}

#[test]
fn numeric_capture_parses_to_u64() {
    assert_eq!(heed!("take {$count} gold", "take 42 gold"), Some(42u64));
}

#[test]
fn zero_captures_matches_literal_only() {
    assert_eq!(heed!("look around", "look around"), Some(()));
    assert_eq!(heed!("look around", "look elsewhere"), None);
}

#[test]
fn tolerates_surrounding_and_extra_whitespace() {
    assert_eq!(
        heed!("take {item}", "  take   sword  "),
        Some("sword".to_string())
    );
}

#[test]
fn three_captures_returns_three_tuple() {
    assert_eq!(
        heed!(
            "trade {$count} {item} for {target}",
            "trade 3 sword for shield"
        ),
        Some((3u64, "sword".to_string(), "shield".to_string()))
    );
}
```

- [ ] **Step 2: Register the module and run**

Add to `tests/ranting/main.rs`, alphabetically (after `mod error_messages;`
and before `mod inclusive_language;`, matching the existing ordering):

```rust
mod heed;
```

Run: `cargo test --offline heed`
Expected: 9 tests pass (`single_word_capture`, `no_match_returns_none`,
`multi_capture_returns_positional_tuple`,
`greedy_capture_spans_multiple_words`,
`greedy_capture_before_trailing_literal_and_capture`,
`numeric_capture_parses_to_u64`, `zero_captures_matches_literal_only`,
`tolerates_surrounding_and_extra_whitespace`,
`three_captures_returns_three_tuple`).

Then run the full suite to confirm no regressions:
`cargo test --offline` (root) and `cd ranting_derive && cargo test --offline --lib`
Expected: all pass — total integration test count in the root crate should
be the prior count (202, per the last session's work) plus these 9 = 211.

Revert lockfile drift: `git checkout -- Cargo.lock ranting_derive/Cargo.lock`

- [ ] **Step 3: Commit**

```bash
git add tests/ranting/heed.rs tests/ranting/main.rs
git commit -m "test: add heed!() integration tests"
```

---

### Task 5: Docs and roadmap

**Files:**
- Modify: `CLAUDE.md`
- Modify: `README.md`
- Modify: `ROADMAP.md`

**Interfaces:** none (docs only).

- [ ] **Step 1: CLAUDE.md**

Add a new bullet under "Non-obvious behaviors" (near the `say_with!()`
bullets), and a short paragraph under "Architecture: Two Crates + Shared
Code" describing the split (mirrors the existing `say_with!()` paragraph
style):

```markdown
**`heed!()` input parsing** (v1.1): `heed!(template, input)` is the inverse
direction of `say!()`'s placeholder syntax, but a deliberately smaller
grammar — literal words plus `{name}`/`{name...}`/`{$name}` captures only,
no article/verb/pronoun-case markers (see
`docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md` for why
full grammatical inversion isn't attempted). `ranting_derive/src/heed.rs`
compiles the template into an anchored regex at compile time;
`ranting::HeedMatcher` (`src/heed.rs`) owns the actual `regex::Regex`
(compiled once, cached via `OnceLock`) so generated code never references
`regex::` types directly — this is why `ranting_derive`'s regex 1.6.0 and
`ranting`'s regex 1.11 never need to match versions.
```

```markdown
- **`heed!()` capture syntax**: `{name}` captures one whitespace-delimited
  token; `{name...}` captures greedily up to the next literal or end of
  input; `{$name}` captures digits and parses them to `u64`. Two captures
  with no literal text between them is a compile-time error (ambiguous —
  there's no way to know where one capture ends and the next begins).
  Return type is positional, like `say!()`: bare `Option<T>` for 0/1
  captures, `Option<(T1, T2, ...)>` for 2+.
```

- [ ] **Step 2: README.md**

Add a new section after the existing macro documentation (find the section
describing `ack!()`/`nay!()` and add after it):

```markdown
## Parsing input with `heed!()`

`heed!()` is the reverse direction from `say!()` — matching input text
against a template to extract values, in the spirit of C's `scanf`:

```rust
use ranting::heed;

fn main() {
    assert_eq!(
        heed!("take {item}", "take sword"),
        Some("sword".to_string())
    );
    assert_eq!(
        heed!("give {item} to {target}", "give sword to guard"),
        Some(("sword".to_string(), "guard".to_string()))
    );
    assert_eq!(heed!("take {item}", "drop sword"), None);
}
```

- `{name}` captures a single word; `{name...}` captures greedily (multiple
  words) up to the next literal word or the end of input; `{$name}`
  captures digits and parses them as a `u64`.
- Returns `None` if the input doesn't match the template.
- Two placeholders with nothing but whitespace between them
  (`"{a}{b}"`) is a compile-time error — there would be no way to know
  where one capture ends and the next begins.
- `heed!()` doesn't understand `say!()`'s grammar markers (`=`, `@`,
  `` ` ``, `~`, tense markers, articles) — it matches plain input text
  against literal words and named captures only.
```

- [ ] **Step 3: ROADMAP.md**

Add a new item after item 7 in the "Upcoming Priority Features" list
(before "### v1.1 Success Criteria"):

```markdown
8. **Input Parsing (`heed!()`)** (v1 scope, see
   `docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md`)
   - `heed!(template, input)` matches free-form input text against a
     template — literal words plus `{name}`/`{name...}`/`{$name}`
     captures — the command-parser half of the input-parsing feasibility
     brainstorm. The full-grammatical-inversion half (`unsay!()`) was
     explicitly not pursued: several of `say!()`'s inflection choices are
     not injective (multiple original values render to the same text), so
     a general inverse isn't a buildable spec.
   - v2 (not yet scoped in detail): `#[derive(Heed)]` +
     `#[heed(template = "...")]` on a user struct, generating
     `fn heed(input: &str) -> Option<Self>`, built on the same matching
     engine as v1 rather than duplicating it.
```

- [ ] **Step 4: Verify and commit**

Run the full verification pass one final time:

```bash
cargo test --offline
cd ranting_derive && cargo test --offline --lib && cd ..
cargo clippy --offline --all-targets
cd ranting_derive && cargo clippy --offline --all-targets && cd ..
git checkout -- Cargo.lock ranting_derive/Cargo.lock
git status --short
```

Expected: all tests pass, no new clippy warnings beyond the pre-existing
baseline (dead-code warnings on unrelated shared-file items, `map_or`
simplification suggestion in `plurals.rs`, etc. — see prior session's
verification for the known baseline), and `git status --short` shows only
`CLAUDE.md`, `README.md`, `ROADMAP.md` modified.

```bash
git add CLAUDE.md README.md ROADMAP.md
git commit -m "docs: document heed!() input parsing"
```

---

## Self-Review Notes

- **Spec coverage**: every v1 decision from the approved spec
  (`docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md`) has a
  task — capture syntax (Task 2), numeric-capture scope (Task 2/3, `{$name}`
  only), return shape (Task 3, positional tuple), reusable core logic for a
  future v2 derive (Task 1's `HeedMatcher` + Task 2's `compile_heed_template`
  are both already factored out as standalone, reusable units, not inlined
  into `Heed::to_tokens`).
- **Naming**: `heed!()` used consistently across all tasks and docs, matching
  the spec's naming decision; `unsay!()`/feature B is explicitly *not*
  referenced as pending work anywhere in this plan, matching the spec's
  recommendation not to pursue it.
- **Type consistency checked**: `HeedMatcher::new`/`match_input` signatures
  in Task 1 match every call site generated in Task 3; `HeedCapture`/
  `HeedCaptureKind` defined in Task 2 are the exact names `Heed::to_tokens`
  in Task 3 consumes; the `Cargo.lock` revert command and clippy invocation
  match the pattern already established in this repo's session history.

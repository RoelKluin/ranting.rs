# Input parsing: `heed!()`, `#[derive(Heed)]`, `ask!()`

Read before touching `ranting_core/src/heed_template.rs`, `ranting_derive/src/heed.rs`,
`heed_derive.rs`, `src/heed.rs` or `src/answerable.rs`. All three surfaces share **one** template
compiler (`ranting_core::heed_template::compile_heed_template`), so a change to it changes all
three.

**The whole subsystem sits behind a `heed` Cargo feature, default-on** (2026-08-20) — same
forwarding shape as the existing `debug` feature: `ranting_core`'s `heed_template` module,
`ranting_derive`'s `heed`/`heed_derive` modules plus its `heed`/`ask`/`Heed` proc-macro entry
points and the `Ask` type, and `ranting`'s `heed`/`answerable` modules plus their `pub use`
re-exports, are all `#[cfg(feature = "heed")]`. Default-on because `ranting_ja`'s public
`Shopkeeper` (`Answerable` impl) relies on it. A new `_custom` hook or template-compiler change
under this feature needs no extra cfg work — add it inside the already-gated files/items; only a
*new* top-level module or re-export needs its own `#[cfg(feature = "heed")]`.

**The compiler itself lives in `ranting_core`, not `ranting_derive`** (2026-08-18) — moved there
so a runtime template (see `HeedMatcher::from_template` below) can reach the same algorithm a
`proc-macro = true` crate cannot be depended on from `ranting`, and `ranting_derive` cannot depend
on `ranting` without an illegal cycle. It is span-agnostic: `HeedTemplateError` carries byte
ranges, not `syn` spans. `ranting_derive/src/heed.rs`'s `compile_heed_template(lit: &StrLit)` is
now a thin wrapper — call the `ranting_core` function on the literal's text, then turn a
`HeedTemplateError` into a spanned `syn::Error` via `slice.slice(err.range()).error(&err.to_string())`.
`heed_derive.rs` and `ask!()` (`ranting_derive/src/lib.rs`) needed no changes; they already called
through the wrapper.

## `heed!()`

`heed!(template, input)` is the inverse direction of `say!()`'s placeholder syntax, but a
deliberately smaller grammar — literal words plus `{name}`/`{name...}`/`{$name}` captures only, no
article/verb/pronoun-case markers. See
`docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md` for why full grammatical inversion
isn't attempted.

`ranting_derive/src/heed.rs` compiles the template into an anchored regex at compile time;
`ranting::HeedMatcher` (`src/heed.rs`) owns the actual `regex::Regex` (compiled once, cached via
`OnceLock`) so generated code never references `regex::` types directly — **this is why
the two crates' regex versions never need to match.** (They happen to be the same today — both
declare `regex = "1.11"` — so the decoupling is currently unobservable; it is a property of the
generated code, not of the manifests.)

Capture syntax: `{name}` captures one whitespace-delimited token; `{name...}` captures lazily (`.+?`) up
to the next literal or end of input; `{$name}` captures digits and parses them to `u64`. Two
captures with a **zero-width** gap between them are a **compile-time error** (ambiguous) — the
check fires on an empty raw gap, so `{a} {b}` (separated by whitespace) is fine and is pinned by
`ranting_derive/src/heed.rs`'s `whitespace_separated_captures_are_allowed`. Return type is
positional, like `say!()`: bare `Option<T>` for 0/1 captures, `Option<(T1, T2, ...)>` for 2+.

**`heed!()` itself always needs its template as a string literal** — its typed return shape comes
from reading the template's own text at compile time, which a template only known at runtime
(read from a file, typed by a user, ...) cannot supply. `HeedMatcher::from_template(template: &str)
-> Result<HeedMatcher, HeedTemplateError>` (`src/heed.rs`) is the runtime alternative: it calls the
same `ranting_core::heed_template::compile_heed_template`, `Box::leak`s the owned pattern/names
into the `'static` shape `HeedMatcher` already needs (documented as a deliberate one-time cost —
build once, e.g. at startup from a vocabulary file, not in a hot loop), and every capture comes
back as a plain `String` via `HeedMatcher::match_input`/`capture_names()` — never a typed tuple or
`u64`, since that shape can't be derived without knowing the template at compile time. This is the
same always-`String` compromise `ask!()`'s `Answerable::Captures` already makes, for the identical
reason. `HeedMatcher` and `match_input` are no longer `#[doc(hidden)]` — `from_template` is a real,
documented entry point now, not just `heed!()`'s own codegen detail.

## `#[derive(Heed)]`

v1.3, ROADMAP.md Phase 3 item 8's v2 — struct-level sugar over `heed!()`, **not** a separate
matching engine. `heed_derive::derive_heed` calls the same `compile_heed_template`, then generates
`impl StructName { pub fn heed(input: &str) -> Option<Self> }`. Every capture must have a
same-named field and vice versa — a stale field or an unmapped capture is a compile error, not a
silently-ignored gap — and each field's type is checked against its capture kind (`String` for
`{name}`/`{name...}`, `u64` for `{$name}`). Only structs (named fields, or a unit struct for a
zero-capture template); no enums. Field declaration order need not match template capture order.

Fixed 2026-08-16: it used to fail to compile on an empty braced struct (`struct Foo {}`),
generating bare `Self` for any zero-field struct without distinguishing it from a true unit
struct (`struct Foo;`) — `Self` doesn't typecheck for the braced form, which needs `Self {}`.
`heed_derive.rs::derive_heed` now branches on the `Fields::Unit` vs. `Fields::Named` variant
itself rather than on "zero fields," and `struct Wait {}` is pinned by
`tests/ranting/heed_derive.rs::zero_captures_empty_braced_struct_still_generates_heed`. See
`docs/architecture-review-2026-08-14.md` §1.1 for the original discovery.

## Whitespace is the only word boundary, permanently

ROADMAP.md Phase 6 item 9. A *decided restriction*, not an unfinished edge — no tokenizer hook
exists or will. It covers all three surfaces even though the ROADMAP item's title names only two.

The mechanism is `build_heed_pattern`, which joins every pair of adjacent segments with a mandatory
`\s+` (a punctuation-only literal is the sole exception, attaching to the preceding segment);
`{name}` is `\S+`, `{$name}` is `\d+`, `{name...}` is `.+?`. Three consequences:

1. It is **not** an ASCII/Latin restriction — the regex is script-agnostic, so
   `heed!("取る {item}", "取る 剣")` → `"剣"` and `heed!("เอา {item}", "เอา ดาบ")` → `"ดาบ"` work
   exactly like the English examples. What fails is a template whose segments abut, in any script.
2. The failure is an **honest `None`, never a wrong capture** — `heed!("{item}を取る", "剣を取る")`
   and `heed!("{a}的{b}", "我的剑")` both return `None` rather than letting regex backtracking invent
   a split. Same "don't silently guess" stance that makes two zero-gap captures a *compile* error.
3. The escape hatch is that an unspaced clause is exactly one `\S+` token, so
   `heed!("{clause}", "剣を取る")` hands the whole run back for the caller's own segmenter.

The punctuation-only exemption is script-agnostic too (`` {item}、 取る `` matches `"剣、 取る"`) but
is *per-segment*: `` {item}、取る `` is one alphanumeric-containing literal token, so it takes the
`\s+` and returns `None`.

Rejected: a pluggable tokenizer boundary in `compile_heed_template` — see DONE.md Phase 6 item 9
for why the compile-time/runtime split makes that a new registration mechanism nothing else in the
crate has. Pinned by `tests/ranting/script_segmentation.rs`.

## `ask!()` and `Answerable`

Phase 5, v1.2.1. `ask!(speaker, audience, template, input)` reuses the same compiler to parse
`input`, then forwards the captures to `audience.answer(&speaker, captures)` — the `Answerable`
trait (`src/answerable.rs`). Unlike `heed!()`, captures are always plain `String`/tuples of
`String` regardless of `{$name}` markers (no `u64` conversion), because `Answerable::Captures` is a
fixed associated type — parse what you need inside `answer()`. Returns `Option<String>`, `None` on
no match, **without calling `answer()` at all**.

Known limitation: `Captures` being per-type means one implementor supports exactly one capture
arity everywhere it's used as an `ask!()` audience.

`ask!()` needed no `from_template`-equivalent: its only job beyond `heed!()`'s own is forwarding
captures to `Answerable::answer`, a plain public trait method, so a caller with a runtime template
already reaches it via `HeedMatcher::from_template(&template)?.match_input(input)` plus a manual
call, building `Self::Captures` from the returned `Vec<String>` by hand
(`tests/ranting/heed_dynamic.rs::a_runtime_template_can_still_reach_answerable_by_hand`).
`#[derive(Heed)]` has no runtime equivalent at all, and cannot — its generated struct fields *are*
the compile-time template knowledge; there is no struct to generate from a `String` at runtime.

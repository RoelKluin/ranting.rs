# Architecture / documentation review — 2026-08-14

Two-stage parallel audit of the repo (eight read-only investigators, one adversarial
verification pass). Every claim below was checked twice: once by the investigator that
found it, once by a fresh verifier at the cited location and against surrounding code.
Claims the verifier could not confirm are not in this document.

Companion to `docs/architecture-review-2026-08-13.md`, which covers the pre-`ranting_core`
history. This one covers the state after Phase 6 and after the 2026-08-14 ROADMAP→DONE
split.

## Scope and method

Eight topics, each owning a disjoint file set: runtime placeholder engine; `ranting_core`;
`ranting_derive`; the `heed!()`/`ask!()`/`#[derive(Heed)]` family; `collections.rs` +
`narration.rs`; the two falsifier crates; the documentation surface; tests/data/scripts.
No investigator could write. All edits happened after verification.

**No investigator contradicted another.** Three findings were independently corroborated
from opposite directions — the `ranting_derive/build.rs` adjective codegen (topics 2 and
3), and the hook inventory (topic 1 counting trait methods, topic 5 counting wrapper
delegations).

## 1. Code defects

### 1.1 `#[derive(Heed)]` cannot be used on an empty braced struct

`ranting_derive/src/heed_derive.rs`. A braced struct with zero fields (`struct Foo {}`)
yields an empty iterator from `Fields::Named`, making it indistinguishable downstream from
`Fields::Unit` (`heed_derive.rs:43-54`). Codegen branches on `field_idents.is_empty()`
(`:136-143`) and emits a bare `Self`, which is only legal for tuple and unit structs.

Verified by compiling, not by reading. With a zero-capture template:

```
error: the `Self` constructor can only be used with tuple or unit structs
  = note: this error originates in the derive macro `Heed`
```

The explanatory comment at `heed_derive.rs:132-135` has the rule backwards: it claims a
unit struct needs bare `Self` while braced structs do not. In fact `Self {}` is legal for
**both**, so emitting `Self {}` unconditionally would fix the bug in one token.

Not documented as behavior anywhere, and deliberately not documented by this review — it
is a bug to fix, not an invariant to enshrine. Untested: the existing coverage
(`tests/ranting/heed_derive.rs:113`) uses a unit struct, which takes the working path.

**Impact**: low. `struct Foo {}` is an unusual way to spell a unit struct. But the failure
is a derive-macro error with no span pointing at the user's actual mistake, so anyone who
hits it will not easily see why.

### 1.2 `#[derive(Heed)]` field-type checking is textual

`heed_derive.rs:31-33` compares `quote!(#ty).to_string().replace(' ', "")` against the
literals `"String"` and `"u64"`. A field typed `std::string::String`, or through any type
alias, is rejected despite being the correct type. Undocumented.

### 1.3 The punctuation exemption ignores template-side whitespace

`ranting_derive/src/heed.rs:150`. The punctuation-only-literal exemption suppresses the
mandatory `\s+` based on the *segment's* content, without regard to whitespace written in
the template. So `heed!("take {item} .", "take sword .")` compiles to `...\S+)\.\s*$` and
does **not** match, even though the input is spaced exactly as the template is. `README.md`
documents only the permissive direction (punctuation may abut). Untested.

### 1.4 Capture names are ASCII-only

`heed.rs:41-48`. Matching is script-agnostic (confirmed — see §3.4), but the capture
*identifier* inside the braces must be ASCII. `{$n...}` is also silently rejected because
`$` is stripped before `...` is examined (`heed.rs:86-92`). Neither is documented; neither
is likely to matter, since capture names are the template author's own choice.

### 1.5 `{?article noun}` renders literal garbage unless the entity skips articles — ✅ **FIXED 2026-08-14**

Found while probing for `docs/superpowers/specs/2026-08-14-japanese-falsification-spike.md`.
`README.md` documents a leading `?` on an article as "its display depends (see `no_article`)",
with the example `say!("{?the 0} was great!", activity)`. That works for an entity whose
`skip_article()` is `true`:

```rust
say!("{?the 0} was great!", Breakfast {})   // no_article = true  -> "breakfast was great!"
```

For any ordinary noun it does not hide-or-show the article — it produces literal nonsense, with
no error at either compile time or run time:

```rust
let dog = Noun::new("dog", "it");
say!("{?the dog}")   // -> "?thes dog"
say!("{?a dog}")     // -> "?as dog"
```

`?the` is being taken as the noun word rather than as a marked article, pluralized to `thes`, and
emitted with the `?` still attached, leaving the real noun to render as a post-noun word. Every
test and doc example of this syntax uses a `no_article = true` entity, which is why it has never
surfaced. The documented behavior is the `false` case; the implemented behavior is only the `true`
case.

**Cause**: `ArticleKind::classify` (and the macro's hand-kept copy in
`ranting_derive/src/lib.rs::article_kind_tokens`) trimmed a leading `!` and nothing else, so
`?the` classified as `ArticleKind::Other`. `Other` is the *pre-noun verb* path — the word is
offered to `inflect_article_custom` (English returns `None`) and then conjugated as a verb, which
is where the `s` came from. The `no_article = true` case looked fine only because
`get_article_or_so`'s `skip_article()` early return fires *before* the classification match, so
the broken branch was never reached by any test or example.

**Fixed**: both classification sites now trim `['!', '?']`, and `get_article_or_so` strips both
from the word before it reaches a hook or `adapt_article`. `?the` is now exactly `the`, which is
the documented reading — the entity is consulted for every article kind anyway, so `?` asks for
behavior that was already the default. Pinned by
`tests/ranting/article_classification.rs::question_marked_article_is_still_an_article` (plus the
`no_article` half, which is what every pre-existing test exercised) and
`ranting_core`'s `strips_leading_question_mark_before_classifying`.

**Noted while fixing, not addressed**: `{!the 0}` — the emphasis marker `get_article_or_so`
checks for at its `skip_article()` early return (`!s.starts_with('!')`) — does not parse. The
grammar rejects it with "expected a noun or variable name, found `!the 0`", so that branch may be
unreachable from `say!()` altogether.

### 1.6 `{?$n noun}` leaves a double space — ✅ **FIXED 2026-08-14**

A hidden numeral rendered nothing, but its separator survived: `say!("I see {?$0 boot}", 2)` →
`"I see  boots"`. It was **pinned** by `tests/ranting/numeral.rs` rather than flagged, so it read
as intended behavior in the one place it was written down — for two phases. Cosmetic in English;
on the critical path for Japanese, where hiding the numeral was the only candidate workaround for
the numeral-noun separator gap (Japanese spike §1).

**Fixed** as ROADMAP.md Phase 7 item 13. A hidden numeral sits between two separators — its own
leading one and the noun's — and the pair now collapses to one, the same way a zero-length
article's does; `NumeralSpec` gained a `hidden: bool` so the slot is representable at all. Which
separator survives matters: keeping the leading one is what leaves `{The ?$n noun}` rendering
`"The raven"`. Both pins now assert the corrected output.

### 1.7 Elision panicked on a multibyte article — ✅ **FIXED 2026-08-14**

`split_at_find_end` (`src/lib.rs`) advanced one **byte** past the byte index `rfind` returned, so
the post-assembly elision splice sliced mid-codepoint whenever the rendered article's last
character was multibyte: *"end byte index 3 is not a char boundary; it is inside 'ل'"*. Any fork
whose `inflect_article_custom` returns a non-ASCII article and which overrides
`elide_article_custom` panicked — Arabic `ال`, Cyrillic `этот`, Greek `τό` alike.

Nothing about it was language-specific; it was byte arithmetic. What kept it alive is worth
recording, because it is the same shape as §4.7: `elide_article_custom` had **no real user at
all** until `ranting_ar` (the Phase 7 item 1 audit says exactly that), and both existing reference
lexicons' articles are ASCII, so no gate in six directories could reach the line. It surfaced on
`ranting_ar`'s very first `cargo test`.

Fixed by advancing `char::len_utf8()` instead of `1`. Pinned by
`tests/ranting/property_based.rs::elision_does_not_panic_on_a_multibyte_article`, which was
verified to fail against the old code rather than merely pass against the new — the whole class of
"regression test that pins nothing" is what §4.7 is about.

## 2. Documentation defects found and fixed on 2026-08-14

All of these were corrected in the same session that produced this review.

| # | Claim | Where | Reality |
|---|---|---|---|
| 2.1 | `SENTENCE_TRIGGER_CHARS` is "the one list both `PH_START`'s regex and `ranting_derive`'s `at_sentence_start` read from, so the two can't drift" | `CLAUDE.md`, and the doc comment at `ranting_core/src/grammar.rs:62-64` | **False, and unimplementable as stated.** `PH_START` is a `concat!` of string literals with the trigger characters hard-coded in its own class (`grammar.rs:56`); a `concat!` cannot interpolate a `&[char]` const. The two lists are hand-maintained duplicates. `SENTENCE_TRIGGER_CHARS`' only readers are `ranting_derive/src/lib.rs:155,165` |
| 2.2 | The `EXTENSIBILITY.md` §2.0 `_with_context` example | `docs/EXTENSIBILITY.md:96-108` | **Did not compile.** Omitted the `count: Option<PlaceholderCount>` parameter added by Phase 6 item 14, and its delegating call passed 4 args where 5 are required. Confirmed with `cargo build` against a scratch crate: `E0050` + `E0061`. The block is fenced ` ```rust ` inside a `.md`, so nothing in the suite compiles it |
| 2.3 | "`*?` and `**` are deliberately not accepted" | `CLAUDE.md` | **Overstated.** Both are accepted, parsed as two ordinary one-character reps; only the *fused* reading is refused (`ph_ext.rs:612-614`). Confirmed by running `say!("{*?who}")` and `say!("{**who}")` — output `[]` and `[Alex]`, no compile error |
| 2.4 | `sentence_start` is baked into `PlaceholderSpec` "alongside the existing `uc` bool" | `CLAUDE.md` | `uc` is **not** a `PlaceholderSpec` field. The struct has 11 fields (`placeholder.rs:357-417`); `uc` is a separate argument at `ranting_derive/src/lib.rs:954,961` |
| 2.5 | "German records seven holes" | `CLAUDE.md` | **Eight** are recorded (`ranting_i18n/README.md` headings at :66,85,104,124,150,173,194,220); **three** are open (2, 3, 4); four are closed (1, 5, 6, 7); hole 8 is a permanent boundary, not an unclosed gap. "Seven" matched neither total. `ranting_es/README.md:41` already said "eight" |
| 2.6 | `ranting_core/build.rs` is "now the *only* place that codegen runs" | `CLAUDE.md` | True of the **verb** tables only. Three build scripts generate three different tables: root `build.rs:61-75` → plurals; `ranting_derive/build.rs:88-105` → adjectives; `ranting_core/build.rs:86-94` → verbs |
| 2.7 | "both `ranting` and `ranting_derive` get [strum] transitively" | `CLAUDE.md` | True of `ranting_derive` only. Root `Cargo.toml:22-23` still declares `strum`/`strum_macros` directly |
| 2.8 | Three `src/lib.rs` line citations | `CLAUDE.md` | All three stale: `:371-381`→`:466-480`, `:493`→`:672`, `:512`→`:828`. Replaced with function-relative descriptions so they cannot rot again |
| 2.9 | "456 tests" | `ROADMAP.md` (introduced 2026-08-14) | 526 compiled tests + 15 doctests. My own error, from a `grep` that missed 75 in-source `#[cfg(test)]` units |
| 2.10 | "Non-English languages (not yet supported; v1.1+ may change this)" | `docs/TUTORIAL.md:51` | Stale since Phase 6 |
| 2.11 | Macro list omitting `say_with!`/`ask!`/`heed!`/`Heed`/trait-object helpers | `ranting_derive/README.md` | Last touched 2023-02-22; predates all of them |

## 3. Verified accurate (recorded so the next audit can skip them)

- **Hook inventory.** Seven `_custom`/`_with_context` pairs + `capitalize`/`_with_context`
  = eight `_with_context` methods, plus one unpaired `is_first_person_subject_custom`.
  The `Ranting` trait has 23 methods total.
- **Wrapper delegation is complete.** `Many`, `Maybe` and `Box` each override **all 23**.
  No hook falls through to a default in any wrapper. `Many::own_count()` substitution
  appears at exactly the 12 `len() == 1` sites and never overrides an explicit count.
- **The falsifier invariant holds.** Both `ranting_i18n` and `ranting_es` depend on
  `ranting` alone. No `ranting_core::`/`ranting_derive::` path in either crate's source or
  manifest.
- **Hole↔test parity is complete** in both crates, both directions.
- **`PH_EXT` really is test-oracle-only**; `Article`/`DemonstrativePronoun` really are dead.
- **The `?` vs `?+` regex fix** (Phase 6 item 26) is in place with a regression test at
  `grammar.rs:255-263`.
- **`gate_dirs` covers every manifest directory** (five when this was written, eight since
  `ranting_ar`/`ranting_ja` landed 2026-08-14 — the glob needed no edit, which was the point).
  `scripts/overnight_loop.sh:73` globs
  `"$REPO_ROOT"/Cargo.toml "$REPO_ROOT"/*/Cargo.toml` — the root manifest is listed
  explicitly, so the subdirectory-only glob concern does not apply.
- **All eleven `docs/EXTENSIBILITY.md` section numbers cited by `CLAUDE.md` exist** with
  matching subjects. A §2.13 exists that `CLAUDE.md` does not cite; not an error.
- **Whitespace segmentation is genuinely script-agnostic** — the punctuation exemption
  uses Unicode `is_alphanumeric`, not an ASCII check.
- **No `@docs/` force-loading reference exists anywhere** in the tree.

## 4. Open questions for the maintainer — not fixed, deliberately

These are decisions, not defects. Nothing was changed for any of them.

### 4.1 Version numbering is contradictory

`CHANGELOG.md:3`'s only release heading is `## v1.3.0`. Every manifest says otherwise:
root and `ranting_derive` are `0.2.1`; `ranting_core`, `ranting_es`, `ranting_i18n` are
`0.1.0`. `README.md` pins its docs.rs badges and links to `/0.2.1/`, while documenting
`heed!()`/`ask!()` and other surface that 0.2.1 predates entirely — so a reader following
the README's own links lands on documentation for a version without the features the
README describes.

Publishing a version is the copyright holder's call, so no manifest was touched.

### 4.2 `cargo package` ships more than it should

No `exclude` or `include` key exists in any of the five manifests. Measured with
`cargo package --list --allow-dirty`: the package contains 42 `tests/` entries, all three
`data/*.txt`, `docs/*.md`, `scripts/overnight_loop.sh`, and the stray tracked
`git_log_oneline.txt`.

Phase 4 item 8's hygiene work (recorded in `DONE.md`) untracked scratch files, and cargo
honors `.gitignore`, so that claim is not false — but untracking is a weaker mechanism
than an `exclude` list, and it does not cover files that are legitimately tracked yet
should not ship. Also still tracked: seven
`.superpowers/sdd/2026-08-12-trait-extensibility-impl/*review-package.txt` files.

### 4.3 Roughly 60 `ROADMAP.md Phase 6 item N` citations inside the falsifier crates

The 2026-08-14 split moved Phase 6 into `DONE.md` and left a redirect stub in
`ROADMAP.md`, so these still resolve. But `ROADMAP.md`'s only phase-numbered content is
now Phase 7, whose items are numbered 1-6 — so a reader who greps `ROADMAP.md` for
"item 1" finds Phase 7's item 1, not the Phase 6 item the citation meant. The citations
live in `ranting_i18n/{README.md,src/*.rs,tests/*.rs}` and `ranting_es/` equivalents.

Options: leave them (the stub works), rewrite the filename only (~60 edits, mechanical),
or leave the source citations and fix only the two READMEs' top-of-file links. Not done
either way — it is churn with a real but modest payoff, and worth a decision rather than
a default.

### 4.4 Untested behaviors that are documented as working

- The `we` + `Person::Second` one-way rendering (`CLAUDE.md`) has no test.
- `Many`/`Maybe`/`Box` delegation of `is_first_person_subject_custom` is implemented at
  `collections.rs:147,576,877` but no test wraps a fork's impl in a wrapper.
- `elide_article_custom`/`_with_context` is overridden by **neither** falsifier crate —
  already noted as Phase 7 item 1's motivating finding, repeated here because it means the
  hook's shape has never been validated against a language that needs it.

### 4.5 The template keyword vocabulary is English, and cannot be localized by any hook

Raised by the maintainer 2026-08-14, from `docs/EXTENSIBILITY.md`'s "For New Language
Modules" example. That example is *correct* — it shows `say!("{the 0 be} noir.", word)`
rendering `"Le chat est noir."`, i.e. English keywords producing French output through the
hooks. The observation is that a French author would rather write `dis!("{le 0 suis}
noir.", word)`. Measured, against a scratch crate depending on `/work`:

| Written | Result |
|---|---|
| `dis!(...)` as a macro alias | **Works today.** `pub use ranting::say as dis;` is enough; nothing in the crate prevents it |
| `{the chat suis}` — French verb in the post-noun slot | **Silently wrong**: renders `"The chat suises noir."`. The slot accepts an arbitrary word and applies English conjugation to it. No error |
| `{le 0}` — French article in the pre-noun slot | **Misleading compile error**: `le` is not in the article keyword list, so it falls through to implicit-variable lookup and rustc reports `cannot find value 'le' in this scope` — naming a variable the author never wrote |

The article keyword list is matched at `ranting_derive/src/lib.rs:886-888` (`"the"`,
`"a"|"an"|"some"`, `"these"|"those"`) during macro expansion. `inflect_article_custom`
decides what `the` *renders as*; it cannot make `le` a recognized keyword, because the
keyword is consumed before any runtime hook exists. So this gap is not reachable from the
hook surface Phase 6 built, and no addition to that surface would close it.

Evidence that the friction is real rather than hypothetical: `ranting_es`'s own tests
write `say!("Vengo de {the *=0}.", ...)` — an English article keyword inside a Spanish
template. Both falsifier crates do this throughout; neither README records it as a hole,
because both were written to test the *hooks*, and the keyword vocabulary sits upstream of
them.

Distinct from the word-order boundary (Phase 6 item 20, `docs/EXTENSIBILITY.md` §2.12).
That boundary says the *order* of words is the caller's template; this is about the
*sigil vocabulary inside a placeholder* being English regardless of template. A fork can
already choose its own word order and its own rendered output — it cannot choose the
keywords it writes to request them.

Not scheduled, not designed. Recorded because it is currently unrecorded anywhere, and
because the two failure modes above (silent mis-conjugation, and an error naming a
phantom variable) are worth knowing about independently of whether localized keywords are
ever adopted. A Phase 7 spike is the natural home if it is pursued.

### 4.6 Stray artifacts in `ranting_derive/`

A 405 KB `tags` file and `mksrc.sh` sit in the crate root. Both are tracked-or-present dev
artifacts unrelated to the crate's source.

### 4.7 The falsifiers cannot see a defect in derive-generated `inflect()` (added 2026-08-14)

Both reference lexicons hand-write `Ranting::inflect` (`ranting_i18n/src/noun.rs:164`,
`ranting_es/src/noun.rs:95`), because suffix arithmetic cannot produce `Füchse` or `voces`.
That is correct for them and is not the finding. The finding is what it costs: **no crate in
this repo exercises the derive-generated `inflect()` fallback against non-English input**, so
the falsification apparatus is structurally blind to English orthography leaking through that
path — the one path a fork gets by default, before it decides to override anything.

It cost something concrete already. ROADMAP.md Phase 7 item 10 inferred "the struct declared
its own rule" from the attribute *values* (`singular_end.is_empty() && plural_end == "s"`),
which made `#[ranting(plural_end = "s")]` — bare append, no orthography, exactly what a German
or Dutch loanword plural needs — indistinguishable from the default, and left `ranting::Noun`
with no opt-out at all. All six crates' gates passed; the defect surfaced only when a reader
asked what the change does to a non-English *caller* rather than to a non-English `Ranting`
*impl*. It was fixed structurally (see item 10's entry and `docs/EXTENSIBILITY.md` §2.15), but
the blind spot that hid it is unchanged.

**Narrowed, not closed, 2026-08-14.** ROADMAP.md Phase 7 item 11 added
`tests/ranting/third_number.rs`, which is the first thing in this repo to exercise
`Ranting::inflect` against non-English input at all — it hand-writes a three-number Arabic noun
and asserts the rendered output. That closes the *hand-written* half: a defect in what `inflect`
is handed, like item 11's own missing count, is now visible to the suite. It does **not** close
the half described above, which is about the *derive-generated* fallback. A fork that overrides
nothing still runs a path nothing in this repo checks against a non-English name.

The residue is not scheduled. The obvious repair — a falsifier that keeps the derived `inflect()` — is in
tension with what the falsifiers are *for*, since a language whose plurals fit suffix
arithmetic is not a language that stresses the API. A cheaper option is a test in `tests/`
that pins derived-`inflect()` output for a handful of non-English names, which is what
`tests/ranting/regular_plurals.rs` now partly does. Recorded so the next audit does not have
to rediscover that "six green gates" excludes this path.

## 5. What this review deliberately did not do

- Did not rewrite any doc to match code where the **code** looked like the bug (§1.1-1.4
  are logged here, not documented as behavior).
- Did not bump any version, add any `exclude` key, or delete any stray file.
- Did not record line counts, per-file test counts, or unused-export inventories, except
  where a doc made a claim about them that turned out wrong.

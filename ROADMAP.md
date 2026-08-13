# ROADMAP.md

**Ranting** is a lightweight, declarative library for ergonomic, grammatically-correct formatted output in Rust.

---

## Vision

Ranting solves the problem of writing natural-sounding, dynamic user-facing text in Rust. Developers should be able to write grammar rules once and let the library handle inflection automatically—no manual string juggling, no brittle conditional text generation.

**Target**: Game engines, interactive fiction, chatbots, and any application that generates user-visible prose.

**Design principle**: Lightweight and ergonomic. Users write clean, declarative placeholders; Ranting handles the grammar.

---

## Current State (v1.0.0)

✅ **v1.0 Complete** — See DONE.md for full Phase 1 & Phase 2 details.

**Features Working**:
- All 7 tenses: Present, Past, Future, Present Continuous, Past Continuous, Present Perfect, Past Perfect
- 118+ irregular verbs with phonetic rules
- Gender-neutral pronouns (singular they/them)
- Named arguments, positional arguments, empty placeholders
- >200 tests (21 unit + 171 integration + 8 doctests)
- Tutorial + 10-recipe Cookbook published
- Zero critical issues

---

## Phase 3 — v1.1.0 — Plurals, Extensibility, and Ecosystem

*Goal: Complete core morphology; enable community contributions.*

**Timeline**: 8-12 weeks post-v1.0 release

### Completed Features

✅ **1. Irregular Noun Plurals** (COMPLETE — 28 hours)
   - ✅ Support 100+ common irregular plurals: child→children, person→people, mouse→mice, goose→geese, etc.
   - ✅ Codegen from data/irregular_plurals.txt (single source of truth, like verbs)
   - ✅ Integrated with existing `#[ranting(plural_end="...")]` attribute system
   - ✅ **Wired 2026-08-13**: `english::inflect_noun_irregular` (backing
     `Ranting::inflect()`'s irregular-noun path) now delegates to
     `get_plural`/`get_singular` in `src/language/plurals.rs` instead of a
     separate duplicate table scan — see `docs/architecture-review-2026-08-13.md`
     for the pre-fix history.
   - ✅ 9 new integration tests + unit tests (217 total tests passing)
   - ✅ Case-preserving lookups (child→Children when capitalized)

### Upcoming Priority Features

✅ **2. Trait-Based Inflection Extensibility** (COMPLETE — 16-20 hours)
   - ✅ Add trait methods for custom grammar rules via `inflect_verb_custom()`, `inflect_pronoun_custom()`, `inflect_article_custom()`
   - ✅ Default impls use `None` (zero breaking changes); users override for domain-specific needs
   - ✅ `PronounCase` enum for pronoun customization (Subjective, Objective, PossessiveDeterminer, PossessivePronoun)
   - ✅ Runtime fallback to English rules when custom method returns `None`
   - ✅ Enables ecosystem forks (ranting-spanish, ranting-pirate, ranting-scottish, etc.)
   - ✅ Full documentation in `docs/EXTENSIBILITY.md` with pirate, Scottish, Spanish examples
   - ✅ 9 integration tests verifying full/partial customization and fallback behavior

✅ **3. Runtime Tense & Viewpoint Selection** (COMPLETE — 16-20 hours) — *Unblocks Recounting M9*
   - ✅ Runtime tense selection: `say_with!(context, "...", args...)` resolves `<`,`=`,`>`,`<=`,`%`,`<%` markers against a runtime `NarrationContext { tense: Option<Tense> }` (7-variant `Tense` enum), falling back to the placeholder's own marker when no override is given. `say!()` is unaffected — unchanged codegen and output.
   - ✅ Context-passing mechanism decided: explicit `say_with!(context, ...)` macro parameter (matches the library's existing explicit-argument style; rejected trait-method — conflates entity `subject` with story-wide settings per item 4 — and thread-local — implicit, fragile across tests/async).
   - Prerequisite this uncovered: `ranting_derive` is a `proc-macro = true` crate and can only export `#[proc_macro]` items, so `ranting` could not call its compile-time verb conjugation functions at runtime. Resolved by making `src/language/verb_conjugate.rs` (repo root) canonical and having `ranting_derive` consume a build-time-generated copy — the inverse of the `english_shared.rs` direction (see CLAUDE.md).
   - ✅ Runtime viewpoint selection: `NarrationContext.narration_person` (`Person::First`/`Second`/`Third`) overrides pronoun set and verb agreement, scoped to nouns declared first-person (`subject` is `"I"`/`"we"`) — the narrator only; other subjects, and other nouns in the same call, pass through unchanged. Third-person rendering falls back to singular "they" (no gender data on a first-person-declared noun to render a gendered pronoun instead); `we`→`Person::Second` renders "you" as a one-way conversion, not round-trippable. See `narration::resolve_viewpoint` in `src/narration.rs` and the CLAUDE.md "Non-obvious behaviors" entry.
   - Backwards-compatible: compile-time markers remain default if context not provided ✅

✅ **4. Narration Context Threading** (COMPLETE — 12-16 hours) — *Supports feature 3*
   - ✅ `NarrationContext` gained `register: Option<Register>` (`Formal`/`Neutral`/`Casual`) and
     `dialect: Option<&'static str>` fields, alongside item 3's `tense`/`narration_person`. Both
     new fields are inert in the crate itself — no built-in English behavior reads them — until a
     `Ranting` implementation consults them.
   - ✅ Integration point: three new default `Ranting` trait methods,
     `inflect_verb_custom_with_context`/`inflect_pronoun_custom_with_context`/
     `inflect_article_custom_with_context`, each taking `ctx: Option<&NarrationContext>` as a
     parameter (never read off `self`) and delegating to the existing non-context hook by
     default. Every verb/pronoun/article call site in `handle_placeholder_impl` now calls the
     `_with_context` hook (`say!()` passes `None`, `say_with!()` passes `Some(ctx)`), so
     overriding only the `_with_context` hook is sufficient — no need to also override the
     original.
   - ✅ `subject` stays an entity property (`Ranting::subjective()`); `register`/`dialect`, like
     `narration_person`, are story-wide settings that only ever arrive via the `ctx` parameter —
     the trait-method-sourced-context design was already rejected in item 3 for conflating the two.
   - ✅ Ecosystem forks: 8 integration tests in `tests/ranting/narration_context_threading.rs`,
     including register-driven verb/article choice and dialect-driven pronoun choice (each via a
     custom `impl Ranting` overriding only the relevant `_with_context` hook), a sentinel proving
     `say!()` calls the `_with_context` hook rather than the plain one, and a fallback check for
     when no register/dialect override applies.
   - ✅ `say!()` unaffected: its call sites pass `ctx: None` to the same `_with_context` hooks, so
     existing `say!()` output is unchanged (verified by `say_macro_still_passes_none_to_context_hooks`).

✅ **5. Reflexive Forms** (COMPLETE — 8-12 hours)
   - ✅ Support myself, yourself, thyself, himself, herself, itself, ourselves,
     yourselves, themselves — the full 9-pronoun set (`SubjectPronoun`'s
     exhaustive match already covers `thou`/`ye` alongside the 7 forms named in
     this item, so the reflexive table follows suit for consistency).
   - ✅ Case marker integration: new `%` case marker (`{%person}`) dispatches to
     `PronounCase::Reflexive` in `handle_placeholder_impl`, routed through the
     existing `inflect_pronoun_custom_with_context` hook first (so a custom
     `Ranting` impl can override reflexive forms, e.g. formal "their own royal
     person") before falling back to `ranting::inflect_reflexive` in
     `src/language/english.rs`. `%` was chosen over the roadmap's illustrative
     `~` because `~` is already the `PossessivePronoun` marker (mine/yours/...);
     reusing it would have broken existing behavior. Regex change lives only in
     the canonical `src/language/english_shared.rs` (`PH_EXT`'s `case` group),
     copied to `ranting_derive` per the existing build.rs mechanism — no
     `ranting_derive` source changes needed.
   - ✅ Completes core pronouns system. Also fixed the `upper()` test in
     `src/language/english.rs`, which previously hardcoded the byte-artifact
     `"theirself"` (possessive `` ` `` + literal `"self"` string concatenation)
     as a stand-in for real reflexive support — it now uses `{%w}` and asserts
     the correct `"themselves"`.
   - ✅ 10 integration tests in `tests/ranting/reflexive_pronouns.rs` (all 9
     pronouns, singular-they reflexive, sentence-start/mid-sentence
     capitalization, forced plural/singular via `+`/`-`, `you` singular vs.
     plural, positional and named args, and custom-hook override/fallback via
     `PronounCase::Reflexive`), plus unit tests in `src/language/english.rs`.

6. ✅ **Comparative & Superlative Adjectives** (10-16 hours)
   - ✅ Handle degree: good → better → best, bad → worse → worst, plus regular
     `-er`/`-est` (fast → faster, big → bigger with CVC doubling, happy →
     happier with y→i) and periphrastic `more`/`most` for longer adjectives
     (beautiful → more beautiful), via `ranting_derive/src/language/adjective.rs`.
   - ✅ Marker-based syntax: new post-noun `!`/`!!` markers (`{noun !word}` for
     comparative, `{noun !!word}` for superlative), baked entirely at compile
     time into a `~DEGREE~WORD[:TRAILING]` sentinel that `handle_placeholder_impl`
     in `src/lib.rs` emits verbatim (with capitalization applied). `!` was
     chosen over the roadmap's illustrative `{+person good}` because `+`/`-`
     already force noun plurality/singularity (see README.md) — reusing them
     would have broken existing behavior, the same collision the Reflexive
     Forms item (Phase 3 item 5) hit and resolved by picking `%` over the
     roadmap's illustrative `~`. `!` was free: its only other appearances are
     sentence-end punctuation detection outside `{}` and a `pre`-slot article
     marker unreachable from the current `PH_EXT` grammar, neither in the
     `post` capture group `!`/`!!` now uses.
   - ✅ Unlike verb tense, degree needs no subject/number/`NarrationContext`
     agreement at runtime, so — following the `irregular_plurals.txt`
     codegen-from-data-file pattern — the irregular table
     (`data/irregular_adjectives.txt`, symlinked into `ranting_derive/data/`)
     and its derived Rust table are generated by `ranting_derive/build.rs`
     only; `ranting` needs no runtime copy, no repo-root canonical
     `include!()` wrapper, and Phase 4's "don't extend the copy mechanisms"
     warning doesn't apply here.
   - ✅ 8 integration tests in `tests/ranting/comparative_adjectives.rs`
     (irregular lookup, monosyllabic suffix + CVC doubling, two-syllable
     consonant+y, periphrastic more/most, sentence-start capitalization,
     trailing words after the degree word).
   - ✅ Rounds out morphology for richer text generation.

7. ✅ **Recursive Type Inflection** (12-16 hours)
   - ✅ Support collections and nested Ranting types: `Vec<Item>`/`Option<Person>`/
     `Box<Noun>` (and any `T: Ranting`) as `say!()` placeholder subjects/arguments.
   - ✅ `Box<T>` gets a direct blanket `impl<T: Ranting> Ranting for Box<T>`, in
     `src/collections.rs` — `Ranting` is local to this crate so implementing it
     for foreign `Box<T>` is allowed by the orphan rules, and `std` already
     provides `impl<T: Display> Display for Box<T>` so the `Ranting: Display`
     supertrait bound is satisfied for free.
   - ⚠️ `Vec<T>` and `Option<T>` could **not** get the same direct blanket-impl
     treatment as originally scoped: `Ranting: Display` requires `Vec<T>`/
     `Option<T>` to implement `Display`, but `Display` is a foreign trait and
     neither `Vec` nor `Option` is `#[fundamental]` (unlike `Box`/`&`), so
     `impl Display for Vec<T>`/`Option<T>` is rejected by the orphan rules
     (E0117) no matter what `T` is — not a case-by-case gap, a hard compiler
     rule. Relaxing the `Display` supertrait to work around it would be a
     breaking public-API change, out of scope here (Phase 4 owns public-API
     cleanup). Resolved with two local newtype wrappers instead — `Many<T>(pub
     Vec<T>)` and `Maybe<T>(pub Option<T>)` in `src/collections.rs` — which
     *are* local types and so can freely implement both `Display` and
     `Ranting`; users wrap their `Vec`/`Option` before passing it to `say!()`.
     This is the same "local wrapper sidesteps the orphan rule" shape already
     used by `boxed_ranting_trait!`/`ref_ranting_trait!` in
     `ranting_derive/src/lib.rs` for `Box<dyn Trait>`/`&dyn Trait`, except
     those work because `Box`/`&` are themselves `#[fundamental]`, which `Vec`/
     `Option` are not — so a macro-generated downstream impl couldn't have
     closed this gap either.
   - ✅ `Many<T>` is a collective noun phrase: rendered name joins items' own
     `name()`s as `"a, b and c"`; plural (`"they"`/`"are"`) whenever the `Vec`
     doesn't hold exactly one item (zero included — "there are no items"),
     delegating plurality/pronoun/custom-hook behavior straight through to the
     single item when there is exactly one, and falling back to built-in
     English rules for custom hooks when there are zero or several items
     (there's no single item to delegate a per-item override to).
     `skip_article()` is `true` when empty, so `{a items}` doesn't leave a
     dangling article.
   - ✅ `Maybe<T>`: `Maybe(Some(x))` delegates every method straight through to
     `x`; `Maybe(None)` renders as empty, singular, subject `"it"`, and also
     `skip_article() == true`.
   - ✅ Not implemented via `#[derive(Ranting)]` as the roadmap entry
     illustrated — `Vec`/`Option`/`Box` are foreign `std` types, not structs
     the crate's own derive macro is invoked on, so there is nothing for
     `#[derive(Ranting)]` to attach to here; the derive macro is unrelated to
     this item's implementation.
   - ✅ All six `inflect_*_custom`/`inflect_*_custom_with_context` hooks (not
     just the five required trait methods) are forwarded by `Box`/`Many`
     (single-item case)/`Maybe`, so a wrapped type's custom overrides (e.g. a
     `ranting-pirate`-style fork) survive wrapping instead of silently
     reverting to English defaults.
   - ✅ 13 integration tests in `tests/ranting/recursive_inflection.rs` (empty/
     single/multi-item `Many`, `Maybe(None)`/`Maybe(Some)`, `Box<Noun>`,
     `Box<Many<Noun>>`/`Many<Box<Noun>>` nesting/composition, uppercase-first-
     char-only join, and custom-hook forwarding through all three wrapper
     types plus the multi-item English-fallback case), plus 2 doctests in
     `src/collections.rs`.

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

### v1.1 Success Criteria
- Irregular plurals support for 100+ nouns
- Trait extensibility API stable and documented
- Runtime tense & viewpoint selection working (unblocks Recounting M9)
- Narration context threading designed and integrated
- Reflexive forms + comparative/superlative working
- Zero breaking changes from v1.0
- Community contributions: 2-3 ecosystem forks
- GitHub engagement: 10+ answered issues/discussions

---

## Phase 4 — v1.2.0 — Architecture Consolidation (Design Review, 2026-08-12)

*Goal: pay down structural debt while there is no userbase to break. Every rename,
crate split, and API change is cheap now and expensive after ecosystem forks exist —
so this phase deliberately precedes Ecosystem Expansion.*

Prioritized (1-2 together delete most of CLAUDE.md's "key constraints"):

1. ✅ **Extract `ranting_core` shared crate** (highest leverage)
   - ✅ Replaced all three code-sharing mechanisms (`english_shared.rs` build.rs
     copy, `verb_conjugate.rs` inverse copy, `irregular_verbs.txt` per-crate
     codegen) with one plain rlib crate, `ranting_core`, that both `ranting`
     and `ranting_derive` depend on as an ordinary path dependency (`ranting_core
     = { version = "0.1.0", path = "../ranting_core" }` from `ranting_derive`,
     `path = "ranting_core"` from `ranting`) — the `serde`/`serde_derive`
     pattern. Both consumers still build with independent `Cargo.lock` files,
     same as before (`ranting_derive` already published standalone; `ranting_core`
     is set up the same way, versioned and publishable, even though it isn't
     published yet).
   - ✅ Moved in: `ranting_core::grammar` (placeholder grammar `PH_START`/
     `PH_EXT`, `SubjectPronoun`, `is_subject`/`is_subjective_plural`/
     `is_first_person_subject`, and the still-unused `Article`/
     `DemonstrativePronoun` enums) and `ranting_core::verb_conjugate`
     (`to_past`/`to_continuous`/`to_future`/`to_past_participle` plus the
     `IRREGULAR_PAST`/`IRREGULAR_PAST_PARTICIPLE` tables, now generated exactly
     once by `ranting_core/build.rs` from `data/irregular_verbs.txt` instead of
     independently by both `ranting`'s and `ranting_derive`'s build.rs scripts).
   - ✅ Deleted: both crates' build.rs copy-into-`OUT_DIR` steps for the code
     above, `ranting_derive/data/english_shared_source.rs`,
     `ranting_derive/data/verb_conjugate_source.rs`, and
     `ranting_derive/data/irregular_verbs.txt` (superseded by `ranting_core`'s
     own `data/irregular_verbs.txt` → `../../data/irregular_verbs.txt`
     symlink). The dual-strum (0.24 + 0.27) compilation constraint is gone:
     `ranting_core` depends on `strum`/`strum_macros` 0.27 only (standardizing
     on the newer version per this item's own design guidance, anticipating
     part of item 2), and `ranting_derive`'s `Cargo.toml` no longer lists
     `strum`/`strum_macros` at all.
   - ⚠️ **Deviation — `SubjectPronoun` had to become `pub`, not just
     `pub(crate)`**: it now lives in a different crate from every call site, so
     `ranting`'s `src/language/english.rs` needs `ranting_core::grammar::
     SubjectPronoun` to be visible across the crate boundary. This is a small,
     unplanned head start on item 4's first bullet ("make `SubjectPronoun`
     public") — only the visibility changed here, not `FromStr`-based storage
     in `Noun` or the panic-removal item 4 actually scopes; item 4 still owns
     the rest of that work.
   - ⚠️ **Deviation — inherent `impl SubjectPronoun { fn forms(...) }` became a
     free function**: an inherent `impl` on a foreign type (E0116) is illegal
     once `SubjectPronoun` isn't local to `ranting` anymore. Converted to a
     free function `pronoun_forms(subject: SubjectPronoun) -> PronounForms` in
     `src/language/english.rs`; all five call sites (`inflect_adjective`,
     `inflect_subjective`, `inflect_objective`, `inflect_possessive`,
     `inflect_reflexive`) and the pronoun-table test updated accordingly. No
     behavior change, just a mechanical consequence of the crate split.
   - ⚠️ **Design call — what did *not* move into `ranting_core`**:
     `ranting_derive/src/language/plurals.rs`'s `get_plural`/`get_singular`/
     `apply_case` stayed hand-duplicated against `src/language/plurals.rs`
     rather than consolidating — both copies are still dead code (not wired
     into any inflection call site in either crate; pre-existing drift risk,
     see `docs/architecture-review-2026-08-13.md`), and moving genuinely-dead
     code into the shared crate wouldn't resolve that, only relocate it.
     Wiring plurals up (or consolidating the lookup functions) remains
     unclaimed work, as CLAUDE.md already noted before this item.
     **Update 2026-08-13**: the `ranting`-side copy is wired now —
     `english::inflect_noun_irregular` delegates to `get_plural`/`get_singular`
     instead of re-implementing the same table scan, so `Ranting::inflect()`'s
     irregular-noun path uses them for real and gains `apply_case`'s
     case-preservation as a side effect (previously lost, since the caller only
     applied `uc_1st_if`'s first-letter rule). The `ranting_derive`-side copy
     was deleted the same day (`src/language/plurals.rs`, its
     `pub mod plurals;` in `mod.rs`, and the `generate_plurals_table`
     codegen + `IRREGULAR_PLURALS` table in `ranting_derive/build.rs`) rather
     than left permanently dead — noun pluralization happens at
     `Ranting::inflect()` runtime, never at `ranting_derive` compile time, so
     it never had a call site to wire into; this also closed out
     `ranting_derive`'s standalone `cargo clippy -D warnings` failure (see
     Phase 4 item 6's follow-up note below). `ranting_derive/src/language/adjective.rs` (comparative/superlative
     tables, Phase 3 item 6) also stayed put — `ranting` has no runtime use
     for it, so it was never one of the three copy mechanisms this item
     targeted; CLAUDE.md's original phrasing already called this out as a
     derive-only table with no repo-root canonical copy.
   - ✅ Full gate clean after the extraction: `cargo fmt --check`, `cargo
     clippy --all-targets -- -D warnings` (root — the only pre-existing
     warnings are the already-known `ranting_derive/src/language/plurals.rs`
     dead-code ones, now surfacing as build-dependency warnings rather than
     workspace-member errors, plus `IRREGULAR_PLURALS`/`Article`/
     `DemonstrativePronoun` dead code, exactly as expected), and `cargo test`
     all pass. Test count: the 11 verb-conjugation proptest/example tests that
     lived in `src/language/verb_conjugate.rs` moved intact to
     `ranting_core/src/verb_conjugate.rs` and still pass there (`cargo test`
     from `ranting_core/`) — `ranting`'s own `cargo test` count dropped from
     302 lib+integration tests to 291 for exactly that reason (302 − 11 = 291;
     252 integration tests and the 11 doctests are unaffected), not because
     any test was deleted. `ranting_derive`'s pre-existing doctest failure
     (`src/lib.rs - derive_ranting`, missing `ranting` as a dev-dependency so
     `use ranting::say` can't resolve) is unchanged by this item — verified
     identical on the pre-extraction commit, not a regression.
   - Note on the `Cargo.lock` diffs: `ranting_derive/Cargo.lock`'s diff is
     large (~350 lines) but not a smuggled dependency bump — its
     `[dev-dependencies] proptest = "1"` was declared in `Cargo.toml` but had
     never actually been resolved into that lockfile before (no `proptest` or
     transitive `rand`/`bit-set`/`tempfile`/etc. entries existed pre-change),
     so running `cargo test` there for the first time in a while pulled in
     the whole dev-dependency tree at once, on top of the intentional
     strum 0.24→0.27 bump (which also pulls a transitive `syn 2.0.119` in
     alongside `ranting_derive`'s own unchanged direct `syn = "1.0.98"`) and
     ordinary semver-compatible `proc-macro2`/`quote` point bumps from
     unifying the whole graph on one version. Root `Cargo.lock`'s diff is the
     expected small one: `strum`/`strum_macros` 0.24 (and their now-orphaned
     `heck` dependency) removed, `ranting_core` added.
   - Note on `ranting_core`'s build.rs: unlike `ranting_derive`'s old
     `generate_verbs_table`, `ranting_core/build.rs` does not assert an exact
     entry count (`expected_count = 118`) against `data/irregular_verbs.txt`
     — only the pre-existing duplicate-base check was kept, since a hardcoded
     count is brittle against legitimate future additions to the data file
     and wasn't load-bearing for correctness.
   - Note on `ranting_core` versioning: both consumers depend on it via
     `version = "0.1.0", path = "..."` (not an exact `=0.1.0` pin, unlike
     `ranting`'s `ranting_derive = "=0.2.1"`). Since `ranting_core` isn't
     published yet and both consumers are edited in lockstep in this repo,
     this is low-risk today; if/when `ranting_core` is published independently,
     revisit whether it should move to an exact pin like `ranting_derive`'s.

2. ✅ **Dependency modernization** (`ranting_derive`)
   - ✅ syn 1.0.98 → 2 (`syn = { version = "2.0", features = ["full"] }`), darling
     0.14.1 → 0.20.11 (0.24 exists on crates.io; stopped at 0.20 deliberately —
     it already satisfies the item's "0.20+" floor and 0.20 vs. 0.24 wasn't
     forced by any compile error, so there was no reason to chase newer).
     Both bumps compiled **with zero source changes required** in
     `ranting_derive/src/{lib,ranting_impl,heed,str_lit}.rs` — the crate's
     usage of `syn`/`darling` (parsing `DeriveInput`, `Expr`, building output
     via `quote!`/`parse_quote!`, `FromDeriveInput`) turned out not to touch any
     of the APIs that moved between those major versions (no `parse_meta`/
     `NestedMeta`/`AttributeArgs` usage — darling owns all attribute parsing
     here). Verified this was a real fresh compile against the new versions,
     not a stale/cached artifact: build output shows `Compiling syn v2.0.119`
     and `Compiling darling v0.20.11` from scratch, and `cargo tree` (both
     from `ranting_derive/` and the repo root) shows zero occurrences of
     `syn v1` or `darling v0.14` anywhere in the resolved graph afterward.
   - ✅ **Strum: already resolved by item 1, nothing left to do here.** Before
     starting, checked `ranting_derive/Cargo.toml` and confirmed item 1's
     `ranting_core` extraction had already removed `strum`/`strum_macros` from
     `ranting_derive`'s own dependency list entirely (not just unified the
     version — dropped outright, since `SubjectPronoun` and its parsing now
     live in `ranting_core`). `cargo tree -i strum` from both `ranting_derive/`
     and the repo root confirms exactly one `strum v0.27.2` in the graph,
     reached transitively through `ranting_core`. So neither of this item's two
     original options ("unify on 0.27" / "hand-write parsing to drop strum from
     the derive path") applied — the derive path never touches strum at all
     post-extraction, direct or transitive-only-for-itself. Text above and in
     item 1 was already accurate; no rewrite needed there, just confirmation.
   - ✅ **Dropped `proc-macro-error`.** Turned out to be dead weight even before
     this change — `grep` found zero uses of `proc_macro_error`/`abort!`/
     `emit_error!`/`abort_call_site!` anywhere in `ranting_derive/src/`; the
     crate already did its own diagnostics via `syn::Error`/`Result` plumbing
     (`parse_str_params` etc. already returned `syn::Result`). Removed the
     `Cargo.toml` line; no source changes needed since nothing referenced it.
   - ✅ Dropped `lazy_static` in both `ranting` and `ranting_derive` →
     `std::sync::LazyLock`. Three call sites converted, all the
     `lazy_static! { static ref X: T = expr; }` → `static X: LazyLock<T> =
     LazyLock::new(|| expr);` shape (`src/lib.rs`'s `OF` regex;
     `ranting_derive/src/lib.rs`'s `PH`/`PHE` and `POSS` regexes). `OnceLock`
     wasn't needed anywhere — every site was "compute once lazily on first
     access," not an explicit set-once pattern, so `LazyLock` is the correct
     fit per the item's own guidance. `heed.rs`'s pre-existing `OnceLock` usage
     (compiled-regex cache behind `HeedMatcher`, a genuine set-once-explicitly
     case) is unrelated and untouched.
   - ✅ Bumped `ranting_derive`'s regex 1.6.0 → 1.11 (landed at 1.13.1 via `^1.11`
     resolution — still satisfies the stated floor). Straightforward version
     bump, no API changes hit; `heed!()`'s regex-version-independence from
     `ranting`'s own regex dep (documented in CLAUDE.md) is unaffected since
     that design was never about the *version* matching, just the type not
     crossing the crate boundary.
   - ✅ Unified editions to 2024: `ranting_derive` and `ranting_core` both
     2021 → 2024 (root `ranting` was already on 2024 from an earlier session).
     Done last, after the syn 2 migration was compiling and green, per the
     item's own sequencing note. Only effect observed was `cargo fmt`
     reformatting import blocks to 2024-edition-idiomatic grouping/ordering in
     `ranting_derive/src/{lib,ranting_impl,str_lit,heed}.rs` and
     `src/language/adjective.rs` (e.g. `use quote::{ToTokens, quote}` ordering,
     multi-line `parse_quote!`/`include!` wrapping) — no behavioral diffs, no
     new clippy findings, no test regressions.
   - ✅ `cargo audit` (installed fresh via `cargo install cargo-audit --locked`,
     network was available in this environment) ran clean — 0 vulnerabilities,
     exit 0 — for all three `Cargo.lock`s (root `ranting`: 63 crates,
     `ranting_core`: 43, `ranting_derive`: 57). `cargo-deny` was not installed
     and not attempted to install (audit alone was sufficient to confirm the
     RUSTSEC-clean claim this item cared about — the `proc-macro-error`
     advisory that motivated this item is now unreachable from any of the
     three dependency graphs). Confirmed via `cargo tree` that `proc-macro-
     error`, `lazy_static`, and `syn v1` no longer appear anywhere in either
     `ranting` or `ranting_derive`'s resolved graph.
   - ✅ Gate green on all three crates standalone (`cargo fmt --check` +
     `cargo clippy --all-targets -- -D warnings` + `cargo test`, run from each
     crate's own directory, re-run *after* `cargo fmt` reformatted files so the
     reported results reflect the final tree, not a pre-fmt snapshot): root
     `ranting` clean (252 tests + 11 doctests, 4 ignored); `ranting_core` clean
     (11 tests, 0 doctests). `ranting_derive` standalone clippy reproduces
     exactly the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead `IRREGULAR_PLURALS`/
     `get_plural`/`get_singular`/`apply_case` in `plurals.rs`, one
     `map_or`→`is_some_and`, two needless-borrow lints) — confirmed identical
     before/after via `git stash` (same 7-error list both times), so nothing
     new was introduced and none of the pre-existing ones were incidentally
     fixed by this migration, despite `lib.rs` itself being touched (by the
     `LazyLock` conversion and `cargo fmt`) — the specific lines clippy flags
     there were untouched by either. `ranting_derive`'s one
     doctest failure (`src/lib.rs - derive_ranting`, `unresolved import
     'ranting'`) is the same pre-existing proc-macro-crate-can't-self-test
     limitation CLAUDE.md already documents ("test in `ranting/src/lib.rs`
     instead") — also confirmed identical via `git stash`, not a regression.
   - ✅ Also verified under the `debug` feature (`#[cfg(feature = "debug")]`
     `eprintln!` diagnostics in `parse_str_params`, exercised by CLAUDE.md's
     documented `cargo test --features debug` command and the
     `tutorial::section_5_debug_feature` integration test) — a default-features
     gate run alone wouldn't compile that cfg'd branch at all. Root
     `cargo test --features debug`: same 252 tests + 11 doctests, all green.
     `ranting_derive`'s `cargo clippy --all-targets --features debug -- -D
     warnings` surfaces two additional pre-existing findings beyond the 7 above
     (`to_string` applied to a `Display` type inside `eprintln!` args, at
     `lib.rs:294` and `lib.rs:430`) — same `git stash` before/after check shows
     the identical 9-finding list both times, so these predate this migration
     too and nothing new was introduced under `--features debug` either.
   - Payoff realized: `proc-macro-error` and `syn 1` (and its old duplicate
     `unicode-ident`/`quote` sub-tree) are gone from every downstream build.

3. ✅ **Typed placeholder spec across the compile-time/runtime seam**
   - ✅ Added `ranting_core::placeholder` (`ranting_core/src/placeholder.rs`),
     a new shared module with `PlaceholderSpec { pre, plurality, noun_space,
     case: CaseKind, post: PostSpec }`, `CaseKind` (`Name`/`Hidden`/
     `Subjective`/`Objective`/`PossessiveDeterminer`/`PossessivePronoun`/
     `Reflexive` — one variant per `case` capture-group character), and
     `PostSpec` (`None`/`PossessiveS`/`Verb(&'static str)`/`Tense { marker:
     TenseMarker, word, trailing, leading_space }`/`Degree { word, trailing,
     leading_space }`) with `TenseMarker` (`Past`/`Continuous`/`Future`/
     `PastContinuous`/`PresentPerfect`/`PastPerfect`). `ranting` re-exports
     the module (`pub use ranting_core::placeholder;`) so macro-generated
     code, which only has a `ranting::` path available, can name the types.
   - ✅ `ranting_derive::handle_param` (`ranting_derive/src/lib.rs`) now
     classifies the post-noun word into a `PostSpec` variant *at compile
     time* and emits it as a struct-literal expression (`ranting::placeholder
     ::PlaceholderSpec { .. }`) baked directly into the generated
     `handle_placeholder`/`handle_placeholder_with_context` call — an
     all-const-fields struct literal is itself a compile-time constant
     expression, so this satisfies "baked by the macro as a const" without
     needing a separate named `const ITEM: T = ..;` declaration (which would
     need one uniquely-named item per placeholder in the expanded code for no
     added benefit). `handle_placeholder`/`handle_placeholder_with_context`
     (`src/lib.rs`) take `spec: PlaceholderSpec` in place of the old
     `caps: [&str; 5]`.
   - ✅ Deleted the `~TENSE~MARKER:WORD` and `~DEGREE~WORD[:TRAILING]` string
     sentinels entirely, along with every runtime `strip_prefix("~TENSE~")`/
     `strip_prefix("~DEGREE~")`/`split_once(':')`/`split_once(' ')` call and
     the two "fallback if marker/colon parsing fails" branches that existed
     only to handle malformed sentinels the macro could never actually
     produce — those states are unrepresentable now (`PostSpec::Tense`/
     `Degree` carry `marker`/`word`/`trailing` as separate typed/string
     fields the macro already split apart, not a single string to
     re-parse). Also fixed a latent (untested, so never user-visible)
     ordering bug this uncovered: the old `~TENSE~` sentinel, when a tense
     marker had trailing words (e.g. `{who <=go} home`), relied on
     `split_at_find_end`'s last-whitespace split to separate the sentinel
     from the trailing word, which — unlike the `~DEGREE~` sentinel, which
     was deliberately exempted from that split via a `starts_with` check for
     exactly this reason — would have pushed the raw, still-unparsed
     `~TENSE~<:word` prefix into the output verbatim; no test exercised a
     tense marker with trailing content, so this was never observed. The new
     `PostSpec::Tense { word, trailing, .. }` renders `word` then
     `" " + trailing` directly, matching the (already-correct)
     `PostSpec::Degree` handling, with no split involved.
   - ✅ **Fixed 2026-08-13** (originally deferred here, then revisited):
     `get_article_or_so`'s article-keyword classification (`"the"` /
     `"a" | "an" | "some"` / `"these" | "those"`) is now a typed
     `ranting_core::placeholder::ArticleKind`, baked at compile time by
     `ranting_derive`, mirroring the `PostSpec`/`CaseKind` treatment above.
     **The original deferral's stated blocker was wrong.** It named the
     second, *chained* `get_article_or_so(noun, s, ...)` call inside the
     `etc1` sub-parse as the problem, reasoning that `etc1` is "derived from
     `pre` only after the runtime backtick-possessive substitution." Tracing
     the actual control flow shows the opposite: that chained call lives
     inside the branch reached only when `has_possesive` (`pre_raw.contains('`')`)
     is `false` — meaning no backtick exists *anywhere* in `pre_raw` — so
     `pre.replace('`', poss.as_str())` is provably a no-op on every path that
     reaches it, and `etc1`/`s` are always compile-time-literal there. The
     part that *can* see runtime text is actually the **first** call
     (`pre`'s first word) when a `` ` `` possessive-substitution sentinel
     falls within it — but that's also provably safe to classify at compile
     time, because a possessive determiner/`Name's` form (the only thing
     that substitution ever produces) can never coincide with a real article
     keyword, so `ArticleKind::Other` is always correct for it without
     inspecting the actual runtime string.
     `ArticleKind::classify` (`ranting_core/src/placeholder.rs`) is the
     canonical reference implementation (unit-tested, including the
     case-sensitivity asymmetry between the two call sites: the first is
     lowercased before classifying, the second isn't — preserved exactly).
     `ranting_derive`'s `handle_param` bakes `PlaceholderSpec::pre_kind`/
     `pre_chained_kind` by replicating `handle_placeholder_impl`'s own
     `pre`/`etc1` splitting at compile time (reusing the same
     `split_at_find_start` helper already duplicated in that file), not by
     calling `ArticleKind::classify` itself (same pattern as `CaseKind`'s/
     `TenseMarker`'s own local `match`es — proc-macro build code just needs
     to emit the right `quote!` tokens, not call a `const fn`).
     `get_article_or_so` still owns the runtime-only parts unaffected by
     this — `Ranting::skip_article()`, the `inflect_article_custom_with_context`
     hook, and a/an/singular-vs-plural rendering. Two new permanent
     regression tests pin the behavior this was traced against:
     `tests/ranting/article_classification.rs`'s `chained_article_after_modal`
     (exercises the second call site directly — no prior test did) and
     `combined_verb_and_backtick_possessive` (the doctest-derived
     `` {can `man pair of #0 boots remain} `` case the original deferral
     worried about).
   - **The overloaded `=` marker, disambiguated**: `=` means "subjective
     pronoun case" before the noun and "continuous tense" after it. Before
     this item, both were bare `&str`s (`case: "="` vs. `post:
     "~TENSE~=:running"`), so the two meanings were told apart only by
     *which array slot the string sat in*, combined with a string prefix on
     one side. Now `case: CaseKind::Subjective` and `post:
     PostSpec::Tense(TenseMarker::Continuous, ..)` are different fields of
     different enum types — the two meanings can't be confused at the type
     level, and (per the point above) there's no shared string
     representation left that needs disambiguating. Documented in
     `ranting_core/src/placeholder.rs`'s module doc comment.
   - ✅ `say!()`'s output is unchanged (still bakes fully-conjugated
     literals through the same code path, now via `PostSpec::Tense`/`Verb`
     instead of the sentinel string); `say_with!()`'s runtime tense
     resolution (`NarrationContext.tense`) is unaffected — verified via the
     existing `tests/ranting/runtime_tense.rs` and
     `tests/ranting/verb_tense.rs` suites, unmodified and still green.
   - ✅ Added 2 regression tests locking in the trailing-word fix above:
     `verb_tense::tense_marker_with_trailing_words` (`say!()`) and
     `runtime_tense::tense_marker_with_trailing_words_runtime_tense`
     (`say_with!()`, both with and without a `NarrationContext.tense`
     override). Root `ranting`'s integration count is therefore 254, not
     252 — a deliberate increase (new coverage for a real, previously-latent
     bug this refactor fixed), not a regression; confirmed the *old* code
     actually produced the garbled `"He ~TENSE~<:went homes"` for
     `say!("{=0 <go home}", person)` via `git stash` before writing the test,
     so the note above isn't just reasoned but verified.
   - ✅ Gate green on all three crates standalone. Root `ranting`: 39 lib +
     254 integration + 11 doctests (4 ignored) — 252 of the integration
     tests and all lib/doctest counts are identical to before this item
     (verified against `git show c305cef7`), +2 for the regression tests
     above; also re-verified under `cargo test --features debug`.
     `ranting_core`: 11 tests, 0 doctests, unaffected (the new `placeholder`
     module has no tests of its own beyond what the `ranting`-crate
     integration suite already exercises end-to-end through
     `say!()`/`say_with!()`). `ranting_derive` standalone: 9 unit tests
     green; clippy reproduces the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead `plurals.rs` code, one
     `map_or`, two needless-borrow lints) under default features, and the
     same 9 (7 + 2 `to_string`-in-`eprintln!`) under `--features debug` —
     confirmed identical via `git stash` before/after for both feature sets,
     nothing new introduced despite this item editing the exact
     `handle_param` function those `eprintln!`s are diagnostic siblings of.
     The one pre-existing doctest failure (`src/lib.rs - derive_ranting`,
     `unresolved import 'ranting'`, CLAUDE.md's documented
     proc-macro-crate-can't-self-test limitation) is unchanged.

4. ✅ **Type the subject; remove runtime panics**
   - ✅ **Already existed from item 1's `ranting_core` extraction** (see item 1's
     own "Deviation" note): `SubjectPronoun` was already `pub` (forced by
     living in a separate crate from its call sites) and already had `FromStr`
     via `#[derive(EnumString)]`. Neither of those needed new work here — this
     item's actual scope was storing the typed enum in `Noun` and removing the
     panics, which item 1 explicitly deferred.
   - ✅ **New: `SubjectPronoun::as_str(&self) -> &'static str`**
     (`ranting_core/src/grammar.rs`), the inverse of `from_str`. Added so
     `Noun`'s `subject` field could change type from `String` to
     `SubjectPronoun` without touching `ranting_derive`'s generic
     `subject = "$"` codegen (`ranting_impl.rs`'s `get_plurality_fns`), which
     emits `self.subject.as_str()` and `ranting::is_subjective_plural(self.subject.as_str())`
     — those call sites don't care whether `self.subject` is a `String` or a
     type with an `as_str()` method, and that codegen path is also used by any
     third-party struct that still declares its own `subject: String` field
     (per the documented `#[ranting(subject = "$")]` contract), so it had to
     stay untouched.
   - ✅ **`Noun.subject: String` → `Noun.subject: SubjectPronoun`** — an
     invalid subject is now genuinely unrepresentable in a constructed `Noun`,
     not just rejected at construction time.
   - ✅ **API-shape decision: kept `Noun::new(&str, &str) -> Self` panicking,
     added `Noun::try_new(&str, &str) -> Result<Self, InvalidSubjectError>`**
     — option (a) from this item's own list of choices, not option (b)
     (`Noun::new` taking `SubjectPronoun` directly). Reasoning: `Noun::new` is
     called from the README, doctests across `src/lib.rs`/`src/collections.rs`/
     `src/language/english.rs`, and every integration test file — dozens of
     call sites, all passing string literals like `"it"`/`"he"`/`"they"`.
     Changing its signature to take `SubjectPronoun` would force every one of
     those to write `SubjectPronoun::It` instead of `"it"` (or a `.parse()`)
     for zero safety gain, since those literals are always valid; it would
     also be a breaking change for any external crate already calling
     `Noun::new`. `Noun::new` keeps its exact old signature and panic
     behavior (now implemented as `Self::try_new(..).expect("not a subject")`
     — same message, same panic path, verified by a
     `#[should_panic(expected = "not a subject")]` test) so no existing call
     site needed touching. `Noun::try_new` is the new, additive, non-panicking
     escape hatch for callers with a runtime/user-supplied subject string —
     the actual "unrepresentable invalid state" is enforced by `Noun`'s field
     now being `SubjectPronoun` rather than `String`, regardless of which
     constructor was used to get there; `try_new` just adds a way to reach
     that guarantee without a panic.
     `InvalidSubjectError(pub String)` (`src/lib.rs`) implements
     `Display`/`Error` and carries the rejected string back to the caller.
   - ✅ **`is_subjective_plural`'s `as usize >= 6` → explicit match**
     (`ranting_core/src/grammar.rs`): matches every `SubjectPronoun` variant
     by name (`I | You | Thou | He | She | It => false`,
     `We | Ye | They => true`), no wildcard arm, so a future new variant is a
     compile error here instead of silently landing on one side of a numeric
     cutoff. Cross-checked against every variant via `SubjectPronoun::iter()`
     in a new `ranting_core` unit test
     (`grammar::tests::is_subjective_plural_covers_every_variant`).
   - ✅ **Both remaining fallible paths now degrade gracefully instead of
     panicking**:
     - `is_subjective_plural`'s `.expect("subject should be a valid pronoun")`
       → invalid input now returns `false` (treated as singular) instead of
       panicking. `is_subject`/`is_subjective_plural` are both public API
       (re-exported from `ranting`), so this is a real behavior change for
       any external caller currently relying on the panic — documented here
       since it's the one behavior change in this item.
     - `inflect()`'s two `.expect(...)` calls on `strip_suffix` failure in
       `ranting_derive/src/ranting_impl.rs`'s generated code (both the
       `subject = "$"` branch and the fixed-subject branch, four `.expect`
       call sites total) → when a name doesn't end in the expected
       `singular_end`/`plural_end`, the fallback now returns the name
       unchanged instead of panicking, matching the "no irregular-table
       match, no regular-suffix match either" case a formatting call
       shouldn't abort the whole program over.
     - Left alone, out of this item's explicit scope: the other five
       `.expect("Not a subject")` calls in `src/language/english.rs`
       (`inflect_adjective`/`inflect_subjective`/`inflect_objective`/
       `inflect_possessive`/`inflect_reflexive`) — those operate on subjects
       that are already-validated `Noun`/`Ranting` data flowing through
       `say!()`'s own call sites, not raw external input, and this item named
       only `inflect()` and `is_subjective_plural`'s `.expect`s explicitly.
       Candidate for a future pass if the same graceful-degradation standard
       should extend there too.
   - ✅ New tests: `ranting_core/src/grammar.rs`'s `tests` module (3 tests —
     `as_str`/`from_str` round-trip via `EnumIter`, exhaustive plurality
     cross-check, invalid-input degradation) and 6 new tests in
     `tests/ranting/property_based.rs` (`Noun::try_new` no-panic proptest,
     `is_subjective_plural` no-panic proptest, invalid-subject degradation,
     `try_new` error contents, `Noun::new`'s panic behavior preserved,
     `inflect()`'s suffix-mismatch graceful degradation via a `#[derive_ranting]`
     struct whose declared plurality doesn't match its name's suffix).
   - ✅ Full gate clean on all three crates; test counts against the
     pre-task baseline (`git show 297056c2`: 39 lib + 254 integration + 11
     doctests in `ranting`, 11 in `ranting_core`, 9 unit in `ranting_derive`):
     `ranting` now 39 lib + 260 integration (+6) + 11 doctests; `ranting_core`
     now 14 (+3) unit tests, 0 doctests (unchanged); `ranting_derive` unchanged
     at 9 unit tests, its one pre-existing doctest failure
     (`src/lib.rs - derive_ranting`, `unresolved import 'ranting'`)
     unaffected — same CLAUDE.md-documented proc-macro-crate-can't-self-test
     limitation, not a regression. `ranting_derive`'s standalone
     `cargo clippy --all-targets -- -D warnings` still fails on exactly the
     same pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead code in `plurals.rs`, one
     `map_or` lint, two needless-borrow lints) — no new findings introduced by
     this item's changes to `ranting_impl.rs`.

5. ✅ **Public API cleanup** (free only while there's no userbase)
   - ✅ Fixed the `inflect_possesive` → `inflect_possessive` typo (public API):
     the `src/language/english.rs` definition, its `src/lib.rs` re-export and
     one internal call site, the `ranting_derive/src/lib.rs` codegen call site
     (`ranting::inflect_possessive(...)`), the `src/language/verb.rs` doc
     comment, `ROADMAP.md`'s own references, and every use in
     `tests/ranting/property_based.rs` (including the two test fn names,
     `inflect_possessive_known_pronouns`/`inflect_possessive_with_case_flag`).
     `grep -rn inflect_possesive` (old spelling) over `.rs`/`.md` now returns
     zero hits outside historical planning docs (`PHASE_2_IMPLEMENTATION_PLAN.md`,
     `docs/superpowers/specs/...`, `docs/superpowers/plans/...`,
     `.superpowers/sdd/...`) — those are dated snapshots of past design
     discussion, not living reference docs, so left as-is rather than rewriting
     history; `CLAUDE.md`'s own mention of the typo (in its "remaining Phase 4
     items" summary line) was also left alone, matching the precedent set by
     items 1-4's commits, none of which touched that summary line even though
     it already listed items 2 and 3 as "still ahead" after they'd landed.
   - ✅ `#[doc(hidden)]` on macro plumbing: added to `handle_placeholder`
     (`src/lib.rs`) — `handle_placeholder_with_context` and
     `handle_tense_marker` turned out to already carry `#[doc(hidden)]` from
     earlier work, so only `handle_placeholder` needed the attribute, matching
     `HeedMatcher`'s existing treatment (`src/heed.rs`).
   - ✅ Reworked `ack!()`/`nay!()` (`ranting_derive/src/lib.rs`) from
     `parse_quote!(return Ok(#output))`/`return Err(#output)` to plain
     `parse_quote!(Ok(#output))`/`Err(#output)` — an ordinary expression, not a
     hidden control-flow statement. **This is a real breaking change**: every
     existing call site relying on the implicit early return needed either an
     explicit `return`, or to already be in tail-expression position.
     Verified the failure mode for call sites that get this wrong isn't
     silent: a bare discarded statement like `nay!("...");` fails to compile
     with `E0282 type annotations needed` in the common case (the `Ok<T>`
     side of `Result<T, E>` is otherwise unconstrained at that statement), and
     even when the type *is* fully inferable from context, `Result`'s
     `#[must_use]` still fires (`unused_must_use`, hard error under this
     repo's `-D warnings` gate) — so a caller who drops the `return` doesn't
     get silent fallthrough, they get a compile error pointing at the exact
     line, in every configuration tried.
     Updated call sites: `tests/ranting/male_female_and_object.rs`'s
     `Person::respond_to` — its `match` was already the function's tail
     expression, so every arm's `ack!(...)`/`nay!(...)` simply dropped its
     trailing `;`; no arm needed a `return` at all because the whole `match`
     was already in tail position (an intermediate draft added explicit
     `return`s here defensively, which clippy's `needless_return` then
     correctly flagged as unnecessary — removed again, since they were never
     needed in the first place). `docs/COOKBOOK.md`'s existing
     "Error handling with ack!()/nay!()" example needed no code change at
     all — its `if`/`else` was already the function's tail expression, so it
     now doubles as a working demonstration of the new expression form (not
     directly compiled by any test — `tests/ranting/cookbook.rs` backs other
     recipes but not this one — verified by hand-compiling an equivalent
     snippet in a scratch test file, since `person.name` in the doc's literal
     form isn't externally visible: `name` is `pub(crate)` on `Noun`).
     `README.md`'s bullet describing `ack!()`/`nay!()` was rewritten to
     describe the expression-based semantics (`Ok(say!(...))`/`Err(say!(...))`,
     usable as a plain expression, `return` is now the caller's job) instead
     of the old "provides an Ok()/Err() return" hidden-control-flow phrasing.
     The two doctests on `ack!`/`nay!` in `src/lib.rs` were updated to write
     `return ack!(...)`/`return nay!(...)` explicitly, since those examples
     are single-statement function bodies where the macro isn't in tail
     position.
   - ✅ New test file `tests/ranting/ack_nay_expression.rs` (registered in
     `tests/ranting/main.rs`), 4 tests proving the new expression-based
     surface: `ack!()`/`nay!()` bound directly to a `let` (not returned at
     all), used as `match`-arm tail values inside a function that never
     writes `return`, and — for contrast — still working when a caller
     chooses to write `return ack!(...)` explicitly (early-return from an
     `if` with no `else`, falling through to a tail-position `nay!(...)` on
     the implicit "no" path).
   - ✅ Full gate clean on all three crates; test counts against the
     pre-task baseline (`git show f3799244`: 39 lib + 260 integration + 11
     doctests in `ranting`, 14 in `ranting_core`, 9 unit in `ranting_derive`):
     `ranting` now 39 lib + 264 integration (+4, the new
     `ack_nay_expression.rs` tests) + 11 doctests (unchanged); `ranting_core`
     unchanged at 14 unit tests; `ranting_derive` unchanged at 9 unit tests,
     its one pre-existing doctest failure (`src/lib.rs - derive_ranting`,
     `unresolved import 'ranting'`) unaffected — same CLAUDE.md-documented
     proc-macro-crate-can't-self-test limitation, not a regression.
     `ranting_derive`'s standalone `cargo clippy --all-targets -- -D warnings`
     still fails on exactly the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead code in `plurals.rs`, one
     `map_or` lint, two needless-borrow lints) — no new findings introduced by
     this item's changes.

6. ✅ **Hand-written placeholder tokenizer** (replaces `PH_EXT` regex internals)
   - ✅ Kept the sigil grammar (`PH_START`, `{...}` discovery) untouched — it's the
     crate's identity and was explicitly out of scope. Only `PH_EXT`'s
     internals-of-the-braces parsing changed.
   - ✅ New module `ranting_core/src/ph_ext.rs`: a hand-written recursive parser,
     `ph_ext::parse(s: &str) -> Result<PhExtMatch, PhExtError>`, called from
     `ranting_derive`'s `parse_str_params` (previously `PHE.is_match(...)` +
     `PHE.replace(...)`, `PHE` = `Regex::new(PH_EXT)`). `PhExtMatch` mirrors the
     handful of `regex::Captures`/`regex::Match` methods `handle_param` already used
     (`.name(...)`, `.as_str()`/`.start()`/`.end()`, plus `.whole()` replacing
     `.get(0)`), so `handle_param`'s ~200-line body needed only a signature change
     (`caps: &Captures` → `caps: &ranting_core::ph_ext::PhExtMatch`) — none of its
     logic changed. `PH_EXT` itself stays in `grammar.rs`, `#[allow(dead_code)]`, as
     the *reference grammar*: `ph_ext`'s own test suite differentially fuzzes the
     hand parser against `Regex::new(PH_EXT)` (a `regex` dev-dependency added to
     `ranting_core/Cargo.toml` for exactly this, not used by any non-test code), so
     the reference stays load-bearing for verification even though nothing parses
     against it at runtime anymore.
   - ⚠️ **Design surprise — `?+` is not possessive here.** The grammar's authors
     clearly intended `(?P<x>...)?+` as "optional, possessive/atomic" (match if
     possible, then never backtrack out of it). Two direct experiments against the
     `regex` crate disprove that reading: `(?:a)?+a` against `"a"` still matches
     (true possessive semantics would reject it), and — more consequentially —
     `(?P<x>[ab])?+c` against `"abc"` matches with `x = "b"`, i.e. the group matched
     *twice*, with only the *last* repetition's span kept as the named capture. So
     `X?+` empirically behaves like `X*` (zero-or-more, greedy, ordinarily
     backtracking) in this crate, not "optional, committed". This isn't a corner
     case nobody hits: `src/language/english.rs`'s pre-existing `verbs_deny`/`upper`
     tests use placeholders like `{=?w weren't}`, where `case` has to match a
     two-character run (`=` then `?`, keeping only `?`) for the existing behavior to
     make sense at all — a naive "optional presence" reading of `?+` cannot produce
     that. `uc`, `pre`, `nr`, and `case` are therefore all implemented as genuine
     repeated (star) groups via one generic engine, `star_candidates`, rather than
     four independent single-optional checks.
   - ⚠️ **Second design surprise — `pre`'s lazy "extra words" loop is reachable.**
     An early version of this module's doc argued (from a since-corrected mental
     model) that `pre`'s trailing `(?:\s+[\w-]+)*?` could never fire, since the
     zero-repetition attempt's local success/failure doesn't depend on repetition
     count. That reasoning only checked *local* success and missed that regex
     backtracking is driven by *overall* (downstream) success — proven wrong by this
     crate's own pre-existing doctest, `` {can `man pair of #0 boots remain} `` (on
     `inflect_possessive`), which needs `pre` to swallow two extra words (`pair`,
     `of`) after `` `man `` so that `nr` (`#0 `) and `noun` (`boots`) can start where
     the template actually intends. Fixed by `finish_pre_candidates`, which yields
     every extra-word count in lazy (fewest-first) priority order and lets the outer
     backtracking search (which already existed for the `?+`-is-really-`X*` finding
     above) reach for a non-zero count only when zero fails downstream.
   - ✅ **Error spans**: textual, not source-span-precise. Errors are `PhExtError {
     start, end, message }` — byte offsets into the placeholder-internals text
     (`ranting_derive` already offsets these into the full template string, exactly
     as it did with the old regex captures) plus a specific message, e.g. for input
     `` `=` ``: `` expected a noun or variable name, found `=` `` , or for
     `` `who run(` `` : `` expected article or verb, found ` run(` `` (the found
     text includes the leading space that separates the noun from its trailing
     content) — replacing the single blanket `"Error in placeholder"`. These
     still flow through `syn::Error`/`LitStr::slice(...).error(...)` exactly as
     before, so they still render as a real compile error pointing at the offending
     substring inside the user's source string literal (not just a stderr message) —
     this *is* source-span-precise in the sense that matters to a `say!()` caller,
     just not decomposed further (e.g. "expected one of: `a`, `an`, `the`, ...").
     Decomposing into per-alternative-expected-token messages was judged not worth
     it: the tokenizer's own internal structure (six `pre`-atom alternatives, a
     handful of modal words, nested articles) doesn't map cleanly onto a short
     "expected X" list without either truncating misleadingly or dumping the whole
     grammar into every error.
   - ✅ **No placeholder-acceptance regression.** `ph_ext::tests::parity_curated_corpus`
     (curated cases, including the two real multi-char-case-run and extra-words
     examples above) and `ph_ext::tests::parity_fuzzed` (a `proptest` property over
     uc/pre/nr/case/noun/post combinations) both assert the hand parser and
     `Regex::new(PH_EXT)` agree exactly — either both reject an input, or both accept
     it with identical capture text for every named group. Two apparent "regex
     inconsistencies" found while building this turned out, on closer inspection, to
     be gaps in the hand parser's own understanding of the grammar (the two design
     surprises above), not actual regex bugs — once fixed, no fuzz exclusions were
     needed at all.
   - ✅ New tests: `ph_ext::tests::error_message_specific_for_bad_post`,
     `error_message_specific_for_missing_noun` (assert on the new specific message
     text), `pre_extra_words_then_second_repetition` (regression test for the second
     design surprise above), plus the differential `parity_curated_corpus`/
     `parity_fuzzed` pair — 5 new tests in `ranting_core`, all in `src/ph_ext.rs`.
   - ✅ Full gate clean on all three crates; test counts against the pre-task
     baseline (`git show f1b08545`: 39 lib + 264 integration + 11 doctests in
     `ranting`, 14 unit in `ranting_core`, 9 unit in `ranting_derive`): `ranting`
     unchanged at 39 lib + 264 integration + 11 doctests (the two previously-passing
     doctests that exercise the fixed extra-words/case-run behavior —
     `language::english::inflect_possessive` and `lib.rs - ack` — kept passing
     throughout, since those bugs were caught and fixed before landing, not shipped
     as regressions); `ranting_core` now 19 unit tests (+5, `ph_ext`'s new tests
     above); `ranting_derive` unchanged at 9 unit tests, its one pre-existing
     doctest failure (`src/lib.rs - derive_ranting`, `unresolved import 'ranting'`)
     unaffected. `ranting_derive`'s standalone `cargo clippy --all-targets -- -D
     warnings` still fails on exactly the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` — no new findings introduced by this
     item's changes.
   - This completes Phase 4's architecture-consolidation restructuring (items 1-6).
     Item 7 (licensing) remains an explicit human decision, not an agent task — see
     [PROPOSED LICENSE CHANGE](#proposed-license-change-awaiting-decision). Item 8
     (repo hygiene) was already complete.
   - **Follow-up, 2026-08-13**: the 7 pre-existing `ranting_derive` clippy
     findings referenced throughout this item's log (dead `plurals.rs` code +
     `map_or`/needless-borrow lints) are now fixed: `ranting_derive`'s dead
     `src/language/plurals.rs` module was deleted outright (see the Phase 4
     item 1 update above) rather than patched with `#[allow(dead_code)]`, and
     the two `needless_borrows_for_generic_args` findings in `src/lib.rs`
     (`x.ends_with(&['x', ...])` → `x.ends_with(['x', ...])`) were fixed
     directly. `cargo clippy --all-targets -- -D warnings` is now clean in
     all three crates individually and as a workspace.
   - **Follow-up, 2026-08-13**: the other `ranting_derive`-gate wart
     referenced throughout this item's log — the pre-existing
     `src/lib.rs - derive_ranting` doctest failure (`unresolved import
     'ranting'`) — is fixed too, the same way CLAUDE.md's "Doctests in
     proc-macro crate" note always said to fix it: the example (singular
     "they" via `#[derive_ranting]`) is now a real, passing doctest on
     `Ranting`'s trait doc in `ranting/src/lib.rs` (where `ranting` and
     `ranting_derive` are both actually in scope), and the original copy on
     `ranting_derive::derive_ranting` itself is marked `` ```rust,ignore ``
     with a comment pointing at the runnable copy, instead of silently
     failing every `cargo test --doc` run in that crate. `ranting_derive`'s
     doctest count goes from "9 unit + 1 failing doctest" to "9 unit + 1
     ignored doctest"; `ranting`'s doctest count goes from 11 to 12.

7. **Licensing decision** (orthogonal, but decides whether the rest gets an audience)
   - GPL-3 on a *library* crate is the single biggest adoption barrier: dependents'
     code must be GPL-compatible, so most of the Rust ecosystem won't touch it.
     Decide deliberately; MIT/Apache-2.0 dual is the ecosystem norm.
   - Either way, prefer `license = "..."` over `license-file` in Cargo.toml so
     tooling (lib.rs, cargo-deny, license scanners) can classify it.
   - ✅ **Decided 2026-08-13**: relicensed to plain `MIT` (not the dual
     `MIT OR Apache-2.0` this section recommended below — the copyright holder's
     explicit choice). `license = "MIT"` in `Cargo.toml`, `ranting_core/Cargo.toml`,
     `ranting_derive/Cargo.toml`; `LICENSE.txt` replaced with the MIT text; all
     `// (c) Roel Kluin <year> GPL v3` source headers updated to `MIT`. Sole
     copyright holder (verified via `git shortlog -sne --all`), so no consent round
     was needed. Note: the already-published 0.2.1 on crates.io remains available
     under GPL-3 — crates.io releases are immutable, so MIT applies from the next
     published version onward.

✅ **8. Repo hygiene** (COMPLETE)
   - ✅ Untracked (`git rm --cached`, files kept on disk) and added to `.gitignore`:
     `ranting.log`, `src.txt`, `git_diff.txt`, `git_status.txt`, `code_analysis.txt`,
     `review.jsonl`, `dodev`, `ideas`, `ranting_derive/ranting_derive.log`,
     `ranting_derive/src.txt` — these previously shipped in the published package.
   - ✅ Also ignored `.cargo-home/` (the container-local `CARGO_HOME` from CLAUDE.md),
     which was untracked but still being swept into `cargo package`.
   - Still shipping, deliberately unreviewed here: `git_log_oneline.txt`, `tasks.txt`,
     `mksrc.sh`, `ranting_derive/mksrc.sh`, `scripts/overnight_loop.sh`,
     `.superpowers/sdd/**`, and the top-level design/report `.md` files. Decide per
     file whether they belong in the crate or behind a Cargo `exclude` list.

---

## PROPOSED LICENSE CHANGE (awaiting decision)

> **Status: DECIDED 2026-08-13 — relicensed to plain MIT.** The analysis below
> recommended dual `MIT OR Apache-2.0`; the copyright holder chose plain `MIT`
> instead. All crates now carry `license = "MIT"`, `LICENSE.txt` holds the MIT
> text, and source headers read `// (c) Roel Kluin <year> MIT`. Kept here for the
> historical tradeoff analysis.

### Current state

| Where | Value |
|-------|-------|
| `Cargo.toml` (ranting) | `license-file = "LICENSE.txt"` |
| `ranting_derive/Cargo.toml` | `license-file = "../LICENSE.txt"` |
| `LICENSE.txt` | verbatim GNU GPL v3 text (no "or later" wording added) |
| `src/lib.rs`, `ranting_derive/src/lib.rs` | `// (c) Roel Kluin 2022 GPL v3` |
| Published | `ranting` / `ranting_derive` 0.2.1 on crates.io, under these terms |

Two facts that shape the decision, both verified in this repo:

- **Sole copyright holder.** `git shortlog -sne --all` shows every commit authored
  by Roel Kluin (four spellings of the same address). There are no third-party
  contributions to re-license, so no CLA round or consent-gathering is needed —
  the decision is unilateral today. That stops being true the first time an
  outside PR is merged.
- **No inbound copyleft.** The grammar data (`data/irregular_verbs.txt`,
  `data/irregular_plurals.txt`) is hand-maintained in-repo, not imported from a
  copyleft or share-alike corpus, so nothing forces GPL from the dependency side.
  *To confirm before acting:* run `cargo deny check licenses` (or `cargo license`)
  over both crates — the direct dependency set is the usual permissive
  MIT/Apache-2.0 proc-macro stack, but it has not been machine-verified here.

### The tradeoff

GPL-3 on a library is not "GPL for this repo" — it is a constraint on every
dependent. Because `ranting` is consumed *only* as a dependency, the license
choice is effectively a choice about who is allowed to use the crate at all.

The proc-macro angle sharpens this. `say!()` does not just get linked; it expands
into the dependent crate's own source at compile time. Whether macro-expanded
output is a derived work of the macro is exactly the kind of question a corporate
legal review will not want to answer — and the safe answer they will give is "use
something else". A GPL proc-macro is a harder sell than a GPL rlib, not an easier
one.

That collides with two commitments already in this roadmap:

- **Vision** targets "game engines, interactive fiction, chatbots" — overwhelmingly
  proprietary or mixed-license codebases.
- **v1.1 success criteria** call for "2-3 ecosystem forks", and Phase 3 item 2
  shipped trait extensibility explicitly to enable `ranting-spanish`,
  `ranting-pirate`, etc. GPL-3 discourages precisely the downstream ecosystem the
  extensibility work was built to attract.

| | GPL-3.0-only (status quo) | MIT OR Apache-2.0 (dual) |
|---|---|---|
| Adoption | Blocks proprietary and most permissive-licensed dependents | Ecosystem default; no review friction |
| Ecosystem forks (v1.1 goal) | Forks must stay GPL; most won't start | Forks unconstrained |
| Improvements flow back | Copyleft obligation on derivatives | Voluntary only |
| Patent grant | Yes (GPL-3 §11) | Yes, via the Apache-2.0 arm |
| Compatibility | Incompatible with Apache-2.0-only dependents | Compatible with essentially everything |
| Reversibility | Can relax to permissive later (sole holder) | Cannot tighten later for released versions |

Two middle options, both weaker than they look:

- **LGPL-3**: designed around dynamic relinking, which Rust's static linking,
  generics, and macro expansion make ill-defined. It buys legal ambiguity rather
  than a real middle ground.
- **GPL-3 + linking exception**: workable, but it is a bespoke license. Scanners
  and `cargo-deny` policies classify it as GPL, so most of the adoption cost
  remains while the copyleft benefit mostly disappears.

### Recommendation

**Adopt `license = "MIT OR Apache-2.0"` for both crates**, matching the Rust
ecosystem norm.

The copyleft protection GPL-3 provides is worth little here — this is a text
formatting library, not a product with a business model to defend — while its cost
is the entire audience the roadmap is written for. Dual MIT/Apache-2.0 also keeps
the Apache-2.0 patent grant, which MIT alone would not.

Timing matters: this belongs in v1.2 alongside the other pre-adoption breaks
(see *Premature API Lock-in* below). Relicensing is free while the author is the
only copyright holder and there is no userbase; each new contributor and dependent
raises the cost permanently.

**If the decision is instead to keep GPL-3**, the Cargo.toml change should still
happen: replace `license-file` with `license = "GPL-3.0-only"` so lib.rs,
cargo-deny, and license scanners can classify the crate. (`GPL-3.0-only` matches
the current wording — `LICENSE.txt` is the plain GPLv3 text and the source headers
say "GPL v3" with no "or later". If "or later" was the intent, record
`GPL-3.0-or-later` instead and say so in the headers.) Choosing this consciously
is a valid outcome; the roadmap only asks that it stop being an accident.

### If approved — implementation checklist

1. Verify dependency licenses: `cargo deny check licenses` over both crates.
2. Replace `license-file` with `license = "MIT OR Apache-2.0"` in `Cargo.toml` and
   `ranting_derive/Cargo.toml`.
3. Add `LICENSE-MIT` and `LICENSE-APACHE`; delete `LICENSE.txt` (or keep it only if
   some component genuinely stays GPL — currently none does).
4. Update the `// (c) Roel Kluin 2022 GPL v3` headers in `src/lib.rs` and
   `ranting_derive/src/lib.rs`; add a License section to `README.md`.
5. Note in `CHANGELOG`/release notes that the already-published 0.2.1 remains
   available under GPL-3 — crates.io releases are immutable, so the new terms
   apply from the next published version onward.
6. Check the box in Phase 4 item 7 and flip the *Key Architecture Decisions* row
   from 🔄 to ✅ with the chosen SPDX expression.

---

### v1.2 Success Criteria
- One shared `ranting_core` crate; zero build.rs copy/symlink mechanisms remain
- No unmaintained dependencies (RUSTSEC-clean); single strum/regex/syn versions
- No stringly-typed macro↔runtime interface; no `~TENSE~` sentinel
- No runtime panics reachable from public API with invalid data
- Placeholder syntax errors report precise spans
- License decision made and recorded

---

## Phase 5 — v1.2.1 — `ask!()` Stabilization ✅

*Goal: give `ask!()` a real, typed contract and test coverage, closing the gap
flagged in the 2026-08-13 docs audit — zero usage anywhere in the repo, no
trait, and an untyped `.answer()` call the macro simply trusted exists.*

**Status**: Complete. Breaking change to `ask!()`'s public signature and
return type, landed deliberately: 0.2.1 has no adopters yet, and Phase 4's
own rationale (pay down structural debt while there's no userbase to break)
applies here too.

1. ✅ **`Answerable` trait + capture-forwarding `ask!()`**
   - New `Answerable` trait in `ranting` (`src/answerable.rs`): `fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String`, with `type Captures` declared per implementor — bare `String` for a single capture, `()` for none, a tuple of `String`s for 2+, mirroring `heed!()`'s existing 0/1-vs-2+ return convention exactly (no `$name` → `u64` auto-typing through the trait boundary; a caller that needs a typed value parses the `String` itself inside `answer()`).
   - `ask!()` reworked from `ask!(speaker, audience, "fmt", args...)` (rendered a question via `say!()`-style formatting, then called `audience.answer(speaker, String)` as untyped duck-typing) to `ask!(speaker, audience, "template", input)` — reuses `heed!()`'s template-parsing engine (`ranting_derive/src/heed.rs::compile_heed_template`) to match `input` against `template`'s `{name}`/`{name...}`/`{$name}` captures, then forwards them to `audience.answer(&speaker, captures)`. Returns `Option<String>` (`None` on no match, `answer()` not called), joining `heed!()` in the crate's Option-returning macros instead of returning whatever `.answer()` happened to return.
   - `ask!` is now re-exported from `ranting` (`pub use ranting_derive::ask;`) — previously the only one of the five core macros not re-exported, an oversight from when it was untested.
   - Known, accepted limitation: `Captures` being an associated type means one type supports exactly one template capture-arity everywhere it's used as an `ask!()` audience — a type needing to answer differently-shaped questions would need `Captures = Vec<String>` (losing arity checking at the call site) or separate wrapper types. Not solved here; no current caller needs it.
2. ✅ **Tests + docs**
   - `tests/ranting/ask.rs`: 5 integration tests — 0/1/2-capture arities, a capture-driven response (`Villager` matching on `topic`), and a no-match `None` case.
   - `docs/API.md`/`docs/CHEATSHEET.md`/`CLAUDE.md` updated to the new signature; the "sparsely used/documented, less-stable corner" caveat lifted now that tests exist.

**Out of scope, proposed for a separate higher-level crate**: Inform7-style
object disambiguation — resolving *which* candidate `Ranting`/`Answerable`
object free-text input refers to, weighted by "likely"/"unlikely" rules when
multiple objects could match (e.g. "talk to it" being far more likely to mean
a nearby person than a stone). This is a world-model/candidate-registry
problem with no existing shape in this crate — `ask!()` only ever targets one
statically-known `audience` expression — and doesn't belong in `ranting`
itself. See [v1.3.0](#v130-ecosystem-expansion) below for where a
`ranting`-adjacent crate proposal like this belongs.

---

## Post-v1.2: Future Directions

### v1.3.0: Ecosystem Expansion
- **`ranting-i18n` Companion Crate** (12-16 weeks post-v1.0):
  - Multi-language support: German, French, Spanish, Japanese, etc.
  - Modular language modules using trait-based extensibility from v1.1
  - Proves extensibility model works; enables global adoption
- **`ranting-if` (or similar) Companion Crate — Inform7-style object disambiguation**
  (proposed 2026-08-13, not scoped): resolves which candidate object among
  several free-text input refers to, using "likely"/"unlikely"-weighted rules
  the way Inform 7's `Understand` rulebook does (e.g. a "talk to" action being
  far more likely to target a person in scope than a stone). Builds on
  `ranting`'s `Answerable` trait (Phase 5) and `heed!()`'s capture parsing,
  but needs a candidate registry, a scoring/priority mechanism, and rule
  authoring syntax that have no home in `ranting` itself — `ask!()` only ever
  targets one statically-known `audience` expression per call site, by design.
  A natural fit for a `ranting`-adjacent crate rather than a `ranting` feature.

### v1.4+: Advanced Features (Community-Driven)
- Dialogue formatting with automatic punctuation and breaks
- Pluralization of entire phrases (not just nouns)
- Subjunctive mood and hypotheticals
- Register and dialect specialization (formal vs. informal, archaic, etc.) via context system from v1.1
- Performance optimizations (cached inflection, const generics)

---

## Key Architecture Decisions ✅

| Decision | Status | Notes |
|----------|--------|-------|
| Two-crate split (ranting + ranting_derive) | ✅ Complete (v1.2) | `ranting_core` shared rlib extracted (Phase 4 item 1, serde/serde_derive pattern); all build.rs copy/symlink sharing deleted |
| Verb table codegen via build.rs | ✅ Complete | Single source of truth: data/irregular_verbs.txt; codegen moves into `ranting_core` in v1.2 |
| Pronoun/article/verb tables → exhaustive match | ✅ Complete | Exhaustive `match` dispatch with `#[deny(...)]` guards; no wildcards; permanent regression tests for string values |
| Derive macro attributes (4 core + 3 cosmetic) | ✅ Complete | subject, name, singular_end, plural_end (core) |
| Compile-time parsing + runtime inflection | ✅ Locked | Catches syntax errors early; enables extensibility. Seam becomes typed (`PlaceholderSpec`) in v1.2, replacing `caps: [&str; 5]` + `~TENSE~` sentinel |
| Documentation (Tutorial + Cookbook) | ✅ Complete | 30-40 min tutorial, 10 practical recipes |
| Placeholder syntax (full grammar support) | ✅ Locked | Sigil grammar is the crate's identity; keep it. v1.2 swaps the `PH_EXT` regex recognizer for a tokenizer (better error spans) without changing the grammar |
| Built-in English rules (extensibility in v1.1) | ✅ v1.0; 🎯 v1.1 | Free functions now; trait methods in v1.1 |
| Irregular noun plurals codegen | ✅ Complete (v1.1); ✅ wired to `Ranting::inflect()` (2026-08-13) | Single source of truth: data/irregular_plurals.txt; `english::inflect_noun_irregular` now delegates to `get_plural`/`get_singular`, so `Ranting::inflect()`'s irregular-noun path uses them (with `apply_case` case-preservation). `ranting_derive`'s own copy is still unwired — no compile-time call site exists — see docs/architecture-review-2026-08-13.md |
| Context-aware runtime tense | ✅ Complete | `say_with!(context, ...)` + `NarrationContext`/`Tense`; unblocks Recounting M9 (tense portion) |
| Context-aware runtime viewpoint | ✅ Complete | `NarrationContext.narration_person` + `Person`; scoped to first-person-declared (`I`/`we`) nouns only; unblocks Recounting M9 (viewpoint portion) |
| Narration context threading (register/dialect) | ✅ Complete | `NarrationContext.register`/`.dialect` are inert in-crate; reachable via 3 new `Ranting::*_with_context` hooks (`ctx` as parameter, never entity-owned), defaulting to the pre-existing hooks |
| Consolidate english_shared.rs | ✅ Complete → superseded (v1.2) | Single canonical copy + build.rs copy solved the drift; `ranting_core` extraction (Phase 4, item 1) replaces the copy mechanism outright |
| Stringly-typed `subject: &str` in public API | ✅ Complete (v1.2) | Phase 4 item 4: `SubjectPronoun` public, typed field in `Noun`, non-panicking `Noun::try_new`; invalid subjects unrepresentable instead of panicking |
| `ack!()`/`nay!()` expand to hidden `return` | ✅ Complete (v1.2) | Phase 4 item 5: reworked to plain `Ok(say!(...))`/`Err(say!(...))` expression forms, usable anywhere an expression is valid |
| GPL-3 via `license-file` | ✅ Complete (v1.2) | Relicensed to plain `license = "MIT"` 2026-08-13 (copyright holder's choice, differs from the dual-license recommendation in [PROPOSED LICENSE CHANGE](#proposed-license-change-awaiting-decision)); already-published 0.2.1 on crates.io remains GPL-3 |

---

## Risk Mitigation

**Macro Complexity**: Regular refactoring; keep proc-macro logic focused; document architecture.

**Code Consolidation**: ✅ Resolved. `english_shared.rs` is now a single canonical file (`src/language/english_shared.rs`); `ranting_derive`'s copy is generated at build time via `build.rs` (see CLAUDE.md), eliminating the manual-sync drift that previously affected the `ASK` regex and `SubjectPronoun` derives. Safe to build runtime tense/viewpoint (item 3) on top of this now.

**Table Maintenance**: Document adding new irregulars; encourage community PRs; keep v1.1 plural tables separate from v1.0 verb tables to avoid corruption.

**Performance Regressions**: Benchmark at phase end; profile compile-time and runtime; set performance budgets (no more than 10% slowdown per feature).

**Ecosystem Fragmentation**: Clear governance for companion crates; version-lock to core; single source of truth for grammar rules.

**Premature API Lock-in**: v1.2 (Phase 4) contains renames (`inflect_possessive`), crate restructuring (`ranting_core`), and possibly a license change. Land these *before* actively recruiting ecosystem forks or promoting adoption — every early adopter converts these from free changes into breaking changes.

**Unmaintained Dependencies**: `proc-macro-error` has an open RUSTSEC advisory and pins syn 1; resolved by Phase 4 item 2. Until then, expect `cargo audit`/`cargo deny` warnings downstream.

---

## How to Contribute

Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular plurals table, language modules, performance optimization

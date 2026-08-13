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
   - ⚠️ Lookup functions (`get_plural`/`get_singular`) exist and are unit-tested in
     `src/language/plurals.rs`, but are **not yet called from any inflection call
     site** (`Noun::inflect()` doesn't invoke them) — see
     `docs/architecture-review-2026-08-13.md`. Wiring this up is unclaimed work.
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

6. **Comparative & Superlative Adjectives** (10-16 hours)
   - Handle degree: good → better → best, bad → worse → worst
   - Marker-based syntax (e.g., `{+person good}` for comparative)
   - Rounds out morphology for richer text generation

7. **Recursive Type Inflection** (12-16 hours)
   - Support collections and nested Ranting types
   - `Vec<Item>` where Item: Ranting, `Option<Person>`, `Box<Noun>`, etc.
   - Use `#[derive(Ranting)]` to generate recursive implementations

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

1. **Extract `ranting_core` shared crate** (highest leverage)
   - Replace all three code-sharing mechanisms (`english_shared.rs` build.rs copy,
     `verb_conjugate.rs` inverse copy, `irregular_verbs.txt` symlink) with one plain
     rlib crate that both `ranting` and `ranting_derive` depend on — proc-macro crates
     can depend on ordinary crates (the `serde`/`serde_derive` pattern).
   - Moves in: grammar tables + codegen from `data/*.txt`, verb conjugation engine,
     pronoun logic, placeholder grammar (`PH_START`/`PH_EXT`).
   - Deletes: both build.rs copy steps, `ranting_derive/data/` symlink fallbacks,
     and the dual-strum (0.24 + 0.27) compilation constraint entirely.

2. **Dependency modernization** (`ranting_derive`)
   - syn 1 → 2, darling 0.14 → 0.20+, unify strum on 0.27 (or drop it from the
     derive path — `SubjectPronoun` parsing is simple enough to hand-write).
   - **Drop `proc-macro-error`** (unmaintained, open RUSTSEC advisory; pins syn 1) —
     use plain `syn::Error` for diagnostics.
   - Drop `lazy_static` in both crates → `std::sync::LazyLock`/`OnceLock`
     (edition 2024; `heed.rs` already uses `OnceLock`).
   - Bump derive-side regex 1.6 → 1.11; unify editions (2021 → 2024) after syn 2.
   - Payoff: fewer proc-macro deps = faster compile for every downstream user.

3. **Typed placeholder spec across the compile-time/runtime seam**
   - Replace `handle_placeholder(..., caps: [&str; 5])` and the `~TENSE~MARKER:WORD`
     string sentinel with a typed struct/enum (`PlaceholderSpec { article, pre_verb,
     case, tense, ... }`) baked by the macro as a const.
   - Deletes the runtime re-parsing (`strip_prefix("~TENSE~")`, `split_once(':')`,
     the "fallback if colon/marker parsing fails" branches for states the macro can
     never produce) and the runtime re-recognition of articles by string-matching
     `"the" | "a" | "some"` in `get_article_or_so` — work the macro already did.

4. **Type the subject; remove runtime panics**
   - Make `SubjectPronoun` public (with `FromStr`) and store it in `Noun`, so invalid
     subjects are unrepresentable instead of `Noun::new` panicking via `assert!`.
   - Replace `is_subjective_plural`'s discriminant comparison (`as usize >= 6` — a
     magic number tied to variant order) with an explicit `match`.
   - Make remaining fallible paths (`inflect()` on non-singularizable plurals,
     `expect` in `is_subjective_plural`) degrade gracefully — a formatting library
     should never panic at runtime on data.

5. **Public API cleanup** (free only while there's no userbase)
   - Fix the `inflect_possesive` → `inflect_possessive` typo (public API).
   - `#[doc(hidden)]` on macro plumbing: `handle_placeholder`,
     `handle_placeholder_with_context`, `handle_tense_marker` (as `HeedMatcher`
     already is) — these are not user API and shouldn't be stability commitments.
   - Rework `ack!()`/`nay!()`: a macro that expands to a hidden `return` can't be
     used as an expression and surprises readers. Prefer expression forms
     (`Ok(say!(...))` / `Err(say!(...))`) and let callers write `return`.

6. **Hand-written placeholder tokenizer** (replaces `PH_EXT` regex internals)
   - Keep the sigil grammar — it's the crate's identity — but recognize placeholder
     internals with a small tokenizer instead of one 12-line regex.
   - Payoff: precise error spans ("expected article or verb, found `…`") instead of
     the blanket "Error in placeholder"; removes the regex-dialect coupling between
     the crates; makes future grammar growth (reflexives, comparatives) tractable.
   - Note: `=` is currently overloaded (subject case before the noun, continuous
     tense after it) — document or disambiguate while the tokenizer lands.

7. **Licensing decision** (orthogonal, but decides whether the rest gets an audience)
   - GPL-3 on a *library* crate is the single biggest adoption barrier: dependents'
     code must be GPL-compatible, so most of the Rust ecosystem won't touch it.
     Decide deliberately; MIT/Apache-2.0 dual is the ecosystem norm.
   - Either way, prefer `license = "..."` over `license-file` in Cargo.toml so
     tooling (lib.rs, cargo-deny, license scanners) can classify it.
   - **- [ ] Decision pending** — analysis and a recommendation are written up below
     in [PROPOSED LICENSE CHANGE](#proposed-license-change-awaiting-decision).
     Nothing in the tree has been relicensed; both crates still carry GPL-3 via
     `license-file`. This box is for the copyright holder to check, not an agent.

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

> **Status: PROPOSAL ONLY — nothing has been relicensed.** Both crates still ship
> GPL-3 (`license-file = "LICENSE.txt"`), and the source headers still read
> `// (c) Roel Kluin 2022 GPL v3`. This section exists so Phase 4 item 7 can be
> decided on evidence; the checkbox there stays unchecked until the copyright
> holder decides. Written 2026-08-13.

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

## Post-v1.2: Future Directions

### v1.3.0: Ecosystem Expansion
- **`ranting-i18n` Companion Crate** (12-16 weeks post-v1.0):
  - Multi-language support: German, French, Spanish, Japanese, etc.
  - Modular language modules using trait-based extensibility from v1.1
  - Proves extensibility model works; enables global adoption

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
| Two-crate split (ranting + ranting_derive) | 🔄 Revisit (v1.2) | Design review 2026-08-12: extract shared `ranting_core` rlib both depend on (serde/serde_derive pattern); deletes all build.rs copy/symlink sharing |
| Verb table codegen via build.rs | ✅ Complete | Single source of truth: data/irregular_verbs.txt; codegen moves into `ranting_core` in v1.2 |
| Pronoun/article/verb tables → exhaustive match | ✅ Complete | Exhaustive `match` dispatch with `#[deny(...)]` guards; no wildcards; permanent regression tests for string values |
| Derive macro attributes (4 core + 3 cosmetic) | ✅ Complete | subject, name, singular_end, plural_end (core) |
| Compile-time parsing + runtime inflection | ✅ Locked | Catches syntax errors early; enables extensibility. Seam becomes typed (`PlaceholderSpec`) in v1.2, replacing `caps: [&str; 5]` + `~TENSE~` sentinel |
| Documentation (Tutorial + Cookbook) | ✅ Complete | 30-40 min tutorial, 10 practical recipes |
| Placeholder syntax (full grammar support) | ✅ Locked | Sigil grammar is the crate's identity; keep it. v1.2 swaps the `PH_EXT` regex recognizer for a tokenizer (better error spans) without changing the grammar |
| Built-in English rules (extensibility in v1.1) | ✅ v1.0; 🎯 v1.1 | Free functions now; trait methods in v1.1 |
| Irregular noun plurals codegen | ✅ Complete (v1.1); ⚠️ lookup not wired to call sites | Single source of truth: data/irregular_plurals.txt; `get_plural`/`get_singular` exist and are tested but currently dead code — see docs/architecture-review-2026-08-13.md |
| Context-aware runtime tense | ✅ Complete | `say_with!(context, ...)` + `NarrationContext`/`Tense`; unblocks Recounting M9 (tense portion) |
| Context-aware runtime viewpoint | ✅ Complete | `NarrationContext.narration_person` + `Person`; scoped to first-person-declared (`I`/`we`) nouns only; unblocks Recounting M9 (viewpoint portion) |
| Narration context threading (register/dialect) | ✅ Complete | `NarrationContext.register`/`.dialect` are inert in-crate; reachable via 3 new `Ranting::*_with_context` hooks (`ctx` as parameter, never entity-owned), defaulting to the pre-existing hooks |
| Consolidate english_shared.rs | ✅ Complete → superseded (v1.2) | Single canonical copy + build.rs copy solved the drift; `ranting_core` extraction (Phase 4, item 1) replaces the copy mechanism outright |
| Stringly-typed `subject: &str` in public API | 🔄 Revisit (v1.2) | Design review 2026-08-12: make `SubjectPronoun` public, store enum in `Noun`; invalid subjects become unrepresentable instead of panicking |
| `ack!()`/`nay!()` expand to hidden `return` | 🔄 Revisit (v1.2) | Not usable as expressions; surprising control flow. Prefer `Ok(say!(...))`/`Err(say!(...))` expression forms |
| GPL-3 via `license-file` | 🔄 Decide (v1.2) | Major adoption barrier for a library crate; ecosystem norm is MIT/Apache-2.0 dual. Use `license = "..."` key either way. Written up in [PROPOSED LICENSE CHANGE](#proposed-license-change-awaiting-decision) — recommendation: `MIT OR Apache-2.0`; awaiting the copyright holder's decision, nothing relicensed yet |

---

## Risk Mitigation

**Macro Complexity**: Regular refactoring; keep proc-macro logic focused; document architecture.

**Code Consolidation**: ✅ Resolved. `english_shared.rs` is now a single canonical file (`src/language/english_shared.rs`); `ranting_derive`'s copy is generated at build time via `build.rs` (see CLAUDE.md), eliminating the manual-sync drift that previously affected the `ASK` regex and `SubjectPronoun` derives. Safe to build runtime tense/viewpoint (item 3) on top of this now.

**Table Maintenance**: Document adding new irregulars; encourage community PRs; keep v1.1 plural tables separate from v1.0 verb tables to avoid corruption.

**Performance Regressions**: Benchmark at phase end; profile compile-time and runtime; set performance budgets (no more than 10% slowdown per feature).

**Ecosystem Fragmentation**: Clear governance for companion crates; version-lock to core; single source of truth for grammar rules.

**Premature API Lock-in**: v1.2 (Phase 4) contains renames (`inflect_possesive`), crate restructuring (`ranting_core`), and possibly a license change. Land these *before* actively recruiting ecosystem forks or promoting adoption — every early adopter converts these from free changes into breaking changes.

**Unmaintained Dependencies**: `proc-macro-error` has an open RUSTSEC advisory and pins syn 1; resolved by Phase 4 item 2. Until then, expect `cargo audit`/`cargo deny` warnings downstream.

---

## How to Contribute

Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular plurals table, language modules, performance optimization

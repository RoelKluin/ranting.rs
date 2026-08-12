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
   - ✅ Automatic lookup in Noun::inflect() method with fallback to regular rules
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

4. **Narration Context Threading** (12-16 hours) — *Supports feature 3*
   - Pass story-wide settings (tense, viewpoint, register, dialect) through `say!()` macro and `Ranting` trait
   - Design integration point: how context flows from story code through placeholder resolution
   - Separate `subject` (entity property) from `narration_person` (story setting)
   - Enable ecosystem forks to customize context behavior

5. **Reflexive Forms** (8-12 hours)
   - Support myself, yourself, himself, herself, itself, ourselves, themselves
   - Case marker integration (e.g., `{~person do}` becomes reflexive pronoun)
   - Completes core pronouns system

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

## Post-v1.1: Future Directions

### v1.2.0: Ecosystem Expansion
- **`ranting-i18n` Companion Crate** (12-16 weeks post-v1.0):
  - Multi-language support: German, French, Spanish, Japanese, etc.
  - Modular language modules using trait-based extensibility from v1.1
  - Proves extensibility model works; enables global adoption

### v1.3+: Advanced Features (Community-Driven)
- Dialogue formatting with automatic punctuation and breaks
- Pluralization of entire phrases (not just nouns)
- Subjunctive mood and hypotheticals
- Register and dialect specialization (formal vs. informal, archaic, etc.) via context system from v1.1
- Performance optimizations (cached inflection, const generics)

---

## Key Architecture Decisions ✅

| Decision | Status | Notes |
|----------|--------|-------|
| Two-crate split (ranting + ranting_derive) | ✅ Locked | Industry standard; no changes needed |
| Verb table codegen via build.rs | ✅ Complete | Single source of truth: data/irregular_verbs.txt |
| Pronoun/article/verb tables → exhaustive match | ✅ Complete | Exhaustive `match` dispatch with `#[deny(...)]` guards; no wildcards; permanent regression tests for string values |
| Derive macro attributes (4 core + 3 cosmetic) | ✅ Complete | subject, name, singular_end, plural_end (core) |
| Compile-time parsing + runtime inflection | ✅ Locked | Catches syntax errors early; enables extensibility |
| Documentation (Tutorial + Cookbook) | ✅ Complete | 30-40 min tutorial, 10 practical recipes |
| Placeholder syntax (full grammar support) | ✅ Locked | Powerful; UX solved via documentation |
| Built-in English rules (extensibility in v1.1) | ✅ v1.0; 🎯 v1.1 | Free functions now; trait methods in v1.1 |
| Irregular noun plurals codegen | ✅ Complete (v1.1) | Single source of truth: data/irregular_plurals.txt; runtime lookup |
| Context-aware runtime tense | ✅ Complete | `say_with!(context, ...)` + `NarrationContext`/`Tense`; unblocks Recounting M9 (tense portion) |
| Context-aware runtime viewpoint | ✅ Complete | `NarrationContext.narration_person` + `Person`; scoped to first-person-declared (`I`/`we`) nouns only; unblocks Recounting M9 (viewpoint portion) |
| Consolidate english_shared.rs | ✅ Complete | Single canonical copy at src/language/english_shared.rs; ranting_derive/build.rs copies it into OUT_DIR at build time (symlink-dereference fallback for packaged builds) |

---

## Risk Mitigation

**Macro Complexity**: Regular refactoring; keep proc-macro logic focused; document architecture.

**Code Consolidation**: ✅ Resolved. `english_shared.rs` is now a single canonical file (`src/language/english_shared.rs`); `ranting_derive`'s copy is generated at build time via `build.rs` (see CLAUDE.md), eliminating the manual-sync drift that previously affected the `ASK` regex and `SubjectPronoun` derives. Safe to build runtime tense/viewpoint (item 3) on top of this now.

**Table Maintenance**: Document adding new irregulars; encourage community PRs; keep v1.1 plural tables separate from v1.0 verb tables to avoid corruption.

**Performance Regressions**: Benchmark at phase end; profile compile-time and runtime; set performance budgets (no more than 10% slowdown per feature).

**Ecosystem Fragmentation**: Clear governance for companion crates; version-lock to core; single source of truth for grammar rules.

---

## How to Contribute

Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular plurals table, language modules, performance optimization

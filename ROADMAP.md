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

3. **Runtime Tense & Viewpoint Selection** (16-20 hours) — *Blocks Recounting M9*
   - Runtime tense selection: choose past/present/future at runtime from `StoryState`, not compile-time only
   - Runtime viewpoint selection: narrate in first/second/third person regardless of entity's `subject` field
   - Design context-passing mechanism: explicit macro parameter (`say_with!(context, ...)`) vs. trait method vs. thread-local
   - Enables interactive fiction and game narratives with runtime story perspective switching
   - Backwards-compatible: compile-time markers remain default if context not provided

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
| Context-aware runtime tense/viewpoint | 🎯 v1.1 | Unblocks Recounting M9; requires design decision on context threading mechanism |
| Consolidate english_shared.rs | 🎯 v1.1 | Currently duplicated in src/ and ranting_derive/src/; must merge before shipping runtime features |

---

## Risk Mitigation

**Macro Complexity**: Regular refactoring; keep proc-macro logic focused; document architecture.

**Code Consolidation**: `english_shared.rs` is duplicated in both `src/` and `ranting_derive/src/` and has already diverged (noted in CLAUDE.md). This is a silent-bug trap for v1.1 features (runtime tense/viewpoint). Consolidate into a single canonical location BEFORE implementing context threading or new grammar rules.

**Table Maintenance**: Document adding new irregulars; encourage community PRs; keep v1.1 plural tables separate from v1.0 verb tables to avoid corruption.

**Performance Regressions**: Benchmark at phase end; profile compile-time and runtime; set performance budgets (no more than 10% slowdown per feature).

**Ecosystem Fragmentation**: Clear governance for companion crates; version-lock to core; single source of truth for grammar rules.

---

## How to Contribute

Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular plurals table, language modules, performance optimization

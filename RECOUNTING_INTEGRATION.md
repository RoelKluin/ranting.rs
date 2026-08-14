# Ranting ↔ Recounting Integration Analysis

**Date**: 2026-08-12  
**Context**: Recounting (parser-IF library for Rust) uses Ranting for all prose generation. This document tracks what Ranting provides today and what gaps exist for Recounting's roadmap.

> **Superseded (2026-08-14).** This analysis describes the v0.2.1 surface and is
> kept as a historical record. All three gaps it lists as blockers have since
> shipped: "Runtime Tense Selection", "Runtime Viewpoint Selection" and
> "Narration Context" are all delivered by `say_with!()` / `NarrationContext`
> (Phase 3, v1.1.0 — see `DONE.md` and `docs/API.md`), so the M9 "Blocked" row
> below is stale. Do not use this file to judge what Ranting supports today.

---

## Current State

### Ranting v0.2.1 ✅

**Working Features for Recounting**:
- Pronoun inflection (I, you, he, she, it, we, they—including singular they)
- Verb conjugation across 7 tenses (present, past, future, continuous, perfect)
- 118+ irregular verbs with phonetic rules
- Articles (a, an, some, the, these, those)
- Noun pluralization (regular + 100+ irregular)
- Possessives and object forms
- `Ranting` trait for custom types (`#[derive(Ranting)]`)
- `Noun` struct for standalone entities
- `say!()` macro for string interpolation with inflection
- Named and positional arguments
- Trait-based extensibility hooks for custom inflection (`inflect_verb_custom`, `inflect_pronoun_custom`, `inflect_article_custom`)

**Test Coverage**: 217+ tests (unit, integration, doctests) covering all major features.

---

## Recounting's Upstream Requirements

These are the four blockers explicitly documented in [recounting/ROADMAP.md](../recounting/ROADMAP.md#upstream-requirements-ranting) for **M9 (Story tense and viewpoint)**.

### 1. ❌ Runtime Tense Selection

**Current**: Tense is compile-time only via markers (`<` past, `=` continuous, `>` future).

```rust
say!("{=person <walk}");   // Compile-time: past tense
// Problem: Can't choose tense at runtime from game state
```

**Needed for Recounting**: Players should see the same story played in past tense, present, or future without rewriting narration. The tense must be selectable at runtime from `StoryState`.

**Estimated impact on Ranting**: Medium—requires threading a `Tense` enum through `Ranting::handle_placeholder()` and the macro pipeline. Backwards-compatible if `Tense` defaults to compile-time markers.

---

### 2. ❌ Runtime Viewpoint Selection

**Current**: Perspective (I, he, she, they) is baked into `Noun` at creation time.

```rust
let hero = Noun::new("Hero", "I");  // Always first-person
// Problem: Can't retell the same story from third-person perspective
```

**Needed for Recounting**: The same narration should work when displayed in:
- First person (I, we)
- Second person (you)
- Third person (he, she, they)

Viewpoint is a story setting, not a noun property.

**Estimated impact on Ranting**: High—requires separating `subject` (a property of the entity) from `narration_person` (a story-wide setting). Major refactor to how `Ranting` resolves pronouns.

**Design note**: This is orthogonal to tense. Both should be runtime values flowing through a context, not struct fields.

---

### 3. ❌ Narration Context

**Current**: Trait hooks are per-type (customizable via `#[derive(Ranting)]` attributes).

```rust
#[derive(Ranting)]
#[ranting(subject = "he")]
struct Pirate { /* ... */ }
```

Story-wide settings (formal vs. informal register, dialect, genre tone) don't have a standard home.

**Needed for Recounting**: A context object that flows through `say!()` and narration calls, carrying:
- Tense choice (M9)
- Viewpoint choice (M9)
- Any other story-wide settings (register, locale, narrative voice quirks)

**Estimated impact on Ranting**: High—requires threading a context through the macro expansion and runtime pipeline. Could follow patterns from other template engines (`askama`, `tera`).

**Design questions**:
- Should context be part of `Ranting` trait? (adds dependency on story concerns)
- Or a separate middleware layer? (keeps Ranting generic, layers responsibility)
- Can it remain opt-in? (defaults to current behavior if not provided)

---

### 4. ⚠️ List Writing Helper

**Current**: Ranting has individual noun/verb/article inflection but no dedicated list formatter.

**Needed for Recounting**: Format lists like:
```
"a sword, a shield, and a cloak"
"the jeweled dagger, the dusty scroll, and the golden key"
"a cat, a dog, some mice, and a parrot"  // mixed singulars/plurals
```

With correct:
- Oxford comma handling
- Article agreement with plurality
- Grouping of identical items ("three swords" not "a sword, a sword, a sword")

**Estimated impact on Ranting**: Low—mostly utility functions, possibly uses existing `Ranting` inflection. Could live in Recounting if it needs game-specific logic (e.g., excluding items by property).

**Current workaround**: Recounting can compose lists manually using existing inflection. This is not a hard blocker.

---

## Architectural Gaps & Technical Debt

### Known Issues in Ranting

1. **Diverged Code**: `english_shared.rs` exists in both `src/` and `ranting_derive/src/` and **has already diverged** (noted in CLAUDE.md). This creates risk of inconsistent inflection between runtime and compile-time paths.
   - **Impact on Recounting**: Low for current features, but high for any new grammar rules (tense, viewpoint). Requires careful synchronization.
   - **Mitigation**: Consolidate into one canonical location during v1.1+ refactoring.

2. **Macro-Only Runtime**: Tense/viewpoint/context changes require macro expansion. Can't inject a parameter into existing `say!()` calls without rewriting them.
   - **For Recounting**: Will need either:
     - A new macro form: `say_with!(context, "...")` to accept a runtime context, OR
     - A trait method: `Noun::say_tense_aware(...)` that doesn't use macros
   - **Recommendation**: Design the context threading now (even if unused in v0.2) to avoid major breakage later.

3. **Edition 2024**: Ranting uses Rust 2024 edition, but most projects (including Recounting) use 2021. Potential compatibility issues if ecosystem lags adoption.
   - **For Recounting**: Monitor and test regularly. File an issue if incompatibilities arise.

---

## Timeline & Impact on Recounting Milestones

| Recounting Milestone | Ranting Dependency | Blocker? | ETA |
|---|---|---|---|
| **M0** (Skeleton) | ✅ Path dependency + workspace setup | ❌ No | Done |
| **M1** (World model) | None (Ranting not used yet) | ❌ No | — |
| **M2** (Narration) | ✅ Basic Noun/inflection in `Ranting` | ❌ No | Ready |
| **M3-M5** (Rules, parser, stdlib) | None | ❌ No | — |
| **M6** (Cloak of Darkness) | ✅ Basic inflection sufficient | ❌ No | Ready |
| **M7** (Macros) | None | ❌ No | — |
| **M8** (Story structure) | None (yet) | ❌ No | — |
| **M9** (Tense & viewpoint) | ❌ #1, #2, #3 above | ✅ **YES** | Blocked |
| **M10** (Beyond) | TBD | — | — |

**Conclusion**: Recounting can proceed through M6/M7/M8 (18+ months of work) without upstream changes. M9 has hard dependencies on Ranting v1.1+ features.

---

## Recommendations

### For Recounting

1. **Now (M0-M8)**: Proceed with basic Ranting integration. Use compile-time tense markers if narration needs variation. Document this as a limitation in M2 design docs.

2. **Before M9**: File formal feature requests in ranting repo with:
   - Concrete use cases (how M9 should work end-to-end)
   - Performance constraints (latency of tense/viewpoint switches)
   - Backwards-compatibility requirements

3. **During M9 planning**: Coordinate on timeline with Ranting maintainer. May need to:
   - Contribute implementation help
   - Accept a v1.1 release cycle (likely Q1-Q2 2026, per Ranting roadmap)
   - Consider shipping M9 MVP with workarounds (runtime selection via separate narration builders) while waiting for trait-level support

### For Ranting

1. **v1.1.0 Roadmap** (already planned, ~8-12 weeks post-v1.0):
   - Prioritize trait-based extensibility (partially done)
   - Design context-passing mechanism for tense/viewpoint (not yet started)
   - Add `say_with!(context, ...)` macro variant or trait method for runtime context

2. **Code Consolidation**: Merge diverged `english_shared.rs` copies before v1.1 to prevent inflection inconsistencies.

3. **v1.1 Success Metric for Recounting**: Publish a `recounting-narrate` example using Ranting v1.1's context hooks to generate tense- and viewpoint-aware narration.

---

## List Writing Design Option

If Recounting needs list formatting, consider this layering:

**Option A: Minimal (Recounting handles)**
```rust
// Recounting builds lists by composing Ranting
fn describe_items(items: &[Item], ctx: &Context) -> String {
    let formatted: Vec<String> = items.iter()
        .map(|item| format!("{a item}", item.ranting_noun()))
        .collect();
    format!("{}", ListFormatter::new(&formatted).oxford_comma().build())
}
```

**Option B: Ranting owns it (future)**
Ranting v1.2+ could provide:
```rust
say!("{list items a=true oxford=true}", items)  // compact syntax
```
But this requires `Ranting` impl on collections (v1.1+ feature), and may not be worth complexity if Recounting's use case is game-specific.

---

## References

- [Recounting ROADMAP.md](../recounting/ROADMAP.md)
- [Recounting .claude/rules/01-project.md](../recounting/.claude/rules/01-project.md)
- [Ranting ROADMAP.md](ROADMAP.md)
- [Ranting CLAUDE.md](CLAUDE.md)
- [Ranting docs/EXTENSIBILITY.md](docs/EXTENSIBILITY.md)

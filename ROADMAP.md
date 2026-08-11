# ROADMAP.md

**Ranting** is a lightweight, declarative library for ergonomic, grammatically-correct formatted output in Rust. This roadmap charts the path to v1.0 and beyond.

---

## Vision

Ranting solves the problem of writing natural-sounding, dynamic user-facing text in Rust. Developers should be able to write grammar rules once and let the library handle inflection automatically—no manual string juggling, no brittle conditional text generation.

**Target**: Game engines, interactive fiction, chatbots, and any application that generates user-visible prose.

**Design principle**: Lightweight and ergonomic. Users write clean, declarative placeholders; Ranting handles the grammar.

---

## Current State (v0.2.1)

✅ **Working**:
- Core `Ranting` trait and `Noun` struct
- Basic subject/object/possessive pronouns (I, you, he, she, it, we, they)
- Simple present tense (do/does, am/are/is)
- Articles (a, an, some, the) with conditional display
- Optional article logic
- `say!()`, `ack!()`, `nay!()` macros
- Partial `ask!()` macro (recently added)
- Positional arguments only
- Integration tests and doctests

❌ **Not working**:
- Named arguments (`person = my_var` syntax)
- Empty placeholders (`{}`)
- **Tense handling**: Modals (can/could/would/should/might/must/shall/will) and auxiliary verbs (is/was/were/have/has/had) work for present and past via `IrregularPluralVerb` table. Missing: regular past-tense formation (-ed suffix handling), irregular lexical past/participle forms (go→went, see→saw), continuous/progressive forms (-ing suffix), and tense-aware suppression of spurious 3rd-person-singular `-s` suffix (e.g. "walked" was incorrectly conjugated to "walkeds").
- Continuous/progressive forms (-ing) — no auto-formation or recognition yet
- Irregular noun plurals (child→children, person→people) — no support, needs `inflections` crate integration
- Custom pluralization rules
- Reflexive forms (myself, yourself, itself) — no pronoun table yet, currently just literal string concatenation
- Comparative/superlative adjectives (better, best) — no support
- Recursive type inflection
- Multi-language support
- Nested/complex type handling
- Format specs in placeholders
- Derived traits alongside Ranting

---

## Release Phases

### Phase 1: **v0.3.0** — Foundation & Ergonomics
*Goal: Make Ranting more ergonomic and feature-complete for present-tense usage.*

**Named Arguments & Empty Placeholders**:
- Implement named argument parsing: `say!("{=person}", person = my_var)`
- Support empty placeholders: `say!("{}", my_var)` using positional/named order
- This is the highest-value feature—users expect `format!()`-like ergonomics
- **Tradeoff**: Slight macro complexity increase for significantly better UX

**Improve Error Messages**:
- Add compile-time validation for placeholder syntax
- Report mismatched argument counts with helpful messages
- Point users to specific placeholder issues in source
- **Tradeoff**: Larger error handling code vs. developer productivity

**Gender-Neutral Pronouns**:
- Expand `Noun` to support singular they/them
- Document inclusive language patterns
- Add examples showing they/them inflection
- **Tradeoff**: Minimal; they already work with current architecture

**Test Coverage**:
- Aim for >80% code coverage
- Add tests for edge cases in argument parsing
- Test error conditions thoroughly
- Use/Convert to table-driven tests, where possible

**Estimated timeline**: 2-3 months

---

### Phase 2: **v0.4.0** — Grammar Depth
*Goal: Handle past, future, conditional, and continuous tenses.*

**Verb Tense Support**:
- Past tense: was/were, had, did
- Future/conditional: will, would, shall, should, may, might, can, could
- Continuous/progressive: -ing forms (is running, was running, will be running)
- **Implementation strategy**:
  - Extend placeholder syntax: `{=person do}` for present → `{=person did}` for past
  - Add `to_past()`, `to_future()` methods or similar to trait
  - use prefix markers: `{=person @past do}`
  - **Tradeoff**: Syntax clarity vs. flexibility

**Irregular Verbs & Plurals**:
- Implement built-in table for common irregulars (go→went, child→children)
- Explore using a crate like `inflections` or `lexeme` to avoid reinventing
- Allow `#[derive(Ranting)]` to specify custom irregular mappings via attributes
- **Strategy**: Built-in + override capability
  - Default implementations for common English irregulars
  - `#[ranting(past = "went", plural = "children")]` for custom types
- **Tradeoff**: Maintenance burden of irregular table vs. coverage

**Reflexive Forms**:
- Support myself, yourself, himself, herself, itself, ourselves, themselves
- Extend placeholder: `{~person do}` for reflexive (currently means adjective)
- Or introduce new marker: `{*person do}` for reflexive
- **Tradeoff**: Adding case to inflection system

**Comparative & Superlative Adjectives**:
- Handle degree: good → better → best, bad → worse → worst
- Extend adjective case to support degree markers
- `{>person}` for comparative, `{>>person}` for superlative (or similar syntax)
- **Tradeoff**: More complex adjective handling

**Ecosystem: `ranting-macros`**:
- Ship first version with advanced utilities:
  - `nested_say!()` for nested inflection
  - `conditional_say!()` for if-else text generation
  - Helpers for bulk text formatting
- Keep simple; defer complex features to later versions
- **Design**: Companion crate uses core `ranting` trait, doesn't fork logic

**Test Coverage**: Maintain >80%, add tense-specific edge cases.

**Estimated timeline**: 3-4 months

---

### Phase 3: **v1.0.0** — Polish & Stability
*Goal: Feature-complete, well-tested, production-ready.*

**Recursive Type Inflection**:
- Support collections and nested Ranting types
- `Vec<Item>` where Item: Ranting
- `Option<Person>`, `Box<Noun>`, etc.
- **Strategy**:
  - Use `#[derive(Ranting)]` to generate recursive implementations
  - Handle `unwrap()` / `map()` internally when needed
  - **Challenge**: Lifetime and reference complexity
  - **Tradeoff**: Convenience vs. unexpected `unwrap()` calls in derived code

**Custom Pluralization Framework**:
- Trait-based API for user-defined plural rules
- `impl PluralizeRule for MyType { ... }`
- Support domain-specific pluralization (e.g., units: 1 liter, 2 liters)
- **Design**: Trait that types can implement; `derive(Ranting)` can delegate to it
- **Tradeoff**: Added complexity for subset of users

**Full Format Spec Support**:
- Allow format specs in placeholders: `{=person:?}`, `{#count:05}`
- Apply format spec after inflection
- **Challenge**: Parsing format specs correctly alongside Ranting syntax
- **Tradeoff**: More complex regex/parsing logic

**Stacking Derives**:
- Ensure `#[derive(Debug, Clone, Ranting)]` works seamlessly
- Test with other common derives (Serialize, Deserialize, etc.)
- **Challenge**: Proc-macro interaction with other derives
- **Tradeoff**: Minimal; mostly testing

**Comprehensive Error Handling**:
- Both compile-time and runtime error paths mature
- Helpful messages for all error categories
- Documentation of error handling patterns for library users

**Documentation & Examples**:
- Extensive API docs with examples for all features
- Tutorial: "Writing Dynamic Text with Ranting"
- Cookbook: common patterns (storytelling, chatbots, game dialogue)
- Migration guide from v0.2 → v1.0

**Performance Audit**:
- Profile compile-time macro expansion
- Profile runtime inflection cost
- Optimize both without sacrificing readability
- Benchmark against `format!()`

**Test Coverage**: >85%, including all tense combinations and edge cases.

**Success Criteria**:
- All features from Phases 1-3 working
- >85% test coverage
- Comprehensive error messages
- Ecosystem ready: `ranting-macros` v1.0.0 companion
- Documentation complete and polished
- **No critical known issues**

**Estimated timeline**: 3-4 months after Phase 2

**Release strategy**: Allow pre-1.0 minor versions to break semver if needed. At 1.0, commit to semver.

---

## Post-1.0: Future Directions

### `ranting-i18n` (Companion Crate)
- Multi-language support: German, French, Spanish, Japanese, etc.
- Modular language modules
- Hook into core Ranting trait for language selection
- Design as separate crate to keep core lightweight
- **Timeline**: 6-12 months after v1.0

### Advanced Features (Speculative)
- Dialogue formatting with automatic punctuation and breaks
- Pluralization of entire phrases (not just nouns)
- Subjunctive mood and hypotheticals
- Context-aware inflection (formal vs. informal register)
- Performance optimizations (cached inflection, const generics)

---

## Key Decisions & Tradeoffs

### 1. Syntax: Placeholder Markers vs. Method Calls
**Decision**: Continue with placeholder-based `{...}` syntax (not trait methods).
- **Why**: Ergonomic and declarative; mirrors `format!()`
- **Tradeoff**: Macro complexity, but worth it for UX

### 2. Compile-Time vs. Runtime Inflection
**Decision**: Hybrid—parse at compile time, apply inflection at runtime.
- **Why**: Allows dynamic Ranting types while catching syntax errors early
- **Tradeoff**: Cannot optimize away all runtime work without losing flexibility

### 3. Named Arguments Implementation
**Decision**: Parse Rust's native named-argument syntax (like `format!()` in 1.58+).
- **Why**: Consistency with Rust conventions
- **Challenge**: Requires careful syn parsing to extract named args from macro input
- **Benefit**: Users already know the syntax

### 4. Recursive Types
**Decision**: Full support via derived `#[derive(Ranting)]` recursion.
- **Why**: Natural for complex types (containers, optionals)
- **Tradeoff**: More generated code, risk of unexpected `unwrap()` in derived implementations
- **Mitigation**: Clear documentation + examples showing how recursion works

### 5. Custom Pluralization
**Decision**: Both built-in irregulars + trait-based extensibility.
- **Why**: Handles 90% of use cases (English irregulars) while allowing domain-specific rules
- **Tradeoff**: Maintenance of irregular table
- **Mitigation**: Use external crate if available; community contributions welcome

### 6. Multi-Language
**Decision**: English-only in core; design for extensibility; separate crate post-1.0.
- **Why**: Keeps core small, allows language modules to evolve independently
- **Tradeoff**: No multi-language before 1.0, but architecture supports it

### 7. Format Specs
**Decision**: Full support in 1.0 (apply after inflection).
- **Why**: Users expect `format!()`-like flexibility
- **Tradeoff**: Parsing complexity; must distinguish Ranting markers from format specs

### 8. Error Handling
**Decision**: Both compile-time validation + runtime errors.
- **Why**: Maximum developer experience
- **Tradeoff**: More error handling code, but justified by UX benefit

### 9. Pre-1.0 Breaking Changes
**Decision**: Allow breaking changes in 0.x minor versions if needed.
- **Why**: Ensures 1.0 is solid; no accumulated tech debt
- **Strategy**: Deprecation period before removal; clear migration guides
- **Communication**: Changelog and migration docs for each breaking change

### 10. Verb Tense & Inflection Data Sourcing (Phase 2)
**Decision**: Use `inflections` crate (optional, feature-gated) for noun pluralization only; build verb tense, reflexive forms, and comparative/superlative entirely in-house as free functions.
- **Why**: No Rust crate covers verb conjugation, reflexive pronouns, or adjective degrees. `inflections` crate is mature for pluralization but doesn't help with tenses. Free functions match the existing architecture (all inflection logic is in `src/language/` as free functions, not trait methods) and avoid the `dyn Ranting` / delegating-impl trap that trait methods would create.
- **Data location**: Verb tense, reflexive, and adjective-degree tables live in `src/language/` (runtime crate only), deliberately NOT duplicated into `ranting_derive/src/language/`, since they aren't part of the placeholder-parsing regex/enum surface CLAUDE.md requires kept in sync.
- **Structural isolation**: New verb-tense tables are separate from `IrregularPluralVerb`/`IRREGULAR_VERBS_1ST`/`IRREGULAR_VERBS_3RD`, which use fragile index-coupled arithmetic into parallel arrays. Do not extend those structures; do not reorder variants.
- **Placeholder syntax**: Prefer `{=person walked}` / `{=person went}` (user writes the already-inflected verb; `inflect_verb` classifies and passes through) over a new `{=person @past do}` marker syntax or a `to_past()` trait method. This works via the existing generic `post` capture group with zero regex/macro change.
- **Timeline**: Skeleton (v0.3.1): detect and preserve past/continuous, fix "walkeds" bug. Full system (v0.4.0): auto-conjugation, future/conditional tenses, -ing forms, reflexive/comparative/superlative.

---

## Risk Mitigation

### Compile-Time Complexity Spiral
**Risk**: Macro code becomes unmaintainable as features accumulate.
**Mitigation**:
- Regular refactoring of `ranting_impl.rs` and `str_lit.rs`
- Keep proc-macro logic focused; move business logic to runtime when possible
- Document macro architecture for future maintainers

### Irregular Verb/Plural Table Maintenance
**Risk**: Table becomes stale or incomplete. New verb-tense tables accidentally corrupt the index-coupled `IrregularPluralVerb` enum arithmetic.
**Mitigation**:
- Use external crate (`inflections`) for noun pluralization only; build verb tenses in-house via structurally-independent tables
- Document how to add new verbs/irregulars
- Encourage community PRs for missing entries
- Keep new tense tables separate from `IrregularPluralVerb`/`IRREGULAR_VERBS_1ST`/`IRREGULAR_VERBS_3RD` to avoid silent corruption of existing 3rd-person-singular verb conjugation

### Performance Regressions
**Risk**: Each new feature makes compilation or runtime slower.
**Mitigation**:
- Benchmark at end of each phase
- Profile both compile-time and runtime
- Set performance budgets (e.g., "no more than 10% compilation slowdown per feature")

### Ecosystem Fragmentation
**Risk**: Companion crates diverge or compete.
**Mitigation**:
- Clear governance: companion crates live in same repo or coordinate closely
- Version-lock companion crates to core
- Single source of truth for grammar rules

---

## Success Metrics

**By v1.0**:
- ✅ 200+ GitHub stars (lightweight library adoption signal)
- ✅ >80% test coverage
- ✅ All planned grammar features working
- ✅ Documentation rated "excellent" by users
- ✅ No critical unresolved issues
- ✅ Ecosystem ready: `ranting-macros` v1.0

**By v1.2 (6 months post-1.0)**:
- ✅ Active community (PRs, discussions, issues)
- ✅ `ranting-i18n` foundation laid or shipped
- ✅ Case studies: published examples from games, chatbots, or real projects
- ✅ Performance within 5% of `format!()` for equivalent output

---

## How to Contribute

This roadmap is evolving. Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular verb/plural table, language modules, performance optimization

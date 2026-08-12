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
- Named arguments (`person = my_var` syntax)
- Empty placeholders (`{}`)
- **Tense markers** (Phase 2): `{=person <walk}` (past), `{=person =walk}` (present continuous), `{=person >walk}` (future), `{=person <=walk}` (past continuous), `{=person %walk}` (present perfect), `{=person <%walk}` (past perfect)
- Regular past-tense formation: adds -ed with phonetic rules (y→ied, e→remove, etc.)
- Irregular past tense via built-in table (go→went, see→saw, etc.)
- Continuous/progressive forms: -ing with phonetic rules (silent e, consonant doubling, ie→y)

❌ **Not working / Deferred to v1.1+**:
- Conditional tenses (would + base verb) — deferred; modals can already conjugate via existing table
- Irregular noun plurals (child→children, person→people) — v1.1 feature; needs table or external crate
- Custom pluralization rules — v1.1
- Reflexive forms (myself, yourself, itself) — v1.1 or later; needs pronoun case expansion
- Comparative/superlative adjectives (better, best) — v1.1 or later
- Recursive type inflection — v1.1
- Multi-language support — v1.2+; separate `ranting-i18n` crate after v1.0
- Nested/complex type handling — v1.1
- Format specs in placeholders — v1.0 (added in Q11 review if needed)
- Derived traits alongside Ranting — v1.0

---

## Release Phases

### Phase 1: **v0.3.0** — Foundation & Ergonomics
*Goal: Make Ranting more ergonomic and feature-complete for present-tense usage.*
- Named Arguments & Empty Placeholders
- Improve Error Messages
- Gender-Neutral Pronouns ("inclusive-pronouns" feature gated)
- Improved Test Coverage
*Status*: **complete** - details in DONE.md

---

### Phase 2: **v1.0.0** — Grammar Depth
*Goal: Handle past, future, and continuous tenses; lock in API for v1.0.*

**Verb Tense Support** ✅ (Stages 1-4 complete in v1.0):
- ✅ **Stage 2** — Past tense marker: `{=person <walk}` → "He walked" / irregular: "He went"
- ✅ **Stage 2** — Present continuous: `{=person =walk}` → "He is walking"
- ✅ **Stage 2** — Future tense: `{=person >walk}` → "He will walk"
- ✅ **Stage 2** — Past continuous: `{=person <=walk}` → "He was walking"
- ✅ **Stage 4** — Present perfect: `{=person %walk}` → "He has walked" / irregular: "He has gone"
- ✅ **Stage 4** — Past perfect: `{=person <%walk}` → "He had walked" / irregular: "He had gone"
- **Implementation**: Marker-based syntax using `<`, `=`, `>`, `<=`, `%`, `<%` in placeholder post-position
  - Markers are composable at Rust level (not regex level) for easier extension
  - Compile-time conjugation in `ranting_derive`, runtime auxiliary insertion
  - All tense combinations (7 distinct tenses) working end-to-end by v1.0 release

**Irregular Verbs & Plurals** (v1.0 design decision per Specialist Review):
- ✅ 118 irregular past-tense verbs in built-in table (go→went, see→saw, take→took, etc.)
- ✅ Regular past-tense rules: -ed with phonetic handling (walk→walked, try→tried, like→liked)
- ✅ **NEW** (118 entries): Irregular past-participle table (go→gone, do→done, see→seen, etc.) for perfect tenses
- ✅ **v1.0 Debt Elimination** ✅: Verb table duplication (src/ + ranting_derive/src/) eliminated via build.rs + data/irregular_verbs.txt codegen (symlinked from ranting_derive/data/)
- ❌ Irregular noun plurals (child→children, person→people) — **v1.1 feature**; planned table-based approach
- ❌ Custom pluralization rules — **v1.1+**
- **Strategy**: Built-in English irregulars for all tenses by v1.0; irregular plurals deferred to v1.1 based on adoption feedback

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

**Documentation & Adoption** (Critical for v1.0):
- **Tutorial** ("Getting Started with Ranting"): 5 sections, 5 worked examples, 30-40 min read
- **Cookbook** (10 recipes): Game dialogue, chatbots, interactive fiction, gender-neutral pronouns, etc.
- **API docs**: Examples on every public function; cross-references between related features
- **ROADMAP clarity**: v1.0 scope (all tenses) vs. v1.1 scope (plurals, extensibility) locked in

**Technical Debt Elimination**:
- ✅ Verb table duplication → codegen (build.rs + data/irregular_verbs.txt, single source of truth)
- Pronoun array fragility → HashMap (eliminates index-coupling risk) [Tier 2]
- Derive macro attributes → Rationalize to 4 core (subject, name, singular_end, plural_end) [Tier 2]

**Test Coverage**: Maintain >85%, add property-based tests for verb conjugation rules.

**Success Criteria for v1.0**:
- All Phase 2 tense markers complete (7 tenses)
- >85% test coverage
- Tutorial + 10-recipe cookbook published
- Zero breaking changes commitment (semver locked)
- Zero critical known issues

**Estimated timeline**: 4-5 weeks (Tier 1 & 2 work from Design Review)

---

### Phase 3: **v1.1.0** — Plurals, Extensibility, and Ecosystem
*Goal: Complete core morphology; enable community contributions.*

**Specialist Consensus & User Priorities** (from Design Review):
- **Vision**: Adoption (primary) → Stability (v1.0) → Completeness (v1.1+)
- **Strategy**: v1.0 ships with complete, stable tense system + excellent docs. v1.1 adds plurals based on user feedback.

**Irregular Noun Plurals**:
- Support 100+ common irregular plurals: child→children, person→people, mouse→mice, goose→geese, etc.
- Build `data/plurals.toml` (parallel to verbs.toml); codegen into `src/language/plurals.rs`
- Integrate with existing `#[ranting(plural_end="...")]` attribute system
- **Time Estimate**: 24-32 hours
- **Rationale**: Frequently requested; enables "5 people walked" vs. "5 person walked"

**Trait-Based Inflection Extensibility**:
- Add `inflect_verb_custom()`, `inflect_noun_custom()` trait methods to `Ranting`
- Enable users to implement custom grammar rules (e.g., Scottish English, Elvish)
- **Design**: Default impls use built-in rules; users can override for domain-specific needs
- **Example**: `impl Ranting for ArchaicEnglish { fn inflect_verb_custom(...) { ... } }`
- **Time Estimate**: 16-20 hours (API design + 3 example impls)
- **Rationale**: Signals extensibility; enables creative use cases (fantasy languages, constructed languages)

**Reflexive Forms**:
- Support myself, yourself, himself, herself, itself, ourselves, themselves
- Add case marker (e.g., `{~person do}` becomes reflexive pronoun in context)
- **Time Estimate**: 8-12 hours
- **Rationale**: Completes core pronouns system

**Comparative & Superlative Adjectives**:
- Handle degree: good → better → best, bad → worse → worst
- Marker-based syntax (e.g., `{+person good}` for comparative, `{++person good}` for superlative)
- **Time Estimate**: 10-16 hours
- **Rationale**: Rounds out morphology; enables richer text generation

**Recursive Type Inflection**:
- Support collections and nested Ranting types
- `Vec<Item>` where Item: Ranting
- `Option<Person>`, `Box<Noun>`, etc.
- **Strategy**: Use `#[derive(Ranting)]` to generate recursive implementations
- **Time Estimate**: 12-16 hours
- **Rationale**: Enables complex data structures

**Performance & Stability Audit**:
- Profile compile-time macro expansion; optimize hot paths
- Profile runtime inflection cost; benchmark against `format!()`
- Verify >85% test coverage across all features
- Update CLAUDE.md with performance notes

**Test Coverage**: >85% across all v1.1 features.

**Success Criteria**:
- Irregular plurals support for 100+ nouns
- Trait extensibility API stable and documented
- Reflexive forms + comparative/superlative working
- Zero breaking changes from v1.0
- Community contributions: 2-3 ecosystem forks (ranting-spanish, domain-specific variants)
- GitHub engagement: 10+ answered issues/discussions

**Estimated timeline**: 8-12 weeks post-v1.0 release

**Release strategy**: Semver-locked; all features are additive, no breaking changes.

---

## Post-v1.1: Future Directions

### v1.2.0: Ecosystem Expansion
- **`ranting-i18n` Companion Crate** (12-16 weeks post-v1.0):
  - Multi-language support: German, French, Spanish, Japanese, etc.
  - Modular language modules using trait-based extensibility from v1.1
  - Hook into `Ranting` trait for language selection
  - Design as separate crate to keep core lightweight
  - **Rationale**: Proves extensibility model works; enables global adoption

### v1.3+: Advanced Features
- Dialogue formatting with automatic punctuation and breaks
- Pluralization of entire phrases (not just nouns)
- Subjunctive mood and hypotheticals
- Context-aware inflection (formal vs. informal register)
- Performance optimizations (cached inflection, const generics)
- **Community-driven**: Prioritized by user feedback from v1.0-1.1

---

## Key Decisions & Tradeoffs (Locked in via Design Review, 2026-08-12)

**5-Specialist Independent Evaluation**: All decisions below reflect unanimous or strong specialist consensus (60%+). See DESIGN_REPORT_SUMMARY.md for full review details.

### 1. Two-Crate Architecture ✅ **UNANIMOUS (5/5)**
**Decision**: Keep ranting + ranting_derive split.
- **Why**: Proc-macro crates are Rust-required; mirrors industry standard (serde, tokio)
- **Status**: Locked; no changes needed

### 2. Verb Table Duplication → Codegen ⚠️ **SPECIALIST CONSENSUS (60%)**
**Decision**: Eliminate manual duplication via build.rs + data/verbs.toml.
- **Why**: Single source of truth; scales as tables grow (perfect participles, future plurals)
- **v1.0 Effort**: 12-16 hours; eliminates sync bugs
- **Rationale**: Codegen preferred over external crate for phase-2 tables

### 3. Pronoun Tables → HashMap ⚠️ **SPECIALIST LEAN (60%)**
**Decision**: Replace fragile index-coupled arrays with HashMap.
- **Why**: Eliminates silent coupling risk; readability improves; zero measurable performance impact
- **v1.0 Effort**: 4-8 hours; enables future pronoun additions
- **Rationale**: Maintainability over micro-optimization

### 4. Compile-Time vs. Runtime Split ✅ **UNANIMOUS (5/5)**
**Decision**: Keep hybrid approach (compile-time parsing, runtime inflection).
- **Why**: Catches syntax errors early; enables extensibility
- **Status**: Locked; documented as intentional architecture

### 5. Trait-Based Inflection Methods 🎯 **DEFER to v1.1 (80% future preference)**
**Decision**: Keep free functions for v1.0; add trait methods in v1.1 via default impls.
- **Why**: Avoids breaking change; current design works; extensibility comes post-v1.0
- **v1.1 Effort**: 16-20 hours; enables custom rule implementations

### 6. Placeholder Syntax ⚠️ **KEEP CURRENT (60% favor); DOCUMENT (100%)**
**Decision**: Full grammar syntax kept; solve learning curve via tutorial + cookbook.
- **Why**: Syntax is powerful + tested; UX concern is documentation, not expressiveness
- **v1.0 Effort**: 40-60 hours documentation (tutorial + 10-recipe cookbook)
- **Rationale**: Better docs resolve concerns without breaking change

### 7. Grammar Rules Sourcing ⚠️ **BUILT-IN + DEFER EXTENSIBILITY (60%)**
**Decision**: v1.0 uses built-in English rules; v1.1 adds trait-based extensibility.
- **Why**: Built-in rules mature + tested; extensibility design deferred for v1.1
- **v1.0**: All tenses (7), no external dependencies
- **v1.1**: Custom rule trait, example impls (Scottish, Elvish, etc.)

### 8. Test Strategy ✅ **INTEGRATION-HEAVY + ADD PROPERTY TESTS (80%)**
**Decision**: Keep integration-heavy primary; add targeted property-based tests for verbs.
- **Why**: Integration tests are right for DSL; property tests catch grammar edge cases
- **v1.0 Effort**: 8-12 hours; validates phonetic rules systematically
- **Rationale**: Additive, not breaking; complements integration suite

### 9. ROADMAP Priority 🎯 **USER DECISION: PERFECT TENSES (Stage 4)**
**Decision**: Continue Phase 2 momentum; perfect tenses in Stage 4 (v1.0); irregular plurals in v1.1.
- **Why**: Tense system is logically coherent; plurals are orthogonal; continuing Phase 2 maintains momentum
- **v1.0 Tenses**: Past, Continuous, Future, Past-Continuous, Present-Perfect, Past-Perfect (7 total)
- **v1.1 Plurals**: Irregular nouns + reflexive + comparative/superlative

### 10. Derive Macro Attributes 🎯 **RATIONALIZE to 4 CORE (60%)**
**Decision**: Clarify 4 core attributes (subject, name, singular_end, plural_end); move cosmetics to builder.
- **v1.0 Effort**: 4-6 hours cleanup + documentation
- **Rationale**: Reduces confusion; improves discoverability

### 11. Documentation 🎯 **UNANIMOUS CRITICAL (5/5)**
**Decision**: Ship v1.0 with comprehensive tutorial + 10-recipe cookbook.
- **Why**: Documentation is adoption blocker; highest-ROI intervention
- **v1.0 Effort**: 40-60 hours (estimated 10x GitHub stars impact)
- **Rationale**: All specialists agree; essential for market success

### 12. Strategic Vision 🎯 **USER DECISION: Adoption → Stability → Completeness**
**Decision**: v1.0 focuses adoption + stability; v1.1 adds completeness based on feedback.
- **v1.0**: Rock-solid tenses + docs + semver commitment
- **v1.1**: Plurals, extensibility, ecosystem growth
- **v1.2+**: Community-driven completeness (reflexive, comparative, i18n)

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

**v1.0 Success Criteria** (4-5 weeks from Design Review):
| Metric | Target | Status |
|--------|--------|--------|
| Test coverage | >85% | ✅ Foundation + proptest coverage (178 tests passing, 10 new property-based tests for conjugation) |
| GitHub stars | 400+ | Currently ~200; adoption push with v1.0 launch |
| crates.io downloads | 5k+/month | Adoption signal; tracked post-launch |
| Documentation | Tutorial + 10-recipe cookbook | ✅ Complete (docs/TUTORIAL.md, docs/COOKBOOK.md, compiled test suite) |
| API stability | Zero breaking changes post-v1.0 | Semver locked; versioning strategy established |
| Tense system | All Phase 2 complete (7 tenses) | Stages 2-4 wired; perfect tenses Stage 4 |
| Debt elimination | Codegen + HashMap refactors done | ✅ Verb table codegen complete (build.rs, data/irregular_verbs.txt, symlink strategy); HashMap refactor deferred to Tier 2 |

**v1.1 Success Criteria** (8-12 weeks post-v1.0):
| Metric | Target | Status |
|--------|--------|--------|
| Irregular plurals | 100+ common forms | Table-based; parallel to verbs.toml approach |
| Trait extensibility | InflectionRule trait + 3 example impls | Enables ecosystem forks (ranting-spanish, etc.) |
| Ecosystem projects | 2-3 forks; domain-specific variants | Community-contributed implementations |
| Community engagement | 10+ answered issues/discussions | Active maintainer presence post-v1.0 |
| Refactoring complete | Reflexive + comparative/superlative done | Phase 3 features shipped |

**v1.2+ Success Criteria** (6+ months post-v1.0):
- `ranting-i18n` foundation or shipped (multi-language support)
- Case studies: published examples from games, chatbots, real projects
- Performance within 5% of `format!()` for equivalent output
- Community fork activity (3+ maintained ecosystem projects)

---

## How to Contribute

This roadmap is evolving. Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular verb/plural table, language modules, performance optimization

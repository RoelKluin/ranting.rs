# SDD ledger — plan: docs/superpowers/plans/2026-08-12-trait-extensibility-impl.md

## Base commit
84f10651d6a7c40deecebebd6e85a1a4d8c3dca4

## Tasks

- [x] Task 1: Add PronounCase Enum and Trait Methods — COMPLETE
  - Commit: 339af7f
  - Tests: 188 passing (180 integration + 8 doctests)
  - Review: ✅ APPROVED (Spec ✅ + Quality ✅)

- [x] Task 2: Implement Custom Method Hooks in handle_placeholder() — COMPLETE
  - Commits: f2d7139, 202facb
  - Initial review: NEEDS_FIXES (3 critical findings)
  - Fix round 1/1: scoped re-review APPROVED
  - Tests: 217 passing (29 unit + 180 integration + 8 doc)

- [x] Task 3: Write Integration Tests for Custom Inflection — COMPLETE
  - Commit: 5baa10e
  - 9 tests created (PirateNoun, Dignitary, SpanishFeminine, ScottishHighlander, PlainNoun)
  - Tests: 9/9 passing, 226 total (no regressions)
  - Review: ✅ APPROVED (Spec ✅ + Quality ✅)

- [x] Task 4: Write EXTENSIBILITY.md Documentation — COMPLETE
  - Commits: 7026d7d, 2cdb4a4
  - Initial review: NEEDS_FIXES (1 quality issue)
  - Fix round 1/1: scoped re-review APPROVED
  - Documentation: 579 lines, all 7 sections, API reference, 3 examples

- [x] Task 5: Update ROADMAP.md and Final Verification — COMPLETE
  - Commit: 90d9ade
  - ROADMAP: Priority 2 marked complete with 7 feature bullet points
  - Tests: 226 passing (29 unit + 189 integration + 8 doc), zero regressions
  - Code quality: clippy passed, fmt compliant, no new warnings
  - Integration: all 5 tasks verified working together

---

## Summary

**Implementation Complete:** Trait-Based Inflection Extensibility (v1.1.0 Priority 2)

**Total Commits:** 7 atomic, reviewable commits
- 339af7f: Task 1 — PronounCase enum and trait methods
- f2d7139: Task 2 — Integration hooks in handle_placeholder()
- 202facb: Task 2 fix — Spacing and param corrections
- 5baa10e: Task 3 — 9 integration tests
- 7026d7d: Task 4 — EXTENSIBILITY.md documentation
- 2cdb4a4: Task 4 fix — Parameter type documentation
- 90d9ade: Task 5 — ROADMAP update and final verification

**Test Status:** 226 passing (no regressions from v1.0)

**Review Status:** All 5 tasks approved individually; whole-branch review discovered 2 BLOCKING findings (C1/C2: article hook panic regression on {these x}/{those x}); critical fix (commit dfd0d96) scoped re-review APPROVED; proceeding to final whole-branch review

---

## Critical Fix (Post-Final Review)

**Blocking Issues from Whole-Branch Review:**
- C1: Uncaught panic regression in get_article_or_so() on {these x}/{those x} paths for non-singularizable plural nouns
- C2: catch_unwind masking the real regression; broken under panic="abort"

**Fix Applied:**
- Commit dfd0d96: Lazy singular computation in article custom hook paths
  - Use noun.name(false) for "the"/"these"/"those" (no singularization needed)
  - Conditional inflect() for "a"/"an"/"some" (only when !as_plural)
  - Removed catch_unwind masking
  - Added regression test in edge_cases.rs
- Scoped re-review: ✅ APPROVED (all criteria met)
- Tests: 227 passing (226 baseline + 1 new regression test)

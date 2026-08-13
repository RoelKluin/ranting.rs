# Phase 2 (v0.4.0) Implementation Plan: Grammar Depth

> **Superseded.** This plan predates Phase 2's completion. See `DONE.md` (Phase 2
> is marked complete, all 7 tenses shipped) and `ROADMAP.md`'s "Current State
> (v1.2.1)" section for what's actually current. Kept here as a historical
> record only — the "Status: In Progress" line below and the "walkeds" bug it
> describes are both stale; that bug is fixed.

**Status**: In Progress (skeleton work started) — historical, see banner above
**Timeline**: 3-4 months | **Target Coverage**: >85%

---

## Overview

Phase 2 adds past, future, conditional, and continuous tenses along with irregular verbs, reflexive forms, and comparative/superlative adjectives. The hybrid approach integrates the `inflections` crate for noun pluralization (optional, feature-gated in a later stage) while building verb/adjective systems directly in Ranting as free functions in `src/language/`, consistent with existing architecture.

**Key architectural decision**: All tense/reflexive/comparative logic uses **free functions**, not `Ranting` trait methods. This matches the pattern of existing inflection functions (`inflect_verb`, `inflect_possesive`, `inflect_objective`), avoids the `dyn Ranting` / delegating-impl trap, and keeps the two-file sync burden focused only on the placeholder-parsing regex/enums (`english_shared.rs` copies).

**Critical requirement**: Stages 2 and 2.5 must be completed together to ensure correct English grammar:
- Stage 2 (tense markers) alone produces `"He walks"` and `"She running"` (both incorrect without auxiliary verbs)
- Stage 2.5 (auxiliary insertion) fixes both to produce `"He will walk"` and `"She is running"` (grammatically correct)

---

## Current State (Skeleton Phase)

**Known bug discovered in exploration**: `say!("{=w walked}", Noun::new("", "he"))` currently produces `"He walkeds"` because `inflect_verb` is present-tense-only and blindly appends 3rd-person `-s` to verbs it doesn't recognize. This skeleton fixes that bug by classifying tense and suppressing suffix rules for non-present forms.

---

## Stage 1: Verb Tense Skeleton (Week 1-2, In Progress)

### Objective
Fix the `"walkeds"` bug; detect and preserve already-inflected verb forms (past, continuous); lay groundwork for full Stage 2 verb tense system.

### Architecture

**New module**: `src/language/verb.rs` (runtime crate only — **not** duplicated into `ranting_derive`)

```rust
pub(crate) enum Tense {
    Present,
    Past,
    Continuous,
}

pub(crate) static IRREGULAR_PAST: &[(&str, &str)] = &[
    ("go", "went"),
    ("see", "saw"),
    ("take", "took"),
    ("make", "made"),
    ("get", "got"),
    ("come", "came"),
    ("give", "gave"),
    ("know", "knew"),
    ("think", "thought"),
    ("find", "found"),
    ("be", "was"),
    ("have", "had"),
    ("do", "did"),
    ("say", "said"),
    ("go", "went"),
    ("eat", "ate"),
    ("run", "ran"),
    ("sit", "sat"),
    ("stand", "stood"),
    ("write", "wrote"),
];

pub(crate) fn detect_tense(verb: &str) -> Tense {
    // Check irregular past table
    if IRREGULAR_PAST.iter().any(|(base, _past)| verb.to_lowercase() == *_past) {
        Tense::Past
    }
    // Check regular past: -ed suffix
    else if verb.to_lowercase().ends_with("ed") && verb.len() > 2 {
        Tense::Past
    }
    // Check continuous: -ing suffix
    else if verb.to_lowercase().ends_with("ing") && verb.len() > 3 {
        Tense::Continuous
    }
    // Default to present
    else {
        Tense::Present
    }
}
```

### Wiring into `inflect_verb`

File: `src/language/english.rs`, lines 69-85 (`"he" | "she" | "it"` branch):

**Before** (current buggy behavior):
```rust
"he" | "she" | "it" => {
    if let Ok(mut val) = IrregularPluralVerb::from_str(s).map(|e| e as usize) {
        // irregular verb lookup...
    } else if s.ends_with(['s', 'o', 'x']) || s.ends_with("ch") || s.ends_with("sh") {
        uc_1st_if(s, uc) + "es"
    } else if let Some(p) = s.strip_suffix('y').filter(...) {
        uc_1st_if(p, uc) + "ies"
    } else {
        uc_1st_if(s, uc) + "s"  // ← Adds -s to "walked" → "walkeds" ✗
    }
}
```

**After** (with tense guard):
```rust
"he" | "she" | "it" => {
    use crate::language::verb::detect_tense;
    
    if let Ok(mut val) = IrregularPluralVerb::from_str(s).map(|e| e as usize) {
        // irregular verb lookup...
    } else if detect_tense(s) != crate::language::verb::Tense::Present {
        // Already a past/continuous form; return as-is
        uc_1st_if(s, uc) + ext
    } else if s.ends_with(['s', 'o', 'x']) || s.ends_with("ch") || s.ends_with("sh") {
        uc_1st_if(s, uc) + "es"
    } else if let Some(p) = s.strip_suffix('y').filter(...) {
        uc_1st_if(p, uc) + "ies"
    } else {
        uc_1st_if(s, uc) + "s"
    }
}
```

### Tests (Stage 1)

**Unit tests** in `src/language/verb.rs`:
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn detect_past_regular() {
        assert_eq!(detect_tense("walked"), Tense::Past);
        assert_eq!(detect_tense("Walked"), Tense::Past);
        assert_eq!(detect_tense("talked"), Tense::Past);
    }
    
    #[test]
    fn detect_past_irregular() {
        for (_, past) in IRREGULAR_PAST {
            assert_eq!(detect_tense(past), Tense::Past);
        }
    }
    
    #[test]
    fn detect_continuous() {
        assert_eq!(detect_tense("walking"), Tense::Continuous);
        assert_eq!(detect_tense("Running"), Tense::Continuous);
    }
    
    #[test]
    fn detect_present() {
        assert_eq!(detect_tense("walk"), Tense::Present);
        assert_eq!(detect_tense("run"), Tense::Present);
        assert_eq!(detect_tense("is"), Tense::Present);
    }
}
```

**Integration tests** in `tests/ranting/verb_tense.rs`:
```rust
use ranting::*;
use ranting_derive::say;

#[test]
fn test_past_verb_no_spurious_suffix() {
    let test_cases = vec![
        ("walked", "walked", "walked", "walked"),
        ("went", "went", "went", "went"),
        ("talked", "talked", "talked", "talked"),
    ];
    
    for (verb, i_expect, you_expect, they_expect) in test_cases {
        let i_subj = Noun::new("", "I");
        let you_subj = Noun::new("", "you");
        let they_subj = Noun::new("", "they");
        
        let i_result = say!("{=0 {}}", i_subj, verb);
        let you_result = say!("{=0 {}}", you_subj, verb);
        let they_result = say!("{=0 {}}", they_subj, verb);
        
        assert_eq!(i_result, format!("I {}", i_expect));
        assert_eq!(you_result, format!("You {}", you_expect));
        assert_eq!(they_result, format!("They {}", they_expect));
    }
}

#[test]
fn test_he_walked_not_walkeds() {
    let he = Noun::new("", "he");
    let result = say!("{=0 walked}", he);
    assert_eq!(result, "He walked");  // ✓ No "walkeds"
}
```

### Known Limitation

This skeleton **does not** auto-conjugate past tense from a base verb. `{=person do}` still produces present-tense `"does"` for 3rd person; to get past tense, users must write `{=person did}`. Full auto-conjugation is deferred to Stage 2.

---

## Stage 2: Tense Markers + Auto-Conjugation (Week 3-4)

### Objective
Add `to_past()`, `to_future()`, `to_continuous()` functions; wire into macro to conjugate verbs at compile time.

**Deliverables**:
- `ranting_derive/src/language/verb.rs`: Compile-time verb conjugation (duplicate of runtime version)
- `ranting_derive/src/lib.rs`: Updated `handle_param()` to detect `<`, `=`, `>` markers and conjugate verbs
- Regex: Updated `PH_EXT` post capture group to allow `[<=>]` markers
- Tests: 10 integration tests for tense markers (all pronouns, regular/irregular verbs)

**Limitation**: This stage produces `"He walks"` for `>walk` and `"She running"` for `=run`. Full correctness deferred to Stage 3.

---

## Stage 2.5: Auxiliary Verb Insertion (Week 5-6, Mandatory Fix)

### Objective
Insert correct auxiliary verbs (`will`, `is/are/am`, `was/were`) before conjugated verbs to produce grammatically correct English.

**Problem**: 
- `{=person >walk}` currently produces `"He walks"` instead of `"He will walk"`
- `{=person =run}` currently produces `"She running"` instead of `"She is running"`

### Architecture

**New module**: `src/language/auxiliary.rs`

```rust
pub(crate) enum AuxiliaryVerb {
    Will,      // Future tense
    IsAre,     // Continuous present
    WasWere,   // Continuous past
    Have,      // Perfect tense
    Had,       // Past perfect
}

pub(crate) fn conjugate_auxiliary(aux: AuxiliaryVerb, subject: &str) -> &'static str {
    match (aux, subject.to_lowercase().as_str()) {
        (AuxiliaryVerb::Will, _) => "will",  // Same for all persons
        (AuxiliaryVerb::IsAre, "i") => "am",
        (AuxiliaryVerb::IsAre, "you" | "we" | "they") => "are",
        (AuxiliaryVerb::IsAre, "he" | "she" | "it") => "is",
        (AuxiliaryVerb::WasWere, "i" | "he" | "she" | "it") => "was",
        (AuxiliaryVerb::WasWere, "you" | "we" | "they") => "were",
        (AuxiliaryVerb::Have, "he" | "she" | "it") => "has",
        (AuxiliaryVerb::Have, _) => "have",
        (AuxiliaryVerb::Had, _) => "had",
    }
}
```

### Macro Wiring

In `ranting_derive/src/lib.rs`, after conjugating the verb:
- If `>` marker: inject `"{subject} will "` before the base verb
- If `=` marker: inject `"{subject} is/are/am "` before the `-ing` form
- If `<` + continuous past: inject `"{subject} was/were "` before the `-ing` form

### Tests

**New integration tests** in `tests/ranting/auxiliary_insertion.rs`:
```rust
#[test]
fn future_tense_with_auxiliary() {
    let he = Noun::new("Alex", "he");
    assert_eq!(say!("{=0 >walk}", he), "He will walk");  // Was: "He walks"
    assert_eq!(say!("{=0 >go}", he), "He will go");
}

#[test]
fn continuous_present_with_auxiliary() {
    let she = Noun::new("Alex", "she");
    assert_eq!(say!("{=0 =run}", she), "She is running");  // Was: "She running"
    assert_eq!(say!("{=0 =walk}", she), "She is walking");
}

#[test]
fn continuous_past_with_auxiliary() {
    let they = Noun::new("", "they");
    assert_eq!(say!("{=0 <wait}", they), "They were waiting");  // Continuous past
}
```

---

## Stage 3: Explicit Tense/Case State on Noun (Week 7-8)

### Objective
Allow storing explicit default tense and grammatical case on `Noun` structs, so placeholders inherit them without explicit markers.

**Problem**: Users shouldn't need `{=person <walk}` if `person` already knows it uses past tense everywhere. The stored state should be a fallback when no explicit marker is given.

### Architecture

**New fields on `Noun` struct** (in `src/lib.rs`):

```rust
pub struct Noun {
    name: String,
    subject: String,
    // New optional fields:
    default_tense: Option<Tense>,      // Past, Present, Future, Continuous
    default_case: Option<GrammaticalCase>,  // Nominative, Accusative, Genitive, etc.
}

pub enum Tense {
    Present,
    Past,
    Continuous,
    Future,
}

pub enum GrammaticalCase {
    Nominative,   // Subject form (I, he, they)
    Accusative,   // Object form (me, him, them)
    Genitive,     // Possessive (my, his, their)
    Dative,       // Indirect object (not deeply used in English)
}
```

**Builder methods**:
```rust
impl Noun {
    pub fn with_tense(mut self, tense: Tense) -> Self {
        self.default_tense = Some(tense);
        self
    }
    
    pub fn with_case(mut self, case: GrammaticalCase) -> Self {
        self.default_case = Some(case);
        self
    }
}
```

### Placeholder Fallback Logic

In `ranting_derive/src/lib.rs`, `handle_param()`:
- If no explicit marker (`<`, `=`, `>`) is found in the placeholder and `Noun.default_tense` is set, use that tense
- If no explicit case marker (`=`, `@`, `` ` ``) is found and `Noun.default_case` is set, use that case

### Tests

**Integration tests** in `tests/ranting/noun_explicit_state.rs`:
```rust
#[test]
fn noun_with_default_past_tense() {
    let person = Noun::new("Alex", "she").with_tense(Tense::Past);
    assert_eq!(say!("{=0 walk}", person), "She walked");  // No marker, uses default
}

#[test]
fn noun_explicit_marker_overrides_default() {
    let person = Noun::new("Alex", "she").with_tense(Tense::Past);
    assert_eq!(say!("{=0 =walk}", person), "She is walking");  // Marker overrides default
}

#[test]
fn noun_with_default_accusative_case() {
    let person = Noun::new("Alex", "she").with_case(GrammaticalCase::Accusative);
    assert_eq!(say!("I saw {@0}", person), "I saw her");  // Default accusative
}
```

---

## Stage 4: Inflections Crate Integration (Week 9-10, Future)

### Objective
Add optional noun pluralization support via the `inflections` crate.

**Implementation outline** (from prior research):
- Cargo.toml: Add `inflections = { version = "0.14", optional = true }`
- Feature flag: `smart-plurals = ["inflections"]`
- `src/language/english.rs`: Add `pluralize_noun()` / `singularize_noun()` functions using inflections API (when feature enabled)
- Update `adapt_article()` to use smart pluralization if available

---

## Stage 5: Reflexive Forms (Week 11-12, Future)

### Objective
Add support for myself, yourself, himself, herself, itself, ourselves, yourselves, themselves.

**New module**: `src/language/reflexive.rs`

```rust
pub(crate) static REFLEXIVE_PRONOUNS: &[&str; 9] = &[
    "myself", "yourself", "thyself", "himself", "herself",
    "itself", "ourselves", "yourselves", "themselves",
];

pub(crate) fn reflexive_form(subject: &str) -> &'static str {
    use std::str::FromStr;
    use super::english_shared::SubjectPronoun;
    
    let pronoun = SubjectPronoun::from_str(subject).unwrap_or(SubjectPronoun::They);
    REFLEXIVE_PRONOUNS[pronoun as usize]
}
```

**Wiring**: `handle_placeholder()` in `src/lib.rs` gains a new case marker (e.g. `*` or introduce `%`) for reflexive forms.

---

## Stage 6: Comparative & Superlative Adjectives (Week 13-14, Future)

### Objective
Support degree markers for adjectives: good→better→best, bad→worse→worst.

**New module**: `src/language/adjective_degree.rs`

```rust
pub(crate) static IRREGULAR_ADJECTIVES: &[(&str, &str, &str)] = &[
    ("good", "better", "best"),
    ("bad", "worse", "worst"),
    ("much", "more", "most"),
    ("little", "less", "least"),
    // ... ~20 irregular entries
];

pub(crate) fn get_comparative(adj: &str) -> String {
    if let Some((_, comp, _)) = IRREGULAR_ADJECTIVES.iter().find(|(base, ..)| adj == *base) {
        comp.to_string()
    } else {
        format!("more {}", adj)
    }
}

pub(crate) fn get_superlative(adj: &str) -> String {
    if let Some((_, _, sup)) = IRREGULAR_ADJECTIVES.iter().find(|(base, ..)| adj == *base) {
        sup.to_string()
    } else {
        format!("most {}", adj)
    }
}
```

---

## Stage 7: Testing & Coverage (Week 15)

### Coverage Targets
- Verb tense classification: 95%
- Adjective degrees: 90%
- Reflexive forms: 100%
- Pluralization integration: 85%
- **Overall: >85%**

### Test Files to Add

1. **`tests/ranting/verb_tense.rs`** (30+ tests)
   - Past/continuous forms across all pronouns
   - Irregular verb lookups

2. **`tests/ranting/reflexive_pronouns.rs`** (15 tests)
   - All 9 pronouns to their reflexive forms
   - Capitalization variants

3. **`tests/ranting/adjective_degrees.rs`** (20 tests)
   - Irregular adjectives
   - Regular adjective formation

4. **`tests/ranting/smart_plurals_integration.rs`** (10 tests, feature-gated)
   - Pluralization with inflections crate

---

## Risk Mitigation

| Risk | Mitigation |
|------|-----------|
| Irregular verb table incompleteness | Start with ~20 common forms; community contributions welcome |
| Index-coupled arithmetic corruption | Keep new tense tables structurally separate from `IrregularPluralVerb`/`IRREGULAR_VERBS_1ST`/`IRREGULAR_VERBS_3RD` enum |
| Compile-time complexity | Keep tense parsing simple; defer auto-conjugation logic to Stage 2 |
| Feature flag interaction | Test all combinations: inflections + smart-plurals + inclusive-pronouns |
| Two-file sync burden (`english_shared.rs`) | New verb/reflexive/adjective data lives in `src/language/` only (not proc-macro crate); no sync needed |

---

## Success Criteria

✅ `"He walked"` (not `"He walkeds"`)  
✅ All irregular verbs in skeleton table classified correctly  
✅ Continuous forms (`-ing`) detected and preserved  
✅ >85% code coverage maintained  
✅ No performance regression (vs Phase 1)  
✅ All new tests pass at 100%  
✅ `cargo clippy` clean  

---

## Architecture Notes

**Why free functions instead of trait methods?**

1. **Consistency**: Existing inflection functions (`inflect_verb`, `inflect_possesive`, `inflect_objective`, `inflect_adjective`) are free functions in `src/language/english.rs`, not trait methods.
2. **`dyn Ranting` safety**: The trait is used as `dyn Ranting` with generated delegating impls (`ranting_derive`'s `boxed_ranting_trait`/`ref_ranting_trait`) that only forward the 5 existing methods. New trait methods would either break existing code (required) or silently not forward (default), creating a correctness trap.
3. **Data independence**: Tense/reflexive/comparative data is derived from the verb/pronoun string, not the noun struct — free functions take exactly the parameters they need.

**Why not duplicate into `ranting_derive/src/language/`?**

The two-file sync burden (CLAUDE.md requirement) applies only to the placeholder-parsing regex (`PH_EXT`) and shared enums like `SubjectPronoun`. Verb tense, reflexive, and adjective-degree data are pure runtime logic with no macro-compile-time dependencies — keeping them in `src/language/` only eliminates sync work.

**Why not extend `IrregularPluralVerb`?**

`IrregularPluralVerb` uses index-coupled arithmetic (`e as usize` into `IRREGULAR_VERBS_1ST`/`IRREGULAR_VERBS_3RD` parallel arrays). Inserting or reordering variants silently corrupts the `am/is/was/has/does` mapping. New tense tables must be structurally independent to avoid this trap.

---

## Known Behavior Notes

The `upper()` test in `src/language/english.rs` (lines 265-278) currently asserts literal `"theirself"` for subject `"they"` — a byte-artifact of possessive-pronoun + literal `"self"` string concatenation in the test's format string:

```rust
say!("{=?w'd} say for {`w}self!")
//                         ^^^^^^^ possessive "their" + literal "self" = "theirself"
```

This is **not** real reflexive handling (correct output is `"themselves"`). Stage 4 (reflexive-pronoun table) will change this test's expected output and fix the behavior. Recording this now prevents it from being mistaken for a regression later.

---

## Build Sequence

1. **Week 1-2** (NOW): Verb tense skeleton — detect past/continuous, fix `"walkeds"` bug, add unit + integration tests
2. **Week 3-4**: Tense markers + auto-conjugation — `<`, `=`, `>` markers conjugate verbs (incomplete but testable)
3. **Week 5-6** (MANDATORY): Auxiliary verb insertion — Fix `"He walks"` → `"He will walk"` and `"She running"` → `"She is running"`
4. **Week 7-8**: Explicit tense/case state on Noun — Store default tense/case on struct, fallback in placeholders
5. **Week 9-10**: Inflections crate integration
6. **Week 11-12**: Reflexive forms
7. **Week 13-14**: Adjective degrees
8. **Week 15**: Testing, documentation, polish

---

## Estimated Effort

- **Stage 1 (Skeleton, Week 1-2)**: ~100 lines core code, ~150 lines tests
- **Stage 2 (Tense markers, Week 3-4)**: ~300 lines core code (proc-macro + verb.rs), ~200 lines tests
- **Stage 2.5 (Auxiliaries, Week 5-6)**: ~200 lines core code (auxiliary.rs + macro wiring), ~250 lines tests
- **Stage 3 (Explicit state, Week 7-8)**: ~150 lines (Noun builder methods + placeholder logic), ~200 lines tests
- **Full Phase 2**: ~1000-1200 lines core code, ~3000-3500 lines tests, ~600 lines documentation

---

## Decision Record: Verb Tense & Inflection Data Sourcing

**Date**: 2026-08-12  
**Scope**: Phase 2 architecture and dependency strategy

### Decisions

1. **`inflections` crate**: Use as optional, feature-gated dependency (later stage, not skeleton) for **noun pluralization only**. It doesn't cover verb tenses, reflexive forms, or adjective degrees.

2. **Verb tense, reflexive forms, comparative/superlative**: Build entirely in-house as **free functions** in `src/language/`, not as `Ranting` trait methods, consistent with existing `inflect_verb`/`inflect_possesive` architecture.

3. **Data location**: New tense/reflexive/adjective data lives in `src/language/` (runtime crate only) — deliberately **not** duplicated into `ranting_derive/src/language/`, since it isn't part of the placeholder-parsing regex/enum surface CLAUDE.md requires kept in sync across both copies.

4. **Structural isolation**: New verb-tense tables are separate from `IrregularPluralVerb`/`IRREGULAR_VERBS_1ST`/`IRREGULAR_VERBS_3RD`, which use fragile index-coupled arithmetic. Do not extend those structures for tense data.

5. **Placeholder syntax**: Prefer `{=person walked}` / `{=person went}` (write the already-inflected verb; let `inflect_verb` classify and passthrough) over introducing new `{=person @past do}` marker syntax or a `to_past()` trait method. This requires zero regex/macro changes and works via the existing generic `post` capture group.

### Alternatives Considered

- **Build everything in-house (Phase 1 approach)**: Higher maintenance burden, duplicates work that `inflections` already solves well.
- **New `Ranting` trait methods**: Architecturally mismatched; creates `dyn Ranting` delegating-impl correctness trap.
- **Duplicate inflection data into `ranting_derive`**: Adds sync burden for no macro-compile-time benefit.
- **Extend `IrregularPluralVerb` enum for tense data**: Silent corruption risk due to index-coupled arithmetic.

### Rationale

- No mature Rust crate covers verb conjugation, reflexive forms, or adjective degrees; in-house build is necessary.
- Free functions fit the existing architecture perfectly and avoid the trait-method trap.
- Keeping new data in runtime-only modules avoids the sync burden and lets `ranting_derive` focus on parsing/macro generation.
- Structural isolation of tense tables prevents accidental corruption of existing irregular-verb handling.

---

## Post-Phase-2 Directions

- **Phase 3**: Recursive type inflection, format specs, i18n groundwork
- **v1.0+**: `ranting-i18n` crate for multi-language support

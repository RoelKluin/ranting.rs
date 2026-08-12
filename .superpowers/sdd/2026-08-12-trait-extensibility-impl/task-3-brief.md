# Task 3: Write Integration Tests for Custom Inflection

**Goal:** Create 9 comprehensive integration tests demonstrating custom verb, pronoun, and article inflection with full/partial customization and fallback behavior.

**Files:**
- Create: `tests/ranting/custom_inflection.rs`
- Modify: `tests/ranting/main.rs` (add module declaration)

**Interfaces:**
- Consumes:
  - `Ranting` trait with three new custom methods (from Task 1)
  - `PronounCase` enum (from Task 1)
  - `#[derive_ranting]` macro
  - `say!()` macro
  - Custom hooks in `handle_placeholder()` (from Task 2)
- Produces: 9 passing integration tests demonstrating full/partial customization and fallback

## Test Suite Design

### Test 1: Custom Verb (Pirate) — `test_custom_verb_pirate`

```rust
#[derive_ranting]
#[ranting(subject = "ye", name = "pirate")]
struct PirateNoun;

impl Ranting for PirateNoun {
    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            "have" | "has" => Some(uc_1st_if("have", uc)),
            "do" | "does" => Some(uc_1st_if("do", uc)),
            _ => None,
        }
    }
}

#[test]
fn test_custom_verb_pirate() {
    let pirate = PirateNoun;
    let result = say!("{=0 be} a scallywag.", pirate);
    assert_eq!(result, "You be a scallywag.".to_string());
}
```

### Test 2: Partial Verb Customization — `test_custom_verb_partial`

```rust
#[test]
fn test_custom_verb_partial() {
    let pirate = PirateNoun;
    let result = say!("{=0 be} {=0 have} treasure.", pirate);
    // "be" and "have" are customized, both use pirate forms
    assert_eq!(result, "You be you have treasure.".to_string());
}
```

### Test 3: Verb Fallback (None) — `test_custom_verb_fallback`

```rust
#[test]
fn test_custom_verb_fallback() {
    let pirate = PirateNoun;
    let result = say!("{=0 walk} forward.", pirate);
    // "walk" is not customized, should use English inflection
    assert_eq!(result, "You walk forward.".to_string());
}
```

### Test 4: Custom Pronoun (Formal) — `test_custom_pronoun_formal`

```rust
#[derive_ranting]
#[ranting(subject = "you", name = "dignitary")]
struct Dignitary;

impl Ranting for Dignitary {
    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if subject == "you" && case == PronounCase::Objective {
            return Some(uc_1st_if("your majesty", uc));
        }
        None
    }
}

#[test]
fn test_custom_pronoun_formal() {
    let dignitary = Dignitary;
    let result = say!("I see {@0}.", dignitary);
    assert_eq!(result, "I see your majesty.".to_string());
}
```

### Test 5: Pronoun Case Routing — `test_custom_pronoun_case_routing`

```rust
#[test]
fn test_custom_pronoun_case_routing() {
    let dignitary = Dignitary;
    
    // Objective case should use custom form
    let result = say!("I see {@0}.", dignitary);
    assert_eq!(result, "I see your majesty.".to_string());
    
    // Subjective case should fall back to English
    let result = say!("{=0 are} here.", dignitary);
    assert_eq!(result, "You are here.".to_string());
}
```

### Test 6: Custom Article (Gendered) — `test_custom_article_gendered`

```rust
#[derive_ranting]
#[ranting(subject = "it", name = "cosa")]  // Spanish: feminine "cosa"
struct SpanishFeminine;

impl Ranting for SpanishFeminine {
    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if article == "the" {
            let form = if as_plural { "las" } else { "la" };
            return Some(uc_1st_if(form, uc));
        }
        None
    }
}

#[test]
fn test_custom_article_gendered() {
    let cosa = SpanishFeminine;
    let result = say!("{the 0}", cosa);
    assert_eq!(result, "la cosa".to_string());
    
    let result = say!("{the +0}", cosa);
    assert_eq!(result, "las cosas".to_string());
}
```

### Test 7: Article Fallback — `test_custom_article_fallback`

```rust
#[test]
fn test_custom_article_fallback() {
    let cosa = SpanishFeminine;
    // "a" is not customized, should use English a/an logic
    let result = say!("{a 0}", cosa);
    assert_eq!(result, "A cosa".to_string());
}
```

### Test 8: Combined Customization (Verb + Pronoun) — `test_custom_combined_verb_pronoun`

```rust
#[derive_ranting]
#[ranting(subject = "he", name = "highlander")]
struct ScottishHighlander;

impl Ranting for ScottishHighlander {
    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            _ => None,
        }
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if subject == "he" && case == PronounCase::Subjective {
            return Some(uc_1st_if("he lad", uc));
        }
        None
    }
}

#[test]
fn test_custom_combined_verb_pronoun() {
    let highlander = ScottishHighlander;
    let result = say!("{=0 be} brave.", highlander);
    assert_eq!(result, "He lad be brave.".to_string());
}
```

### Test 9: Zero Customization (All Fallback) — `test_zero_customization`

```rust
#[derive_ranting]
#[ranting(subject = "it", name = "thing")]
struct PlainNoun;

impl Ranting for PlainNoun {
    // All methods return None — test that fallback works
}

#[test]
fn test_zero_customization() {
    let thing = PlainNoun;
    // All inflections should use English defaults
    let result = say!("{the 0 be} red.", thing);
    assert_eq!(result, "The thing is red.".to_string());
}
```

## Implementation Steps

### Step 1: Create test file

Create `tests/ranting/custom_inflection.rs` with all 9 tests as defined above.

### Step 2: Register module in main.rs

Add to `tests/ranting/main.rs` (alphabetically with other module declarations):

```rust
mod custom_inflection;
```

### Step 3: Run tests to verify all pass

```bash
cargo test --test main custom_inflection --verbose
```

Expected: All 9 tests pass

### Step 4: Run full test suite to verify no regressions

```bash
cargo test --all
```

Expected: All tests pass (217 existing + 9 new = 226 total)

### Step 5: Commit

```bash
git add tests/ranting/custom_inflection.rs tests/ranting/main.rs
git commit -m "test: add 9 integration tests for trait-based inflection customization"
```

## Self-Review Checklist

- [ ] All 9 tests created with full implementations (no placeholder structs)
- [ ] Each test has a unique `#[derive_ranting]` struct (PirateNoun, Dignitary, SpanishFeminine, ScottishHighlander, PlainNoun)
- [ ] Custom implementations use exact patterns from examples (match statements, None fallbacks)
- [ ] Each test has an `assert_eq!()` with expected output
- [ ] Module registered in `tests/ranting/main.rs`
- [ ] `cargo test --test main custom_inflection --verbose` passes all 9 tests
- [ ] `cargo test --all` passes with no regressions (226+ total tests)
- [ ] Commit created with message: "test: add 9 integration tests for trait-based inflection customization"

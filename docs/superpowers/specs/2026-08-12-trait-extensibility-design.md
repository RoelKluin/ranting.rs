# Trait-Based Inflection Extensibility (v1.1.0 Priority 2)

**Date:** 2026-08-12  
**Feature:** Custom grammar rule overrides via Ranting trait methods  
**Scope:** Verbs, pronouns, and articles  
**Time Budget:** 16-20 hours  
**Status:** Design approved, ready for implementation

---

## Overview

Enable users to customize verb conjugation, pronoun forms, and article selection by overriding trait methods on `Ranting`. This unlocks ecosystem forks (ranting-spanish, ranting-pirate, ranting-elvish, etc.) while maintaining zero breaking changes and minimal macro complexity.

### Key Design Principle

**Customization via trait defaults, not framework magic.** Users implement `Ranting` and override only the methods they need. Fallback to English rules happens automatically when custom method returns `None`.

---

## Architecture

### 1. No Macro Changes

The `say!()` macro is unchanged. It continues to:
1. Parse placeholders at compile-time
2. Generate `format!()` calls
3. Call `handle_placeholder()` at runtime with a `Ranting` object

**All extensibility is runtime-only**, via trait method dispatch.

### 2. Runtime Flow (Modified)

Current flow:
```
say!() macro
  → format!() call
    → handle_placeholder(noun, ...)
      → inflect_verb() [English module]
      → inflect_subjective() [English module]
      → adapt_article() [English module]
```

New flow:
```
say!() macro [UNCHANGED]
  → format!() call [UNCHANGED]
    → handle_placeholder(noun, ...) [MODIFIED]
      → Try noun.inflect_verb_custom() → if Some, use it; else
        → inflect_verb() [English fallback]
      → Try noun.inflect_pronoun_custom() → if Some, use it; else
        → inflect_subjective/objective/etc. [English fallback]
      → Try noun.inflect_article_custom() → if Some, use it; else
        → adapt_article() [English fallback]
```

This pattern—try custom, fall back to English—lets users customize incrementally.

---

## Trait Extension

### New Methods on `Ranting`

Add three new methods to the `Ranting` trait in `src/lib.rs`, each with a default `None` implementation:

#### 1. `inflect_verb_custom()`

```rust
fn inflect_verb_custom(
    &self,
    subject: &str,
    verb: &str,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    None
}
```

**Purpose:** Override verb conjugation (tense, plurality, person).

**Parameters:**
- `subject`: Subject pronoun (e.g., "I", "he", "they")
- `verb`: The verb to conjugate (e.g., "be", "walk", "have")
- `as_plural`: Whether to conjugate as plural
- `uc`: Whether to uppercase the first character

**Return:**
- `Some(String)`: Use this conjugated form
- `None`: Fall back to English `inflect_verb()`

**Example (Pirate):**
```rust
fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool, uc: bool) -> Option<String> {
    match verb {
        "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
        "have" | "has" => Some(uc_1st_if("have", uc)),
        _ => None,  // use English for other verbs
    }
}
```

#### 2. `inflect_pronoun_custom()`

```rust
fn inflect_pronoun_custom(
    &self,
    subject: &str,
    case: PronounCase,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    None
}
```

**Purpose:** Override pronoun inflection (subject/object/possessive).

**Parameters:**
- `subject`: Subject pronoun (e.g., "I", "he", "they")
- `case`: Which form of the pronoun (see `PronounCase` enum below)
- `as_plural`: Whether to pluralize
- `uc`: Whether to uppercase

**Return:**
- `Some(String)`: Use this pronoun form
- `None`: Fall back to English `inflect_subjective()`, `inflect_objective()`, etc.

**Example (Formal Register):**
```rust
fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase, as_plural: bool, uc: bool) -> Option<String> {
    // Use formal "vous" for "you" in French
    if subject == "you" && case == PronounCase::Subjective {
        return Some(uc_1st_if("vous", uc));
    }
    None  // use English for others
}
```

#### 3. `inflect_article_custom()`

```rust
fn inflect_article_custom(
    &self,
    article: &str,
    noun_singular: &str,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    None
}
```

**Purpose:** Override article selection and forms (a/an/the/some).

**Parameters:**
- `article`: The requested article (e.g., "a", "the", "some")
- `noun_singular`: The singular form of the noun (used for vowel detection in a/an)
- `as_plural`: Whether the noun is plural
- `uc`: Whether to uppercase

**Return:**
- `Some(String)`: Use this article form
- `None`: Fall back to English `adapt_article()`

**Example (Spanish with Gendered Articles):**
```rust
fn inflect_article_custom(&self, article: &str, noun_singular: &str, as_plural: bool, uc: bool) -> Option<String> {
    match article {
        "the" => {
            let form = if self.name(false).ends_with('a') {
                if as_plural { "las" } else { "la" }
            } else {
                if as_plural { "los" } else { "el" }
            };
            Some(uc_1st_if(form, uc))
        }
        _ => None,  // fall back for a/an/some
    }
}
```

### Supporting Type: `PronounCase` Enum

```rust
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PronounCase {
    /// Subject pronouns: I, you, he, she, it, we, they
    Subjective,
    /// Object pronouns: me, you, him, her, it, us, them
    Objective,
    /// Possessive determiners: my, your, his, her, its, our, their
    PossessiveDeterminer,
    /// Possessive pronouns: mine, yours, his, hers, its, ours, theirs
    PossessivePronoun,
}
```

**Placement:** `src/lib.rs`, public export.

---

## Runtime Integration

### Modified `handle_placeholder()` Function

In `src/lib.rs`, update `handle_placeholder()` to check custom methods before calling English module functions.

#### For Verbs (around line 218)

**Current code:**
```rust
let verb = inflect_verb(subjective, p.as_str(), as_pl, uc);
res.push_str(&verb);
```

**New code:**
```rust
let verb = if let Some(custom) = noun.inflect_verb_custom(subjective, p.as_str(), as_pl, uc) {
    custom
} else {
    inflect_verb(subjective, p.as_str(), as_pl, uc)
};
res.push_str(&verb);
```

#### For Pronouns (lines 247-250)

**Current code:**
```rust
let s = match case {
    "=" => inflect_subjective(subjective, as_pl, uc),
    "@" => inflect_objective(subjective, as_pl, uc),
    "`" => inflect_possesive(subjective, as_pl, uc),
    "~" => inflect_adjective(subjective, as_pl, uc),
    _ => noun.inflect(as_pl, uc),
};
res.push_str(&s);
```

**New code:**
```rust
let s = match case {
    "=" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::Subjective, as_pl, uc) {
            custom
        } else {
            inflect_subjective(subjective, as_pl, uc)
        }
    }
    "@" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::Objective, as_pl, uc) {
            custom
        } else {
            inflect_objective(subjective, as_pl, uc)
        }
    }
    "`" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::PossessiveDeterminer, as_pl, uc) {
            custom
        } else {
            inflect_possesive(subjective, as_pl, uc)
        }
    }
    "~" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::PossessivePronoun, as_pl, uc) {
            custom
        } else {
            inflect_adjective(subjective, as_pl, uc)
        }
    }
    _ => noun.inflect(as_pl, uc),
};
res.push_str(&s);
```

#### For Articles (around line 161, in `get_article_or_so()`)

**Current code:**
```rust
match s.trim_start_matches('!') {
    "the" => Some(uc_1st_if(s, uc)),
    "a" | "an" | "some" => {
        let singular = noun.inflect(false, false);
        let a_or_an = uc_1st_if(get_a_or_an(&singular), uc);
        Some(ranting::adapt_article(&a_or_an, s, space, as_pl, uc))
    }
    "these" | "those" => Some(ranting::adapt_article(s, s, space, as_pl, uc)),
    _ => None,
}
```

**New code:**
```rust
let article_form = s.trim_start_matches('!');
match article_form {
    "the" => {
        let singular = noun.inflect(false, false);
        if let Some(custom) = noun.inflect_article_custom("the", &singular, as_pl, uc) {
            Some(custom + space)
        } else {
            Some(uc_1st_if("the", uc) + space)
        }
    }
    "a" | "an" | "some" => {
        let singular = noun.inflect(false, false);
        if let Some(custom) = noun.inflect_article_custom(article_form, &singular, as_pl, uc) {
            Some(custom + space)
        } else {
            let a_or_an = uc_1st_if(get_a_or_an(&singular), uc);
            Some(ranting::adapt_article(&a_or_an, s, space, as_pl, uc))
        }
    }
    "these" | "those" => {
        let singular = noun.inflect(false, false);
        if let Some(custom) = noun.inflect_article_custom(article_form, &singular, as_pl, uc) {
            Some(custom + space)
        } else {
            Some(ranting::adapt_article(s, s, space, as_pl, uc))
        }
    }
    _ => None,
}
```

---

## Fallback Strategy

When a custom method returns `None`:

| Method | Fallback |
|--------|----------|
| `inflect_verb_custom()` | English `inflect_verb()` function |
| `inflect_pronoun_custom()` | English `inflect_subjective()`, `inflect_objective()`, etc. (by `PronounCase`) |
| `inflect_article_custom()` | English `adapt_article()` logic |

This allows **partial customization**: a user can implement `inflect_verb_custom()` but leave pronouns in English, for example.

---

## Example: Pirate Dialect Fork

Full working example showing how an ecosystem fork would use this feature.

```rust
use ranting::*;
use ranting_derive::*;

#[derive_ranting]
#[ranting(subject = "ye", name = "pirate")]
struct PirateNoun;

impl Ranting for PirateNoun {
    // All derived methods (name, subjective, is_plural, inflect, skip_article)
    // are generated by the macro. Below we override the custom methods.

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        // Pirate English: use "be" for all forms of "to be"
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            "have" | "has" => Some(uc_1st_if("have", uc)),
            "do" | "does" => Some(uc_1st_if("do", uc)),
            // Return None for other verbs → fall back to English
            _ => None,
        }
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        _case: PronounCase,
        _as_plural: bool,
        _uc: bool,
    ) -> Option<String> {
        // No custom pronouns; use English
        None
    }

    fn inflect_article_custom(
        &self,
        _article: &str,
        _noun_singular: &str,
        _as_plural: bool,
        _uc: bool,
    ) -> Option<String> {
        // No custom articles; use English
        None
    }
}

// Usage:
fn main() {
    let pirate = PirateNoun;
    
    println!("{}", say!("{=0 be} a scallywag.", pirate));
    // Output: "You be a scallywag."
    
    println!("{}", say!("{the =0} {=0 have} treasure.", pirate));
    // Output: "The pirate You have treasure."
}
```

---

## Files to Modify

### 1. `src/lib.rs` (Main Changes)

**Add:**
- `PronounCase` enum (public, `#[derive(Copy, Clone, Debug, PartialEq, Eq)]`)
- Three new methods to `Ranting` trait with default `None` impls
- Export `PronounCase` and new trait methods in public API

**Modify:**
- `handle_placeholder()` function to check custom methods before falling back to English functions (see Runtime Integration section above)

### 2. `ranting_derive/src/ranting_impl.rs` (Macro Changes)

**No changes required.** The `#[derive_ranting]` macro does not generate implementations for the custom methods; they use the trait's default `None` implementations.

If a user wants to override, they implement the methods manually in their own `impl Ranting for MyType` block.

### 3. New Test File: `tests/ranting/custom_inflection.rs`

Add integration tests demonstrating:
- Custom verb inflection (pirate, archaic English)
- Custom pronoun inflection (formal register, gendered forms)
- Custom article inflection (Spanish gendered articles)
- Partial customization (some methods overridden, others fall back)
- Fallback to English when custom returns `None`
- Round-trip pluralization with custom rules

### 4. Documentation

**New file: `docs/EXTENSIBILITY.md`**

Document:
- Overview of the extension points
- API reference for each custom method
- Pirate dialect example (full, working code)
- Spanish dialect sketch (verbs, pronouns, articles with gender)
- Best practices for ecosystem forks
- Performance notes (custom method dispatch is zero-cost abstraction)

**Update: `docs/TUTORIAL.md` or main `README.md`**

Add brief section mentioning extensibility and link to `EXTENSIBILITY.md`.

### 5. Update `ROADMAP.md`

Mark Priority 2 complete upon implementation:
```
2. **Trait-Based Inflection Extensibility** (✅ Complete — 16-20 hours)
   - ✅ Custom verb inflection via `inflect_verb_custom()`
   - ✅ Custom pronoun inflection via `inflect_pronoun_custom()` with `PronounCase` enum
   - ✅ Custom article inflection via `inflect_article_custom()`
   - ✅ Default `None` implementations (zero breaking changes)
   - ✅ Runtime fallback to English rules
   - ✅ Pirate + ecosystem fork examples documented
   - ✅ 9 integration tests verifying partial customization, fallback behavior
```

---

## Testing Strategy

### Unit Tests (in modified files)

None needed—the new trait methods are defaults (`None`), so they don't need unit testing.

### Integration Tests (`tests/ranting/custom_inflection.rs`)

**9 tests covering:**

1. **`test_custom_verb_pirate`** — Override verb conjugation; verify "be" replaces "is"/"am"/"are"
2. **`test_custom_verb_partial`** — Override only "be", fall back to English for "have"
3. **`test_custom_verb_fallback`** — Return `None`; verify English rules apply
4. **`test_custom_pronoun_formal`** — Custom subject pronouns (e.g., "vous")
5. **`test_custom_pronoun_case_routing`** — Verify each `PronounCase` variant routes correctly
6. **`test_custom_article_gendered`** — Spanish-style gendered articles (la/el/los/las)
7. **`test_custom_article_fallback`** — Return `None` for articles; verify English adapt_article() applies
8. **`test_custom_combined`** — Verb + pronoun + article all customized together
9. **`test_fallback_cascade`** — Mix of custom (`Some`) and fallback (`None`) in same phrase

### Regression Testing

Run full test suite to verify:
- All existing tests pass (zero breaking changes)
- No performance regression in handle_placeholder()
- Plural/singular inflection still works with custom overrides

---

## Success Criteria

✅ **Functional**
- Custom methods callable from `Ranting` impls
- Fallback to English when custom returns `None`
- Partial customization works (some methods overridden, others default)

✅ **Non-Breaking**
- Zero breaking changes; all existing code compiles unchanged
- Default `None` means no customization required
- Existing `Ranting` impls need no modifications

✅ **Documented**
- Pirate dialect example works end-to-end
- Spanish gendered articles example provided (sketch)
- `EXTENSIBILITY.md` documents all three custom methods
- README links to extensibility docs

✅ **Tested**
- 9 integration tests covering all code paths (custom, fallback, partial)
- Full regression test suite passes
- No performance regression

✅ **Production Ready**
- Code follows existing Ranting style
- Trait methods properly documented with examples
- Ready for ecosystem forks

---

## Open Questions / Deferred

1. **Article customization scope:** Should we also allow customizing "these" / "those"? Currently they're demonstratives. **Answer (deferred to v1.2):** Yes, add to article hook if users request it.

2. **Tense markers and custom verbs:** The macro can insert tense markers (e.g., `~TENSE~<:walked`). Should custom `inflect_verb_custom()` be called for these? **Answer:** Yes, same as any verb inflection.

3. **Adjective customization:** Priority 4 (v1.1 spec) is comparative/superlative adjectives. Custom adjective hook can be added then. **Answer:** Deferred to Priority 4.

---

## Glossary

- **Custom method:** A trait method users override in their own `impl Ranting` block (e.g., `inflect_verb_custom`)
- **Fallback:** Using English module functions when custom method returns `None`
- **Partial customization:** Overriding some custom methods but not others (rest default to `None` → English)
- **Ecosystem fork:** A separate crate (e.g., `ranting-spanish`) that uses Ranting trait extensibility for a different language/dialect

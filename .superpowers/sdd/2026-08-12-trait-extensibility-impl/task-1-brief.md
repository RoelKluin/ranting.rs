# Task 1: Add PronounCase Enum and Trait Methods

**Goal:** Add the foundation for trait-based inflection extensibility by introducing a `PronounCase` enum and three new trait methods to the `Ranting` trait.

**Files:**
- Modify: `src/lib.rs:490-509` (Ranting trait definition)
- Modify: `src/lib.rs` (add PronounCase enum before trait)

**Interfaces:**
- Consumes: Current `Ranting` trait signature (5 existing methods)
- Produces: 
  - `PronounCase` enum (public, 4 variants: `Subjective`, `Objective`, `PossessiveDeterminer`, `PossessivePronoun`)
  - Three new trait methods: `inflect_verb_custom()`, `inflect_pronoun_custom()`, `inflect_article_custom()`
  - All methods have default `None` implementations (no-op by default)

## Steps

### Step 1: Add PronounCase enum above Ranting trait

In `src/lib.rs`, before the `pub trait Ranting` definition (around line 489), insert:

```rust
/// Pronoun grammatical case for customization via inflect_pronoun_custom()
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

### Step 2: Add three new methods to Ranting trait

Inside `pub trait Ranting` definition (after `fn skip_article(&self) -> bool;`), add:

```rust
    /// Customize verb conjugation (tense, plurality, person).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool, uc: bool) -> Option<String> {
    ///     match verb {
    ///         "be" => Some("be".to_string()),  // Pirate: use "be" for all forms
    ///         _ => None,  // Fall back to English for other verbs
    ///     }
    /// }
    /// ```
    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        None
    }

    /// Customize pronoun inflection (subject/object/possessive forms).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// # Arguments
    /// * `subject` - Subject pronoun (e.g., "I", "he", "they")
    /// * `case` - Which pronoun form: Subjective, Objective, PossessiveDeterminer, or PossessivePronoun
    /// * `as_plural` - Whether to pluralize
    /// * `uc` - Whether to uppercase first character
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase, as_plural: bool, uc: bool) -> Option<String> {
    ///     if subject == "you" && case == PronounCase::Subjective {
    ///         Some("vous".to_string())  // Formal French: "vous"
    ///     } else {
    ///         None  // Fall back to English
    ///     }
    /// }
    /// ```
    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        None
    }

    /// Customize article inflection (a/an/the/some, demonstratives, etc.).
    /// Return Some(String) to use custom form, None to fall back to English.
    ///
    /// # Arguments
    /// * `article` - Requested article form (e.g., "a", "the", "some", "these", "those")
    /// * `noun_singular` - Singular form of noun (for vowel detection, gender agreement, etc.)
    /// * `as_plural` - Whether the noun is plural
    /// * `uc` - Whether to uppercase first character
    ///
    /// # Examples
    /// ```ignore
    /// fn inflect_article_custom(&self, article: &str, noun_singular: &str, as_plural: bool, uc: bool) -> Option<String> {
    ///     match article {
    ///         "the" => {
    ///             // Spanish gendered articles: la/el/los/las
    ///             let form = if noun_singular.ends_with('a') {
    ///                 if as_plural { "las" } else { "la" }
    ///             } else {
    ///                 if as_plural { "los" } else { "el" }
    ///             };
    ///             Some(uc_1st_if(form, uc))
    ///         }
    ///         _ => None,  // Fall back to English for a/an/some
    ///     }
    /// }
    /// ```
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

### Step 3: Export PronounCase in public API

Ensure `PronounCase` is defined as `pub enum` in `src/lib.rs` (which it is from Step 1). No additional re-export needed since it's defined directly in the main crate.

### Step 4: Verify trait compiles

```bash
cargo check
```

Expected: No errors. The trait methods are defined with default implementations.

### Step 5: Run existing tests to ensure no regression

```bash
cargo test --all
```

Expected: All existing tests pass (no changes to trait implementation yet, just new methods with `None` defaults).

### Step 6: Commit

```bash
git add src/lib.rs
git commit -m "feat: add PronounCase enum and trait methods (trait-based extensibility foundation)"
```

## Self-Review Checklist

- [ ] PronounCase enum added with all 4 variants before Ranting trait
- [ ] All three trait methods added with `None` default implementations
- [ ] Trait methods are correctly formatted (proper parameter names, documentation)
- [ ] `cargo check` passes
- [ ] `cargo test --all` passes with no regressions
- [ ] Commit created with appropriate message

# Trait-Based Inflection Extensibility

Ranting v1.1 enables custom grammar rules via trait method overrides, allowing ecosystem forks (ranting-spanish, ranting-pirate, ranting-elvish) to implement dialect-specific inflection rules. By implementing trait methods on your custom noun types, you can extend Ranting to support any language, dialect, or specialized grammar system.

## Quick Start

Here's a simple example: a Pirate dialect that uses "be" for all forms of the verb "to be":

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct PirateNoun;

impl fmt::Display for PirateNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pirate")
    }
}

impl Ranting for PirateNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("pirate", uc)
    }

    fn subjective(&self) -> &str {
        "ye"
    }

    fn is_plural(&self) -> bool {
        true
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("pirates", uc)
        } else {
            uc_1st_if("pirate", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

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
}

// Usage:
fn main() {
    let pirate = PirateNoun;
    assert_eq!(
        say!("{=0 be} fearless!", pirate),
        "Ye be fearless!".to_string()
    );
}
```

## Extension Points (API Reference)

### 2.0 Story-Wide Context: the `_with_context` Variants (v1.1)

Each of the three hooks below (`inflect_verb_custom`, `inflect_pronoun_custom`,
`inflect_article_custom`) has a `_with_context` counterpart —
`inflect_verb_custom_with_context`, `inflect_pronoun_custom_with_context`,
`inflect_article_custom_with_context` — that takes one extra parameter,
`ctx: Option<&NarrationContext>`, and is what every call site in the crate
actually invokes. The default implementation of each `_with_context` method
ignores `ctx` and delegates to the plain hook, so everything above still
works unchanged — override the plain hook if you don't need story-wide
context, and only reach for the `_with_context` variant when you do.

`say!()` calls the `_with_context` hooks with `ctx: None`. `say_with!(context, ...)`
calls them with `ctx: Some(&context)`, where `context: NarrationContext` carries
`tense`/`narration_person` (resolved by the crate itself, see the runtime tense/viewpoint
sections above) plus `register: Option<Register>` (`Formal`/`Neutral`/`Casual`) and
`dialect: Option<&'static str>`, which the crate never interprets — they exist purely
for your hook to branch on:

```rust
fn inflect_verb_custom_with_context(
    &self,
    subject: &str,
    verb: &str,
    as_plural: bool,
    uc: bool,
    ctx: Option<&NarrationContext>,
) -> Option<String> {
    match (verb, ctx.and_then(|c| c.register)) {
        ("greet", Some(Register::Formal)) => Some(uc_1st_if("bows before", uc)),
        _ => self.inflect_verb_custom(subject, verb, as_plural, uc),
    }
}
```

`ctx` always arrives as a parameter, never read off `self` — an entity's own `subject`
stays a property of the entity, while `register`/`dialect`/`narration_person` are
story-wide settings that can differ per `say_with!()` call for the same noun.

### 2.1 Verb Inflection: `inflect_verb_custom()`

Customize verb conjugation for any tense, plurality, or person.

**Signature:**
```rust
fn inflect_verb_custom(
    &self,
    subject: &str,
    verb: &str,
    as_plural: bool,
    uc: bool,
) -> Option<String>
```

**Parameters:**
- `subject` (&str): The subject pronoun (e.g., "I", "you", "he", "she", "it", "we", "they")
- `verb` (&str): The verb to inflect (e.g., "be", "have", "walk")
- `as_plural` (bool): Whether to conjugate for plural form
- `uc` (bool): Whether to uppercase the first character (handle contractions with `uc_1st_if`)

**Return Values:**
- `Some(String)`: A custom verb form (used by the macro)
- `None`: Use English default conjugation (no overhead)

**Example: Pirate English**

```rust
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
        _ => None,  // Fall back to English for other verbs
    }
}
```

**Best Practice:** Return `None` for verbs you don't customize. This automatically triggers English fallback with zero overhead.

### 2.2 Pronoun Inflection: `inflect_pronoun_custom()`

Customize pronoun forms (subject, object, possessive).

**Signature:**
```rust
fn inflect_pronoun_custom(
    &self,
    subject: &str,
    case: PronounCase,
    as_plural: bool,
    uc: bool,
) -> Option<String>
```

**Parameters:**
- `subject` (&str): The subject pronoun (e.g., "I", "you", "he", "she", "it", "we", "they")
- `case` (PronounCase): Which pronoun form is requested (see enum below)
- `as_plural` (bool): Whether to pluralize the pronoun
- `uc` (bool): Whether to uppercase the first character

**`PronounCase` Enum:**
```rust
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

**Example: Formal French (vous for plural you)**

```rust
fn inflect_pronoun_custom(
    &self,
    subject: &str,
    case: PronounCase,
    _as_plural: bool,
    uc: bool,
) -> Option<String> {
    if subject == "you" && case == PronounCase::Subjective {
        // Formal French uses "vous" for plural "you"
        return Some(uc_1st_if("vous", uc));
    }
    None  // Fall back to English for other pronouns
}
```

**Best Practice:** Use case routing (match on `PronounCase`) to handle specific pronoun forms independently. This allows you to customize only the forms you need (e.g., subjective only) while falling back to English for others.

### 2.3 Article Inflection: `inflect_article_custom()`

Customize article forms (a/an/the/some/demonstratives).

**Signature:**
```rust
fn inflect_article_custom(
    &self,
    article: &str,
    noun_singular: &str,
    as_plural: bool,
    uc: bool,
) -> Option<String>
```

**Parameters:**
- `article` (&str): The requested article form ("a", "an", "the", "some", "these", "those")
- `noun_singular` (&str): The singular inflected form of the noun (useful for vowel/gender detection)
- `as_plural` (bool): Whether the noun is plural
- `uc` (bool): Whether to uppercase the first character

**Return Values:**
- `Some(String)`: A custom article form (returned with no trailing space; caller adds spacing)
- `None`: Use English default article logic (no overhead)

**Example: Spanish Gendered Articles**

```rust
fn inflect_article_custom(
    &self,
    article: &str,
    noun_singular: &str,
    as_plural: bool,
    uc: bool,
) -> Option<String> {
    match article {
        "the" => {
            // Spanish gendered articles: la/el/los/las based on noun ending
            let form = if noun_singular.ends_with('a') {
                if as_plural { "las" } else { "la" }
            } else {
                if as_plural { "los" } else { "el" }
            };
            Some(uc_1st_if(form, uc))
        }
        _ => None,  // Fall back to English for a/an/some
    }
}
```

**Best Practice:** Examine `noun_singular` for vowel/gender patterns. This parameter is the singularized form of the noun, allowing you to make decisions based on linguistic properties (e.g., French uses "un" for masculine, "une" for feminine).

## Partial Customization

You don't need to implement all three custom methods. If you only need verb customization, implement `inflect_verb_custom()` and leave the other two as default (returning `None`). The trait provides default implementations for all three methods:

```rust
impl Ranting for MyNoun {
    // ... required methods (name, subjective, is_plural, inflect, skip_article) ...

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        // Custom verb logic here
        match verb {
            "be" => Some(uc_1st_if("am", uc)),
            _ => None,
        }
    }

    // inflect_pronoun_custom and inflect_article_custom are not overridden
    // They return None by default, triggering English fallback
}
```

This is perfectly valid. Returning `None` automatically triggers English fallback with zero overhead.

## Full Examples

### 4.1 Pirate Dialect

A complete pirate dialect implementation that customizes verb conjugation:

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct PirateNoun;

impl fmt::Display for PirateNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pirate")
    }
}

impl Ranting for PirateNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("pirate", uc)
    }

    fn subjective(&self) -> &str {
        "ye"
    }

    fn is_plural(&self) -> bool {
        true
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("pirates", uc)
        } else {
            uc_1st_if("pirate", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

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

fn main() {
    let pirate = PirateNoun;
    
    // Usage examples:
    let result1 = say!("{=0 be} fearless!", pirate);
    assert_eq!(result1, "Ye be fearless!".to_string());
    
    let result2 = say!("{=0 have} {the 0}?", pirate);
    assert_eq!(result2, "Ye have the pirate?".to_string());
}
```

**Output:**
- `"{=0 be} fearless!"` → `"Ye be fearless!"`
- `"{=0 have} treasure"` → `"Ye have treasure"`

### 4.2 Scottish Highland English

A dialect combining verb and pronoun customization:

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct ScottishHighlander;

impl fmt::Display for ScottishHighlander {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "highlander")
    }
}

impl Ranting for ScottishHighlander {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("highlander", uc)
    }

    fn subjective(&self) -> &str {
        "he"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("highlanders", uc)
        } else {
            uc_1st_if("highlander", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

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

fn main() {
    let highlander = ScottishHighlander;
    
    // Case routing demonstration:
    let result1 = say!("{=0 be} brave.", highlander);
    // Subjective case uses custom "he lad"
    assert_eq!(result1, "He lad be brave.".to_string());
    
    let result2 = say!("I see {@0}.", highlander);
    // Objective case falls back to English "him"
    assert_eq!(result2, "I see him.".to_string());
}
```

**Output:**
- `"{=0 be} brave"` → `"He lad be brave"` (custom pronoun + custom verb)
- `"I see {@0}"` → `"I see him"` (objective case falls back to English)

### 4.3 Spanish with Gendered Articles and Verbs

A complete Spanish implementation with both article and verb customization:

```rust
use ranting::*;
use std::fmt;

#[derive(Clone, Copy)]
struct SpanishNoun;

impl fmt::Display for SpanishNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cosa")
    }
}

impl Ranting for SpanishNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("cosa", uc)
    }

    fn subjective(&self) -> &str {
        "it"
    }

    fn is_plural(&self) -> bool {
        false
    }

    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        if to_plural {
            uc_1st_if("cosas", uc)
        } else {
            uc_1st_if("cosa", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" => {
                // Spanish "ser": "es" (singular) or "son" (plural)
                let form = if as_plural { "son" } else { "es" };
                Some(uc_1st_if(form, uc))
            }
            _ => None,
        }
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        noun_singular: &str,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if article == "the" {
            // Spanish gendered articles based on noun ending
            let form = if noun_singular.ends_with('a') {
                if as_plural { "las" } else { "la" }
            } else {
                if as_plural { "los" } else { "el" }
            };
            return Some(uc_1st_if(form, uc));
        }
        None
    }
}

fn main() {
    let cosa = SpanishNoun;
    
    // Article and verb customization working together:
    let result1 = say!("{the 0 be} hermosa", cosa);
    // Feminine ending 'a' triggers "la", verb "be" becomes "es"
    assert_eq!(result1, "La cosa es hermosa".to_string());
    
    let result2 = say!("{the +0 be} hermosas", cosa);
    // Plural form triggers "las"
    assert_eq!(result2, "Las cosas son hermosas".to_string());
}
```

**Output:**
- `"{the 0 be} hermosa"` → `"La cosa es hermosa"` (feminine article + Spanish verb)
- `"{the +0 be} hermosas"` → `"Las cosas son hermosas"` (plural feminine + plural verb)

## Best Practices

1. **Partial customization is fine** — Return `None` for any inflection you don't customize. This immediately triggers English default behavior with zero overhead, avoiding code duplication.

2. **Use `uc_1st_if()` for capitalization** — This helper function correctly handles both regular words and contractions (e.g., "'tis" → "'Tis"). Always use it when your custom method needs to apply uppercase logic.

3. **Test your overrides with integration tests** — Create tests similar to `tests/ranting/custom_inflection.rs` that exercise your custom methods with real `say!()` calls. Include edge cases like empty forms, contractions, and plural/singular transitions.

4. **Document your dialect/language in your ecosystem fork's README** — If you create a new crate (e.g., `ranting-french`), include examples of the customizations you've implemented and any cultural/linguistic notes that help users understand the rules.

5. **Keep custom methods fast** — These methods are called for every placeholder that requires inflection. Avoid complex lookups or allocations; prefer simple match statements on the verb/article/case enums and return early for non-matching cases.

## Performance Notes

Custom method dispatch is a **zero-cost abstraction** in Ranting:

- **If your method returns `None`:** The English fallback is used without any function-call overhead beyond the Option check itself.
- **If your method returns `Some(String)`:** You pay the cost of string creation (unavoidable) and returning the custom form.
- **No additional function-call overhead:** Unlike virtual dispatch, the method call is inlined by the compiler, and the Option check is a single branch.

Example: A pirate verb method that matches and returns a custom form costs one match statement and one Option wrap. This is negligible compared to the cost of string handling in the macro itself.

## Contributing Custom Rules

The Ranting ecosystem grows when users and contributors build dialect-specific forks. Here's how to contribute:

### For English Inflection Bugs
Open a GitHub issue at [RoelKluin/ranting.rs](https://github.com/RoelKluin/ranting.rs) with:
- Your test case (a `say!()` expression and expected output)
- The actual output you got
- A description of which English rule is broken

### For New Language Modules
Create a companion crate following the naming convention `ranting-<language>` or `ranting-<dialect>`:

```toml
[package]
name = "ranting-french"
version = "0.1.0"

[dependencies]
ranting = "0.2"
```

In your crate:
1. Define custom noun types with `impl Ranting` blocks for your language
2. Pre-build common patterns (e.g., `FrenchMasculineNoun`, `FrenchFeminineNoun`)
3. Export them from your `lib.rs` for users to import
4. Include examples and documentation in your README

Example use:
```rust
use ranting::say;
use ranting_french::FrenchNoun;

fn main() {
    let word = FrenchNoun::masculine("chat");  // "cat"
    assert_eq!(
        say!("{the 0 be} noir.", word),
        "Le chat est noir.".to_string()
    );
}
```

By creating these forks, you help the Ranting ecosystem support more languages and dialects while keeping the core library lean and focused on English.

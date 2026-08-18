# Getting Started with Ranting

*[🇪🇸 Leer en español](es/TUTORIAL.md)*

A practical guide to building pronoun-aware text generation with the `say!()` macro. Estimated read time: 30-40 minutes.

---

## 1. What is Ranting? Why `say!()` vs `format!()`?

Ranting solves a fundamental problem with Rust's built-in string formatting: **pronouns and verbs must agree with each other, but `format!()` has no notion of grammar.**

Compare:

```rust
// With format!() — the template is hardcoded for one pronoun ("they"), so it
// silently produces wrong grammar for anyone else:
format!("{} do say their name is Jordan.", "he")
// Output: "he do say their name is Jordan." ✗ Wrong verb form and possessive

// With say!() and Ranting — the same template adapts to the noun's pronoun
use ranting::*;
fn say_this(who: Noun, title: &Noun) -> String {
    say!("{=who do} say {`who title are} {who}.")
}
let title = Noun::new("name", "it");
say_this(Noun::new("Jordan", "he"), &title)
// Output: "He does say his name is Jordan." ✓ Correct
say_this(Noun::new("Jordan", "she"), &title)
// Output: "She does say her name is Jordan." ✓ Also correct, no code change
say_this(Noun::new("Jordan", "they"), &title)
// Output: "They do say their name is Jordan." ✓ Singular "they", still correct
```

The `say!()` macro **automatically conjugates verbs and adapts articles based on the pronoun**, so your text is grammatically correct regardless of who it's about.

### Why this matters

- **Inclusive design**: Support all pronouns (he, she, they, I, you, etc.) without conditional branches.
- **Less boilerplate**: No `if subject == "they" { ... } else { ... }` chains.
- **Type-safe**: Pronouns are enforced at compile time via the `Ranting` trait.
- **Domain-specific**: Designed specifically for English grammar rules—not a general-purpose formatter.

### When to use Ranting

- **Game dialogue**: NPC speech, item descriptions, combat feedback.
- **Chatbots**: Responses that adapt to any pronoun/name.
- **User-facing text**: Profile bios, notifications, generated narratives.

### When to skip it

- Template-heavy text (use a template engine instead).
- Non-English languages. `ranting` inflects English by default, but every article, pronoun,
  verb, adjective, numeral, preposition and capitalization decision is overridable through the
  `Ranting` trait's `_custom` hooks — see `docs/EXTENSIBILITY.md`, and `ranting_i18n`/`ranting_es`
  for worked German and Spanish implementations built on the public API alone. Word *order*
  stays in your template: a non-English application supplies one template per language.

---

## 2. Your First `say!()` Macro: Pronouns and Subjects

The core of Ranting is the `Noun` struct, which pairs a name with a pronoun:

```rust
use ranting::*;

let jane = Noun::new("Jane", "I");
let tarzan = Noun::new("Tarzan", "he");
let pat = Noun::new("Pat", "they");
let jeffersons = Noun::new("The Jeffersons", "they");
```

### Placeholder syntax: displaying pronouns

Placeholders in `say!()` use **case markers** to control what form of the pronoun is displayed:

| Marker | Name | Example | Output |
|--------|------|---------|--------|
| `=` | Subject | `{=jane}` | `I` |
| `@` | Object | `{@jane}` | `Me` |
| `` ` `` | Possessive determiner | `{`jane}` | `My` |
| `~` | Possessive pronoun | `{~jane}` | `Mine` |
| `*` | Display name | `{*jeffersons who have}` | `The Jeffersons who have` |
| `*=`, `*@`, `` *` ``, `*~`, `*%` | Fused: case-marks like the bare marker (a custom `inflect_article_custom` sees the real `GrammaticalCase`), but still displays the noun's name instead of switching to a pronoun | `{the *=noun}` | article rendered case-correctly, noun's name shown |

Tested examples from `tests/ranting/tutorial.rs::section_2_first_say_pronouns`:

```rust
say!("{=jane}")        // "I"
say!("{=tarzan}")      // "He"
say!("{=pat}")         // "They"

say!("{`jane}")        // "My"
say!("{`tarzan}")      // "His"
say!("{`pat}")         // "Their"

say!("{*jeffersons who have} a book.")       // "The Jeffersons who have a book."
say!("{=jane}, {*jane}, who have a book.")   // "I, Jane, who have a book."
```

The trailing text after a `*` (or bare) noun still runs through verb conjugation — its first
word is treated as the verb (so `say!("{*tarzan walk}")` gives `"Tarzan walks"`). `who` isn't
special syntax; it only stays inert here because `jeffersons`'s own declared pronoun (`"they"`) is
plural, which doesn't change a following word's spelling. A third-person-singular noun's `who`
would itself get conjugated (wrongly) if placed right after the case marker, which is why the
`{=jane}, {*jane}, who have a book.` example puts `who` in the sentence's own literal text
instead, after two separate placeholders, rather than inside a single noun's verb slot.

### Positional arguments

You can reference arguments by position:

```rust
let noun = Noun::new("Alice", "she");
say!("{=0}", noun)     // "She"
say!("{@0}", noun)     // "her"
```

Or by name:

```rust
say!("{=person}", person = noun)   // "She"
```

---

## 3. Tense Markers: Past, Present, Continuous, and Future

Ranting supports **six distinct verb tenses** via prefix markers in the `{...verb}` post-position:

| Marker | Tense | Example | Output |
|--------|-------|---------|--------|
| (none) | Present | `{=kate walk}` | `She walks` |
| `<` | Past | `{=kate <walk}` | `She walked` |
| `=` | Present continuous | `{=kate =run}` | `She is running` |
| `>` | Future | `{=kate >paint}` | `She will paint` |
| `<=` | Past continuous | `{=kate <=dance}` | `She were dancing` |
| `%` | Present perfect | `{=kate %finish}` | `She has finished` |
| `<%` | Past perfect | `{=kate <%leave}` | `She had left` |

### How tense markers work

- **Compile-time conjugation**: The `ranting_derive` crate conjugates the base verb (e.g., "walk" → "walked", "run" → "running").
- **Runtime auxiliary insertion**: The `ranting` crate inserts the correct auxiliary verb ("is", "have", "had", "will").
- **Automatic agreement**: Verb form + auxiliary always agree with the subject pronoun.

Tested examples from `tests/ranting/tutorial.rs::section_3_*`:

```rust
// Past
say!("{=kate <walk}");  // "She walked"

// Continuous (present or past)
say!("{=luis =run}");   // "He is running"
say!("{=alex <=dance}"); // "They were dancing"

// Future
say!("{=sophia >paint}");  // "She will paint"

// Perfect (present or past)
say!("{=morgan %finish}");  // "She has finished"
say!("{=jordan <%leave}");  // "He had left"
```

### Irregular verbs

Irregular verbs (go → went, see → saw, be → been, etc.) are handled automatically via a built-in table of 118+ irregular forms. No special syntax needed:

```rust
say!("{=hero <go} into dungeon.");  // "He went..." (not "go'd")
```

---

## 4. Common Pitfalls and the Full Placeholder Syntax

### Sentence-start auto-capitalization

Placeholders at the sentence start automatically uppercase their first character:

```rust
say!("{=avery walk} quickly.");       // "She walks quickly." (sentence start)
say!("When {=avery walk}, she..."); // "When she runs, she..." (mid-sentence)
```

### Full placeholder grammar

A complete placeholder can include articles, plurality markers, and case markers:

```
{[,^]?(article)?([+-]|#var)?(\*[`=@~%]|[`=@~?*])?noun( verb)}
```

- **Case modifiers** (optional): `,` (enforce lowercase) or `^` (enforce uppercase).
- **Article** (optional): `a`, `an`, `some`, `the`, `these`, `those`.
- **Plurality** (optional): `+` (force plural), `-` (force singular), `$var`/`#var` (numeric-driven).
- **Case marker** (optional): `` ` `` (possessive determiner), `=` (subject), `@` (object), `~` (possessive pronoun), `*` (display name), `?` (hidden); the fused two-character forms `*=`/`*@`/`` *` ``/`*~`/`*%` case-mark the placeholder like the bare marker while still displaying the noun's name.
- **Noun**: A variable name or positional index.
- **Verb** (optional): A base verb, optionally prefixed with a tense marker (`<`, `=`, `>`, etc.).

### Article adaptation

Articles automatically adapt based on the noun's plurality — `a`/`an` becomes `some`
for a plural subject, while `the` stays `the`:

```rust
let dog = Noun::new("dog", "it");
say!("{a dog}")      // "A dog"
say!("{the dog}")    // "The dog"

let dogs = Noun::new("dog", "they");
say!("{a dogs}")     // "Some dog"
say!("{the dogs}")   // "The dog"
```

### Forcing singular/plural

```rust
let person = Noun::new("person", "it");
say!("{-=0 do}", person)   // "It does"   (forced singular)
say!("{+=0 do}", person)   // "They do"   (forced plural)
```

---

## 5. Debugging with `--features debug`

When developing, you can see how the `say!()` macro is being compiled via a debug feature flag:

```bash
cargo test --features debug
```

This prints the compile-time expansion of each `say!()` placeholder to the format!() call that will be generated. Use this to verify the macro is generating the expected code.

Example (theoretical output):

```
// At compile time:
say!("{=kate walk}");

// Expands to something like:
format!("{}...", handle_placeholder(kate, ...))
```

Tested example from `tests/ranting/tutorial.rs::section_5_debug_feature`:

```rust
let morgan = Noun::new("Morgan", "she");
let result = say!("{=morgan walk}");
assert_eq!(result, "She walks");
```

---

## Next Steps

Now that you understand the basics, explore the **Cookbook** for 10 practical recipes covering game dialogue, chatbots, interactive fiction, and more.

### Quick reference

- **Pronouns**: I, you, he, she, it, we, ye, they
- **Tense markers**: `<` (past), `=` (continuous), `>` (future), `%` (perfect), `<=` (past continuous), `<%` (past perfect)
- **Case markers**: `=` (subject), `@` (object), `` ` `` (possessive determiner), `~` (possessive pronoun)
- **Plurality**: `+` (plural), `-` (singular), `$var`/`#var` (numeric-driven)

### Resources

- [Ranting on crates.io](https://crates.io/crates/ranting)
- [API documentation on docs.rs](https://docs.rs/ranting/)
- Source code: https://github.com/RoelKluin/ranting.rs

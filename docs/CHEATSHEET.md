# Ranting Cheatsheet

A single-page quick reference for `say!()` placeholder syntax and the crate's
public macros/types. For a guided introduction see the [Tutorial](TUTORIAL.md);
for worked examples see the [Cookbook](COOKBOOK.md); for the full public API
see [API.md](API.md) or [docs.rs](https://docs.rs/ranting).

Every example on this page was run against the current codebase — outputs are
copy-pasted from real `say!()` calls, not hand-typed.

## Placeholder anatomy

```
{[,^]?(article)?([+-]|[$#]?var)?[`=@~%*?]?noun( verb)( !word | !!word)}
```

- **Case modifier** (optional, first): `,` forces lowercase, `^` forces
  uppercase, overriding the sentence-start default.
- **Article** (optional): `a`, `an`, `some`, `the`, `these`, `those`.
- **Plurality** (optional): `+` force plural, `-` force singular, `$var`/`#var`
  numeric-driven.
- **Case marker** (optional): which grammatical form of the noun to render —
  see the table below. No marker at all renders the noun's **name**, not a
  pronoun.
- **Noun**: a variable name or positional index (`0`, `1`, ...).
- **Verb** (optional): a base verb, optionally prefixed with a tense marker.
- **Degree** (optional, after a post-noun word): `!word` comparative, `!!word`
  superlative.

## Case markers

No marker at all shows the noun's **name**, not a pronoun — this is the most
common surprise, see the first row.

| Marker | Meaning | Example | Output |
|---|---|---|---|
| *(none)* | Name (default) | `say!("{person walk}")` with `Noun::new("person","he")` | `"Person walks"` |
| `=` | Subject | `say!("{=person walk}")` (same noun) | `"He walks"` |
| `@` | Object | `say!("{@0}", noun)` | `"her"` |
| `` ` `` | Possessive determiner | ``say!("{`jane}")`` with `Noun::new("Jane","I")` | `"My"` |
| `~` | Adjective (same as possessive) | ``say!("{~tarzan}")`` | `"His"` |
| `%` | Reflexive | `say!("Only {^%alex} can decide that.")` with `alex = Noun::new("Alex","she")` | `"Only Herself can decide that."` |
| `*` | Display name, verb still agrees | `say!("{*tarzan who have} book")` | `"Tarzan who has book"` |
| `?` | Hidden — inflects but isn't shown | `say!("{?=person are} here, but {=person} stays invisible above.")` | `"She is here, but she stays invisible above."` |

`?` composes with other markers, e.g. `{?the noun}` hides the noun (and its
article) while still driving agreement elsewhere in the sentence — see
`README.md`'s `no_article` example.

## Articles

`a`/`an`/`some` adapt to the noun's plurality; `the` is invariant; `these`/
`those` convert to `this`/`that` for a singular noun.

| Placeholder | Noun | Output |
|---|---|---|
| `{a dog}` | `Noun::new("dog","it")` | `"A dog"` |
| `{the dog}` | same | `"The dog"` |
| `{a dogs}` | `Noun::new("dog","they")` | `"Some dog"` |
| `{the dogs}` | same | `"The dog"` |
| `{these dog}` | `Noun::new("dog","it")` (singular) | `"This dog"` |
| `{those dog}` | same | `"That dog"` |
| `{these 0}` | `Noun::new("one","they")` (plural) | `"These one"` |
| `{those 0}` | same | `"Those one"` |

Note the article adapts to plurality on its own, but the noun's own displayed
name does not — `{a dogs}` gives `"Some dog"`, not `"Some dogs"`; add `+` (see
below) to pluralize the name too.

## Plurality forcing

| Placeholder | Example | Output |
|---|---|---|
| `{+0}` | `say!("{+0}", book)` with `book = Noun::new("book","it")` | `"Books"` |
| `{-=0 do}` | `say!("{-=0 do}", person)` with plural-declared `person` | `"It does"` |
| `{+=0 do}` | same `person` | `"They do"` |
| `{$count noun}` | `say!("I see {$count apple}.", count = 3, apple = ...)` | `"I see 3 apples."` |
| `{#count noun}` | same, but count spelled out | `"I see three apples."` |
| `{#count ?noun}` | count word shown, noun hidden | `"I count three."` |
| `{$one noun}` | `count = 1` | `"I see 1 apple."` |

## Case-forcing modifiers (`,` / `^`)

Only needed mid-sentence — a placeholder at sentence start auto-capitalizes by
default.

| Placeholder | Context | Output |
|---|---|---|
| `{,+0}` | `"The person is actually a {,+0}."` (irregular plural, forced lowercase) | `"The person is actually a people."` |
| `{^%alex}` | `"Only {^%alex} can decide that."` (forced uppercase mid-sentence) | `"Only Herself can decide that."` |

## Verb tenses

Write the **base** verb; a tense marker prefixes it. Never pass an
already-conjugated verb (`{=person goes}` → `"He goess"` ✗) — the tense marker
does the conjugating.

| Tense | Marker | Example | Output |
|---|---|---|---|
| Present | *(none)* | `{=person walk}` | "He walks" |
| Past | `<` | `{=person <walk}` | "He walked" (irregular: "He went") |
| Present continuous | `=` | `{=person =walk}` | "He is walking" |
| Future | `>` | `{=person >walk}` | "He will walk" |
| Past continuous | `<=` | `{=person <=walk}` | "He was walking" |
| Present perfect | `%` | `{=person %walk}` | "He has walked" (irregular: "He has gone") |
| Past perfect | `<%` | `{=person <%walk}` | "He had walked" (irregular: "He had gone") |

`say_with!(context, "...", args...)` resolves these markers against a runtime
`NarrationContext.tense` instead, falling back to the marker's own default
when the context doesn't override it. `say!()` output is unaffected either way.

## Comparative / superlative (degree markers)

```rust
say!("{?w !good} than that.", w)   // "better than that."
say!("{?w !!good} in class", w)    // "best in class"
```
Irregular table plus regular `-er`/`-est` (with CVC doubling, y→i) and
periphrastic `more`/`most` for longer adjectives. Resolved entirely at
compile time — no `NarrationContext`/subject agreement needed.

## Macros

| Macro | Signature | Behavior |
|---|---|---|
| `say!()` | `say!(fmt, args...)` | Builds a `String`, like `format!()` but with placeholder grammar. |
| `say_with!()` | `say_with!(context, fmt, args...)` | Like `say!()`, but resolves tense/viewpoint markers against a runtime `NarrationContext`. |
| `ack!()` | `ack!(fmt, args...)` | Expands to `Ok(say!(fmt, args...))` — a plain expression, not a hidden `return`. |
| `nay!()` | `nay!(fmt, args...)` | Expands to `Err(say!(fmt, args...))` — same, a plain expression. |
| `heed!()` | `heed!(template, input)` | Inverse direction: matches `input` text against `template`, returns captures. |
| `ask!()` | `ask!(speaker, audience, template, input)` | Parses `input` against `template` like `heed!()`, forwards captures to `audience`'s `Answerable::answer`. Returns `Option<String>`. |

```rust
let result: Result<String, String> = ack!("{=p are} welcome.");
assert_eq!(result, Ok("She is welcome.".to_string()));

let result: Result<String, String> = nay!("{=p can't} get in {`p} house.");
assert_eq!(result, Err("She can't get in her house.".to_string()));
```

### `heed!()`

```rust
heed!("take {item}", "take sword")                    // Some("sword".to_string())
heed!("give {item} to {target}", "give sword to guard") // Some(("sword".to_string(), "guard".to_string()))
heed!("take {item}", "drop sword")                    // None
```
`{name}` — one token. `{name...}` — greedy, to the next literal or end of
input. `{$name}` — digits, parsed as `u64`. Two adjacent captures with no
literal text between them is a **compile-time** error (ambiguous).

### `#[derive(Heed)]`

```rust
#[derive(Heed)]
#[heed(template = "give {item} to {target}")]
struct Give { item: String, target: String }

Give::heed("give sword to guard") // Some(Give { item: "sword".into(), target: "guard".into() })
Give::heed("drop sword")          // None
```
Struct-flavored `heed!()` — same template grammar, but binds captures to
same-named fields instead of returning a positional tuple. Every capture
needs a matching field and vice versa; field type must match capture kind
(`String` vs `u64`). See API.md for the full rules.

### `ask!()`

```rust
struct Dog;
impl Answerable for Dog {
    type Captures = ();
    fn answer(&self, _speaker: &dyn Ranting, _: ()) -> String {
        "Woof!".to_string()
    }
}
let player = Noun::new("Jo", "she");
ask!(player, Dog, "pet dog", "pet dog") // Some("Woof!".to_string())
ask!(player, Dog, "pet dog", "kick dog") // None
```
Same template grammar as `heed!()`, but captures are forwarded to
`audience`'s [`Answerable::answer`](API.md#the-answerable-trait-asks-audience)
instead of returned directly — see API.md for the full shape and a
capture-driven example.

## Core types

| Type | Purpose |
|---|---|
| `Noun` | The concrete `Ranting` implementor. `Noun::new(name, subject)` panics on an invalid subject; `Noun::try_new` returns `Result<Noun, InvalidSubjectError>`. |
| `Many<T: Ranting>` | Wraps `Vec<T>` — a collective noun phrase (`"a, b and c"`), plural whenever `len() != 1` (zero included). |
| `Maybe<T: Ranting>` | Wraps `Option<T>` — `Some(x)` delegates to `x`; `None` renders as nothing, singular, subject `"it"`. |
| `Box<T: Ranting>` | Delegates every `Ranting` method straight through to the boxed value. |
| `NarrationContext` | Builder: `.tense(Tense)`, `.narration_person(Person)`, `.register(Register)`, `.dialect(&'static str)`. |
| `Answerable` | `ask!()`'s audience contract: `fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String`. |

See [API.md](API.md) for the full surface, or [docs.rs](https://docs.rs/ranting)
for generated reference docs.

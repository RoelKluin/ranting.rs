# API Reference

A structured overview of Ranting's public surface — what's exported, what it's
for, and how the pieces fit together. This complements, but doesn't replace,
the generated reference at [docs.rs/ranting](https://docs.rs/ranting): every
public item here has full rustdoc (including runnable examples) there. For a
guided introduction, see the [Tutorial](TUTORIAL.md); for placeholder syntax
at a glance, see the [Cheatsheet](CHEATSHEET.md).

Only `ranting`'s and `ranting_derive`'s public items are covered.
`ranting_core` is an internal, unversioned implementation crate shared by the
two — nothing in it is part of this crate's semver surface, even where
`ranting` re-exports an item originating there.

## Macros (from `ranting_derive`, re-exported by `ranting`)

| Macro | Signature | What it does |
|---|---|---|
| `say!` | `say!(fmt, args...)` | Parses `fmt`'s placeholders at compile time and generates a `format!()` call; returns `String`. The crate's core macro. |
| `say_with!` | `say_with!(context: &NarrationContext, fmt, args...)` | Like `say!()`, but placeholders with a tense marker bake the *uninflected* base verb, deferring tense resolution to `context.tense` at runtime. Falls back to the marker's own default tense when the context doesn't override it. Also threads `context.narration_person` (viewpoint override) and `context.register`/`.dialect` (available to custom `Ranting` impls). Without a context, output is identical to `say!()`. |
| `ack!` | `ack!(fmt, args...)` | Expands to `Ok(say!(fmt, args...))` — a plain expression, usable anywhere an expression is valid (`let`-bound, match-arm tail, etc). Write `return ack!(...)` yourself for early-return behavior. Intended for "allow" responses, not error handling. |
| `nay!` | `nay!(fmt, args...)` | Expands to `Err(say!(fmt, args...))` — same shape as `ack!()`. |
| `heed!` | `heed!(template, input)` | The inverse of `say!()`'s placeholder grammar: matches `input` text against `template`'s literal words plus `{name}`/`{name...}`/`{$name}` captures. Returns `None` on no match; on match, a bare value for 0/1 captures or a tuple for 2+, positional like `say!()`. Deliberately smaller than `say!()`'s grammar — no article/verb/pronoun-case markers. |
| `ask!` | `ask!(speaker, audience, template, input)` | Parses `input` against `template` exactly like `heed!()`, then forwards the captures to `audience`'s [`Answerable::answer`](#answerable-trait-asks-audience) — `audience.answer(&speaker, captures)`. Returns `Option<String>` (`None` on no match), joining `heed!()` in the Option-returning half of the macro family. Reworked in Phase 5 (ROADMAP.md) from an earlier, untested, duck-typed `audience.answer(speaker, format!(...))` shape. |
| `boxed_ranting_trait!` | `boxed_ranting_trait!(SomeTrait)` | Generates a `Ranting` impl for `&'_ dyn SomeTrait` where `SomeTrait: Ranting`. For when you want to use a boxed trait object as a `say!()` placeholder subject. |
| `ref_ranting_trait!` | `ref_ranting_trait!(SomeTrait)` | Same, for `Box<dyn SomeTrait>`. |

`#[derive_ranting]` + `#[ranting(...)]` (attribute + derive macro pair) are
covered under [Deriving `Ranting`](#deriving-ranting) below.

## The `Ranting` trait

The trait a type must implement to be usable as a `say!()` placeholder
subject. `Noun` implements it directly; `#[derive_ranting]` generates an impl
for your own structs/enums; `Box<T>`, `Many<T>`, `Maybe<T>` all forward it
(see [Wrapper types](#wrapper-types)).

**Required methods** (no default — every implementor must define these):

| Method | Returns | Purpose |
|---|---|---|
| `name(&self, uc: bool) -> String` | display name | struct name, `#[ranting(name = "...")]`, or `self.name` if `name = "$"` |
| `subjective(&self) -> &str` | subject pronoun | `"it"`, `#[ranting(subject = "...")]`, or `self.subject` if `subject = "$"` |
| `is_plural(&self) -> bool` | plurality | usually from `subjective()`; overridable for `you` |
| `inflect(&self, to_plural: bool, uc: bool) -> String` | singular/plural name form | uses `#[ranting(singular_end, plural_end)]` |
| `skip_article(&self) -> bool` | whether to omit an article | for proper nouns, uncountables, etc; `#[ranting(no_article = true)]` |

**Customization hooks** (all default to `None`, meaning "fall back to
built-in English rules"; each `_with_context` variant is what every call site
in the crate actually invokes — `say!()` passes `ctx: None`, `say_with!()`
passes `Some(ctx)` — so overriding only the `_with_context` form is enough):

| Method pair | Customizes |
|---|---|
| `inflect_verb_custom` / `_with_context` | verb conjugation (tense, plurality, person) |
| `inflect_pronoun_custom` / `_with_context` | pronoun form, keyed by `PronounCase` (`Subjective`/`Objective`/`PossessiveDeterminer`/`PossessivePronoun`/`Reflexive`) |
| `inflect_article_custom` / `_with_context` | article form (a/an/the/some, demonstratives) |

`ctx: Option<&NarrationContext>` is always a plain parameter on these hooks,
never read from `self` — an entity's own `subject` stays entity-owned, while
tense/viewpoint/register/dialect are story-wide settings that vary per call.
This is the mechanism [`docs/EXTENSIBILITY.md`](EXTENSIBILITY.md) uses to
build ecosystem forks (Spanish, pirate, Scottish, etc. dialects) without
forking the crate itself.

`Ranting: std::fmt::Display` — anything usable as a placeholder subject is
also directly interpolatable via `{}`.

## `Noun`

The concrete, built-in `Ranting` implementor — use it when you don't need a
custom struct/enum.

```rust
Noun::new(name: &str, subject: &str) -> Noun            // panics on an invalid subject
Noun::try_new(name: &str, subject: &str) -> Result<Noun, InvalidSubjectError>  // non-panicking
```

Valid `subject` values: `"I"`, `"you"`, `"thou"`, `"he"`, `"she"`, `"it"`,
`"we"`, `"ye"`, `"they"`. `Noun::new` is kept for backward-compatible call
sites that already know their subject string is valid; prefer `try_new` when
`subject` isn't a compile-time literal.

## Wrapper types

`Vec<T>` and `Option<T>` can't implement `Ranting` directly — the trait
requires `Display`, and Rust's orphan rules forbid implementing that foreign
trait for the foreign, non-`#[fundamental]` `Vec`/`Option` regardless of `T`.
Hence two local newtype wrappers (`Box<T>` has no such problem, since `std`
already provides `Display` for it):

| Type | Wraps | Behavior |
|---|---|---|
| `Box<T: Ranting>` | — | Delegates every `Ranting` method straight through to `*self`. |
| `Many<T: Ranting>` | `Vec<T>` | Collective noun phrase. Name renders as `"a, b and c"`. Plural whenever `len() != 1` (zero items included — "there are no items", not "there is no item"). Delegates plurality/pronoun/custom-hook behavior straight through when exactly one item; falls back to built-in English for 0 or 2+. `skip_article()` is `true` when empty. |
| `Maybe<T: Ranting>` | `Option<T>` | `Maybe(Some(x))` behaves exactly like `x`. `Maybe(None)` renders as nothing, is singular with subject `"it"`, and skips its article. |

These compose: `Many<Box<Noun>>`, `Box<Many<Noun>>`, etc. all work.

## Narration context (`say_with!()`)

```rust
NarrationContext::new()
    .tense(Tense::Past)
    .narration_person(Person::Third)
    .register(Register::Formal)
    .dialect("pirate")
```

All fields are `Option`, builder methods are chainable, and the struct is
`Copy` (reusable across multiple `say_with!()` calls).

| Type | Variants | Interpreted by the crate? |
|---|---|---|
| `Tense` | `Present`, `Past`, `Future`, `PresentContinuous`, `PastContinuous`, `PresentPerfect`, `PastPerfect` | Yes — resolves `<`/`=`/`>`/`<=`/`%`/`<%` markers at runtime. |
| `Person` | `First`, `Second`, `Third` | Yes, but scoped: only overrides nouns declared first-person (`subject` exactly `"I"`/`"we"`). Third-person rendering always falls back to singular "they" (no gender data on a first-person-declared noun). `we` → `Person::Second` renders "you", same as `I` would — one-way, not round-trippable. |
| `Register` | `Formal`, `Neutral`, `Casual` | No — inert unless a `Ranting` impl reads `ctx.register` from one of the `*_with_context` hooks. `None` means "no override"; `Some(Register::Neutral)` is a distinct, explicit middle value. |
| `dialect` | `&'static str`, fork-defined | No — same as `register`, purely a hook for custom impls. |

## The `Answerable` trait (`ask!()`'s audience)

Implemented by anything that can be the audience of an `ask!()` call:

```rust
pub trait Answerable {
    type Captures;
    fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String;
}
```

`Captures` mirrors `heed!()`'s own 0/1-vs-2+ convention (`()` for zero
captures, a bare `String` for one, a tuple of `String`s for two or more) —
but unlike `heed!()`, a `{$name}` numeric capture is **not** auto-converted
to `u64` here; `Captures` is always `String`/tuples of `String`, since a
trait method needs one fixed signature regardless of which template reached
it. Parse what you need inside `answer()`.

```rust
struct Dog;
impl Answerable for Dog {
    type Captures = ();
    fn answer(&self, _speaker: &dyn Ranting, _: ()) -> String {
        "Woof!".to_string()
    }
}

struct Villager;
impl Answerable for Villager {
    type Captures = String;
    fn answer(&self, speaker: &dyn Ranting, topic: String) -> String {
        match topic.as_str() {
            "bone" => format!("{} not looking for bones here.", speaker.subjective()),
            _ => "I don't know anything about that.".to_string(),
        }
    }
}

let player = Noun::new("Jo", "she");
assert_eq!(ask!(player, Dog, "pet dog", "pet dog"), Some("Woof!".to_string()));
assert_eq!(
    ask!(player, Villager, "about {topic}", "about bone"),
    Some("she not looking for bones here.".to_string())
);
```

Because `Captures` is an associated type, one implementor supports exactly
one capture arity everywhere it's used as an `ask!()` audience — a type
needing to answer differently-shaped questions would need `Captures =
Vec<String>` (losing compile-time arity checking) or a different design.
Accepted as a known limitation for now; see ROADMAP.md Phase 5.

## `heed!()` matching internals

`HeedMatcher` (in `ranting`, `#[doc(hidden)]`) backs the generated code
behind every `heed!()` call site — one `static HeedMatcher` per call site,
regex compiled once via `OnceLock`. Not meant to be constructed directly;
covered here only because it's the reason `ranting_derive`'s regex version
and `ranting`'s never need to match: `ranting_derive/src/heed.rs` only builds
the pattern string and validates it at compile time, and generated code
references `ranting::HeedMatcher`, never `regex::` types directly.

## Deriving `Ranting`

```rust
#[derive_ranting]
#[ranting(subject = "he", name = "Merlin")]
struct Wizard {}
```

**Core attributes** — determine grammatical function:

| Attribute | Default | Meaning |
|---|---|---|
| `subject` | `"it"` | The subject pronoun; `"$"` means read a `subject: String` field on the struct instead. |
| `name` | struct/enum name | Display name; `"$"` means read a `name: String` field instead. |
| `singular_end` | `""` | Suffix stripped when singularizing (for `inflect()`). |
| `plural_end` | `"s"` | Suffix added when pluralizing. |

**Cosmetic attributes** — formatting/display only:

| Attribute | Default | Meaning |
|---|---|---|
| `plural_you` | `false` | If `subject = "you"`, whether it refers to a plural "you". |
| `uc` | `false` | Whether the name always starts uppercase. |
| `no_article` | `false` | Whether to skip articles (proper nouns, meals, uncountables). |

## Feature flags

| Feature | Default? | Effect |
|---|---|---|
| `inclusive-pronouns` | ✅ | Enables docs/examples for gender-neutral singular "they" support. |
| `debug` | — | Prints each `say!()` pattern with its generated `format!()` conversion at compile time. |

## What's not covered here

- `PronounCase`, `InvalidSubjectError` — small supporting types, see
  their rustdoc.
- Anything in `ranting_core` — internal, unversioned, not part of this
  crate's public API even where re-exported.

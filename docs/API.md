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
covered under [Deriving `Ranting`](#deriving-ranting) below. `#[derive(Heed)]`
+ `#[heed(...)]` is covered under [Deriving `Heed`](#deriving-heed) below.

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
| `inflect(&self, to_plural: bool, uc: bool, case: GrammaticalCase) -> String` | singular/plural name form | uses `#[ranting(singular_end, plural_end)]`; `case` is the placeholder's own `GrammaticalCase` (`Name` for a bare `` {noun} ``), threaded through so a case-declining fork's `inflect()` can pick a form without needing a separate case-aware hook — see the fused-marker note below |
| `skip_article(&self) -> bool` | whether to omit an article | for proper nouns, uncountables, etc; `#[ranting(no_article = true)]` |

**Defaulted methods:**

| Method | Returns | Purpose |
|---|---|---|
| `noun_class(&self) -> NounClass` | lexical gender / noun class | `NounClass::UNSET` unless set; `#[ranting(gender = "...")]`, or `self.gender` if `gender = "$"`. See [`NounClass`](#nounclass) |
| `is_first_person_subject_custom(&self, subject: &str) -> bool` | whether `subject` counts as first-person | defaults to `ranting_core::grammar::is_first_person_subject` (`subject == "I" \|\| subject == "we"`); scopes `say_with!()`'s `NarrationContext.narration_person` viewpoint override (see [Narration context](#narration-context-say_with)) to a fork's own first-person labels, e.g. `ich`/`wir`. See `docs/EXTENSIBILITY.md` §2.10. |

**Customization hooks** (all default to `None`, meaning "fall back to
built-in English rules"; each `_with_context` variant is what every call site
in the crate actually invokes — `say!()` passes `ctx: None`, `say_with!()`
passes `Some(ctx)` — so overriding only the `_with_context` form is enough):

| Method pair | Customizes |
|---|---|
| `inflect_verb_custom` / `_with_context` | verb conjugation (tense, plurality, person) |
| `inflect_pronoun_custom` / `_with_context` | pronoun form, keyed by `PronounCase` (`Subjective`/`Objective`/`PossessiveDeterminer`/`PossessivePronoun`/`Reflexive`) and `NounClass` |
| `inflect_article_custom` / `_with_context` | article form (a/an/the/some, demonstratives), keyed by `GrammaticalCase` and `NounClass` |
| `inflect_adjective_custom` / `_with_context` | the post-noun `!`/`!!` adjective, keyed by [`AdjectiveDegree`](#adjectivedegree), `GrammaticalCase` and `NounClass` |
| `elide_article_custom` / `_with_context` | elision/contraction of a rendered article with the word after it — see [Elision](#elision-elide_article_custom) |
| `inflect_preposition_custom` / `_with_context` | fusion of a template-literal preposition with the article rendered right after it — see [Preposition Fusion](#preposition-fusion-inflect_preposition_custom) |
| `inflect_numeral_custom` / `_with_context` | how a placeholder's `#var`/`$var` number is written, keyed by [`NumeralStyle`](#numeralstyle), `GrammaticalCase` and `NounClass` — see [Numerals](#numerals-inflect_numeral_custom) |

The pronoun, article, adjective, elision, preposition-fusion and numeral hooks
also receive the noun's own [`NounClass`](#nounclass) as a `class` parameter,
and the article, adjective, elision, preposition-fusion and numeral hooks
their `GrammaticalCase`; the verb hook receives neither. Six of these hook
pairs — verb, pronoun, article, elision, preposition-fusion and adjective —
additionally take `count: Option<PlaceholderCount>`, sourced from the
placeholder's own `#var`/`$var` marker (`None` for a bare placeholder); the
numeral hook is the exception, since it already gets its own richer
`count: Option<i64>` (see [`PlaceholderCount`](#placeholdercount)).
`Many<T>` substitutes its own `Vec`'s length for `count` when delegating one
of these hooks to its single item — see [Wrapper types](#wrapper-types). See
`.claude/rules/extension-hooks.md`'s "What `as_plural: bool` promises" and
wrapper-delegation entries for the full history (ROADMAP.md Phase 6 items
14 and 15).

**Orthography hook** (defaults to today's English behavior rather than to
`None` — see [`OrthographyRole`](#orthographyrole)):

| Method pair | Customizes |
|---|---|
| `capitalize` / `_with_context` | whether and how a rendered word is capitalized, keyed by [`OrthographyRole`](#orthographyrole) |

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

```rust
Noun::with_noun_class(self, class: NounClass) -> Noun    // chains off new/try_new
```

Both constructors leave the class [`UNSET`](#nounclass); `with_noun_class`
consumes and returns the `Noun`, so it chains:
`Noun::new("Katze", "she").with_noun_class(NounClass::new("feminine"))`.

```rust
Noun::with_plural_end(self, plural_end: &str) -> Noun     // chains, like with_noun_class
Noun::with_singular_end(self, singular_end: &str) -> Noun
```

`Noun` has no `#[ranting(..)]` attributes to write, so these are how it opts out
of English's regular plural rules — the runtime equivalent of
`#[ranting(plural_end = "...")]`, with the same contract (see [Deriving
`Ranting`](#deriving-ranting)). Either one alone is enough; the suffix is then
appended/stripped literally with no English orthography. Declaring `"s"` is
meaningful and differs from leaving it unset:
`Noun::new("Party", "it").with_plural_end("s")` pluralizes to `Partys`, while a
plain `Noun::new("Party", "it")` gets `Parties`.

## `DeclaredEnding`

The trait `#[ranting(singular_end = "$")]` / `#[ranting(plural_end = "$")]` read
their field through, so both field shapes work:

| Field type | `declared()` | Meaning |
|---|---|---|
| `String` / `&str` | always `Some` | The struct declared a rule — literal strip-and-append. This is the documented shape. |
| `Option<String>` / `Option<&str>` | `None` when empty | Can additionally say "no rule declared" at runtime, i.e. use the language's own rules. |

Implement it yourself only for a third field shape. [`Noun`](#noun) uses the
`Option` case, which is what lets `with_plural_end` exist without every `Noun`
losing the English rules.

## `NounClass`

An open-ended lexical-gender / noun-class label carried by the entity and
handed to the pronoun, article and adjective hooks as their `class` parameter, so a
non-English implementation can pick `der`/`die`/`das` without an external
gender table keyed by the display string (which breaks on homographs, names,
and runtime-built nouns).

```rust
pub struct NounClass(&'static str);              // a newtype, not a closed enum

NounClass::UNSET                                  // == NounClass::new("")
NounClass::new(label: &'static str) -> NounClass
NounClass::as_str(&self) -> &'static str          // "" when unset
NounClass::is_unset(&self) -> bool
```

It is a newtype over `&'static str` rather than an
`enum { Masculine, Feminine, Neuter }` because Bantu languages have a
dozen-plus classes and Danish has common/neuter. `ranting` never interprets
the label — English has no lexical gender — it only carries it, like
`NarrationContext::dialect`. `Copy`, `Eq`, `Hash`, `Display`, and `Default`
(= `UNSET`).

Set it with `#[ranting(gender = "...")]`, `Noun::with_noun_class`, or by
overriding `Ranting::noun_class`. A noun that sets none reports `UNSET` and
renders byte-identically to how it did before this channel existed. See
[`docs/EXTENSIBILITY.md` §2.4](EXTENSIBILITY.md) for the worked German
example.

## `AdjectiveDegree`

Which degree marker a post-noun adjective was written with, handed to the
adjective hook:

```rust
pub enum AdjectiveDegree { Comparative, Superlative }   // `!` and `!!`
```

```rust
fn inflect_adjective_custom(
    &self,
    adjective: &str,          // as written in the placeholder ("noir", not "noirer")
    degree: AdjectiveDegree,
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
    count: Option<PlaceholderCount>,
    uc: bool,
) -> Option<String>
```

English resolves `!`/`!!` at compile time and needs no agreement, so this hook
defaults to `None` and English output is unaffected by its existence. It is
there for languages whose adjectives agree with their noun in gender, number
and case: `un chat noir` / `une robe noire` / `des chats noirs`. The hook
receives the adjective **as written**, since the compile-time English form is
not reversible back into it; on `None` that English form is emitted, `uc`
included (a custom form applies `uc_1st_if` itself).

There is no positive-degree variant because there is no positive-degree
marker: an unmarked post-noun word is parsed as a verb, so a fork whose
adjectives merely agree writes `!` and ignores `degree`. See
[`docs/EXTENSIBILITY.md` §2.5](EXTENSIBILITY.md) and
`tests/ranting/adjective_agreement.rs`.

## `NumeralStyle`

Which numeral notation a placeholder asked for, handed to the numeral hook:

```rust
pub enum NumeralStyle { Words, Digits }   // `{#n boots}` and `{$n boots}`
```

Like `AdjectiveDegree` and `GrammaticalCase` (and unlike `NounClass` and
`OrthographyRole`) this mirrors a compile-time type the macro bakes, because
the `#`/`$` marker is written in the placeholder. See
[Numerals](#numerals-inflect_numeral_custom).

## `PlaceholderCount`

```rust
pub struct PlaceholderCount {
    pub value: i64,             // the placeholder's `#var`/`$var` numeral, as an integer
    pub fraction_digits: u32,   // digits actually rendered after a decimal point, else 0
}
```

Carried as `count: Option<PlaceholderCount>` on six of the seven
`_custom`/`_with_context` hook pairs (verb, pronoun, article, elision,
preposition-fusion, adjective — ROADMAP.md Phase 6 item 14), `None` for a
placeholder with no `#var`/`$var` marker. `inflect_numeral_custom` is the
exception: it takes its own, differently-typed `count: Option<i64>` instead
— see [Numerals](#numerals-inflect_numeral_custom) for why that hook's
existing numeral signal made a second `PlaceholderCount` parameter there
redundant. `Many<T>` fills the `PlaceholderCount` gap with its own length
when delegating to a single item — see [Wrapper types](#wrapper-types).

## `OrthographyRole`

Which part of a rendered placeholder a word is, handed to the capitalization
hook:

```rust
pub enum OrthographyRole { Article, Verb, Pronoun, Noun, Adjective }

fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool, sentence_start: bool) -> String
fn capitalize_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> String
```

`capitalize` returns a `String`, not an `Option<String>`: it *is* the fallback,
not a chance to decline one, which is why it isn't named `_custom`. Its default
is exactly `uc_1st_if(word, uc)` — the call every one of those sites used to
make directly — so English output is unchanged unless you override it.

It exists because sentence-start uppercasing is an English assumption: German
capitalizes every noun wherever it stands, Japanese/Chinese/Arabic/Hebrew have
no letter case so `uc` is meaningless, and Turkish needs `i` → `İ`, which
`char::to_uppercase` gets wrong for a Turkish locale. The hook decides what is
*done* with `uc`; it does not decide `uc` itself — sentence position and the
`,`/`^` markers are resolved by the macro at compile time. Case *preservation*
of a word's own spelling (an irregular plural's ALL-CAPS/Title/lowercase
pattern) is a different thing and is not routed here.

`sentence_start` (Phase 6 item 17) is `uc`'s underlying signal alone, without
the `,`/`^`/uppercase-pre-word overrides folded in — `uc` and `sentence_start`
can disagree in both directions (`` {The 0} `` mid-sentence: `uc == true`,
`sentence_start == false`; `` {,noun} `` right after a period: `uc == false`,
`sentence_start == true`). Most forks only need `uc` and can ignore the new
parameter; it exists for a caseless-script fork that still tracks sentence
boundaries, or a downstream word-order layer (see
[`docs/EXTENSIBILITY.md` §2.6](EXTENSIBILITY.md)).

Sentence-start detection itself was also widened in the same change: it used
to recognize only an ASCII `.`/`?`/`!` followed by whitespace. It now also
recognizes Greek's question mark, Japanese/Chinese full-width terminators
(which take no following space), Urdu's full stop, and Spanish's opening
`¿`/`¡` — see `ranting_core::grammar::SENTENCE_TRIGGER_CHARS` and
`tests/ranting/sentence_detection.rs`.

One asymmetry: at `OrthographyRole::Noun` the word has already been through
`inflect()`, which takes `uc` itself, so the hook is passed `uc: false` there.
An always-capitalize fork ignores `uc` anyway; a fork needing position-sensitive
noun casing overrides `name`/`inflect`. See
[`docs/EXTENSIBILITY.md` §2.6](EXTENSIBILITY.md) and
`tests/ranting/orthography.rs`.

## Elision (`elide_article_custom`)

The one hook that runs *after* assembly rather than instead of it:

```rust
fn elide_article_custom(
    &self,
    article: &str,      // as rendered, capitalization included
    separator: &str,    // whitespace between it and what follows, usually " "
    following: &str,    // rendered text adjacent to it: any pre-text words the
                        // placeholder carried after the article, then the number,
                        // then the noun name or case-selected pronoun
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
    count: Option<PlaceholderCount>,
) -> Option<String>     // Some(fused) replaces all three; None (default) keeps them
fn elide_article_custom_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> Option<String>
```

English `a`/`an` is the crate's only article choice that depends on the word
*after* the article, and it is hard-coded phonology. `inflect_article_custom`
cannot express its equivalent for another language for a structural reason
rather than a missing parameter: it returns its string *before* the following
text has been rendered. French `l'homme` vs `le chien`, Italian `lo`/`il`/`l'`
and Portuguese article fusion need both words at once, so this hook gets them
after assembly and returns one fused replacement for the lot.

There is no `uc` parameter — `article` arrives already capitalized, so `uc` has
nothing left to decide. English `a`/`an` never routes through here, and the
default returns `None`, so `say!()`'s English output is byte-identical.

Not reachable from here: preposition-article fusion across a placeholder
boundary (`de` + `le` → `du`) — that gap now has its own hook, described next
— and hidden nouns (`` {?the noun} ``), which render nothing to elide
against. See [`docs/EXTENSIBILITY.md` §2.7](EXTENSIBILITY.md) and
`tests/ranting/elision.rs`.

## Preposition Fusion (`inflect_preposition_custom`)

The literal word immediately before a placeholder in the template, fused with
the article rendered right after it — German `zu` + `dem` → `zum`, Spanish
`de` + `el` → `del`:

```rust
fn inflect_preposition_custom(
    &self,
    preposition: &str,   // the literal word exactly as written in the template
    article: &str,       // as rendered — from inflect_article_custom or the English fallback
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
    count: Option<PlaceholderCount>,
    uc: bool,
) -> Option<String>      // Some(fused) replaces both preposition and article; None keeps both as rendered
fn inflect_preposition_custom_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> Option<String>
```

The preposition is template literal text sitting *before* the placeholder's
`{...}` even opens, so `inflect_article_custom` (which renders before the
following text exists) and `elide_article_custom` (whose span starts at the
article, never before it) cannot reach it. `ranting_derive::parse_str_params`
captures the single literal word immediately adjacent to a placeholder — the
same regex match `at_sentence_start` already reads — and bakes it into
`PlaceholderSpec::preposition` instead of discarding it, so this hook can see
it. Called at the same post-assembly point as `elide_article_custom`, and
tried first: if it returns `Some`, the preposition and the article it
consumed are both replaced and `elide_article_custom` is not called at all
(the article no longer exists to elide). If it returns `None` — the default,
and every case English needs — the preposition renders exactly as written and
`elide_article_custom` still gets its normal, unaffected chance, so `say!()`'s
English output is byte-identical either way.

Not reachable from here: a multi-word preposition, or one separated from the
placeholder by punctuation or an adverb (only the single adjacent word is
ever captured); anything rendered between the preposition and the article
(the hook is only offered the directly-adjacent case); and hidden nouns
(`` {?the noun} ``), which render no article to fuse against. See
[`docs/EXTENSIBILITY.md` §2.14](EXTENSIBILITY.md) and
`tests/ranting/preposition_fusion.rs`.

## Numerals (`inflect_numeral_custom`)

```rust
fn inflect_numeral_custom(
    &self,
    numeral: &str,      // the number as English renders it — the fallback if this declines:
                        // the spelled-out word for Words, the formatted digits for Digits
    count: Option<i64>, // the number itself; always Some for Words, parsed back out of
                        // `numeral` for Digits (None for a float, padded or non-numeric arg)
    style: NumeralStyle,
    case: GrammaticalCase,
    class: NounClass,
    as_plural: bool,
) -> Option<String>     // Some(numeral) replaces the rendering; None (default) keeps English's
fn inflect_numeral_custom_with_context(/* the same, plus */ ctx: Option<&NarrationContext>) -> Option<String>
```

A placeholder writes its number two ways and both were hard-coded English:
`` {#n boots} `` spelled it out via the `english-numbers` crate, `` {$n boots} ``
printed the argument's own `Display`, i.e. ASCII digits. Other languages need
their own speller, several agree the numeral with the noun's gender (Russian
`два стола` / `две книги`), and several scripts have their own digits. `#var` is
therefore spelled at runtime now, from a count the macro bakes, with the same
English speller as the fallback — so the hook can replace it wholesale.

There is no `uc` parameter: the numeral is never capitalized (a sentence-initial
placeholder spends its `uc` on the article, verb or noun). A returned string
replaces the rendering outright, so a `:fmt` width/fill spec on `$var` is not
re-applied to it. Not called when nothing numeric renders — no `#var`/`$var`
marker, or a hidden one (`` {?$n boots} ``); `heed!()`/`ask!()`'s `{$name}` is
input parsing and does not route here either. The `count` is local to the
numeral and does not discharge the count channel still owed on the `as_plural`
hooks. See [`docs/EXTENSIBILITY.md` §2.8](EXTENSIBILITY.md) and
`tests/ranting/numeral.rs`.

## Wrapper types

`Vec<T>` and `Option<T>` can't implement `Ranting` directly — the trait
requires `Display`, and Rust's orphan rules forbid implementing that foreign
trait for the foreign, non-`#[fundamental]` `Vec`/`Option` regardless of `T`.
Hence two local newtype wrappers (`Box<T>` has no such problem, since `std`
already provides `Display` for it):

| Type | Wraps | Behavior |
|---|---|---|
| `Box<T: Ranting>` | — | Delegates every `Ranting` method straight through to `*self`. |
| `Many<T: Ranting>` | `Vec<T>` | Collective noun phrase. Name renders as `"a, b and c"`. Plural whenever `len() != 1` (zero items included — "there are no items", not "there is no item"). Delegates plurality/pronoun/custom-hook behavior straight through when exactly one item; falls back to built-in English for 0 or 2+. `skip_article()` is `true` when empty. `noun_class()` reports the single item's class when `len() == 1`, else `NounClass::UNSET`. For the same `len() == 1` delegation, `count: Option<PlaceholderCount>` is substituted with `Some(PlaceholderCount { value: 1, .. })` (its own length) whenever the placeholder itself carried no numeral, so a fork's hook still sees a count even with no `#var`/`$var` present; an explicit placeholder numeral is left untouched. |
| `Maybe<T: Ranting>` | `Option<T>` | `Maybe(Some(x))` behaves exactly like `x`. `Maybe(None)` renders as nothing, is singular with subject `"it"`, skips its article, and reports `NounClass::UNSET`. |

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
| `Person` | `First`, `Second`, `Third` | Yes, but scoped: only overrides nouns declared first-person (default: `subject` exactly `"I"`/`"we"`, overridable per-implementor via `Ranting::is_first_person_subject_custom`). Third-person rendering always falls back to singular "they" (no gender data on a first-person-declared noun). `we` → `Person::Second` renders "you", same as `I` would — one-way, not round-trippable. |
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

## Deriving `Heed`

```rust
use ranting::Heed;

#[derive(Heed)]
#[heed(template = "give {item} to {target}")]
struct Give {
    item: String,
    target: String,
}

assert_eq!(
    Give::heed("give sword to guard"),
    Some(Give { item: "sword".to_string(), target: "guard".to_string() })
);
assert_eq!(Give::heed("drop sword"), None);
```

Struct-level sugar over `heed!()` — not a separate matching engine. The
`#[heed(template = "...")]` attribute is compiled with the exact same
template parser `heed!()` uses, and generates `fn heed(input: &str) ->
Option<Self>` on the struct. Rules:

- Every capture in the template must have a same-named field, and every
  field must have a same-named capture — this is a one-to-one mapping, not
  partial. A stale field or a renamed/unmapped capture is a compile error.
- Field types are checked against their capture kind: `{name}`/`{name...}`
  require a `String` field; `{$name}` requires a `u64` field.
- Only structs are supported (named fields, or a unit struct for a
  zero-capture template) — no enums.
- Field declaration order is independent of the template's capture-appearance
  order; the derive maps by name, not position.

See `ranting_derive/src/heed_derive.rs` and `tests/ranting/heed_derive.rs`.

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
| `singular_end` | unset (`""`) | Suffix stripped when singularizing (for `inflect()`). `"$"` means read a `singular_end: String` field instead — or an `Option<String>` one, which can additionally say "unset" at runtime (see [`DeclaredEnding`](#declaredending)). |
| `plural_end` | unset (`"s"`) | Suffix added when pluralizing. **Writing neither attribute selects English's regular rules** (`fly`→`flies`, `box`→`boxes`, `bookshelf`→`bookshelves`, `mother-in-law`→`mothers-in-law`), applied after the `data/irregular_plurals.txt` lookup. Writing either declares your own rule, and the suffix is then stripped/appended literally with no English orthography: `plural_end = "e"` on a noun named `Fuchs` gives `Fuchse`, not `Fuchses`. **The switch is whether you wrote the attribute, not its value** — `plural_end = "s"` is a genuine opt-out (bare append, no orthography: `Party`→`Partys`, where the rules say `Parties`), which is what a German/Dutch/Danish loanword plural needs. Singularization always strips literally — the inverse rules are deliberately not implemented, since no spelling rule separates `cities`→`city` from `movies`→`movie`. |
| `gender` | `""` (unset) | Lexical gender / noun class label, e.g. `"masculine"`; any label a fork wants. `"$"` means read a `gender: ranting::NounClass` field instead. Surfaces as [`noun_class()`](#nounclass); omitting it generates no `noun_class` override at all. |

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

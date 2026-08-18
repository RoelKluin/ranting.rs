# Ranting Advanced Examples

Where the [Tutorial](TUTORIAL.md) teaches one feature at a time and the [Cookbook](COOKBOOK.md)
gives one recipe per use case, this page shows **several features composed in one passage** —
the shape of text `say!()` is actually meant to produce. Each scene combines placeholder markers
that are individually simple but, put together, do more than any single example elsewhere in the
docs shows at once.

> All examples are backed by compiled tests in `tests/ranting/advanced.rs`, so they're checked on
> every `cargo test`, and every rendered output on this page is copy-pasted from a real `say!()`
> call — not hand-typed.

For the syntax behind any individual marker used below, see the [Cheatsheet](CHEATSHEET.md).

---

## Scene 1: Introduce once, refer to invisibly

A character's name only needs writing once. Later placeholders can reference the same noun purely
to drive agreement — a superlative adjective, a past-perfect verb — without repeating it.

```rust
use ranting::*;

let jane = Noun::new("Jane", "she");
say!(
    "{jane}, {the ?jane !!good} in class, {?jane <%receive} a bad mark.",
    jane
)
// "Jane, the best in class, had received a bad mark."
```

- `{jane}` renders the name.
- `{the ?jane !!good}` — `?` hides the noun entirely, but the superlative degree marker (`!!good`
  → "best") and the article ("the") still render, agreeing with `jane`'s declared pronoun.
- `{?jane <%receive}` — hidden again, this time driving a past-perfect verb ("had received") with
  full person/number agreement, with no noun to show for it.

**Tested**: `tests/ranting/advanced.rs::scene_1_introduce_once_refer_invisibly`

---

## Scene 2: A merchant's inventory

Numerals and future tense compose in the same sentence, agreeing with two different nouns at once.

```rust
use ranting::*;

let merchant = Noun::new("Merchant", "she");
let sword = Noun::new("sword", "it");
say!(
    "{=merchant have} {$n sword} for sale. If you buy them all, {=merchant >give} you a discount.",
    merchant, n = 3, sword
)
// "She has 3 swords for sale. If you buy them all, she will give you a discount."
```

- `{$n sword}` renders the digit count and pluralizes `sword` to match it — one placeholder for
  both the number and the noun's agreement.
- `{=merchant have}`/`{=merchant >give}` conjugate the same verb differently (present vs. future)
  from one declared noun.

**Tested**: `tests/ranting/advanced.rs::scene_2_a_merchants_inventory`

---

## Scene 3: Nothing left, but plenty of gold

The zero-count idiom and a mass noun's own quantifier selection, in one breath.

```rust
use ranting::*;

let item = Noun::new("item", "it");
let merchant = Noun::new("Merchant", "she");
let gold = Noun::new("gold", "it").with_mass();
say!(
    "There {are no ?$n item} left in the shop. {=merchant have} {much gold}, though.",
    n = 0i64, item, merchant, gold
)
// "There are no items left in the shop. She has much gold, though."
```

- `{are no ?$n item}` — the numeral is hidden (`?$n`), but it still drives `are`/`is` agreement
  and pluralizes `item`; passing `n = 1` instead renders "There is no item left...".
- `{much gold}` — `much`/`many` picks itself based on `gold.is_mass()`, the same way `a`/`an`/
  `some` already does for count nouns.

**Tested**: `tests/ranting/advanced.rs::scene_3_nothing_left_but_plenty_of_gold`

---

## Scene 4: Only they can decide

Comparative degree, a hidden-noun-driven superlative, a reflexive pronoun, and a possessive
determiner, across three different declared pronouns in one sentence.

```rust
use ranting::*;

let tarzan = Noun::new("Tarzan", "he");
let jane = Noun::new("Jane", "she");
let alex = Noun::new("Alex", "they");
say!(
    "{=tarzan are} {?jane !strong} than {jane}, but {=alex decide} for {%alex} what is {?alex !!good} for {`alex} own future.",
    tarzan, jane, alex
)
// "He is stronger than Jane, but they decide for themselves what is best for their own future."
```

- `{?jane !strong}` hides `jane` a second time (she was already named later in the same clause)
  while still rendering the comparative form ("stronger").
- `{%alex}` (reflexive) and `` {`alex} `` (possessive determiner) both agree with `alex`'s
  singular-they declaration ("themselves", "their") without any special-casing.

**Tested**: `tests/ranting/advanced.rs::scene_4_only_they_can_decide`

---

## Scene 5: Third time's not the charm

An ordinal numeral, reused with a different count, alongside a possessive determiner.

```rust
use ranting::*;

let tarzan = Noun::new("Tarzan", "he");
let attempt = Noun::new("attempt", "it");
say!(
    "This is {`tarzan} {##n attempt}. If {=tarzan fail} again, this will be {`tarzan} {##m attempt}.",
    tarzan, n = 3i64, attempt, m = 4i64
)
// "This is his third attempt. If he fails again, this will be his fourth attempt."
```

- `{##n attempt}` spells the ordinal ("third", "fourth") from a plain integer count — no
  `-st`/`-nd`/`-rd`/`-th` bookkeeping in the template.
- `` {`tarzan} `` renders "his" both times from the same declared noun.

**Tested**: `tests/ranting/advanced.rs::scene_5_third_times_not_the_charm`

---

## Scene 6: The Joneses' menagerie

A plural proper name's possessive, article elision, and an irregular plural, together.

```rust
use ranting::*;

let joneses = Noun::new("Joneses", "they");
let owl = Noun::new("owl", "it");
let goose = Noun::new("goose", "it");
say!(
    "{the 0's} house has {an owl} and {a +2}.",
    joneses, owl, goose
)
// "The Joneses' house has an owl and some geese."
```

- `{the 0's}` renders the bare apostrophe ("Joneses'"), not "Joneses's" — a plural name takes the
  bare form, a singular one ending in `s` still takes `'s`.
- `{an owl}` elides "a" to "an" against the following vowel sound.
- `{a +2}` forces `goose` plural, which is irregular ("geese"), and adapts the article to "some"
  to match.

**Tested**: `tests/ranting/advanced.rs::scene_6_the_joneses_menagerie`

---

## Scene 7: Seeing isn't always seeing

Everything above composes markers that ship with `ranting`. This scene shows the other axis of
depth: a single template word ("see") rendering as a genuinely different verb — "perceive",
"regard" — chosen per entity by an ordinary Rust field, via the same `_custom` hook mechanism the
crate's non-English forks use. Nothing here is i18n-specific; it works entirely within English.

```rust
use ranting::*;

enum Sense { Sight, Insight, Judgment }

struct Watcher {
    noun: Noun,
    sense: Sense,
}

impl std::fmt::Display for Watcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.noun)
    }
}

impl Ranting for Watcher {
    fn subjective(&self) -> &str { self.noun.subjective() }
    fn name(&self, uc: bool) -> String { self.noun.name(uc) }
    fn is_plural(&self) -> bool { self.noun.is_plural() }
    fn skip_article(&self) -> bool { self.noun.skip_article() }
    fn inflect(
        &self,
        as_pl: bool,
        uc: bool,
        case: GrammaticalCase,
        count: Option<PlaceholderCount>,
    ) -> String {
        self.noun.inflect(as_pl, uc, case, count)
    }

    fn inflect_verb_custom(
        &self,
        subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if verb != "see" {
            return None;
        }
        let synonym = match self.sense {
            Sense::Sight => return None, // fall back to the default "see"/"sees"
            Sense::Insight => "perceive",
            Sense::Judgment => "regard",
        };
        let conjugated = match subject {
            "he" | "she" | "it" => format!("{synonym}s"),
            _ => synonym.to_string(),
        };
        Some(capitalize_if(&conjugated, uc))
    }
}

let oracle = Watcher { noun: Noun::new("Oracle", "she"), sense: Sense::Insight };
say!("{=oracle see} the truth.", oracle)
// "She perceives the truth."

let judge = Watcher { noun: Noun::new("Judge", "he"), sense: Sense::Judgment };
say!("{=judge see} the case fairly.", judge)
// "He regards the case fairly."

let guard = Watcher { noun: Noun::new("Guard", "they"), sense: Sense::Sight };
say!("{=guard see} the gate.", guard)
// "They see the gate."
```

The template never changes — `{=noun see}` is written once. Each `Watcher`'s own `sense` field
picks the rendered verb, and person/number agreement (`perceives` vs. `perceive`, `regards` vs.
`regard`) still runs correctly per entity, exactly as it would for the built-in `see`/`sees`.

One limit worth knowing: this handles the plain present-tense verb slot. A tense-marked verb
(`{<=oracle see}`, past) hands the hook the *already-conjugated* form ("saw"), not the base word,
since `say!()` bakes tense conjugation at compile time — matching on `verb == "see"` alone won't
catch it. A hook that wants synonym-swapping to compose with every tense marker needs to match on
each tense's conjugated form too.

**Tested**: `tests/ranting/advanced.rs::scene_7_seeing_isnt_always_seeing`

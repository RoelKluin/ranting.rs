# Design spike: negative-count agreement, gated on `is_mass()`

**Status**: design spike, PROPOSED only — **no code in this repository is changed by this
document**. ROADMAP.md Phase 9 item 2. Confirms the defect against the built crate, proposes an
exact gating change scoped to `Plurality::CardinalWords` (`#var`) only, and states plainly that
the two existing pinned assertions this would flip are currently pinned to the *old* behavior on
purpose — a maintainer has to choose which reading of English wins before either can move.

## The question, grounded

`docs/architecture-review-2026-08-15.md` §1.12 and ROADMAP.md Phase 8 §1.12 recorded, but
deliberately did not schedule, this: `as_pl` for a spelled numeral is computed at
`src/lib.rs:721` as

```rust
Plurality::CardinalWords => count != Some(1),
```

so a negative count of magnitude one takes the plural, because `-1 != 1`. That is correct for a
measure noun ("minus one degrees" is what English idiom actually says) and wrong for a countable
one ("minus one item", not "minus one items"). The record explicitly deferred this to "the
mass/count split item 3 part (b) would supply" — Phase 8 item 3 landed `Ranting::is_mass()`
2026-08-17, which is what unblocks this item.

**Confirmed against the built crate** (`cargo test --test ranting
numeral::a_negative_words_numeral_spells_the_sign_as_a_word`, passes today):

```rust
let boot = Noun::new("boot", "it"); // plain countable noun, is_mass() == false by default
assert_eq!(say!("I see {#0 boot}", -1), "I see minus one boots");   // tests/ranting/numeral.rs:359
assert_eq!(say!("{#0 boot}", -1), "Minus one boots");                // tests/ranting/numeral.rs:374
```

`boot` is an ordinary countable noun (`Noun::new`, no `.with_mass()`), so today's plural agreement
on `-1` is exactly the case §1.12 calls wrong.

**Confirmed `is_mass()` is reachable at the call site.** `Plurality::CardinalWords => count !=
Some(1)` sits inside `handle_placeholder_impl` (`src/lib.rs:681-731`), which takes `noun: &R`
where `R: Ranting` as its first parameter — the exact same binding every other arm in that
function already reads (`noun.is_plural()` one line above, `noun.name(false)` elsewhere in the
file). `Ranting::is_mass(&self) -> bool` (`src/lib.rs:2245-2247`, default `false`) is an ordinary
trait method on that same `noun`, so `noun.is_mass()` is available at line 721 with no signature
change anywhere — no new hook, no new hook parameter, no new field on `PlaceholderSpec`.

## Why "gate on `!self.is_mass()`" is not quite the shape

The item's own candidate text ("gate the negative-count case on `self.is_mass()` the same way
`AAnSome` and `MuchMany` already do") reads naturally as *only non-mass nouns take the plural
here*, i.e. something like:

```rust
Plurality::CardinalWords => !noun.is_mass() && count != Some(1),
```

That is wrong on inspection: it would make **every** mass-noun count singular, including `-5`
("minus five degrees" would render "minus five degree") and even `+2` for a mass noun, which
`AAnSome`/`MuchMany` never touch — those two arms only ever pick *which word* (`a`/`some`,
`much`/`many`) surrounds an already-agreed noun; they don't change `as_pl` itself. The actual
defect is narrower than "mass nouns behave differently from count nouns in general" — it is
specifically about **magnitude-one negative counts**. Every other count (`0`, `2`, `-2`, `-21`,
...) is unambiguously plural in English for both mass and countable nouns alike ("zero items",
"minus two items", "minus two degrees" are all fine as written today and are not in dispute).

The precise equivalent, scoped to the one ambiguous case:

```rust
Plurality::CardinalWords => match count {
    Some(1) => false,               // unchanged: "one item"
    Some(-1) => noun.is_mass(),     // the fix: "minus one item" (count) vs "minus one degrees" (mass)
    _ => true,                      // unchanged: 0, +2, -2, -21, ... all plural regardless of mass/count
},
```

This is a strict narrowing of the current `count != Some(1)` rule at exactly one input value
(`Some(-1)`) and leaves every other input — including `Some(1)` and every magnitude ≥ 2, positive
or negative — byte-identical to today. It does *not* generalize "gate on `is_mass()`" to the whole
arm, because nothing else in the arm is actually wrong per §1.12's own analysis.

**Scope note**: this spike, like the ROADMAP item, is scoped to `Plurality::CardinalWords`
(`#var`, the spelled form — the only place "minus one" is spelled as a word at all).
`Plurality::CardinalDigits` (`$var`, `src/lib.rs:727-730`) has the same `s != "1"` shape and would
technically have the identical ambiguity ("-1 items" vs "-1 item"), but nothing in the architecture
review or the ROADMAP item discusses it, digit renderings read less like a grammatical claim to a
native reader than a spelled-out sentence does, and extending the fix there is a separate,
unscoped decision — left out of this proposal, not silently assumed either way.

## Is this a breaking change to pinned test output? Yes, to two assertions.

`grep -rn "minus one" tests/ CHANGELOG.md docs/` turns up two pinned assertions that the proposed
gate above would flip, both in `tests/ranting/numeral.rs`:

1. `a_negative_words_numeral_spells_the_sign_as_a_word` (line 356-375), on `Noun::new("boot",
   "it")` — a plain countable noun, `is_mass()` defaults `false`:
   - `say!("I see {#0 boot}", -1)` — pinned `"I see minus one boots"` (line 359) → would become
     `"I see minus one boot"`.
   - `say!("{#0 boot}", -1)` — pinned `"Minus one boots"` (line 374) → would become `"Minus one
     boot"`.
   - The comment at lines 363-365 ("The plural on 'minus one boots' is deliberate... Do not 'fix'
     it by sniffing the word") would need rewriting — not because sniffing the word is now correct
     (it still isn't; the fix reads `count`, not the rendered string, exactly as the existing
     comment insists it must), but because the *conclusion* changes for a non-mass noun.
2. `a_negative_numeral_reaches_the_hook_as_one_replaceable_string` (line 377-389), on a
   `RussianNoun` fixture whose `inflect()` (line 56-71) picks `self.plural` iff `to_plural` (i.e.
   `as_pl`) is true, and which has no `is_mass()` override (defaults `false`):
   - `say!("есть {#0 1}", -1, stol)` — pinned `"есть minus one стола"` (line 383, the plural
     genitive form) → would become `"есть minus one стол"` (the singular `self.singular` field).

Both are genuine, intentional pins written *for* the current behavior — §1.12's own text
acknowledges the current output is "right for measures" and only flags countables as wrong, and
`boot`/`RussianNoun`'s `стол` are both ordinary countable nouns, so both tests currently assert the
behavior this item proposes to change. Landing the gate as scoped above means editing both pinned
assertions in the same change (to `"I see minus one boot"`, `"Minus one boot"`,
`"есть minus one стол"`) and rewriting the "deliberate" comment. That is a test-output change, not
a public-API break — no signature moves, no new trait method requirement, `is_mass()` already
exists and already defaults `false` — but it is real enough that a maintainer should rule on it
explicitly rather than have it arrive as a side effect of an unrelated pass.

## Worth noting: this repurposes `is_mass()` for something adjacent to what it was built for

`is_mass()` (`.claude/rules/pluralization.md`, `tests/ranting/mass_count.rs`) exists to distinguish
genuine mass nouns — "information", "water", "sand" — from count nouns, for the `a`/`an`/`some`
and `much`/`many`/`less`/`fewer` selections `docs/superpowers/specs/2026-08-15-quantifier-determiners.md`
covers. "Degree" (the architecture review's own example, "minus one degrees") is not a mass noun
in the linguistic sense — a caller would write `say!("It is {#0 degree}.", n)` and expect ordinary
count-noun behavior (`"It is one degree."`, `"It is two degrees."`), yet under this proposal the
*only* way to get idiomatic "minus one degrees" out of the crate would be for the caller to also
declare `degree` mass (`#[ranting(mass)]` or `Noun::new("degree", "it").with_mass()`), which is
false for every other purpose that flag serves — a mass-flagged "degree" would also start picking
`some`/`much`/`less` instead of `a`/`many`/`fewer` at every other placeholder in the same template,
which is not what a caller reaching for "minus one degrees" is asking for.

Put plainly: `is_mass()` is a serviceable *proxy* for "does this negative-one count feel like a
measure or a discrete thing", not a semantically clean fit — it happens to be the only signal the
crate has, which is exactly why §1.12 called out the mass/count split as the prerequisite rather
than inventing a new flag. A maintainer choosing to land this should decide whether that proxy
fit is close enough, or whether "minus one degrees" is better served by the caller writing
`{#0 degree}` differently (e.g. accepting "minus one degree" as also acceptable colloquial English,
sidestepping the tension entirely) rather than by reusing `is_mass()`. This spike does not resolve
that tension; it surfaces it as the one soft spot in an otherwise mechanical change.

## Test cases worth adding, if this lands

Beyond updating the two flipped assertions above, this exercises a corner `tests/ranting/
mass_count.rs` does not currently touch (its existing cases are all non-negative):

- A genuinely mass-flagged noun (`Noun::new("degree", "it").with_mass()`, or reusing the existing
  `information`-style fixture) at `count == -1`: `say!("It is {#0 degree}.", -1)` should still
  render the plural ("minus one degrees") — the case the fix must *preserve*, not just the case it
  changes.
- The boundary just past magnitude one, on the same noun, to pin that the narrowing really is
  scoped to `Some(-1)` alone: `count == -2` on both a mass and a non-mass noun should render
  plural unchanged either way ("minus two items", "minus two degrees").
- `count == 0` and `count == 1` on both noun kinds, to pin that this arm's two already-settled
  cases (`0` plural, `1` singular) are untouched by the new match arm — regression coverage for
  the "leave everything else alone" claim above, not new behavior.
- If `CardinalDigits` (`$var`) is ever brought into scope by a later decision, the equivalent
  digit-form cases (`$0` with `n = -1`) on the same two noun kinds — deliberately not proposed as
  part of *this* item, per the scope note above, but worth flagging so the digit path doesn't
  silently diverge from whatever the word path decides.

## Recommendation

Land the narrowed match arm shown above — `Some(1) => false, Some(-1) => noun.is_mass(), _ =>
true` — scoped to `Plurality::CardinalWords` only, in the same change as: updating the two pinned
`tests/ranting/numeral.rs` assertions and their explanatory comment, and adding the mass/non-mass
`-1` pair (plus the `-2`/`0`/`1` boundary cases) to `tests/ranting/mass_count.rs` or
`tests/ranting/numeral.rs`. Left open for a maintainer's ruling, since this spike changes no code:

1. **Whether flipping the two pinned assertions is acceptable** — both are intentional pins of the
   behavior this item exists to change, not incidental collateral.
2. **Whether reusing `is_mass()` as the discriminator is the right long-term shape**, given the
   "degree" tension above, versus leaving §1.12 open until a narrower flag (or accepting "minus
   one degree" as also idiomatic) removes the tension entirely.
3. **Whether `CardinalDigits` (`$var`) should move in lockstep or stay explicitly out of scope** —
   this spike recommends leaving it alone for now, but that is a choice, not a default.

## What stays unchanged under this recommendation, until implemented

- `say!("I see {#0 boot}", -1)` keeps rendering `"I see minus one boots"` — today's pinned
  behavior, right for measures and wrong for countables per §1.12, unchanged until a maintainer
  rules and the code lands.
- `Plurality::CardinalDigits` (`$var`) keeps its existing `s != "1"` rule untouched regardless of
  what happens to `CardinalWords` — no code change proposed for it here.
- `AAnSome`/`MuchMany`'s own `is_mass()` gating (`src/lib.rs:423`, and the quantifier-determiners
  spike) is untouched; this item does not add a new hook parameter or change what either arm reads.

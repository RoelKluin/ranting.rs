# Nouns that collide with the closed pre-noun vocabulary

**Kind:** gap — actionable  
**Distinct words:** 10  
**Corpus occurrences:** 73

## Cause

`ranting_core::grammar::PH_EXT`'s `pre` group accepts a closed set of articles plus modal/auxiliary verbs (`can`, `may`, `shall`, `will`, `are`, `were`, `have`, `had`, `do`, `could`, `would`, `should`, `must`, `might`, each optionally with `n't`). Some of those words are also ordinary nouns. When one is the placeholder's noun *and* an article is inside the placeholder, the pre-word reading wins and consumes a token.

## Why it fails

`say!("{The can can} hold water.")` renders "Can can hold water." -- the article is gone. The three neighbouring shapes are all correct: `"A {can can} hold water."` (article outside the placeholder), `{The +can can}`, and `{=can can}`. So this is not "`can` is broken", it is one specific shape, and the failure is silent output corruption rather than a compile error. Note the two columns below are *parses*, not rendered strings: the render is only obtainable by compiling, and the one verified render is quoted here. Read the case table with the `heuristic` label in mind: the *parse* is certain, but whether a given word is really used as a noun rests on the determiner cue, which over-fires on prose that quotes grammar words. `can`, `will` and `must` are genuine English nouns; check the quoted sentences before trusting the rest.

## What ranting needs

Documentation, not code -- checked by compiling, not assumed. `say!("{The *can can} hold water.")` renders "The can can hold water.", so the `*` marker already fixes every case in the table below. What is missing is any way to find that out: README.md's only `*` example is `"A {*can can} contain water."`, which puts the article *outside* the placeholder -- and that shape renders correctly *without* `*`, so the example demonstrates the marker in the one position where it changes nothing. A reader hitting the real failure has no reason to reach for `*`, and no error message to prompt them; the output is silently wrong. Fix: change the README example to `{The *can can}` and say what `*` is for -- disambiguating a noun that is also in the closed pre-word vocabulary. Leaving the parser alone is the right call here: preferring the token-consuming reading would change how every existing template parses, to buy what one documented character already buys.

## Cases, most frequent first

| Word | ranting renders | English requires | Occurrences | Confidence |
|---|---|---|---|---|
| `are` | `pre="are " noun="hold" post=""` | `pre="The " noun="are" post=" hold"` | 32 | heuristic |
| `can` | `pre="can " noun="hold" post=""` | `pre="The " noun="can" post=" hold"` | 11 | heuristic |
| `would` | `pre="would " noun="hold" post=""` | `pre="The " noun="would" post=" hold"` | 7 | heuristic |
| `have` | `pre="have " noun="hold" post=""` | `pre="The " noun="have" post=" hold"` | 6 | heuristic |
| `were` | `pre="were " noun="hold" post=""` | `pre="The " noun="were" post=" hold"` | 6 | heuristic |
| `should` | `pre="should " noun="hold" post=""` | `pre="The " noun="should" post=" hold"` | 3 | heuristic |
| `can't` | `pre="can't " noun="hold" post=""` | `pre="The " noun="can't" post=" hold"` | 2 | heuristic |
| `do` | `pre="do " noun="hold" post=""` | `pre="The " noun="do" post=" hold"` | 2 | heuristic |
| `must` | `pre="must " noun="hold" post=""` | `pre="The " noun="must" post=" hold"` | 2 | heuristic |
| `will` | `pre="will " noun="hold" post=""` | `pre="The " noun="will" post=" hold"` | 2 | heuristic |

## Evidence

### `are`

- README.md:29 — say!("{=who do} say { who title are} {who}.")
- README.md:64 — say!("{=jordan are} a talented engineer.
- README.md:65 — "They are a talented engineer.

### `can`

- README.md:21 — - A say!() macro produces a String similar to format!() , but with placeholder markers a pronouns can be
- README.md:73 — You can use derive(Ranting) on a struct or enum for similar
- README.md:112 — - A given Ranting Enum or Struct can also be inflected to plural or singular.

### `would`

- README.md:135 — case-marks the placeholder exactly as the bare marker would (an inflect article custom
- README.md:254 — - Two placeholders directly adjacent, with no text at all between them ( {a}{b} ), is a compile-time error — there would be no way to kno...
- README.md:284 — split {a}的{b} out of 我的剑 finds a split rather than the intended one, which would trade a

### `have`

- README.md:59 — say!("{=alex have} shared { alex} pronouns:
- README.md:60 — "They have shared their pronouns:
- README.md:70 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.

### `were`

- docs/API.md:371 — A placeholder writes its number two ways and both were hard-coded English:
- docs/EXTENSIBILITY.md:800 — A placeholder can write its number two ways, and before this hook both were
- docs/EXTENSIBILITY.md:894 — count they were handed, None included.

### `should`

- README.md:193 — Not for error handling, because true errors should be easy to search in code.
- README.md:215 — uc false - whether the name should always start with uppercase (advanced)
- docs/COOKBOOK.md:91 — User bios should respect the person's pronouns and be grammatically correct in one pass.

### `can't`

- README.md:153 — Vec T / Option T can't implement Ranting directly — the trait requires Display , and Rust's
- docs/API.md:391 — Vec T and Option T can't implement Ranting directly — the trait
- docs/CHEATSHEET.md:140 — Result String, String = nay!("{=p can't} get in { p} house.");

### `do`

- README.md:29 — say!("{=who do} say { who title are} {who}.")
- README.md:37 — "I do say my name is Jane.".to string()
- README.md:70 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.

### `must`

- README.md:184 — The noun must carry a case marker ( {el =0} , not {el 0} ) —
- README.md:196 — if "$", the struct must contain a subject:
- README.md:197 — if "$", the struct must contain a name:

### `will`

- README.md:170 — application needs one template per language, and no inflection hook will ever change that.
- README.md:260 — They ship no word segmenter and will not gain one.
- README.md:278 — way it is normally written, heed!() will simply not match.


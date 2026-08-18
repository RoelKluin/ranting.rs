# Nouns that collide with the closed pre-noun vocabulary

**Kind:** gap — actionable  
**Distinct words:** 20  
**Corpus occurrences:** 218

## Cause

`ranting_core::grammar::PH_EXT`'s `pre` group accepts a closed set of articles plus modal/auxiliary verbs (`can`, `may`, `shall`, `will`, `are`, `were`, `have`, `had`, `do`, `could`, `would`, `should`, `must`, `might`, each optionally with `n't`). Some of those words are also ordinary nouns. When one is the placeholder's noun *and* an article is inside the placeholder, the pre-word reading wins and consumes a token.

## Why it fails

`say!("{The can can} hold water.")` renders "Can can hold water." -- the article is gone. The three neighbouring shapes are all correct: `"A {can can} hold water."` (article outside the placeholder), `{The +can can}`, and `{=can can}`. So this is not "`can` is broken", it is one specific shape, and the failure is silent output corruption rather than a compile error. Note the two columns below are *parses*, not rendered strings: the render is only obtainable by compiling, and the one verified render is quoted here. Read the case table with the `heuristic` label in mind: the *parse* is certain, but whether a given word is really used as a noun rests on the determiner cue, which over-fires on prose that quotes grammar words. `can`, `will` and `must` are genuine English nouns; check the quoted sentences before trusting the rest.

## What ranting needs

Documentation, not code -- checked by compiling, not assumed. `say!("{The *can can} hold water.")` renders "The can can hold water.", so the `*` marker already fixes every case in the table below. What is missing is any way to find that out: README.md's only `*` example is `"A {*can can} contain water."`, which puts the article *outside* the placeholder -- and that shape renders correctly *without* `*`, so the example demonstrates the marker in the one position where it changes nothing. A reader hitting the real failure has no reason to reach for `*`, and no error message to prompt them; the output is silently wrong. Fix: change the README example to `{The *can can}` and say what `*` is for -- disambiguating a noun that is also in the closed pre-word vocabulary. Leaving the parser alone is the right call here: preferring the token-consuming reading would change how every existing template parses, to buy what one documented character already buys.

## Cases, most frequent first

| Word | ranting renders | English requires | Occurrences | Confidence |
|---|---|---|---|---|
| `are` | `pre="are " noun="hold" post=""` | `pre="The " noun="are" post=" hold"` | 49 | heuristic |
| `no` | `pre="no " noun="hold" post=""` | `pre="The " noun="no" post=" hold"` | 26 | heuristic |
| `can` | `pre="can " noun="hold" post=""` | `pre="The " noun="can" post=" hold"` | 23 | heuristic |
| `would` | `pre="would " noun="hold" post=""` | `pre="The " noun="would" post=" hold"` | 18 | heuristic |
| `either` | `pre="either " noun="hold" post=""` | `pre="The " noun="either" post=" hold"` | 13 | heuristic |
| `all` | `pre="all " noun="hold" post=""` | `pre="The " noun="all" post=" hold"` | 12 | heuristic |
| `were` | `pre="were " noun="hold" post=""` | `pre="The " noun="were" post=" hold"` | 11 | heuristic |
| `each` | `pre="each " noun="hold" post=""` | `pre="The " noun="each" post=" hold"` | 10 | heuristic |
| `every` | `pre="every " noun="hold" post=""` | `pre="The " noun="every" post=" hold"` | 9 | heuristic |
| `many` | `pre="many " noun="hold" post=""` | `pre="The " noun="many" post=" hold"` | 8 | heuristic |
| `have` | `pre="have " noun="hold" post=""` | `pre="The " noun="have" post=" hold"` | 6 | heuristic |
| `less` | `pre="less " noun="hold" post=""` | `pre="The " noun="less" post=" hold"` | 6 | heuristic |
| `should` | `pre="should " noun="hold" post=""` | `pre="The " noun="should" post=" hold"` | 6 | heuristic |
| `much` | `pre="much " noun="hold" post=""` | `pre="The " noun="much" post=" hold"` | 4 | heuristic |
| `must` | `pre="must " noun="hold" post=""` | `pre="The " noun="must" post=" hold"` | 4 | heuristic |
| `could` | `pre="could " noun="hold" post=""` | `pre="The " noun="could" post=" hold"` | 3 | heuristic |
| `do` | `pre="do " noun="hold" post=""` | `pre="The " noun="do" post=" hold"` | 3 | heuristic |
| `had` | `pre="had " noun="hold" post=""` | `pre="The " noun="had" post=" hold"` | 3 | heuristic |
| `can't` | `pre="can't " noun="hold" post=""` | `pre="The " noun="can't" post=" hold"` | 2 | heuristic |
| `will` | `pre="will " noun="hold" post=""` | `pre="The " noun="will" post=" hold"` | 2 | heuristic |

## Evidence

### `are`

- README.md:10 — (Tutorial, Cookbook and Cheatsheet are also available en español (docs/es/TUTORIAL.md).)
- README.md:22 — ranting core and ranting derive are internal crates ranting itself
- README.md:23 — The rest are downstream crates that depend on published ranting alone, each built to

### `no`

- README.md:157 — Degree needs no subject/number agreement, so it's resolved once at compile time from an irregular
- README.md:180 — Vec doesn't hold exactly one item (zero items included — "there are no items", not "there
- README.md:181 — is no item"), and delegates plurality/pronoun/custom-hook behavior straight through to the

### `can`

- README.md:45 — docs/EXTENSIBILITY.md for what a non-English fork can and can't reach through the public API.
- README.md:51 — - A say!() macro produces a String similar to format!() , but with placeholder markers a pronouns can be
- README.md:103 — You can use derive(Ranting) on a struct or enum for similar

### `would`

- README.md:162 — Needed when a word placed before the noun would otherwise be misparsed as an article:
- README.md:169 — case-marks the placeholder exactly as the bare marker would (an inflect article custom
- README.md:242 — plural wants ( Party → Partys , where the rules would say Parties ).

### `either`

- README.md:236 — Writing either one instead declares your own rule,
- docs/API.md:135 — Either one alone is enough;
- docs/API.md:384 — English output is byte-identical either way.

### `all`

- README.md:101 — All pronouns inflect correctly:
- README.md:293 — - Two placeholders directly adjacent, with no text at all between them ( {a}{b} ), is a compile-time error — there would be no way to kno...
- README.md:299 — only — all three share one template compiler.

### `were`

- docs/API.md:411 — A placeholder writes its number two ways and both were hard-coded English:
- docs/EXTENSIBILITY.md:819 — A placeholder can write its number two ways, and before this hook both were
- docs/EXTENSIBILITY.md:923 — count they were handed, None included.

### `each`

- README.md:21 — This repository is not a Cargo workspace — each crate below has its own Cargo.toml / Cargo.lock
- README.md:23 — The rest are downstream crates that depend on published ranting alone, each built to
- README.md:44 — See .claude/rules/crate-layout.md for the full rationale behind each crate's role, and

### `every`

- README.md:177 — Ranting — delegates every method straight through to the boxed value.
- README.md:302 — - Every boundary between a template's segments — literal-to-capture, capture-to-literal,
- docs/API.md:5 — every

### `many`

- README.md:178 — Many T (wraps Vec T , T:
- README.md:182 — An empty Many skips its article rather than leaving
- README.md:189 — Vec / Option types regardless of T — hence the Many / Maybe wrapper types ( Box has no such

### `have`

- README.md:89 — say!("{=alex have} shared { alex} pronouns:
- README.md:90 — "They have shared their pronouns:
- README.md:100 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.

### `less`

- docs/EXTENSIBILITY.md:649 — self -less free function inflect noun irregular and is not routed through any hook.
- docs/EXTENSIBILITY.md:1506 — article slot, and the much / many / less / fewer quantifier pair below.
- docs/EXTENSIBILITY.md:1532 — less / fewer "less information" "fewer items" is mass() , not number

### `should`

- README.md:227 — Not for error handling, because true errors should be easy to search in code.
- README.md:254 — uc false - whether the name should always start with uppercase (advanced)
- docs/COOKBOOK.md:93 — User bios should respect the person's pronouns and be grammatically correct in one pass.

### `much`

- docs/EXTENSIBILITY.md:1506 — article slot, and the much / many / less / fewer quantifier pair below.
- docs/EXTENSIBILITY.md:1531 — much / many "much information" "many items" is mass() , not number
- docs/EXTENSIBILITY.md:1531 — much / many "much information" "many items" is mass() , not number

### `must`

- README.md:218 — The noun must carry a case marker ( {el =0} , not {el 0} ) —
- README.md:230 — if "$", the struct must contain a subject:
- README.md:231 — if "$", the struct must contain a name:

### `could`

- docs/EXTENSIBILITY.md:601 — exactly one item, and otherwise decline (there is no single entity whose gender could agree).
- docs/EXTENSIBILITY.md:1467 — was the best any fork could do.
- docs/EXTENSIBILITY.md:1518 — mass article, already in the closed vocabulary, that a plain a/an guess could never reach.

### `do`

- README.md:59 — say!("{=who do} say { who title are} {who}.")
- README.md:67 — "I do say my name is Jane.".to string()
- README.md:100 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.

### `had`

- docs/CHEATSHEET.md:111 — Past perfect % {=person %walk} "He had walked" (irregular:
- docs/CHEATSHEET.md:111 — "He had gone")
- docs/EXTENSIBILITY.md:371 — Before this change a non-English template still had to be written with English keywords —

### `can't`

- README.md:28 — adjectives can't reach ranting 's postnominal !
- README.md:45 — docs/EXTENSIBILITY.md for what a non-English fork can and can't reach through the public API.
- README.md:187 — Vec T / Option T can't implement Ranting directly — the trait requires Display , and Rust's

### `will`

- README.md:204 — application needs one template per language, and no inflection hook will ever change that.
- README.md:299 — They ship no word segmenter and will not gain one.
- README.md:317 — way it is normally written, heed!() will simply not match.


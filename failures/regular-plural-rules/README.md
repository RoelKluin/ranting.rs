# No regular English pluralization rules

**Kind:** gap — actionable  
**Distinct words:** 18  
**Corpus occurrences:** 188

## Cause

`Ranting::inflect()`'s regular path appends the `plural_end` attribute, which defaults to `"s"` and is concatenated verbatim. `src/language/plurals.rs` provides only a lookup into the generated `IRREGULAR_PLURALS` table (`data/irregular_plurals.txt`, 63 lines); when a word is absent from it there is no rule to fall back on, only the suffix.

## Why it fails

Every English noun outside the 63-line table whose plural is not formed by bare `-s` renders wrong: `{+fly}` gives "flys", `{+box}` gives "boxs", `{+church}` gives "churchs", `{+knife}` gives "knifes". The failure is silent -- there is no error, just an incorrect word in the output -- and it affects the crate's single most-used feature.

## What ranting needs

Regular orthographic rules in `src/language/plurals.rs`, applied before the append-`plural_end` fallback: consonant + `y` → `ies`; `s`/`x`/`z`/`ch`/`sh` → `es`; the `f`/`fe` → `ves` stems. `ranting_gaps/src/english.rs::regular_plural` is an executable reference implementation with the counterexamples (`day`/`days`, `roof`/`roofs`, `chief`/`chiefs`) already pinned in its tests. The classes that need a lexicon rather than a spelling rule -- `-o` → `oes` for `hero` but `-os` for `piano`, `-us` → `-i` for Latin borrowings but not for `bus` -- stay table entries, which is what the table should be reserved for. Note `Ranting::inflect` takes `to_plural` and the noun form only, so this is a change to the English rules, not to any trait signature; a non-English fork already overrides the whole path.

## Cases, most frequent first

| Word | ranting renders | English requires | Occurrences | Confidence |
|---|---|---|---|---|
| `entity` | `entitys` | `entities` | 76 | attested |
| `match` | `matchs` | `matches` | 21 | attested |
| `boundary` | `boundarys` | `boundaries` | 16 | attested |
| `class` | `classs` | `classes` | 14 | attested |
| `fix` | `fixs` | `fixes` | 11 | attested |
| `regex` | `regexs` | `regexes` | 10 | attested |
| `dependency` | `dependencys` | `dependencies` | 7 | attested |
| `copy` | `copys` | `copies` | 5 | attested |
| `haus` | `hauss` | `hauses` | 5 | attested |
| `vocabulary` | `vocabularys` | `vocabularies` | 5 | attested |
| `branch` | `branchs` | `branches` | 4 | attested |
| `carry` | `carrys` | `carries` | 2 | attested |
| `category` | `categorys` | `categories` | 2 | attested |
| `directory` | `directorys` | `directories` | 2 | attested |
| `fuzz` | `fuzzs` | `fuzzes` | 2 | attested |
| `half` | `halfs` | `halves` | 2 | attested |
| `priority` | `prioritys` | `priorities` | 2 | attested |
| `property` | `propertys` | `properties` | 2 | attested |

## Evidence

### `entity`

- docs/API.md:128 — An open-ended lexical-gender / noun-class label carried by the entity and
- docs/EXTENSIBILITY.md:113 — stays a property of the entity, while register / dialect / narration person are
- docs/EXTENSIBILITY.md:262 — The noun's own lexical gender / noun class, carried by the entity rather

### `match`

- README.md:191 — so they can be used anywhere an expression is valid (bound to a let , as a match arm's tail value, etc.);
- README.md:246 — - Returns None if the input doesn't match the template.
- README.md:271 — way it is normally written, heed!() will simply not match.

### `boundary`

- README.md:168 — - Word order is a permanent boundary, not a gap :
- README.md:250 — Whitespace is the only word boundary heed!() / ask!() know
- README.md:256 — - Every boundary between a template's segments — literal-to-capture, capture-to-literal,

### `class`

- README.md:130 — say!("{?w !!good} in class", w) → "best in class" .
- README.md:130 — say!("{?w !!good} in class", w) → "best in class" .
- README.md:200 — gender "" - the lexical gender / noun class label, e.g.

### `fix`

- docs/EXTENSIBILITY.md:586 — every English user, the shape of point fix ROADMAP.md Phase 6 item 1 rejected for German word
- docs/EXTENSIBILITY.md:942 — The fix is a fused two-character form of the case marker:
- docs/EXTENSIBILITY.md:1061 — The fix is not a hook.

### `regex`

- README.md:276 — with no whitespace to anchor on, a regex asked to
- docs/API.md:336 — same regex match at sentence start already reads — and bakes it into
- docs/API.md:481 — regex compiled once via OnceLock .

### `dependency`

- docs/architecture-review-2026-08-13.md:35 — dependency of ranting , its own dead code shows as a plain warning, not an
- docs/architecture-review-2026-08-13.md:149 — ahead" — dependency modernization, the typed caps:
- docs/superpowers/plans/2026-08-12-heed-input-parsing-impl.md:9 — Rust, syn / quote / proc-macro2 (already ranting derive dependencies), regex (already a dependency of both crates — no new dependency for...

### `copy`

- docs/API.md:147 — Copy , Eq , Hash , Display , and Default
- docs/API.md:416 — Copy (reusable across multiple say with!() calls).
- docs/EXTENSIBILITY.md:13 — derive(Clone, Copy)

### `haus`

- docs/EXTENSIBILITY.md:478 — der Hund / die Katze / das Haus from one code path.
- docs/EXTENSIBILITY.md:1283 — say!("in {the 0}.", haus) (haus declared dative) → "im Haus." , where an unhandled pair (a
- docs/EXTENSIBILITY.md:1283 — say!("in {the 0}.", haus) (haus declared dative) → "im Haus." , where an unhandled pair (a

### `vocabulary`

- README.md:183 — itself knows no non-English vocabulary;
- docs/EXTENSIBILITY.md:399 — vocabulary — it lives entirely in the match above, which is what keeps languages modular.
- docs/EXTENSIBILITY.md:1094 — match wearing a struct instead of a bare arm list (no reduction, only new vocabulary) or a table

### `branch`

- docs/COOKBOOK.md:60 — // Past branch:
- docs/COOKBOOK.md:64 — // Future branch:
- docs/EXTENSIBILITY.md:93 — for your hook to branch on:

### `carry`

- README.md:125 — - A post-noun word can carry a degree marker to convert it to its comparative or superlative form:
- README.md:184 — The noun must carry a case marker ( {el =0} , not {el 0} ) —
- docs/EXTENSIBILITY.md:251 — the noun must carry a

### `category`

- docs/superpowers/specs/2026-08-13-number-categories.md:5 — English-owned, and add a count channel (not a category channel) to the
- docs/superpowers/specs/2026-08-13-number-categories.md:34 — morphological consequence of the category rather than a separate axis.
- docs/superpowers/specs/2026-08-13-number-categories.md:119 — number category would not reach them even if every other site had it.

### `directory`

- CLAUDE.md:20 — directory).
- CLAUDE.md:21 — inside each sibling crate directory, e.g.
- CLAUDE.md:23 — already do this — its gate dirs / run gate helpers iterate every directory with its own

### `fuzz`

- docs/superpowers/specs/2026-08-13-number-categories.md:432 — A dual/paucal marker in the placeholder grammar ( {2noun} ) Would close the "categorial number with no numeral" case, but adds grammar su...
- docs/superpowers/specs/2026-08-13-preposition-fusion.md:66 — "will x" is a valid pre-noun slot in the fuzz corpus
- docs/superpowers/specs/2026-08-14-language-modularity.md:256 — — the differential fuzz comparing the hand-written parser against the PH EXT

### `half`

- docs/API.md:24 — Returns Option String ( None on no match), joining heed!() in the Option-returning half of the macro family.
- docs/architecture-review-2026-08-13.md:230 — half of the gap.
- docs/superpowers/plans/2026-08-12-heed-input-parsing-impl.md:5 — Add heed!(template, input) , a new expression macro that matches free-form input text against a small template grammar (literal words + n...

### `priority`

- docs/superpowers/plans/2026-08-12-heed-input-parsing-impl.md:810 — Add a new item after item 7 in the "Upcoming Priority Features" list
- docs/superpowers/plans/2026-08-12-trait-extensibility-impl.md:994 — ROADMAP.md (mark Priority 2 complete)
- docs/superpowers/plans/2026-08-12-trait-extensibility-impl.md:1002 — Update ROADMAP.md to mark Priority 2 complete

### `property`

- docs/EXTENSIBILITY.md:113 — stays a property of the entity, while register / dialect / narration person are
- docs/EXTENSIBILITY.md:476 — is a property of the entity , exactly like subject , so that is where it lives.
- docs/EXTENSIBILITY.md:1004 — It is a permanent property


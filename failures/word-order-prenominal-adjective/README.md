# Prenominal adjectives have no placeholder slot (permanent boundary)

**Kind:** boundary — measured, not actionable  
**Distinct words:** 40  
**Corpus occurrences:** 1799

## Cause

The `!`/`!!` degree slot in `PH_EXT`'s `post` group is post-noun only. There is no pre-noun adjective slot, and per `docs/EXTENSIBILITY.md` §2.12 there will not be one: word order belongs to the template, and the template belongs to the caller.

## Why it fails

An English attributive adjective precedes its noun, so it has to be written as literal text outside the placeholder, where it receives no agreement and no degree inflection. This is the same structural mismatch `ranting_i18n` records as its German hole 8 (`der kleine Hund`) -- and the reason `ranting_es` was chosen as the second falsifier, since Spanish's post-nominal `el gato negro` lands exactly where the `!` slot renders. English itself is on the German side of that split, which is easy to lose sight of.

## Why there is nothing to do

Nothing. This is a decision on record, not an unfilled gap, and the count below is evidence about its cost rather than a request. What the count is *for*: if a given application's text is mostly bare noun phrases, the boundary is a footnote; if it is adjective-heavy prose, most of its noun phrases cannot be expressed as a single placeholder at all, and that is worth knowing before adopting the crate rather than after. Treat a high number as an argument about scope, not as a bug report.

## Cases, most frequent first

| Word | ranting renders | English requires | Occurrences | Confidence |
|---|---|---|---|---|
| `same` | `literal text outside the placeholder: `the same {noun}`` | `agreeing with the noun, as `{noun !same}` does post-noun` | 444 | heuristic |
| `own` | `literal text outside the placeholder: `the own {noun}`` | `agreeing with the noun, as `{noun !own}` does post-noun` | 208 | heuristic |
| `new` | `literal text outside the placeholder: `the new {noun}`` | `agreeing with the noun, as `{noun !new}` does post-noun` | 116 | heuristic |
| `english` | `literal text outside the placeholder: `the english {noun}`` | `agreeing with the noun, as `{noun !english}` does post-noun` | 106 | heuristic |
| `first` | `literal text outside the placeholder: `the first {noun}`` | `agreeing with the noun, as `{noun !first}` does post-noun` | 99 | heuristic |
| `other` | `literal text outside the placeholder: `the other {noun}`` | `agreeing with the noun, as `{noun !other}` does post-noun` | 98 | heuristic |
| `numeral` | `literal text outside the placeholder: `the numeral {noun}`` | `agreeing with the noun, as `{noun !numeral}` does post-noun` | 82 | heuristic |
| `literal` | `literal text outside the placeholder: `the literal {noun}`` | `agreeing with the noun, as `{noun !literal}` does post-noun` | 46 | heuristic |
| `whole` | `literal text outside the placeholder: `the whole {noun}`` | `agreeing with the noun, as `{noun !whole}` does post-noun` | 38 | heuristic |
| `five` | `literal text outside the placeholder: `the five {noun}`` | `agreeing with the noun, as `{noun !five}` does post-noun` | 37 | heuristic |
| `real` | `literal text outside the placeholder: `the real {noun}`` | `agreeing with the noun, as `{noun !real}` does post-noun` | 37 | heuristic |
| `single` | `literal text outside the placeholder: `the single {noun}`` | `agreeing with the noun, as `{noun !single}` does post-noun` | 34 | heuristic |
| `full` | `literal text outside the placeholder: `the full {noun}`` | `agreeing with the noun, as `{noun !full}` does post-noun` | 33 | heuristic |
| `last` | `literal text outside the placeholder: `the last {noun}`` | `agreeing with the noun, as `{noun !last}` does post-noun` | 31 | heuristic |
| `derive` | `literal text outside the placeholder: `the derive {noun}`` | `agreeing with the noun, as `{noun !derive}` does post-noun` | 28 | heuristic |
| `public` | `literal text outside the placeholder: `the public {noun}`` | `agreeing with the noun, as `{noun !public}` does post-noun` | 28 | heuristic |
| `ordinal` | `literal text outside the placeholder: `the ordinal {noun}`` | `agreeing with the noun, as `{noun !ordinal}` does post-noun` | 25 | heuristic |
| `plural` | `literal text outside the placeholder: `the plural {noun}`` | `agreeing with the noun, as `{noun !plural}` does post-noun` | 24 | heuristic |
| `next` | `literal text outside the placeholder: `the next {noun}`` | `agreeing with the noun, as `{noun !next}` does post-noun` | 23 | heuristic |
| `ordinary` | `literal text outside the placeholder: `the ordinary {noun}`` | `agreeing with the noun, as `{noun !ordinary}` does post-noun` | 23 | heuristic |
| `adjective` | `literal text outside the placeholder: `the adjective {noun}`` | `agreeing with the noun, as `{noun !adjective}` does post-noun` | 22 | heuristic |
| `dual` | `literal text outside the placeholder: `the dual {noun}`` | `agreeing with the noun, as `{noun !dual}` does post-noun` | 22 | heuristic |
| `boundary` | `literal text outside the placeholder: `the boundary {noun}`` | `agreeing with the noun, as `{noun !boundary}` does post-noun` | 21 | heuristic |
| `actual` | `literal text outside the placeholder: `the actual {noun}`` | `agreeing with the noun, as `{noun !actual}` does post-noun` | 17 | heuristic |
| `identical` | `literal text outside the placeholder: `the identical {noun}`` | `agreeing with the noun, as `{noun !identical}` does post-noun` | 16 | heuristic |
| `old` | `literal text outside the placeholder: `the old {noun}`` | `agreeing with the noun, as `{noun !old}` does post-noun` | 16 | heuristic |
| `arabic` | `literal text outside the placeholder: `the arabic {noun}`` | `agreeing with the noun, as `{noun !arabic}` does post-noun` | 12 | heuristic |
| `auxiliary` | `literal text outside the placeholder: `the auxiliary {noun}`` | `agreeing with the noun, as `{noun !auxiliary}` does post-noun` | 11 | heuristic |
| `table` | `literal text outside the placeholder: `the table {noun}`` | `agreeing with the noun, as `{noun !table}` does post-noun` | 11 | heuristic |
| `external` | `literal text outside the placeholder: `the external {noun}`` | `agreeing with the noun, as `{noun !external}` does post-noun` | 10 | heuristic |
| `capital` | `literal text outside the placeholder: `the capital {noun}`` | `agreeing with the noun, as `{noun !capital}` does post-noun` | 9 | heuristic |
| `subjunctive` | `literal text outside the placeholder: `the subjunctive {noun}`` | `agreeing with the noun, as `{noun !subjunctive}` does post-noun` | 9 | heuristic |
| `vocabulary` | `literal text outside the placeholder: `the vocabulary {noun}`` | `agreeing with the noun, as `{noun !vocabulary}` does post-noun` | 9 | heuristic |
| `grammatical` | `literal text outside the placeholder: `the grammatical {noun}`` | `agreeing with the noun, as `{noun !grammatical}` does post-noun` | 8 | heuristic |
| `natural` | `literal text outside the placeholder: `the natural {noun}`` | `agreeing with the noun, as `{noun !natural}` does post-noun` | 8 | heuristic |
| `passive` | `literal text outside the placeholder: `the passive {noun}`` | `agreeing with the noun, as `{noun !passive}` does post-noun` | 8 | heuristic |
| `runnable` | `literal text outside the placeholder: `the runnable {noun}`` | `agreeing with the noun, as `{noun !runnable}` does post-noun` | 8 | heuristic |
| `signal` | `literal text outside the placeholder: `the signal {noun}`` | `agreeing with the noun, as `{noun !signal}` does post-noun` | 8 | heuristic |
| `free` | `literal text outside the placeholder: `the free {noun}`` | `agreeing with the noun, as `{noun !free}` does post-noun` | 7 | heuristic |
| `main` | `literal text outside the placeholder: `the main {noun}`` | `agreeing with the noun, as `{noun !main}` does post-noun` | 7 | heuristic |

## Evidence

### `same`

- README.md:40 — - ranting es gaps (ranting es gaps/README.md) — the same idea one level down:
- README.md:170 — override still sees the same grammatical role), but keeps displaying the noun's name instead
- README.md:240 — ranting(plural end = "s") is a real opt-out and is not the same as leaving it off:

### `own`

- README.md:21 — This repository is not a Cargo workspace — each crate below has its own Cargo.toml / Cargo.lock
- README.md:22 — and its own cargo test .
- README.md:179 — the items' own names as "a, b and c" ;

### `new`

- README.md:10 — New to Ranting?
- README.md:64 — let title = Noun::new("name", "it");
- README.md:66 — say this(Noun::new("Jane", "I"), &title),

### `english`

- README.md:39 — arbitrary English text and reports what ranting fails to inflect, ranked by corpus frequency.
- README.md:42 — against open-vocabulary English.
- README.md:213 — recognise as an English article is handed to inflect article custom , so a Spanish

### `first`

- README.md:33 — the first non-Indo-European fork, exercising a
- README.md:35 — the first fork whose decisive finding is a
- README.md:112 — Also an article or verb with an uppercase causess an uppercase for the first character.

### `other`

- README.md:144 — Other words within the
- docs/API.md:413 — Other languages need
- docs/CHEATSHEET.md:51 — composes with other markers, e.g.

### `numeral`

- README.md:36 — confirmation rather than a gap, plus a numeral/counter-noun separator defect it surfaced and
- docs/API.md:46 — count is the placeholder's own numeral, None when it wrote none (which is not a count of one), so a language with a third morphological n...
- docs/API.md:68 — elide numeral custom / with context the same, for a rendered numeral and the noun after it (Japanese 一匹の猫) — see Elision ( elision-elide ...

### `literal`

- README.md:291 — {name...} captures greedily (multiple words) up to the next literal word or the end of input;
- README.md:294 — - heed!() doesn't understand say!() 's grammar markers ( = , @ , , ~ , tense markers, articles) — it matches plain input text against lit...
- README.md:304 — {$name} a run of digits, and {name...} runs up to the next whitespace-separated literal.

### `whole`

- README.md:318 — - The supported approach for such input is to capture the unsegmented run whole and segment it
- docs/EXTENSIBILITY.md:415 — is improved is where the error points — at the template literal rather than the whole
- docs/EXTENSIBILITY.md:895 — the one string a returned Some replaces whole.

### `five`

- docs/EXTENSIBILITY.md:324 — from the five placeholder markers ( = , @ , , ~ , % ) plus the markerless Name / Hidden
- docs/EXTENSIBILITY.md:325 — they answer which of five English-shaped display forms did this placeholder marker
- docs/EXTENSIBILITY.md:333 — accusative, dative, genitive) and English's five markers are different taxonomies that cross-cut

### `real`

- README.md:41 — ranting es 's closed Spanish lexicon against real Spanish text instead of inspecting ranting
- README.md:172 — always returns a real pronoun still get a case-correct article with the name shown, without a
- README.md:240 — ranting(plural end = "s") is a real opt-out and is not the same as leaving it off:

### `single`

- README.md:100 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.
- README.md:182 — single item when there is exactly one.
- README.md:291 — - {name} captures a single word;

### `full`

- README.md:10 — Keep the Cheatsheet (docs/CHEATSHEET.md) open while you write, or browse the API Reference (docs/API.md) for the full public surface.
- README.md:44 — See .claude/rules/crate-layout.md for the full rationale behind each crate's role, and
- docs/API.md:6 — public item here has full rustdoc (including runnable examples) there.

### `last`

- docs/EXTENSIBILITY.md:780 — any article whose last character is multibyte — Arabic ال , Cyrillic этот , Greek τό alike —
- docs/architecture-review-2026-08-14.md:137 — the post-assembly elision splice sliced mid-codepoint whenever the rendered article's last
- docs/architecture-review-2026-08-14.md:169 — / Heed /trait-object helpers ranting derive/README.md Last touched 2023-02-22;

### `derive`

- README.md:8 — This library provides Ranting (https://docs.rs/ranting/1.3.1/ranting/trait.Ranting.html), a trait for inflection within say!() (https://d...
- README.md:8 — This library provides Ranting (https://docs.rs/ranting/1.3.1/ranting/trait.Ranting.html), a trait for inflection within say!() (https://d...
- README.md:22 — ranting core and ranting derive are internal crates ranting itself

### `public`

- README.md:10 — Keep the Cheatsheet (docs/CHEATSHEET.md) open while you write, or browse the API Reference (docs/API.md) for the full public surface.
- README.md:24 — falsify (or, for the two dev tools, inspect) the claim that ranting 's public API gives a
- README.md:45 — docs/EXTENSIBILITY.md for what a non-English fork can and can't reach through the public API.

### `ordinal`

- docs/superpowers/specs/2026-08-15-ordinal-numerals.md:1 — an ordinal channel for the numeral slot
- docs/superpowers/specs/2026-08-15-ordinal-numerals.md:4 — var , baked as a new NumeralKind::Ordinal and mirrored into a new public
- docs/superpowers/specs/2026-08-15-ordinal-numerals.md:5 — NumeralStyle::Ordinal variant — not implemented by this document .

### `plural`

- README.md:52 — A verb alongside, always specified in plural, inflects accordingly.
- README.md:100 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.
- README.md:142 — - A given Ranting Enum or Struct can also be inflected to plural or singular.

### `next`

- README.md:291 — {name...} captures greedily (multiple words) up to the next literal word or the end of input;
- README.md:293 — - Two placeholders directly adjacent, with no text at all between them ( {a}{b} ), is a compile-time error — there would be no way to kno...
- README.md:304 — {$name} a run of digits, and {name...} runs up to the next whitespace-separated literal.

### `ordinary`

- README.md:309 — "{item}、取る" ), since a literal that mixes punctuation with word characters is an ordinary
- docs/EXTENSIBILITY.md:411 — parse runs only when the ordinary English parse fails , and an unmarked two-word placeholder
- docs/EXTENSIBILITY.md:450 — Which label a given entity carries is ordinary per-value data,

### `adjective`

- README.md:31 — slot renders, so this is the crate that exercises adjective agreement with genuinely correct
- README.md:152 — ~ - adjective
- docs/API.md:66 — inflect adjective custom / with context the post-noun !

### `dual`

- README.md:34 — third morphological number (the dual) and preposition/article fusion.
- docs/EXTENSIBILITY.md:868 — bool alone — so Arabic dual, Slavic paucal
- docs/EXTENSIBILITY.md:1398 — morphological number — Arabic's dual, a Slavic paucal — the placeholder's own numeral is what

### `boundary`

- README.md:202 — - Word order is a permanent boundary, not a gap :
- README.md:296 — Whitespace is the only word boundary heed!() / ask!() know
- README.md:302 — - Every boundary between a template's segments — literal-to-capture, capture-to-literal,

### `actual`

- docs/EXTENSIBILITY.md:1920 — - The actual output you got
- docs/architecture-review-2026-08-13.md:30 — actual noun-inflection path ( Noun::inflect() doesn't use them).
- docs/architecture-review-2026-08-13.md:78 — actual diff against the fixed baseline showed all 12 build and pass tests

### `identical`

- docs/API.md:20 — Without a context, output is identical to say!() .
- docs/API.md:342 — identical signature bar its first parameter ( numeral rather than article )
- docs/API.md:343 — and the identical contract.

### `old`

- docs/EXTENSIBILITY.md:873 — the old test
- docs/EXTENSIBILITY.md:937 — exactly the old hard-coded check ( ranting core::grammar::is first person subject ), so English
- docs/EXTENSIBILITY.md:965 — declining language that wants "Der Hund bellt." , the only way to reach that with the old grammar

### `arabic`

- README.md:33 — - ranting ar (ranting ar/README.md) — Arabic;
- docs/API.md:272 — capitalizes every noun wherever it stands, Japanese/Chinese/Arabic/Hebrew have
- docs/EXTENSIBILITY.md:641 — Japanese, Chinese, Arabic and Hebrew have no letter case, so uc is meaningless and the honest

### `auxiliary`

- docs/COOKBOOK.md:140 — Describe actions in different tenses with correct auxiliary verbs.
- docs/TUTORIAL.md:136 — - Runtime auxiliary insertion :
- docs/TUTORIAL.md:136 — The ranting crate inserts the correct auxiliary verb ("is", "have", "had", "will").

### `table`

- README.md:158 — table ( data/irregular adjectives.txt ) plus regular -er / -est (or periphrastic more / most for
- README.md:235 — bookshelf → bookshelves , mother-in-law → mothers-in-law — after the irregular table in
- docs/API.md:160 — gender table keyed by the display string (which breaks on homographs, names,

### `external`

- docs/API.md:159 — non-English implementation can pick der / die / das without an external
- docs/superpowers/specs/2026-08-13-number-categories.md:223 — grammatical case, number , and register/dialect without an external
- docs/superpowers/specs/2026-08-14-language-modularity.md:324 — user-supplied configuration, or an external lint reading the source.

### `capital`

- docs/EXTENSIBILITY.md:693 — capital can only have come from the hook.
- docs/architecture-review-2026-08-15.md:317 — thing it can — but a numeral is not on that list, so the capital lands on the noun , several
- docs/architecture-review-2026-08-15.md:335 — - var (spelled out) — the numeral is a word, so it should take the capital:

### `subjunctive`

- docs/architecture-review-2026-08-15.md:117 — 1.5 The subjunctive were is rewritten to was , in both persons — ✅ FIXED 2026-08-17
- docs/architecture-review-2026-08-15.md:136 — subjunctive is not recoverable from the verb:
- docs/architecture-review-2026-08-15.md:151 — tests/ranting/subjunctive verbatim.rs (rendering) and ranting derive/src/lib.rs 's

### `vocabulary`

- README.md:217 — itself knows no non-English vocabulary;
- docs/EXTENSIBILITY.md:405 — vocabulary — it lives entirely in the match above, which is what keeps languages modular.
- docs/EXTENSIBILITY.md:1123 — match wearing a struct instead of a bare arm list (no reduction, only new vocabulary) or a table

### `grammatical`

- README.md:170 — override still sees the same grammatical role), but keeps displaying the noun's name instead
- docs/API.md:576 — Core attributes — determine grammatical function:
- docs/CHEATSHEET.md:24 — which grammatical form of the noun to render —

### `natural`

- docs/architecture-review-2026-08-14.md:282 — A Phase 7 spike is the natural home if it is pursued.
- docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md:50 — that's also the natural default here —
- docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md:175 — say!() exists to produce natural

### `passive`

- docs/architecture-review-2026-08-15.md:490 — complex English needs that no placeholder can express (the participle channel and passive voice,
- docs/superpowers/specs/2026-08-15-participle-channel.md:1 — the participle channel — passive voice, future perfect, perfect progressive
- docs/superpowers/specs/2026-08-15-participle-channel.md:4 — spellings — =% / =% (passive), % (future perfect), %= / %= (perfect

### `runnable`

- docs/API.md:6 — public item here has full rustdoc (including runnable examples) there.
- docs/EXTENSIBILITY.md:513 — See tests/ranting/noun class.rs for the runnable version, including the accusative
- docs/EXTENSIBILITY.md:585 — is the runnable version, including a French superlative ( le plus noir ) that uses degree .

### `signal`

- README.md:25 — non-English language enough signal to build a correct implementation on:
- docs/API.md:250 — existing numeral signal made a second PlaceholderCount parameter there
- docs/API.md:280 — sentence start (Phase 6 item 17) is uc 's underlying signal alone, without

### `free`

- docs/EXTENSIBILITY.md:649 — self -less free function inflect noun irregular and is not routed through any hook.
- docs/architecture-review-2026-08-13.md:226 — free:
- docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md:168 — "running" → "run" needs to know whether to undouble, "reading" → "read" doesn't) — solvable, but needs its own reverse rule set and test ...

### `main`

- README.md:63 — fn main() {
- README.md:84 — fn main() {
- README.md:127 — fn main() {


# Prenominal adjectives have no placeholder slot (permanent boundary)

**Kind:** boundary — measured, not actionable  
**Distinct words:** 40  
**Corpus occurrences:** 1250

## Cause

The `!`/`!!` degree slot in `PH_EXT`'s `post` group is post-noun only. There is no pre-noun adjective slot, and per `docs/EXTENSIBILITY.md` §2.12 there will not be one: word order belongs to the template, and the template belongs to the caller.

## Why it fails

An English attributive adjective precedes its noun, so it has to be written as literal text outside the placeholder, where it receives no agreement and no degree inflection. This is the same structural mismatch `ranting_i18n` records as its German hole 8 (`der kleine Hund`) -- and the reason `ranting_es` was chosen as the second falsifier, since Spanish's post-nominal `el gato negro` lands exactly where the `!` slot renders. English itself is on the German side of that split, which is easy to lose sight of.

## Why there is nothing to do

Nothing. This is a decision on record, not an unfilled gap, and the count below is evidence about its cost rather than a request. What the count is *for*: if a given application's text is mostly bare noun phrases, the boundary is a footnote; if it is adjective-heavy prose, most of its noun phrases cannot be expressed as a single placeholder at all, and that is worth knowing before adopting the crate rather than after. Treat a high number as an argument about scope, not as a bug report.

## Cases, most frequent first

| Word | ranting renders | English requires | Occurrences | Confidence |
|---|---|---|---|---|
| `same` | `literal text outside the placeholder: `the same {noun}`` | `agreeing with the noun, as `{noun !same}` does post-noun` | 280 | heuristic |
| `own` | `literal text outside the placeholder: `the own {noun}`` | `agreeing with the noun, as `{noun !own}` does post-noun` | 170 | heuristic |
| `english` | `literal text outside the placeholder: `the english {noun}`` | `agreeing with the noun, as `{noun !english}` does post-noun` | 87 | heuristic |
| `new` | `literal text outside the placeholder: `the new {noun}`` | `agreeing with the noun, as `{noun !new}` does post-noun` | 75 | heuristic |
| `other` | `literal text outside the placeholder: `the other {noun}`` | `agreeing with the noun, as `{noun !other}` does post-noun` | 69 | heuristic |
| `first` | `literal text outside the placeholder: `the first {noun}`` | `agreeing with the noun, as `{noun !first}` does post-noun` | 48 | heuristic |
| `numeral` | `literal text outside the placeholder: `the numeral {noun}`` | `agreeing with the noun, as `{noun !numeral}` does post-noun` | 45 | heuristic |
| `literal` | `literal text outside the placeholder: `the literal {noun}`` | `agreeing with the noun, as `{noun !literal}` does post-noun` | 42 | heuristic |
| `five` | `literal text outside the placeholder: `the five {noun}`` | `agreeing with the noun, as `{noun !five}` does post-noun` | 33 | heuristic |
| `full` | `literal text outside the placeholder: `the full {noun}`` | `agreeing with the noun, as `{noun !full}` does post-noun` | 33 | heuristic |
| `single` | `literal text outside the placeholder: `the single {noun}`` | `agreeing with the noun, as `{noun !single}` does post-noun` | 32 | heuristic |
| `whole` | `literal text outside the placeholder: `the whole {noun}`` | `agreeing with the noun, as `{noun !whole}` does post-noun` | 28 | heuristic |
| `adjective` | `literal text outside the placeholder: `the adjective {noun}`` | `agreeing with the noun, as `{noun !adjective}` does post-noun` | 26 | heuristic |
| `public` | `literal text outside the placeholder: `the public {noun}`` | `agreeing with the noun, as `{noun !public}` does post-noun` | 26 | heuristic |
| `next` | `literal text outside the placeholder: `the next {noun}`` | `agreeing with the noun, as `{noun !next}` does post-noun` | 25 | heuristic |
| `derive` | `literal text outside the placeholder: `the derive {noun}`` | `agreeing with the noun, as `{noun !derive}` does post-noun` | 21 | heuristic |
| `real` | `literal text outside the placeholder: `the real {noun}`` | `agreeing with the noun, as `{noun !real}` does post-noun` | 20 | heuristic |
| `ordinary` | `literal text outside the placeholder: `the ordinary {noun}`` | `agreeing with the noun, as `{noun !ordinary}` does post-noun` | 18 | heuristic |
| `boundary` | `literal text outside the placeholder: `the boundary {noun}`` | `agreeing with the noun, as `{noun !boundary}` does post-noun` | 16 | heuristic |
| `actual` | `literal text outside the placeholder: `the actual {noun}`` | `agreeing with the noun, as `{noun !actual}` does post-noun` | 15 | heuristic |
| `old` | `literal text outside the placeholder: `the old {noun}`` | `agreeing with the noun, as `{noun !old}` does post-noun` | 14 | heuristic |
| `last` | `literal text outside the placeholder: `the last {noun}`` | `agreeing with the noun, as `{noun !last}` does post-noun` | 11 | heuristic |
| `plural` | `literal text outside the placeholder: `the plural {noun}`` | `agreeing with the noun, as `{noun !plural}` does post-noun` | 10 | heuristic |
| `identical` | `literal text outside the placeholder: `the identical {noun}`` | `agreeing with the noun, as `{noun !identical}` does post-noun` | 9 | heuristic |
| `library` | `literal text outside the placeholder: `the library {noun}`` | `agreeing with the noun, as `{noun !library}` does post-noun` | 9 | heuristic |
| `original` | `literal text outside the placeholder: `the original {noun}`` | `agreeing with the noun, as `{noun !original}` does post-noun` | 9 | heuristic |
| `dative` | `literal text outside the placeholder: `the dative {noun}`` | `agreeing with the noun, as `{noun !dative}` does post-noun` | 7 | heuristic |
| `runnable` | `literal text outside the placeholder: `the runnable {noun}`` | `agreeing with the noun, as `{noun !runnable}` does post-noun` | 7 | heuristic |
| `table` | `literal text outside the placeholder: `the table {noun}`` | `agreeing with the noun, as `{noun !table}` does post-noun` | 7 | heuristic |
| `differential` | `literal text outside the placeholder: `the differential {noun}`` | `agreeing with the noun, as `{noun !differential}` does post-noun` | 6 | heuristic |
| `external` | `literal text outside the placeholder: `the external {noun}`` | `agreeing with the noun, as `{noun !external}` does post-noun` | 6 | heuristic |
| `numeric` | `literal text outside the placeholder: `the numeric {noun}`` | `agreeing with the noun, as `{noun !numeric}` does post-noun` | 6 | heuristic |
| `signal` | `literal text outside the placeholder: `the signal {noun}`` | `agreeing with the noun, as `{noun !signal}` does post-noun` | 6 | heuristic |
| `spanish` | `literal text outside the placeholder: `the spanish {noun}`` | `agreeing with the noun, as `{noun !spanish}` does post-noun` | 6 | heuristic |
| `local` | `literal text outside the placeholder: `the local {noun}`` | `agreeing with the noun, as `{noun !local}` does post-noun` | 5 | heuristic |
| `main` | `literal text outside the placeholder: `the main {noun}`` | `agreeing with the noun, as `{noun !main}` does post-noun` | 5 | heuristic |
| `tutorial` | `literal text outside the placeholder: `the tutorial {noun}`` | `agreeing with the noun, as `{noun !tutorial}` does post-noun` | 5 | heuristic |
| `vocabulary` | `literal text outside the placeholder: `the vocabulary {noun}`` | `agreeing with the noun, as `{noun !vocabulary}` does post-noun` | 5 | heuristic |
| `alternative` | `literal text outside the placeholder: `the alternative {noun}`` | `agreeing with the noun, as `{noun !alternative}` does post-noun` | 4 | heuristic |
| `authoritative` | `literal text outside the placeholder: `the authoritative {noun}`` | `agreeing with the noun, as `{noun !authoritative}` does post-noun` | 4 | heuristic |

## Evidence

### `same`

- README.md:136 — override still sees the same grammatical role), but keeps displaying the noun's name instead
- README.md:267 — "{item}, take" matches "sword, take" , and the same holds for non-ASCII
- docs/API.md:22 — nay!(fmt, args...) Expands to Err(say!(fmt, args...)) — same shape as ack!() .

### `own`

- README.md:145 — the items' own names as "a, b and c" ;
- README.md:178 — - A non-English template may write its own article word instead.
- README.md:202 — Setting either one instead declares your own rule,

### `english`

- README.md:179 — recognise as an English article is handed to inflect article custom , so a Spanish
- README.md:180 — implementation can accept el / la / los / las and inflect them like the English keywords:
- README.md:185 — an unmarked two-word placeholder keeps its English "noun + post-noun verb" reading.

### `new`

- README.md:10 — New to Ranting?
- README.md:34 — let title = Noun::new("name", "it");
- README.md:36 — say this(Noun::new("Jane", "I"), &title),

### `other`

- README.md:114 — Other words within the
- docs/API.md:373 — Other languages need
- docs/CHEATSHEET.md:49 — composes with other markers, e.g.

### `first`

- README.md:82 — Also an article or verb with an uppercase causess an uppercase for the first character.
- docs/API.md:54 — is first person subject custom(&self, subject:
- docs/API.md:54 — &str) - bool whether subject counts as first-person defaults to ranting core::grammar::is first person subject ( subject == "I" subject =...

### `numeral`

- docs/API.md:69 — inflect numeral custom / with context how a placeholder's var / $var number is written, keyed by NumeralStyle ( numeralstyle), Grammatica...
- docs/API.md:69 — inflect numeral custom / with context how a placeholder's var / $var number is written, keyed by NumeralStyle ( numeralstyle), Grammatica...
- docs/API.md:71 — The pronoun, article, adjective, elision, preposition-fusion and numeral hooks

### `literal`

- README.md:252 — {name...} captures greedily (multiple words) up to the next literal word or the end of input;
- README.md:255 — - heed!() doesn't understand say!() 's grammar markers ( = , @ , , ~ , tense markers, articles) — it matches plain input text against lit...
- README.md:265 — {$name} a run of digits, and {name...} runs up to the next whitespace-separated literal.

### `five`

- docs/EXTENSIBILITY.md:318 — from the five placeholder markers ( = , @ , , ~ , % ) plus the markerless Name / Hidden
- docs/EXTENSIBILITY.md:319 — they answer which of five English-shaped display forms did this placeholder marker
- docs/EXTENSIBILITY.md:327 — accusative, dative, genitive) and English's five markers are different taxonomies that cross-cut

### `full`

- README.md:10 — Keep the Cheatsheet (docs/CHEATSHEET.md) open while you write, or browse the API Reference (docs/API.md) for the full public surface.
- docs/API.md:6 — public item here has full rustdoc (including runnable examples) there.
- docs/API.md:83 — length as the count" entries for the full history (ROADMAP.md Phase 6 items

### `single`

- README.md:70 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.
- README.md:148 — single item when there is exactly one.
- README.md:252 — - {name} captures a single word;

### `whole`

- README.md:279 — - The supported approach for such input is to capture the unsegmented run whole and segment it
- docs/EXTENSIBILITY.md:409 — is improved is where the error points — at the template literal rather than the whole
- docs/EXTENSIBILITY.md:1295 — Unlike following in §2.7 (which can be a whole rendered phrase

### `adjective`

- README.md:122 — ~ - adjective
- docs/API.md:66 — inflect adjective custom / with context the post-noun !
- docs/API.md:66 — adjective, keyed by AdjectiveDegree ( adjectivedegree), GrammaticalCase and NounClass

### `public`

- README.md:10 — Keep the Cheatsheet (docs/CHEATSHEET.md) open while you write, or browse the API Reference (docs/API.md) for the full public surface.
- docs/API.md:3 — A structured overview of Ranting's public surface — what's exported, what it's
- docs/API.md:6 — public item here has full rustdoc (including runnable examples) there.

### `next`

- README.md:252 — {name...} captures greedily (multiple words) up to the next literal word or the end of input;
- README.md:254 — - Two placeholders directly adjacent, with no text at all between them ( {a}{b} ), is a compile-time error — there would be no way to kno...
- README.md:265 — {$name} a run of digits, and {name...} runs up to the next whitespace-separated literal.

### `derive`

- README.md:8 — This library provides Ranting (https://docs.rs/ranting/0.2.1/ranting/trait.Ranting.html), a trait for inflection within say!() (https://d...
- README.md:8 — This library provides Ranting (https://docs.rs/ranting/0.2.1/ranting/trait.Ranting.html), a trait for inflection within say!() (https://d...
- README.md:26 — use ranting derive::

### `real`

- README.md:138 — always returns a real pronoun still get a case-correct article with the name shown, without a
- README.md:280 — yourself with a real tokenizer:
- docs/CHEATSHEET.md:9 — copy-pasted from real say!() calls, not hand-typed.

### `ordinary`

- README.md:270 — "{item}、取る" ), since a literal that mixes punctuation with word characters is an ordinary
- docs/EXTENSIBILITY.md:405 — parse runs only when the ordinary English parse fails , and an unmarked two-word placeholder
- docs/EXTENSIBILITY.md:444 — Which label a given entity carries is ordinary per-value data,

### `boundary`

- README.md:168 — - Word order is a permanent boundary, not a gap :
- README.md:257 — Whitespace is the only word boundary heed!() / ask!() know
- README.md:263 — - Every boundary between a template's segments — literal-to-capture, capture-to-literal,

### `actual`

- docs/EXTENSIBILITY.md:1672 — - The actual output you got
- docs/architecture-review-2026-08-13.md:30 — actual noun-inflection path ( Noun::inflect() doesn't use them).
- docs/architecture-review-2026-08-13.md:78 — actual diff against the fixed baseline showed all 12 build and pass tests

### `old`

- docs/EXTENSIBILITY.md:854 — the old test
- docs/EXTENSIBILITY.md:908 — exactly the old hard-coded check ( ranting core::grammar::is first person subject ), so English
- docs/EXTENSIBILITY.md:936 — declining language that wants "Der Hund bellt." , the only way to reach that with the old grammar

### `last`

- docs/architecture-review-2026-08-14.md:89 — / Heed /trait-object helpers ranting derive/README.md Last touched 2023-02-22;
- docs/superpowers/plans/2026-08-12-heed-input-parsing-impl.md:283 — if matches!(segments.last(), Some(HeedSegment::Capture( ))) {
- docs/superpowers/plans/2026-08-12-heed-input-parsing-impl.md:716 — be the prior count (202, per the last session's work) plus these 9 = 211.

### `plural`

- README.md:22 — A verb alongside, always specified in plural, inflects accordingly.
- README.md:70 — Singular "they" conjugates as plural in form (they are, they have, they do) while referring to a single individual.
- README.md:112 — - A given Ranting Enum or Struct can also be inflected to plural or singular.

### `identical`

- docs/API.md:20 — Without a context, output is identical to say!() .
- docs/EXTENSIBILITY.md:944 — marker (the article/elision hooks see the identical GrammaticalCase ) but renders the noun's name
- docs/EXTENSIBILITY.md:1184 — None (or no say with!() context at all) means no override in effect — identical

### `library`

- README.md:8 — This library provides Ranting (https://docs.rs/ranting/0.2.1/ranting/trait.Ranting.html), a trait for inflection within say!() (https://d...
- README.md:48 — The library fully supports singular they/them pronouns for individuals who prefer gender-neutral language:
- docs/EXTENSIBILITY.md:1707 — By creating these forks, you help the Ranting ecosystem support more languages and dialects while keeping the core library lean and focus...

### `original`

- docs/EXTENSIBILITY.md:978 — ( =@~% ), falling back to the original single-character class — and ranting core::
- docs/superpowers/plans/2026-08-12-heed-input-parsing-impl.md:821 — not injective (multiple original values render to the same text), so
- docs/superpowers/plans/2026-08-12-trait-extensibility-impl.md:1033 — All tests pass (original tests + 9 new tests = ~226 total)

### `dative`

- docs/EXTENSIBILITY.md:326 — Why German dative/genitive can't be named by case alone.
- docs/EXTENSIBILITY.md:327 — accusative, dative, genitive) and English's five markers are different taxonomies that cross-cut
- docs/EXTENSIBILITY.md:328 — @ ( Objective ) covers both accusative and dative direct/indirect objects, and a

### `runnable`

- docs/API.md:6 — public item here has full rustdoc (including runnable examples) there.
- docs/EXTENSIBILITY.md:507 — See tests/ranting/noun class.rs for the runnable version, including the accusative
- docs/EXTENSIBILITY.md:579 — is the runnable version, including a French superlative ( le plus noir ) that uses degree .

### `table`

- README.md:128 — table ( data/irregular adjectives.txt ) plus regular -er / -est (or periphrastic more / most for
- README.md:201 — bookshelf → bookshelves , mother-in-law → mothers-in-law — after the irregular table in
- docs/API.md:131 — gender table keyed by the display string (which breaks on homographs, names,

### `differential`

- docs/superpowers/specs/2026-08-14-language-modularity.md:256 — — the differential fuzz comparing the hand-written parser against the PH EXT
- docs/superpowers/specs/2026-08-14-language-modularity.md:401 — differential test checks.
- docs/superpowers/specs/2026-08-14-language-modularity.md:405 — reference grammar for the English pass — which is what the differential fuzz was

### `external`

- docs/API.md:130 — non-English implementation can pick der / die / das without an external
- docs/superpowers/specs/2026-08-13-number-categories.md:223 — grammatical case, number , and register/dialect without an external
- docs/superpowers/specs/2026-08-14-language-modularity.md:324 — user-supplied configuration, or an external lint reading the source.

### `numeric`

- README.md:113 — If prependeded by $var or var , plurality of the noun is adapted to the numeric variable var.
- README.md:173 — - If a Noun or numeric plurality has a leading question mark, it is hidden but its inferred inflection does apply.
- README.md:219 — Positional arguments and numeric references are supported, as well as named arguments:

### `signal`

- docs/API.md:220 — existing numeral signal made a second PlaceholderCount parameter there
- docs/API.md:250 — sentence start (Phase 6 item 17) is uc 's underlying signal alone, without
- docs/EXTENSIBILITY.md:648 — sentence start is the first signal alone, computed once

### `spanish`

- README.md:179 — recognise as an English article is handed to inflect article custom , so a Spanish
- docs/API.md:97 — build ecosystem forks (Spanish, pirate, Scottish, etc.
- docs/API.md:314 — the article rendered right after it — German zu + dem → zum , Spanish

### `local`

- docs/API.md:384 — The count is local to the
- docs/API.md:394 — Hence two local newtype wrappers ( Box T has no such problem, since std
- docs/EXTENSIBILITY.md:848 — The count here is local to the numeral.

### `main`

- README.md:33 — fn main() {
- README.md:54 — fn main() {
- README.md:97 — fn main() {

### `tutorial`

- README.md:10 — Start with the Tutorial (docs/TUTORIAL.md) (30-40 min read) or jump to the Cookbook (docs/COOKBOOK.md) (10 practical recipes).
- README.md:10 — Start with the Tutorial (docs/TUTORIAL.md) (30-40 min read) or jump to the Cookbook (docs/COOKBOOK.md) (10 practical recipes).
- docs/API.md:7 — guided introduction, see the Tutorial (TUTORIAL.md);

### `vocabulary`

- README.md:183 — itself knows no non-English vocabulary;
- docs/EXTENSIBILITY.md:399 — vocabulary — it lives entirely in the match above, which is what keeps languages modular.
- docs/EXTENSIBILITY.md:1094 — match wearing a struct instead of a bare arm list (no reduction, only new vocabulary) or a table

### `alternative`

- docs/EXTENSIBILITY.md:893 — alternative count to offer, and Maybe(None) has none at all — so both keep forwarding whatever
- docs/superpowers/specs/2026-08-14-language-modularity.md:391 — alternative in a repeated group is not local to that alternative.
- docs/superpowers/specs/2026-08-14-language-modularity.md:391 — alternative in a repeated group is not local to that alternative.

### `authoritative`

- docs/superpowers/specs/2026-08-13-number-categories.md:4 — bool authoritative and
- docs/superpowers/specs/2026-08-13-number-categories.md:203 — (b) Parallel channel alongside the bool, bool authoritative for English One parameter added to 6 (soon 8) hooks;
- docs/superpowers/specs/2026-08-13-number-categories.md:280 — authoritative for every English fallback path, so say!() 's output is


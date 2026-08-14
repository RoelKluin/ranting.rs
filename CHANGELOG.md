# Changelog

## Unreleased

### Changed (breaking)

- **`Ranting::inflect` takes a fifth parameter,
  `count: Option<PlaceholderCount>`** (ROADMAP.md Phase 7 item 11). Every
  hand-written `Ranting` impl must add it; derive-generated impls are
  regenerated and need no change. **Rendered output is unchanged** — nothing in
  the crate reads the new parameter, and English has no form for it to select.
  - Why: `inflect` renders *the counted noun itself* and was the one call Phase
    6 item 14 did not give a count, so a language with a third morphological
    number could agree in that number everywhere except on the noun. Arabic
    `{$n kitab}` with `n = 2` gave every agreeing hook
    `PlaceholderCount { value: 2, .. }` and gave the noun the plural `kutub`
    rather than the dual `kitābān` — output that looks grammatical and is wrong
    in one word.
  - `None` means the placeholder wrote no numeral, which is **not** the same as
    a count of one. `Many` substitutes its own length when the placeholder
    supplied none, never overriding an explicit numeral.
  - See `docs/EXTENSIBILITY.md` §2.16 and `tests/ranting/third_number.rs`.

### Fixed

- **`{?article noun}` rendered literal garbage** unless the entity's
  `skip_article()` was `true`. The `?` marker (README's "display depends on
  `no_article`", e.g. `say!("{?the 0} was great!", activity)`) was not stripped
  before the word was classified, so `?the` was taken for a pre-noun *verb* and
  conjugated: `say!("{?the dog}")` rendered `"?thes dog"` and `say!("{?a dog}")`
  rendered `"?as dog"`, with no error at compile time or run time. `?the` is now
  simply `the`, which is the documented reading. The `no_article = true` half —
  the only half any test or example exercised, and why this survived — is
  unchanged.

- **Regular English pluralization** (ROADMAP.md Phase 7 item 10). `{+noun}` on a
  noun absent from `data/irregular_plurals.txt` used to append the `plural_end`
  attribute (default `"s"`) verbatim — there were no rules at all — so
  `{+entity}` rendered `"entitys"`, `{+box}` rendered `"boxs"` and
  `{+mother-in-law}` rendered `"mother-in-laws"`. English's regular orthographic
  rules now apply: consonant + `y` → `ies`, `-es` after `s`/`x`/`z`/`ch`/`sh`,
  the `-f`/`-fe` → `-ves` stems (which fire for compounds like `bookshelf`, the
  bare words being table rows already), and head pluralization for hyphenated
  compounds.
  - **This changes rendered output.** A struct that declares `singular_end` or
    `plural_end` is unaffected: declaring either states a rule of its own and
    still gets the literal strip-and-append, which is what keeps a non-English
    impl from acquiring English orthography by accident. What counts is that the
    attribute was *written*, not what value it was given — `plural_end = "s"` is
    a genuine opt-out (bare append, no orthography), which is what a German,
    Dutch or Danish loanword plural needs: `Party` → `Partys`, where the rules
    say `Parties`. Names ending in a consonant + `y` are the class where the two
    paths differ, and the only class the rules made *previously-correct* output
    wrong for.
  - Singularization is deliberately unchanged — no spelling rule separates
    `cities` → `city` from `movies` → `movie`, so `{-cities}` still renders
    `"citie"`.
  - `data/irregular_plurals.txt` gained the `-ch`-as-/k/ words (`stomach`,
    `epoch`, `monarch`, …) the spelling-only rules cannot recognize, plus `bus`.

### Added

- `ranting::inflect_noun_regular`, the public entry point derive-generated
  `inflect()` impls use once the irregular table misses. Its `singular_end`/
  `plural_end` parameters are `Option<&str>`, `None` meaning "no rule declared".
- `Noun::with_plural_end` / `Noun::with_singular_end`, chaining off
  `new`/`try_new` like `with_noun_class`. `Noun` has no `#[ranting(..)]`
  attributes to write, so these are its opt-out from the regular rules.
- `ranting::DeclaredEnding`, the trait `#[ranting(singular_end = "$")]` /
  `#[ranting(plural_end = "$")]` read their field through. A `String` field
  (the documented shape) always counts as declared; an `Option<String>` one can
  additionally say "unset" at runtime, which is how `Noun` keeps the English
  rules for every noun that never calls `with_plural_end`.

## v1.3.0 — Internationalization Foundations

Phase 6's goal was narrow: make a non-English `Ranting` implementation
*buildable* — not build one and ship it. This release lands the signals a
fork needs and could not previously obtain (gender, grammatical case, degree,
orthographic role, numeral style, a length-derived count) as new hooks and
types on `Ranting`, defaulting everywhere to today's English behavior, and
then spends two reference lexicons — German (`ranting_i18n`) and Spanish
(`ranting_es`) — proving the set is close to sufficient. It is not a
translation system: no vocabulary, message catalogue, or word-order engine
shipped in `ranting` itself. Several Phase 6 items were doc-only design
spikes that changed no code at all (see "Deliberately not done" below) —
this release is smaller in code than the number of ROADMAP items suggests.

### Added

**Six new/extended `_custom` hook pairs on `Ranting`** (each hook has an
`_with_context` twin taking an extra `ctx: Option<&NarrationContext>`; all
default to `None`/today's English output, so no existing impl needs to
change to keep compiling and rendering identically):

- `inflect_adjective_custom`/`_with_context` — runtime adjective agreement
  for the `{noun !adj}`/`{noun !!adj}` degree slot (previously resolved
  entirely at compile time). Receives the adjective as written,
  `AdjectiveDegree`, `GrammaticalCase`, `NounClass`, `as_plural`, `count` and
  `uc`.
- `elide_article_custom`/`_with_context` — a post-assembly hook for
  phonological elision/fusion (French `le`+vowel → `l'`), receiving the
  rendered `article`, `separator` and `following` text.
- `inflect_numeral_custom`/`_with_context` — locale-aware numeral rendering
  for `#var`/`$var`, receiving `NumeralStyle` (words vs. digits), `count`,
  case, class and `as_plural`.
- `inflect_preposition_custom`/`_with_context` — fuses a literal pre-noun
  preposition with the rendered article (German `zu`+`dem`→`zum`, Spanish
  `de`+`el`→`del`), fed the preposition text the macro previously discarded.
- `inflect_article_custom`/`_with_context` and `inflect_pronoun_custom`/
  `_with_context` — both extended (not new) with a `class: NounClass`
  parameter, and later a `count` parameter (see Breaking Changes).

Plus a new fallback-taking-over hook, not part of the `_custom`/`None`
convention above since there's nothing to decline into:

- `capitalize`/`capitalize_with_context` — routes every sentence-position
  capitalization decision through an overridable hook (`OrthographyRole` +
  `uc` + `sentence_start`), instead of calling `uc_1st_if` directly at each
  call site.

**New public types**:

- `NounClass` — an open-ended lexical-gender/noun-class label (`Noun` gets
  one via `#[ranting(gender = "...")]`), read by `noun_class()` and threaded
  into the article/pronoun/adjective/preposition hooks. Not a closed
  `enum { Masculine, Feminine, Neuter }` — deliberately, since Bantu
  languages have a dozen-plus classes and Danish has common/neuter.
- `GrammaticalCase` — the noun's grammatical role at a given placeholder
  occurrence (`Name`/`Subjective`/`Objective`/`Possessive`/`Reflexive`),
  mirrored from `ranting_core::placeholder::CaseKind`. Threaded into
  `inflect_article_custom`, `elide_article_custom`, `inflect_preposition_custom`
  and (new in this release) `Ranting::inflect` itself.
- The fused `*=`/`*@`/`` *` ``/`*~`/`*%` marker forms — `*` was already a
  case-marker-position character (meaning "no case marker, but mark this as
  the placeholder's Ranting element"); fused with a real case marker it now
  case-marks the placeholder exactly like the bare marker but renders the
  noun's *name* instead of switching to a pronoun (`display_as_name: bool`
  on `PlaceholderSpec`). Lets a fork whose `inflect_pronoun_custom` always
  returns a real pronoun still get a case-correct article with the name
  shown, for the same entity, in the same sentence a bare `` {@noun} ``
  renders a real pronoun for. See `docs/EXTENSIBILITY.md` §2.11.
- `AdjectiveDegree` — mirrors `ranting_core::placeholder::DegreeKind`
  (`Comparative`/`Superlative`) for `inflect_adjective_custom`.
- `OrthographyRole` — which call site is asking `capitalize` to decide
  (`Article`/`Verb`/`Pronoun`/`Noun`/`Adjective`).
- `NumeralStyle` — `Words` (`#var`) vs. `Digits` (`$var`) for
  `inflect_numeral_custom`.
- `PlaceholderCount` — `{ value: i64, fraction_digits: u32 }`, the count
  channel threaded into five hook pairs (see Breaking Changes).

**Behavioral additions**:

- `is_first_person_subject_custom(&self, subject: &str) -> bool` —
  `narration::resolve_viewpoint`'s first-person check (previously
  hard-coded to `matches!(subject, "I" | "we")`) is now overridable, so a
  fork whose first-person labels are e.g. `ich`/`wir` can make
  `NarrationContext.narration_person` retelling work instead of silently
  no-op'ing.
- `Many<T>` now substitutes its own `Vec`'s length as the `count` for the
  five count-carrying hook pairs when the placeholder itself carried no
  numeral (`count.or_else(|| self.own_count())`) — only at exactly one item;
  an explicit placeholder numeral, and a `Many` of zero or 2+ items, are
  unaffected.
- Sentence detection widened beyond ASCII `.`/`?`/`!`: `PH_START` now also
  recognizes Greek's question mark (U+037E), Urdu's full stop (U+06D4),
  CJK full-width terminators (`。`/`！`/`？`, which take no following space),
  and Spanish's opening `¿`/`¡` (which mark sentence-initial from *before*
  the placeholder). The single source of truth for "is this character a
  sentence trigger" is the new `ranting_core::grammar::SENTENCE_TRIGGER_CHARS`.
- `say_with!()` and `derive_ranting` are now re-exported from `ranting` —
  previously a crate depending on `ranting`'s public API alone could never
  construct a call carrying a `NarrationContext`, making every
  `_with_context` hook unreachable in practice from outside this repo.

### Breaking changes

Every one of the five signature breaks below is additive in *behavior*
(English `say!()`/`say_with!()` output is byte-identical before and after —
verified by the full pre-existing test suite passing unchanged) but is a
**source break for any downstream override** of the named hook. If your
`Ranting` impl overrides any of these methods, it needs updating to match
the new parameter list before it compiles again.

**1. `class: NounClass` added to the article and pronoun hooks:**

```rust
// Before
fn inflect_article_custom(&self, article: &str, noun_singular: &str,
    case: GrammaticalCase, as_plural: bool, uc: bool) -> Option<String>
fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase,
    as_plural: bool, uc: bool) -> Option<String>

// After
fn inflect_article_custom(&self, article: &str, noun_singular: &str,
    case: GrammaticalCase, class: NounClass, as_plural: bool,
    count: Option<PlaceholderCount>, uc: bool) -> Option<String>
fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase,
    class: NounClass, as_plural: bool, count: Option<PlaceholderCount>,
    uc: bool) -> Option<String>
```

(`class` landed first; `count` landed later in the same phase — see below.
Both are shown together since anyone updating an override has to handle
both regardless of which commit they're diffing against.)

**2. `count: Option<PlaceholderCount>` added to five hook pairs, plus
`case: GrammaticalCase` added to `Ranting::inflect` — the "owed" signature
break from the number-category design spike, done once:**

```rust
// Before
fn inflect_verb_custom(&self, subject: &str, verb: &str,
    as_plural: bool, uc: bool) -> Option<String>
fn elide_article_custom(&self, article: &str, separator: &str,
    following: &str, case: GrammaticalCase, class: NounClass,
    as_plural: bool) -> Option<String>
fn inflect_adjective_custom(&self, adjective: &str, degree: AdjectiveDegree,
    case: GrammaticalCase, class: NounClass, as_plural: bool,
    uc: bool) -> Option<String>
fn inflect(&self, to_plural: bool, uc: bool) -> String

// After
fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool,
    count: Option<PlaceholderCount>, uc: bool) -> Option<String>
fn elide_article_custom(&self, article: &str, separator: &str,
    following: &str, case: GrammaticalCase, class: NounClass,
    as_plural: bool, count: Option<PlaceholderCount>) -> Option<String>
fn inflect_adjective_custom(&self, adjective: &str, degree: AdjectiveDegree,
    case: GrammaticalCase, class: NounClass, as_plural: bool,
    count: Option<PlaceholderCount>, uc: bool) -> Option<String>
fn inflect(&self, to_plural: bool, uc: bool, case: GrammaticalCase) -> String
```

(`inflect_article_custom`/`inflect_pronoun_custom` also gained `count` here,
on top of the `class` parameter shown above.) All ten affected methods
(five hooks × the plain form and its `_with_context` twin) changed in one
commit. `inflect_numeral_custom` was deliberately **not** touched by this
break — it already carries its own, differently-typed `count: Option<i64>`
from when it landed.

**3. `sentence_start: bool` added to `capitalize`:**

```rust
// Before
fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool) -> String

// After
fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool,
    sentence_start: bool) -> String
```

`uc` and `sentence_start` can disagree in both directions — `uc` also folds
in the `,`/`^` markers and an uppercase pre-text word, `sentence_start` is
the raw compile-time-known position signal alone.

**Not breaking, for context**: `inflect_adjective_custom`,
`elide_article_custom`, `inflect_numeral_custom`, `inflect_preposition_custom`
and `capitalize` were all themselves *new* in this release (see Added
above) — they only became a compatibility concern for anyone who adopted
them mid-phase. `Many`'s count substitution (item 15) and the fused `*=`
markers (item 19) required no hook signature change at all.

### Deliberately not done

Several Phase 6 items concluded "change nothing, document it" after scoring
concrete alternatives — these are permanent boundaries, not gaps awaiting a
future release:

- **Word order stays in the caller's template.** `ranting` inflects words
  within a template; the order of those words is the template's. No hook
  can move text it doesn't own — German verb-second, Japanese/Korean SOV,
  VSO languages, and suffixed definite articles are all out of reach by
  construction, not by omission. See `docs/EXTENSIBILITY.md` §2.12 and
  `docs/superpowers/specs/2026-08-13-word-order-feasibility.md`.
- **`GrammaticalCase` stays at English's five-marker inventory.** It scopes
  to "which of `say!()`'s five markers did this occurrence use," not to a
  general syntactic-case representation — German's four cases cross-cut
  that split, so no re-slicing of the existing variants recovers a clean
  match, and a fork needing more (dative, genitive) carries the real case
  on the entity instead. See `docs/EXTENSIBILITY.md` §2.3.1 and
  `docs/superpowers/specs/2026-08-13-grammatical-case-inventory.md`.
- **Per-language template selection stays caller-side.** `say!()` parses
  its literal as a `syn::LitStr` at compile time, before any runtime value
  exists, so a runtime catalogue lookup is a compile error, not a slow
  path — selecting a template by language costs languages × sentences of
  source text no matter how it's spelled. See `docs/EXTENSIBILITY.md` §2.12
  and `docs/superpowers/specs/2026-08-13-template-selection.md`.
- **Whitespace stays `heed!()`'s only word boundary**, for `heed!()`,
  `ask!()` and `#[derive(Heed)]` alike, permanently — not an ASCII/Latin
  restriction (it's script-agnostic), but a template whose segments abut
  without whitespace, in any script, returns an honest `None` rather than a
  silently wrong capture. See the README's "Whitespace is the only word
  boundary" section.

### Known gaps (not this release's to close)

The `ranting_i18n` (German) and `ranting_es` (Spanish) falsifier crates each
document, in their own `README.md`, the specific constructions their
language still cannot reach through `ranting`'s public API even after this
release — including German's inability to reach `inflect_adjective_custom`
in a grammatically correct sentence at all (prenominal attributive
adjectives vs. the post-noun-only `!` slot), unreported adjective-declension
class, and (for both languages, closed together by item 26 above) what
remained of preposition-article fusion before this release landed the
`inflect_preposition_custom` hook. Read `ranting_i18n/README.md` and
`ranting_es/README.md` before assuming a construction works — this phase
closes the gaps it names, not every gap a non-English language might hit.

### See also

- `docs/EXTENSIBILITY.md` — the extension-point reference for every hook
  above, with worked examples per language.
- `docs/superpowers/specs/2026-08-13-*.md` — the design spikes behind the
  "Deliberately not done" decisions, each with its scored alternatives.
- `ROADMAP.md` Phase 6 — the full item-by-item implementation log this
  changelog summarizes for a crate user rather than a roadmap reader.

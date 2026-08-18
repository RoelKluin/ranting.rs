# Design spike: derive-generated `inflect()` against non-English input

**Status**: design spike, PROPOSED only — **no code in this repository is changed by this
document**. ROADMAP.md Phase 9 item 3, citing `docs/architecture-review-2026-08-14.md` §4.7 and
`.claude/rules/pluralization.md`'s "Blind spot worth knowing" note. Confirms the current state,
evaluates the item's two named candidate shapes, and recommends one — while stating plainly that
the recommended path is expected to *find a real gap*, not confirm the default sufficient, because
the gap's shape is already visible from reading the source rather than needing to be discovered.

## Confirming the state

Both existing non-English falsifiers hand-roll `impl Ranting for ...` directly, and **neither uses
`#[derive_ranting]` at all**:

```
ranting_i18n/src/noun.rs:139:   impl Ranting for GermanNoun
ranting_i18n/src/person.rs:84:  impl Ranting for GermanPerson
ranting_es/src/noun.rs:74:      impl Ranting for SpanishNoun
ranting_es/src/person.rs:94:    impl Ranting for SpanishPerson
```

This is one notch stronger than §4.7's framing ("both reference lexicons hand-write
`Ranting::inflect`") — it isn't just `inflect()` that's hand-written, the derive macro is never
invoked anywhere in either crate. `GermanNoun::inflect` (`ranting_i18n/src/noun.rs:164`) calls
`self.entry.form(self.case_for(case), to_plural)`, a lookup into `lexicon`'s hand-built table
(`HAUS`/`HUND`/`KATZE`), never `ranting::inflect_noun_regular`. `SpanishNoun::inflect`
(`ranting_es/src/noun.rs:95`, not reproduced here) does the same against its own four-noun table.
So the derive-generated fallback path — `ranting_derive/src/ranting_impl.rs`'s
`get_plurality_fns`, whose four fallback sites call `ranting::inflect_noun_regular` — has **zero**
call sites reachable from either falsifier crate, not merely an untested one. `.claude/rules/
pluralization.md`'s "Blind spot worth knowing" note and §4.7 both describe this correctly; the
"never uses `#[derive_ranting]` at all" detail just makes concrete why: there is no partial
coverage to extend, only a clean absence.

`src/language/plurals.rs`'s `regular_plural_lowercase` (the function `inflect_noun_regular`
reaches once the irregular table misses) is orthographic-only, by design (`.claude/rules/
pluralization.md` point 2): consonant+`y`→`ies`, `-es` after a sibilant (`s`/`x`/`z`/`ch`/`sh`/
`ss`), the `-f`/`-fe`→`-ves` stem lists, else bare append-`s`. Nothing in it, or in the irregular
table it falls back from, knows any language but English.

## What running it against German/Spanish nouns would produce

This was worked out by reading `regular_plural_lowercase` directly rather than by compiling and
running a scratch fixture — the `mktemp`/cargo-in-scratch-directory step this spike would have
used (the same approach `2026-08-17-hook-falsification-depth.md` took for its own throwaway
fixture) required an approval this non-interactive session could not grant, the identical
limitation that spike hit with `scripts/hook_audit.sh`. The function's logic is fully legible from
source, so the predictions below are exact, not estimated — each is a direct trace through
`regular_plural_lowercase`'s match arms, not a guess about German/Spanish morphology:

| Noun | Language, real plural | English rule that fires | Rendered |
|---|---|---|---|
| `Fuchs` | German, `Füchse` (umlaut + `-e`) | ends in `s` → sibilant `-es` | `Fuchses` |
| `Buch` | German, `Bücher` (umlaut + `-er`) | ends in `ch` → sibilant `-es` | `Buches` |
| `voz` | Spanish, `voces` (`z`→`c` + `-es`) | ends in `z` → sibilant `-es` | `vozes` |
| `luz` | Spanish, `luces` (`z`→`c` + `-es`) | ends in `z` → sibilant `-es` | `luzes` |

Every one of these is wrong, and wrong in the way `.claude/rules/pluralization.md`'s own framing
predicts: German plurals routinely need a stem-internal umlaut and/or a non-`-s` ending
(`-e`/`-er`/`-en`), which no suffix-append rule can produce; Spanish `-z`→`-ces` needs a
consonant *change*, not just a suffix, and English's sibilant rule instead appends `-es` onto the
unchanged `-z`. This is exactly the shape `docs/architecture-review-2026-08-14.md` §4.7 already
names as the reason both crates hand-write `inflect()` in the first place ("suffix arithmetic
cannot produce `Füchse` or `voces`") — the spike does not discover a new fact, it exercises a
fact the repo already asserts but has never run a test against.

**This is not an ambiguous case that might go either way.** Every regular-plural rule in
`src/language/plurals.rs` is deliberately orthographic-English-only (see `.claude/rules/
pluralization.md` point 2's own scope statement); German and Spanish were never in view when it
was written. Running it against either language is expected, with near certainty, to surface
mangled output — confirming a known-but-never-pinned defect, not discovering an unknown one.

## The item's two candidate shapes

**(a) A fifth, deliberately minimal falsifier crate that leans on the derive default.**
A new `ranting_xx/` directory, own `Cargo.toml` depending on `ranting` alone (satisfying
`.claude/rules/crate-layout.md`'s falsifier contract as written), using `#[derive_ranting]` on a
handful of structs and writing **no** `inflect()` override and **no** `singular_end`/`plural_end`
attributes for at least one noun, so that noun's plural renders through
`ranting::inflect_noun_regular` for real. Minimal by design — the point isn't a new reference
lexicon with its own README/holes list, it's a narrow fixture whose only job is to run the derived
path against non-English spelling and record what comes out.

**(b) Extend an existing falsifier's lexicon (`ranting_i18n` or `ranting_es`) with entries that go
through the default derive path instead of an override.** Concretely this cannot mean "add a few
nouns to `GermanNoun`/`SpanishNoun` without setting `plural_end`" — those structs are not
`#[derive_ranting]`-generated at all (see "Confirming the state" above), so there is no derive
fallback for a subset of their nouns to opt into; `GermanNoun::inflect`/`SpanishNoun::inflect` are
plain hand-written methods that either consult the lexicon table or don't, with no derive
machinery in the call graph at any point. To reach the derive path from inside an *existing*
falsifier crate, (b) would actually mean introducing a **second, `#[derive_ranting]`-based** noun
type into `ranting_i18n` or `ranting_es`, alongside the existing hand-rolled one — e.g. a
`GermanWordDefault` struct using `#[derive_ranting]` with no `singular_end`/`plural_end` and no
manual `inflect()`, living next to `GermanNoun` in the same crate. That is a structural change
disguised as an extension: it is not "add rows to a table," it is "add a second `Ranting`
implementation strategy to a crate that has used exactly one since it was created."

## Recommendation: (a), a minimal fifth crate — but scoped as a pinned-defect fixture, not a
## reference lexicon

Once (b) is understood correctly (a second impl strategy bolted onto an existing crate, not a
lexicon extension), it no longer reads as the cheaper option. It also sits awkwardly with
`ranting_i18n`'s and `ranting_es`'s own stated identity: both READMEs and `.claude/rules/
crate-layout.md`'s own description frame each existing falsifier as *one coherent implementation
strategy* for its language, and mixing "the derive default, deliberately unfixed" into a crate that
otherwise demonstrates "here is what a real German/Spanish implementation looks like" muddies that
framing for a reader — hole lists and README claims would need to explain why some nouns in
`GermanNoun`-the-family render correctly and others don't, for a reason that has nothing to do with
German grammar.

(a) keeps the falsifier contract clean (own directory, own manifest, `ranting` alone — no exception
needed to `.claude/rules/crate-layout.md`) and keeps the finding legible: a reader opening a new,
small crate whose entire point is "these nouns intentionally use no override" does not need to
disentangle that from four other nouns in the same file that do the opposite for unrelated reasons.

**What it would prove, stated plainly**: not a falsification in the item-1/item-2/Phase-7 sense of
"finds an unknown gap in the hook surface." The expected, near-certain result is **confirming** a
gap the repo already asserts in prose (§4.7, `.claude/rules/pluralization.md`) but has never pinned
as a running test. That is still worth doing — §4.7's own words are "the falsification apparatus is
structurally blind to English orthography leaking through that path," and a spike that turns an
assertion into a pinned, re-runnable regression test closes exactly that blindness, the same way
`tests/ranting/third_number.rs` closed §4.7's *hand-written* half (Phase 7 item 11) without needing
to discover anything new. The value is in the pin, not in the discovery.

**Given that**, this spike further suggests the crate does not need the full weight of a fifth
falsifier (own README, holes-numbered `tests/holes.rs`, the falsifier-contract ceremony every
other one carries) — a maintainer could reasonably choose instead to add a lighter-weight fixture
that stays inside the existing test-only surface (e.g. a `ranting_derive`-facing test in this
repo's own `tests/ranting/` using a struct with a German/Spanish name and no attributes, pinning
the mangled output the way `regular_plurals.rs` pins the English rules) rather than standing up a
sixth directory with its own manifest. That is a real fork in the road this spike surfaces but does
not resolve: **is the goal "close the falsification-apparatus blind spot with the least new
surface" (favors a pinned test in the main crate) or "keep growing the falsifier-crate roster,
consistent with how items 1/2/3/5/6 all preferred a real downstream crate over an in-repo
fixture" (favors (a))?** Both are legitimate under this repo's own precedent; recommending (a) over
(b) is confident, recommending a full crate over an in-repo pinned test is not — that choice is
left to the maintainer.

## What this spike does not do

- Does not create `ranting_xx/`, any new crate manifest, or any new test file, in this repo or
  outside it left committed. The four predicted outputs in the table above were derived by tracing
  `src/language/plurals.rs`'s `regular_plural_lowercase` by hand against known-correct German and
  Spanish plurals, not by compiling and running a scratch fixture — that step was attempted (a
  `mktemp`-rooted scratch crate depending on `ranting` via a path dependency, the same shape
  `2026-08-17-hook-falsification-depth.md` used) and blocked on a sandbox approval this
  non-interactive session could not grant. Nothing was left in this repo's tree either way;
  `git status --porcelain` is unaffected by this spike.
- Does not touch any `.rs` or `Cargo.toml` file, and does not modify `ranting_i18n`, `ranting_es`,
  or any existing test.
- Does not decide between "a full fifth falsifier crate" and "a lighter pinned test in the main
  crate's own `tests/ranting/`" — both satisfy the falsification-apparatus goal §4.7 names; the
  choice between them is left to the maintainer, per the last paragraph of the Recommendation
  section above.

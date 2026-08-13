# Architecture Review — 2026-08-13

Findings from a doc/code consistency audit (7 parallel investigations + a
verification pass) covering both crates' source, generated-code sharing
mechanisms, and the repo's in-progress-work tracking. This is a point-in-time
findings record, not a durable spec — see `CLAUDE.md` and `ROADMAP.md` for
current architecture and status; this doc is referenced from a couple of
spots where it has the full detail.

## 1. Irregular-plural lookup functions are dead code in both crates

`get_plural`/`get_singular`/`apply_case` exist in two places:

- `src/language/plurals.rs:12,23,35` (canonical, `ranting`)
- `ranting_derive/src/language/plurals.rs:14-52` (hand-duplicated, **not**
  generated via the usual `include!(OUT_DIR)` pattern the way
  `english_shared.rs`/`verb_conjugate.rs` are)

Both copies are genuinely unreachable dead code today:

- Zero callers of the `ranting` copy anywhere in `src/` or `tests/` outside
  its own module and unit tests.
- Zero callers of the `ranting_derive` copy anywhere in `ranting_derive/src/`
  (`lib.rs`, `ranting_impl.rs`, `str_lit.rs`, `heed.rs`) outside its own
  module.

The table-generation codegen itself (`data/irregular_plurals.txt` →
`$OUT_DIR/irregular_plurals_generated.rs` via `build.rs`) is complete and
correct — the gap is purely that nothing calls the lookup functions from an
actual noun-inflection path (`Noun::inflect()` doesn't use them).

**Why this matters**: `ranting_derive`'s copy currently escapes
`cargo clippy -D warnings` detection because that gate only hard-errors the
*primary* package being compiled — when `ranting_derive` builds as a path
dependency of `ranting`, its own dead code shows as a plain warning, not an
error. Running `cargo clippy` from inside `ranting_derive/` directly would
likely surface it.

**The two copies have also drifted** from each other, despite
`ranting_derive/src/language/plurals.rs:7`'s own comment that "both copies
must be kept in sync": the `ranting` copy has `#[allow(dead_code)]` and uses
`.is_some_and(...)`; the `ranting_derive` copy has neither and still uses
`.map_or(false, ...)`. There's no build-time check enforcing the sync.

**Also missing**: unlike `irregular_verbs.txt`, there is no
`ranting_derive/data/irregular_plurals.txt` symlink. A packaged (non-dev)
build would hit the packaged-build fallback path in `find_data_file` and
fail to locate this file unless one is added before the next publish.

## 2. A stale dead-code justification comment (now fixed)

`src/language/english_shared.rs`'s `Article`/`DemonstrativePronoun` enums had
an `#[allow(dead_code)]` comment (added in an earlier session pass while
unbreaking the clippy gate) claiming they were "kept for `ranting_derive`'s
compile-time article handling." That's not true — grep across
`ranting_derive/src/` finds zero references to either type; the derive
crate's real article logic works via string-literal matching
(`ranting_derive/src/lib.rs:215-216,493,500`), and `ranting`'s own real
article logic lives in a separate, actually-used `ArticleOrSo` enum
(`src/language/english.rs:12-18`). The comment has been corrected to state
they're unused in both crates rather than falsely justify them as
cross-crate-consumed.

## 3. No durable record of the overnight-loop stash situation

`git stash list` currently holds 10 entries (`stash@{0}`–`stash@{9}`) each
prefixed `"failed: ..."` by `scripts/overnight_loop.sh`'s automatic
stash-on-gate-failure behavior (`overnight_loop.sh:110-124`). Neither
`CLAUDE.md` nor `ROADMAP.md` mentions `git stash` anywhere.

Context (from direct investigation, not this audit's subagents): all 10 were
stashed because the CI-style gate (`cargo fmt --check && cargo clippy -D
warnings && cargo test`) was broken independently of any of the 12 tasks —
pre-existing dead code and two clippy style lints on the starting commit made
every task in that run unwinnable regardless of its own diff. That baseline
was fixed on `master` (commit `0fe23b24`) and merged into
`overnight/2026-08-12` (`76fac22e`). Rebuilding and retesting each stash's
actual diff against the fixed baseline showed all 12 build and pass tests
cleanly. Two (Licensing proposal, Repo hygiene) were replayed and committed;
the other 10 remain in `git stash list`, deliberately not replayed due to
heavy file-overlap between them (most touch `ROADMAP.md`; several touch
`src/lib.rs`). `overnight_loop.sh` now has a pre-flight gate check
(added same session) so a future run against a broken baseline fails in
minutes instead of stashing a whole night's work.

`tasks.txt` still lists these 10 items as pending work for a future
`overnight_loop.sh` run — that's the existing tracking mechanism, and it's
accurate. This section exists only because the *history* of why they failed
and what's already been verified about them had no other durable home.

## 4. Stale top-level docs

`PHASE_2_IMPLEMENTATION_PLAN.md` claimed "Status: In Progress" and described
a "walkeds" bug as unfixed, while `DONE.md`/`ROADMAP.md` confirm Phase 2 has
long since shipped complete, including that exact bug fixed. A superseded
banner was added pointing to `DONE.md`.

`DESIGN_REPORT_SUMMARY.md`, `RECOUNTING_INTEGRATION.md`, and
`ARGUMENT_PARSING_IMPROVEMENTS.md` are dated (or near-dated) 2026-08-1{1,2}
snapshots whose recommendations/concerns are now reflected as done in
`ROADMAP.md`/`README.md`. None of the three make an active "in progress"
claim the way `PHASE_2_IMPLEMENTATION_PLAN.md` did, and nothing in
`CLAUDE.md`/`ROADMAP.md` points to them, so they were left as-is —
orphaned historical artifacts rather than actively misleading ones.

## 5. `{`who title are}`-style placeholders mishandle "they" (fixed)

While verifying `docs/TUTORIAL.md`'s Section 1 example (the `say_this(who,
title)` pattern from README.md), found that the three-token possessive
placeholder pattern `` {`who title are} `` produces the wrong possessive
determiner specifically when `who` is declared `subject = "they"`:

```rust
fn say_this(who: Noun, title: &Noun) -> String {
    say!("{=who do} say {`who title are} {who}.")
}
let title = Noun::new("name", "it");
say_this(Noun::new("Jordan", "I"),  &title)  // "I do say my name is Jordan."     ✓
say_this(Noun::new("Jordan", "he"), &title)  // "He does say his name is Jordan." ✓
say_this(Noun::new("Jordan", "she"),&title)  // "She does say her name is Jordan."✓
say_this(Noun::new("Jordan", "they"),&title) // "They do say its name is Jordan." ✗ expected "their"
```

Reproduced directly (not inferred from docs). Neither `tests/ranting/tutorial.rs`
nor `tests/ranting/readme_example.rs` exercised `who = "they"` for this
pattern — both only covered `"I"` and `"he"` — so this gap had no test coverage
either way. Root cause: `ranting_derive/src/lib.rs`'s codegen for the
backtick-possessive substitution (`` `who `` → `who`'s possessive determiner)
hardcoded `to_plural: false` when calling `ranting::inflect_possessive`,
regardless of `who`'s actual plurality — so a `they`-declared `who` was always
singularized before its possessive form was looked up, landing on "its"
(`it`'s form) instead of "their". I/he/she happened to look correct only
because singularizing an already-singular pronoun is a no-op.

**Fixed**: `ranting_derive/src/lib.rs:765` now passes `#expr.is_plural()`
instead of the hardcoded `false`, so the possessive reflects `who`'s real
declared plurality. Regression coverage added: `tests/ranting/tutorial.rs`'s
`section_1_why_say_vs_format` gained a `"they"` case, and
`tests/ranting/property_based.rs` gained a `"they"`-adjacent no-panic property
test for `inflect_possessive`/`inflect_reflexive` (see section 6 below —
found alongside a related graceful-degradation fix for the same two
functions). `docs/TUTORIAL.md`'s Section 1 example now also shows the
`"they"` case instead of avoiding it.

## 6. CLAUDE.md's "Planned restructuring" section was describing finished work as future (fixed)

A second docs-audit pass the same day found `CLAUDE.md`'s "Planned restructuring"
section (top of file) still described Phase 4 items 2, 3, 5, and 7 as "still
ahead" — dependency modernization, the typed `caps: [&str; 5]`/`~TENSE~` →
`PlaceholderSpec` interface, the `inflect_possesive` typo fix, and (unmentioned
at all) the MIT relicensing — even though `ROADMAP.md` and the live source both
confirm all of Phase 4's 8 items are done. Renamed the section to "Architecture
status" and rewrote it to state Phase 4 is complete, per the doc-sync triage
rule (stale claim in a doc, code is correct → fix the doc where the claim
lives). Also added `ranting_core::placeholder`/`ranting_core::ph_ext` (Phase 4
items 3 and 6's new modules — together over 1200 lines, most of `ranting_core`,
and previously unmentioned in the Architecture section), `ranting_impl.rs`
(`ranting_derive`'s core codegen entrypoint) and `src/language/auxiliary.rs`
(auxiliary-verb conjugation for tense markers) to `CLAUDE.md`, none of which
had been named anywhere in the file despite being live, load-bearing code.
`ROADMAP.md`'s own "Current State (v1.0.0)" banner was equally stale (predating
Phases 3-5, all done further down the same file) — updated to "v1.2.1".

**Internal tension in `ROADMAP.md` itself — now resolved in code.** Phase 4
item 4's implementation notes explicitly listed five `.expect("Not a
subject")` calls in `src/language/english.rs`
(`inflect_adjective`/`inflect_subjective`/`inflect_objective`/
`inflect_possessive`/`inflect_reflexive`) as a deliberate deferral — "left
alone, out of this item's explicit scope" — reasoning that they operate on
already-validated data. But two of those five (`inflect_possessive`,
`inflect_reflexive`) are public functions taking a raw `subject: &str`, so
they *are* reachable with unvalidated input from outside `say!()`'s own call
sites, in tension with a separate "v1.2 success criteria" bullet elsewhere in
`ROADMAP.md` claiming "no runtime panics reachable from public API with
invalid data."

**Fixed**: all five `.expect("Not a subject")` calls in
`src/language/english.rs` now degrade gracefully to `SubjectPronoun::It`'s
forms on unrecognized input, the same treatment `is_subjective_plural`
already got in Phase 4 item 4 — not just the two public ones, since fixing
all five for a consistent invariant ("`SubjectPronoun::from_str` failure
never panics anywhere in this file") was cheap and removes the tension
entirely rather than leaving `inflect_adjective`/`inflect_subjective`/
`inflect_objective` on a different standard from their two public siblings.
Regression coverage: `tests/ranting/property_based.rs` gained
`inflect_possessive_and_reflexive_invalid_subject_degrade_to_it` and two
`proptest` no-panic properties (`prop_inflect_possessive_no_panic`,
`prop_inflect_reflexive_no_panic`).

Three other top-level docs (`ARGUMENT_PARSING_IMPROVEMENTS.md`,
`DESIGN_REPORT_SUMMARY.md`, `RECOUNTING_INTEGRATION.md`) were reconsidered for
a superseded banner (matching `PHASE_2_IMPLEMENTATION_PLAN.md`'s precedent) but
left as-is, per section 4's already-recorded reasoning above: none make an
active "in progress" claim, so a banner isn't load-bearing the way it was for
`PHASE_2_IMPLEMENTATION_PLAN.md`'s stale "Status: In Progress" line.

## 7. `ranting-i18n` feasibility spike: German case declension (fixed)

Before scoping the `ranting-i18n` companion crate proposed in ROADMAP.md's
Post-v1.2 section, spiked whether the existing `inflect_*_custom` trait
hooks (built for lexicon-level forks like pirate/Spanish) could produce
case-correct German output. Verb agreement (`bellt`/`bellen` via
`as_plural`) worked immediately. Gendered/case-declined articles did not: a
`Hund` (dog) noun used once as a sentence subject and once as the object of
"I saw" produced two `inflect_article_custom` calls with an **identical**
`(article, noun_singular, as_plural, uc)` tuple — the hook had no signal
distinguishing "this placeholder is the subject" from "this placeholder is
an object," so it necessarily returned the same article form for both,
yielding the ungrammatical `"I saw der Hund."` (should be *den Hund*).

**Fixed**: `inflect_article_custom`/`inflect_article_custom_with_context`
gained a `case: GrammaticalCase` parameter (`src/lib.rs`), threaded from the
placeholder's own `CaseKind` (its case marker — `` {the =noun} `` is
`Subjective`, `` {the @noun} `` is `Objective`, etc.; a bare `` {the noun} ``
with no marker reports `GrammaticalCase::Name`, since English has nothing
more specific to give in that form). `GrammaticalCase` is a new public type
that mirrors `ranting_core::placeholder::CaseKind` via a `From` impl rather
than exposing `CaseKind` itself, since `ranting_core` types aren't part of
`ranting`'s public semver surface. Re-running the spike with the fix and an
`inflect_pronoun_custom` override that keeps returning the noun's own name
(rather than falling back to an English pronoun for `` =noun ``/`` @noun ``)
produced correct output: `"Der Mann bellt."` (nominative) and `"Ich sah den
Mann."` (accusative). Regression coverage: `tests/ranting/grammatical_case.rs`.

This closes the specific gap the spike found, but doesn't make `ranting-i18n`
free: word order is still baked into the literal template string around
placeholders (English's fixed slot order), so German verb-second or SOV
languages still need per-language templates, not just per-language
inflection hooks. `GrammaticalCase` only fixes the article/case-declension
half of the gap.

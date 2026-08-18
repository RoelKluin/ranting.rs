# Design spike: exercising the nine never-overridden hooks

**Status**: design spike, PROPOSED only — **no code in this repository is changed by this
document**. ROADMAP.md Phase 9 item 1. Confirms the current audit state (with one correction),
picks one concrete falsification target from the item's own candidate text, verifies its expected
behavior against a throwaway fixture built and run against the real crate (never committed), and
recommends it as a genuine gap rather than a "confirms the default" finding — a stronger result
than the item asked for.

## Confirming the audit, by hand

`scripts/hook_audit.sh` could not be run directly in this session (its execution required an
approval this session's sandbox never grants), so its check was reproduced by hand: read the
`Ranting` trait's method list from `src/lib.rs`, then `grep -rl "fn <method>" <fork>/src/` for
each of the eight `_with_context` twins plus `is_first_person_subject_custom`, across all four
falsifiers (`ranting_i18n`, `ranting_es`, `ranting_ar`, `ranting_ja`) plus `ranting_gaps` (which
contributes zero to every column — it is not a `Ranting` implementor, per
`.claude/rules/crate-layout.md`).

**Eight of the nine are still never overridden, confirmed clean today:**
`inflect_pronoun_custom_with_context`, `inflect_article_custom_with_context`,
`elide_article_custom_with_context`, `elide_numeral_custom_with_context`,
`inflect_preposition_custom_with_context`, `inflect_adjective_custom_with_context`,
`inflect_numeral_custom_with_context`, `capitalize_with_context`, and
`is_first_person_subject_custom`.

**One correction to §4.1's count**: `inflect_verb_custom_with_context` **is** overridden today, by
`ranting_ja/src/noun.rs:103` (`JapaneseNoun::inflect_verb_custom_with_context`, reading
`ctx.register` to pick sonkeigo/plain verb forms). That override was added in commit `99562a36`
("feat: ranting_ja, the fourth falsifier", Phase 7 item 6) — which landed *before* commit
`32db2729` ("docs: record hook_audit.sh finding — nine hooks never exercised by a fork", the
commit that wrote §4.1). `git log --oneline` confirms the ordering (`32db2729` is newer). So the
"nine, across every fork" claim was already stale the moment it was written — the audit that
produced it either wasn't rerun against the current tree, or was misread. §4.1 should read "eight
hooks, plus `is_first_person_subject_custom`" (still nine total, but the `_with_context` count
drops from eight to seven) — a documentation fix, not a code question, and out of scope for this
spike to make (see "What this spike does not do" below).

**The ROADMAP item's own suggested register-change falsification is also already done.** Item 1's
text proposes "a mid-story register *change* — different politeness to two different addressees in
one `say_with!()` sequence" as the natural next step for `ranting_ja`. That test already exists:
`ranting_ja/tests/japanese.rs::register_can_vary_per_utterance_within_one_scene` does exactly
this — a formal `say_with!()` call to `sensei` followed by a casual one to `hito`, in the same
test, pinned since the same commit that added the override. (`NarrationContext` is per-call, not
per-story — see `.claude/rules/extension-hooks.md`'s `_with_context` mechanism section — so "one
`say_with!()` sequence" can only mean a sequence of calls sharing a scene, never two registers
inside a single macro invocation; that is exactly the shape this test already has.)

Net effect: of the two candidate shapes item 1's own text names, the register one is spent. What
is left, and what this spike picks, is the other one: **a first-person label other than `I`/`we`
exercising `is_first_person_subject_custom`.**

## The falsification target

**Falsifier crate**: `ranting_i18n` (German). It already has the needed fixture halfway built:
`GermanPerson::ICH`/`GermanPerson::WIR` (`ranting_i18n/src/person.rs`) declare `subjective()` as
`"ich"`/`"wir"` — first-person labels other than English's — and the crate's own verb hook
(`Person::from_subject`, `ranting_i18n/src/lexicon.rs:214`) is keyed on those exact strings. No
other falsifier has as natural a fit: `ranting_es`'s `yo`/`nosotros`, `ranting_ar`'s `أنا`/`نحن`
would work identically for the *first* part of this test (declaring the label), but German already
has the *named-entity-plus-declared-subject* shape needed (see next paragraph), so extending it is
less new-fixture surface than building one from scratch elsewhere.

**The test would assert**: retelling a first-person-declared German entity through
`say_with!()`'s `narration_person` override renders a **grammatically coherent** sentence — not
merely "doesn't panic" or "renders something." Two variants, both worth pinning:

1. `NarrationContext::new().narration_person(Person::Third)` applied to a `wir`-declared narrator,
   asserting the output is idiomatic third-person German (`"Sie sind alt."` or equivalent — not a
   mix of English and German words).
2. The same for `Person::Second`, asserting a real `du`/`Sie`-form German sentence rather than
   whatever the current mechanism actually produces.

This needs a **named narrator fixture** shaped like the root crate's existing
`tests/ranting/first_person_hook.rs::GermanNarrator` (name + declared `subject: "ich"/"wir"`, not
`GermanPerson`, which *is* the pronoun rather than a character who happens to speak in first
person) — `ranting_i18n` has no such fixture today; item 1 would need one added, analogous to that
root-crate synthetic one but exercising `ranting_i18n`'s own real `lexicon::conjugate` and
`Person::from_subject` instead of a hand-rolled inline table.

## Verified against the real crate: this is a genuine gap, not a "confirms sufficient" result

The item's text asks only "is the hook exercised" — but the deeper question a fork actually needs
answered is "does exercising it work," which is what a real, non-synthetic test would surface.
Both were checked empirically, against `ranting_i18n` as built, by adding a throwaway,
never-committed test file (`ranting_i18n/tests/zz_scratch_hook_falsification.rs`, three fixtures,
run with `cargo test -- --nocapture`, then `git clean -f`'d — `git status --porcelain` is clean
afterward). No file in the repo was left changed by this verification; the exact code is
reproduced below so the result is checkable without rerunning it.

**Fixture 1 — `is_first_person_subject_custom` overridden (`"ich"`/`"wir"`), no
`inflect_pronoun_custom` override** (a `GermanNarrator`-shaped struct with `subject: "wir"`,
`inflect_verb_custom` wired to `lexicon::conjugate`, everything else default — the same shape as
the root crate's own `tests/ranting/first_person_hook.rs::GermanNarrator`, minus a pronoun hook):

```
plain:         "It sind alt."       // say!() — unrecognized "wir" pronoun degrades to "it" (documented default)
retold-third:  "They sind alt."     // narration_person(Third) — SILENTLY DOES SOMETHING, but wrong on both counts
retold-second: "You ist alt."       // narration_person(Second) — "You is" is not correct English either
```

This is not the item's assumed "silent no-op" failure mode — `narration_person` is **not** inert
here (unlike the pre-hook `UnhookedGermanNarrator` case in `tests/ranting/first_person_hook.rs`,
which never fires because `is_first_person_subject_custom` doesn't recognize `"wir"`). Once the
crate's real `Person::from_subject`/`lexicon::conjugate` supply the verb, `resolve_viewpoint`'s own
hardcoded English pronoun ("they"/"you") is what leaks through unfused into an otherwise-German
sentence, because nothing here overrides `inflect_pronoun_custom`. The result is a mixed-language
sentence that compiles, runs, and asserts happily — worse than a no-op, since nothing signals that
anything is wrong.

**Fixture 2 — `is_first_person_subject_custom` correctly hooked, `inflect_pronoun_custom` also
overridden exactly the way the real, shipped `GermanPerson::inflect_pronoun_custom`
(`ranting_i18n/src/person.rs:113`) is written** — reading `case` to pick `self.subject` /
`self.objective()` / etc., and **ignoring the `_subject` parameter it's handed** (confirmed by
reading `person.rs`: the parameter is literally named `_subject`, unused):

```
plain-hooked:          "Wir sind alt."
retold-hooked-third:   "Wir sind alt."   // byte-identical to plain — a TRUE silent no-op
```

Closing the exact gap item 1 names — overriding `is_first_person_subject_custom` — is **necessary
but not sufficient**. `resolve_viewpoint` (`src/narration.rs:162`) only ever changes the `subject`
string threaded into the verb/pronoun hooks; it never touches which hook a `Ranting` impl chose to
write. `GermanPerson`'s pronoun hook is, by design, keyed on `self` (which pronoun *this instance
is*) rather than on the passed `subject` parameter, which is the entirely reasonable choice for an
entity whose whole identity is a fixed personal pronoun — but it means the one hook actually
carrying `narration_person`'s payload downstream is silently discarded by exactly the fixture that
looks, on paper, like the natural German answer to `GermanNarrator`. The verb conjugation
(`inflect_verb_custom`, correctly reading the overridden `subject`) *does* react — it is only the
rendered pronoun word that regresses to the entity's own declared one, which is why the sentence
comes out byte-identical to the non-retold original rather than visibly wrong the way Fixture 1's
does.

## Recommendation

This is scoped as **item 1 finding a real gap**, not confirming the default sufficient — a valid
outcome under this repo's own falsifier philosophy (`.claude/rules/crate-layout.md`,
`docs/architecture-review-2026-08-15.md` §4.1's own framing: "the falsifier contract is designed
to surface exactly this kind of gap"). Specifically:

1. `narration::resolve_viewpoint`'s rendered-subject strings (`"you"`, `"they"`, hardcoded in
   `src/narration.rs`) are English words. They flow into the fork's `subject` parameter on every
   `_with_context` hook, which is fine for a hook that does its own lookup keyed on that string (as
   `ranting_i18n`'s verb hook does — by coincidence of English `they`'s plurality matching German's
   third-person-plural verb form, `Person::Second`/`Third` retelling of a `wir`-declared entity
   produces the *correct German verb form* today) — but is not fine for a hook whose job is to
   *render* that subject as a word, since there is no German word for the English string `"they"`.
2. No hook in the current surface gives a fork a way to supply "the retold subject, but in my own
   language" — the closest thing, `inflect_pronoun_custom`'s `subject` parameter, exists but a
   reasonable, already-shipped implementation (`GermanPerson`) does not read it, and *cannot*
   meaningfully read it as `"they"`/`"you"` even if it wanted to, since those are English-only
   labels with no path back to a German pronoun.
3. This is worth a maintainer's ruling on scope before landing a fix: is the gap "`ranting_i18n`
   should add a `GermanNarrator`-shaped fixture and accept that first-person viewpoint retelling
   degrades to the verb-only form for now" (a documented hole, like the other README-numbered
   ones), or does the crate need a new signal — e.g. `resolve_viewpoint` handing the fork
   `Person::Third`/`Person::Second` itself (not just a rendered English string) so a fork's own
   pronoun hook could pick its own word for "the entity retold in this grammatical person" — which
   would be a `Ranting`/`NarrationContext` surface change, not a falsifier-crate-only fix, and a
   bigger lift than item 1's own text anticipated.

## What this spike does not do

- Does not edit `docs/architecture-review-2026-08-15.md` §4.1's stale "eight ... never overridden"
  count — flagged above, left for the maintainer to fix in the same pass as whichever item 1
  follow-up is chosen, since it's a one-line correction adjacent to real scope work rather than
  a spike-worthy question on its own.
- Does not add the `GermanNarrator`-shaped fixture, the two pinned assertions, or any hook
  override to `ranting_i18n` — that is the implementation this spike is scoping, not doing.
- Does not propose a `resolve_viewpoint`/`NarrationContext` signature change — recommendation
  point 3 names it as one possible resolution, explicitly left to a maintainer rather than designed
  here, since it would touch public API rather than stay inside one falsifier crate the way every
  other item-1 candidate does.
- Does not touch `.rs`/`Cargo.toml` files or existing tests. The verification fixtures quoted above
  ran against a throwaway, uncommitted `ranting_i18n/tests/zz_scratch_hook_falsification.rs`, which
  was `git clean -f`'d before this document was written; `git status --porcelain` shows no residue.

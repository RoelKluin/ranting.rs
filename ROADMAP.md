# ROADMAP.md

**Ranting** is a lightweight, declarative library for ergonomic, grammatically-correct formatted output in Rust.

---

## Vision

Ranting solves the problem of writing natural-sounding, dynamic user-facing text in Rust. Developers should be able to write grammar rules once and let the library handle inflection automatically—no manual string juggling, no brittle conditional text generation.

**Target**: Game engines, interactive fiction, chatbots, and any application that generates user-visible prose.

**Design principle**: Lightweight and ergonomic. Users write clean, declarative placeholders; Ranting handles the grammar.

---

## Current State

✅ **Phases 1-6 complete** — the full record for every one of them, item by item, now
lives in [DONE.md](DONE.md). This file is the forward-looking roadmap only.

✅ **Phase 7 (v1.4.0, Falsification, Round Two: Beyond Indo-European) is complete**
— see its section below. Its three spikes (items 1-3), the build decision they fed
(item 4), four unrelated items (7-10), the signature change the decision blocked on
(item 11), **both** reference lexicons (items 5 and 6) and the two items they
scheduled (12 and 13) all landed on 2026-08-14.

📋 **Phase 8 (English grammar depth) is scoped, partly landed** — seven recorded defects
plus five missing channels, from a grammarian's end-to-end review of the placeholder
surface against complex-sentence English (2026-08-15). See its section below; the
defect half is `docs/architecture-review-2026-08-15.md` §§1.5-1.12; §§1.6, 1.7 and 1.9
landed on 2026-08-15.

**Shipping today**:
- All 7 tenses, 118+ irregular verbs, irregular noun plurals, gender-neutral pronouns
- `say!()`/`say_with!()`/`ack!()`/`nay!()`/`heed!()`/`ask!()`/`#[derive(Heed)]`
- Eight `_custom`/`_with_context` inflection hook pairs (verb, pronoun, article,
  adjective, article elision, numeral, numeral elision, preposition), plus the
  `capitalize`/`capitalize_with_context` pair and one unpaired
  `is_first_person_subject_custom` — 25 `Ranting` trait methods in all, carrying
  grammatical case, noun class, count, and orthography role
- Eight independent Cargo manifests, no workspace: three library crates
  (`ranting`, `ranting_core`, `ranting_derive`), four downstream falsifiers
  (`ranting_i18n` German, `ranting_es` Spanish, `ranting_ar` Arabic,
  `ranting_ja` Japanese) and the `ranting_gaps` dev tool
- A green gate in every one of the eight manifest directories; see CLAUDE.md for the loop

---

## Completed phases — see DONE.md

Phases 1 through 6 are finished and were moved out of this file (2026-08-14) to keep
the roadmap forward-looking. **Cross-references elsewhere in the repo of the form
"ROADMAP.md Phase N item M" still name this file; the item they point at is in
[DONE.md](DONE.md), under the same phase and item number.**

| Phase | Version | Scope |
|---|---|---|
| 1 | v0.3.0 | Foundation & ergonomics — named arguments, error messages, singular they, test coverage |
| 2 | v1.0.0 | Grammar depth — all 7 tenses, irregular verb tables via codegen, tutorial + cookbook |
| 3 | v1.1.0 | Plurals, extensibility, ecosystem — irregular plurals, the first three `_custom` hooks, `say_with!()`/`NarrationContext`, reflexives, comparatives, `Box`/`Many`/`Maybe`, `heed!()` |
| 4 | v1.2.0 | Architecture consolidation — `ranting_core` extraction, dependency modernization, typed `PlaceholderSpec`, hand-written `ph_ext` parser, public-API cleanup, MIT relicensing |
| 5 | v1.2.1 | `ask!()` stabilization — the `Answerable` trait, capture-forwarding `ask!()`, first test coverage |
| 6 | v1.3.0 | Internationalization foundations — 22 items plus follow-ups: `NounClass`, `inflect_adjective_custom`, `capitalize`/`OrthographyRole`, `elide_article_custom`, `inflect_numeral_custom`, `inflect_preposition_custom`, the count channel, non-Latin sentence detection, the fused `*=`/`*@` markers, the word-order boundary, and the two falsifier crates |

---


## Phase 7 — v1.4.0 — Falsification, Round Two: Beyond Indo-European

*Goal: German and Spanish are both Indo-European and, more specifically, both
fusional languages that decline/conjugate by affixing a closed set of endings
onto a stable stem — exactly the shape `NounClass`, `GrammaticalCase`,
`AdjectiveDegree` and the `_custom` hooks were designed against. Nothing in
Phase 6 has yet been tried against a language that breaks that shape: a
grammatical number system with a third value (dual), noun-class marking
that isn't gender at all (numeral classifiers), a politeness system that
isn't a pronoun swap (register-driven honorifics), or a morphology that
isn't affixal (Semitic root-and-pattern). This phase does not add a tenth
`_custom` hook on spec; it spends two cheap spikes finding out whether Phase
6's hook surface needs one before writing any lexicon code, following the
spike-before-signature-break discipline items 1/3/4 established in Phase 6.*

**What this phase explicitly is not**: a re-opening of `ranting_i18n`'s holes
2/3/4 or of `docs/superpowers/specs/2026-08-13-adjective-declension-class.md`.
Three independent design spikes (items 3, 18, and the declension-class spike)
each separately concluded "the entity carries its own case/declension state,
not the placeholder" — a converged, deliberate answer, not unfinished work.
Nothing below revisits that conclusion; a Phase 7 lexicon that happens to be
case-declining (Arabic is) is expected to reach the same conclusion a third
time and cite it, not re-litigate it.

**Ordering rationale**: both language spikes (items 1 and 2) are cheap,
independent of each other, and produce documents, not code — exactly item
1/3/4's Phase 6 shape, for the exact reason Phase 6 gave: a lexicon crate is
16-24 hours, so deciding on paper which gaps are real and worth 16-24 hours
of falsification is what keeps that spend from being guessed at. Item 3 (the
unused-hook audit) is cheaper than either spike and its finding — which
hooks in the existing eight-hook-pair surface have *never* been exercised by
a real fork — is direct input to items 1 and 2: a candidate language earns
priority partly by which unused hooks it would finally exercise, not only by
which new gaps it would find. Item 3 has no dependency on 1/2 and can run
first or in parallel. Items 4 and 5 (the builds) are contingent on what 1 and
2 conclude and are scoped provisionally below; if a spike recommends against
building, its build item is dropped rather than executed anyway.

1. **Unused-hook audit** (doc-only, 3-5 hours) — ✅ **DONE 2026-08-14**;
   findings in `docs/superpowers/specs/2026-08-14-unused-hook-audit.md`,
   re-runnable via `scripts/hook_audit.sh`. Headline: ten of twenty-three
   methods have never been overridden by a fork, and item 14's `count`
   channel is ignored by all fourteen hook declarations that carry it.
   *Cheap, informs items 2 and 3; no dependency on either*
   - Phase 6 shipped seven `_custom` hook pairs (verb, pronoun, article,
     adjective, elision, numeral, preposition), an eighth `capitalize` pair, and
     one unpaired `is_first_person_subject_custom` — eight `_with_context`
     twins in total, 23 `Ranting` trait methods. An unused
     hook is a design risk, not a feature: nothing has verified its shape
     matches a real need, only that it compiles and defaults correctly.
   - Verified already, ahead of scoping this item, by reading both existing
     forks' source: **`elide_article_custom`/`_with_context` is overridden by
     neither `ranting_i18n` nor `ranting_es`** — German's real fusions are
     all preposition+article (routed through item 26's
     `inflect_preposition_custom` instead) and Spanish's two contractions are
     the same; the hook item 7 built for `le`+`homme`→`l'homme` has never
     been exercised by a language that actually needs it. Neither fork
     overrides **any** of the eight `_with_context` methods either — the
     `hole_1_*` dialect test in `ranting_i18n/tests/holes.rs` proves the
     *plumbing* (item 12) delivers `NarrationContext` to the default body,
     not that a fork's own logic ever branches on `ctx.dialect`/
     `ctx.register`. `is_first_person_subject_custom` (item 16) is
     overridden by neither. `inflect_numeral_custom`'s `NumeralStyle::Digits`
     arm (a script's own digit system for `$var`) is exercised only by a
     main-crate test (`tests/ranting/numeral.rs`'s Devanagari case), never by
     a fork's own lexicon — item 8's own follow-up note already says so.
   - Deliverable: a short doc (`docs/superpowers/specs/` or a
     `docs/EXTENSIBILITY.md` addendum) listing every hook pair and whether it
     has ever been overridden outside `src/`/`tests/ranting/` — the audit
     above, made complete and re-runnable (grep for `fn <hook_name>` under
     `ranting_i18n/src/` and `ranting_es/src/`, excluding `_with_context`
     twins found by name) rather than asserted from memory. No code changes.
   - What it proves: whether "ships with an English-preserving default" has
     quietly become a way to defer ever finding out if a hook's *shape* is
     right, for four-plus hooks running two-for-two on real forks never
     needing them.

2. **Arabic falsification spike** — ✅ **DONE 2026-08-14**;
   `docs/superpowers/specs/2026-08-14-arabic-falsification-spike.md`. Verdict:
   **build `ranting-ar`, but only after the signature change below.** Unlike
   the Phase 6 spikes this one ran the code — a throwaway path-dependency
   crate, not committed — so every finding is observed output rather than
   reasoning from signatures, and two of the five questions resolved opposite
   to this item's stated expectation.
   - **The count channel does *not* close the dual, and this item's prediction
     was wrong.** `Ranting::inflect` — the call that renders the counted noun —
     takes `to_plural: bool`, not a count. Item 14 widened it in the same
     commit, with `case: GrammaticalCase`. So `{$n kitab}` with `n = 2` renders
     `kutub`, never `kitābān`, while the *verb* hook does see
     `PlaceholderCount { value: 2, .. }` and can agree in the dual. Arabic dual
     is therefore **half** expressible — everything that agrees with the noun,
     but not the noun — which is a worse failure than either extreme, since the
     output is grammatical-looking and wrong in one word. The `Cell`
     side-channel that smuggles the count from `inflect_numeral_custom` into
     `inflect` was tried and works, and is not an answer: it contaminates later
     placeholders in the same template (`"{$n kitab} and {+kitab}"` renders the
     dual twice), depends on undocumented hook call order, and makes a `&self`
     trait stateful. **Owed, unscheduled**: `count: Option<PlaceholderCount>`
     on `Ranting::inflect`, same type and source as item 14's.
     `2026-08-13-number-categories.md` now carries a correction section saying
     so; its inventory missed `inflect` because `inflect` is not a `_custom`
     hook.
   - **`elide_article_custom` passes its first real test.** Sun-letter
     assimilation works: `following` is the noun alone, so the trigger
     consonant is `following.chars().next()`, and the post-assembly design that
     lets a fork drop the separator is exactly what `al-` bound to its noun
     needs. One trap worth documenting in §2.7: the article arrives
     **capitalized** sentence-initially, so `match article { "the" => .. }`
     silently falls through to `None`.
   - Dual with no numeral stays unreachable and **should stay that way** — it
     is a grammar change, and with the `inflect` fix a fork can carry a bare
     dual as entity state the way `ranting_i18n` carries definiteness.
   - Root-and-pattern morphology has no seam consequence: "the base form" is
     whatever the template wrote, and the seam passes it through uninterpreted.
     RTL is out of scope as predicted; Arabic-Indic digits already work through
     `inflect_numeral_custom`.
   - Two unrelated defects found while probing, filed as
     `docs/architecture-review-2026-08-14.md` §1.5 and §1.6.

   <details><summary>Original scope (kept for the record)</summary>
   - Score, without writing a lexicon, what a `ranting-ar` crate would
     falsify that German/Spanish structurally cannot:
     - **Dual number, with a numeral present.** `docs/superpowers/specs/
       2026-08-13-number-categories.md` scored option (b) — a `count`
       channel, dual/paucal/CLDR reachable "when a numeral is present" — as
       its recommendation, and item 14 landed exactly that shape:
       `PlaceholderCount { value: i64, .. }` on five hook pairs plus the
       pre-existing `count: Option<i64>` on `inflect_numeral_custom`. That
       *should* mean `say!("{$n kitab}", 2, book)` can render Arabic dual
       (`kitābān`, not the plural `kutub`) by branching on `count.value == 2`
       in a fork's own hook — but no fork has ever tried it. The spec's own
       "unreachable" verdict was written before item 14 shipped and needs
       correcting to "reachable in principle, unverified"; an Arabic spike
       is the cheapest way to find out whether the raw `i64` is actually
       sufficient or whether something else (a paucal-range check, agreement
       with a counted noun's own gender) is still missing.
     - **Dual with no numeral.** Arabic marks dual on the noun itself
       (`kitābāni` "two books" spoken with no digit written), which is
       exactly the case the number-categories spec says stays impossible
       under item 14: `match_nr` accepts only `[+-]|(#|\??\$)\w+`, so there
       is no placeholder marker a bare dual could use, and this is a
       *grammar* change item 14 deliberately didn't make. An Arabic spike
       would be the first language to hit this gap for real rather than
       hypothetically, and should state plainly whether that makes a
       `ranting-ar` crate materially less useful than `ranting_i18n`/
       `ranting_es` (which never needed a bare-marker number distinction) or
       merely narrows its scope to "dual is spelled out with a written
       numeral or not modeled."
     - **Definite-article assimilation ("sun letters").** Arabic's definite
       article `al-` assimilates to the following consonant for a closed set
       of fourteen "sun letters" (`al-shams` → spoken/written `ash-shams`)
       and stays `al-` for the rest ("moon letters"). This is elision in
       exactly the sense item 7 built `elide_article_custom` for — article
       plus following word, inside one placeholder — and per item 1 above,
       no existing fork has ever called that hook. An Arabic lexicon would
       be its first real user, which is a sharper test of the hook's design
       than a French toy example: is `elide_article_custom`'s two-string
       signature (article, following) sufficient to look up which of
       fourteen consonant classes triggers assimilation, or does it need the
       consonant itself surfaced more directly than string-inspecting
       `following.chars().next()`?
     - **Non-concatenative (root-and-pattern) morphology.** Arabic verb and
       noun forms are built from a triliteral root mapped onto a template
       (`k-t-b` → `kataba`/`yaktubu`/`kitāb`/`maktaba`), not a stem plus
       affix. Every `_custom` hook returns an opaque `String`, so mechanically
       nothing blocks this — but it is worth stating explicitly, because it
       is the first candidate language where "the base form" (the concept
       item 5's `PostSpec::Degree.base` and `say_with!()`'s baked
       uninflected base verb both rely on baking at compile time) may not be
       a well-formed notion the way an English/German/Spanish stem is. The
       spike should say plainly whether root-and-pattern morphology is
       simply the fork's own lookup-table problem (most likely) or whether
       it exposes an assumption baked into what "base form" means at the
       macro↔runtime seam.
     - **Right-to-left rendering.** Score this explicitly and expect to
       reject it as in-scope: `ranting` assembles plain `String`s via
       `format!()` and never inserts direction-control characters or
       layout markup, so Arabic script in the output renders
       right-to-left the same way any RTL text does in its consumer's own
       renderer — this is a property of the text's Unicode script, not
       something `ranting` does or needs to do. The one real question worth
       recording is Arabic-Indic digit direction inside RTL text (`$var`
       rendering `١٢` vs `12`, an `inflect_numeral_custom` question already
       covered by item 8's digit-system channel, not a new one) — likely to
       conclude "not a `ranting` gap," in the same register as item 9's
       "tokenization isn't `ranting`'s job" conclusion for `heed!()`.
   - What it proves: whether the count channel item 14 shipped without a
     live consumer actually closes the number-categories spec's central
     gap, and whether `elide_article_custom` — currently a hook two-for-two
     unused — has the right shape for the one real elision case Phase 6's
     two lexicons never had.
   </details>

3. **Japanese falsification spike** — ✅ **DONE 2026-08-14**;
   `docs/superpowers/specs/2026-08-14-japanese-falsification-spike.md`.
   Verdict: **build `ranting-ja`, at lower priority than `ranting-ar`.** Same
   method (probes against real output). The decisive finding is not one of the
   four this item scoped.
   - **`NarrationContext.register` is the right shape for keigo, and this is
     its first real consumer** — teineigo renders off `register` alone with no
     pronoun in the template and no entity state (`猫 です` / `猫 だ`). The
     per-addressee variation this item worried about is a non-issue:
     `NarrationContext` is per-*call*, not per-story, so two politeness levels
     in one scene are two contexts. The docs' "story-wide" wording is what
     misleads, and should be softened. This is the only evidence that
     `register` is not dead weight; item 1's audit could not settle it from
     inside the repo.
   - **The `NounClass`-as-classifier question dissolves.** A fork reads
     `self.classifier` — which counter a noun takes is a property of that noun,
     so `class` is never consulted. Item 2's open-ended design is not falsified
     by Japanese, merely unexercised; Bantu, not Japanese, is the genericity
     test.
   - **What replaced it is a real gap**: the numeral and the noun are joined by
     a hard-coded space no hook can remove, so 「一匹の猫」 is unreachable and
     「一匹の 猫」 renders. Exactly parallel to Arabic's article-bound-to-noun
     case, except item 7 gave the article a hook and the numeral never got one.
     Every escape hatch is worse (`{?$n noun}` leaves its own stray space —
     §1.6; writing the numeral in the template kills item 8's channel).
     **Owed, unscheduled**: pass the separator to `inflect_numeral_custom` and
     honor an empty return, or add a numeral-side splice matching
     `elide_article_custom`'s.
   - `heed!()`'s whitespace boundary holds against genuinely unspaced input and
     fails honestly (`None`, never an invented split). The escape hatch is fine
     for `heed!()`; for `ask!()` it thins the value proposition to "call
     `answer()` with the input string", so `ask!()` is useful for Japanese
     *command* input and not for *prose* input. Item 9's decision stands.
   - Two of eight hook pairs would be live in a `ranting-ja`. That is the
     intended degradation, not a design smell — unoverridden hooks generate no
     code. The cost is discoverability, which is `docs/EXTENSIBILITY.md`'s
     problem.

   <details><summary>Original scope (kept for the record)</summary>
   - Score, without writing a lexicon, what a `ranting-ja` crate would
     falsify that German/Spanish structurally cannot:
     - **Numeral classifiers (josuushi).** Japanese counting requires a
       classifier selected by the *kind* of noun being counted — `一匹`
       (hiki, small animals) vs `一人` (nin, people) vs `一本` (hon,
       long thin objects) — not by the noun's own inflection. `NounClass`
       (item 2) was scoped as an open `&'static str` label specifically so
       a non-gender axis could reuse it ("Bantu has a dozen-plus classes and
       Danish has common/neuter" was the stated justification for staying
       open-ended) — but every existing consumer of `class` is
       `inflect_article_custom`/`inflect_pronoun_custom`/
       `inflect_adjective_custom`, article/pronoun/adjective agreement, and
       Japanese needs none of those three for this purpose. The real
       question is whether `inflect_numeral_custom`, which already receives
       `class: NounClass`, can carry a classifier string in that same
       parameter and render `一匹の猫` from it, or whether "read the
       counter off `class`" is a misuse of a parameter documented as a
       gender/lexical-class label. This is a sharper genericity test of
       item 2's design than anything German or Spanish (both classifier-free
       languages) could pose.
     - **Register/politeness (keigo) as `NarrationContext.register`'s first
       real consumer.** Item 3's pronoun-inventory spike concluded T-V
       (`du`/`Sie`, `tú`/`usted`) rides the addressee's own declared subject
       label, not `NarrationContext.register` — because German/Spanish
       politeness *is* a pronoun slot. Japanese politeness is not: `-desu`/
       `-masu` (teineigo), plain form, and the sonkeigo/kenjougo
       honorific-verb-substitution system operate on the *verb*, largely
       independent of whether a pronoun is even present (Japanese is
       pervasively pro-drop, closer to Spanish than German on that axis but
       for a different reason — politeness, not person-recoverability). This
       is the first candidate where `register`'s story-wide, addressee-
       independent design might actually be the *right* shape rather than a
       mismatch — or might reveal that keigo also varies per-addressee
       (formal to a stranger, plain to a friend, in the same scene) the same
       way T-V does, in which case `register` is the wrong axis for it too,
       for the same reason item 3 rejected it for T-V. Either finding
       falsifies something Phase 6 asserted without a non-Indo-European
       language to check it against.
     - **Whitespace-only word boundary, exercised for real.** Item 9 declared
       `heed!()`/`ask!()`/`#[derive(Heed)]`'s space-only tokenization a
       permanent boundary and proved it script-agnostic
       (`heed!("取る {item}", "取る 剣")` already works) — but every
       existing worked example is a *constructed* template with deliberate
       spaces, not real Japanese input, which is written with no spaces at
       all except between clauses at furigana/textbook boundaries. A
       Japanese spike should attempt `heed!()`/`ask!()` against genuinely
       natural Japanese sentences (not gloss-style spaced examples) and
       report whether the "capture the whole unsegmented clause, segment it
       yourself" escape hatch item 9 documents is actually usable for a real
       `ask!()` audience, or whether it pushes so much work onto the caller
       that `ask!()`'s value proposition (parse input, call `answer()`)
       doesn't survive contact with the language most likely to need it.
     - **Design load for a language with almost no inflection.** Japanese
       nouns don't decline for number, gender, or case at all; adjectives
       (`i`-adjectives) conjugate for tense/negation/politeness but not
       agreement. A `ranting-ja` implementation would leave `NounClass`
       largely at `UNSET` (modulo the classifier question above),
       `GrammaticalCase` unused (Japanese case is marked by postpositional
       particles — `が`/`を`/`に` — which are template literal text under
       item 1's word-order boundary, not a hook), and most of
       `inflect_adjective_custom`/`inflect_pronoun_custom` close to
       pass-through. The spike should state plainly whether that is a
       *fine* outcome (a hook surface sized for maximally inflected
       languages naturally degrades to near-nothing for a low-inflection
       one, at zero cost) or a *design smell* (eight hook pairs is a lot of
       API surface for a fork that uses two of them).
   - What it proves: whether `NounClass` generalizes past gender to a
     genuinely different noun-classification axis (classifiers), whether
     `NarrationContext.register` — inert and unused by both existing forks —
     has a real consumer or is the wrong shape entirely, and whether the
     `heed!()` whitespace boundary is livable in practice for the language
     it was most explicitly written with in mind (item 9 names Japanese by
     name three times).
   </details>

4. **Build decision** (no dedicated hours — a synthesis step, not a spike) —
   ✅ **DONE 2026-08-14**; written as the addendum at the end of
   `docs/superpowers/specs/2026-08-14-arabic-falsification-spike.md`, pointed to
   from the Japanese spike's own recommendation section.
   - **Verdict: build both, ordered — new item 11 first, then `ranting-ar`
     (item 5), then `ranting-ja` (item 6).** Both clear item 10/23's bar, on
     different axes, and the expectation stated below survived being checked.
   - **Arabic** clears it on a live defect in a shipped signature (a counted
     noun cannot render a third number, because `Ranting::inflect` was the one
     call item 14 did not widen) and becomes `elide_article_custom`'s first real
     user. **Japanese** clears it on §1's numeral-noun separator plus a
     *confirmation* — `register` read for real, and passing. A confirmation is
     close to the "another working example" the bar rejects; what clears it is
     **item 1**: publishing freezes the trait, `register` has been inert since
     Phase 3, and an audit from inside the repo structurally cannot settle
     whether an unused hook's shape is right. Each lexicon converts a
     never-exercised surface into an exercised one *before* the freeze; building
     neither does not leave that surface unjudged, it freezes it unjudged.
   - **`ranting-ar` blocks on item 11; `ranting-ja` blocks on nothing.** This
     inverts the repo's own *record the hole, then fix it* precedent
     (`ranting_i18n` hole 1 → item 12), so the addendum justifies the exception
     rather than adopting it silently. The discriminator is whether the gap has
     a workaround the lexicon would be **forced to encode**: Arabic has two
     (the `Cell` side-channel, or omitting the dual) and both make
     `tests/holes.rs` a record of something other than the gap; Japanese has
     none, so its hole test is honest against today's code.
   - Two of item 1's ten never-overridden methods stay that way after both
     builds — the eight `_with_context` twins as a class (overriding only the
     twin is the documented sufficient shape) and
     `is_first_person_subject_custom`. The success criteria below already admit
     that outcome; it is stated here rather than discovered at freeze time.
   - Schedules new items 11, 12 and 13, and de-provisionalizes items 5 and 6.

   <details><summary>Original scope (kept for the record)</summary>
   - Read items 1-3 together and decide, in writing (a short addendum to
     whichever spike doc(s) are richer, not a new document), whether to build
     `ranting_ar`, `ranting_ja`, both, or neither, using the same bar item 10
     and item 23 implicitly used: build a language only when its spike found
     *falsification value existing forks cannot supply* — not "another
     working example." Given the findings items 2 and 3 are scoped to
     surface (dual-with-numeral as item 14's first live consumer and
     `elide_article_custom`'s first real user for Arabic; classifier reuse
     of `NounClass` and `register`'s first real consumer for Japanese), the
     working expectation is that **both** clear that bar on different axes —
     but this item exists precisely so that expectation is checked against
     the spikes' actual conclusions rather than assumed here.
   - If a spike recommends against building (for example, if Arabic's dual
     turns out to need a grammar change no one wants to make, mirroring how
     item 4's spike separated "cheap and worth doing" from "correct but too
     expensive"), record that as a legitimate outcome exactly as Phase 6's
     own spikes did, and drop the corresponding build item below rather than
     building it anyway.
   </details>

5. **`ranting-ar` — Arabic reference lexicon** — ✅ **DONE 2026-08-14**;
   `ranting_ar/`, 21 tests (14 in `tests/arabic.rs`, 6 in `tests/holes.rs`, 1
   doctest). Built after item 11, as item 4 required, so its `tests/holes.rs`
   records gaps rather than workarounds.
   - **Both axes item 4 bought it for came through.** The dual renders on the
     noun *and* on the verb agreeing with it, across sound and broken plurals —
     the two halves that disagreed before item 11. `elide_article_custom` got
     its first real user and the spike's open question is answered: the
     two-string signature is sufficient, since the sun-letter trigger is
     `following.chars().next()` and dropping the separator is what "the hook
     replaces all three" already allows.
   - **It found a panic in `ranting` on its first run**, unrelated to Arabic:
     `split_at_find_end` advanced one *byte* past a byte index `rfind` returned,
     so the elision splice sliced mid-codepoint for any article whose last
     character is multibyte. Greek, Cyrillic and CJK were equally affected; it
     survived because the hook had no real user and both other forks' articles
     are ASCII. Fixed, pinned by
     `tests/ranting/property_based.rs::elision_does_not_panic_on_a_multibyte_article`.
     This is the fourth defect found by review or by running code rather than by
     the gates.
   - **Root-and-pattern morphology has no seam consequence**, as the spike
     predicted: a broken plural and a sound one are indistinguishable at the
     seam, both being table rows returned as an opaque `String`.
   - Six holes recorded: bare dual with no numeral (a grammar change, out of
     scope by the number-categories spec's own boundary), no genitive in
     `GrammaticalCase` (`ranting_i18n` hole 3 reconfirmed from an unrelated
     family), bound pronouns, the construct state, VSO word order (a boundary,
     like German's hole 8) and item 12's numeral separator.
     - **Hole 3 got narrower under probing, and is sharper for it.** Bound
       pronouns are not simply unreachable: a template may abut two
       placeholders, so `{0}{`1}` renders `كتابه` correctly. What is missing is
       the power to *rewrite the preceding placeholder's output*, which the
       suffix needs — a feminine `ة` becomes `ت` before it, so `طالبة` + `ه`
       renders `طالبةه` where Arabic needs `طالبته`. That is precisely what
       `elide_article_custom` does for an article, with no pronoun-side
       equivalent. Worth noting as a candidate for a future item, not scheduled.
   - **`inflect_adjective_custom` is deliberately not implemented.** Arabic
     adjectives are post-nominal like Spanish's, so `ranting_es` already proves
     the hook works in that position; a second working example is not a
     falsification. Recorded in the crate README as a scoping choice.

   <details><summary>Original scope (kept for the record)</summary>
   - Same falsification contract as items 10 and 23: own directory
     (`ranting_ar/`), own `Cargo.toml`/`Cargo.lock`, depends on `ranting`
     alone (no `ranting_core`, no `ranting_derive`, no `pub(crate)` item, no
     fork of `handle_placeholder_impl`), a `tests/holes.rs` pinning every
     gap it finds by name rather than working around it, a README following
     `ranting_i18n`'s/`ranting_es`'s shape including an explicit "holes that
     do not reproduce here" section for whichever of the seven German/one
     Spanish holes turn out not to apply to Arabic's grammar.
   - Scope (provisional — item 2 sets the final vocabulary): a small closed
     set of nouns exercising sound and broken plurals, the dual with a
     written numeral, the sun-letter/moon-letter article split, and verb
     agreement across person/number/gender, sized the same way German's
     three-noun/four-verb vocabulary was.
   - Not scoped, per item 2's expected finding: VSO word order (already a
     named-unreachable case in item 1's spec — Arabic would only reconfirm
     it, not discover it) and bare-marker dual with no numeral (a grammar
     change, out of scope per the number-categories spec's own boundary).
   - What it proves: whether item 14's count channel and item 7's elision
     hook, both real Phase 6 surface with zero real-fork mileage before
     this, hold up against the first language actually built to need them.
   </details>

6. **`ranting-ja` — Japanese reference lexicon** — ✅ **DONE 2026-08-14**;
   `ranting_ja/`, 17 tests (10 in `tests/japanese.rs`, 6 in `tests/holes.rs`, 1
   doctest), at item 4's narrowed scope.
   - **`NarrationContext::register` has a real consumer and passes.** Inert since
     Phase 3; `say_with!(formal, "{0 are}", neko)` renders `猫 です` and the
     casual context `猫 だ`, with no pronoun in the template and no entity state
     — the *only* difference is the register. Sonkeigo (食べる → 召し上がる)
     needed no additional signal either: it is a lookup keyed by verb, register
     and `&self`, all three already in the hook. This is the confirmation item 4
     said the audit could not produce from inside the repo, and it is why the
     build was justified before the trait freezes.
   - **First fork ever to override a `_with_context` twin.** The item 1 audit's
     never-overridden list is now **eight**, down from ten before item 5:
     `elide_article_custom` went to `ranting_ar`, `inflect_verb_custom_with_context`
     to this crate. The eight that remain are the seven other twins plus
     `is_first_person_subject_custom`, exactly as item 4 predicted.
   - **Two of eight hook pairs are live, recorded as a finding, not as holes.**
     `inflect` is an identity function here — including item 11's `count`, which
     Arabic needed a day earlier. Two crates landing a day apart and disagreeing
     completely about which half of the surface matters is the apparatus working.
   - **The `NounClass`-as-classifier question dissolved**, as the item 3 spike
     predicted: which counter a noun takes is a property of that noun, so the
     hook's `&self` suffices and `NounClass` stays `UNSET`. Item 2's open-ended
     `&'static str` is therefore *not exercised* by Japanese rather than
     falsified by it; Bantu remains the sharp test.
   - Five holes, numbered 1-5 in the crate README and named `hole_N_*` in its
     `tests/holes.rs`: **1** the numeral-noun separator (item 12, shipped
     **wrong** — `一匹の 猫` — since there is no workaround to encode), with
     sub-case **1b** for the `?`-hidden numeral's surviving leading space
     (findings §1.6), which is why hole 1's obvious escape hatch fails too;
     **2** unspaced prose returning an honest `None`; **3** `ask!()` degenerating
     to a function call on prose; **4** case particles as template text (the
     word-order boundary from an SOV direction); **5** `Register`'s three values
     against keigo's finer gradation.
     - Hole 4 is **cheaper than first written**, and the correction is the
       useful part: a verb has to hang off some placeholder, so a verb-final
       clause looks like it needs a noun repeated at the end — but the hidden
       marker solves it exactly. `"{0}が{1}を{?0 see}"` renders
       `猫が本を見ます`, idiomatic SOV with correct politeness and no
       duplication. The boundary is real; this particular cost of it is not.

   <details><summary>Original scope (kept for the record)</summary>
   **Blocked on nothing**:
   item 12's separator gap has no workaround to encode, so this crate's
   `tests/holes.rs` records it honestly against today's code — that hole test is
   what justifies item 12, not the other way round.
   - Same falsification contract as items 10, 23 and 5 above.
   - Scope, **smaller than this item's original provisional sizing** because
     item 3 §4 found six of the eight hook pairs untouched by Japanese — sizing
     it like German's would be padding: a small noun set with classifiers,
     teineigo verb forms driven by `NarrationContext.register`, and one
     `ask!()` audience over *spaced, command-style* input to pin item 3 §3's
     narrowing (`ask!()` is useful for Japanese command input and not for
     Japanese prose input; against unspaced prose every template collapses to a
     single `{clause}` capture). Unspaced input gets one `heed!()` example
     showing the honest `None` and the `{clause}` escape hatch, the same way
     item 9 treated an honest `None` as success rather than failure.
   - The six unused hook pairs go in its `README.md` as a **finding**, not as
     holes: a surface sized for maximally-inflected languages degrading to
     near-nothing for a low-inflection one is the intended shape, and Japanese
     is the evidence that it degrades cleanly.
   - Not scoped, per item 1's already-locked boundary: SOV word order with
     postpositional particles (named unreachable in item 1's spec already;
     Japanese would only reconfirm it).
   - What it proves: whether `NounClass` survives being asked to carry a
     genuinely different kind of noun classification than gender, and
     whether `NarrationContext.register` — designed in Phase 3, still inert
     after Phase 6 — turns out to have a real consumer at all.
   </details>

7. **Native-language article keywords** — ✅ **DONE 2026-08-14**; design,
   rejected alternatives and implementation notes in
   `docs/superpowers/specs/2026-08-14-language-modularity.md`
   - Added out of order, from a maintainer question about writing templates in
     the author's own language rather than with English keywords. Not a
     falsification item: it removes the last structural English assumption in
     the *authoring* surface, where items 2/3 probe the *inflection* surface.
   - A template may now write its own article (`` {el *=gato} ``); the word is
     handed to `inflect_article_custom`, so the module still picks the form and
     agreement holds (`` {el +*=gato} `` → `los gatos`). `ranting` learns no
     non-English vocabulary — it lives in the fork's hook.
   - Two changes, which must ship together: `ranting_core::ph_ext::parse` runs
     an open-pre-word pass only for input the English pass rejects (so existing
     templates are byte-identical, and `` {w is} `` keeps its noun+post
     reading), and `get_article_or_so`'s `ArticleKind::Other` arm calls the hook
     instead of returning `None` (without which the native word renders as inert
     literal text and gets no agreement).
   - Accepted cost, decided by the maintainer: a misspelled article
     (`` {teh gato} ``) now renders instead of failing to compile. `ranting` is
     not a spelling corrector; the post-noun slot already rendered *and
     conjugated* invented words, so this makes the two slots consistent.
   - Known limitation, pinned by test: the noun needs a case marker.
     `` {el gato} `` is unchanged and still errors, because the open pass runs
     only when the English pass fails and an unmarked two-word placeholder
     parses as noun + post-noun verb.
   - Also verified here, and previously unchecked: `ranting_i18n` and
     `ranting_es` compose in one binary through the public API alone, neither
     needing `ranting_core`/`ranting_derive`.

8. **The `{el gato}` diagnostic** — ✅ **DONE 2026-08-14** (partly; the residue is
   a boundary, not a gap). Full reasoning in
   `docs/superpowers/specs/2026-08-14-language-modularity.md`'s appendix
   "The `{el 0}` diagnostic, as far as it goes".
   - Filed by item 7 as the last rough edge it left behind: after item 7,
     `E0425: cannot find value 'el'` became the *only* remaining failure mode
     for a native-keyword template, and it names a variable the author never
     wrote.
   - **The wording cannot be fixed, and this is now a decided boundary.** The
     message is rustc's, emitted during name resolution of an identifier the
     macro baked; a proc macro cannot intercept or annotate it. Replacing it
     means rejecting the template ourselves, which requires deciding at
     expansion time that `el` is not a variable — undecidable, because
     `` {el gato} `` and `` {person walk} `` are the same shape and the latter
     is live syntax in the test suite. Recognising a list of known non-English
     article words would decide it, and is exactly the vocabulary-in-`ranting`
     item 7 exists to avoid.
   - **What landed instead.** (a) `path_from` takes the template literal's span
     rather than `Span::call_site()`, moving the caret from the whole
     `say!(...)` onto the literal — the entire stable-toolchain win, since
     `proc_macro2::Literal::subspan` (which would narrow to the word) is
     nightly-only and returns `None` on stable. (b) A real defect found while
     investigating, unrelated to the filed one: `syn::Ident::new` *panicked* on
     a hyphenated noun (`` {gato-negro} ``, which `ph_ext`'s word matcher
     admits), surfacing as `error: proc macro panicked`. `check_ident_path` now
     returns a spanned, worded error — decidable, unlike (a), because no Rust
     variable can be named `gato-negro` whatever is in scope.
   - Trap for anyone touching the guard: it must use `syn::Ident::parse_any`,
     not `syn::parse_str::<syn::Ident>`. `Ident::new` accepts keywords, and
     `` {self} `` is live syntax — the strict predicate broke five call sites.
   - Not scoped: a compile-fail harness. This repo has no `trybuild` and
     CLAUDE.md says "integration tests only"; the two diagnostics were verified
     by compiling a scratch crate against a path dependency, and the unit tests
     pin `check_ident_path` directly. Adding `trybuild` is a maintainer call.

9. **`ranting_gaps` — a corpus-driven gap finder** — ✅ **DONE 2026-08-14**.
   Tool in `ranting_gaps/`, its own README, generated output in `failures/`.
   - Reads arbitrary English text and reports what `ranting` cannot inflect,
     ranked by how often the text needs it. Each cause answers four questions:
     what causes it, why it fails (with the string `ranting` *actually*
     renders, obtained by running the inflection), how common it is, and what
     `ranting` would need.
   - **It found the largest defect in the crate.** `Ranting::inflect()`'s
     regular path appends the `plural_end` attribute, default `"s"`. There is
     no `y`→`ies`, no `-es` after a sibilant, no `f`→`ves` — everything English
     does beyond append-`s` must be a row in `data/irregular_plurals.txt`, all
     63 lines of it. So `{+entity}` renders "entitys", `{+match}` renders
     "matchs", `{+city}` renders "citys". Silent wrong output, on the crate's
     most-used feature. `ranting_gaps/src/english.rs::regular_plural` is an
     executable specification of the missing rules, with the counterexamples
     (`day`/`days`, `roof`/`roofs`, `chief`/`chiefs`) pinned in its tests.
     **Fixed by item 10 below.**
   - Also found: hyphenated compounds pluralize the wrong element
     (`mother-in-laws` for `mothers-in-law`), which the `plural_end` escape
     hatch cannot even work around, since the `-s` goes in the middle. Also
     fixed by item 10.
   - Resolved rather than merely reported: the `{can can}` case. `{The can can}`
     renders "Can can hold water." — the article vanishes — but
     `{The *can can}` is correct, so `*` already fixes it. The gap was
     documentation: README.md's only `*` example used to be `"A {*can can}
     contain water."`, which puts the article *outside* the placeholder, and
     that shape renders correctly *without* `*` — the example demonstrated the
     marker in the one position where it changed nothing. **Fixed 2026-08-16**:
     README.md's `*` bullet now shows the actual `{The can can}` vs.
     `{The *can can}` contrast, with both rendered outputs.
   - Distinguishes `gap` from `boundary`, so the permanent word-order boundary
     (item 20 / §2.12) is *counted* but never listed as work. That count —
     1223 occurrences against this repo's own prose, the largest number in the
     report — is evidence about scope, not a bug report.
   - Not scoped, deliberately: invariant plurals (`sheep`, `fish`) and unlisted
     irregular verbs. Both need to infer inflection from attestation patterns
     rather than spelling, and both produce findings a human must hand-filter.
   - Not a falsifier: it depends on `ranting_core` (for `ph_ext::parse` as the
     pre-word oracle) and that is deliberate — see `CLAUDE.md`'s architecture
     section. The `ranting_i18n`/`ranting_es` contract is unchanged.

10. **Regular English pluralization rules** — ✅ **DONE 2026-08-14**. Closes the
    defect item 9 found. `src/language/plurals.rs` gained `regular_plural` and
    `compound_plural`; the new public `ranting::inflect_noun_regular` is what
    derive-generated `inflect()` calls once the irregular table misses.
    - Rules are *orthographic only* — consonant + `y` → `ies`, `-es` after
      `s`/`x`/`z`/`ch`/`sh`, and the `-f`/`-fe` → `-ves` stem lists. The classes
      that need to know what a word means or where it was borrowed from (`hero`
      vs. `piano`, Latin `-us` vs. `bus`, `quiz` → `quizzes`, whose consonant
      doubling is conditioned by stress rather than letters) stay table entries.
      That split is the point: it says what `data/irregular_plurals.txt` is
      *for*, rather than treating it as the place every non-`-s` plural goes.
    - The `-fe` stems (`knife`, `wife`, `life`) and some `-f` stems (`leaf`,
      `loaf`, `wolf`, `thief`, `elf`) are table rows already, so for those
      words the rule only ever fires for **compounds** the table's exact-match
      lookup misses: `bookshelf` → `bookshelves`, `housewife` → `housewives`.
      But `calf`/`half`/`shelf`/`self` have no row — the rule fires on the
      bare word too, not only on compounds built from it. Not redundant with
      the table either way (see `docs/architecture-review-2026-08-15.md` §1.3).
    - **The compatibility contract is the `singular_end`/`plural_end`
      attributes.** Write neither and the rules apply; a struct that *writes*
      either one has stated a rule of its own and keeps the literal
      strip-and-append it always got. That is what stops a non-English impl
      using `plural_end` as an escape hatch from silently acquiring English
      orthography — `tests/ranting/regular_plurals.rs` pins it with a German
      `plural_end = "e"` struct whose name (`Fuchs`) would otherwise take `-es`.
    - **The switch is whether the attribute was written, not its value** — a
      third defect, found by asking what this change does to a non-English
      *caller* rather than to a non-English `Ranting` impl. The first cut tested
      `singular_end.is_empty() && plural_end == "s"`, which made
      `#[ranting(plural_end = "s")]` — literal append-`s`, no orthography —
      indistinguishable from the default, so the one opt-out a German, Dutch or
      Danish **loanword** plural actually needs (`Partys`, `Babys`, where the
      rules say `Parties`) silently got the English rules. `ranting::Noun`, with
      no attributes to write at all, had no opt-out whatsoever. Consonant + `y`
      is the class where the paths diverge, and the only class where these rules
      made previously-*correct* output wrong: bare append-`s` was right for it by
      accident. Fixed structurally — `RantingOptions`'s two fields are
      `Option<String>`, `inflect_noun_regular` takes `Option<&str>` and defaults
      to `""`/`"s"` only inside the literal path, and `= "$"` reads the field
      through a new public `ranting::DeclaredEnding` trait so that a `String`
      field still means "declared" while an `Option<String>` one can say "unset"
      at runtime. That is what `Noun::with_plural_end`/`with_singular_end` use.
      English output is unchanged (the both-absent arm is untouched); both
      falsifiers hand-write `inflect`, so neither was affected either way.
    - **Singularization was deliberately left alone**, and this is the item's
      one open asymmetry. Every inverse rule has a counterexample class spelling
      cannot separate from its positive class: `-ies` → `-y` fixes `cities` but
      breaks `movies` → "movy", which today's naive `-s` strip gets *right*, and
      a `-ves` → `-f` suffix rule turns `olives` into "olife". Trading one wrong
      class for another is not progress, so `{-cities}` still renders "citie".
      Pinned as `singularization_is_deliberately_unchanged` so it reads as a
      decision rather than an oversight. Reopening it needs a lexicon, not a
      rule.
    - **`ranting_gaps/src/english.rs` keeps its own copy of the rules on
      purpose** — it is the differential oracle the probes compare `ranting`
      against, so making it call `ranting::inflect_noun_regular` would make the
      probes agree by construction and report zero findings forever. Both files
      carry a note saying so; the arrangement is the same one `CLAUDE.md`
      records for `PH_EXT` versus `ph_ext`.
    - Acceptance was the tool itself, not a hand-written test: rerunning
      `ranting-gaps` over the same 111k-word corpus drops `regular-plural-rules`
      (188 occurrences) and `compound-head-plural` from the report entirely,
      leaving only the word-order boundary and the pre-word homographs. Both
      probes were kept and their tests inverted into regression guards — an
      empty finding now means "the rules are present and agree", and their
      report text says a future case is a *divergence* between the two
      implementations, not a missing feature.
    - Zero existing tests changed: nothing in the suite had pinned "boxs".
    - Two defects in the first cut, both found by review rather than by the
      gates, both now pinned: (a) the rules matched on a lowercased copy of the
      name and then sliced the *original*, which rendered `CITY` as "CITIes" and
      **panicked** on any name whose byte length changes when lowercased
      (`\u{212A}nife`) — they now run wholly on the lowercased form and restore
      case through the same `apply_case` the irregular path uses, except where
      the rule merely appends, which leaves interior capitals (`iPhone` →
      `iPhones`) intact; `tests/ranting/property_based.rs::prop_inflect_no_panic`
      is the general guard. (b) `ranting_gaps`'s probe skipped any word whose
      render was not bare append-`s` — a valid proxy for "the table decided
      this" only *before* the fix, and afterwards a skip of precisely the words
      the new rules touch, leaving the differential check inert. It asks
      `inflect_noun_irregular` directly now, which is what makes the empty
      report above mean what it says.
    - `data/irregular_plurals.txt` grew the rows the spelling-only rules cannot
      derive: the `-ch`-pronounced-/k/ words (`stomach`, `epoch`, `monarch`,
      `patriarch`, `matriarch`, `eunuch`, `loch`, `tech`), which the sibilant
      rule would otherwise render "stomaches", and `bus`, whose `buses` would
      otherwise singularize to "buse". These are the concrete cost of keeping
      the rules lexicon-free, and they are the point of the table.

11. **`count` on `Ranting::inflect`** — ✅ **DONE 2026-08-14**; **item 5 is
    unblocked.** Acceptance is `tests/ranting/third_number.rs`, six tests built
    on the Arabic spike's own `ArNoun`: `{$0 1}` with counts 1/2/3 renders
    `kitab`/`kitaban`/`kutub`, `{#0 1}` does the same through the spelled-out
    channel, `"{$0 1} and {+1}"` renders the dual **once** (the `Cell` hack's
    failure mode), `None` is shown distinguishable from a count of one, English
    is byte-identical, and `Many` fills the gap from its own length. That file
    is also the first thing in the repo to exercise `inflect` against
    non-English input at all — the §4.7 blind spot, now narrowed rather than
    closed (the *derive-generated* fallback is still unexercised there).
    - One thing the acceptance test had to work around and future items should
      know: **a bare `{noun}` never reaches `inflect`** — with no marker the
      macro renders through `Display`. Pre-existing and unchanged; `{+noun}` is
      how you get an uncounted `inflect` call.
    - Mechanically: trait declaration, three `src/collections.rs` wrappers,
      three derive-generated sites, two `handle_placeholder_impl` render sites,
      and 52 hand-written impls across tests and both falsifiers. The
      `ArticleKind::AAnSome` call passes **`None`** rather than the count — it
      asks what the noun's singular *spelling* is so a/an can be picked from
      its first letter, already forces `to_plural = false`, and loses no signal,
      since item 14 gave `inflect_article_custom_with_context` its own `count`.

    <details><summary>Original scope (kept for the record)</summary>
    Add `count: Option<PlaceholderCount>` to
    `fn inflect(&self, to_plural: bool, uc: bool, case: GrammaticalCase)` — the
    same type and from the same source as item 14's — so a counted noun can
    render a third morphological number. Item 14 widened five hook pairs and
    `Ranting::inflect`'s `case: GrammaticalCase` in one commit but left
    `inflect` itself count-less, which is why `{$n kitab}` with `n = 2` renders
    the Arabic plural while every hook that *agrees* with that noun sees
    `PlaceholderCount { value: 2, .. }` — grammatical-looking output, wrong in
    one word. `docs/superpowers/specs/2026-08-13-number-categories.md`'s
    inventory missed it because `inflect` is not a `_custom` hook, and now
    carries a correction section saying so.
    - **Not** the `Cell` side-channel that smuggles the count from
      `inflect_numeral_custom` into `inflect`. It was tried, it works, and it is
      not an answer: it contaminates later placeholders in the same template
      (`"{$n kitab} and {+kitab}"` renders the dual twice), depends on
      undocumented hook call order, and makes a `&self` trait stateful.
    - English-preserving by construction — English has no third number, so
      `None`/any value renders identically. Breaking for a fork that hand-writes
      `inflect`, which is both falsifiers plus every derive-generated impl.
      **Item 14 already widened this exact signature**, adding `case` in the
      same commit as the five hook pairs — so this is a second widening of a
      function that could have taken the count then, not a fresh precedent to
      follow. Item 14's commit is where to look for how to land it mechanically
      (two `get_plurality_fns` sites in `ranting_derive/src/ranting_impl.rs`,
      the trait declaration in `src/lib.rs`, both falsifiers' hand-written
      impls).
    - CLDR categories stay **out** (`2026-08-13-number-categories.md`); this
      hands a fork the raw count, exactly as item 14 did elsewhere.
    </details>

12. **The numeral-noun separator** — ✅ **DONE 2026-08-14**. `Ranting` gained an
    eighth `_custom` pair, `elide_numeral_custom`/`_with_context`: the
    numeral-side twin of item 7's article hook, same post-assembly splice, same
    replace-all-three contract. `ranting_ja` overrides it in the same commit, so
    it never spends a day on item 1's never-overridden list, and 「一匹の猫」
    renders correctly — that crate's hole 1 is struck.
    - **Ordering was the one real design question.** Rendered order is
      `[article][numeral][noun]`, and the numeral splice runs **first** because
      it is the inner boundary: every byte it rewrites is at or after
      `article_span`'s end, so that span stays valid, while the reverse order
      would move the numeral out from under this splice's own span. Getting it
      wrong misplaces text silently rather than panicking — §1.7's lesson — so
      `ranting_ar/tests/arabic.rs` asserts it, that crate being the only place
      in the repo where both splices can fire.
    - Not called for a hidden numeral, the same gate a hidden noun gives the
      article hook. English output is unchanged: the default returns `None`.
    - The chosen shape is the second of the two candidates below. The first —
      passing the separator to `inflect_numeral_custom` — would have re-signed a
      hook four crates already implement, to no benefit.

    <details><summary>Original scope (kept for the record)</summary>
    `handle_placeholder_impl` pushes a hard-coded space between
    the rendered numeral and the noun, and no hook is offered it — so Japanese's
    「一匹の猫」 is unreachable and 「一匹の 猫」 is what renders. Exactly parallel
    to Arabic's article-bound-to-noun case, except that one got item 7's
    `elide_article_custom` and this one got nothing. Two candidate shapes, both
    from item 3 §1: pass the separator to `inflect_numeral_custom` and honor an
    empty return, or add a numeral-side splice matching `elide_article_custom`'s
    post-assembly design. The second is the closer precedent — that hook exists
    *precisely* so a fork can drop a separator — and keeps the numeral hook's
    signature alone.
    - The existing escape hatches are all worse than the gap and none of them
      substitutes for this: `{?$n neko}` hides the numeral and still leaves a
      leading space (item 13), writing the numeral as template literal text
      makes item 8 dead for that fork, and squeezing spaces after `say!()`
      returns would corrupt Latin text in the same template.
    </details>

13. **Two residues both spikes left** — ✅ **DONE 2026-08-14**.
    - **`{?$n noun}`'s stray space is fixed**, not documented as intended.
      `say!("I see {?$0 boot}", 2)` renders `"I see boots"`. A hidden numeral
      sits between two separators — its own leading one and the noun's — and
      with nothing rendered between them the pair has to collapse to one, the
      same way a zero-length article's does. **Which one survives matters**:
      keeping the leading space and swallowing the noun's is what leaves
      `{The ?$n noun}` rendering `"The raven"`, since there the leading space is
      the article's only separator; the first cut swallowed the wrong one and
      produced `"Theraven"`. `NumeralSpec` grew a `hidden: bool` so the slot is
      representable at all — it used to be simply absent from the spec, which is
      what made the space unreachable. The two `tests/ranting/numeral.rs` pins
      that asserted the defect are re-pinned to the correct output.
    - **"Story-wide" is gone** from `src/narration.rs` and
      `.claude/rules/extension-hooks.md`, replaced with the point it was
      obscuring: `NarrationContext` is **per call**, and a different context per
      utterance is ordinary usage. `ranting_ja`'s
      `register_can_vary_per_utterance_within_one_scene` is the runnable
      version.

    <details><summary>Original scope (kept for the record)</summary>
    - **`{?$n noun}` renders a double space** (`"I see  boots"`, `"есть  стол"`).
      It is currently *pinned* by `tests/ranting/numeral.rs`, so it reads as
      intended behavior rather than as a defect; filed as
      `docs/architecture-review-2026-08-14.md` §1.6. Cosmetic for English, on
      the critical path of item 12's only workaround for Japanese. Decide it
      either way — fix and re-pin, or state in the test why the space is
      intended — but stop letting the test assert it silently.
    - **"Story-wide" is the wrong word for `NarrationContext`** in the docs and
      in `.claude/rules/extension-hooks.md`. It describes the *intended* use,
      not a constraint the type imposes: the context is per-call, so keigo
      varying per addressee within one scene is expressible today by
      constructing a different context per utterance. The current wording
      invites a fork to conclude such variation is out of scope, which this
      ROADMAP itself nearly did in item 3.
    </details>

14. **`ranting_es_gaps` — a corpus-driven agreement checker for `ranting_es`** —
    ✅ **DONE 2026-08-16**. Not part of item 4's original build decision; added
    later from a brainstorm exploring whether `ranting_gaps`'s corpus-driven
    approach generalizes past English. Tool in `ranting_es_gaps/`, its own
    README, `failures/`-shaped generated output (not committed — no fixed
    corpus ships with the tool, see below).
    - **It cannot be `ranting_gaps` for Spanish.** `ranting_gaps` nominates
      candidate words from an open English vocabulary and finds bugs in
      `ranting`'s *general* regular-pluralization rule. `ranting_es`'s
      lexicon is a closed set — 4 nouns, 4 verbs, 3 adjectives, numerals
      `0..=12`, every gender/conjugation hand-listed rather than
      suffix-generated (`problema` is coded masculine specifically to prove
      there's no `-o`/`-a` gender-guessing heuristic to test) — so there is
      no general rule to differentially check against brand-new words.
    - **Enumerate-then-attest, not nominate-then-filter.** Every comparison
      the tool makes (4 nouns × article/number combinations, 3 adjectives ×
      4 nouns × number, 4 verbs × 6 persons, 2 prepositions × 8 article
      forms) is enumerated directly from `ranting_es::lexicon` up front. A
      corpus only grades each enumerated case's confidence (`certain` vs.
      `attested`) — it never decides whether a case exists, so there is no
      `--min-occurrences`/`--unattested` filtering the way `ranting_gaps` has.
    - Five probes: `article_agreement`, `adjective_agreement`,
      `preposition_fusion`, `verb_person` (all `Kind::Gap`) and
      `lexicon_coverage` (`Kind::Boundary` — measures how much of a given
      corpus's noun phrases fall outside the closed lexicon; not a bug list).
      All four `Kind::Gap` probes report zero mismatches against `ranting_es`
      as it stands today, pinned by each probe's own differential test.
    - **Not a falsifier**, same shape as `ranting_gaps` one level down: it
      depends on both `ranting` (trait/type surface the probes call directly)
      and `ranting_es` (system under test), which does not relax
      `ranting_es/Cargo.toml`'s own `ranting`-alone contract. Picked up by
      `scripts/hook_audit.sh`'s fork-detection grep for the same reason
      `ranting_gaps` is (it depends on `ranting` directly) and contributes
      zero to every column there, since it calls hooks rather than defining
      them.
    - **No corpus ships with the tool.** A hand-curated Spanish sample built
      to contain the closed lexicon's words in agreeing forms would be
      circular — writing `la casa negra` *because* the expected output is
      already known defeats the point of an independent oracle. Point it at
      real Spanish text (a Tatoeba dump, a Wikipedia extract) instead.
    - Full rationale, what it deliberately cannot do, and the `NOT_HOLES`
      checklist of `ranting_es/README.md` behaviors it must never misreport
      as bugs: `ranting_es_gaps/README.md`.

### v1.4 Success Criteria (finalized by item 4, 2026-08-14)
- Items 1-3 answer, in writing, whether Arabic and/or Japanese would falsify
  something German/Spanish could not, before any lexicon code is written
- The unused-hook list from item 1 either gains a real consumer through
  items 5/6, or Phase 7 states explicitly why it remains legitimately unused
  (a hook whose only job is an English-preserving default for a construction
  no scoped fork needs is not automatically a defect). Item 4 settled which:
  `elide_article_custom` gains one through item 5 and `register` through
  item 6; the eight `_with_context` twins and
  `is_first_person_subject_custom` stay unused, legitimately, and this
  criterion is met by saying so before the trait is frozen — not after
- Zero behavioral change to existing `say!()`/`say_with!()` output, exactly
  as every Phase 6 item required — additive, English-preserving, verified by
  the existing suite passing unchanged
- Any new gap a built lexicon finds is recorded in that crate's README as a
  numbered hole, not worked around — the same falsification contract items
  10 and 23 used

---

## Phase 8 — English grammar depth (scoped 2026-08-15, not started)

*Goal: Phases 6 and 7 spent four falsifier crates asking whether the hook surface
carries enough signal for a **non-English** implementation. This phase asks the
question that was never asked in the other direction: whether the English the crate
ships can carry a **complex sentence**. A grammarian reviewed the placeholder surface
end to end for that and found two distinct kinds of answer — constructions with no
channel at all (below), and six places where `ranting` renders something wrong from
input the caller wrote correctly (`docs/architecture-review-2026-08-15.md` §§1.5-1.12).
The defects are item 6; items 1-5 are the missing channels, ordered by how often a
writer of ordinary prose hits them.*

**No version number yet.** Items 1-3 each change what an existing template can render
and item 6 changes what existing templates *do* render, so how this phase slices into
releases depends on which items are taken — that is a decision for whoever schedules
it, not a number to guess now.

**What this phase is not**: a re-opening of the word-order boundary. Two candidates
the review raised are declined for that reason and are named under Non-goals below;
they are cited, not re-litigated (Key Architecture Decisions, "Word order lives in the
literal template").

1. **The participle channel, and passive voice with it** *(largest gap; parts already
   exist)* — `ranting_core::verb_conjugate::to_past_participle` is already built and
   already fed by the irregular tables, but the only markers that reach it are `%`
   (present perfect) and `<%` (past perfect). So a template cannot compose
   `be` + participle for a *variable* verb: passive voice ("the sword **is taken**"),
   future perfect ("**will have taken**"), perfect progressive ("**has been picking**")
   and the conditional perfect are all hand-written today, for a verb whose form the
   crate already knows how to produce. Passive alone is pervasive in the descriptive
   prose this crate targets. Shape suggested by the review: a fused marker in the
   family the `*=`/`*@` work established (`=%verb` → "is/are seen", `>%verb` → "will
   have seen"), which reuses `PH_EXT`'s existing fused-marker precedent rather than
   adding a grammar level. Agreement on the auxiliary is already correct machinery.

   **PROPOSED (2026-08-15 spike — NOT implemented; maintainer decision needed).**
   `docs/superpowers/specs/2026-08-15-participle-channel.md` recommends five
   enumerated fused spellings as new `TenseMarker` variants — `=%` (present
   passive, "is taken"), `<=%` (past passive), `>%` (future perfect), `%=`
   (present perfect progressive, "has been picking"), `<%=` (past perfect
   progressive) — all composed from already-taken `post` characters, so
   `PH_EXT`/`ph_ext` need **no** grammar or parser edits (each spelling is a
   compile error today, making English byte-identity hold by construction), and
   the passive's auxiliary agreement reuses `AuxiliaryVerb::IsAre`/`WasWere`
   unchanged. Three decisions are left for sign-off, per the spike: the spellings
   themselves; the `ctx.tense` × voice interaction under `say_with!()` (the spike
   recommends tense-axis-only overrides that preserve voice — the naive extension
   silently renders a passive placeholder active); and whether all five land
   together (recommended) or the passive pair first. The sigil grammar is Locked,
   so nothing ships until a maintainer rules.
2. **A subjunctive escape hatch** *(fixes the one place the crate damages correct
   input)* — the defect is §1.5; the *feature* question is what the fix should be.
   Indicative-vs-subjunctive is a property of the clause (`if`, `wish`, mandative
   `demand that`), which lives in the caller's template and is not recoverable from
   the verb, so a smarter conjugator cannot do it. The two shapes worth spiking are a
   verbatim marker ("this verb form is final, inflect nothing") — which is cheap,
   general, and useful well beyond the subjunctive — and a `NarrationContext` mood
   flag, which is not, since mood varies per clause and `NarrationContext` is per
   call. Retires the "Subjunctive mood and hypotheticals" bullet from
   *v1.4+: Advanced Features* below, which named the gap without knowing the crate
   actively overwrites it.

   **PROPOSED (2026-08-15, not decided, not implemented)** — design spike at
   `docs/superpowers/specs/2026-08-15-verbatim-verb-marker.md` recommends the
   verbatim-marker shape: a new post-noun marker character (shortlisted `;`/`|`/
   `&`/`/`, tiebreak favors `;`) baked as a new `PostSpec::Verbatim(&'static str)`
   variant that bypasses person/number agreement for the marked word, e.g.
   `{=i ;were}` → `"I were"`. The spike rejects the `NarrationContext` mood-flag
   shape outright (mood is per-clause; `NarrationContext` is per call, and one
   `say_with!()` invocation can mix clauses of different mood). It leaves two
   things for a maintainer to actually decide, since the sigil grammar is Locked
   and this spike does not change any code: the exact character, and whether
   `PostSpec::Verbatim` bypasses `inflect_verb_custom_with_context` entirely or
   still calls it with a "don't touch" signal (the latter is a hook-signature
   break). Until one is chosen and implemented, §1.5 stays open.
3. **Agreeing quantifiers, and the mass/count distinction** — `ArticleOrSo` stops at
   a/an/some/the/these/those, so *no*, *every*, *each*, *either*, *much*/*many* and
   *less*/*fewer* have no channel and a quantified noun phrase is hand-assembled.
   Two sub-parts, separable: (a) quantifiers that agree in number, which is the
   existing `these`/`those`→`this`/`that` machinery pointed at more words — *no* is
   number-transparent ("no item" / "no items"), *every*↔*all* swaps on number; and
   (b) a mass/count flag on the entity (`#[ranting(mass)]`, the shape `gender` already
   uses), without which `{a 0}` on "information" renders "an information" and nothing
   can pick *much* over *many*. (b) is what makes (a) correct rather than merely
   available. Zero-count idiom ("there are **no** items") is expressible today via
   `` {?#n +items} `` but is undiscoverable; (a) is also its ergonomic fix.
4. **Ordinals** — `#var` spells cardinals only, so "the **third** attempt" cannot come
   out of a placeholder. Pure word-form inflection with no word movement, i.e. squarely
   inside the boundary. Cheapest of the five and it has a second constituency: ordinals
   agree in gender in Spanish and Arabic, so an `ord` variant of `#` handed to
   `inflect_numeral_custom` (which already carries `NumeralStyle` and a real `count`)
   gives `ranting_es`/`ranting_ar` something to override, against the never-exercised
   surface §4.1 records. `english_numbers::convert_no_fmt`'s behavior on negatives is
   already guarded (item 6's §1.9), but its unhyphenated "twentyone" is not — an
   ordinal speller inherits that spelling question.

   **PROPOSED (2026-08-15, not decided, not implemented)** — design spike at
   `docs/superpowers/specs/2026-08-15-ordinal-numerals.md` recommends a doubled
   numeral marker, `` {the ##n attempt} ``, baked as a new `NumeralKind::Ordinal`
   and mirrored into a new public `NumeralStyle::Ordinal` carrying the same real
   `count: Option<i64>` the cardinal channel already carries. `##` cannot parse
   today (`match_nr` requires `\w` after `#`), so no existing template changes.
   The spike states plainly that the enum is public, re-exported and not
   `#[non_exhaustive]`, so the variant is a **major-version break**: all four
   falsifiers match it exhaustively with no wildcard and would stop compiling,
   as would any downstream `match`; `#[non_exhaustive]` is itself breaking and
   trades the error for a silently-swallowing `_` arm. Agreement decouples —
   `as_pl` falls through to `noun.is_plural()` ("the third attempt", not
   "attempts") while the count still flows, which is what Spanish/Arabic ordinal
   gender agreement needs. Rejected: a standalone free character (spends one of
   eight for a variant of an existing marker) and a `:fmt`-style `ord` suffix
   (structurally impossible — `PH_START` splits `:fmt` off before `PH_EXT`).
   Three things are left for a maintainer to decide, since the sigil grammar is
   Locked and this spike does not change any code: whether the digit ordinal
   (`$$var` → `"3rd"`) is taken in the *same* break rather than a later second
   one; whether the stringly-typed `plurality` dispatch is retyped alongside it
   (four of the ten change sites exist only because it is a `&str`, and two of
   those fail silently — `` {##n attempt} `` would otherwise render a cardinal,
   and agree plural); and whether `nr` gains a one-repetition restriction while
   its alternation is widened.
5. **Adverb derivation** — quick→quickly, happy→happily, is in-place word inflection of
   exactly the kind the crate already does for degree (`!`/`!!`), and has no channel.
   Lowest priority of the five: the adjective slot is post-noun only, so the sentence
   positions an adverb actually occupies are frequently ones a template writes as
   literal text anyway. Scoped here to be *decided*, possibly declined — it may be a
   channel that exists and is rarely reachable, which is the same shape as
   `ranting_i18n`'s prenominal-adjective hole.
6. **The recorded defects** — `docs/architecture-review-2026-08-15.md` §§1.5-1.12,
   each verified against the source: seven defects plus one agreement question left
   as a maintainer's call (§1.12). Five change rendered English and are therefore
   **breaking** under the byte-identity invariant, so they want one release between
   them rather than five:
   - §1.5 subjunctive `were`→`was`, both persons, pinned by a regression test at
     `english.rs:555` (**breaking**; the fix is item 2, the two are the same work)
   - §1.6 phrasal verbs take third-person `-s` on the last word — "He pick ups"
     (**breaking**; bare present only, tense-marked forms are already correct)
     — ✅ **done 2026-08-15**: the real split was in `src/lib.rs`'s `PostSpec::Verb`
     handling, not `inflect_verb` itself — it cut the placeholder's post-noun text at
     its *last* whitespace and conjugated the trailing particle instead of the verb.
     It now splits at the *first* whitespace, conjugating the head word and
     re-appending the remainder unchanged; a single-word verb is byte-identical to
     before. See CHANGELOG.md's Changed (breaking) entry and
     `tests/ranting/verb_tense.rs`.
   - §1.7 plural proper names get `'s` — "the Joneses's", because `is_name` looks at
     the first character and nothing else (**breaking**; smallest of the six)
     — ✅ **done 2026-08-15**: `adapt_possesive_s` no longer consults `is_name` at
     all — the bare apostrophe now fires whenever the noun is plural, name or not,
     and `'s` otherwise, so `"Myles's"` (a singular name) is unaffected. `is_name`
     was deleted as dead code. See CHANGELOG.md's Changed (breaking) entry and
     `tests/ranting/possessive_apostrophe.rs`.
   - §1.10 space-separated compound nouns pluralize on the tail — "attorney generals",
     where the hyphenated spelling is already correct (**breaking**; the head-detection
     lists exist, the risk to bound is ordinary modifier + head)
     — ✅ **done 2026-08-15**: `compound_plural` now also splits on a single space, gated
     behind the same closed `PREPOSITIONS`/`POSTPOSED_ADJECTIVES` lists the hyphenated
     form already used, rebuilding with whichever separator the input used. "red house",
     "post office" and "fire engine" are pinned as still tail-pluralizing. See
     CHANGELOG.md's Changed (breaking) entry, `src/language/plurals.rs`'s own tests, and
     `tests/ranting/regular_plurals.rs`.
   - §1.8 `{=0 walking}` → "She walking", silently, and is *pinned* as a test. Nothing
     to fix at runtime — what is missing is a compile-time diagnostic, which the macro
     has the string to produce (**not breaking**)
   - §1.9 a negative `#var` spells "negativeone" (**not breaking**; upstream, guard it)
     — ✅ **done 2026-08-15**: a private `spell_count` in `src/lib.rs` spells the
     magnitude and prefixes `"minus "`, inside the one string the numeral hook may
     still replace wholesale. "minus twentyone", not "minus twenty-one" — upstream
     spells positive 21 as "twentyone" and non-negative output is unchanged.
     `i64::MIN`'s pre-existing upstream panic is deliberately left as it was.
     Pinned by `tests/ranting/numeral.rs`
   - §1.11 a sentence-initial numeral spends the placeholder's `uc` on the **noun** —
     `` {#n item} `` renders "two Items fell.", `` {$n item} `` renders "2 Items fell.",
     while `` {the #n item} `` is correct because the article takes the capital
     (**breaking**; found 2026-08-15 spot-checking §1.9's fix, but older than the
     review). Two fixes, not one: `#var` should capitalize the spelled numeral,
     `$var` should drop the `uc` rather than pass it on. `inflect_numeral_custom`'s
     doc at `src/lib.rs:2454` states the current behavior as policy and has to change
     with it
     — ✅ **done 2026-08-15**: capitalization stays on the crate side of the hook
     (no new `uc` parameter) — the `hidden: false` numeral branch in
     `handle_placeholder_impl` now spends `uc` on the rendered numeral itself when
     `uc && sentence_start && !rendered.is_empty()`, capitalizing a spelled `#var`
     and dropping `uc` outright for a digit `$var`. Gated on `sentence_start` (not
     `uc` alone) so a mid-sentence forced-uppercase placeholder (`` {^#n item} ``)
     is untouched, and only on the `hidden: false` branch so a hidden numeral
     (`` {?$n item} ``) still lets `uc` fall through to the noun as before. See
     CHANGELOG.md's Changed (breaking) entry and `tests/ranting/numeral.rs`.
   - §1.12 a negative `#var` count agrees plural — "minus one items", from
     `as_pl = count != Some(1)`. **Recorded as a maintainer's call, not scheduled**:
     the plural is right for measures ("minus one degrees") and wrong for countables,
     which is the mass/count split item 3 part (b) would supply. Deciding agreement
     from the count rather than the rendered word is correct either way and must stay

**Non-goals, with the decision they cite**
- **Relative and interrogative pronoun case** (who/whom/whose). The case machinery
  exists and the review rated the gap real, but selecting a relative pronoun requires
  knowing its antecedent and its role in a subordinate clause — knowledge that lives in
  the sentence's structure, not in the entity or the placeholder. Declined on the same
  grounds as Phase 6 item 1's locked boundary, not on cost.
- **Reciprocals** (each other / one another), **imperatives**, **indefinite pronouns**
  and **existential *there***: all correctly literal template text today. The review
  confirmed this rather than finding a gap.
- **Modal conjugation** — modals are invariant in English and the crate models that
  correctly; there is nothing to add.
- **"Each of the boys is"** — notional agreement across a partitive. The `OF` heuristic
  handles the partitive *head* already; a general treatment needs the clause, see the
  first bullet.

---

## Post-v1.2: Future Directions

### v1.3.0+: Beyond Phase 6
- **`ranting-i18n` Companion Crate** — now scoped as
  [Phase 6](DONE.md#phase-6--v130--internationalization-foundations) in DONE.md, which owns
  the full breakdown. Summary: `ranting` gains the signals a non-English
  implementation needs (noun class, adjective agreement, orthography, elision,
  numerals) and answers the word-order question in writing; the companion crate
  itself lands as Phase 6 item 10, one German reference lexicon whose job is to
  falsify the claim that items 1-9 are sufficient. Multi-language breadth
  (French, Spanish, Japanese, …) follows only after German proves the mechanism.
- **`ranting-if` (or similar) Companion Crate — Inform7-style object disambiguation**
  (proposed 2026-08-13, not scoped): resolves which candidate object among
  several free-text input refers to, using "likely"/"unlikely"-weighted rules
  the way Inform 7's `Understand` rulebook does (e.g. a "talk to" action being
  far more likely to target a person in scope than a stone). Builds on
  `ranting`'s `Answerable` trait (Phase 5) and `heed!()`'s capture parsing,
  but needs a candidate registry, a scoring/priority mechanism, and rule
  authoring syntax that have no home in `ranting` itself — `ask!()` only ever
  targets one statically-known `audience` expression per call site, by design.
  A natural fit for a `ranting`-adjacent crate rather than a `ranting` feature.

### v1.4+: Advanced Features (Community-Driven)
- Dialogue formatting with automatic punctuation and breaks
- Pluralization of entire phrases (not just nouns) — partly overtaken by Phase 8 item 6:
  the *compound noun* half of this is a defect today (§1.10, "attorney generals"), not a
  future feature. What remains here is pluralizing an arbitrary phrase, which is not the
  same problem
- ~~Subjunctive mood and hypotheticals~~ — **superseded by Phase 8 item 2**, which
  found the crate does not merely lack the subjunctive: it rewrites `were` to `was`
  in both persons and offers no way to opt out
- Register and dialect specialization (formal vs. informal, archaic, etc.) via context system from v1.1
  — the overlap with [Phase 6 item 3](DONE.md#phase-6--v130--internationalization-foundations) is now
  settled: T-V pronoun selection (`du`/`Sie`, `tu`/`vous`) rides the addressee's **own declared
  subject label** (`Sie`/`vous` are pronoun slots, not modifiers), so neither
  `NarrationContext.register` nor a new per-addressee channel owns it; `register` stays story-wide
  and inert, a documented fallback for the indifferent case only. This bullet therefore covers only
  English-internal register/archaism, which needs no new pronoun inventory. See
  `docs/superpowers/specs/2026-08-13-pronoun-inventory.md`.
- Performance optimizations (cached inflection, const generics)

---

## Key Architecture Decisions ✅

| Decision | Status | Notes |
|----------|--------|-------|
| Two-crate split (ranting + ranting_derive) | ✅ Complete (v1.2) | `ranting_core` shared rlib extracted (Phase 4 item 1, serde/serde_derive pattern); all build.rs copy/symlink sharing deleted |
| Verb table codegen via build.rs | ✅ Complete | Single source of truth: data/irregular_verbs.txt; codegen moves into `ranting_core` in v1.2 |
| Pronoun/article/verb tables → exhaustive match | ✅ Complete | Exhaustive `match` dispatch with `#[deny(...)]` guards; no wildcards; permanent regression tests for string values |
| Derive macro attributes (4 core + 3 cosmetic) | ✅ Complete | subject, name, singular_end, plural_end (core) |
| Compile-time parsing + runtime inflection | ✅ Locked | Catches syntax errors early; enables extensibility. Seam becomes typed (`PlaceholderSpec`) in v1.2, replacing `caps: [&str; 5]` + `~TENSE~` sentinel |
| Documentation (Tutorial + Cookbook) | ✅ Complete | 30-40 min tutorial, 10 practical recipes |
| Placeholder syntax (full grammar support) | ✅ Locked | Sigil grammar is the crate's identity; keep it. v1.2 swaps the `PH_EXT` regex recognizer for a tokenizer (better error spans) without changing the grammar |
| Built-in English rules (extensibility in v1.1) | ✅ v1.0; 🎯 v1.1 | Free functions now; trait methods in v1.1 |
| Irregular noun plurals codegen | ✅ Complete (v1.1); ✅ wired to `Ranting::inflect()` (2026-08-13) | Single source of truth: data/irregular_plurals.txt; `english::inflect_noun_irregular` now delegates to `get_plural`/`get_singular`, so `Ranting::inflect()`'s irregular-noun path uses them (with `apply_case` case-preservation). `ranting_derive`'s own copy is still unwired — no compile-time call site exists — see docs/architecture-review-2026-08-13.md |
| Context-aware runtime tense | ✅ Complete | `say_with!(context, ...)` + `NarrationContext`/`Tense`; unblocks Recounting M9 (tense portion) |
| Context-aware runtime viewpoint | ✅ Complete | `NarrationContext.narration_person` + `Person`; scoped to first-person-declared (`I`/`we`) nouns only; unblocks Recounting M9 (viewpoint portion) |
| Narration context threading (register/dialect) | ✅ Complete | `NarrationContext.register`/`.dialect` are inert in-crate; reachable via 3 new `Ranting::*_with_context` hooks (`ctx` as parameter, never entity-owned), defaulting to the pre-existing hooks |
| Consolidate english_shared.rs | ✅ Complete → superseded (v1.2) | Single canonical copy + build.rs copy solved the drift; `ranting_core` extraction (Phase 4, item 1) replaces the copy mechanism outright |
| Stringly-typed `subject: &str` in public API | ✅ Complete (v1.2) | Phase 4 item 4: `SubjectPronoun` public, typed field in `Noun`, non-panicking `Noun::try_new`; invalid subjects unrepresentable instead of panicking |
| `ack!()`/`nay!()` expand to hidden `return` | ✅ Complete (v1.2) | Phase 4 item 5: reworked to plain `Ok(say!(...))`/`Err(say!(...))` expression forms, usable anywhere an expression is valid |
| Word order lives in the literal template, not the placeholders | ✅ Locked (v1.3, Phase 6 item 1) | **Permanent boundary**: `ranting` inflects within a template and will not reorder across placeholders — nor within one (the pre→`nr`→noun→post assembly is fixed too). Non-English callers supply per-language templates. Numbered slots + reorder metadata rejected (blocked by the compile-time `format!()` seam); `sentence!()` syntax-tree API rejected (works, but abandons the sigil grammar). See `docs/superpowers/specs/2026-08-13-word-order-feasibility.md` |
| Noun gender / noun class as an entity property | ✅ Complete (v1.3, Phase 6 item 2) | Open-ended `&'static str` class label, not a closed Masc/Fem/Neut enum — Bantu has a dozen-plus classes, Danish has common/neuter. Threaded like `GrammaticalCase` (commit `11d531ed`) |
| `SubjectPronoun` is a closed English enum | ✅ Locked (v1.3, Phase 6 item 3) | **Stays English-only, unchanged**: the parallel fork-owned pronoun set already exists (`inflect_pronoun_custom`/`inflect_verb_custom`, consulted *first*; `subjective() -> &str` is an uninterpreted channel), so option (c) is doc-only and breaks nobody. Extending the enum is semver-major for every downstream `match` (re-exported, not `#[non_exhaustive]`); an open channel trades a build failure for silent `it`/`its`/`itself` at five `unwrap_or(It)` sites and reverses Phase 4 item 4's invariant. T-V (`du`/`Sie`, `tu`/`vous`) is a pronoun slot, so it rides the addressee's own subject label — `NarrationContext.register` stays story-wide and inert, a documented default only. See `docs/superpowers/specs/2026-08-13-pronoun-inventory.md` |
| `GrammaticalCase` is locked at English's five-marker inventory | ✅ Locked (v1.3, Phase 6 item 24) | **Stays exactly seven variants (`Name`/`Subjective`/`Objective`/`PossessiveDeterminer`/`PossessivePronoun`/`Reflexive`/`Hidden`), unchanged**: it mirrors which of the five placeholder markers (`=`/`@`/`` ` ``/`~`/`%`) a call site used, not a general syntactic-case representation — German's four cases cross-cut those five markers, so no re-slicing recovers a clean mapping. New variants/markers are semver-major for every exhaustive match on `CaseKind`/`GrammaticalCase` (rejected sub-options a1/a2); an open string-typed channel is likewise breaking on `inflect_article_custom`/`_with_context` and doesn't close the gap either way (option b). A fork past two-way case marking carries its own case on the entity instead — `GermanNoun::in_case` (`ranting_i18n`) — the same pattern `NounClass` already uses. Hole 3 in `ranting_i18n/README.md` stays open by design. See `docs/superpowers/specs/2026-08-13-grammatical-case-inventory.md` |
| Preposition-article fusion is a dedicated hook, not a grammar change | ✅ Complete (v1.3, Phase 6 item 26) | `Ranting::inflect_preposition_custom`/`_with_context`, fed the literal word `ranting_derive::parse_str_params` now captures immediately before a placeholder (`PlaceholderSpec::preposition`) plus the rendered article, called at the same post-assembly point as `elide_article_custom` and tried first. Closes `ranting_i18n/README.md` hole 7 and `ranting_es/README.md` hole 1 — the sole hole Spanish's independent lexicon hit. The pre-noun placeholder slot itself (item 25's option (a)) stays a closed English word list, unwidened; the fix never needed the preposition to be inside the placeholder. See `docs/superpowers/specs/2026-08-13-preposition-fusion.md` |
| Number is `bool` throughout the hook signatures | ✅ Complete (v1.3, Phase 6 items 4 + 14) | Arabic dual / Slavic paucal / CLDR categories don't fit. Item 4's spike stated the cost before it was paid; item 14 then paid it once, adding `count: Option<PlaceholderCount>` alongside the bool to five hook pairs (and `case` to `Ranting::inflect`) in one signature break. The bool stays authoritative for English agreement; CLDR categories stay out of the crate. See `docs/superpowers/specs/2026-08-13-number-categories.md` |
| English orthography, phonology and numerals hard-coded | ✅ Complete (v1.3, Phase 6 items 5-8) | Adjective agreement, `uc_1st_if`/`apply_case`, `a`/`an` elision, and `#var` spelling all became hooks with English-preserving defaults: `inflect_adjective_custom`, `capitalize`/`OrthographyRole`, `elide_article_custom`, `inflect_numeral_custom` |
| GPL-3 via `license-file` | ✅ Complete (v1.2) | Relicensed to plain `license = "MIT"` 2026-08-13 (copyright holder's choice, differs from the dual-license recommendation in [the historical license analysis](DONE.md#historical-license-change-analysis-decided-implemented--kept-for-context)); already-published 0.2.1 on crates.io remains GPL-3 |

---

## Risk Mitigation

**Macro Complexity**: Regular refactoring; keep proc-macro logic focused; document architecture.

**Code Consolidation**: ✅ Resolved. `english_shared.rs` is now a single canonical file (`src/language/english_shared.rs`); `ranting_derive`'s copy is generated at build time via `build.rs` (see CLAUDE.md), eliminating the manual-sync drift that previously affected the `ASK` regex and `SubjectPronoun` derives. Safe to build runtime tense/viewpoint (item 3) on top of this now.

**Table Maintenance**: Document adding new irregulars; encourage community PRs; keep v1.1 plural tables separate from v1.0 verb tables to avoid corruption.

**Performance Regressions**: Benchmark at phase end; profile compile-time and runtime; set performance budgets (no more than 10% slowdown per feature).

**Ecosystem Fragmentation**: Clear governance for companion crates; version-lock to core; single source of truth for grammar rules.

**Premature API Lock-in**: v1.2 (Phase 4) contains renames (`inflect_possessive`), crate restructuring (`ranting_core`), and possibly a license change. Land these *before* actively recruiting ecosystem forks or promoting adoption — every early adopter converts these from free changes into breaking changes.

**Unmaintained Dependencies**: `proc-macro-error` has an open RUSTSEC advisory and pins syn 1; resolved by Phase 4 item 2. Until then, expect `cargo audit`/`cargo deny` warnings downstream.

---

## How to Contribute

Community feedback is welcome:
- Open issues for feature requests or concerns
- PRs welcome for bug fixes; coordinate on features via issues first
- Help needed: irregular plurals table, language modules, performance optimization

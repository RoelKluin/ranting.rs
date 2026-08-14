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

🎯 **Phase 7 (v1.4.0, Falsification, Round Two: Beyond Indo-European)** is the next
phase — see its section below. It is drafted, not started.

**Shipping today**:
- All 7 tenses, 118+ irregular verbs, irregular noun plurals, gender-neutral pronouns
- `say!()`/`say_with!()`/`ack!()`/`nay!()`/`heed!()`/`ask!()`/`#[derive(Heed)]`
- Seven `_custom`/`_with_context` inflection hook pairs (verb, pronoun, article,
  adjective, elision, numeral, preposition), plus the `capitalize`/`capitalize_with_context`
  pair and one unpaired `is_first_person_subject_custom` — 23 `Ranting` trait methods in
  all, carrying grammatical case, noun class, count, and orthography role
- Five crates: `ranting`, `ranting_core`, `ranting_derive`, and the two downstream
  falsifiers `ranting_i18n` (German) and `ranting_es` (Spanish)
- 526 compiled tests across all five crates, plus 15 runnable doctests; zero critical issues

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

1. **Unused-hook audit** (doc-only, 3-5 hours) — *cheap, informs items 2 and
   3; no dependency on either*
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
     overrides **any** of the twelve `_with_context` methods either — the
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

2. **Arabic falsification spike** (doc-only, 6-10 hours)
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

3. **Japanese falsification spike** (doc-only, 6-10 hours)
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

4. **Build decision** (no dedicated hours — a synthesis step, not a spike) —
   *depends on items 1-3*
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

5. **`ranting-ar` — Arabic reference lexicon** (16-24 hours, *provisional,
   scope set by item 2 and confirmed by item 4*) — third acceptance test
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

6. **`ranting-ja` — Japanese reference lexicon** (16-24 hours, *provisional,
   scope set by item 3 and confirmed by item 4*) — fourth acceptance test
   - Same falsification contract as items 10, 23 and 5 above.
   - Scope (provisional — item 3 sets the final vocabulary): a small closed
     set of nouns spanning at least two classifier categories, `i`-adjective
     and verb conjugation across teineigo/plain register, and — if item 3's
     spike concludes it is worth attempting rather than declining up front —
     one `heed!()`/`ask!()` example against real, unspaced Japanese input,
     with a `None` result and a documented workaround treated as a
     legitimate finding, the same way item 9 treated an honest `None` as
     success rather than failure.
   - Not scoped, per item 1's already-locked boundary: SOV word order with
     postpositional particles (named unreachable in item 1's spec already;
     Japanese would only reconfirm it).
   - What it proves: whether `NounClass` survives being asked to carry a
     genuinely different kind of noun classification than gender, and
     whether `NarrationContext.register` — designed in Phase 3, still inert
     after Phase 6 — turns out to have a real consumer at all.

### v1.4 Success Criteria (provisional — finalized by item 4)
- Items 1-3 answer, in writing, whether Arabic and/or Japanese would falsify
  something German/Spanish could not, before any lexicon code is written
- The unused-hook list from item 1 either gains a real consumer through
  items 5/6, or Phase 7 states explicitly why it remains legitimately unused
  (a hook whose only job is an English-preserving default for a construction
  no scoped fork needs is not automatically a defect)
- Zero behavioral change to existing `say!()`/`say_with!()` output, exactly
  as every Phase 6 item required — additive, English-preserving, verified by
  the existing suite passing unchanged
- Any new gap a built lexicon finds is recorded in that crate's README as a
  numbered hole, not worked around — the same falsification contract items
  10 and 23 used

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
- Pluralization of entire phrases (not just nouns)
- Subjunctive mood and hypotheticals
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

# ROADMAP.md

**Ranting** is a lightweight, declarative library for ergonomic, grammatically-correct formatted output in Rust.

---

## Vision

Ranting solves the problem of writing natural-sounding, dynamic user-facing text in Rust. Developers should be able to write grammar rules once and let the library handle inflection automatically—no manual string juggling, no brittle conditional text generation.

**Target**: Game engines, interactive fiction, chatbots, and any application that generates user-visible prose.

**Design principle**: Lightweight and ergonomic. Users write clean, declarative placeholders; Ranting handles the grammar.

---

## Current State (v1.2.1)

✅ **Phases 1-5 Complete** (through v1.2.1) — See DONE.md for full Phase 1 & Phase 2 details;
Phase 3 (v1.1.0, plurals/extensibility), Phase 4 (v1.2.0, architecture consolidation — all 8
items including the `ranting_core` extraction, dependency modernization, the typed placeholder
spec, public-API cleanup, and the MIT relicensing) and Phase 5 (v1.2.1, `ask!()` stabilization)
are each marked done further down in this file.

🎯 **Phase 6 (v1.3.0, Internationalization Foundations)** is the planned next phase — see its
section below. Note that v1.3 is not untouched: `GrammaticalCase` on `inflect_article_custom`
and `#[derive(Heed)]` have already landed under the v1.3 label ahead of the phase being written
up, and `GrammaticalCase` in particular is the pattern Phase 6 is built on.

**v1.0 features working**:
- All 7 tenses: Present, Past, Future, Present Continuous, Past Continuous, Present Perfect, Past Perfect
- 118+ irregular verbs with phonetic rules
- Gender-neutral pronouns (singular they/them)
- Named arguments, positional arguments, empty placeholders
- >200 tests (21 unit + 171 integration + 8 doctests)
- Tutorial + 10-recipe Cookbook published
- Zero critical issues

---

## Phase 3 — v1.1.0 — Plurals, Extensibility, and Ecosystem

*Goal: Complete core morphology; enable community contributions.*

**Timeline**: 8-12 weeks post-v1.0 release

### Completed Features

✅ **1. Irregular Noun Plurals** (COMPLETE — 28 hours)
   - ✅ Support 100+ common irregular plurals: child→children, person→people, mouse→mice, goose→geese, etc.
   - ✅ Codegen from data/irregular_plurals.txt (single source of truth, like verbs)
   - ✅ Integrated with existing `#[ranting(plural_end="...")]` attribute system
   - ✅ **Wired 2026-08-13**: `english::inflect_noun_irregular` (backing
     `Ranting::inflect()`'s irregular-noun path) now delegates to
     `get_plural`/`get_singular` in `src/language/plurals.rs` instead of a
     separate duplicate table scan — see `docs/architecture-review-2026-08-13.md`
     for the pre-fix history.
   - ✅ 9 new integration tests + unit tests (217 total tests passing)
   - ✅ Case-preserving lookups (child→Children when capitalized)

### Upcoming Priority Features

✅ **2. Trait-Based Inflection Extensibility** (COMPLETE — 16-20 hours)
   - ✅ Add trait methods for custom grammar rules via `inflect_verb_custom()`, `inflect_pronoun_custom()`, `inflect_article_custom()`
   - ✅ Default impls use `None` (zero breaking changes); users override for domain-specific needs
   - ✅ `PronounCase` enum for pronoun customization (Subjective, Objective, PossessiveDeterminer, PossessivePronoun)
   - ✅ Runtime fallback to English rules when custom method returns `None`
   - ✅ Enables ecosystem forks (ranting-spanish, ranting-pirate, ranting-scottish, etc.)
   - ✅ Full documentation in `docs/EXTENSIBILITY.md` with pirate, Scottish, Spanish examples
   - ✅ (v1.3) `GrammaticalCase` parameter added to `inflect_article_custom`/
     `_with_context`, threaded from the placeholder's own case marker
     (`` {the =noun} ``/`` {the @noun} `` → `Subjective`/`Objective`; bare
     `` {the noun} `` → `Name`). Closes a gap a `ranting-i18n` feasibility
     spike found: without it, a case-declining language (German
     `der`/`den`/`dem`) couldn't distinguish a subject placeholder from an
     object placeholder — both produced identical hook calls. See
     `docs/architecture-review-2026-08-13.md` section 7 and
     `tests/ranting/grammatical_case.rs`.
   - ⚠️ **Phase 6 item 10 follow-up on that `GrammaticalCase` bullet**: the
     German reference lexicon shows the fix is narrower than "closes the gap".
     `GrammaticalCase` mirrors English's marker inventory, which has no dative —
     `@` means accusative-or-dative — so a German fork must carry the case on
     the entity to reach `dem`/`der` at all, and *once it does, the `case`
     parameter is ignorable*: `{the =noun}` and `{the @noun}` then render
     identically. It made two of German's four cases expressible. Separately,
     the `inflect_pronoun_custom` override this bullet's own test recommends
     (return the noun's name, so `` {the =noun} `` reads "Der Mann") applies to
     every case-marked placeholder for that entity, which makes real German
     pronouns unreachable for it. See `ranting_i18n/README.md` holes 3 and 5.
   - ✅ 9 integration tests verifying full/partial customization and fallback behavior

✅ **3. Runtime Tense & Viewpoint Selection** (COMPLETE — 16-20 hours) — *Unblocks Recounting M9*
   - ✅ Runtime tense selection: `say_with!(context, "...", args...)` resolves `<`,`=`,`>`,`<=`,`%`,`<%` markers against a runtime `NarrationContext { tense: Option<Tense> }` (7-variant `Tense` enum), falling back to the placeholder's own marker when no override is given. `say!()` is unaffected — unchanged codegen and output.
   - ✅ Context-passing mechanism decided: explicit `say_with!(context, ...)` macro parameter (matches the library's existing explicit-argument style; rejected trait-method — conflates entity `subject` with story-wide settings per item 4 — and thread-local — implicit, fragile across tests/async).
   - Prerequisite this uncovered: `ranting_derive` is a `proc-macro = true` crate and can only export `#[proc_macro]` items, so `ranting` could not call its compile-time verb conjugation functions at runtime. Resolved by making `src/language/verb_conjugate.rs` (repo root) canonical and having `ranting_derive` consume a build-time-generated copy — the inverse of the `english_shared.rs` direction (see CLAUDE.md).
   - ✅ Runtime viewpoint selection: `NarrationContext.narration_person` (`Person::First`/`Second`/`Third`) overrides pronoun set and verb agreement, scoped to nouns declared first-person (`subject` is `"I"`/`"we"`) — the narrator only; other subjects, and other nouns in the same call, pass through unchanged. Third-person rendering falls back to singular "they" (no gender data on a first-person-declared noun to render a gendered pronoun instead); `we`→`Person::Second` renders "you" as a one-way conversion, not round-trippable. See `narration::resolve_viewpoint` in `src/narration.rs` and the CLAUDE.md "Non-obvious behaviors" entry.
   - Backwards-compatible: compile-time markers remain default if context not provided ✅

✅ **4. Narration Context Threading** (COMPLETE — 12-16 hours) — *Supports feature 3*
   - ✅ `NarrationContext` gained `register: Option<Register>` (`Formal`/`Neutral`/`Casual`) and
     `dialect: Option<&'static str>` fields, alongside item 3's `tense`/`narration_person`. Both
     new fields are inert in the crate itself — no built-in English behavior reads them — until a
     `Ranting` implementation consults them.
   - ✅ Integration point: three new default `Ranting` trait methods,
     `inflect_verb_custom_with_context`/`inflect_pronoun_custom_with_context`/
     `inflect_article_custom_with_context`, each taking `ctx: Option<&NarrationContext>` as a
     parameter (never read off `self`) and delegating to the existing non-context hook by
     default. Every verb/pronoun/article call site in `handle_placeholder_impl` now calls the
     `_with_context` hook (`say!()` passes `None`, `say_with!()` passes `Some(ctx)`), so
     overriding only the `_with_context` hook is sufficient — no need to also override the
     original.
   - ✅ `subject` stays an entity property (`Ranting::subjective()`); `register`/`dialect`, like
     `narration_person`, are story-wide settings that only ever arrive via the `ctx` parameter —
     the trait-method-sourced-context design was already rejected in item 3 for conflating the two.
   - ✅ Ecosystem forks: 8 integration tests in `tests/ranting/narration_context_threading.rs`,
     including register-driven verb/article choice and dialect-driven pronoun choice (each via a
     custom `impl Ranting` overriding only the relevant `_with_context` hook), a sentinel proving
     `say!()` calls the `_with_context` hook rather than the plain one, and a fallback check for
     when no register/dialect override applies.
   - ✅ `say!()` unaffected: its call sites pass `ctx: None` to the same `_with_context` hooks, so
     existing `say!()` output is unchanged (verified by `say_macro_still_passes_none_to_context_hooks`).
   - ✅ **Phase 6 item 10 follow-up, closed by item 12**: the whole `_with_context`
     mechanism — here and in item 3 — was unreachable from a crate depending on
     `ranting`'s public API alone. `ranting` re-exported `say`/`ack`/`nay`/`heed`/
     `Heed`/`ask`/`boxed_ranting_trait`/`ref_ranting_trait`, but **not `say_with`
     and not `derive_ranting`**, so a companion crate could never construct a
     call that delivers a `NarrationContext`; every `_with_context` hook it
     overrode received `None`. `NarrationContext`, `Tense`, `Person` and
     `Register` were already public, so this was a re-export gap rather than a
     design one — but it cost the German lexicon `dialect`-selected digits,
     register-driven wording and runtime tense. Item 12 added the two missing
     re-exports; see `ranting_i18n/README.md` hole 1 (now closed) for the
     re-verification from the companion crate's side.

✅ **5. Reflexive Forms** (COMPLETE — 8-12 hours)
   - ✅ Support myself, yourself, thyself, himself, herself, itself, ourselves,
     yourselves, themselves — the full 9-pronoun set (`SubjectPronoun`'s
     exhaustive match already covers `thou`/`ye` alongside the 7 forms named in
     this item, so the reflexive table follows suit for consistency).
   - ✅ Case marker integration: new `%` case marker (`{%person}`) dispatches to
     `PronounCase::Reflexive` in `handle_placeholder_impl`, routed through the
     existing `inflect_pronoun_custom_with_context` hook first (so a custom
     `Ranting` impl can override reflexive forms, e.g. formal "their own royal
     person") before falling back to `ranting::inflect_reflexive` in
     `src/language/english.rs`. `%` was chosen over the roadmap's illustrative
     `~` because `~` is already the `PossessivePronoun` marker (mine/yours/...);
     reusing it would have broken existing behavior. Regex change lives only in
     the canonical `src/language/english_shared.rs` (`PH_EXT`'s `case` group),
     copied to `ranting_derive` per the existing build.rs mechanism — no
     `ranting_derive` source changes needed.
   - ✅ Completes core pronouns system. Also fixed the `upper()` test in
     `src/language/english.rs`, which previously hardcoded the byte-artifact
     `"theirself"` (possessive `` ` `` + literal `"self"` string concatenation)
     as a stand-in for real reflexive support — it now uses `{%w}` and asserts
     the correct `"themselves"`.
   - ✅ 10 integration tests in `tests/ranting/reflexive_pronouns.rs` (all 9
     pronouns, singular-they reflexive, sentence-start/mid-sentence
     capitalization, forced plural/singular via `+`/`-`, `you` singular vs.
     plural, positional and named args, and custom-hook override/fallback via
     `PronounCase::Reflexive`), plus unit tests in `src/language/english.rs`.

6. ✅ **Comparative & Superlative Adjectives** (10-16 hours)
   - ✅ Handle degree: good → better → best, bad → worse → worst, plus regular
     `-er`/`-est` (fast → faster, big → bigger with CVC doubling, happy →
     happier with y→i) and periphrastic `more`/`most` for longer adjectives
     (beautiful → more beautiful), via `ranting_derive/src/language/adjective.rs`.
   - ✅ Marker-based syntax: new post-noun `!`/`!!` markers (`{noun !word}` for
     comparative, `{noun !!word}` for superlative), baked entirely at compile
     time into a `~DEGREE~WORD[:TRAILING]` sentinel that `handle_placeholder_impl`
     in `src/lib.rs` emits verbatim (with capitalization applied). `!` was
     chosen over the roadmap's illustrative `{+person good}` because `+`/`-`
     already force noun plurality/singularity (see README.md) — reusing them
     would have broken existing behavior, the same collision the Reflexive
     Forms item (Phase 3 item 5) hit and resolved by picking `%` over the
     roadmap's illustrative `~`. `!` was free: its only other appearances are
     sentence-end punctuation detection outside `{}` and a `pre`-slot article
     marker unreachable from the current `PH_EXT` grammar, neither in the
     `post` capture group `!`/`!!` now uses.
   - ✅ Unlike verb tense, degree needs no subject/number/`NarrationContext`
     agreement at runtime, so — following the `irregular_plurals.txt`
     codegen-from-data-file pattern — the irregular table
     (`data/irregular_adjectives.txt`, symlinked into `ranting_derive/data/`)
     and its derived Rust table are generated by `ranting_derive/build.rs`
     only; `ranting` needs no runtime copy, no repo-root canonical
     `include!()` wrapper, and Phase 4's "don't extend the copy mechanisms"
     warning doesn't apply here.
   - ✅ 8 integration tests in `tests/ranting/comparative_adjectives.rs`
     (irregular lookup, monosyllabic suffix + CVC doubling, two-syllable
     consonant+y, periphrastic more/most, sentence-start capitalization,
     trailing words after the degree word).
   - ✅ Rounds out morphology for richer text generation.

7. ✅ **Recursive Type Inflection** (12-16 hours)
   - ✅ Support collections and nested Ranting types: `Vec<Item>`/`Option<Person>`/
     `Box<Noun>` (and any `T: Ranting`) as `say!()` placeholder subjects/arguments.
   - ✅ `Box<T>` gets a direct blanket `impl<T: Ranting> Ranting for Box<T>`, in
     `src/collections.rs` — `Ranting` is local to this crate so implementing it
     for foreign `Box<T>` is allowed by the orphan rules, and `std` already
     provides `impl<T: Display> Display for Box<T>` so the `Ranting: Display`
     supertrait bound is satisfied for free.
   - ⚠️ `Vec<T>` and `Option<T>` could **not** get the same direct blanket-impl
     treatment as originally scoped: `Ranting: Display` requires `Vec<T>`/
     `Option<T>` to implement `Display`, but `Display` is a foreign trait and
     neither `Vec` nor `Option` is `#[fundamental]` (unlike `Box`/`&`), so
     `impl Display for Vec<T>`/`Option<T>` is rejected by the orphan rules
     (E0117) no matter what `T` is — not a case-by-case gap, a hard compiler
     rule. Relaxing the `Display` supertrait to work around it would be a
     breaking public-API change, out of scope here (Phase 4 owns public-API
     cleanup). Resolved with two local newtype wrappers instead — `Many<T>(pub
     Vec<T>)` and `Maybe<T>(pub Option<T>)` in `src/collections.rs` — which
     *are* local types and so can freely implement both `Display` and
     `Ranting`; users wrap their `Vec`/`Option` before passing it to `say!()`.
     This is the same "local wrapper sidesteps the orphan rule" shape already
     used by `boxed_ranting_trait!`/`ref_ranting_trait!` in
     `ranting_derive/src/lib.rs` for `Box<dyn Trait>`/`&dyn Trait`, except
     those work because `Box`/`&` are themselves `#[fundamental]`, which `Vec`/
     `Option` are not — so a macro-generated downstream impl couldn't have
     closed this gap either.
   - ✅ `Many<T>` is a collective noun phrase: rendered name joins items' own
     `name()`s as `"a, b and c"`; plural (`"they"`/`"are"`) whenever the `Vec`
     doesn't hold exactly one item (zero included — "there are no items"),
     delegating plurality/pronoun/custom-hook behavior straight through to the
     single item when there is exactly one, and falling back to built-in
     English rules for custom hooks when there are zero or several items
     (there's no single item to delegate a per-item override to).
     `skip_article()` is `true` when empty, so `{a items}` doesn't leave a
     dangling article.
   - ✅ `Maybe<T>`: `Maybe(Some(x))` delegates every method straight through to
     `x`; `Maybe(None)` renders as empty, singular, subject `"it"`, and also
     `skip_article() == true`.
   - ✅ Not implemented via `#[derive(Ranting)]` as the roadmap entry
     illustrated — `Vec`/`Option`/`Box` are foreign `std` types, not structs
     the crate's own derive macro is invoked on, so there is nothing for
     `#[derive(Ranting)]` to attach to here; the derive macro is unrelated to
     this item's implementation.
   - ✅ All six `inflect_*_custom`/`inflect_*_custom_with_context` hooks (not
     just the five required trait methods) are forwarded by `Box`/`Many`
     (single-item case)/`Maybe`, so a wrapped type's custom overrides (e.g. a
     `ranting-pirate`-style fork) survive wrapping instead of silently
     reverting to English defaults.
   - ✅ 13 integration tests in `tests/ranting/recursive_inflection.rs` (empty/
     single/multi-item `Many`, `Maybe(None)`/`Maybe(Some)`, `Box<Noun>`,
     `Box<Many<Noun>>`/`Many<Box<Noun>>` nesting/composition, uppercase-first-
     char-only join, and custom-hook forwarding through all three wrapper
     types plus the multi-item English-fallback case), plus 2 doctests in
     `src/collections.rs`.

8. ✅ **Input Parsing (`heed!()`)** (v1 + v2, see
   `docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md`)
   - ✅ v1: `heed!(template, input)` matches free-form input text against a
     template — literal words plus `{name}`/`{name...}`/`{$name}`
     captures — the command-parser half of the input-parsing feasibility
     brainstorm. The full-grammatical-inversion half (`unsay!()`) was
     explicitly not pursued: several of `say!()`'s inflection choices are
     not injective (multiple original values render to the same text), so
     a general inverse isn't a buildable spec.
   - ✅ v2: `#[derive(Heed)]` + `#[heed(template = "...")]` on a user struct,
     generating `fn heed(input: &str) -> Option<Self>`
     (`ranting_derive/src/heed_derive.rs::derive_heed`), reusing v1's
     `heed::compile_heed_template` rather than duplicating the matching
     engine. Every template capture must have a same-named field and vice
     versa (one-to-one, not partial — a stale field or unmapped capture is
     a compile error), with each field's type checked against its capture
     kind (`String` for `{name}`/`{name...}`, `u64` for `{$name}`). Only
     structs are supported (named fields, or a unit struct for a
     zero-capture template); field declaration order is independent of
     template capture order, since the derive maps by name. 8 integration
     tests in `tests/ranting/heed_derive.rs`.

### v1.1 Success Criteria
- Irregular plurals support for 100+ nouns
- Trait extensibility API stable and documented
- Runtime tense & viewpoint selection working (unblocks Recounting M9)
- Narration context threading designed and integrated
- Reflexive forms + comparative/superlative working
- Zero breaking changes from v1.0
- Community contributions: 2-3 ecosystem forks
- GitHub engagement: 10+ answered issues/discussions

---

## Phase 4 — v1.2.0 — Architecture Consolidation (Design Review, 2026-08-12)

*Goal: pay down structural debt while there is no userbase to break. Every rename,
crate split, and API change is cheap now and expensive after ecosystem forks exist —
so this phase deliberately precedes Ecosystem Expansion.*

Prioritized (1-2 together delete most of CLAUDE.md's "key constraints"):

1. ✅ **Extract `ranting_core` shared crate** (highest leverage)
   - ✅ Replaced all three code-sharing mechanisms (`english_shared.rs` build.rs
     copy, `verb_conjugate.rs` inverse copy, `irregular_verbs.txt` per-crate
     codegen) with one plain rlib crate, `ranting_core`, that both `ranting`
     and `ranting_derive` depend on as an ordinary path dependency (`ranting_core
     = { version = "0.1.0", path = "../ranting_core" }` from `ranting_derive`,
     `path = "ranting_core"` from `ranting`) — the `serde`/`serde_derive`
     pattern. Both consumers still build with independent `Cargo.lock` files,
     same as before (`ranting_derive` already published standalone; `ranting_core`
     is set up the same way, versioned and publishable, even though it isn't
     published yet).
   - ✅ Moved in: `ranting_core::grammar` (placeholder grammar `PH_START`/
     `PH_EXT`, `SubjectPronoun`, `is_subject`/`is_subjective_plural`/
     `is_first_person_subject`, and the still-unused `Article`/
     `DemonstrativePronoun` enums) and `ranting_core::verb_conjugate`
     (`to_past`/`to_continuous`/`to_future`/`to_past_participle` plus the
     `IRREGULAR_PAST`/`IRREGULAR_PAST_PARTICIPLE` tables, now generated exactly
     once by `ranting_core/build.rs` from `data/irregular_verbs.txt` instead of
     independently by both `ranting`'s and `ranting_derive`'s build.rs scripts).
   - ✅ Deleted: both crates' build.rs copy-into-`OUT_DIR` steps for the code
     above, `ranting_derive/data/english_shared_source.rs`,
     `ranting_derive/data/verb_conjugate_source.rs`, and
     `ranting_derive/data/irregular_verbs.txt` (superseded by `ranting_core`'s
     own `data/irregular_verbs.txt` → `../../data/irregular_verbs.txt`
     symlink). The dual-strum (0.24 + 0.27) compilation constraint is gone:
     `ranting_core` depends on `strum`/`strum_macros` 0.27 only (standardizing
     on the newer version per this item's own design guidance, anticipating
     part of item 2), and `ranting_derive`'s `Cargo.toml` no longer lists
     `strum`/`strum_macros` at all.
   - ⚠️ **Deviation — `SubjectPronoun` had to become `pub`, not just
     `pub(crate)`**: it now lives in a different crate from every call site, so
     `ranting`'s `src/language/english.rs` needs `ranting_core::grammar::
     SubjectPronoun` to be visible across the crate boundary. This is a small,
     unplanned head start on item 4's first bullet ("make `SubjectPronoun`
     public") — only the visibility changed here, not `FromStr`-based storage
     in `Noun` or the panic-removal item 4 actually scopes; item 4 still owns
     the rest of that work.
   - ⚠️ **Deviation — inherent `impl SubjectPronoun { fn forms(...) }` became a
     free function**: an inherent `impl` on a foreign type (E0116) is illegal
     once `SubjectPronoun` isn't local to `ranting` anymore. Converted to a
     free function `pronoun_forms(subject: SubjectPronoun) -> PronounForms` in
     `src/language/english.rs`; all five call sites (`inflect_adjective`,
     `inflect_subjective`, `inflect_objective`, `inflect_possessive`,
     `inflect_reflexive`) and the pronoun-table test updated accordingly. No
     behavior change, just a mechanical consequence of the crate split.
   - ⚠️ **Design call — what did *not* move into `ranting_core`**:
     `ranting_derive/src/language/plurals.rs`'s `get_plural`/`get_singular`/
     `apply_case` stayed hand-duplicated against `src/language/plurals.rs`
     rather than consolidating — both copies are still dead code (not wired
     into any inflection call site in either crate; pre-existing drift risk,
     see `docs/architecture-review-2026-08-13.md`), and moving genuinely-dead
     code into the shared crate wouldn't resolve that, only relocate it.
     Wiring plurals up (or consolidating the lookup functions) remains
     unclaimed work, as CLAUDE.md already noted before this item.
     **Update 2026-08-13**: the `ranting`-side copy is wired now —
     `english::inflect_noun_irregular` delegates to `get_plural`/`get_singular`
     instead of re-implementing the same table scan, so `Ranting::inflect()`'s
     irregular-noun path uses them for real and gains `apply_case`'s
     case-preservation as a side effect (previously lost, since the caller only
     applied `uc_1st_if`'s first-letter rule). The `ranting_derive`-side copy
     was deleted the same day (`src/language/plurals.rs`, its
     `pub mod plurals;` in `mod.rs`, and the `generate_plurals_table`
     codegen + `IRREGULAR_PLURALS` table in `ranting_derive/build.rs`) rather
     than left permanently dead — noun pluralization happens at
     `Ranting::inflect()` runtime, never at `ranting_derive` compile time, so
     it never had a call site to wire into; this also closed out
     `ranting_derive`'s standalone `cargo clippy -D warnings` failure (see
     Phase 4 item 6's follow-up note below). `ranting_derive/src/language/adjective.rs` (comparative/superlative
     tables, Phase 3 item 6) also stayed put — `ranting` has no runtime use
     for it, so it was never one of the three copy mechanisms this item
     targeted; CLAUDE.md's original phrasing already called this out as a
     derive-only table with no repo-root canonical copy.
   - ✅ Full gate clean after the extraction: `cargo fmt --check`, `cargo
     clippy --all-targets -- -D warnings` (root — the only pre-existing
     warnings are the already-known `ranting_derive/src/language/plurals.rs`
     dead-code ones, now surfacing as build-dependency warnings rather than
     workspace-member errors, plus `IRREGULAR_PLURALS`/`Article`/
     `DemonstrativePronoun` dead code, exactly as expected), and `cargo test`
     all pass. Test count: the 11 verb-conjugation proptest/example tests that
     lived in `src/language/verb_conjugate.rs` moved intact to
     `ranting_core/src/verb_conjugate.rs` and still pass there (`cargo test`
     from `ranting_core/`) — `ranting`'s own `cargo test` count dropped from
     302 lib+integration tests to 291 for exactly that reason (302 − 11 = 291;
     252 integration tests and the 11 doctests are unaffected), not because
     any test was deleted. `ranting_derive`'s pre-existing doctest failure
     (`src/lib.rs - derive_ranting`, missing `ranting` as a dev-dependency so
     `use ranting::say` can't resolve) is unchanged by this item — verified
     identical on the pre-extraction commit, not a regression.
   - Note on the `Cargo.lock` diffs: `ranting_derive/Cargo.lock`'s diff is
     large (~350 lines) but not a smuggled dependency bump — its
     `[dev-dependencies] proptest = "1"` was declared in `Cargo.toml` but had
     never actually been resolved into that lockfile before (no `proptest` or
     transitive `rand`/`bit-set`/`tempfile`/etc. entries existed pre-change),
     so running `cargo test` there for the first time in a while pulled in
     the whole dev-dependency tree at once, on top of the intentional
     strum 0.24→0.27 bump (which also pulls a transitive `syn 2.0.119` in
     alongside `ranting_derive`'s own unchanged direct `syn = "1.0.98"`) and
     ordinary semver-compatible `proc-macro2`/`quote` point bumps from
     unifying the whole graph on one version. Root `Cargo.lock`'s diff is the
     expected small one: `strum`/`strum_macros` 0.24 (and their now-orphaned
     `heck` dependency) removed, `ranting_core` added.
   - Note on `ranting_core`'s build.rs: unlike `ranting_derive`'s old
     `generate_verbs_table`, `ranting_core/build.rs` does not assert an exact
     entry count (`expected_count = 118`) against `data/irregular_verbs.txt`
     — only the pre-existing duplicate-base check was kept, since a hardcoded
     count is brittle against legitimate future additions to the data file
     and wasn't load-bearing for correctness.
   - Note on `ranting_core` versioning: both consumers depend on it via
     `version = "0.1.0", path = "..."` (not an exact `=0.1.0` pin, unlike
     `ranting`'s `ranting_derive = "=0.2.1"`). Since `ranting_core` isn't
     published yet and both consumers are edited in lockstep in this repo,
     this is low-risk today; if/when `ranting_core` is published independently,
     revisit whether it should move to an exact pin like `ranting_derive`'s.

2. ✅ **Dependency modernization** (`ranting_derive`)
   - ✅ syn 1.0.98 → 2 (`syn = { version = "2.0", features = ["full"] }`), darling
     0.14.1 → 0.20.11 (0.24 exists on crates.io; stopped at 0.20 deliberately —
     it already satisfies the item's "0.20+" floor and 0.20 vs. 0.24 wasn't
     forced by any compile error, so there was no reason to chase newer).
     Both bumps compiled **with zero source changes required** in
     `ranting_derive/src/{lib,ranting_impl,heed,str_lit}.rs` — the crate's
     usage of `syn`/`darling` (parsing `DeriveInput`, `Expr`, building output
     via `quote!`/`parse_quote!`, `FromDeriveInput`) turned out not to touch any
     of the APIs that moved between those major versions (no `parse_meta`/
     `NestedMeta`/`AttributeArgs` usage — darling owns all attribute parsing
     here). Verified this was a real fresh compile against the new versions,
     not a stale/cached artifact: build output shows `Compiling syn v2.0.119`
     and `Compiling darling v0.20.11` from scratch, and `cargo tree` (both
     from `ranting_derive/` and the repo root) shows zero occurrences of
     `syn v1` or `darling v0.14` anywhere in the resolved graph afterward.
   - ✅ **Strum: already resolved by item 1, nothing left to do here.** Before
     starting, checked `ranting_derive/Cargo.toml` and confirmed item 1's
     `ranting_core` extraction had already removed `strum`/`strum_macros` from
     `ranting_derive`'s own dependency list entirely (not just unified the
     version — dropped outright, since `SubjectPronoun` and its parsing now
     live in `ranting_core`). `cargo tree -i strum` from both `ranting_derive/`
     and the repo root confirms exactly one `strum v0.27.2` in the graph,
     reached transitively through `ranting_core`. So neither of this item's two
     original options ("unify on 0.27" / "hand-write parsing to drop strum from
     the derive path") applied — the derive path never touches strum at all
     post-extraction, direct or transitive-only-for-itself. Text above and in
     item 1 was already accurate; no rewrite needed there, just confirmation.
   - ✅ **Dropped `proc-macro-error`.** Turned out to be dead weight even before
     this change — `grep` found zero uses of `proc_macro_error`/`abort!`/
     `emit_error!`/`abort_call_site!` anywhere in `ranting_derive/src/`; the
     crate already did its own diagnostics via `syn::Error`/`Result` plumbing
     (`parse_str_params` etc. already returned `syn::Result`). Removed the
     `Cargo.toml` line; no source changes needed since nothing referenced it.
   - ✅ Dropped `lazy_static` in both `ranting` and `ranting_derive` →
     `std::sync::LazyLock`. Three call sites converted, all the
     `lazy_static! { static ref X: T = expr; }` → `static X: LazyLock<T> =
     LazyLock::new(|| expr);` shape (`src/lib.rs`'s `OF` regex;
     `ranting_derive/src/lib.rs`'s `PH`/`PHE` and `POSS` regexes). `OnceLock`
     wasn't needed anywhere — every site was "compute once lazily on first
     access," not an explicit set-once pattern, so `LazyLock` is the correct
     fit per the item's own guidance. `heed.rs`'s pre-existing `OnceLock` usage
     (compiled-regex cache behind `HeedMatcher`, a genuine set-once-explicitly
     case) is unrelated and untouched.
   - ✅ Bumped `ranting_derive`'s regex 1.6.0 → 1.11 (landed at 1.13.1 via `^1.11`
     resolution — still satisfies the stated floor). Straightforward version
     bump, no API changes hit; `heed!()`'s regex-version-independence from
     `ranting`'s own regex dep (documented in CLAUDE.md) is unaffected since
     that design was never about the *version* matching, just the type not
     crossing the crate boundary.
   - ✅ Unified editions to 2024: `ranting_derive` and `ranting_core` both
     2021 → 2024 (root `ranting` was already on 2024 from an earlier session).
     Done last, after the syn 2 migration was compiling and green, per the
     item's own sequencing note. Only effect observed was `cargo fmt`
     reformatting import blocks to 2024-edition-idiomatic grouping/ordering in
     `ranting_derive/src/{lib,ranting_impl,str_lit,heed}.rs` and
     `src/language/adjective.rs` (e.g. `use quote::{ToTokens, quote}` ordering,
     multi-line `parse_quote!`/`include!` wrapping) — no behavioral diffs, no
     new clippy findings, no test regressions.
   - ✅ `cargo audit` (installed fresh via `cargo install cargo-audit --locked`,
     network was available in this environment) ran clean — 0 vulnerabilities,
     exit 0 — for all three `Cargo.lock`s (root `ranting`: 63 crates,
     `ranting_core`: 43, `ranting_derive`: 57). `cargo-deny` was not installed
     and not attempted to install (audit alone was sufficient to confirm the
     RUSTSEC-clean claim this item cared about — the `proc-macro-error`
     advisory that motivated this item is now unreachable from any of the
     three dependency graphs). Confirmed via `cargo tree` that `proc-macro-
     error`, `lazy_static`, and `syn v1` no longer appear anywhere in either
     `ranting` or `ranting_derive`'s resolved graph.
   - ✅ Gate green on all three crates standalone (`cargo fmt --check` +
     `cargo clippy --all-targets -- -D warnings` + `cargo test`, run from each
     crate's own directory, re-run *after* `cargo fmt` reformatted files so the
     reported results reflect the final tree, not a pre-fmt snapshot): root
     `ranting` clean (252 tests + 11 doctests, 4 ignored); `ranting_core` clean
     (11 tests, 0 doctests). `ranting_derive` standalone clippy reproduces
     exactly the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead `IRREGULAR_PLURALS`/
     `get_plural`/`get_singular`/`apply_case` in `plurals.rs`, one
     `map_or`→`is_some_and`, two needless-borrow lints) — confirmed identical
     before/after via `git stash` (same 7-error list both times), so nothing
     new was introduced and none of the pre-existing ones were incidentally
     fixed by this migration, despite `lib.rs` itself being touched (by the
     `LazyLock` conversion and `cargo fmt`) — the specific lines clippy flags
     there were untouched by either. `ranting_derive`'s one
     doctest failure (`src/lib.rs - derive_ranting`, `unresolved import
     'ranting'`) is the same pre-existing proc-macro-crate-can't-self-test
     limitation CLAUDE.md already documents ("test in `ranting/src/lib.rs`
     instead") — also confirmed identical via `git stash`, not a regression.
   - ✅ Also verified under the `debug` feature (`#[cfg(feature = "debug")]`
     `eprintln!` diagnostics in `parse_str_params`, exercised by CLAUDE.md's
     documented `cargo test --features debug` command and the
     `tutorial::section_5_debug_feature` integration test) — a default-features
     gate run alone wouldn't compile that cfg'd branch at all. Root
     `cargo test --features debug`: same 252 tests + 11 doctests, all green.
     `ranting_derive`'s `cargo clippy --all-targets --features debug -- -D
     warnings` surfaces two additional pre-existing findings beyond the 7 above
     (`to_string` applied to a `Display` type inside `eprintln!` args, at
     `lib.rs:294` and `lib.rs:430`) — same `git stash` before/after check shows
     the identical 9-finding list both times, so these predate this migration
     too and nothing new was introduced under `--features debug` either.
   - Payoff realized: `proc-macro-error` and `syn 1` (and its old duplicate
     `unicode-ident`/`quote` sub-tree) are gone from every downstream build.

3. ✅ **Typed placeholder spec across the compile-time/runtime seam**
   - ✅ Added `ranting_core::placeholder` (`ranting_core/src/placeholder.rs`),
     a new shared module with `PlaceholderSpec { pre, plurality, noun_space,
     case: CaseKind, post: PostSpec }`, `CaseKind` (`Name`/`Hidden`/
     `Subjective`/`Objective`/`PossessiveDeterminer`/`PossessivePronoun`/
     `Reflexive` — one variant per `case` capture-group character), and
     `PostSpec` (`None`/`PossessiveS`/`Verb(&'static str)`/`Tense { marker:
     TenseMarker, word, trailing, leading_space }`/`Degree { word, trailing,
     leading_space }`) with `TenseMarker` (`Past`/`Continuous`/`Future`/
     `PastContinuous`/`PresentPerfect`/`PastPerfect`). `ranting` re-exports
     the module (`pub use ranting_core::placeholder;`) so macro-generated
     code, which only has a `ranting::` path available, can name the types.
   - ✅ `ranting_derive::handle_param` (`ranting_derive/src/lib.rs`) now
     classifies the post-noun word into a `PostSpec` variant *at compile
     time* and emits it as a struct-literal expression (`ranting::placeholder
     ::PlaceholderSpec { .. }`) baked directly into the generated
     `handle_placeholder`/`handle_placeholder_with_context` call — an
     all-const-fields struct literal is itself a compile-time constant
     expression, so this satisfies "baked by the macro as a const" without
     needing a separate named `const ITEM: T = ..;` declaration (which would
     need one uniquely-named item per placeholder in the expanded code for no
     added benefit). `handle_placeholder`/`handle_placeholder_with_context`
     (`src/lib.rs`) take `spec: PlaceholderSpec` in place of the old
     `caps: [&str; 5]`.
   - ✅ Deleted the `~TENSE~MARKER:WORD` and `~DEGREE~WORD[:TRAILING]` string
     sentinels entirely, along with every runtime `strip_prefix("~TENSE~")`/
     `strip_prefix("~DEGREE~")`/`split_once(':')`/`split_once(' ')` call and
     the two "fallback if marker/colon parsing fails" branches that existed
     only to handle malformed sentinels the macro could never actually
     produce — those states are unrepresentable now (`PostSpec::Tense`/
     `Degree` carry `marker`/`word`/`trailing` as separate typed/string
     fields the macro already split apart, not a single string to
     re-parse). Also fixed a latent (untested, so never user-visible)
     ordering bug this uncovered: the old `~TENSE~` sentinel, when a tense
     marker had trailing words (e.g. `{who <=go} home`), relied on
     `split_at_find_end`'s last-whitespace split to separate the sentinel
     from the trailing word, which — unlike the `~DEGREE~` sentinel, which
     was deliberately exempted from that split via a `starts_with` check for
     exactly this reason — would have pushed the raw, still-unparsed
     `~TENSE~<:word` prefix into the output verbatim; no test exercised a
     tense marker with trailing content, so this was never observed. The new
     `PostSpec::Tense { word, trailing, .. }` renders `word` then
     `" " + trailing` directly, matching the (already-correct)
     `PostSpec::Degree` handling, with no split involved.
   - ✅ **Fixed 2026-08-13** (originally deferred here, then revisited):
     `get_article_or_so`'s article-keyword classification (`"the"` /
     `"a" | "an" | "some"` / `"these" | "those"`) is now a typed
     `ranting_core::placeholder::ArticleKind`, baked at compile time by
     `ranting_derive`, mirroring the `PostSpec`/`CaseKind` treatment above.
     **The original deferral's stated blocker was wrong.** It named the
     second, *chained* `get_article_or_so(noun, s, ...)` call inside the
     `etc1` sub-parse as the problem, reasoning that `etc1` is "derived from
     `pre` only after the runtime backtick-possessive substitution." Tracing
     the actual control flow shows the opposite: that chained call lives
     inside the branch reached only when `has_possesive` (`pre_raw.contains('`')`)
     is `false` — meaning no backtick exists *anywhere* in `pre_raw` — so
     `pre.replace('`', poss.as_str())` is provably a no-op on every path that
     reaches it, and `etc1`/`s` are always compile-time-literal there. The
     part that *can* see runtime text is actually the **first** call
     (`pre`'s first word) when a `` ` `` possessive-substitution sentinel
     falls within it — but that's also provably safe to classify at compile
     time, because a possessive determiner/`Name's` form (the only thing
     that substitution ever produces) can never coincide with a real article
     keyword, so `ArticleKind::Other` is always correct for it without
     inspecting the actual runtime string.
     `ArticleKind::classify` (`ranting_core/src/placeholder.rs`) is the
     canonical reference implementation (unit-tested, including the
     case-sensitivity asymmetry between the two call sites: the first is
     lowercased before classifying, the second isn't — preserved exactly).
     `ranting_derive`'s `handle_param` bakes `PlaceholderSpec::pre_kind`/
     `pre_chained_kind` by replicating `handle_placeholder_impl`'s own
     `pre`/`etc1` splitting at compile time (reusing the same
     `split_at_find_start` helper already duplicated in that file), not by
     calling `ArticleKind::classify` itself (same pattern as `CaseKind`'s/
     `TenseMarker`'s own local `match`es — proc-macro build code just needs
     to emit the right `quote!` tokens, not call a `const fn`).
     `get_article_or_so` still owns the runtime-only parts unaffected by
     this — `Ranting::skip_article()`, the `inflect_article_custom_with_context`
     hook, and a/an/singular-vs-plural rendering. Two new permanent
     regression tests pin the behavior this was traced against:
     `tests/ranting/article_classification.rs`'s `chained_article_after_modal`
     (exercises the second call site directly — no prior test did) and
     `combined_verb_and_backtick_possessive` (the doctest-derived
     `` {can `man pair of #0 boots remain} `` case the original deferral
     worried about).
   - **The overloaded `=` marker, disambiguated**: `=` means "subjective
     pronoun case" before the noun and "continuous tense" after it. Before
     this item, both were bare `&str`s (`case: "="` vs. `post:
     "~TENSE~=:running"`), so the two meanings were told apart only by
     *which array slot the string sat in*, combined with a string prefix on
     one side. Now `case: CaseKind::Subjective` and `post:
     PostSpec::Tense(TenseMarker::Continuous, ..)` are different fields of
     different enum types — the two meanings can't be confused at the type
     level, and (per the point above) there's no shared string
     representation left that needs disambiguating. Documented in
     `ranting_core/src/placeholder.rs`'s module doc comment.
   - ✅ `say!()`'s output is unchanged (still bakes fully-conjugated
     literals through the same code path, now via `PostSpec::Tense`/`Verb`
     instead of the sentinel string); `say_with!()`'s runtime tense
     resolution (`NarrationContext.tense`) is unaffected — verified via the
     existing `tests/ranting/runtime_tense.rs` and
     `tests/ranting/verb_tense.rs` suites, unmodified and still green.
   - ✅ Added 2 regression tests locking in the trailing-word fix above:
     `verb_tense::tense_marker_with_trailing_words` (`say!()`) and
     `runtime_tense::tense_marker_with_trailing_words_runtime_tense`
     (`say_with!()`, both with and without a `NarrationContext.tense`
     override). Root `ranting`'s integration count is therefore 254, not
     252 — a deliberate increase (new coverage for a real, previously-latent
     bug this refactor fixed), not a regression; confirmed the *old* code
     actually produced the garbled `"He ~TENSE~<:went homes"` for
     `say!("{=0 <go home}", person)` via `git stash` before writing the test,
     so the note above isn't just reasoned but verified.
   - ✅ Gate green on all three crates standalone. Root `ranting`: 39 lib +
     254 integration + 11 doctests (4 ignored) — 252 of the integration
     tests and all lib/doctest counts are identical to before this item
     (verified against `git show c305cef7`), +2 for the regression tests
     above; also re-verified under `cargo test --features debug`.
     `ranting_core`: 11 tests, 0 doctests, unaffected (the new `placeholder`
     module has no tests of its own beyond what the `ranting`-crate
     integration suite already exercises end-to-end through
     `say!()`/`say_with!()`). `ranting_derive` standalone: 9 unit tests
     green; clippy reproduces the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead `plurals.rs` code, one
     `map_or`, two needless-borrow lints) under default features, and the
     same 9 (7 + 2 `to_string`-in-`eprintln!`) under `--features debug` —
     confirmed identical via `git stash` before/after for both feature sets,
     nothing new introduced despite this item editing the exact
     `handle_param` function those `eprintln!`s are diagnostic siblings of.
     The one pre-existing doctest failure (`src/lib.rs - derive_ranting`,
     `unresolved import 'ranting'`, CLAUDE.md's documented
     proc-macro-crate-can't-self-test limitation) is unchanged.

4. ✅ **Type the subject; remove runtime panics**
   - ✅ **Already existed from item 1's `ranting_core` extraction** (see item 1's
     own "Deviation" note): `SubjectPronoun` was already `pub` (forced by
     living in a separate crate from its call sites) and already had `FromStr`
     via `#[derive(EnumString)]`. Neither of those needed new work here — this
     item's actual scope was storing the typed enum in `Noun` and removing the
     panics, which item 1 explicitly deferred.
   - ✅ **New: `SubjectPronoun::as_str(&self) -> &'static str`**
     (`ranting_core/src/grammar.rs`), the inverse of `from_str`. Added so
     `Noun`'s `subject` field could change type from `String` to
     `SubjectPronoun` without touching `ranting_derive`'s generic
     `subject = "$"` codegen (`ranting_impl.rs`'s `get_plurality_fns`), which
     emits `self.subject.as_str()` and `ranting::is_subjective_plural(self.subject.as_str())`
     — those call sites don't care whether `self.subject` is a `String` or a
     type with an `as_str()` method, and that codegen path is also used by any
     third-party struct that still declares its own `subject: String` field
     (per the documented `#[ranting(subject = "$")]` contract), so it had to
     stay untouched.
   - ✅ **`Noun.subject: String` → `Noun.subject: SubjectPronoun`** — an
     invalid subject is now genuinely unrepresentable in a constructed `Noun`,
     not just rejected at construction time.
   - ✅ **API-shape decision: kept `Noun::new(&str, &str) -> Self` panicking,
     added `Noun::try_new(&str, &str) -> Result<Self, InvalidSubjectError>`**
     — option (a) from this item's own list of choices, not option (b)
     (`Noun::new` taking `SubjectPronoun` directly). Reasoning: `Noun::new` is
     called from the README, doctests across `src/lib.rs`/`src/collections.rs`/
     `src/language/english.rs`, and every integration test file — dozens of
     call sites, all passing string literals like `"it"`/`"he"`/`"they"`.
     Changing its signature to take `SubjectPronoun` would force every one of
     those to write `SubjectPronoun::It` instead of `"it"` (or a `.parse()`)
     for zero safety gain, since those literals are always valid; it would
     also be a breaking change for any external crate already calling
     `Noun::new`. `Noun::new` keeps its exact old signature and panic
     behavior (now implemented as `Self::try_new(..).expect("not a subject")`
     — same message, same panic path, verified by a
     `#[should_panic(expected = "not a subject")]` test) so no existing call
     site needed touching. `Noun::try_new` is the new, additive, non-panicking
     escape hatch for callers with a runtime/user-supplied subject string —
     the actual "unrepresentable invalid state" is enforced by `Noun`'s field
     now being `SubjectPronoun` rather than `String`, regardless of which
     constructor was used to get there; `try_new` just adds a way to reach
     that guarantee without a panic.
     `InvalidSubjectError(pub String)` (`src/lib.rs`) implements
     `Display`/`Error` and carries the rejected string back to the caller.
   - ✅ **`is_subjective_plural`'s `as usize >= 6` → explicit match**
     (`ranting_core/src/grammar.rs`): matches every `SubjectPronoun` variant
     by name (`I | You | Thou | He | She | It => false`,
     `We | Ye | They => true`), no wildcard arm, so a future new variant is a
     compile error here instead of silently landing on one side of a numeric
     cutoff. Cross-checked against every variant via `SubjectPronoun::iter()`
     in a new `ranting_core` unit test
     (`grammar::tests::is_subjective_plural_covers_every_variant`).
   - ✅ **Both remaining fallible paths now degrade gracefully instead of
     panicking**:
     - `is_subjective_plural`'s `.expect("subject should be a valid pronoun")`
       → invalid input now returns `false` (treated as singular) instead of
       panicking. `is_subject`/`is_subjective_plural` are both public API
       (re-exported from `ranting`), so this is a real behavior change for
       any external caller currently relying on the panic — documented here
       since it's the one behavior change in this item.
     - `inflect()`'s two `.expect(...)` calls on `strip_suffix` failure in
       `ranting_derive/src/ranting_impl.rs`'s generated code (both the
       `subject = "$"` branch and the fixed-subject branch, four `.expect`
       call sites total) → when a name doesn't end in the expected
       `singular_end`/`plural_end`, the fallback now returns the name
       unchanged instead of panicking, matching the "no irregular-table
       match, no regular-suffix match either" case a formatting call
       shouldn't abort the whole program over.
     - Left alone, out of this item's explicit scope: the other five
       `.expect("Not a subject")` calls in `src/language/english.rs`
       (`inflect_adjective`/`inflect_subjective`/`inflect_objective`/
       `inflect_possessive`/`inflect_reflexive`) — those operate on subjects
       that are already-validated `Noun`/`Ranting` data flowing through
       `say!()`'s own call sites, not raw external input, and this item named
       only `inflect()` and `is_subjective_plural`'s `.expect`s explicitly.
       Candidate for a future pass if the same graceful-degradation standard
       should extend there too.
   - ✅ New tests: `ranting_core/src/grammar.rs`'s `tests` module (3 tests —
     `as_str`/`from_str` round-trip via `EnumIter`, exhaustive plurality
     cross-check, invalid-input degradation) and 6 new tests in
     `tests/ranting/property_based.rs` (`Noun::try_new` no-panic proptest,
     `is_subjective_plural` no-panic proptest, invalid-subject degradation,
     `try_new` error contents, `Noun::new`'s panic behavior preserved,
     `inflect()`'s suffix-mismatch graceful degradation via a `#[derive_ranting]`
     struct whose declared plurality doesn't match its name's suffix).
   - ✅ Full gate clean on all three crates; test counts against the
     pre-task baseline (`git show 297056c2`: 39 lib + 254 integration + 11
     doctests in `ranting`, 11 in `ranting_core`, 9 unit in `ranting_derive`):
     `ranting` now 39 lib + 260 integration (+6) + 11 doctests; `ranting_core`
     now 14 (+3) unit tests, 0 doctests (unchanged); `ranting_derive` unchanged
     at 9 unit tests, its one pre-existing doctest failure
     (`src/lib.rs - derive_ranting`, `unresolved import 'ranting'`)
     unaffected — same CLAUDE.md-documented proc-macro-crate-can't-self-test
     limitation, not a regression. `ranting_derive`'s standalone
     `cargo clippy --all-targets -- -D warnings` still fails on exactly the
     same pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead code in `plurals.rs`, one
     `map_or` lint, two needless-borrow lints) — no new findings introduced by
     this item's changes to `ranting_impl.rs`.

5. ✅ **Public API cleanup** (free only while there's no userbase)
   - ✅ Fixed the `inflect_possesive` → `inflect_possessive` typo (public API):
     the `src/language/english.rs` definition, its `src/lib.rs` re-export and
     one internal call site, the `ranting_derive/src/lib.rs` codegen call site
     (`ranting::inflect_possessive(...)`), the `src/language/verb.rs` doc
     comment, `ROADMAP.md`'s own references, and every use in
     `tests/ranting/property_based.rs` (including the two test fn names,
     `inflect_possessive_known_pronouns`/`inflect_possessive_with_case_flag`).
     `grep -rn inflect_possesive` (old spelling) over `.rs`/`.md` now returns
     zero hits outside historical planning docs (`PHASE_2_IMPLEMENTATION_PLAN.md`,
     `docs/superpowers/specs/...`, `docs/superpowers/plans/...`,
     `.superpowers/sdd/...`) — those are dated snapshots of past design
     discussion, not living reference docs, so left as-is rather than rewriting
     history; `CLAUDE.md`'s own mention of the typo (in its "remaining Phase 4
     items" summary line) was also left alone, matching the precedent set by
     items 1-4's commits, none of which touched that summary line even though
     it already listed items 2 and 3 as "still ahead" after they'd landed.
   - ✅ `#[doc(hidden)]` on macro plumbing: added to `handle_placeholder`
     (`src/lib.rs`) — `handle_placeholder_with_context` and
     `handle_tense_marker` turned out to already carry `#[doc(hidden)]` from
     earlier work, so only `handle_placeholder` needed the attribute, matching
     `HeedMatcher`'s existing treatment (`src/heed.rs`).
   - ✅ Reworked `ack!()`/`nay!()` (`ranting_derive/src/lib.rs`) from
     `parse_quote!(return Ok(#output))`/`return Err(#output)` to plain
     `parse_quote!(Ok(#output))`/`Err(#output)` — an ordinary expression, not a
     hidden control-flow statement. **This is a real breaking change**: every
     existing call site relying on the implicit early return needed either an
     explicit `return`, or to already be in tail-expression position.
     Verified the failure mode for call sites that get this wrong isn't
     silent: a bare discarded statement like `nay!("...");` fails to compile
     with `E0282 type annotations needed` in the common case (the `Ok<T>`
     side of `Result<T, E>` is otherwise unconstrained at that statement), and
     even when the type *is* fully inferable from context, `Result`'s
     `#[must_use]` still fires (`unused_must_use`, hard error under this
     repo's `-D warnings` gate) — so a caller who drops the `return` doesn't
     get silent fallthrough, they get a compile error pointing at the exact
     line, in every configuration tried.
     Updated call sites: `tests/ranting/male_female_and_object.rs`'s
     `Person::respond_to` — its `match` was already the function's tail
     expression, so every arm's `ack!(...)`/`nay!(...)` simply dropped its
     trailing `;`; no arm needed a `return` at all because the whole `match`
     was already in tail position (an intermediate draft added explicit
     `return`s here defensively, which clippy's `needless_return` then
     correctly flagged as unnecessary — removed again, since they were never
     needed in the first place). `docs/COOKBOOK.md`'s existing
     "Error handling with ack!()/nay!()" example needed no code change at
     all — its `if`/`else` was already the function's tail expression, so it
     now doubles as a working demonstration of the new expression form (not
     directly compiled by any test — `tests/ranting/cookbook.rs` backs other
     recipes but not this one — verified by hand-compiling an equivalent
     snippet in a scratch test file, since `person.name` in the doc's literal
     form isn't externally visible: `name` is `pub(crate)` on `Noun`).
     `README.md`'s bullet describing `ack!()`/`nay!()` was rewritten to
     describe the expression-based semantics (`Ok(say!(...))`/`Err(say!(...))`,
     usable as a plain expression, `return` is now the caller's job) instead
     of the old "provides an Ok()/Err() return" hidden-control-flow phrasing.
     The two doctests on `ack!`/`nay!` in `src/lib.rs` were updated to write
     `return ack!(...)`/`return nay!(...)` explicitly, since those examples
     are single-statement function bodies where the macro isn't in tail
     position.
   - ✅ New test file `tests/ranting/ack_nay_expression.rs` (registered in
     `tests/ranting/main.rs`), 4 tests proving the new expression-based
     surface: `ack!()`/`nay!()` bound directly to a `let` (not returned at
     all), used as `match`-arm tail values inside a function that never
     writes `return`, and — for contrast — still working when a caller
     chooses to write `return ack!(...)` explicitly (early-return from an
     `if` with no `else`, falling through to a tail-position `nay!(...)` on
     the implicit "no" path).
   - ✅ Full gate clean on all three crates; test counts against the
     pre-task baseline (`git show f3799244`: 39 lib + 260 integration + 11
     doctests in `ranting`, 14 in `ranting_core`, 9 unit in `ranting_derive`):
     `ranting` now 39 lib + 264 integration (+4, the new
     `ack_nay_expression.rs` tests) + 11 doctests (unchanged); `ranting_core`
     unchanged at 14 unit tests; `ranting_derive` unchanged at 9 unit tests,
     its one pre-existing doctest failure (`src/lib.rs - derive_ranting`,
     `unresolved import 'ranting'`) unaffected — same CLAUDE.md-documented
     proc-macro-crate-can't-self-test limitation, not a regression.
     `ranting_derive`'s standalone `cargo clippy --all-targets -- -D warnings`
     still fails on exactly the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` (dead code in `plurals.rs`, one
     `map_or` lint, two needless-borrow lints) — no new findings introduced by
     this item's changes.

6. ✅ **Hand-written placeholder tokenizer** (replaces `PH_EXT` regex internals)
   - ✅ Kept the sigil grammar (`PH_START`, `{...}` discovery) untouched — it's the
     crate's identity and was explicitly out of scope. Only `PH_EXT`'s
     internals-of-the-braces parsing changed.
   - ✅ New module `ranting_core/src/ph_ext.rs`: a hand-written recursive parser,
     `ph_ext::parse(s: &str) -> Result<PhExtMatch, PhExtError>`, called from
     `ranting_derive`'s `parse_str_params` (previously `PHE.is_match(...)` +
     `PHE.replace(...)`, `PHE` = `Regex::new(PH_EXT)`). `PhExtMatch` mirrors the
     handful of `regex::Captures`/`regex::Match` methods `handle_param` already used
     (`.name(...)`, `.as_str()`/`.start()`/`.end()`, plus `.whole()` replacing
     `.get(0)`), so `handle_param`'s ~200-line body needed only a signature change
     (`caps: &Captures` → `caps: &ranting_core::ph_ext::PhExtMatch`) — none of its
     logic changed. `PH_EXT` itself stays in `grammar.rs`, `#[allow(dead_code)]`, as
     the *reference grammar*: `ph_ext`'s own test suite differentially fuzzes the
     hand parser against `Regex::new(PH_EXT)` (a `regex` dev-dependency added to
     `ranting_core/Cargo.toml` for exactly this, not used by any non-test code), so
     the reference stays load-bearing for verification even though nothing parses
     against it at runtime anymore.
   - ⚠️ **Design surprise — `?+` is not possessive here.** The grammar's authors
     clearly intended `(?P<x>...)?+` as "optional, possessive/atomic" (match if
     possible, then never backtrack out of it). Two direct experiments against the
     `regex` crate disprove that reading: `(?:a)?+a` against `"a"` still matches
     (true possessive semantics would reject it), and — more consequentially —
     `(?P<x>[ab])?+c` against `"abc"` matches with `x = "b"`, i.e. the group matched
     *twice*, with only the *last* repetition's span kept as the named capture. So
     `X?+` empirically behaves like `X*` (zero-or-more, greedy, ordinarily
     backtracking) in this crate, not "optional, committed". This isn't a corner
     case nobody hits: `src/language/english.rs`'s pre-existing `verbs_deny`/`upper`
     tests use placeholders like `{=?w weren't}`, where `case` has to match a
     two-character run (`=` then `?`, keeping only `?`) for the existing behavior to
     make sense at all — a naive "optional presence" reading of `?+` cannot produce
     that. `uc`, `pre`, `nr`, and `case` are therefore all implemented as genuine
     repeated (star) groups via one generic engine, `star_candidates`, rather than
     four independent single-optional checks.
   - ⚠️ **Second design surprise — `pre`'s lazy "extra words" loop is reachable.**
     An early version of this module's doc argued (from a since-corrected mental
     model) that `pre`'s trailing `(?:\s+[\w-]+)*?` could never fire, since the
     zero-repetition attempt's local success/failure doesn't depend on repetition
     count. That reasoning only checked *local* success and missed that regex
     backtracking is driven by *overall* (downstream) success — proven wrong by this
     crate's own pre-existing doctest, `` {can `man pair of #0 boots remain} `` (on
     `inflect_possessive`), which needs `pre` to swallow two extra words (`pair`,
     `of`) after `` `man `` so that `nr` (`#0 `) and `noun` (`boots`) can start where
     the template actually intends. Fixed by `finish_pre_candidates`, which yields
     every extra-word count in lazy (fewest-first) priority order and lets the outer
     backtracking search (which already existed for the `?+`-is-really-`X*` finding
     above) reach for a non-zero count only when zero fails downstream.
   - ✅ **Error spans**: textual, not source-span-precise. Errors are `PhExtError {
     start, end, message }` — byte offsets into the placeholder-internals text
     (`ranting_derive` already offsets these into the full template string, exactly
     as it did with the old regex captures) plus a specific message, e.g. for input
     `` `=` ``: `` expected a noun or variable name, found `=` `` , or for
     `` `who run(` `` : `` expected article or verb, found ` run(` `` (the found
     text includes the leading space that separates the noun from its trailing
     content) — replacing the single blanket `"Error in placeholder"`. These
     still flow through `syn::Error`/`LitStr::slice(...).error(...)` exactly as
     before, so they still render as a real compile error pointing at the offending
     substring inside the user's source string literal (not just a stderr message) —
     this *is* source-span-precise in the sense that matters to a `say!()` caller,
     just not decomposed further (e.g. "expected one of: `a`, `an`, `the`, ...").
     Decomposing into per-alternative-expected-token messages was judged not worth
     it: the tokenizer's own internal structure (six `pre`-atom alternatives, a
     handful of modal words, nested articles) doesn't map cleanly onto a short
     "expected X" list without either truncating misleadingly or dumping the whole
     grammar into every error.
   - ✅ **No placeholder-acceptance regression.** `ph_ext::tests::parity_curated_corpus`
     (curated cases, including the two real multi-char-case-run and extra-words
     examples above) and `ph_ext::tests::parity_fuzzed` (a `proptest` property over
     uc/pre/nr/case/noun/post combinations) both assert the hand parser and
     `Regex::new(PH_EXT)` agree exactly — either both reject an input, or both accept
     it with identical capture text for every named group. Two apparent "regex
     inconsistencies" found while building this turned out, on closer inspection, to
     be gaps in the hand parser's own understanding of the grammar (the two design
     surprises above), not actual regex bugs — once fixed, no fuzz exclusions were
     needed at all.
   - ✅ New tests: `ph_ext::tests::error_message_specific_for_bad_post`,
     `error_message_specific_for_missing_noun` (assert on the new specific message
     text), `pre_extra_words_then_second_repetition` (regression test for the second
     design surprise above), plus the differential `parity_curated_corpus`/
     `parity_fuzzed` pair — 5 new tests in `ranting_core`, all in `src/ph_ext.rs`.
   - ✅ Full gate clean on all three crates; test counts against the pre-task
     baseline (`git show f1b08545`: 39 lib + 264 integration + 11 doctests in
     `ranting`, 14 unit in `ranting_core`, 9 unit in `ranting_derive`): `ranting`
     unchanged at 39 lib + 264 integration + 11 doctests (the two previously-passing
     doctests that exercise the fixed extra-words/case-run behavior —
     `language::english::inflect_possessive` and `lib.rs - ack` — kept passing
     throughout, since those bugs were caught and fixed before landing, not shipped
     as regressions); `ranting_core` now 19 unit tests (+5, `ph_ext`'s new tests
     above); `ranting_derive` unchanged at 9 unit tests, its one pre-existing
     doctest failure (`src/lib.rs - derive_ranting`, `unresolved import 'ranting'`)
     unaffected. `ranting_derive`'s standalone `cargo clippy --all-targets -- -D
     warnings` still fails on exactly the same 7 pre-existing findings documented in
     `docs/architecture-review-2026-08-13.md` — no new findings introduced by this
     item's changes.
   - This completes Phase 4's architecture-consolidation restructuring (items 1-6).
     Item 7 (licensing) remains an explicit human decision, not an agent task — see
     [PROPOSED LICENSE CHANGE](#proposed-license-change-awaiting-decision). Item 8
     (repo hygiene) was already complete.
   - **Follow-up, 2026-08-13**: the 7 pre-existing `ranting_derive` clippy
     findings referenced throughout this item's log (dead `plurals.rs` code +
     `map_or`/needless-borrow lints) are now fixed: `ranting_derive`'s dead
     `src/language/plurals.rs` module was deleted outright (see the Phase 4
     item 1 update above) rather than patched with `#[allow(dead_code)]`, and
     the two `needless_borrows_for_generic_args` findings in `src/lib.rs`
     (`x.ends_with(&['x', ...])` → `x.ends_with(['x', ...])`) were fixed
     directly. `cargo clippy --all-targets -- -D warnings` is now clean in
     all three crates individually and as a workspace.
   - **Follow-up, 2026-08-13**: the other `ranting_derive`-gate wart
     referenced throughout this item's log — the pre-existing
     `src/lib.rs - derive_ranting` doctest failure (`unresolved import
     'ranting'`) — is fixed too, the same way CLAUDE.md's "Doctests in
     proc-macro crate" note always said to fix it: the example (singular
     "they" via `#[derive_ranting]`) is now a real, passing doctest on
     `Ranting`'s trait doc in `ranting/src/lib.rs` (where `ranting` and
     `ranting_derive` are both actually in scope), and the original copy on
     `ranting_derive::derive_ranting` itself is marked `` ```rust,ignore ``
     with a comment pointing at the runnable copy, instead of silently
     failing every `cargo test --doc` run in that crate. `ranting_derive`'s
     doctest count goes from "9 unit + 1 failing doctest" to "9 unit + 1
     ignored doctest"; `ranting`'s doctest count goes from 11 to 12.

7. **Licensing decision** (orthogonal, but decides whether the rest gets an audience)
   - GPL-3 on a *library* crate is the single biggest adoption barrier: dependents'
     code must be GPL-compatible, so most of the Rust ecosystem won't touch it.
     Decide deliberately; MIT/Apache-2.0 dual is the ecosystem norm.
   - Either way, prefer `license = "..."` over `license-file` in Cargo.toml so
     tooling (lib.rs, cargo-deny, license scanners) can classify it.
   - ✅ **Decided 2026-08-13**: relicensed to plain `MIT` (not the dual
     `MIT OR Apache-2.0` this section recommended below — the copyright holder's
     explicit choice). `license = "MIT"` in `Cargo.toml`, `ranting_core/Cargo.toml`,
     `ranting_derive/Cargo.toml`; `LICENSE.txt` replaced with the MIT text; all
     `// (c) Roel Kluin <year> GPL v3` source headers updated to `MIT`. Sole
     copyright holder (verified via `git shortlog -sne --all`), so no consent round
     was needed. Note: the already-published 0.2.1 on crates.io remains available
     under GPL-3 — crates.io releases are immutable, so MIT applies from the next
     published version onward.

✅ **8. Repo hygiene** (COMPLETE)
   - ✅ Untracked (`git rm --cached`, files kept on disk) and added to `.gitignore`:
     `ranting.log`, `src.txt`, `git_diff.txt`, `git_status.txt`, `code_analysis.txt`,
     `review.jsonl`, `dodev`, `ideas`, `ranting_derive/ranting_derive.log`,
     `ranting_derive/src.txt` — these previously shipped in the published package.
   - ✅ Also ignored `.cargo-home/` (the container-local `CARGO_HOME` from CLAUDE.md),
     which was untracked but still being swept into `cargo package`.
   - Still shipping, deliberately unreviewed here: `git_log_oneline.txt`, `tasks.txt`,
     `mksrc.sh`, `ranting_derive/mksrc.sh`, `scripts/overnight_loop.sh`,
     `.superpowers/sdd/**`, and the top-level design/report `.md` files. Decide per
     file whether they belong in the crate or behind a Cargo `exclude` list.

---

## Historical license change analysis (decided, implemented — kept for context)

> **Status: DECIDED and IMPLEMENTED 2026-08-13 — relicensed to plain MIT.** The
> analysis below recommended dual `MIT OR Apache-2.0`; the copyright holder chose
> plain `MIT` instead. All crates carry `license = "MIT"`, `LICENSE.txt` holds the
> MIT text, and source headers read `// (c) Roel Kluin <year> MIT`. Nothing below
> is a pending action — the "Current state" and "If approved" sections describe
> the pre-decision GPL-3 baseline this analysis was written against, kept only
> for the historical tradeoff reasoning. See the *Key Architecture Decisions*
> table for the authoritative current state.

### State at the time of this analysis (superseded — see banner above)

| Where | Value (2026-08-13, pre-decision) |
|-------|-------|
| `Cargo.toml` (ranting) | `license-file = "LICENSE.txt"` |
| `ranting_derive/Cargo.toml` | `license-file = "../LICENSE.txt"` |
| `LICENSE.txt` | verbatim GNU GPL v3 text (no "or later" wording added) |
| `src/lib.rs`, `ranting_derive/src/lib.rs` | `// (c) Roel Kluin 2022 GPL v3` |
| Published | `ranting` / `ranting_derive` 0.2.1 on crates.io, under these terms |

Two facts that shaped the decision, both verified in this repo at the time:

- **Sole copyright holder.** `git shortlog -sne --all` shows every commit authored
  by Roel Kluin (four spellings of the same address). There are no third-party
  contributions to re-license, so no CLA round or consent-gathering is needed —
  the decision is unilateral today. That stops being true the first time an
  outside PR is merged.
- **No inbound copyleft.** The grammar data (`data/irregular_verbs.txt`,
  `data/irregular_plurals.txt`) is hand-maintained in-repo, not imported from a
  copyleft or share-alike corpus, so nothing forces GPL from the dependency side.
  *To confirm before acting:* run `cargo deny check licenses` (or `cargo license`)
  over both crates — the direct dependency set is the usual permissive
  MIT/Apache-2.0 proc-macro stack, but it has not been machine-verified here.

### The tradeoff

GPL-3 on a library is not "GPL for this repo" — it is a constraint on every
dependent. Because `ranting` is consumed *only* as a dependency, the license
choice is effectively a choice about who is allowed to use the crate at all.

The proc-macro angle sharpens this. `say!()` does not just get linked; it expands
into the dependent crate's own source at compile time. Whether macro-expanded
output is a derived work of the macro is exactly the kind of question a corporate
legal review will not want to answer — and the safe answer they will give is "use
something else". A GPL proc-macro is a harder sell than a GPL rlib, not an easier
one.

That collides with two commitments already in this roadmap:

- **Vision** targets "game engines, interactive fiction, chatbots" — overwhelmingly
  proprietary or mixed-license codebases.
- **v1.1 success criteria** call for "2-3 ecosystem forks", and Phase 3 item 2
  shipped trait extensibility explicitly to enable `ranting-spanish`,
  `ranting-pirate`, etc. GPL-3 discourages precisely the downstream ecosystem the
  extensibility work was built to attract.

| | GPL-3.0-only (status quo) | MIT OR Apache-2.0 (dual) |
|---|---|---|
| Adoption | Blocks proprietary and most permissive-licensed dependents | Ecosystem default; no review friction |
| Ecosystem forks (v1.1 goal) | Forks must stay GPL; most won't start | Forks unconstrained |
| Improvements flow back | Copyleft obligation on derivatives | Voluntary only |
| Patent grant | Yes (GPL-3 §11) | Yes, via the Apache-2.0 arm |
| Compatibility | Incompatible with Apache-2.0-only dependents | Compatible with essentially everything |
| Reversibility | Can relax to permissive later (sole holder) | Cannot tighten later for released versions |

Two middle options, both weaker than they look:

- **LGPL-3**: designed around dynamic relinking, which Rust's static linking,
  generics, and macro expansion make ill-defined. It buys legal ambiguity rather
  than a real middle ground.
- **GPL-3 + linking exception**: workable, but it is a bespoke license. Scanners
  and `cargo-deny` policies classify it as GPL, so most of the adoption cost
  remains while the copyleft benefit mostly disappears.

### Recommendation

**Adopt `license = "MIT OR Apache-2.0"` for both crates**, matching the Rust
ecosystem norm.

The copyleft protection GPL-3 provides is worth little here — this is a text
formatting library, not a product with a business model to defend — while its cost
is the entire audience the roadmap is written for. Dual MIT/Apache-2.0 also keeps
the Apache-2.0 patent grant, which MIT alone would not.

Timing matters: this belongs in v1.2 alongside the other pre-adoption breaks
(see *Premature API Lock-in* below). Relicensing is free while the author is the
only copyright holder and there is no userbase; each new contributor and dependent
raises the cost permanently.

**If the decision is instead to keep GPL-3**, the Cargo.toml change should still
happen: replace `license-file` with `license = "GPL-3.0-only"` so lib.rs,
cargo-deny, and license scanners can classify the crate. (`GPL-3.0-only` matches
the current wording — `LICENSE.txt` is the plain GPLv3 text and the source headers
say "GPL v3" with no "or later". If "or later" was the intent, record
`GPL-3.0-or-later` instead and say so in the headers.) Choosing this consciously
is a valid outcome; the roadmap only asks that it stop being an accident.

### Implementation checklist this analysis proposed (not what was actually done)

The recommendation above was `MIT OR Apache-2.0`; the actual decision was plain
`MIT`, so this checklist doesn't match what shipped in every particular (no
`LICENSE-MIT`/`LICENSE-APACHE` split, `LICENSE.txt` was kept and now holds the
MIT text instead of being deleted). Kept verbatim for the historical record.

1. Verify dependency licenses: `cargo deny check licenses` over both crates.
2. Replace `license-file` with `license = "MIT OR Apache-2.0"` in `Cargo.toml` and
   `ranting_derive/Cargo.toml`.
3. Add `LICENSE-MIT` and `LICENSE-APACHE`; delete `LICENSE.txt` (or keep it only if
   some component genuinely stays GPL — currently none does).
4. Update the `// (c) Roel Kluin 2022 GPL v3` headers in `src/lib.rs` and
   `ranting_derive/src/lib.rs`; add a License section to `README.md`.
5. Note in `CHANGELOG`/release notes that the already-published 0.2.1 remains
   available under GPL-3 — crates.io releases are immutable, so the new terms
   apply from the next published version onward.
6. Check the box in Phase 4 item 7 and flip the *Key Architecture Decisions* row
   from 🔄 to ✅ with the chosen SPDX expression.

---

### v1.2 Success Criteria
- One shared `ranting_core` crate; zero build.rs copy/symlink mechanisms remain
- No unmaintained dependencies (RUSTSEC-clean); single strum/regex/syn versions
- No stringly-typed macro↔runtime interface; no `~TENSE~` sentinel
- No runtime panics reachable from public API with invalid data
- Placeholder syntax errors report precise spans
- License decision made and recorded

---

## Phase 5 — v1.2.1 — `ask!()` Stabilization ✅

*Goal: give `ask!()` a real, typed contract and test coverage, closing the gap
flagged in the 2026-08-13 docs audit — zero usage anywhere in the repo, no
trait, and an untyped `.answer()` call the macro simply trusted exists.*

**Status**: Complete. Breaking change to `ask!()`'s public signature and
return type, landed deliberately: 0.2.1 has no adopters yet, and Phase 4's
own rationale (pay down structural debt while there's no userbase to break)
applies here too.

1. ✅ **`Answerable` trait + capture-forwarding `ask!()`**
   - New `Answerable` trait in `ranting` (`src/answerable.rs`): `fn answer(&self, speaker: &dyn Ranting, captures: Self::Captures) -> String`, with `type Captures` declared per implementor — bare `String` for a single capture, `()` for none, a tuple of `String`s for 2+, mirroring `heed!()`'s existing 0/1-vs-2+ return convention exactly (no `$name` → `u64` auto-typing through the trait boundary; a caller that needs a typed value parses the `String` itself inside `answer()`).
   - `ask!()` reworked from `ask!(speaker, audience, "fmt", args...)` (rendered a question via `say!()`-style formatting, then called `audience.answer(speaker, String)` as untyped duck-typing) to `ask!(speaker, audience, "template", input)` — reuses `heed!()`'s template-parsing engine (`ranting_derive/src/heed.rs::compile_heed_template`) to match `input` against `template`'s `{name}`/`{name...}`/`{$name}` captures, then forwards them to `audience.answer(&speaker, captures)`. Returns `Option<String>` (`None` on no match, `answer()` not called), joining `heed!()` in the crate's Option-returning macros instead of returning whatever `.answer()` happened to return.
   - `ask!` is now re-exported from `ranting` (`pub use ranting_derive::ask;`) — previously the only one of the five core macros not re-exported, an oversight from when it was untested.
   - Known, accepted limitation: `Captures` being an associated type means one type supports exactly one template capture-arity everywhere it's used as an `ask!()` audience — a type needing to answer differently-shaped questions would need `Captures = Vec<String>` (losing arity checking at the call site) or separate wrapper types. Not solved here; no current caller needs it.
2. ✅ **Tests + docs**
   - `tests/ranting/ask.rs`: 5 integration tests — 0/1/2-capture arities, a capture-driven response (`Villager` matching on `topic`), and a no-match `None` case.
   - `docs/API.md`/`docs/CHEATSHEET.md`/`CLAUDE.md` updated to the new signature; the "sparsely used/documented, less-stable corner" caveat lifted now that tests exist.

**Out of scope, proposed for a separate higher-level crate**: Inform7-style
object disambiguation — resolving *which* candidate `Ranting`/`Answerable`
object free-text input refers to, weighted by "likely"/"unlikely" rules when
multiple objects could match (e.g. "talk to it" being far more likely to mean
a nearby person than a stone). This is a world-model/candidate-registry
problem with no existing shape in this crate — `ask!()` only ever targets one
statically-known `audience` expression — and doesn't belong in `ranting`
itself. See [v1.3.0+](#v130-beyond-phase-6) below for where a
`ranting`-adjacent crate proposal like this belongs.

---

## Phase 6 — v1.3.0 — Internationalization Foundations

*Goal: make `ranting-i18n` **buildable** — not build it. This phase lands, in
`ranting`/`ranting_core`, the signals a non-English `Ranting` implementation
needs and cannot currently obtain, then proves the set is sufficient with one
reference lexicon. Language-specific vocabulary and rules stay out of `ranting`
and go in the companion crate; only the mechanism lands here.*

**Framing.** The 2026-08-13 architecture review's German spike
(`docs/architecture-review-2026-08-13.md` §7) is the model for this whole phase:
a concrete non-English sentence is attempted through the existing hooks, the
signal that turns out to be missing is named, and exactly that signal is added
to the trait seam. `GrammaticalCase` (v1.3, commit `11d531ed`) was the first
such fix and is the **reference implementation pattern** for items 2 and 5–8
below: mirror a `ranting_core` type into a public `ranting` type via `From`
(never expose `ranting_core` — it is not part of `ranting`'s semver surface),
add the parameter to the `_custom` hook *and* its `_with_context` twin, thread
it from `handle_placeholder_impl`, add a `tests/ranting/*.rs` file with a
worked example in a real language.

**What is explicitly not in scope**: shipping German/French/Japanese lexicons in
`ranting` itself, and any translation-catalogue/message-format machinery
(`gettext`, ICU MessageFormat, Fluent). Ranting inflects text a program already
composes; it is not a translation system, and conflating the two is the failure
mode this phase is structured to avoid.

**Ordering rationale**: items 1, 3 and 4 are design spikes that produce
documents, not code, because each of them either has several defensible answers
(word order) or forces a breaking change to a signature that appears in every
hook (number). Deciding them on paper first is what keeps items 2 and 5–9 from
being rewritten mid-phase. Items 5–8 are independent of each other and can land
in any order. Item 10 is the acceptance test for items 1–9 and must land last.

1. ✅ **Word-order & template-slot design spike** (doc-only, 6-10 hours) —
   *blocks item 10; de-risks the whole phase*
   - The unclosed half of the German spike: `GrammaticalCase` fixed article
     declension, but word order is still baked into the literal string
     *between* placeholders, in English's fixed slot order. `say!("{=dog}
     {dog bark} at {@cat}")` cannot become German verb-second or Japanese SOV
     by any per-noun inflection hook, because no hook can move text it does
     not own.
   - Enumerate and score at least these options in
     `docs/superpowers/specs/2026-08-13-word-order-feasibility.md`, in the
     style of `2026-08-12-input-parsing-feasibility.md` (which is the
     precedent for "spike concludes *don't build this*" being an acceptable
     outcome):
     (a) **per-language template sets** — the caller selects the template
     string by language, `ranting` only inflects within it; costs nothing in
     the crate, pushes the work to the caller;
     (b) **numbered slots with per-language reorder metadata** — templates
     declare roles (`{subj}`/`{verb}`/`{obj}`) and a language module supplies
     a permutation;
     (c) **a syntax-tree API** (`sentence!(subject, verb, object)`) that
     renders through a language module, sidestepping literal templates
     entirely.
   - Deliverable is a recommendation with rejected alternatives recorded and
     *why*, plus an explicit statement of which parts of `ranting-i18n` remain
     impossible under the recommendation. A "(a), documented as a permanent
     boundary" conclusion is a legitimate result — it is likely the honest
     one, and stating it plainly is more valuable than a half-built (b).
   - No production code, so it lands trivially green; nothing after it has to
     guess.
   - ✅ **COMPLETE 2026-08-13** —
     `docs/superpowers/specs/2026-08-13-word-order-feasibility.md`. Conclusion:
     **(a) per-language template sets, documented as a permanent boundary.**
     Doc-only as scoped; no production code, no test changes.
   - Grounding the spike established in `handle_placeholder_impl`
     (`src/lib.rs`) and `impl ToTokens for Say` (`ranting_derive/src/lib.rs`):
     the seam is a single `format!(lit, args…)` whose literal (all
     inter-placeholder text) and argument order are compile-time constants; a
     hook receives `(&R, poss, nr, uc, spec, ctx)` with no sibling reference
     and returns one `String` for one hole. Sharper than expected:
     **intra-placeholder order is hard-coded too** — the fixed
     pre→`nr`→noun→post `push_str` sequence blocks suffixed definite articles
     (Romanian/Norwegian/Bulgarian) *inside* a single placeholder,
     independently of any cross-placeholder question.
   - (b) numbered slots + reorder metadata — **rejected, blocked by
     mechanism**: a runtime permutation has nowhere to apply without replacing
     `format!()` codegen with a runtime assembler (reversing the ✅ Locked
     "Compile-time parsing + runtime inflection" decision); inter-placeholder
     glue words (English `at`) need deletion/insertion, not permutation, which
     is translation and explicitly out of the phase's scope; and it would have
     to make the intra-placeholder assembly order data as well. Undisclosed
     prerequisites it drags in: a sentence-initial-`uc` signal (item 6,
     extended — `uc` is decided by template position at compile time and
     cleared after the first emission, so permuted slots break it) and a
     matching reorder story for `heed!()`/`ask!()`, which compile the same kind
     of literal template.
   - (c) `sentence!()` syntax-tree API — **rejected, blocked by identity, not
     by feasibility**: it demonstrably works (no literal template means no text
     the crate doesn't own), but abandons the sigil grammar the
     architecture-decisions table marks ✅ Locked as "the crate's identity",
     ships a permanent second product surface beside `say!()`, and puts
     per-language syntax rules back inside the crate. If ever wanted, it
     belongs in a downstream crate depending on `ranting` for morphology —
     the `ranting-if` shape — not in Phase 6.
   - Also rejected: an "(a) now, (c) later" hedge (leaves the boundary open in
     the docs and invites items 2–9 to be designed against a future that will
     not arrive), and a point fix adding a second verb position to the
     placeholder grammar (addresses only German separable prefixes, adds
     grammar surface for every English user, leaves the rest of the list).
   - **Recorded for item 10** (which requires this outcome be written in item
     1's spec and the companion crate's README): German verb-second and
     clause-final verb placement are *not* reachable through `ranting`'s hooks;
     the German reference lexicon must carry word order in its own template
     strings. Stays impossible under (a), by named construction: German V2 with
     a clause-final separable prefix/participle (one verb, two positions — and
     `handle_placeholder_impl` asserts a placeholder cannot have both a pre-
     and a post-verb), Japanese/Korean/Turkish SOV with postpositions, VSO
     (Irish/Welsh/Classical Arabic), suffixed definite articles, Romance
     post-nominal adjective position (item 5 gives agreement, i.e. the right
     *form*, never movement), sentence-final question particles, and any
     construction where a word is deleted rather than inflected.
   - Follow-up, non-blocking: where the boundary statement lands
     (`docs/EXTENSIBILITY.md` in full, one-line pointers from README.md and
     CLAUDE.md), and whether item 6's orthography hook should expose
     "sentence-initial" explicitly rather than the implicit `uc: bool`.
   - ✅ **Confirmed by item 10, from the other side**: the German reference
     lexicon carries word order in its own template strings, exactly as (a)
     predicts, and `ranting_i18n/README.md` §"Word order" states it plainly as
     this item requires. Verb-second is reachable only by writing a German
     template; a verb split across two positions (`Der Hund macht die Tür auf`)
     is not expressible at all. Pinned by `hole_8_*` in
     `ranting_i18n/tests/holes.rs`. No change to this spec was needed — the
     "Recorded for item 10" bullet above already said it.

2. ✅ **Lexical gender / noun-class channel** (10-14 hours) — *the single
   highest-leverage enabling change*
   - Today a fork has no way to learn a noun's gender: `inflect_article_custom`
     receives `noun_singular` as a bare `&str`, so `ranting-german` must keep an
     external `HashMap<&str, Gender>` keyed by display string — which breaks on
     homographs, on names, and on any noun built at runtime. Gender is a
     property *of the entity*, exactly like `subject`, and belongs on the noun.
   - Follow `SubjectPronoun`'s Phase 4 item 4 treatment: a public open-ended
     noun-class channel on `Noun` (a `#[ranting(gender = "...")]` derive
     attribute plus a `Ranting::noun_class()` trait method defaulting to
     "unset"), deliberately *not* a closed `enum { Masculine, Feminine, Neuter }`
     — Bantu languages have a dozen-plus classes and Danish has common/neuter,
     so a closed English-adjacent enum would be wrong on arrival. A newtype over
     a `&'static str` class label keeps `ranting` agnostic about what the
     classes are.
   - Thread it into `inflect_article_custom`/`_with_context`,
     `inflect_pronoun_custom`/`_with_context`, and (once item 5 lands) the
     adjective hook, mirroring `GrammaticalCase`'s threading exactly.
   - Must be additive: existing impls that don't set a class keep today's
     behavior byte-for-byte. Worked test: German `der Hund`/`die Katze`/`das
     Haus` selecting three different articles from one code path.
   - ✅ **COMPLETE 2026-08-13** — `NounClass` (public, in `ranting`), a
     defaulted `Ranting::noun_class()`, a `#[ranting(gender = "...")]` derive
     attribute, and a `class: NounClass` parameter on
     `inflect_article_custom`/`_with_context` and
     `inflect_pronoun_custom`/`_with_context`. 293 integration tests + 14
     doctests pass under both `cargo test` and `cargo test --all-features`
     (285 + 12 before; 8 new tests in `tests/ranting/noun_class.rs`), with
     every pre-existing assertion byte-identical — only hook signatures in
     those files changed. `cargo clippy --all-targets --all-features
     -D warnings` is clean, as is `ranting_derive`'s standalone clippy.
   - **Deviation from item 1's stated reference pattern, deliberately**:
     `GrammaticalCase` mirrors `ranting_core::placeholder::CaseKind` via `From`
     because `CaseKind` lives at the macro↔runtime seam — the derive macro
     parses it out of placeholder syntax and bakes it into a `PlaceholderSpec`.
     A noun class is never written in a placeholder; it comes off the entity at
     runtime, so there is no `ranting_core` type to mirror and none was
     invented. `NounClass` is defined in `ranting` alone. Everything else in the
     pattern was followed: parameter on the `_custom` hook *and* its
     `_with_context` twin, threaded from `handle_placeholder_impl` /
     `get_article_or_so`, worked example in a real language under
     `tests/ranting/`.
   - **Newtype over `&'static str`, as scoped** — `NounClass::new(label)`,
     `as_str()`, `is_unset()`, and `UNSET` (defined as the empty label, so a
     fork matching on `as_str()` sees `""` and "absent" as one case, not two).
     `Copy`/`Eq`/`Hash`/`Display`/`Default`. What `&'static str` makes static is
     the *set of labels*, not the *assignment*: which label an entity carries is
     ordinary per-value data, which is what fixes the homograph problem the item
     motivates — `tests/ranting/noun_class.rs` asserts `die Band` (music group)
     and `das Band` (ribbon) are distinguishable, where a
     `HashMap<&str, Gender>` keyed by display string has one entry for both.
   - **Threading is redundant for a plain impl but load-bearing for the
     wrappers**: the hook already has `self`, hence `self.noun_class()` — but
     `Many`/`Maybe`/`Box` read the class off the *wrapper* at the call site
     while `self` inside the delegated-to hook is the *inner* value, so the
     parameter is what keeps those consistent. All three now override
     `noun_class()`: `Many` reports its single item's class only at
     `len() == 1`, `Maybe(None)` and a 0-or-2+ `Many` report `UNSET` (no one
     unambiguous class to report). `ref_expr_ranting_trait` delegates it too,
     alongside `skip_article`.
   - **Additivity, mechanically**: an absent `gender` attribute generates *no*
     `noun_class()` override at all, so derived impls are byte-identical to
     pre-change codegen rather than merely equivalent. `gender = "$"` reads a
     `gender: ranting::NounClass` field (attribute name == field name, the same
     rule as `name`/`subject`) — that is how `Noun` carries one, since its
     `Ranting` impl is derived and cannot be hand-extended with a second
     `impl Ranting for Noun`; the public setter is the chaining
     `Noun::with_noun_class(NounClass)`.
   - **Not threaded into `inflect_verb_custom`**, deliberately: verb agreement
     in the target languages is person/number, not noun class, and the item
     scopes the verb hook out. `inflect_article_custom_with_context` (8 args
     with `self`) and `inflect_pronoun_custom_with_context` carry
     `#[allow(clippy::too_many_arguments)]` — flat args on a public hook, unlike
     `GrammaticalCase`'s struct-bundling of the *private* `get_article_or_so`.
     The adjective hook is item 5's to extend.
   - Documented in `docs/EXTENSIBILITY.md` §2.4 (with the `der Hund`/`die
     Katze`/`das Haus` table and a note that §4.3's Spanish example's
     spelling-based gender heuristic is what `class` replaces — it gets
     `el problema`/`la mano` wrong), `docs/API.md`, and a CLAUDE.md
     "Non-obvious behaviors" bullet. The `gender` attribute was also added to
     the three other places that enumerate the derive attributes: README.md's
     core-attributes list, and the `// ## Derive Attributes` comment block
     above `pub trait Ranting` in `src/lib.rs`.
   - ⚠️ **Item 10 follow-up**: `NounClass` itself came through the German
     lexicon intact — one code path, all three genders, gender read off the
     entity. Two adjacent gaps it does *not* close were recorded there instead:
     `GrammaticalCase` has no dative, so a German fork ends up ignoring the
     `case` parameter entirely (README hole 3), and `inflect_pronoun_custom`
     serves both case marking and pronoun display, so a fork that renders
     "Der Hund" cannot also render "ihn" for the same entity (hole 5). See
     `ranting_i18n/README.md`.

3. ✅ **Pronoun-inventory & T-V register design spike** (doc-only, 8-12 hours) —
   *the deepest open question in the phase*
   - `SubjectPronoun` is a closed enum of English pronouns, now typed into
     `Noun` (Phase 4 item 4) and matched exhaustively with `#[deny(...)]`
     guards throughout `src/language/english.rs` — deliberately, and that
     exhaustiveness is a stated architecture decision, not an accident. It
     cannot express: T-V distinction (`du`/`Sie`, `tu`/`vous`),
     clusivity (inclusive vs. exclusive "we"), dual number, or gendered plurals
     (`ils`/`elles`).
   - Note the overlap to resolve: `NarrationContext.register`
     (`Formal`/`Neutral`/`Casual`, Phase 3 item 4) already exists and is
     *inert* — no built-in behavior reads it. T-V selection is the obvious
     first real consumer, but `register` is a story-wide setting while T-V is a
     per-addressee relationship, so they are not the same axis. The spike must
     say which one owns T-V, or that both do and how they compose.
   - Options to score in
     `docs/superpowers/specs/2026-08-13-pronoun-inventory.md`: extend the enum
     with non-English variants (breaks the "English rules live in
     `src/language/english.rs`" separation); make the pronoun channel open like
     item 2's noun class (loses exhaustive-match safety the architecture
     decisions table calls out); or keep `SubjectPronoun` English-only and give
     forks a parallel pronoun-set trait that `handle_placeholder_impl` consults
     first (most conservative, most plumbing).
   - Deliverable: recommendation + explicit note of which option is breaking
     and for whom.
   - ✅ **COMPLETE 2026-08-13** —
     `docs/superpowers/specs/2026-08-13-pronoun-inventory.md`. Conclusion:
     **(c) keep `SubjectPronoun` English-only, in its *thin* form — the
     parallel fork-owned pronoun set already exists; document it, change
     nothing.** Doc-only as scoped; no production code, no test changes.
   - Grounding: the option-(c) deliverable is already installed.
     `handle_placeholder_impl` (`src/lib.rs:479-553`) consults
     `inflect_pronoun_custom_with_context` **first** in all five `CaseKind`
     pronoun arms, English being the `else` branch; `conjugate_verb`
     (`src/lib.rs:305`) and `get_article_or_so` do the same. The subject label
     reaches those hooks uninterpreted (`pluralize_pronoun`'s English
     rewriting happens inside the *fallback*), and
     `Ranting::subjective() -> &str` is a string, not a `SubjectPronoun`.
     Sharper than expected: `ranting_derive/src/ranting_impl.rs:174-181`
     emits a literal `subject` attribute **without validating it against
     `is_subject`**, so `#[ranting(subject = "Sie")]` compiles today and
     `subjective()` returns `"Sie"`. Only `Noun` is closed (typed
     `SubjectPronoun` field + `try_new`), so a fork declares its own
     `#[derive_ranting]` struct — the whole of what (c) costs.
   - **Correction to this item's own premise, recorded**: the
     `#[deny(clippy::wildcard_enum_match_arm)]` guards in
     `src/language/english.rs` sit on `ArticleOrSo` (line 20) and
     `IrregularPluralVerb` (line 68), not on `SubjectPronoun`.
     `SubjectPronoun`'s net is the compiler's own exhaustiveness check on a
     closed enum at three total matches (`as_str`, `is_subjective_plural`,
     `pronoun_forms`) plus three `SubjectPronoun::iter()`-driven tripwire
     tests, one asserting an exact variant count. Stronger than a lint — which
     is what makes option (b)'s loss of it total rather than partial.
   - (a) extend the enum with non-English variants — **rejected**: puts
     non-English vocabulary in `ranting_core::grammar` and forces
     `pronoun_forms` (an *English* module) to answer "what is `Sie`'s
     reflexive?"; unbounded, with no defensible stopping point (the same
     argument that made item 2's `NounClass` open); and **semver-major for
     every downstream user, English-only ones included** — `SubjectPronoun` is
     `pub use`-re-exported (`src/lib.rs:56`) and carries no
     `#[non_exhaustive]` (verified: the attribute appears nowhere in `src/` or
     `ranting_core/src/`), so any downstream exhaustive `match` stops
     compiling. Exhaustive-match safety is *preserved* here — that is the
     option's cost, not its flaw.
   - (b) open pronoun channel, `NounClass`-style — **rejected, on the
     exhaustive-match trade specifically**: it swaps a hard build failure for
     wrong output. Five sites in `src/language/english.rs` (`inflect_adjective`
     /`_subjective`/`_objective`/`_possessive`/`_reflexive`) do
     `SubjectPronoun::from_str(..).unwrap_or(SubjectPronoun::It)`, so an
     unhandled label silently renders `it`/`its`/`itself`; `inflect_verb`'s and
     `pluralize_pronoun`'s string matches pass it through to their `_` arms;
     `is_subjective_plural` and `is_first_person_subject` silently answer
     `false`. The `NounClass` precedent does **not** transfer: the crate never
     *reads* a noun class, it only forwards it, whereas it reads the subject
     label at every site just named.
     Breaking as a **stated-invariant** break, not a signature one — it
     reverses Phase 4 item 4's "invalid subjects unrepresentable" and makes
     `Noun::try_new`/`InvalidSubjectError` decorative, with no compile error
     anywhere to mark it. (c) breaks nobody.
   - **T-V ownership, resolved by collapsing the axis**: German `Sie` and
     French `vous` *are* pronoun slots (3pl/2pl reused as polite 2sg), not
     modifiers over a pronoun — so under (c) the addressee's declared subject
     label already carries the distinction and **no new per-addressee channel
     is needed**. T-V is per-addressee (one scene addresses one character `du`
     and another `Sie`), which is why story-wide `register` cannot own it.
     `NarrationContext.register` stays story-wide and **stays inert**: its
     documented role is a default for the *indifferent* case only, with the
     precedence rule — declared label > `ctx.register` > nothing — stated in
     the spec and `docs/EXTENSIBILITY.md`, never arbitrated in-crate (that
     needs knowing two labels denote one referent addressed two ways, which is
     fork knowledge).
   - Also rejected: a separate per-addressee `Politeness`/`Honorific` channel
     (redundant with the subject label, and would need in-crate arbitration
     against it); making `register` drive T-V in-crate (wrong granularity —
     mixed-formality addressees in one sentence are the common case); and
     adding `#[non_exhaustive]` to `SubjectPronoun` pre-emptively (costs a
     major version to buy flexibility only the rejected (a) would use).
   - **No dependency on item 4**: formal `Sie` (plural agreement, singular
     reference) is already expressible via the shipped singular-"they"
     precedent — `is_subjective_plural("they") == true` drives agreement while
     reference stays singular. Item 4 should not count T-V among its motivating
     cases; genuine dual/paucal remain its problem.
   - **Recorded for item 10** — what stays out of reach, by named
     construction: a true speaker×addressee T-V *relation* (`say!()` has no
     speaker channel; `ask!()`'s speaker isn't threaded into inflection, so the
     caller selects the addressee representation before the macro); `Noun` as a
     carrier for a non-English pronoun; and in-crate `register`-vs-label
     arbitration. One gap has **no fork-side workaround**:
     `narration::is_first_person_subject` is `matches!(subject, "I" | "we")`,
     so a fork whose labels are `ich`/`wir` gets a silent no-op from
     `NarrationContext.narration_person`, and `resolve_viewpoint` is
     `pub(crate)`, consulted before any trait method.
   - Follow-up, non-blocking: whether `is_first_person_subject` should become a
     `Ranting` hook defaulting to today's behavior (small, additive,
     English-preserving — but production code, hence out of a doc-only item);
     the `docs/EXTENSIBILITY.md` section on non-English pronoun inventories
     (open `subjective()` channel, the five `PronounCase` arms, the T-V
     precedence rule, and the `unwrap_or(It)` degrade as the documented
     consequence of an unhandled arm); and whether item 10's German lexicon
     should exercise `du`/`Sie` as the cheapest validation of this spike's
     central claim.

4. ✅ **Number-category design spike** (doc-only, 6-8 hours)
   - Number is boolean everywhere in the crate — `is_plural()`,
     `as_plural: bool` in all six `_custom` hooks, `inflect(to_plural: bool,
     ...)`, the `+`/`-` markers, `#var`/`$var` numeric agreement. Arabic dual,
     Slavic paucal, and CLDR's `zero/one/two/few/many/other` do not fit a bool,
     and Welsh/Irish have number-triggered mutation on top.
   - This is a **breaking change to every hook signature** if taken, which is
     why it is a spike and not an implementation item: the cost has to be
     stated before it is paid. Score at least: (a) leave it boolean and
     document non-English plural categories as out of scope; (b) add a parallel
     `plural_category()` channel alongside the bool, bool staying authoritative
     for English; (c) replace the bool with a `Number` enum in a single
     coordinated breaking release.
   - Record the finding in
     `docs/superpowers/specs/2026-08-13-number-categories.md` and, whichever
     option is recommended, state plainly in ROADMAP.md and CLAUDE.md what
     `as_plural: bool` does and does not promise, so a fork author isn't
     surprised.
   - ✅ **COMPLETE 2026-08-13** —
     `docs/superpowers/specs/2026-08-13-number-categories.md`. Conclusion:
     **(b), narrowed — keep the bool authoritative for English and add a
     *count* channel (not a category channel) to the hooks, folded into item
     5's already-planned signature change.** Doc-only as scoped; no production
     code, no test changes. The recommended change is *not* implemented by
     this item and must be scheduled — item 5 is the recommended landing site.
   - **What `as_plural: bool` promises** (the statement this item requires in
     both files; the CLAUDE.md "Non-obvious behaviors" bullet is its twin):
     *render the plural **agreement** form*, resolved per placeholder
     occurrence at `src/lib.rs:371-381`. It does **not** promise the referent
     count is > 1 — `is_subjective_plural("they")` is `true` (singular *they*:
     plural agreement, singular reference), an empty `Many` is plural ("there
     **are** no items"), and `inflect_reflexive` special-cases the same bool
     for `yourself`/`yourselves` because "you" is number-underspecified
     elsewhere. It does not let a fork recover the count either.
   - **Inventory, as the item asked** — the bool appears in: the 6 `_custom`
     hooks (`src/lib.rs:1121`-`1289`) plus `is_plural()`/`inflect(to_plural,..)`
     (`:1077`/`:1081`); its single computation site `as_pl`
     (`src/lib.rs:371-381`) and every consumer (`ArticleRenderCtx.as_pl`,
     `conjugate_verb`, the five `CaseKind` pronoun arms, `adapt_possesive_s`);
     nine functions in `src/language/english.rs` (three of them **public**:
     `inflect_possessive`, `inflect_reflexive`, `inflect_noun_irregular`) and
     `plurals.rs`'s `get_plural`/`get_singular`; public
     `is_subjective_plural` (`ranting_core/src/grammar.rs:130`), the ultimate
     source of an entity's number — `Noun` has **no** number field, it derives
     from the `SubjectPronoun`; 24 delegations in `src/collections.rs`; the
     `plural_you` derive attribute; and generated code —
     `ranting_derive/src/lib.rs:781` bakes `#expr.is_plural()` into every
     possessive call site and `ranting_impl.rs:118-215` emits
     `is_plural`/`inflect` in two branches, so changing those two return types
     is a **lockstep `ranting`↔`ranting_derive` version bump**, not a trait
     edit. `PlaceholderSpec.plurality` is already a `&'static str`, but
     `ph_ext::match_nr` accepts only `[+-]|(#|\??\$)\w+`, so a dual marker
     would be a compile-time *grammar* change.
   - **The discriminating finding**: the number is **gone by runtime**. `nr`
     reaches `handle_placeholder` as an already-formatted `String` (`#var` is
     already English *words*), the bool is recovered by string-sniffing
     (`nr.trim_start() != "one"`, `s != "1"`), and that string is never passed
     to any hook. So Arabic dual, Slavic paucal and CLDR
     `zero/one/two/few/many/other` are unreachable **with no side-table
     workaround** — unlike item 2's gender gap — and a literal
     `plural_category()` cannot be computed from what hooks receive today by
     the fork *or* the crate. That is (b)'s real cost, and it is most of (c)'s
     plumbing.
   - (a) keep the bool, categories out of scope — **rejected**: it contradicts
     this phase's own v1.3 success criterion 1, which names "number" among the
     signals obtainable without an external side table. If ever taken, that
     criterion must be reworded in the same change; shipping (a) while leaving
     it standing is the one outcome the spec rules out.
   - (c) `Number` enum replacing the bool — **rejected on cost-to-increment**:
     breaks four kinds of surface at once (trait signatures, four public free
     functions, generated code, 24 wrapper delegations) for every user
     including English-only ones; **still** needs (b)'s numeral threading to be
     usable; needs a grammar change before any non-boolean variant is
     constructible (a `Dual` nothing can produce is worse than a bool); and
     forces `src/language/english.rs` to interpret non-English categories —
     item 3's objection to extending `SubjectPronoun`, repeated.
   - **Two amendments to (b) as scoped**, both forced by the code: a *count*,
     not a category (mapping number→category is a per-language, versioned CLDR
     function; `ranting` carries the signal, never the interpretation — item
     2's `NounClass` precedent), and a hook *parameter*, not an entity method
     (`Many`/`Maybe`/`Box` delegate hooks where `self` is the *inner* value —
     item 2's exact wrapper argument). Also rejected: passing the rendered
     `nr: &str` (the fork would parse back a string its own item-8 numeral hook
     wrote — and note item 8, as landed, does exactly that for `$var`'s own
     count, which is affordable only because that hook *is* the numeral), a `{2noun}` dual marker (grammar surface for every English user to
     serve one construction — item 1's point-fix objection), and deferring past
     item 5 (costs the phase a second signature break).
   - **Prerequisite recorded for item 8, regardless of option** — *since closed
     by item 8 itself, see its notes*: `src/lib.rs`'s `#var` arm compared
     against the literal English word `"one"`, so the moment a fork's numeral
     hook spelled `#var` in its own language the placeholder silently took
     plural agreement for a count of one. Item 8 made `#var` bake a count and
     that arm test `count != Some(1)`, evaluated before the numeral hook runs.
     Note this does *not* discharge the count channel (b) recommends: item 8's
     count reaches the numeral hook only.
   - **Stays impossible under the recommendation**: categorial number with no
     numeral in the placeholder (bare `{+noun}` dual), numeral-governed case
     (Russian *два дома* — `GrammaticalCase` comes from the placeholder's
     `CaseKind`, never from a numeral), Welsh/Irish number-triggered mutation
     (needs item 7 as well), and number under `conjugate_auxiliary`
     (`src/language/auxiliary.rs:15`), which takes no number at all and sits
     behind no hook.
   - Follow-up, non-blocking: the exact count payload (bare `i64` vs. a struct
     carrying visible fraction digits, so the existing `1.0`-is-not-`one`
     behavior at `src/lib.rs:379` stays expressible); whether the count goes on
     all six hooks or only article/pronoun/adjective (current reading: all six —
     unlike `NounClass`, number *is* a verb-agreement axis); and whether `Many`
     should report its `len()` as the count when a placeholder has no numeral,
     the only path to categorial number without a grammar change.
   - ⚠️ **Item 10 follow-up**: the German lexicon adds a second signal owed at
     the same signature-break site — `Ranting::inflect` takes `to_plural` but no
     case, so German's own noun declension (`den Hunden`, `des Hauses`) has to
     be carried on the entity (`ranting_i18n/README.md` hole 2). Whatever
     breaks these signatures for the count should settle case on `inflect` too.

5. ✅ **Adjective-agreement runtime hook** (10-14 hours)
   - Degree (`!`/`!!`, Phase 3 item 6) is baked entirely at compile time in
     `ranting_derive/src/language/adjective.rs`; `ranting` has no runtime
     adjective path at all. Romance and Germanic adjectives agree with their
     noun in gender, number, and (German) case and definiteness — none of which
     is known at compile time.
   - Add `Ranting::inflect_adjective_custom`/`_with_context`, receiving the
     adjective, plus `GrammaticalCase`, number, and item 2's noun class; call
     it from `handle_placeholder_impl` before the compile-time-baked degree
     form is emitted, falling back to that baked form when the hook returns
     `None` — so `say!()`'s English output is unchanged.
   - Worked test: French `un chat noir` / `une robe noire` / `des chats noirs`
     from one template.
   - **The number type for this hook is decided by item 4's spike**
     (`docs/superpowers/specs/2026-08-13-number-categories.md`): `as_plural:
     bool` *plus* a count parameter — and that count parameter should be added
     to the other six `_custom` hooks in this same change, so Phase 6 breaks
     hook signatures once rather than twice. Read that spec before fixing this
     hook's signature.
   - ✅ **COMPLETE 2026-08-13** — `Ranting::inflect_adjective_custom`/
     `_with_context`, taking the adjective *as written* plus `AdjectiveDegree`,
     `GrammaticalCase`, `NounClass`, `as_plural` and `uc` (and `ctx` on the
     `_with_context` twin), called from `handle_placeholder_impl`'s
     `PostSpec::Degree` arm ahead of the compile-time-baked degree form. 301
     integration tests + 14 doctests + 39 unit tests pass under both
     `cargo test` and `cargo test --all-features` (293 + 14 + 39 before; 8 new
     tests in `tests/ranting/adjective_agreement.rs`), every pre-existing
     assertion byte-identical — including all of
     `tests/ranting/comparative_adjectives.rs`, which is the English-unchanged
     canary. `cargo clippy --all-targets --all-features -D warnings` is clean,
     as is `ranting_derive`'s standalone clippy.
   - **The base adjective had to be baked, and wasn't before.**
     `PostSpec::Degree` carried only `word`, the resolved English form
     (`!good` → `"better"`, `!noir` → `"noirer"`), which is not reversible back
     into what the template wrote. Recovering `"noir"` from `"noirer"` would be
     exactly the string-sniffing item 4's spike named as the bug rather than
     the fix, so `PostSpec::Degree` gained `base: &'static str` and
     `degree: DegreeKind` alongside `word`. Precedent cited in the type's own
     docs: `say_with!()` already bakes the *uninflected base verb* into
     `PostSpec::Tense` for the same reason. `PostSpec` is `ranting_core`, i.e.
     not public semver surface, so this cost nothing outside the repo.
   - **Follows item 1's reference pattern, including the mirror `NounClass`
     skipped**: `AdjectiveDegree` (public, in `ranting`) mirrors
     `ranting_core::placeholder::DegreeKind` via `From`, because unlike a noun
     class the degree marker *is* written in the placeholder and so does exist
     at the macro↔runtime seam. Parameter on the `_custom` hook and its
     `_with_context` twin, threaded from `handle_placeholder_impl`, delegated
     by `Many`/`Maybe`/`Box` (`src/collections.rs`) on the same
     exactly-one-item rule as every other hook, worked example in a real
     language under `tests/ranting/`. `_with_context` carries
     `#[allow(clippy::too_many_arguments)]`, matching item 2's flat-args
     choice for public hooks.
   - **Additivity**: the hook defaults to `None` and English never consults it,
     so `say!()` output is unchanged by construction — the fallback path still
     emits `word` with the same uppercase-first-character pass it always did.
     `uc` is passed *in* to a custom form (which applies `uc_1st_if` itself),
     mirroring the article and pronoun hooks. Generated code is untouched
     except for the two extra baked fields.
   - **Known limitation, recorded rather than fixed: `!` is the only adjective
     slot.** The grammar has no positive-degree marker — an unmarked post-noun
     word is parsed as a *verb* (and `PostSpec::Verb` is deliberately not
     routed to this hook, since a real verb through an adjective hook would
     break English), and an adjective outside the placeholder is literal text
     no hook can reach. So the French worked example writes `{a 0 !noir}` for a
     plain agreeing adjective and ignores `degree`; there is no
     `AdjectiveDegree::Positive` because no marker could produce one. Adding a
     positive-degree marker would add grammar surface for every English user to
     serve one construction — the shape of point fix item 1 rejected for the
     German second verb position. Also unchanged by this item, per item 1:
     agreement gives the right *form*, never movement, so Romance
     post-nominal/prenominal adjective placement stays with the caller's
     template.
   - **Deviation from this item's own text, deliberately: item 4's count
     parameter did *not* land here.** The hook takes `as_plural: bool` alone,
     and the other six `_custom` hooks are untouched. Reasons: the count is not
     recoverable at the call site (`nr` is already a formatted `String` — item
     4's spike says so explicitly), so threading it requires baking the numeric
     value through `handle_placeholder`, a `#[doc(hidden)] pub` function
     *generated code names* — a lockstep macro↔runtime change, plus a new
     public count type, plus ~24 wrapper delegations and every hook-overriding
     test file. That is a larger change than item 5 itself and independent of
     adjective agreement. **Consequence, stated plainly: Phase 6 will pay a
     second hook-signature break** when the count lands (as its own item, or
     folded into item 6/7/8 — item 8 needs it either way, see that item's
     prerequisite note). The v1.3 success criterion naming "number" is
     deliberately *not* reworded: nothing here narrows it, and the count
     channel item 4 recommends remains the way to satisfy it.
   - ⚠️ **Item 10 follow-up**: the German lexicon produces the full weak/mixed/
     strong ending table through this hook, and then cannot use any of it.
     German attributive adjectives are prenominal while the `!` slot is
     post-noun only, and German predicative adjectives are uninflected — so
     **no German sentence renders this hook's output correctly**
     (`ranting_i18n/README.md` hole 4a; the position half is item 1's permanent
     boundary, not a gap here). Separately, the hook is never told which article
     was rendered, and `self` cannot know, so weak-vs-mixed
     (`der kleine Hund` / `ein kleiner Hund`) is unreachable and must be carried
     on the entity (hole 4b) — a third candidate for item 4's signature break.

6. ✅ **Orthography & capitalization hook** (8-12 hours)
   - `uc_1st_if`, the sentence-start-uppercase default, the `,`/`^` markers, and
     `apply_case`'s all-caps/title-case/lowercase preservation are English
     orthographic assumptions compiled into the crate. German capitalizes every
     noun regardless of position; Japanese, Chinese, Arabic and Hebrew have no
     case at all, so `uc: bool` is meaningless; Turkish has dotted/dotless `i`
     that `char::to_uppercase` gets wrong for a Turkish locale.
   - Route capitalization through a `Ranting` hook (defaulting to today's
     behavior) rather than calling `uc_1st_if` directly at each site, so a
     language module can make it a no-op or an always-capitalize.
   - Explicitly check the `uc` plumbing through `Many`/`Maybe`/`Box`
     (`src/collections.rs`) still behaves — the "uppercase first char only"
     join is tested there already.
   - **Implementation notes (2026-08-13).** `Ranting::capitalize(&self, word,
     role: OrthographyRole, uc) -> String` plus a `capitalize_with_context`
     pair, defaulting to `uc_1st_if(word, uc)`. Every fallback path in
     `handle_placeholder_impl`/`get_article_or_so` that called `uc_1st_if`
     directly — the `the` and a/an/demonstrative article arms, the pre-noun
     possessive substitution, `conjugate_verb`'s English fallback, the five
     pronoun-case arms, the noun name, and the two inline uppercase-first-char
     blocks in the `PostSpec::Tense`/`Degree` arms — now calls it instead. All
     301 pre-existing tests and every doctest pass unchanged; `cargo clippy
     --all-targets -D warnings` is clean.
   - **Returns `String`, not `Option<String>`, and is not named `_custom`.** In
     this crate `_custom` means "`None` declines and English takes over"; this
     hook *is* what takes over, so an `Option` would have no meaning. The
     `inflect_*_custom` hooks keep receiving `uc` untouched and keep applying it
     themselves — the fallback-path-only contract item 5 already documented on
     `inflect_adjective_custom` is what this hook slots into, not a change to it.
   - **`OrthographyRole` is defined in `ranting` alone, not mirrored from
     `ranting_core`** — the `NounClass` rule, not the `GrammaticalCase` rule. A
     case marker is written in placeholder syntax, so `CaseKind` exists at the
     macro↔runtime seam to mirror; a call-site role is never written anywhere,
     it is a property of where the renderer is in assembling output.
   - **The one asymmetry, and the bug it avoids: `OrthographyRole::Noun` is
     passed `uc: false`.** Four roles get an uncapitalized word and a truthful
     `uc`; the noun name cannot, because it has already been through
     `inflect()`, which takes `uc` itself and is user-implementable. Crucially
     that is *not* equivalent to `uc_1st_if`: a derive-generated `name()` with
     `opt.uc == false` implements `uc == true` as **"as written"**, preserving
     the first character rather than uppercasing it, so
     `#[ranting(name = "designer")]` renders `"designer arrived."` even
     sentence-initially. Both routing `uc` through the hook and the
     apply-it-twice variant were tried and *both* change that output (verified
     empirically against the pre-change build, not reasoned about) — the
     idempotence that makes double-application safe elsewhere does not hold
     here. Passing `uc: false` at that one site makes the output byte-identical
     by construction while leaving an always-capitalize fork (which ignores
     `uc`) fully functional. A fork needing position-sensitive noun casing
     overrides `name`/`inflect` instead. Note the corollary: the four
     `ranting::uc_1st_if` calls `ranting_derive/src/ranting_impl.rs` emits
     inside the generated `name()`/`inflect()` are deliberately left alone —
     they sit *below* the hook, which is the same fact the `uc: false` above
     encodes. "Every site is routed" is true of `handle_placeholder_impl` and
     `get_article_or_so`, not of derive-generated code. `tests/ranting/orthography.rs::
     lowercase_name_attribute_still_renders_lowercase` is the regression guard.
     The pre-noun possessive-substitution site (`` {`jane cat} `` → `"Her
     cat"`) is `OrthographyRole::Noun` too and is pre-capitalized the same way.
   - **Deliberately *not* rerouted, per this item's own opening paragraph.**
     (a) `apply_case` in `src/language/plurals.rs` stays as it is: it is case
     *preservation* of a looked-up irregular plural's own spelling, not
     sentence-position capitalization, and it is reached through the `self`-less
     free function `inflect_noun_irregular`, which has no entity to ask. (b) The
     `,`/`^` markers and the sentence-start default stay compile-time: they
     decide the *value* of `uc`, while this hook decides what is *done* with it.
     Moving either would mean re-parsing template text at runtime.
   - **Wrappers.** `Many` delegates both hook forms only at `len() == 1` and
     otherwise keeps the English default — the same rule as `noun_class()`, for
     the same reason (a multi-item phrase is one joined string whose members may
     disagree). `Maybe(Some(x))` delegates to `x`, `Maybe(None)` and `Box<T>`
     behave as everywhere else. The existing uppercase-first-char-only join
     tests in `src/collections.rs` are unchanged and still pass;
     `tests/ranting/orthography.rs` adds explicit assertions for all three
     wrappers plus the composed `Many<Box<T>>`.
   - **Tests** (`tests/ranting/orthography.rs`, 10 tests): a German
     always-capitalize-nouns override asserted *mid-sentence* (where `uc` is
     false, so only the hook can produce the capital) against a hook-less twin
     with identical data; a caseless-script no-op; a role-recording probe
     pinning which role each site reports and what `uc` it gets; a Turkish
     `capitalize_with_context` keyed on `NarrationContext::dialect`; the
     lowercase-`name` regression guard; and a byte-identical-English guard.

7. ✅ **Phonological elision / contraction hook** (6-10 hours)
   - The `a`/`an` choice is hard-coded English phonology. French `le`+vowel →
     `l'`, `de`+`le` → `du`; Italian `lo`/`il`/`l'`; Portuguese preposition-
     article fusion. None is expressible today: the article hook returns a
     string and never sees what follows it.
   - Give the article hook (or a new post-assembly hook) the *following* word,
     so a fork can elide or fuse. Keep English `a`/`an` on the existing path
     unchanged. Worked test: `l'homme` vs `le chien`.
   - **✅ COMPLETE 2026-08-13** — `Ranting::elide_article_custom`/
     `_with_context`, taking the rendered `article`, the `separator` between,
     and the rendered `following` text, plus `case`/`class`/`as_plural`, and
     returning `Option<String>`: `Some` replaces all three with one fused
     string, `None` (the default) keeps them exactly as rendered. All 311
     pre-existing tests, 14 new ones and every doctest pass; `cargo clippy
     --all-targets` is clean and `cargo fmt` applied.
   - **Chosen: a new post-assembly hook. Rejected: a `following: &str`
     parameter on `inflect_article_custom`.** The rejection is structural, not
     stylistic: at `get_article_or_so` time the following text *does not exist
     yet*. `nr` and the noun are pushed into `res` afterwards (`src/lib.rs`
     lines ~493 and ~512), and the noun's form depends on `inflect()` or
     pronoun-case selection, so passing it would mean rendering the noun twice
     or inverting `handle_placeholder_impl` to assemble right-to-left. It would
     also break the article hook's signature a *seventh* time while item 4's
     count-channel break is still owed. The post-assembly shape additionally
     lets a fork drop the separator (`l'homme`) — a `following` parameter could
     not, since the separator is emitted by a different call site.
   - **Mechanism.** `handle_placeholder_impl` records the byte span of the last
     article it pushed into `res` (both call sites: the leading article and the
     chained one after a pre-noun verb — last one wins, since that is the one
     adjacent to the noun), then splices just after the noun/pronoun is pushed.
     `get_article_or_so` is deliberately left with its signature unchanged,
     which is what makes the byte-identical-English claim easy to defend. The
     separator is collected from *both* sides of the boundary rather than
     assumed: `space` is normally empty and the whitespace actually comes from
     `noun_space`, pushed later.
   - **No `uc` parameter**, unlike the other seven hooks. By the time this runs
     the article is already rendered *and* capitalized — by
     `inflect_article_custom` or by `capitalize` on the fallback path — so `uc`
     would have nothing left to decide, and `uc` itself has already been reset
     to `false` at the splice point. A fork re-casing its fused form inspects
     the first character or calls `capitalize` (item 6) itself.
   - **`de` + `le` → `du` did *not* land, and cannot on this design.** The
     preposition lives in the template's *literal* text, outside the
     placeholder, and `` {de le chien} `` parses `de` as a pre-noun verb
     (`ArticleKind::Other` → `None` → conjugated). Second rejected alternative:
     a post-pass over the whole assembled `say!()` output, which would re-scan
     rendered text and couple each placeholder to its neighbours — a much
     larger blast radius than this item, for one Romance contraction class.
     Recorded here rather than papered over; article↔following-word elision
     (`l'homme`, Italian `lo`/`il`/`l'`) is what the item delivers. Related
     boundary: a hidden noun (`` {?the noun} ``) renders nothing to elide
     against, so the splice sits inside the `case != Hidden` block and the hook
     is not called there — `elision.rs::hidden_noun_does_not_reach_the_hook`
     pins it.
   - **Wrappers.** `Many` delegates only at `len() == 1` (for 2+ items
     `following` is the joined phrase, whose members may elide differently),
     `Maybe(Some(x))` delegates to `x`, `Box<T>` forwards — the same rule as
     `noun_class()` and `capitalize()`.
   - **Tests** (`tests/ranting/elision.rs`, 14 tests): the `l'homme` vs
     `le chien` worked example; both genders (`l'école`/`la voiture`); aspirate
     h declining per-noun (`le héros`), which also shows elision is a *lexical*
     property carried by the entity, not derivable from spelling; plural `les`;
     sentence-initial `L'homme`; Italian `lo`/`il`/`l'` from one hook body; a
     recording probe pinning what `separator` and `following` contain, including
     `"2 chiens"` when a number renders between and `"set of 2 chiens"` for
     `` {a set of $n p} `` — `following` is whatever is *adjacent*, so the
     placeholder's own pre-text words after the article are part of it; the
     hidden-noun boundary (`` {the ?p} ``, the noun-position `?`, not the
     article-position `?` of `` {?the p} ``) and the
     chained-article-after-a-verb path; a byte-identical-English guard for
     `a`/`an`/`the`/`these`/`those`; and all three wrappers plus `Many<Box<T>>`.
   - ⚠️ **Item 10 follow-up**: German has no use for this hook at all — every
     German fusion is preposition+article (`in dem` → `im`, `zu dem` → `zum`,
     `an das` → `ans`), i.e. exactly the case this item already recorded as
     unreachable. The German lexicon sharpens that record twice. (1) The obvious
     escape — writing the preposition inside the placeholder so the hook can see
     it — does not exist: the pre-noun slot accepts an article or a hard-coded
     English modal word and nothing else, so `say!("{in the =haus}")` is a
     *compile* error (`ranting_i18n/README.md` hole 7). (2) The splice is
     skipped when the article renders empty, so the stray separator left by
     German's absent indefinite plural article cannot be spliced away either
     (hole 6).

8. ✅ **Locale-aware numeral rendering** (6-10 hours)
   - `#var` spells a number out in English words. Every other language needs its
     own, and several have gender/case agreement on the numeral itself
     (Russian `два`/`две`), plus non-ASCII digit systems for `$var`.
   - Add a numeral hook on `Ranting` (or a small `Numeral` trait a language
     module implements), defaulting to the current English speller, threaded
     wherever `#var`/`$var` is rendered.
   - **Prerequisite, found by item 4's spike**: `src/lib.rs:376` decides
     plurality for `#var` by string-sniffing the rendered numeral against the
     literal English word `"one"`. Overriding `#var` rendering therefore
     silently flips number agreement for a count of one (`"eins"` != `"one"` →
     plural article, verb and pronoun). Either take the count from item 4's
     recommended count channel or document the break explicitly. See
     `docs/superpowers/specs/2026-08-13-number-categories.md`.
   - **Landed as `Ranting::inflect_numeral_custom`/`_with_context`** (the sixth
     and last `_custom` pair — verb, pronoun, article, adjective, elision,
     numeral; twelve methods with their `_with_context` twins), taking the English rendering, `count: Option<i64>`,
     `NumeralStyle` (`Words` for `#var`, `Digits` for `$var`), `GrammaticalCase`,
     `NounClass` and `as_plural`, returning `Option<String>`. Default `None`
     keeps `rant_convert_numbers` for `#var` and the argument's own `Display`
     for `$var`. English output verified byte-identical by running 18 numeral
     placeholder shapes against the pre-change sources and diffing.
   - **Chosen: a hook on `Ranting`. Rejected: a separate `Numeral` trait a
     language module implements.** The deciding argument is the same one item 2
     used for `NounClass` and item 5 for adjective agreement: the data the hook
     needs is carried *by the entity*. Russian `два`/`две` is gender agreement
     with the counted noun, so a `Numeral` implementor would have to be handed
     the noun's `noun_class()`, `is_plural()` and case anyway — i.e. exactly this
     signature, minus `self`, plus a second registration mechanism (a trait
     object or type parameter to select the language module, which nothing else
     in the crate has). It would also fragment `Many`/`Maybe`/`Box`: those
     delegate hooks by inner value, and a language module sitting outside the
     entity has no equivalent notion. A free-standing trait pays for itself only
     if numerals are noun-independent — they are not in the languages the item
     names.
   - **Implementation notes.**
     - `#var` moved from compile time to runtime. The macro used to bake
       `ranting::rant_convert_numbers(n as i64)` — a finished English word —
       into the `format!()` argument; it bakes the count instead now, and
       `handle_placeholder_impl` calls the same speller as the hook's fallback.
       Without this a fork could only post-process English words, which is not
       "its own speller". `handle_placeholder`/`_with_context` therefore take a
       `count: Option<i64>` parameter: a runtime value, so it cannot live in the
       `Copy` `PlaceholderSpec` alongside the rest.
     - `$var` was deliberately *not* moved. Its argument need only be `Display`
       (a float, a `{:>4}` width), so baking `as i64` for it would fail to
       compile code that works today. It is still rendered by the macro with its
       `:fmt` spec applied, and its count is recovered at runtime by parsing that
       string — `None` when it isn't a plain integer, which is honest and is what
       a digit-transcribing fork (the actual `$var` use case) doesn't need.
     - The number's leading space moved out of the baked string into the new
       `placeholder::NumeralSpec { kind, leading_space }`, so the hook is handed
       the numeral alone. Keeping it in the string was not viable: with `{$n:>5}`
       there is no way to tell template space from format padding. `NumeralSpec`
       being `Option` on the spec makes "space with no numeral" unrepresentable,
       and covers the hidden case (`` {?$n noun} ``) as absence — nothing renders,
       so the hook is not called, matching item 7's hidden-noun rule.
     - `NumeralKind` is mirrored into the public `NumeralStyle` via `From`,
       following `CaseKind`→`GrammaticalCase` and `DegreeKind`→`AdjectiveDegree`
       rather than item 2's `NounClass`: the `#`/`$` marker is written in the
       placeholder, so there is something at the macro↔runtime seam to mirror.
     - The prerequisite above is **closed, not documented around**: the `#var`
       arm of the `as_pl` match tests `count != Some(1)` now. Equivalent for
       English (`rant_convert_numbers` spells only 1 as exactly `"one"`; `-1` is
       `"negativeone"`), and it puts the decision *before* the hook, so a custom
       numeral cannot reach it. `$var`'s arm was left byte-for-byte alone on
       purpose — its `s != "1" && s.split('.').next() != Some("1")` also catches
       `"1.0"`, which a parsed `i64` would not.
     - No `uc` parameter, as for item 7's elision hook: the crate never
       capitalizes a numeral (a sentence-initial placeholder spends `uc` on the
       article, verb or noun), so there would be nothing to decide. A returned
       string replaces the rendering outright, so a `$var` width/fill spec is not
       re-applied to it — documented on the hook.
     - Scope kept narrow, twice. The count is local to the numeral and does
       **not** discharge item 4's owed count channel on the other five pairs.
       And `heed!()`/`ask!()`'s `{$name}` is the inverse direction (input
       parsing, a deliberately smaller grammar) and is not routed here.
     - Wrappers follow the established rule: `Box` straight through, `Many` only
       at `len() == 1`, `Maybe(Some)` through and `Maybe(None)` unreachable.
     - `tests/ranting/numeral.rs` has the worked Russian example (`два стола` vs
       `две книги` from one template, gender off the entity), a Devanagari-digit
       `$var` override, the count-of-one agreement guard, the hidden and
       no-numeral non-call cases, and the byte-identical-English guards.
   - ⚠️ **Item 10 follow-up, partially closed by item 12**: the hook itself
     came through the German lexicon working (German numerals spelled here,
     `1` agreeing like an article, out-of-range counts falling back to
     English). The locale channel this item's docs point at —
     `NarrationContext::dialect` on `inflect_numeral_custom_with_context`, for
     a script's own digits — was unreachable from a companion crate because
     `say_with!()` wasn't re-exported from `ranting`; item 12 fixed the
     re-export (`ranting_i18n/README.md` hole 1, now closed), so the channel is
     reachable. No fork has yet implemented a script's own digit system through
     it — that part remains future work, just no longer blocked.

9. ✅ **Non-space-delimited script support in `heed!()`/`ask!()`** (10-14 hours)
   - `{name}` is documented as capturing "one whitespace-delimited token" and the
     compiled regex is built on that assumption — which returns nothing useful
     for Japanese, Chinese or Thai input, where words are not space-separated.
   - Decide and implement: either a documented, permanent restriction to
     space-delimited scripts (cheap, honest, and consistent with the
     input-parsing spec's precedent of declining unbuildable generality), or a
     pluggable tokenizer boundary in `ranting_derive/src/heed.rs`'s compiler.
     Whichever is chosen, README.md's `heed!()` section and CLAUDE.md's capture-
     syntax bullet must state it explicitly rather than leaving it implied.
   - **Chosen: the documented, permanent restriction. Rejected: a pluggable
     tokenizer boundary in `compile_heed_template`.** No code changed in
     `ranting_derive/src/heed.rs` or `src/heed.rs` beyond comments recording the
     decision at the `\s+` join and on the `heed!()` macro doc; the deliverable is
     the stated contract plus `tests/ranting/script_segmentation.rs` (19 tests)
     pinning it, and the README/CLAUDE.md statements the item required.
   - **The restriction, stated precisely.** `build_heed_pattern` joins every pair
     of adjacent segments with a mandatory `\s+`, and the capture patterns are
     `\S+` / `\d+` / `.+?`. So the rule is *whitespace is the only word boundary
     the macros know*: every literal↔capture boundary in a template must be
     whitespace in the input. The one carve-out is a punctuation-only literal,
     which is exempt from its leading `\s+` — and that too is script-agnostic
     (`` {item}、 取る `` matches `"剣、 取る"` with U+3001). It is *per-segment*,
     though: `` {item}、取る `` is a single whitespace-delimited template token
     containing word characters, so it is an ordinary literal and returns `None`.
     Both cases are tested, because "every boundary needs a space" is an
     overstatement the README would otherwise have made.
   - **It covers `#[derive(Heed)]` too, which the item's title doesn't name.**
     `heed_derive::derive_heed` calls the same `compile_heed_template`, so the
     restriction is inherited verbatim rather than merely analogous; documenting
     only `heed!()`/`ask!()` would have left a first-class v1.3 surface silent on
     it. Covered in the CLAUDE.md `#[derive(Heed)]` bullet and by two tests.
   - **It is not an ASCII or Latin-script restriction, and the docs say so in
     those terms.** The compiled regex is script-agnostic, which the probe run
     confirmed before any doc was written: `heed!("取る {item}", "取る 剣")` →
     `Some("剣")`, `heed!("拿 {item}", "拿 剑")` → `Some("剑")`,
     `heed!("เอา {item}", "เอา ดาบ")` → `Some("ดาบ")`, and `{$n}` parses `"3 個"`
     fine. Writing this up as "Japanese/Chinese/Thai unsupported" would have been
     false; the honest boundary is the *separator*, since that is what the regex
     actually enforces. Documenting the wrong axis was the main risk here.
   - **The failure mode is `None`, verified, never a wrong capture.**
     `heed!("{item}を取る", "剣を取る")`, `heed!("剣を{action...}", "剣を取る")`,
     `heed!("{$n}個", "3個")`, `heed!("เอา{item}", "เอาดาบ")` and
     `heed!("{a}的{b}", "我的剑")` all return `None`. This is what makes "cheap and
     honest" accurate rather than a euphemism, and it was checked before the
     decision was written down precisely because a silently-wrong capture would
     have broken the framing.
   - **Escape hatch, documented rather than invented.** An unsegmented clause is
     exactly one `\S+` token, so `heed!("{clause}", "剣を取る")` and
     `heed!("命令 {rest...}", "命令 剣を取る")` hand the whole run back for the
     caller to segment with a real tokenizer. No new syntax was needed for this —
     it already falls out of `\S+`, and pointing at it costs nothing.
   - **Why the pluggable boundary loses.** Three arguments, in order of weight.
     - *It cannot be plugged in from where the user is.* `compile_heed_template`
       runs inside the proc macro at compile time, so a runtime-registered
       tokenizer can never reach it, and a compile-time one means a
       proc-macro-visible trait or attribute selecting a segmenter — a second
       registration mechanism that nothing else in the crate has. Contrast Phase
       6 items 2/5/8, where every hook could hang off `Ranting` because the data
       was carried by the entity at runtime; `heed!()` has no entity and no
       runtime seam at template-compile time.
     - *A boundary alone doesn't segment anything.* Making the `\s+` joins
       optional (`\s*`) is the cheap version, and it is strictly worse than
       today: `{a}的{b}` against `我的剑` would then find *a* split by
       backtracking rather than the intended one, converting a clear `None` into
       a silently wrong capture. That is the exact ambiguity
       `docs/superpowers/specs/2026-08-12-input-parsing-feasibility.md` declined
       for multi-word captures, and the same reasoning that makes two zero-gap
       captures a compile error today. Real segmentation needs dictionary- or
       model-based tokenization (MeCab/Jieba class), which is a dependency and a
       data file, not a boundary.
     - *Precedent.* The feasibility spec's whole method is declining unbuildable
       generality and writing down what was declined — feature B (`unsay!()`) and
       `#name` reverse-parsing were both cut that way. This is the same shape.
   - **Scope note.** This does not restrict `say!()` output, only `heed!()`/
     `ask!()` input; a fork can render any script it likes through the Phase 6
     hooks and still be unable to parse that script back with `heed!()`. That
     asymmetry is intended — inflection is the crate's job, tokenization isn't.

10. ✅ **`ranting-i18n` companion crate — German reference lexicon** (16-24 hours)
    — *the acceptance test for items 1-9*
    - A separate crate (not a `ranting` module, not a `ranting` feature flag),
      depending only on `ranting`'s **public** API, implementing German for a
      deliberately small closed vocabulary: three nouns of different gender,
      nominative/accusative/dative, definite and indefinite articles, adjective
      agreement, and verb agreement.
    - Its purpose is falsification: any place it needs `ranting_core`, a
      `pub(crate)` item, or a fork of `handle_placeholder_impl` is a hole items
      1-9 failed to close, and gets recorded as such. If the honest outcome is
      "German still needs per-language templates for word order," that must be
      written down in the crate's README and in item 1's spec, not papered over.
    - Also the proof that `docs/EXTENSIBILITY.md`'s pirate/Scottish/Spanish
      examples generalize past lexicon-level substitution to genuine
      morphological difference.
    - ✅ **COMPLETE 2026-08-13** — `ranting_i18n/` (crate name `ranting-i18n`),
      its own directory with its own `Cargo.toml`/`Cargo.lock`, exactly as
      `ranting_core`/`ranting_derive` are: this repo is not a cargo workspace.
      Its only dependency is `ranting = { path = ".." }` — no `ranting_core`, no
      `ranting_derive`, no `pub(crate)` item, no fork of
      `handle_placeholder_impl`. 31 tests (20 in `tests/german.rs`, 10 in
      `tests/holes.rs`, 1 doctest); `cargo fmt --check`, `cargo clippy -D
      warnings` and `cargo test` green there and in the repo root (root suite
      unchanged at 409 passing, confirming the "zero behavioral change" success
      criterion from the other side — the lexicon is additive and touched no
      crate source).
    - ✅ **What items 1-9 delivered, confirmed working**: `der`/`die`/`das`/
      `den`/`dem`/`des` and `ein`/`eine`/`einen`/`einem`/`einer`/`eines` from
      one code path (item 2's `NounClass` read off the entity, never off the
      display string); present tense across all six persons including `sein`
      and stem-changing `sehen`/`schlafen`; the complete weak/mixed/strong
      attributive-adjective ending table (item 5); German numerals with `1`
      agreeing like an article (item 8); noun capitalization (item 6);
      `er`/`ihn`/`ihm`/`sich`/`mich`/`euch`. This discharges the item's third
      bullet: it is genuine morphological difference, not the lexicon-level
      substitution `docs/EXTENSIBILITY.md`'s pirate/Scottish examples show.
    - ✅ **Word order — the honest outcome, as item 10 required in writing**:
      German still needs per-language templates. Verb-second is reachable only
      because the caller writes a German template string; a verb split across
      two positions (`Der Hund macht die Tür auf`) is not expressible at all.
      Restated in `ranting_i18n/README.md` §"Word order"; item 1's spec already
      recorded it under "Recorded for item 10", so nothing there needed
      changing. Pinned by `hole_8_*` in `ranting_i18n/tests/holes.rs`.
    - ⚠️ **Seven holes found and recorded, not worked around.** Each is
      numbered in `ranting_i18n/README.md`, pinned by a `hole_N_*` test that
      asserts what the crate *actually* renders, and cross-referenced below to
      the item it belongs to. Where the lexicon carries state on the entity
      instead (case, article definiteness, name-vs-pronoun rendering), the
      README says so and names the hook that should have carried it.
      1. **`say_with!()` and `#[derive_ranting]` are not re-exported from
         `ranting`** — only `say`/`ack`/`nay`/`heed`/`Heed`/`ask`/
         `boxed_ranting_trait`/`ref_ranting_trait` are. So a companion crate on
         the public API alone can never deliver a `NarrationContext`, and all
         twelve `_with_context` hooks are unreachable in practice even though
         they are public: `say!()` always passes `None`. Costs German the
         `dialect`-selected digit system item 8's own docs advertise, plus
         register and runtime tense. → Phase 3 items 3 & 4, Phase 6 item 8.
      2. **`Ranting::inflect` takes number but not case** — German declines the
         noun itself (`den Hunden`, `des Hauses`), so the form must come off the
         entity. Same signature-break site as the count channel still owed. →
         Phase 6 item 4.
      3. **`GrammaticalCase` has no dative, so a fork ends up ignoring it** —
         the sharper form of the known "English doesn't distinguish accusative
         from dative" note: once the entity carries the case (the only route to
         dative), `{the =noun}` and `{the @noun}` produce *identical* output and
         the `case` parameter becomes dead. Five English markers collapse onto
         German's four cases, `@` meaning accusative-or-dative. Rejected
         workaround: smuggling case through `NarrationContext.dialect` — per-
         placeholder information in story-wide state; reachable since item 12
         closed hole 1, but still the wrong shape for per-placeholder data.
         → Phase 3 item 2's v1.3 `GrammaticalCase` bullet, Phase 6 item 2.
      4. **Attributive adjectives — wrong position, and declension class is not
         reported.** (4a) The `!` slot is post-noun only, so `{the =hund
         !klein}` gives "Der Hund kleine"; since German predicative adjectives
         are uninflected, **there is no German sentence in which this hook's
         output is correct German** — it proves the agreement mechanism, and
         German cannot use it without putting the adjective in literal template
         text. Rejected workaround: adjective-in-pre-noun-slot via
         `inflect_verb_custom`, which fails twice (hole 7, plus the "verb before
         and after?" assert). (4b) Weak vs. mixed vs. strong depends on which
         article was rendered; the hook is not told and `self` cannot know, so
         `with_article` carries it. → Phase 6 item 5 (position half owned by
         item 1).
      5. **One hook serves both case marking and pronoun display** — to render
         "Der Hund bellt" a fork must make `inflect_pronoun_custom` return the
         name (as `tests/ranting/grammatical_case.rs` itself does), after which
         real pronouns are unreachable *for that entity*: `{@hund}` gives "Hund",
         not "ihn". → Phase 3 item 2, Phase 6 item 2.
      6. **An article that renders as nothing still emits its separator** —
         German has no indefinite plural article, `inflect_article_custom` can
         only say so by returning `""`, and the result is a stray leading space.
         `elide_article_custom` cannot repair it: the post-assembly splice is
         skipped for a zero-length article span, so the hook is never called
         (proved with a probe that would fire if it were). `skip_article` is
         per-entity and unconditional, so it is not the answer either. → Phase 6
         item 7, item 2.
      7. **The pre-noun slot is a closed English word list** — an article or one
         of the hard-coded modal words (`can`/`may`/`shall`/`will`/`are`/`were`/
         `had`/`have`/…); `say!("{in the =haus}")` is a *compile* error. This
         sharpens item 7's already-recorded "`de` + `le` → `du` is not
         reachable": the obvious escape of writing the preposition inside the
         placeholder does not exist, so no hook ever sees a German preposition
         and every German fusion (`im`, `zum`, `ans`) is out of reach. → Phase 6
         item 7, item 1.
    - ✅ **Also observed** (not holes): a partial lexicon degrades honestly —
      unknown verbs, adjectives and out-of-range numerals fall back to English
      rather than being invented. But an unrecognized `subjective()` degrades
      *silently*: `"er"` is not an English pronoun, so a declined verb takes the
      catch-all arm of `english::inflect_verb` and renders the bare form
      ("Der Hund walk."). That is the cost the "`SubjectPronoun` is a closed
      English enum" decision already names, now confirmed and bounded — it is
      visible only for words the fork's own hook declined. Item 6's `capitalize`
      turns out to have nothing to do for German (nouns are capitalized by
      `name`/`inflect` already); its real customers remain Turkish and the
      caseless scripts, as its docs say.

### Phase 6 follow-ups — items 11-23 (queued 2026-08-13, post-run)

*Items 1-10 are complete. These come from two sources: the three design spikes'
own "open questions" sections, which authorized concrete follow-up work nobody
had picked up; and the seven holes item 10's German lexicon found. Ordered for
execution — the two outright bugs and the gate gap land first, the owed
signature break next, documentation after that, and the Spanish lexicon last
because it exercises everything before it.*

11. **Zero-length article still emits its separator** (4-8 hours) — *a bug, not
    a gap; found by item 10, hole 6*
    - German has no indefinite plural article, so `inflect_article_custom`
      returns `""` to mean "no article here" — and the separator is emitted
      anyway, rendering `" Hunde bellen."` with a leading/doubled space. This is
      user-visible wrong output reachable from the public API.
    - `elide_article_custom` (item 7) cannot repair it: the post-assembly splice
      is skipped when the recorded article span is empty, so the hook is never
      called for a zero-length article. `skip_article()` is the wrong tool — it
      is per-entity and unconditional, so it cannot mean "no article in the
      plural only" and would swallow `der`/`die`/`das` too.
    - Cheapest, highest-value item in this batch.

✅ 12. **Re-export `say_with!` and `derive_ranting` from `ranting`** (2-4 hours) —
    *found by item 10, hole 1*
    - `ranting` re-exports `say`, `ack`, `nay`, `heed`, `Heed`, `ask`,
      `boxed_ranting_trait` and `ref_ranting_trait` — but not `say_with` and not
      `derive_ranting`. A crate depending only on `ranting`'s public API
      therefore cannot construct a call carrying a `NarrationContext`, which
      makes all twelve `_with_context` hooks dead weight to a companion crate
      and puts `dialect`-selected locales out of reach.
    - Almost certainly an oversight rather than a decision — every other macro
      is re-exported, and `ask!` was itself found unexported and fixed in
      Phase 5 for the same reason.
    - ✅ Added `pub use ranting_derive::say_with;` and
      `pub use ranting_derive::derive_ranting;` to `src/lib.rs`, alongside the
      existing re-exports. Pure addition — no signature or behavior change, so
      `say!()`'s output stays byte-identical; confirmed by the full existing
      test suite passing unchanged.
    - ✅ `src/lib.rs`'s `say_with!` re-export carries its own doctest
      (`{=jordan <arrive}` under a `Tense::Past` `NarrationContext`), matching
      the doc-comment style already used for `say!`/`ack`/`nay`.
    - ✅ New `tests/ranting/reexports.rs` integration test, deliberately using
      only `use ranting::*;` — no `use ranting_derive::*;`, unlike every other
      file under `tests/ranting/` — to prove both macros resolve through
      `ranting` alone: one test calls `say_with!()` with a tense override, the
      other applies `#[derive_ranting]` to a local struct.
    - ✅ Verified from `ranting_i18n` (which depends on `ranting` alone, per
      item 10's falsification contract): `hole_1_*` in
      `ranting_i18n/tests/holes.rs` now additionally calls `say_with!()` with a
      `NarrationContext { dialect: Some("de-AT"), .. }` and asserts the dialect
      arrives at `inflect_article_custom_with_context` — kept under its
      original test name (findable from the README/ROADMAP cross-references)
      rather than renamed, with the pre-existing `say!()`-always-passes-`None`
      assertion left in place alongside it. `ranting_i18n/Cargo.toml` still has
      no `ranting_derive` dependency.
    - ✅ `ranting_i18n/README.md` hole 1 struck (marked closed, kept numbered
      for the cross-references) with a note on what closed it and what's still
      future work (nothing implements a `dialect`-selected digit system yet —
      that's item 8's follow-up, no longer blocked but not done either); its
      cross-reference from hole 3's rejected-workaround paragraph updated to
      say reachability is no longer the blocker, shape-mismatch still is. Item
      3/4's ⚠️ follow-up bullet above, and item 8's ⚠️ follow-up bullet, both
      updated to ✅/closed-with-caveat accordingly.
    - ✅ `docs/API.md`'s macro table already listed `say_with!`/`derive_ranting`
      under "Macros (from `ranting_derive`, re-exported by `ranting`)" — it
      described the intended end state rather than the pre-item-12 reality, so
      no edit was needed there; it's accurate now. `CLAUDE.md`'s "Non-obvious
      behaviors" bullet on this topic *did* need updating — it explicitly
      documented the gap as still-open (item 10's finding) — flipped to
      describe the closed state and the tests that pin it.
    - ✅ Side effect caught by `cargo clippy --all-targets -- -D warnings`:
      seven files under `tests/ranting/` (`readme_example.rs`, `noun_class.rs`,
      `argument_parsing.rs`, `adjective_agreement.rs`,
      `male_female_and_object.rs`, `irregular_plurals.rs`,
      `inclusive_language.rs`) wrote `use ranting_derive::*;` alongside
      `use ranting::*;` purely to reach `derive_ranting`/`say_with` — once
      `ranting` re-exports both, that glob import has nothing left to
      contribute and clippy flags it `unused_imports`. Removed the redundant
      line from each; files that name specific items instead of a glob
      (`orthography.rs`, `runtime_viewpoint.rs`, `runtime_tense.rs`,
      `narration_context_threading.rs`, `numeral.rs`) are unaffected — an
      explicit `use path::name;` isn't flagged as unused merely because the
      name is also reachable through a glob elsewhere.

✅ 13. **The gate must cover sibling crates** (4-6 hours)
    - This repo is not a workspace, so `cargo test` at the root never compiles
      `ranting_i18n`. Item 10's crate passed its own gate, but the overnight
      loop's gate could not have caught a broken lexicon — verified after the
      fact, not by the gate.
    - Make the root gate (and `scripts/overnight_loop.sh`'s) cover every sibling
      crate, so this holds for future language modules too.
    - ✅ `scripts/overnight_loop.sh` gained `gate_dirs()` (lists every directory
      containing a `Cargo.toml` — the repo root plus every immediate
      subdirectory, so `ranting_core`, `ranting_derive` and `ranting_i18n` are
      discovered rather than hardcoded — a future sibling crate needs no edit
      here) and `run_gate()`/`run_gate_in()`, which run `cargo fmt --check`,
      `cargo clippy -- -D warnings` and `cargo test` inside each of those
      directories via `cd`, stopping and reporting which directory failed on
      the first failure. Both the pre-flight check and the per-task gate now
      call `run_gate` instead of running the three cargo commands inline at
      the repo root only. Incremental task consumption, the per-repo
      `LOG_DIR`, and every other script behavior are unchanged.
    - ✅ Verified by deliberately appending a badly-formatted function to
      `ranting_i18n/src/lexicon.rs` and confirming
      `cargo fmt --manifest-path ranting_i18n/Cargo.toml --check` fails (it
      did, with the expected diff), then reverting via `git checkout --
      ranting_i18n/src/lexicon.rs` and confirming the check passes clean
      again. This exercises the same fmt/clippy/test triad `run_gate_in` runs
      per directory, just without needing to invoke the full script (this
      task's own `claude -p` invocation is restricted to `Bash(cargo *)`/
      `Bash(git *)`, so the verification used `--manifest-path` rather than
      `cd`).
    - ✅ Documented the gate's now-multi-crate scope in CLAUDE.md's Commands
      section, so `cargo test` at the root is no longer read as sufficient on
      its own.

14. **Numeral count channel, plus case on `Ranting::inflect`** (12-16 hours) —
    *the owed signature break, done once*
    - `docs/superpowers/specs/2026-08-13-number-categories.md` recommends a
      count channel and says to land it inside item 5's signature change; item 5
      already shipped, so it needs its own pass. Item 10's hole 2 wants a case
      parameter on `Ranting::inflect` at the same site (German declines the noun
      itself: dative plural `den Hunden`, genitive `des Hauses`). Both are
      breaking changes to hook signatures — do them together, not twice.

✅ 15. **`Many` exposes its length as the placeholder count** (4-6 hours) —
    open question 3 of the number-categories spec; depends on item 14.
    - ✅ `Many<T>` (`src/collections.rs`) is the one wrapper that genuinely knows
      a count with no numeral in the placeholder at all — its own `Vec`'s
      length — so its `own_count()` helper substitutes
      `count.or_else(|| self.own_count())` before delegating each of the five
      count-carrying hook pairs (`inflect_verb_custom`, `inflect_pronoun_custom`,
      `inflect_article_custom`, `elide_article_custom`, `inflect_adjective_custom`,
      and their `_with_context` twins) to its single item. An explicit
      placeholder numeral is left untouched — `Many` only fills in a `None`,
      never overrides a `Some`.
    - ✅ Scoped to the pre-existing `len() == 1` delegation arm only, per the
      spec's own framing ("cheap... not blocking") and confirmed against
      `tests/ranting/elision.rs`'s `many_with_two_items_does_not_elide`: a
      `Many` holding zero or 2+ items has no single item to delegate a hook
      call to at all, so there is no hook invocation for a substituted count
      to accompany, and that test's pinned "keeps the English default"
      behavior for 2+ items is unchanged.
    - ✅ `Maybe`/`Box` deliberately left untouched, as scoped: each holds at
      most one value with no alternative count to offer (`Maybe(None)` has
      none at all), so both keep forwarding whatever `count` the placeholder
      itself supplied, `None` included.
    - ✅ `as_plural: bool` behavior of `Many`/`Maybe`/`Box` is unchanged for
      all three — this item only affects what `count` a hook receives, never
      `is_plural()`/`inflect()`/the plurality bool computed at
      `handle_placeholder_impl`.
    - ✅ New `tests/ranting/many_count.rs`: a `CountProbe` type whose
      `inflect_verb_custom` renders the `count` it was handed, covering an
      empty `Many` (no hook call, English default), a single-item `Many`
      (`count: Some(1)` where a bare placeholder used to hand `None`), a
      2+-item `Many` (no hook call, unchanged), and an explicit-numeral
      placeholder on a single-item `Many` (the placeholder's own count wins
      over `Many`'s length).
    - ✅ Documented in `docs/EXTENSIBILITY.md` §2.9 and a new CLAUDE.md
      "Non-obvious behaviors" bullet.

16. **`is_first_person_subject` as an overridable hook** (6-8 hours) — open
    question 1 of the pronoun-inventory spec, which calls it the one named gap
    with no fork-side workaround.

17. **Sentence detection beyond Latin punctuation** (8-12 hours)
    - `PH_START` decides a placeholder is sentence-initial only after an ASCII
      `.`/`?`/`!` *followed by whitespace*, so auto-capitalization silently
      misses after Greek `;` (and Greek has case), Japanese/Chinese `。` (no
      following space), Urdu `۔`, and before Spanish opening `¿`. Item 6 routed
      capitalization through a hook but never touched detection, which is
      upstream of it.

18. **Dative/genitive on `GrammaticalCase`** (doc-only spike, 6-8 hours) —
    *found by item 10, hole 3*
    - `GrammaticalCase` carries English's inventory; German has four cases and
      `@` means accusative-or-dative, so `dem`/`der` are unreachable. The sharper
      finding: once the entity must carry case to reach dative at all, the `case`
      parameter becomes *ignorable* — `{the =0}` and `{the @0}` render
      identically. A spike, not an implementation task: it is a public enum in a
      trait signature, and the pronoun-inventory spec set the precedent that
      "change nothing, document it" is a legitimate conclusion.

19. **Case marking and pronoun display share one hook** (8-12 hours) — *found by
    item 10, hole 5*
    - A case marker does two jobs: it tells `inflect_article_custom` the noun's
      role *and* switches the noun slot from name to pronoun. A fork that
      overrides `inflect_pronoun_custom` to return the name (which
      `tests/ranting/grammatical_case.rs` demonstrates as typical) then loses
      real pronouns for that entity — `say!("Ich sehe {@0}.", hund)` renders
      "Ich sehe Hund." Needs a way to say "case-mark this, but render the name".

20. **Document the word-order boundary** (doc-only, 4-6 hours) — open question 1
    of the word-order spec: `docs/EXTENSIBILITY.md` in full, one-line pointers
    from CLAUDE.md and README.md, written as a permanent boundary not a TODO.

21. **Document non-English pronoun inventories** (doc-only, 6-8 hours) — the
    entire follow-up the pronoun-inventory spec authorizes (its recommendation
    is "change nothing, document what exists"), plus flipping the
    `SubjectPronoun` row in Key Architecture Decisions to ✅ Locked.

22. **Per-language template selection spike** (doc-only, 6-10 hours)
    - Item 1 recommends per-language templates and says the caller selects one
      before the `say!()` call — but never how. `say!()` parses its literal at
      compile time, so selection cannot be a runtime catalogue lookup; it must be
      a literal `match lang { … }` at every call site, scaling as languages ×
      sentences. That is the real ergonomic cost of item 1's recommendation and
      it is unexamined.

23. **Spanish reference lexicon** (16-24 hours) — *the second acceptance test*
    - Item 10 proved German exercises the hooks but **structurally cannot use
      the adjective hook at all** (hole 4a: attributive adjectives are prenominal,
      the `!` slot is post-noun, and predicative adjectives are uninflected — so
      no German sentence renders the hook's output correctly). Spanish
      adjectives are post-nominal (`el gato negro`), so Spanish can exercise
      item 5 end-to-end where German cannot.
    - Also the largest-userbase language that fully exercises the phase
      (~485M speakers): `el`/`la` (item 2), `tú`/`usted` (item 3),
      agreement (item 5), `¿` (item 17), `de`+`el`→`del` and `el agua`
      (item 7), numeral agreement (item 8). `docs/EXTENSIBILITY.md` §4.3
      already carries a complete Spanish impl to build from.
    - Same falsification contract as item 10: public API only, every hole
      recorded in the crate's README rather than worked around.

### v1.3 Success Criteria
- A non-English `Ranting` impl can obtain gender/noun class, grammatical case,
  number, and register/dialect **without** an external string-keyed side table
- Word-order feasibility answered in writing, with the boundary of what
  `ranting` will and will not do stated explicitly rather than left open
- Adjective agreement, capitalization, elision and numeral rendering all have
  runtime hooks defaulting to today's English behavior
- Zero behavioral change to existing `say!()`/`say_with!()` output — every item
  above is additive with an English-preserving default (verified by the existing
  suite passing unchanged, not by new tests alone)
- One working reference language module (German) built on the public API only,
  with every remaining gap it hits recorded rather than worked around

---

## Post-v1.2: Future Directions

### v1.3.0+: Beyond Phase 6
- **`ranting-i18n` Companion Crate** — now scoped as
  [Phase 6](#phase-6--v130--internationalization-foundations) above, which owns
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
  — the overlap with [Phase 6 item 3](#phase-6--v130--internationalization-foundations) is now
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
| Noun gender / noun class as an entity property | 🎯 v1.3 (Phase 6 item 2) | Open-ended `&'static str` class label, not a closed Masc/Fem/Neut enum — Bantu has a dozen-plus classes, Danish has common/neuter. Threaded like `GrammaticalCase` (commit `11d531ed`) |
| `SubjectPronoun` is a closed English enum | ✅ Locked (v1.3, Phase 6 item 3) | **Stays English-only, unchanged**: the parallel fork-owned pronoun set already exists (`inflect_pronoun_custom`/`inflect_verb_custom`, consulted *first*; `subjective() -> &str` is an uninterpreted channel), so option (c) is doc-only and breaks nobody. Extending the enum is semver-major for every downstream `match` (re-exported, not `#[non_exhaustive]`); an open channel trades a build failure for silent `it`/`its`/`itself` at five `unwrap_or(It)` sites and reverses Phase 4 item 4's invariant. T-V (`du`/`Sie`, `tu`/`vous`) is a pronoun slot, so it rides the addressee's own subject label — `NarrationContext.register` stays story-wide and inert, a documented default only. See `docs/superpowers/specs/2026-08-13-pronoun-inventory.md` |
| Number is `bool` throughout the hook signatures | 🎯 v1.3 (Phase 6 item 4) | Arabic dual / Slavic paucal / CLDR categories don't fit. Replacing it is breaking in all six `_custom` hooks — spike states the cost before it's paid |
| English orthography, phonology and numerals hard-coded | 🎯 v1.3 (Phase 6 items 5-8) | Adjective agreement, `uc_1st_if`/`apply_case`, `a`/`an` elision, and `#var` spelling all become hooks with English-preserving defaults |
| GPL-3 via `license-file` | ✅ Complete (v1.2) | Relicensed to plain `license = "MIT"` 2026-08-13 (copyright holder's choice, differs from the dual-license recommendation in [PROPOSED LICENSE CHANGE](#proposed-license-change-awaiting-decision)); already-published 0.2.1 on crates.io remains GPL-3 |

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

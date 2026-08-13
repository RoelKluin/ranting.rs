# Completed Work

## Phase 1: v0.3.0 — Foundation & Ergonomics ✅

*Goal: Make Ranting more ergonomic and feature-complete for present-tense usage.*

**Status**: Complete

- ✅ Named Arguments & Empty Placeholders (`say!("{=person}", person = my_var)`)
- ✅ Error Messages (compile-time validation for placeholder syntax)
- ✅ Gender-Neutral Pronouns (singular they/them support)
- ✅ Test Coverage (>80% code coverage, 21 unit + 171 integration + 8 doctests)

**Features Working**:
- Core `Ranting` trait and `Noun` struct
- Basic subject/object/possessive pronouns (I, you, he, she, it, we, they, thou, ye)
- Simple present tense (do/does, am/are/is)
- Articles (a, an, some, the) with conditional display
- Optional article logic
- `say!()`, `ack!()`, `nay!()` macros
- Partial `ask!()` macro

---

## Phase 2: v1.0.0 — Grammar Depth ✅

*Goal: Handle past, future, and continuous tenses; lock in API for v1.0.*

**Status**: Complete

### Verb Tense Support ✅
- ✅ Past tense marker: `{=person <walk}` → "He walked" / irregular: "He went"
- ✅ Present continuous: `{=person =walk}` → "He is walking"
- ✅ Future tense: `{=person >walk}` → "He will walk"
- ✅ Past continuous: `{=person <=walk}` → "He was walking"
- ✅ Present perfect: `{=person %walk}` → "He has walked" / irregular: "He has gone"
- ✅ Past perfect: `{=person <%walk}` → "He had walked" / irregular: "He had gone"
- **7 distinct tenses** all working end-to-end

### Irregular Verbs ✅
- ✅ 118 irregular past-tense verbs in built-in table (go→went, see→saw, take→took, etc.)
- ✅ 118 irregular past-participle table (go→gone, do→done, see→seen, etc.)
- ✅ Regular past-tense rules with phonetic handling (walk→walked, try→tried, like→liked)
- ✅ Continuous forms with phonetic rules (silent e, consonant doubling, ie→y)

### Technical Debt Elimination ✅
- ✅ **Verb table duplication → codegen** (build.rs + data/irregular_verbs.txt, single source of truth; symlinked for ranting_derive)
- ✅ **Pronoun array fragility → exhaustive match dispatch** (PronounForms struct, ArticleOrSo methods, IrregularPluralVerb methods; eliminates index-coupling risk)
- ✅ **Derive macro attributes → Rationalized to 4 core** (subject, name, singular_end, plural_end; 3 cosmetic: plural_you, uc, no_article)

### Documentation & Adoption ✅
- ✅ Tutorial ("Getting Started with Ranting"): 5 sections, 5 worked examples, 30-40 min read
- ✅ Cookbook (10 recipes): Game dialogue, chatbots, interactive fiction, gender-neutral pronouns, etc.
- ✅ API docs: Examples on every public function; cross-references between related features
- ✅ ROADMAP clarity: v1.0 scope (all tenses) vs. v1.1 scope (plurals, extensibility) locked in

### Success Criteria Met
- ✅ Test coverage >85% (200 tests: 21 unit + 171 integration + 8 doctests)
- ✅ All Phase 2 tense markers complete (7 tenses)
- ✅ Tutorial + 10-recipe cookbook published
- ✅ Zero breaking changes commitment (semver locked)
- ✅ Zero critical known issues

---

## Phase 3: v1.1.0 — Plurals, Extensibility, and Ecosystem ✅

*Goal: Complete core morphology; enable community contributions.*

**Status**: Complete

- ✅ Irregular noun plurals: 100+ nouns, codegen from `data/irregular_plurals.txt`.
  ⚠️ `get_plural`/`get_singular` are unit-tested but still not wired into any
  inflection call site — see `docs/architecture-review-2026-08-13.md`.
- ✅ Trait-based inflection extensibility: `inflect_verb_custom()`/`inflect_pronoun_custom()`/
  `inflect_article_custom()` with `None`-fallback defaults; documented in `docs/EXTENSIBILITY.md`.
- ✅ Runtime tense & viewpoint selection: `say_with!(context, ...)` + `NarrationContext`/`Tense`/
  `Person`, unblocking Recounting M9. `say!()` output is unaffected.
- ✅ Narration context threading: `NarrationContext.register`/`.dialect`, surfaced via three
  `Ranting::*_with_context` default trait methods (`ctx` as a parameter, never entity-owned).
- ✅ Reflexive pronouns (`%` case marker): full 9-pronoun set (myself..themselves, thyself).
- ✅ Comparative/superlative adjectives (`!`/`!!` post-noun markers), codegen from
  `data/irregular_adjectives.txt`.
- ✅ Recursive type inflection: `Box<T>`, `Many<T>` (wraps `Vec<T>`), `Maybe<T>` (wraps
  `Option<T>`) as `say!()` placeholder subjects — `Vec`/`Option` can't get direct impls
  due to orphan rules, hence the wrapper types.
- ✅ `heed!()` input parsing (v1 scope): `heed!(template, input)` matches literal words plus
  `{name}`/`{name...}`/`{$name}` captures against free-form input text. Full grammatical
  inversion (`unsay!()`) was explicitly not pursued — several `say!()` inflection choices
  aren't injective, so no general inverse is buildable.

### v1.1 Success Criteria Met
- ✅ Irregular plurals support for 100+ nouns
- ✅ Trait extensibility API stable and documented
- ✅ Runtime tense & viewpoint selection working (unblocks Recounting M9)
- ✅ Narration context threading designed and integrated
- ✅ Reflexive forms + comparative/superlative working
- ✅ Zero breaking changes from v1.0

---

## Phase 4: v1.2.0 — Architecture Consolidation ✅ (items 1-6, 8; item 7 pending)

*Goal: pay down structural debt while there is no userbase to break.*

**Status**: Items 1-6 and 8 complete; item 7 (licensing) is an explicit pending human
decision, not an agent task — see ROADMAP.md's "Proposed License Change" section.

- ✅ Extracted `ranting_core`: one shared plain rlib crate `ranting`/`ranting_derive` both
  depend on as an ordinary path dependency, replacing all three build.rs copy-into-`OUT_DIR`
  mechanisms and the dual-strum-version constraint.
- ✅ Dependency modernization: syn 1→2, darling 0.14→0.20, regex 1.6→1.11 in `ranting_derive`;
  dropped `proc-macro-error`/`lazy_static` for `std::sync::LazyLock`; edition 2024 everywhere.
- ✅ Typed placeholder spec: `PlaceholderSpec`/`CaseKind`/`PostSpec` (in `ranting_core::placeholder`)
  replace the old `caps: [&str; 5]` array and `~TENSE~`/`~DEGREE~` string sentinels.
- ✅ Typed `SubjectPronoun` stored in `Noun`; non-panicking `Noun::try_new`; `Noun::new()` keeps
  its documented panic as an intentional convenience constructor.
- ✅ Public API cleanup: `inflect_possesive` → `inflect_possessive`; `handle_placeholder` marked
  `#[doc(hidden)]`; `ack!()`/`nay!()` now expand to plain `Ok(say!(...))`/`Err(say!(...))`
  expressions instead of a hidden `return`.
- ✅ Hand-written placeholder tokenizer in `ranting_core::ph_ext::parse`, giving precise
  per-error compile messages; the old `PH_EXT` regex is retained only as a `#[cfg(test)]`
  differential-fuzz reference oracle, not on the compile path.
- ✅ Repo hygiene (item 8): scratch/log/scraped-source files excluded from publishing.
- 🔄 Licensing decision (item 7): proposal is `MIT OR Apache-2.0` (replacing `GPL-3` via
  `license-file`); awaiting the copyright holder's decision — nothing relicensed yet.

### v1.2 Success Criteria
- ✅ One shared `ranting_core` crate; zero build.rs copy/symlink mechanisms remain
- ✅ No unmaintained dependencies; single strum/regex/syn versions
- ✅ No stringly-typed macro↔runtime interface; no `~TENSE~` sentinel
- ✅ No runtime panics reachable from public API with invalid data
- ✅ Placeholder syntax errors report precise spans
- 🔄 License decision — pending


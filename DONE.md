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


# `ranting-ar` — an Arabic reference lexicon

This crate is [ROADMAP.md](../ROADMAP.md) **Phase 7 item 5**: the *third* acceptance test, after
`ranting_i18n`'s German (Phase 6 item 10) and `ranting_es`'s Spanish (item 23).

It implements Arabic for a deliberately tiny closed vocabulary — five nouns spanning sound and
broken plurals and both letter classes, two verbs conjugated across twelve person/gender/number
combinations, the numerals `0..=10` with gender polarity, and the definite article — **using only
`ranting`'s public API**. Its `Cargo.toml` depends on `ranting` and nothing else: no
`ranting_core`, no `ranting_derive`, no `pub(crate)` item, and no fork of
`handle_placeholder_impl`.

Its purpose is falsification, not utility. Everywhere Arabic cannot reach something through the
public trait seam, that is a hole and it is written down here rather than worked around. Each hole
is pinned by a test in [`tests/holes.rs`](tests/holes.rs) that asserts what the crate *actually*
renders — so if a later change closes one, the test fails and the entry gets struck rather than
quietly rotting.

`cargo fmt --check`, `cargo clippy -- -D warnings` and `cargo test` are green here and in every
other directory of the repository.

## Why Arabic, after German and Spanish

Both existing falsifiers are Indo-European and, more specifically, fusional: they decline and
conjugate by affixing a closed set of endings onto a stable stem, which is exactly the shape
`NounClass`, `GrammaticalCase`, `AdjectiveDegree` and the `_custom` hooks were designed against.
The Phase 7 item 2 spike scored Arabic for two things neither of them can reach, and the item 4
build decision confirmed both:

**A third morphological number.** `كتاب` / `كتابان` / `كتب` — singular, dual, plural. German and
Spanish have nothing to ask for beyond `to_plural: bool`, so nothing in the repo had ever pushed
on it. The spike found that Phase 6 item 14's count channel reached every hook that *agrees* with
a noun but not `Ranting::inflect`, which renders the noun itself — so Arabic dual came out
*half* expressible: the verb agreed in the dual while the noun beside it rendered the plural.
Output that looks grammatical and is wrong in one word is a worse failure than either extreme,
and it is not reachable by any Indo-European lexicon. **Phase 7 item 11 closed it**, and this
crate is what it was closed for; `tests/arabic.rs::the_dual_renders_on_the_counted_noun` and
`::the_verb_agrees_in_the_dual` are the two halves now agreeing.

**`elide_article_custom`'s first real user.** The Phase 7 item 1 audit found that hook overridden
by neither existing fork: German's and Spanish's real fusions are all preposition-side, routed
through `inflect_preposition_custom` instead, so the hook built for `le`+`homme`→`l'homme` had
never been exercised by a language that needs it. Arabic's `ال` is written *bound* to its noun and
assimilates to the fourteen sun letters. Both halves are exactly what that hook's post-assembly
design offers — and the answer to the spike's open question is that the two-string signature is
sufficient: the trigger consonant is `following.chars().next()`, and dropping the separator is
what "the hook replaces all three" already allows.

## What works through the hooks alone

| Arabic | Reached via |
| --- | --- |
| Singular / **dual** / plural on the noun, selected by the placeholder's numeral (`كتاب` / `كتابان` / `كتب`), across both sound and broken plurals | `Ranting::inflect`'s `count` (Phase 7 item 11) |
| Verb agreement across person, gender and number **including the dual** (`كتبا`, `كتبتا`, `كتبن`) | `inflect_verb_custom` + `count` |
| `ال` written bound to its noun, with sun-letter assimilation (`الشّمس`) and moon letters left alone (`القمر`) | `elide_article_custom` |
| The **absence** of an indefinite article — `{a 0}` renders the bare noun | `inflect_article_custom` returning an empty string |
| Arabic-Indic digits (`١٢`), including under a `:fmt` width spec | `inflect_numeral_custom`, `NumeralStyle::Digits` |
| Spelled numerals with **gender polarity** — `ثلاثة كتب` (masculine noun, feminine-marked numeral) but `ثلاث طالبات` | `inflect_numeral_custom`, `NumeralStyle::Words` + `NounClass` |
| Subject pronouns including the dual `هما` | `inflect_pronoun_custom` + `count` |

## Root-and-pattern morphology has no seam consequence

The spike asked whether non-concatenative morphology (`k-t-b` → `kataba`/`kitāb`/`maktaba`, a root
mapped onto a template rather than a stem plus affix) breaks the assumption that "the base form"
is a well-defined notion at the macro↔runtime seam. It does not, and this crate is the evidence:
every hook returns an opaque `String`, so a form is a table row here exactly as a suffixed form is
a table row in `ranting_es`. Nothing at the seam can tell a broken plural from a sound one —
`tests/arabic.rs::the_dual_works_for_sound_and_broken_plurals_alike` asserts both through the same
call. The answer is "it is the fork's own lookup-table problem", as the spike predicted.

Right-to-left rendering is likewise not a `ranting` question: the crate assembles `String`s with
`format!()` and never emits direction-control characters or layout markup, so Arabic script
renders right-to-left in the consumer's renderer the way any RTL text does.

## Holes that do not reproduce here

- **`ranting_i18n`'s hole 4a — adjective position.** German attributive adjectives are prenominal,
  so German structurally cannot reach `inflect_adjective_custom` in a correct sentence. Arabic's
  are post-nominal like Spanish's (`الكتاب الجديد`), so the `!` slot is in the right position.
  This crate deliberately does **not** implement the adjective hook: `ranting_es` already proves
  it works there, and a second working example is not a falsification. That is a scoping choice,
  not a gap.
- **`ranting_es`'s euphonic-article case.** `ال` is invariant for gender, number and (unvocalized)
  case, so there is no article-selection rule at all — everything interesting about the Arabic
  article happens in elision instead.
- **Capitalization.** The Arabic script is caseless, so `capitalize`'s English default is a no-op
  on every string this crate produces and is not overridden. Phase 6 item 6's own customer list
  names caseless scripts as the *expected* outcome rather than a gap.
- **Preposition-article fusion.** Arabic prepositions do not fuse with `ال` orthographically
  (`في الكتاب` is two words), so `inflect_preposition_custom` — which both other forks exercise —
  simply has nothing to do here.

## The holes

### 1. The dual needs a numeral written in the placeholder

Arabic marks the dual on the noun with no numeral at all: `كتابان` is "two books", written and
spoken without a digit. `+`/`-` are the only number markers the placeholder grammar accepts and
`+` means plural, so there is no marker a bare dual could use. This is a **grammar** change rather
than a hook signature, and `docs/superpowers/specs/2026-08-13-number-categories.md` puts it
deliberately out of scope; Phase 7 item 11 closed the numbered half and left this half open on
purpose.

`ArabicNoun` deliberately carries **no** `dual` field, though entity state is the workaround
`ranting_i18n` uses for definiteness and would work here too. Taking it would hide the hole rather
than pin it. Pinned by `hole_1_a_bare_dual_is_unreachable`.

### 2. `GrammaticalCase` has no genitive

Arabic nouns decline for nominative, accusative and genitive, and the genitive is obligatory after
any preposition (`في الكتابِ`). `GrammaticalCase` carries English's five-marker inventory, so a
placeholder can say subjective or objective and nothing else.

This is the same shape as `ranting_i18n`'s hole 3 — German needs a dative — reached from an
unrelated language family, which is worth recording: the gap is not a German quirk. Unvocalized
script does not write the case ending, so nothing is *visibly* wrong in this crate's output, which
is the trap rather than a reprieve. Pinned by `hole_2_the_genitive_case_has_no_marker`.

### 3. A bound pronoun cannot change the word it attaches to

Arabic's object and possessive pronouns are bound morphemes on the host word — `كتابه` "his book",
`رأيته` "I saw him" — not free words. `inflect_pronoun_custom` returns a string rendered as its own
placeholder, so on its own it emits the bare suffix standing alone.

Probing this narrowed it, and the narrowed version is the interesting one. **Juxtaposition is
enough when the host does not change**: a template may abut two placeholders, and `{0}{`1}` really
does render `كتابه`. What it cannot do is *rewrite the preceding placeholder's output* — and the
suffix changes its host, since a feminine noun's `ة` becomes `ت` before it. So `طالبة` + `ه`
renders `طالبةه` where Arabic needs `طالبته`.

That is exactly the power `elide_article_custom` has for an article — replace the article, the
separator and the following text as one — and there is no equivalent on the pronoun side. It is
filed as a hole rather than as the word-order boundary (§2.12) for that reason: the *position* is
reachable, the *fusion* is not. Pinned by `hole_3_bound_pronouns_cannot_attach_to_their_host`.

### 4. The construct state (الإضافة) is not expressible

In a possessive chain the first noun takes **no** article even when definite, and the second takes
the genitive: `كتاب المعلم` "the teacher's book", never `الكتاب المعلم`. Definiteness there is a
property of the noun's *position in the phrase*, while `skip_article` is a property of the entity,
so nothing lets one entity know it is in construct. A template can of course write the right words
— that part is the caller's job and not a gap — but the same entity cannot be definite in one
position and article-less in another. Pinned by `hole_4_the_construct_state_is_not_expressible`.

### 5. Word order (a boundary, not a gap)

Classical Arabic's neutral order is verb-subject-object. `ranting` inflects words within a
template; their order is the template's, and the template is the caller's. Recorded the way
`ranting_i18n`'s hole 8 is — a permanent boundary that this crate reconfirms from a
non-Indo-European direction. Every word still inflects correctly in a VSO template. Pinned by
`hole_5_verb_initial_word_order_is_a_boundary_not_a_gap`.

### 6. The numeral is always separated from its noun by a space

`handle_placeholder_impl` pushes a hard-coded space between the rendered numeral and the noun, and
offers it to no hook — unlike the article separator, which `elide_article_custom` receives and may
drop. Arabic writes that space, so this costs nothing here; it is recorded because this crate
proves the asymmetry is real rather than Japanese-specific. Found by the Phase 7 item 3 spike,
scheduled as ROADMAP.md Phase 7 item 12. Pinned by
`hole_6_the_numeral_is_always_separated_from_its_noun_by_a_space`.

## Also observed, not holes

- **A panic in `ranting`, found on this crate's first run.** `split_at_find_end` advanced one
  *byte* past a byte index `rfind` had returned, so the elision splice sliced mid-codepoint and
  panicked for any article whose last character is multibyte — Arabic, Greek, Cyrillic and CJK
  alike. Nothing about it was Arabic-specific; it survived because `elide_article_custom` had no
  real user until this crate and both other forks' articles are ASCII. Fixed in `ranting` and
  pinned by `tests/ranting/property_based.rs::elision_does_not_panic_on_a_multibyte_article`.
  Recorded here because "the first fork to use a hook finds a defect in it" is the apparatus
  working, not an Arabic gap.
- **An unmodelled verb degrades to the bare form, not to English agreement.** `say!("{0 sing}")`
  renders `sing`, not `sings`, because English agreement is driven by the subject label and this
  crate's labels are `هو`/`هي`/`هم`/`هن`, which `ranting` does not recognize and degrades to
  non-agreement on (Phase 4 item 4). That is what makes an unmodelled verb visibly wrong rather
  than plausibly wrong, and it is the same channel `ranting_i18n` and `ranting_es` use.
- **`inflect`'s `case` parameter is unread here**, for the same reason `ranting_es` leaves it
  unread: unvocalized Arabic does not write case endings, so there is nothing for the entity to
  select. A vocalized fork would read it and immediately hit hole 2.

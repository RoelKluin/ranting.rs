# Feasibility: dative/genitive on `GrammaticalCase`

**Status**: design spike complete; conclusion is **change nothing, document
it** — the same shape of conclusion as
`2026-08-13-pronoun-inventory.md` (Phase 6 item 3) and
`2026-08-12-input-parsing-feasibility.md` (feature B, `unsay!()`), now the
fourth spike in the crate to land there. ROADMAP.md Phase 6 item 18, found by
item 10's `ranting_i18n` lexicon as hole 3.

## Motivation

`GrammaticalCase` (`src/lib.rs:1195-1213`) is a seven-variant enum — `Name`,
`Subjective`, `Objective`, `PossessiveDeterminer`, `PossessivePronoun`,
`Reflexive`, `Hidden` — mirrored 1:1 from `ranting_core::placeholder::CaseKind`
(`ranting_core/src/placeholder.rs:40-58`) via `impl From<CaseKind> for
GrammaticalCase`, which in turn mirrors the five case markers (`=`, `@`,
`` ` ``, `~`, `%`) plus the markerless `Name`/`Hidden` pair. It is English's
own case inventory: nominative, accusative-doing-duty-for-both-objective-
cases, and two flavors of genitive-ish possessive. Its own doc comment on
`Objective` already concedes the gap: "English doesn't distinguish accusative
from dative; neither does this."

`ranting_i18n`'s `GermanNoun` (Phase 6 item 10) makes the gap concrete. German
has four grammatical cases — nominative, accusative, dative, genitive — and
`@` is read as accusative, so dative (`dem`/`der`/`den` + noun-plural `-n`)
and genitive (`des`/`der` + noun `-s`) are unreachable from *any* placeholder
marker. `ranting_i18n/README.md`'s hole 3 states the sharper finding this
spike is scoped to examine, not just the missing variant: **once the entity
must carry the case to reach dative at all, the `case` parameter handed to
`inflect_article_custom` becomes ignorable.**

```rust
let dativ = GermanNoun::hund().in_case(Case::Dative);
assert_eq!(say!("{the =0}", dativ), "Dem Hund");
assert_eq!(say!("{the @0}", dativ), "Dem Hund");
```

Both calls hand `inflect_article_custom` a different `GrammaticalCase`
(`Subjective` vs. `Objective`) and get the same output, because `GermanNoun`'s
impl reads its own `Case::Dative` off `self` and never consults the `case`
parameter it was passed. This is pinned by `hole_3_grammatical_case_cannot_
express_dative_so_the_marker_is_ignored` in `ranting_i18n/tests/holes.rs`.

The question this spike answers is not "should German be supported" — item 10
already proved a German lexicon is buildable through the public API, dative
included (via `GermanNoun::in_case`, see hole 2) — but **does widening
`GrammaticalCase` itself change anything for a fork that has already crossed
that threshold**, and what would it cost to try.

## What the code actually does today

### `GrammaticalCase` is derived, not chosen, and arrives after the decision is already made

`GrammaticalCase` is computed once, at the `CaseKind` → `GrammaticalCase`
`From` conversion, straight off which of the five case markers (or their
absence) appears in the placeholder — a **compile-time-baked, per-occurrence**
fact about the template text (`ranting_core::placeholder::PlaceholderSpec`
bakes the `CaseKind` at macro-expansion time; nothing at runtime can change
which marker a given `{...}` used). It is handed to `inflect_article_custom`
purely as *information about which of five renderings this call site wants*
— subject pronoun, object pronoun, possessive determiner, possessive pronoun,
or reflexive. It was never a request for the hook to *compute* case marking
from scratch; German's own case system (nominative subject vs. accusative
direct object vs. dative indirect object vs. genitive possessor) does not map
onto "which of five pronoun-ish renderings is this" at all, since German case
selection depends on syntactic role (direct vs. indirect object, governed
preposition, genitive attribute) that `say!()`'s placeholder grammar has no
representation for — a placeholder does not know whether its noun is the
direct or indirect object of the surrounding sentence, only which display
form (`=`/`@`/etc.) the caller wrote.

### The five-marker split and the four-case split are different taxonomies, not the same one at different granularity

English's markers answer "as what part of speech does this noun display" —
subject pronoun, object pronoun, possessive-before-a-noun, possessive-alone,
reflexive. German's cases answer "what syntactic role does this noun play in
its clause" — and a single German case can appear under more than one of
English's display forms (a dative noun can be a subject-like topic, an
object, or the target of a dative preposition, all still "dative"), while a
single English marker (`@`, objective) covers two German cases (accusative
*and* dative direct/indirect objects). There is no finer split of the
existing five markers that recovers a clean four-way partition — the two
taxonomies cross-cut each other. This is *why* hole 3 exists at all: it is
not that `GrammaticalCase` is missing two enum variants in an otherwise
matching shape, it is that the marker set was never designed to name German's
distinction in the first place.

### The entity is the only thing that can name the missing distinction

Given the above, the only place left to say "this occurrence is dative, not
accusative" is the entity itself — which is exactly what `GermanNoun::in_case`
does (`ranting_i18n/src/noun.rs`), the same shape `NounClass` (Phase 6 item 2)
and hole 5's `Render::Name`/`Render::Pronoun` flag already use for other
German distinctions the placeholder grammar can't express. Once that's true,
the `case: GrammaticalCase` parameter's job shrinks to "which of the five
*English-shaped* renderings does this call site want", which for a German noun
under `Objective` is already saturated by "did the caller write `=` or `@`" —
a question `GermanNoun` doesn't need answered a second time, because it
already knows its case from `self`.

## Options, scored

Scored against three things ROADMAP.md's Key Architecture Decisions table
already tracks as deliberate for sibling enums: exhaustive-match safety
(`GrammaticalCase` is matched — see `src/lib.rs`'s `CaseKind`/
`GrammaticalCase` conversion and every `_custom` hook call site that
constructs one), whether it needs a new placeholder marker (the marker set —
"Placeholder syntax (full grammar support)" — is ✅ Locked, "the crate's
identity"), and breaking-change surface.

### (a) Add `Dative`/`Genitive` variants to `GrammaticalCase` — rejected

Extend the enum with two more members, mapped from... nothing — this is the
first problem. `CaseKind` (the thing `GrammaticalCase` mirrors) has exactly
one variant per marker plus `Name`/`Hidden`; there is no sixth or seventh
marker to parse a `Dative`/`Genitive` `CaseKind` out of. So this option
splits into two sub-options, both bad:

**(a1) New markers.** Invent two new sigils so `` {the ^noun} `` means dative
and some other mark means genitive. This needs a new placeholder marker,
which the marker set being ✅ Locked forecloses without revisiting that
decision — and even if revisited, English has no grammatical use for either
mark (English does not decline dative/genitive on pronouns beyond the
possessive markers it already has), so two new sigils would exist solely to
be ignored by every English caller and every English-only downstream `match`.
It is **breaking**: two new `CaseKind`/`GrammaticalCase` variants are a hard
build failure at every exhaustive match on either enum, in-crate and
downstream, with no marker-set precedent for adding sigils that only a
non-English fork would ever write.

**(a2) Variants with no new marker.** Add `Dative`/`Genitive` to
`GrammaticalCase` alone, reachable only by... nothing — `GrammaticalCase` is
constructed exclusively via `From<CaseKind>` at one call site
(`handle_placeholder_impl`), so a variant `CaseKind` can never produce is dead
on arrival; no caller could ever observe `case == GrammaticalCase::Dative`
without a marker to request it. This does not close hole 3 — a fork's
`inflect_article_custom` still receives `Objective` for both accusative and
dative uses of `@`, unable to tell them apart, so the ignorability finding is
untouched. It is **still breaking** (an exhaustive match downstream fails to
compile against the widened enum) for zero behavioral gain.

Both sub-options are breaking for every downstream exhaustive match on
`GrammaticalCase` (it carries no `#[non_exhaustive]` — confirmed absent from
`src/lib.rs`), the same asymmetry `2026-08-13-pronoun-inventory.md` found for
extending `SubjectPronoun`: a closed enum's safety net is a compile error on
an unhandled variant, and every new variant collects on every match everywhere,
English-only downstream crates included, for a distinction only a
case-declining fork can use.

### (b) Make the case channel open-ended, `NounClass`-style — rejected

Replace `GrammaticalCase` with a newtype over `&'static str` (or widen
`inflect_article_custom`'s `case` parameter to something a fork populates
itself), so a German fork can hand back `"dative"`/`"genitive"` and a
Sanskrit fork eight-way case marking, unconstrained by what `say!()`'s own
markers distinguish.

This does not transfer the way `NounClass` does, for the same reason
`2026-08-13-pronoun-inventory.md` found option (b) didn't transfer for
`SubjectPronoun`, but the mechanism is different here and worth stating
precisely. `NounClass` is a label `ranting` only *forwards* — it is read off
`self.noun_class()` and handed to a hook, and `ranting` never branches on its
value. `GrammaticalCase`, by contrast, is *computed by `ranting` itself* from
`CaseKind`, which is baked from which marker the caller wrote. An open string
channel here would need one of two shapes, and both are worse than what
exists:

- **Populate it from `self`**, i.e. ask the entity what case it's in before
  calling `inflect_article_custom`. But the entity does not know its
  syntactic role either — `GermanNoun::in_case(Case::Dative)` only knows
  "someone told me I'm dative", set by the caller before the `say!()` call,
  which is precisely what the entity-carried-state pattern (option (c),
  below) already achieves without touching the parameter's type at all. Making
  the parameter a string the entity echoes back to itself is a longer path to
  the same place.
- **Populate it from the marker, still**, just stringified (`"objective"`
  instead of `GrammaticalCase::Objective`). This changes nothing about hole 3
  — the marker set still only distinguishes five things, so the string still
  collapses accusative and dative onto the same value. Opening the *type*
  without closing the *marker* gap it's meant to carry doesn't help.

Either shape is a **breaking** signature change on `inflect_article_custom`/
`_with_context` (and every override, `ranting_i18n`'s `GermanNoun` included —
the same "signature break on a trait method with a default body still
requires updating every impl that overrides it" cost Phase 6 item 17 names
for `capitalize`), for a channel that either duplicates entity-carried state
or doesn't close the gap it was built to close. **No new marker is needed**
for this option, which is the one respect in which it's cheaper than (a) —
but it fails on the same "doesn't close hole 3" ground either way, so the
marker-set question is moot.

### (c) Change nothing; document that a fork past two cases carries case on the entity — recommended

State plainly, as the crate's answer, what `ranting_i18n` already
demonstrates in practice: `GrammaticalCase` is scoped to *which of five
English-shaped display forms a placeholder marker requested*, not to a
general syntactic-case representation, and a fork whose language has more
cases than that finds out on contact — the same way `SubjectPronoun` finding
told a fork with more pronouns than English's nine to own its pronoun set.

| What a fork needs | Where it already exists |
|---|---|
| Know which of the five markers a placeholder used | `case: GrammaticalCase` parameter, unchanged |
| Distinguish two syntactic cases that collapse onto the same marker (dative vs. accusative under `@`) | Carry the actual case on the entity — `GermanNoun::in_case`, read from `self` inside the hook |
| Render the noun's own declined form (not just the article) under that case | `Ranting::inflect()`'s `case: GrammaticalCase` parameter (Phase 6 item 14) is *also* saturated at `Name`/`Hidden` only (hole 2) — same entity-carries-it answer, `GermanNoun`'s own inflection table |
| A case-agreement chain longer than English's five slots (adjective declension class, hole 4b) | Entity-carried again — `GermanNoun::with_article` |

This is not a new mechanism — it's the same one Phase 6 item 2 (`NounClass`)
and hole 5 (`Render::Name`/`Render::Pronoun`) already established for "the
placeholder grammar has no marker for this distinction, so the entity carries
it". Hole 3's finding sharpens *why* it applies here too: once the entity
carries the case, as it must to reach dative or genitive at all, having
`inflect_article_custom` *also* receive `case: GrammaticalCase` doesn't hurt
— a fork that ignores the parameter (as `GermanNoun` does) loses nothing it
was using, and a fork whose distinction is *coarser* than English's five
markers can still consult it. The parameter is not wrong, only insufficient
past two-way case marking, and insufficiency there does not justify widening
it in a way that breaks every user who has three-way-or-fewer needs.

**Exhaustive-match safety: fully preserved** — `GrammaticalCase` stays seven
variants, in-crate and downstream matches are untouched.

**No new placeholder marker.** The marker set stays exactly as ✅ Locked.

**Breaking, and for whom: nobody.** Doc-only, like item 21's pronoun-inventory
write-up. No signature, no invariant, no codegen change.

**What (c) costs:** a fork whose language distinguishes more cases than
`GrammaticalCase`'s five markers do (German's dative/genitive split from
accusative; Slavic locative/instrumental; Sanskrit's eight-way system) must
carry its own case state on the entity and read it inside every hook that
needs it — `inflect_article_custom`, `inflect_pronoun_custom`, `inflect()`,
and any adjective-declension-class logic, exactly the pattern `GermanNoun`
already uses across holes 2/3/4b/5. This is not free — it's a per-entity
builder method (`in_case`) and internal state the fork owns — but it is the
same one-time cost item 10 already paid to build the working German lexicon,
not a new one this recommendation introduces.

## Recommendation

**(c): change nothing, document it.**

Concretely, the follow-up work this spike authorizes is documentation only:

1. This spec, as the record of the finding and the rejected alternatives.
2. `docs/EXTENSIBILITY.md` — fold this into `GrammaticalCase`'s section (or
   add a subsection under it) explaining what the enum's five variants scope
   to, and pointing at `GermanNoun::in_case` as the worked pattern for a case
   the marker set can't name.
3. ROADMAP.md Key Architecture Decisions — add a row for `GrammaticalCase` (it
   has none today; `NounClass`'s row is the nearest analogue and is itself
   still 🎯, not ✅) stating it is locked at English's five-marker inventory,
   with this spec as the pointer — mirroring the `SubjectPronoun` row's shape.
4. Item 18 itself, marked done in ROADMAP.md with this outcome recorded as
   sub-bullets, same as items 1/3/17 recorded theirs.

## What stays out of reach under this recommendation

Named by construction, in item 1's and item 3's style:

- **A `case` parameter that, by itself, disambiguates German's four cases
  from five English-shaped markers.** The two taxonomies cross-cut each
  other; no re-slicing of the existing five variants recovers a clean
  four-way split, and adding variants either needs a marker the ✅ Locked
  grammar doesn't have (rejected sub-option a1) or adds a variant nothing
  can ever construct (rejected sub-option a2).
- **A single mechanism that both names a case finer than English's and stays
  non-breaking.** Every way examined to widen `GrammaticalCase`'s type or
  its variant count is a breaking signature/enum change reaching every
  downstream exhaustive match or hook override; only leaving it alone and
  documenting the entity-carries-it pattern is not.
- **Recovering the marker→case mapping from `inflect()`'s own `case`
  parameter instead.** Item 14's `Ranting::inflect()` case parameter has the
  identical saturation problem (hole 2): the only call site that reaches
  `inflect()` is bare-placeholder rendering, always `Name`/`Hidden`, so it
  cannot carry a real case either. Both parameters bottom out at the same
  answer.
- **Closing hole 3 itself.** This item is doc-only by design (ROADMAP.md
  states it as a "doc-only spike"); the hole stays open and pinned by
  `hole_3_grammatical_case_cannot_express_dative_so_the_marker_is_ignored`
  in `ranting_i18n/tests/holes.rs`, exactly as items 3/17's doc-only
  conclusions left their own hole tests (where applicable) unstruck.

## Rejected alternatives, recorded

- **(a1) new markers for dative/genitive** — rejected: needs revisiting the
  ✅ Locked marker set for two sigils English has no use for; breaking for
  every downstream exhaustive match on `CaseKind`/`GrammaticalCase`.
- **(a2) new variants with no new marker** — rejected: unreachable from any
  marker, so it doesn't close hole 3 either; still breaking.
- **(b) open string-typed case channel** — rejected: doesn't transfer the
  `NounClass` precedent (`ranting` computes `GrammaticalCase` itself, it
  doesn't merely forward it); either duplicates entity-carried state or still
  collapses accusative/dative if sourced from the marker; breaking signature
  change on `inflect_article_custom`/`_with_context` and every override.

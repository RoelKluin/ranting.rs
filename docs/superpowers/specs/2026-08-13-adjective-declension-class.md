# Feasibility: reporting adjective declension class to `inflect_adjective_custom`

**Status**: design spike complete; conclusion is **change nothing, document
it** — the same shape of conclusion as `2026-08-13-grammatical-case-inventory.md`
(Phase 6 item 18), `2026-08-13-pronoun-inventory.md` (Phase 6 item 3) and
`2026-08-12-input-parsing-feasibility.md` (feature B). ROADMAP.md Phase 6
item 27 (queued after item 26, whose number this task was originally handed
under before item 26 was assigned to the preposition-fusion hook — see the
note at the end of this document). Found by item 10's `ranting_i18n` lexicon
as hole 4b, the half of
hole 4 that is **not** the prenominal-position problem item 1/20 already
close out as a permanent word-order boundary.

## Motivation

German attributive adjective endings decline by which article — if any —
preceded the adjective, independent of the adjective's own comparative/
superlative degree:

```text
strong (no article):     kleiner Hund    /  kleine Katze   /  kleines Haus
weak   (definite art.):  der kleine Hund /  die kleine Katze / das kleine Haus
mixed  (indefinite art.): ein kleiner Hund / eine kleine Katze / ein kleines Haus
```

`Ranting::inflect_adjective_custom`/`_with_context` (`src/lib.rs:2040-2071`,
Phase 6 item 5) already receives `adjective`, `degree: AdjectiveDegree`,
`case: GrammaticalCase`, `class: NounClass`, `as_plural: bool`,
`count: Option<PlaceholderCount>` and `uc: bool` — every agreement input
item 5 and item 14 wired up. None of them says which article rendered in the
same placeholder. `self` cannot answer it either: the article is chosen at
`get_article_or_so` (`src/lib.rs:235`) from template text (`` {the ...} ``
vs. `` {a ...} `` vs. no article word at all), a per-*placeholder* fact, not
a per-*entity* one — the same noun renders `der Hund`, `ein Hund` and `Hund`
across three different templates.

`ranting_i18n/README.md` states this precisely as hole 4b:

> German endings depend on which article precedes: weak after `der`
> (`der kleine Hund`), mixed after `ein` (`ein kleiner Hund`), strong after
> none (`kleiner Hund`). `inflect_adjective_custom` receives `degree`, `case`,
> `class`, `as_plural` and `uc` — never the rendered article — and `self`
> cannot know it either, because the article is template text chosen per
> placeholder. So the choice is carried on the entity
> (`GermanNoun::with_article`); without it the lexicon must guess, and
> guesses weak.

Hole 4b is scoped separately from 4a (the prenominal-position problem, owned
by item 1's ✅ Locked word-order boundary — see
`docs/superpowers/specs/2026-08-13-word-order-feasibility.md`) precisely
because 4a is unfixable by any hook (`ranting` will not move text) while 4b
is a missing *signal*, the kind of gap the hook system exists to close. This
spike is scoped to 4b alone.

## What the code actually does today

### The article is rendered before the adjective hook runs, and stays in scope

`handle_placeholder_impl` (`src/lib.rs:436`) renders the article first —
`get_article_or_so` is called and, on success, the byte range it wrote into
`res` is recorded as `article_span: Option<(usize, usize)>` (`src/lib.rs:529`,
546-562, 598-616). The `PostSpec::Degree` arm that calls
`inflect_adjective_custom_with_context` runs much later in the same function
(`src/lib.rs:1011-1030`), with `res` and `article_span` both still in scope
and untouched by anything in between except by the noun-count/case
machinery that computed the values already passed to the hook. This is the
*same* mechanism `elide_article_custom` (item 7) and
`inflect_preposition_custom` (item 26) already use: both are called at a
point after the article's rendered text is sitting in `res`, and
`inflect_preposition_custom` already takes the rendered article as a plain
`article: &str` parameter (`src/lib.rs:1954`, the `inflect_preposition_custom`
hook signature). So handing the adjective hook the same slice is not a new
capability for the runtime to gain — it is exposing a value the function
computes anyway to one more of its own call sites, once removed from where
`elide_article_custom`/`inflect_preposition_custom` already receive it.

### But knowing the rendered word is not the same as knowing the declension class

`get_article_or_so` renders whatever `inflect_article_custom`/the built-in
English article table returns — for German that's `der`/`die`/`das`/`ein`/
`eine`/`kein`/… or `""` (no article, per hole 6/item 11). A fork receiving
that raw string still has to classify it into strong/weak/mixed itself, the
same three-way split `ranting_core::placeholder::ArticleKind` already
performs for a *different* purpose: `ArticleKind::classify` (`ranting_core/
src/placeholder.rs:250-296`) recognizes `the` → `The`, `a`/`an`/`some` →
`AAnSome`, `these`/`those` → `TheseThose`, everything else → `Other` — and
it exists to classify the closed pre-noun word list the preposition-fusion
hook reads (`PlaceholderSpec::pre_kind`/`pre_chained_kind`), not to
classify the noun's own rendered article. Its four variants don't line up
with German's three declension classes either: `The` and `TheseThose` are
both "definite" in German's sense (weak declension), `AAnSome` conflates
`a`/`an` (mixed, since German indefinite articles decline) with `some`
(which has no German equivalent choice at all), and `Other` doesn't
distinguish "no article rendered" from "an article word `ArticleKind`
doesn't recognize". Reusing `ArticleKind` as-is would misclassify on arrival;
correctly it would need its own English-agnostic three/four-way enum
(strong / weak / mixed / and does "no article" collapse into strong, or
need a fourth arm?) — a second classification scheme sitting next to
`ArticleKind`'s, answering a different question.

## Options, scored

Scored the same way `2026-08-13-grammatical-case-inventory.md` scored
`GrammaticalCase`'s options: exhaustive-match safety, whether it needs a new
placeholder marker, and breaking-change surface — plus, specific to this
gap, whether the option actually closes hole 4b or only relocates it.

### (a) Add a rendered-article parameter to `inflect_adjective_custom`/`_with_context` — rejected as insufficient on its own

Pass `article: &str` (the text `get_article_or_so` already wrote into `res`,
sliced by the existing `article_span`) as a new argument, mirroring
`inflect_preposition_custom`'s own `article: &str` parameter exactly.

This is mechanically the cheapest of the three options — the value already
exists in scope at the call site, so there's no new state to compute, only a
new parameter to thread. But it hands the fork a raw string, not a
classification: `GermanNoun` (or any fork) would still have to answer
"is `der` weak, is `ein` mixed, is `""` strong" itself, string-matching
against its own language's article table inside every override. That's not
nothing — it's strictly better than guessing, since the fork at least learns
*which* article rendered — but it stops short of closing 4b outright, in the
same way `2026-08-13-preposition-fusion.md`'s option (a) (opening the
pre-noun word list alone) was rejected for fixing nothing by itself: the
signal arrives, the classification work is still the fork's.

**Breaking**: yes. `inflect_adjective_custom`/`_with_context` are trait
methods with default bodies (`None`/delegate), so adding a parameter is a
compile break for every existing override — `ranting_i18n::GermanNoun`'s and
`ranting_es::SpanishNoun`'s own `inflect_adjective_custom` implementations
(`ranting_i18n/src/noun.rs:239`, `ranting_es/src/noun.rs:169`; both crates
implement it) included, the identical cost
`2026-08-13-grammatical-case-inventory.md`
names for widening `inflect_article_custom`'s signature and item 17 names
for `capitalize`.

**No new placeholder marker** — the article is already rendered from
existing template syntax; nothing new is written by the caller.

### (b) An `ArticleKind`-shaped typed parameter mirroring `ranting_core` — rejected

Instead of a raw string, pass a closed enum classifying the rendered article
into declension-relevant buckets (e.g. `ArticleKind::{None, Definite,
Indefinite, Other}`), following the same "mirror a `ranting_core` type at
the macro↔runtime seam" pattern `CaseKind`→`GrammaticalCase` and
`DegreeKind`→`AdjectiveDegree` already use.

This closes more of the gap than (a) — the classification work moves from
every fork's own code into one shared enum — but two things make it worse,
not better, than (a):

- **English's own `The`/`AAnSome`/`TheseThose`/`Other` split, reused for
  this, is the wrong split for German's three-way strong/weak/mixed
  distinction** (see above: `The`/`TheseThose` collapse together, `AAnSome`
  conflates `a`/`an` with `some`). A *correctly*-shaped enum for this purpose
  is not `ArticleKind` reused, but a new type — `AdjectiveDeclensionClass`
  or similar — invented specifically for a distinction English doesn't have
  at all (English adjectives don't decline by preceding article; "big dog"/
  "the big dog"/"a big dog" use the identical word "big"). Unlike `CaseKind`
  and `DegreeKind`, which mirror a marker *English's own grammar already
  uses* (case markers, degree markers), there would be nothing at the
  macro↔runtime seam to mirror — the same objection
  `2026-08-13-grammatical-case-inventory.md`'s option (a1) raises against
  inventing `Dative`/`Genitive` `CaseKind` variants with no marker to parse
  them from. `ArticleKind` is baked at compile time from the pre-noun *word
  list* (the closed English article/modal-verb set `ph_ext::parse` matches);
  the noun's own article is not that — it is whichever string
  `inflect_article_custom`/the built-in table happened to return at
  *runtime*, so there is no compile-time marker to classify at all, only a
  runtime string, same as option (a).
- **A closed enum invites the same "collects on every downstream exhaustive
  match, for a distinction only some forks need" cost every closed-enum
  widening in this crate's history has paid** (`SubjectPronoun`,
  `GrammaticalCase`'s rejected a1/a2). Since the runtime value being
  classified is already just a string (there's no `CaseKind`/`DegreeKind`
  equivalent baked at compile time to classify instead), inventing a new
  closed enum purely to wrap that string buys exhaustiveness safety at the
  cost of a second breaking change layered on top of (a)'s — passing the raw
  `article: &str` and letting the fork's own code decide "is this word my
  language's definite article" is strictly cheaper for the same amount of
  information, since the fork owns its own article table already (it had to,
  to render the article via `inflect_article_custom` in the first place).

**Breaking**: yes, and by a strictly worse margin than (a) for the same
underlying information — a second enum type joins the trait surface, still
requires every override site to update, and still doesn't relieve the fork
of writing its own classification logic (it just moves the *shape* of the
lookup table from `match` against a runtime string to `match` against a
`ranting`-defined enum populated by `ranting`'s classification of that same
string — the fork wrote the classification either way, at (a)'s call site
that receives the raw string, or before this option's enum could exist at
all, since something still has to decide which enum variant a given rendered
article maps to, and the only party that can correctly decide that for a
non-English article table is the fork's own `inflect_article_custom`
implementation).

**No new placeholder marker**, same as (a).

### (c) Carry the article/declension choice on the entity, as hole 4b's own text already does — recommended

State plainly what `ranting_i18n`'s own hole 4b write-up already names as
the answer: `GermanNoun::with_article` sets the declension-relevant article
on the entity before the `say!()` call, and `inflect_adjective_custom` reads
it off `self` inside the hook body, exactly the pattern `NounClass` (item 2),
hole 3's `GermanNoun::in_case` (dative/genitive, closed by the
grammatical-case-inventory spike's own recommendation) and hole 5's
`Render::Name`/`Render::Pronoun` flag already establish for "the placeholder
grammar has no marker for this distinction, so the entity carries it".

| What a fork needs | Where it already exists |
|---|---|
| Know the adjective's base form, degree, case, class, plurality, count, uc | `inflect_adjective_custom`'s existing seven parameters, unchanged |
| Know which article precedes it, to pick strong/weak/mixed | Carry it on the entity — `GermanNoun::with_article`, read from `self` inside the hook |
| Keep the choice correct per *placeholder*, not per *entity*, since the same noun takes different articles across templates | The caller sets it via the builder immediately before each `say!()` call that needs a particular article, the same discipline `GermanNoun::in_case` already requires for hole 3's dative/genitive |

This is not a new mechanism for the crate to build — it is `ranting_i18n`
already doing it, informally, as hole 4b's own prose states
("the choice is carried on the entity"); this option is naming that as the
crate's committed answer rather than a workaround the lexicon happened to
reach for.

**Exhaustive-match safety: fully preserved** — no enum changes anywhere.

**No new placeholder marker.**

**Breaking, and for whom: nobody.** Doc-only, like items 18/21's write-ups.
No signature, no invariant, no codegen change — `inflect_adjective_custom`'s
seven-parameter signature stays exactly as item 14 left it.

**What (c) costs:** a fork whose language declines adjectives by preceding
article must set that state on the entity itself, per placeholder, before
calling `say!()`/`say_with!()` — a builder method and internal state the
fork owns, and a discipline (set it fresh each time the article context
changes) that is easy to get wrong silently: nothing prevents a caller from
reusing a `GermanNoun` built with `.with_article(Definite)` in a template
that actually renders `ein`, producing weak endings after an indefinite
article with no error, only wrong German. This is the same "guesses weak
without it" risk hole 4b's own text already names, restated as an ongoing
cost of the recommended answer rather than a defect only the unfixed state
had.

## Recommendation

**(c): change nothing, document it.**

Concretely, the follow-up work this spike authorizes is documentation only:

1. This spec, as the record of the finding and the rejected alternatives.
2. `docs/EXTENSIBILITY.md` §2.5 (`inflect_adjective_custom`'s section) — add
   a note pointing at entity-carried declension state as the pattern for a
   fork whose adjectives agree with the preceding article, cross-referencing
   §2.4 (`NounClass`) and the case-inventory spike's equivalent note for
   `GrammaticalCase`.
3. ROADMAP.md Key Architecture Decisions — no new row is needed; item 5's row
   already covers `inflect_adjective_custom`'s scope, and this finding is a
   refinement of what that row already says rather than a new locked
   decision surface.
4. Item 27 itself, marked done in ROADMAP.md with this outcome recorded as
   sub-bullets, matching items 1/3/17/18's shape.

### If this is ever revisited: bundle it, don't take it alone

Unlike item 25's preposition-fusion spike — which named its own option (b)
as "the real fix, to be bundled with any other owed hook-signature break"
and was itself picked up later as item 26 once that trigger fired — **there
is no currently-owed hook-signature break for this recommendation to ride
along with.** Item 4's number/count-channel debt, the one open breaking-change
item this document's peers repeatedly flagged as "pay once, not per-item",
was closed across items 14 (five hook pairs plus `Ranting::inflect`'s `case`
parameter), 8 (`inflect_numeral_custom`, designed with `count` from the
start) and 26 (`inflect_preposition_custom`, likewise) — see CLAUDE.md's
"What `as_plural: bool` promises" bullet, which now states that debt closed
rather than owed. So **if option (a) or (b) above is ever picked up**, it
would be a *new* signature break opened for this alone, not a discharge of
an existing one — and per this crate's own established discipline (item 14's
"do them together, not twice", item 25's explicit bundling instruction), it
should not be taken in isolation. It should wait until some other concrete
need forces a hook-signature change on `inflect_adjective_custom`/
`_with_context` (or another hook already sharing that break, the way item 14
bundled `case` onto the count-channel break), and land together with that,
rather than paying a standalone breaking change for a gap (c) already
answers without one.

## What stays out of reach under this recommendation

Named by construction, in items 18/21's style:

- **A hook parameter that, by itself, tells a fork whether to render strong,
  weak or mixed adjective endings without the fork tracking anything on the
  entity.** Both typed alternatives examined (raw string, classified enum)
  require the fork to either classify the rendered article itself (option
  (a)) or have `ranting` classify it into a scheme built for a distinction
  English doesn't have (option (b), which still doesn't relieve the fork of
  authoring the article-to-class mapping, just relocates where it's
  authored) — neither is "free" in the way (c) is free of new surface, and
  neither is recommended, so this stays unreachable without entity-carried
  state either way.
- **A single mechanism that reports the preceding article and stays
  non-breaking.** Every way examined to add this information to
  `inflect_adjective_custom`'s signature is a breaking change reaching every
  existing override; only leaving the signature alone and documenting the
  entity-carries-it pattern is not.
- **Closing hole 4b itself.** This item is doc-only by design; the hole
  stays open and pinned by
  `hole_4b_declension_class_is_not_reported_so_it_must_be_carried_on_the_entity`
  in `ranting_i18n/tests/holes.rs`, exactly
  as items 3/17/18's doc-only conclusions left their own holes unstruck.
- **4a, the position problem.** Untouched by this spike, unchanged from
  item 1's ✅ Locked word-order boundary — German attributive adjectives are
  prenominal, the `!`/`!!` slot is post-noun only, and no hook (this one or
  any other) moves template text. This document is scoped to 4b alone, as
  stated at the top.

## Rejected alternatives, recorded

- **(a) rendered-article string parameter on `inflect_adjective_custom`** —
  rejected as insufficient on its own: cheapest to add (the value already
  exists in scope, mirroring `inflect_preposition_custom`'s own `article:
  &str`), but still leaves the fork to classify the string into a
  declension class itself; breaking for every existing override.
- **(b) `ArticleKind`-shaped typed parameter mirroring `ranting_core`** —
  rejected: English's existing `ArticleKind` split (`The`/`AAnSome`/
  `TheseThose`/`Other`) doesn't line up with German's strong/weak/mixed
  three-way distinction, and a correctly-shaped new enum would have nothing
  at the macro↔runtime seam to mirror (there is no compile-time marker for
  the noun's own rendered article, unlike `CaseKind`/`DegreeKind`); strictly
  more breaking-change surface than (a) for the same information, since the
  fork still authors the classification, only relocated.


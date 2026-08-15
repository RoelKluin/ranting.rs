# Design spike: agreeing quantifiers, and the mass/count distinction

**Status**: design spike complete; conclusion is **(a) six quantifier
words/pairs — `no`, `every`↔`all`, `each`, `either`/`neither`, `much`/`many`,
`less`/`fewer` — as new variants on the existing `ArticleKind`/`ArticleOrSo`
pair, reaching `inflect_article_custom_with_context` through new
`get_article_or_so` arms with the exact signal set the article arms carry
today, and (b) a `#[ranting(mass)]` attribute compiling into a new
`Ranting::is_mass()` trait method with a `false` default, in the shape
`gender`→`noun_class()` already uses — neither implemented by this
document**. ROADMAP.md Phase 8 item 3. The sigil grammar is Locked
(ROADMAP.md's Key Architecture Decisions table, "Placeholder syntax (full
grammar support)") and part (a) reserves new pre-slot words, so this spike
answers the scoping questions a maintainer needs to rule on a grammar change,
and stops there — no code in this repository is changed by it.

This is the fourth Phase 8 spike, following the shape of the others in this
directory (ground the question in the code as it stands, survey options
including the rejected ones, state a recommendation and what stays out of
reach under it). The item has two separable halves and this spike keeps them
separate throughout: **(a)** which quantifier words earn a slot and where
they live, and **(b)** where the mass/count flag lives — with a closing
section on why (a) shipped without (b) would be wrong in a way the repo's
review culture keeps finding (grammatical-looking output, wrong in one word).

Every rendering claim below marked *(verified)* was checked by compiling a
scratch crate against a path dependency, the same procedure
`.claude/rules/placeholder-grammar.md` records for diagnostics — this repo
has no `trybuild` harness.

## The question, grounded

`src/language/english.rs:12`'s `ArticleOrSo` (the private runtime
word-adapter `adapt_article` parses the requested word into) and
`ranting_core/src/placeholder.rs:262`'s `ArticleKind` (the compile-time
classification `ranting_derive` bakes into `PlaceholderSpec::pre_kind`) both
stop at a/an/some/the/these/those. So *no*, *every*, *each*, *either*,
*much*/*many* and *less*/*fewer* have no channel, and a quantified noun
phrase is hand-assembled — with the number agreement that motivates using a
placeholder at all done by hand too.

What actually happens today when a template tries, read from the source and
verified against the built crate, is worse than "no channel" — it is three
different accidents depending on spelling:

1. **`{no item}` is `E0425: cannot find value 'no'`** *(verified)*. The
   English parse pass reads it as `noun="no"`, `post=" item"` — the ordinary
   noun + post-noun-verb shape — exactly the `{el gato}` behavior
   `.claude/rules/placeholder-grammar.md` records as permanent for words
   outside the closed pre vocabulary.
2. **`{no +item}` renders `"No items"`** *(verified)* — but only by a chain
   of accidents. The English pass fails outright (`+item` cannot be `post`
   text), so the input reaches `ph_ext::parse`'s **open pass**
   (`ranting_core/src/ph_ext.rs:729-730`), which accepts `no ` as an open
   pre word. At runtime that word classifies as `ArticleKind::Other`, is
   offered to `inflect_article_custom_with_context` (the language-modularity
   arm, `src/lib.rs:380-396`), declined by every English impl, and falls
   through to the **pre-noun verb path** — where plural agreement happens to
   leave a base verb untouched. Force singular agreement and the mask slips:
   `{no $n item}` with `n = 1` renders **`"Noes 1 item left."`**
   *(verified)* — the quantifier conjugated as a third-person verb. The
   plural-looking success is the verb path idling, not a feature.
3. **`{Some info}` on a singular renders `"An information"`** *(verified)*,
   same as `{A info}`. `adapt_article` (`src/language/english.rs:140-161`)
   maps `some` to `ArticleOrSo::A` and, when not `as_plural`, discards it in
   favor of the computed a/an. So *some* as the unstressed mass article
   ("some information", "some water") is unreachable even though the word is
   in the closed vocabulary — which is part (b)'s territory and previewed
   here because it shows the mass gap is not confined to the new words.

One more grounding fact: the still-unused `DemonstrativePronoun` dead enum
(`ranting_core/src/grammar.rs:96-102`, kept per Phase 4 item 1) already
lists `No` and `Neither` alongside the demonstratives. The vocabulary gap
was anticipated the last time this corner was surveyed; this item is that
enum's question finally asked properly.

## Part (a) — the quantifier words

### Which words earn a slot, and how each behaves on number

The selection criterion is the one that justifies a placeholder at all:
**the word (or the phrase around it) varies with number or countability**,
so hand-writing it either forfeits agreement or hard-codes it wrong. Words
that never vary are already perfectly writable as literal template text and
earn nothing.

| Word(s) | On singular agreement | On plural agreement | Number behavior |
|---|---|---|---|
| `no` | "no item" | "no items" | **transparent** — the word never changes; only the noun and verb agree around it |
| `every` | "every item" | "all items" | **swaps to a suppletive plural** — the `these`→`this` machinery pointed at a new pair |
| `each` | "each item" | — | **forces the singular** (see below for what "forces" can mean) |
| `either` / `neither` | "either item" | — | **forces the singular**, same shape as `each` |
| `much` / `many` | "much information" (mass) | "many items" (count) | **picked by countability, not number** — unrenderable correctly until part (b) exists |
| `less` / `fewer` | "less information" (mass) | "fewer items" (count) | same — this pair *is* the canonical mass/count distinction |

Named for the maintainer and deliberately cut, so the first change stays
reviewable: `both` (forces plural, dual-flavored), `all` as a directly
writable keyword (reachable as `{every +item}` under this proposal, and
number-transparent-ish on its own), `such`, `enough`, `several`, `most`,
`any`. Each would be one more row in the same match sites, nothing
structural; `any` interacts with polarity (negative/interrogative contexts)
and should not be attempted without its own look.

"Forces the singular" has a precise, cheap meaning here: `pre`'s first word
is classified **at compile time** (`article_kind_tokens`,
`ranting_derive/src/lib.rs:944-956`), so the macro can bake
`as_plural = false` for `each`/`either`/`neither` exactly as a written `-`
marker does — no runtime machinery, and `{each +item}` becomes a place for a
compile *warning or error* rather than "Each items". `every` does the
reverse of nothing: it stays `every` on singular agreement and renders `all`
when the placeholder's number resolves plural, which is `adapt_article`'s
existing `these`→`this` selection (`singular_demonstrative`,
`src/language/english.rs:31-38`) with one more pair in the table. `no`
renders itself in both numbers — the only work is *not* conjugating it as a
verb, i.e. giving it a real `ArticleKind` so it stops reaching the verb
path.

`much`/`many` and `less`/`fewer` are listed as slot-earners because they are
the item's stated scope, but their selection axis is `is_mass()`, not
`as_plural` — they are the concrete dependency of part (a) on part (b), and
the recommendation is that **they land only in the same change as (b) or a
later one**, never before it (see the final section).

### `ArticleOrSo`, or a sibling type? — `ArticleOrSo` + `ArticleKind`, no sibling

Quantifiers and articles are mutually exclusive occupants of the same
determiner slot: one `pre` first-word position, one classification site in
the macro, one dispatch site at runtime (`get_article_or_so`,
`src/lib.rs:255-398`), one word-adaptation table (`adapt_article`). A
sibling `QuantifierKind` with its own dispatch function would have to
duplicate the entire hook-offer protocol — the `skip_article()` gate, the
`!`/`?` marker stripping, the `inflect_article_custom_with_context` call
with its eight arguments, the `capitalize_with_context` fallback, the
`space` handling — to express new rows of the same table. That is the same
seam-duplication argument that rejected `PostSpec::Passive` in the
participle spike, and it is rejected here for the same reason.

So: new variants on **both** existing types, which are two halves of one
seam —

- `ArticleKind` (compile-time dispatch): say `No`, `EveryAll`, `Each`,
  `EitherNeither`, `MuchMany`, `LessFewer` — following the existing
  pair-per-variant naming (`AAnSome`, `TheseThose`), since the runtime
  re-reads the concrete word from the baked `article` string exactly as the
  `an`-vs-`some` distinction is re-read today. The derive's hand-kept mirror
  `article_kind_tokens` gains the same rows (it is documented as manually
  synchronized with `ArticleKind::classify`, like `CaseKind`'s and
  `TenseMarker`'s local matches).
- `ArticleOrSo` (private runtime word table): variants whose
  `plural_or_definite`/`singular_demonstrative` — or a successor method
  with a less demonstrative-specific name — encode the number behavior
  table above. This enum is `pub(super)` and freely extensible; its
  `#[deny(clippy::wildcard_enum_match_arm)]` discipline is exactly the right
  guard for the new rows. One trap inside it: `adapt_article`'s
  `ArticleOrSo::from_str(requested).expect(..)` panics on an unrecognized
  word, so the new `ArticleKind` arms must parse before they call it, or the
  word list must be extended in both places in the same commit — the
  existing arms already guarantee this by construction (only recognized
  kinds reach `adapt_article`), and the new arms keep that structure.

### How every proposed word reaches `inflect_article_custom` — the hard requirement

`.claude/rules/extension-hooks.md`'s `inflect_article_custom` paragraph is
the contract: the hook receives the article word as a **string**, plus
`GrammaticalCase` (threaded from the placeholder's own `CaseKind`; a bare
`{the noun}` reports `GrammaticalCase::Name`), `NounClass`, `as_plural`,
`count: Option<PlaceholderCount>` and `uc`, with the `_with_context` twin
adding `ctx`. Every quantifier arm added to `get_article_or_so` is written
in the image of the existing `ArticleKind::The` arm (`src/lib.rs:286-305`):
offer `article_form` (the quantifier word, `!`/`?` markers already
stripped), the noun's singular, `case.into()`, `noun.noun_class()`, `as_pl`,
the `count` already threaded through `ArticleRenderCtx`, `uc` and `ctx` to
`inflect_article_custom_with_context` **first**; only on `None` run the
English number-selection fallback and `capitalize_with_context` with
`OrthographyRole::Article`. Three consequences, stated so a maintainer can
check them rather than trust them:

1. **A fork overrides quantifiers with zero new surface.** The hook
   signature does not change — no new parameter, no new hook pair, no
   `ArticleKind` in any hook signature (the enum never crosses the trait
   boundary today and still doesn't). A German fork returning `Some("kein")`
   for `article == "no"` does so with the same case/class/count signals its
   `der`/`den`/`dem` selection already uses. This is the difference between
   the proposal and today's accident: currently `no` reaches the hook only
   through the open pass's `Other` arm *for the placeholder spellings that
   happen to parse*, and the English fallback behind it is the verb path.
2. **The case signal is identical by construction** — same `CaseKind` →
   `GrammaticalCase` conversion, same `Name` default for unmarked
   placeholders, and the fused `*=`/`*@` markers keep delivering the real
   grammatical role, because none of that machinery is touched.
3. **The count signal is identical by construction** — `ArticleRenderCtx
   .count` is already populated for every call, `Many`'s
   `count.or_else(|| self.own_count())` substitution rides along unchanged.

4. **English output stays byte-identical, with one deliberate exception
   class.** For the words as *nouns/idents* nothing changes that compiles
   today into something different — see the breakage analysis next.

### What reserving the words breaks, and what growing `ArticleKind` breaks

Two separate questions; the honest answers differ in kind.

**Reserving the words in the closed pre vocabulary** (the `PH_EXT` `pre`
alternation at `ranting_core/src/grammar.rs:132-137` plus `ph_ext`'s
`match_a1..a4` family — this *is* a character-class-level grammar change,
so, unlike the participle spike, the `PH_EXT`/`ph_ext` parity lockstep is
fully engaged: both sides change, `assert_parity`/`parity_fuzzed` corpus
rows document the new words, and `article_kind_tokens` in `ranting_derive`
is the third hand-kept site). The parse-priority lesson from
`placeholder-grammar.md` ("a new alternative in a repeated group is not
local to that alternative") applies with a known blast radius:

- `{no item}`-shaped templates: today a compile error (E0425) unless the
  caller *has a variable named `no`/`every`/`each`/`either`* — in which case
  the template currently renders that variable's name plus `item` conjugated
  as a verb, and would reparse as quantifier + noun. This is a real,
  breaking reparse for pathologically-named variables, and it must be said
  out loud — but it is also **exactly the reservation class the grammar
  already owns**: a variable named `some` or `the` is shadowed in pre
  position today, and the 14 modal words are reserved the same way. The new
  words join an existing class of reserved first words rather than creating
  a new kind of hazard.
- `{no +item}`/`{no $n item}`-shaped templates (the open-pass accident):
  output changes from verb-path rendering to article-path rendering. In the
  plural spellings the bytes happen to be identical ("No items"); in the
  singular spellings the change is `"Noes 1 item"` → `"No 1 item"` — a
  defect fix, but formally an output change from input that compiles today,
  so it belongs in the announcement, not under byte-identity.
- Non-English open-pass templates (`{no *=gato}`, Spanish first-person
  plural): the word now classifies as a quantifier instead of `Other`.
  The hook is still called first with the same string, so a fork that
  answers keeps working; only the English *fallback* behind a declined hook
  changes. Worth one pinned test; not worth a design change.

**Growing the public `ArticleKind` enum.** The enum is nominally reachable
downstream as `ranting::placeholder::ArticleKind` (via the undocumented
`pub use ranting_core::placeholder` that generated code needs,
`src/lib.rs:90`), it is not `#[non_exhaustive]`, and a third-party
exhaustive `match` on it would stop compiling. Three mitigating facts, each
checked against the tree: `.claude/rules/crate-layout.md` states that
nothing in `ranting_core` is part of `ranting`'s public semver surface even
where re-exported; no hook signature carries the type, so the *supported*
extension surface never sees it; and none of the four falsifiers nor either
gaps crate matches (or mentions) it — unlike `NumeralStyle`, whose growth
the ordinal spike had to score as a hard major-version break precisely
because the falsifiers match it exhaustively with no wildcard. Inside the
repo the exhaustive matches that gain arms are `get_article_or_so`'s
four-arm `match` and the derive's mirror; `ArticleOrSo`'s
wildcard-denied `impl`s are private and gain rows freely. Recommendation:
grow the enum without `#[non_exhaustive]` (which is itself a breaking
attribute to add, and trades a loud downstream error for a silently
swallowing `_` arm — the same trade the ordinal spike declined), cite the
crate-layout policy in the changelog, and let the maintainer decide whether
that policy citation is enough or the change waits for a major anyway.

### The zero-count idiom, corrected and retired into ergonomics

ROADMAP.md item 3 says "there are no items" is expressible today as
`` {?#n +items} ``. **That spelling does not parse** *(verified: `error:
expected article or verb`)* — `match_nr` accepts `?` only before `$`
(`ranting_core/src/ph_ext.rs:611-615`, matching `PH_EXT`'s `\??\$`), and
`?#n` plus `+` would be two occupants of the single `nr` slot besides. The
idiom that actually works is:

```rust
say!("There {are no ?$n item}.", n)
// n = 0 → "There are no items."    (verified)
// n = 1 → "There is no item."      (verified)
```

— `are` as the closed-vocabulary pre modal, `no` as an **inert extra pre
word** (`finish_pre_candidates`' `(?:\s+[\w-]+)*?` tail), and the hidden
numeral `?$n` carrying count-driven agreement to the verb and noun while
rendering nothing. It is genuinely good — including the `n = 1` singular,
which a hand-written "are … items" gets wrong — and genuinely
undiscoverable: nothing in it says "zero"; the `no` is untouched literal
text that only reads as a quantifier because English number agreement
happens to bend around it. Part (a)'s `no` variant is the ergonomic
replacement (`{no +item}` meaning what it appears to mean, through the
article channel), the idiom keeps working unchanged, and the ROADMAP text
should carry the corrected spelling either way — done in the PROPOSED
section this spike ships with.

## Part (b) — where the mass/count flag lives

### `#[ranting(mass)]` attribute, trait method, or both? — both, one mechanism

The precedent to copy is exact, and it is `gender`'s: `#[ranting(gender =
"...")]` generates a `noun_class()` **trait-method override**, and when the
attribute is absent generates *nothing at all*, leaving the trait default —
so an undeclared struct is byte-identical to pre-existing codegen
(`get_noun_class_fn`, `ranting_derive/src/ranting_impl.rs:214-231`). The
attribute is the declaration surface; the trait method is the mechanism;
they are not competing options. Concretely:

- `Ranting::is_mass(&self) -> bool { false }` — a defaulted trait method,
  the same shape as `skip_article()`. The default is what preserves English
  byte-identity: no existing type declares mass, so no existing call site
  changes.
- `#[ranting(mass)]` — a bare boolean attribute (the `no_article` parsing
  shape, `RantingOptions`' `no_article: bool`) generating the `true`
  override. A `mass = "$"` field-read variant can follow the `gender = "$"`
  precedent later if a fork needs per-instance massness; not proposed now.
- `Noun::with_mass()` — the runtime builder, because `Noun` has no
  attributes to declare and would otherwise be the one `Ranting` impl in the
  crate with no way to say it. This is the `with_plural_end`
  /`DeclaredEnding` precedent from the pluralization work, repeated exactly.

A trait method *alone* (no attribute) would work but wrong-foots the derive
crowd — every other per-entity grammatical fact (`subject`, `gender`,
`no_article`, `singular_end`) is declarable in the attribute, and mass would
be the one fact requiring a hand-written impl block. An attribute *alone*
(macro-resolved, no trait method) cannot work at all: the consumers of the
flag are runtime sites (`get_a_or_an` selection, `adapt_article`'s
successor, the `much`/`many` pick) reached through `&dyn`-shaped generic
code that can only ask the entity, and `Many`/`Maybe`/`Box` delegation
(`src/collections.rs`) needs a method to forward under the existing
`len() == 1` rule.

**Rejected: encoding mass in `NounClass`** (e.g. `gender = "mass"`). The
axes are orthogonal — *das Wasser* is neuter **and** mass; Spanish *la
información* is feminine **and** mass — so overloading the class label
steals it from languages that need both. And `NounClass` is deliberately
never interpreted by `ranting` (`extension-hooks.md`), whereas `is_mass`
exists precisely to *be* interpreted, by the a/an logic and the quantifier
table. Different contract, different method.

**What (b) fixes on its own, before any quantifier lands** — the item's own
example: `{a 0}` on "information" renders `"An information"` *(verified)*
because `get_a_or_an` runs unconditionally in the `AAnSome` arm. With
`is_mass()` true, that arm renders `some` (the unstressed mass article —
which the closed vocabulary already contains and `adapt_article` currently
discards on singulars, per accident 3 above) or, under a maintainer-chosen
alternative, elides the article entirely; either is a rendering change
**only for types that newly declare mass**, so byte-identity holds. The
hook, as always, is offered the word first, so a fork can do something else
entirely.

Whether `is_mass` should also be surfaced as a parameter on
`inflect_article_custom` (alongside `class`) is deliberately **not**
proposed: the hook has `self`, and the one place `self` is the wrong object
— wrapper delegation — already substitutes correctly because `Many`
delegates the whole hook call. The `class` parameter exists for that
wrapper case; `is_mass` reached through the delegated `self` needs no
duplicate. If a falsifier proves otherwise, that is a finding, per the
falsifier contract.

## Why (a) is only correct once (b) exists

Stated as the ordering constraint a maintainer would actually schedule by:

1. **Two of the six proposed slots are unimplementable without (b).**
   `much`/`many` and `less`/`fewer` select on countability. Without
   `is_mass()` the only available proxy is `is_plural()`, which guesses
   wrong on exactly the nouns the words exist for: "information" is
   singular, so a number-driven pick renders `"much items"` /
   `"many information"`-class errors — grammatical-looking, wrong in one
   word, the failure mode `docs/architecture-review-2026-08-15.md` exists to
   catch. These two pairs must not ship before (b) under any slicing.
2. **The other four are correct-by-luck without (b), and (a) widens the
   luck's exposure.** `no` is number-transparent and mass-safe ("no
   information") only because a mass noun's `is_plural()` happens to be
   false and singularization is the identity on it. `every`/`each`/`either`
   are count-only quantifiers; on a mass noun ("every information") the
   crate would render the writer's error faithfully — acceptable, same
   stance as `{a information}` today — but *diagnosing* it, or degrading it
   gracefully, requires the flag. Shipping (a) first bakes in "the article
   channel has no idea what kind of noun this is" at six new call sites.
3. **(b) is also what makes the existing vocabulary correct.** The `{a 0}` →
   "an information" defect and the unreachable mass-`some` (accident 3) are
   in the *shipped* article set; (b) fixes them with no help from (a). The
   dependency runs one way only.

Recommended slicing, therefore: **(b) first or simultaneously; (a)'s
number-driven words (`no`, `every`↔`all`, `each`, `either`/`neither`) in the
same or a following change; `much`/`many` and `less`/`fewer` strictly after
(b) is in.**

## Recommendation

Ship part (b) as `is_mass()` + `#[ranting(mass)]` + `Noun::with_mass()`,
then part (a) as `ArticleKind`/`ArticleOrSo` variants dispatched through new
`get_article_or_so` arms that offer every word to
`inflect_article_custom_with_context` with the unchanged signal set. This
spike does not implement either — the sigil grammar is Locked, part (a)
reserves pre-slot words and grows a nominally-public enum, and part (b)
changes what `{a 0}` renders for newly-declared types; all of that needs a
maintainer's sign-off. Decisions left open for that sign-off:

1. **The word list's cut line** — the six proposed rows versus adding
   `both`/`all`-as-keyword now; `any` explicitly deferred.
2. **The mass-`AAnSome` rendering** — `some` versus article elision for
   `{a 0}` on a declared-mass noun (both defensible; `some` recommended
   because the word is already in the vocabulary and the elision story
   belongs to `skip_article`/`no_article`).
3. **`{each +item}` and kin** — compile error versus warning versus silent
   `as_plural = false` override for a number-forcing quantifier meeting a
   contradicting marker (error recommended; the repo's "don't silently
   guess" stance).
4. **Semver posture for `ArticleKind`** — minor with the crate-layout policy
   citation versus waiting for a major; `#[non_exhaustive]` recommended
   against in either case.
5. **The ROADMAP idiom correction** — the `` {?#n +items} `` spelling in
   item 3's text does not parse and should read
   `` {are no ?$n item} ``; carried in the PROPOSED section regardless of
   the rest.

## What stays impossible under this recommendation, until implemented

- `{no item}`, `{every item}`, `{each item}`, `{either item}` keep failing
  with E0425 (or keep rendering a same-named variable as a noun); the
  quantified noun phrase stays hand-assembled.
- `{no $n item}` with a singular count keeps rendering `"Noes 1 item"`
  through the open-pass verb path.
- `{a 0}` on "information" keeps rendering `"An information"`, and `some` on
  a singular keeps being discarded for a/an — there is no way to declare a
  noun mass.
- "much/many" and "less/fewer" remain literal template text, hard-coded per
  template, with no agreement.
- The zero-count idiom `` {are no ?$n item} `` keeps working exactly as it
  does today, before and after.

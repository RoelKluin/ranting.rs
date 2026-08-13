# Feasibility: pronoun inventory and T-V register for non-English `Ranting`

**Status**: design spike complete; conclusion is **don't build this** —
option (c), keep `SubjectPronoun` English-only, recommended in its *thin*
form: the "parallel fork-owned pronoun set consulted first by
`handle_placeholder_impl`" that item 3 scopes **already exists** as
`inflect_pronoun_custom`/`inflect_verb_custom` (+ their `_with_context`
twins), and `Ranting::subjective() -> &str` is already an open string
channel. No new trait, no new channel, and no change to `SubjectPronoun`
is proposed by this document. ROADMAP.md Phase 6 item 3.

This is the third spike in the crate to conclude "the honest answer is
narrower than the ask"; `2026-08-12-input-parsing-feasibility.md` (feature B,
`unsay!()`) and `2026-08-13-word-order-feasibility.md` (Phase 6 item 1) are
the precedents.

## Motivation

`SubjectPronoun` (`ranting_core/src/grammar.rs`) is a closed enum of nine
English pronouns — `I`, `You`, `Thou`, `He`, `She`, `It`, `We`, `Ye`, `They`
— typed into `Noun::subject` by Phase 4 item 4 and matched totally (no
wildcard arms) at every site that derives a form from it. It cannot express:

- **T-V distinction** — German `du`/`Sie`, French `tu`/`vous`;
- **clusivity** — inclusive vs. exclusive "we";
- **dual number** — a distinct "we two"/"you two";
- **gendered plurals** — French `ils`/`elles`.

That exhaustiveness is a stated architecture decision ("Pronoun/article/verb
tables → exhaustive match | ✅ Complete"), not an accident, so the question
this spike answers is not "can the inventory be widened" but **what does each
way of widening it cost in exhaustive-match safety, and who does it break**.

### One correction to item 3's premise

ROADMAP.md item 3 says `SubjectPronoun` is "matched exhaustively with
`#[deny(...)]` guards throughout `src/language/english.rs`". That is very
slightly overstated, and the distinction matters for scoring option (b). The
two `#[deny(clippy::wildcard_enum_match_arm)]` attributes in that file sit on
`impl ArticleOrSo` (line 20) and `impl IrregularPluralVerb` (line 68) —
neither of which is `SubjectPronoun`. `SubjectPronoun`'s own total matches
are:

| Site | File | Guard |
|---|---|---|
| `SubjectPronoun::as_str` | `ranting_core/src/grammar.rs:101` | total match, no attribute |
| `is_subjective_plural` | `ranting_core/src/grammar.rs:130` | total match + doc comment stating why there is no wildcard |
| `pronoun_forms` | `src/language/english.rs:195` | total match + comment, no attribute |

So the safety net is *the compiler's own* exhaustiveness check on a closed
enum, plus three `SubjectPronoun::iter()`-driven tripwire tests
(`as_str_round_trips_through_from_str`,
`is_subjective_plural_covers_every_variant` in `grammar.rs`, and
`pronoun_forms_match_expected_table` in `english.rs`, which asserts
`SubjectPronoun::iter().count() == EXPECTED.len()`). It is not a
clippy-lint net. That makes the net *stronger* than stated for option (a) —
a new variant is a hard build failure, not a lint — and makes option (b)'s
loss of it correspondingly total, because an open channel bypasses the
compiler check rather than downgrading it.

## What the code actually does today

Everything in this section is a property of the code as it stands
(`src/lib.rs::handle_placeholder_impl`, `src/language/english.rs`,
`ranting_derive/src/ranting_impl.rs`), not a projection.

### The pronoun hook is already consulted first

`handle_placeholder_impl` (`src/lib.rs:479-553`) dispatches on
`CaseKind`, and each of the five pronoun-rendering arms has the identical
shape:

```rust
CaseKind::Subjective => {
    if let Some(custom) = noun.inflect_pronoun_custom_with_context(
        subjective, PronounCase::Subjective, noun_class, pronoun_as_pl, uc, ctx,
    ) { custom } else { inflect_subjective(subjective, pronoun_as_pl, uc) }
}
```

The English table is the *fallback*. `conjugate_verb` (`src/lib.rs:305`) has
the same shape for `inflect_verb_custom_with_context`, and
`get_article_or_so` for `inflect_article_custom_with_context`. This is
precisely the "parallel pronoun set that `handle_placeholder_impl` consults
first" that item 3 describes as option (c)'s deliverable — it landed in v1.1
(Phase 3 item 4) and was extended with `NounClass` in v1.3 (Phase 6 item 2).

### The subject label reaching the hook is uninterpreted

The hook receives `subjective: &str`, and the only transformation applied
before the call is `narration::resolve_viewpoint` — which is a no-op unless
`say_with!()` set `narration_person` *and* the declared subject is `I`/`we`.
Nothing lowercases, normalizes, pluralizes or validates the label on its way
to the hook: `pluralize_pronoun`'s English rewriting happens *inside*
`inflect_subjective` and friends, i.e. only in the fallback branch the hook
already replaced.

### The string channel is already open — for derived structs

`ranting_derive/src/ranting_impl.rs:174-181` emits, for a literal `subject`
attribute:

```rust
fn is_plural(&self) -> bool { #is_plural }
fn subjective(&self) -> &str { #subject_str }
```

with `is_plural` computed at macro-expansion time by
`language::is_subjective_plural(subject_str)`. There is **no validation** of
`subject_str` against `is_subject`. So this compiles today:

```rust
#[derive_ranting]
#[ranting(subject = "Sie", gender = "polite")]
struct Kunde {}
```

`subjective()` returns `"Sie"`; `is_plural()` is `false` (the degrade path,
since `SubjectPronoun::from_str("Sie")` fails). The `subject = "$"` branch
(lines 128-135) is open in the same way at runtime, reading whatever
`self.subject: String` holds. `Ranting::subjective`'s signature is
`fn subjective(&self) -> &str` — a string, not a `SubjectPronoun` — and
`Ranting::is_plural` is overridable, so a fork can already carry `Sie`,
`vous`, `ils`, `elles`, an inclusive/exclusive `we`, or a dual, and render
all five cases plus verb agreement from its own tables.

The one type that is *not* open is `Noun` itself: its field is
`subject: SubjectPronoun` and `Noun::try_new` returns
`Err(InvalidSubjectError)` for anything `SubjectPronoun::from_str` rejects.
That is Phase 4 item 4's deliberate invariant, and it is the whole of what a
fork gives up: a fork carrying `Sie` declares its own
`#[derive_ranting] #[ranting(subject = "$")] struct` instead of reusing
`Noun`.

### What leaks when a fork *doesn't* override a hook

This is the real cost surface, and it is the same one option (b) would
institutionalize. Five sites in `src/language/english.rs` — `inflect_adjective`
(265), `inflect_subjective` (272), `inflect_objective` (279),
`inflect_possessive` (303), `inflect_reflexive` (332) — do:

```rust
let forms = pronoun_forms(SubjectPronoun::from_str(pluralized).unwrap_or(SubjectPronoun::It));
```

An unrecognized label silently renders as `it`/`it`/`its`/`its`/`itself`.
That degrade-don't-panic behavior is correct for a formatting library (Phase 4
item 4 chose it deliberately), and it is contained today because it is only
reachable via a fork that opted out of the closed `Noun` constructor. Two
further English string-matches behave the same way:

- `is_subjective_plural` returns `false` for any unrecognized label
  (`grammar.rs:141`), so `Sie` is singular for agreement unless the fork
  overrides `is_plural`;
- `inflect_verb` (`english.rs:105`) dispatches on
  `match pluralize_pronoun(subject, as_plural)` with arms `"I"`,
  `"he" | "she" | "it"`, and `_` verbatim — so an unrecognized label falls to
  the `_` arm and gets the **plural/uninflected** verb form. Note this is a
  *different* silent degrade from the five pronoun sites above, which give
  `it`'s forms; and note it happens to be right for `Sie sind` (see
  *Interaction with item 4*);
- `pluralize_pronoun` itself (`english.rs:164`) rewrites on the string
  literals `"I"`/`"thou"`/`"he"|"she"|"it"` and the inverse
  `"we"`/`"ye"`/`"they"`, passing anything else through unchanged;
- `narration::is_first_person_subject` is literally
  `matches!(subject, "I" | "we")` (`grammar.rs:150`). **A fork whose
  first-person labels are `ich`/`wir` gets a silent no-op from
  `NarrationContext.narration_person`** — `resolve_viewpoint` returns `None`
  and the viewpoint override never fires. This is a named residual gap, not a
  hypothetical; see *What stays out of reach*.

## Options, scored

Scored primarily against exhaustive-match safety, which the Key Architecture
Decisions table calls out as deliberate, and secondarily against the
"English rules live in `src/language/english.rs`" separation.

### (a) Extend `SubjectPronoun` with non-English variants — rejected

Add `Sie`, `Vous`, `Ils`, `Elles`, `WeIncl`, `WeExcl`, a dual, … to the enum
in `ranting_core::grammar`.

**Exhaustive-match safety: preserved, and it is what makes this option
expensive.** Every added variant is a hard build failure at three named sites
(`as_str`, `is_subjective_plural`, `pronoun_forms`) plus three
`iter()`-driven tests, one of which asserts an exact variant count. That is
the net working exactly as designed — the cost is paid, not traded away.

**Rejected on two grounds:**

1. **It puts non-English vocabulary in the shared grammar crate.** `pronoun_forms`
   lives in `src/language/english.rs` and returns five English forms per
   variant. A `Sie` variant forces that English function to answer "what is
   `Sie`'s reflexive?" — either with German (`sich`, in the English module) or
   with a placeholder that renders `itself`. Both are worse than not having the
   variant. The same applies to the enum's `#[strum(serialize_all = "lowercase")]`
   `FromStr`, which becomes a mixed-language recognizer.
2. **It is unbounded.** There is no defensible stopping point between `Sie` and
   the full Bantu/Austronesian/Semitic pronoun space, which is the same argument
   that made Phase 6 item 2's `NounClass` an open `&'static str` rather than a
   closed `Masculine/Feminine/Neuter` enum. A closed enum is right *because* it
   is scoped to one language.

**Breaking, and for whom:** `SubjectPronoun` is `pub use`-re-exported from
`ranting` (`src/lib.rs:56`) and carries **no `#[non_exhaustive]`** (verified:
the attribute appears nowhere in `src/` or `ranting_core/src/`). So any
downstream crate that writes an exhaustive `match` on a `SubjectPronoun`
stops compiling the moment a variant is added — a semver-major break for
*every* user, English-only ones included, in exchange for a feature only
non-English forks want. (If (a) were ever revisited, adding
`#[non_exhaustive]` would have to land first, in its own major release, and
would itself weaken the in-crate exhaustiveness story.)

### (b) Open pronoun channel, `NounClass`-style — rejected

A newtype over `&'static str` (or the existing `subjective() -> &str` promoted
to a first-class documented channel with `SubjectPronoun` demoted to an
English-internal detail), mirroring Phase 6 item 2's `NounClass`.

**Exhaustive-match safety: destroyed, and the failure mode is wrong output
rather than a failed build.** This is the decisive asymmetry. Under (a) an
unhandled pronoun is a compile error at three named sites; under (b) it is
`SubjectPronoun::from_str(label).unwrap_or(SubjectPronoun::It)` at five sites
rendering `it`/`its`/`itself`, plus `is_subjective_plural` silently answering
`false`, plus `is_first_person_subject` silently answering `false`. A German
fork that forgets one `PronounCase` arm in its `inflect_pronoun_custom` gets
`itself` in the middle of a sentence, with nothing — not the compiler, not a
test tripwire — pointing at it.

**The `NounClass` precedent does not transfer.** `ranting` never interprets a
noun class; it only hands the label to hooks, so there is no in-crate table
for an unknown label to fall off. `ranting` *does* interpret the subject
label, at every site listed under *What leaks when a fork doesn't override a
hook* above — five pronoun-form lookups, `inflect_verb`'s and
`pluralize_pronoun`'s string matches, `is_subjective_plural`,
`is_first_person_subject`. Opening a channel the crate reads is categorically
different from opening one it only forwards.

**Breaking, and for whom:** not a signature break — a **stated-invariant**
break. Phase 4 item 4's "invalid subjects unrepresentable instead of
panicking" (Key Architecture Decisions row: *Stringly-typed `subject: &str` in
public API | ✅ Complete (v1.2)*) is exactly the property (b) reverses, and
`Noun::try_new`/`InvalidSubjectError` become decorative — an error type that
can no longer be returned for any label a fork might legitimately use. English
users see no compile error and no behavior change; they lose a guarantee. That
is the worst shape of break: invisible.

### (c) Keep `SubjectPronoun` English-only; forks own their pronoun set — recommended

Item 3 describes this as "most conservative, most plumbing". The finding of
this spike is that **the plumbing is already installed**, so the option is
conservative *without* being expensive:

| Item 3's stated deliverable | Status today |
|---|---|
| parallel fork-owned pronoun set | `inflect_pronoun_custom` + `PronounCase` (5 cases), v1.1 / v1.3 |
| consulted first by `handle_placeholder_impl` | yes — `src/lib.rs:479-553`, English is the `else` branch |
| a way for the fork to name its pronoun | `Ranting::subjective() -> &str`, uninterpreted at the hook boundary |
| verb agreement under a fork's pronoun | `inflect_verb_custom`/`_with_context`, consulted first (`src/lib.rs:305`) |
| article agreement | `inflect_article_custom` + `GrammaticalCase` (v1.3) + `NounClass` (v1.3) |
| carrying the pronoun on the entity | `#[ranting(subject = "…")]` / `subject = "$"`, unvalidated |

**Exhaustive-match safety: fully preserved.** `SubjectPronoun` stays closed,
its nine variants stay English, the three total matches and three tripwire
tests keep working, and the `unwrap_or(It)` degrade stays confined to the
already-documented "fork opted out" path.

**Breaking, and for whom: nobody.** Doc-only. No signature, no invariant, no
codegen changes.

**What (c) costs:** a fork cannot use `Noun` as the carrier for a non-English
pronoun and must declare its own `#[derive_ranting]` struct. Given that such
a fork needs its own `inflect_pronoun_custom` table anyway — i.e. it is
already writing an impl — this is a small, one-time cost, and it is what keeps
Phase 4 item 4's invariant intact for everyone else.

## T-V: resolved by collapsing it, not by adding an axis

Item 3 requires this spike to say whether the story-wide, already-inert
`NarrationContext.register` (`Formal`/`Neutral`/`Casual`) or a new
per-addressee channel owns the T-V distinction, or how they compose.

**Neither. T-V is a pronoun-inventory question, not a register question, and
it needs no new channel at all.**

The observation that collapses it: German `Sie` and French `vous` are not
modifiers applied to a pronoun — they *are* pronoun slots, borrowed from
elsewhere in the same inventory (3pl and 2pl respectively, reused as polite
2sg). Under option (c) the addressee's declared subject label already *is* the
T-V choice:

```rust
#[derive_ranting] #[ranting(subject = "du")]  struct Freund {}
#[derive_ranting] #[ranting(subject = "Sie")] struct Kunde {}
```

Both render through the same `inflect_pronoun_custom`/`inflect_verb_custom`
impl, in the same `say!()` call, with no context involved. This is exactly the
right granularity, because **T-V is per-addressee**: one scene routinely
addresses one character with `du` and another with `Sie`, which a story-wide
setting cannot express and a story-wide setting should not try to.

**Where `register` lands.** It stays story-wide and stays inert — no built-in
behavior reads it, unchanged from Phase 3 item 4. Its documented role for T-V
is a **default for the indifferent case only**, and the precedence rule is a
statement in this spec (and in `docs/EXTENSIBILITY.md`), not behavior in the
crate:

1. the addressee's own declared subject label wins, always;
2. a fork *may* consult `ctx.register` in `inflect_pronoun_custom_with_context`
   when its own entity model leaves address form unspecified (e.g. a generic
   "the merchant" with no declared politeness);
3. `register: None` means "no override in effect", identical to having no
   context — the same rule `Register`'s own doc comment already states.

`ranting` will not arbitrate between them, because arbitrating requires
knowing that `Sie` and `du` are the same person addressed two ways, which is
language knowledge that belongs in the fork.

**The boundary, stated in item 1's style.** A *true* T-V relation is
speaker×addressee, not a property of the addressee alone: A may address B
formally while B addresses A informally, and the same entity is `du` to one
speaker and `Sie` to another in the same story. Modelling that requires a
speaker channel threaded into inflection. `say!()` structurally has none — a
placeholder knows only its own noun, and `handle_placeholder_impl` has no
sibling or narrator reference (established in
`2026-08-13-word-order-feasibility.md`). `ask!(speaker, audience, template,
input)` has a speaker, but it is `heed!()`'s input-matching direction and the
speaker reaches `Answerable::answer(&self, speaker, captures)` only — verified
at `ranting_derive/src/lib.rs:340`, the single generated call site, and
`src/answerable.rs:30` — never an inflection hook. So the per-addressee
approximation above is the ceiling: **a fork that needs a relation matrix
must select the addressee entity (or its declared label) per speaker at the
call site, before `say!()` is reached.**

## Interaction with item 4 (number categories) — no dependency

Formal `Sie` takes plural verb agreement with singular reference. That looks
like it needs item 4's `as_plural: bool` widened, and it does not: the crate
already ships exactly this pattern as singular "they" (README: *"Singular
'they' conjugates as plural in form while referring to a single
individual"*), where `is_subjective_plural("they") == true` drives agreement
while reference stays singular. A fork's `Sie` sets `is_plural() -> true` and
gets `Sie sind` from the same mechanism — and gets it even without an
`inflect_verb_custom` override, because `inflect_verb`'s `_` arm (the one an
unrecognized label falls to) *is* the plural/uninflected form. **Item 3 does
not block on item 4**,
and item 4 should not treat T-V as one of its motivating cases. Genuine dual
and paucal remain item 4's problem.

## Recommendation

**(c), in its thin form: change nothing, document what already exists.**

Concretely, the follow-up work this spike authorizes is documentation only:

1. `docs/EXTENSIBILITY.md` — a section on non-English pronoun inventories:
   `subjective() -> &str` is an open, uninterpreted channel; the five
   `PronounCase` arms plus `inflect_verb_custom` are the fork's table; declare
   a `#[ranting(subject = "…")]` struct rather than reusing `Noun`; the T-V
   precedence rule (addressee label > `ctx.register` > nothing); and the
   `unwrap_or(It)` degrade as the documented consequence of leaving an arm
   unhandled.
2. ROADMAP.md Key Architecture Decisions — flip the `SubjectPronoun is a
   closed English enum` row to ✅ Locked with this spec as the pointer.
3. Item 10's German reference lexicon inherits two named constraints (below)
   and is where the claims in this document get exercised for real.

## What stays out of reach under this recommendation

Named by construction, in item 1's style:

- **A speaker×addressee T-V relation.** No speaker channel exists in
  `say!()`; `ask!()`'s speaker is not threaded into inflection. Callers pick
  the addressee representation before the macro.
- **`NarrationContext.narration_person` for a non-English first person.**
  `is_first_person_subject` is `matches!(subject, "I" | "we")`. A fork whose
  labels are `ich`/`wir` gets a silent no-op, and there is no hook to override
  it — `resolve_viewpoint` is `pub(crate)` and consulted before any trait
  method. This is the one place where option (c) genuinely leaves a fork
  without a workaround short of not using `say_with!()`'s viewpoint override.
  Worth raising as its own item if item 10 hits it; deliberately not fixed
  here, because fixing it is production code and this item is doc-only.
- **`Noun` as a carrier for a non-English pronoun.** By construction —
  Phase 4 item 4's invariant. Forks declare their own struct.
- **Any in-crate arbitration between `register` and a declared T-V label.**
  Requires knowing two labels denote the same referent addressed differently;
  that is language knowledge, and it stays in the fork.
- **Compile-time rejection of a nonsense subject label.** The derive macro
  does not validate literal `subject` attributes against `is_subject`, which
  is precisely what makes (c) work for forks — and equally means an English
  user's `#[ranting(subject = "he,")]` typo degrades to `it`'s forms at
  runtime instead of failing the build. Tightening it would break (c);
  documenting it is the trade.

## Rejected alternatives, recorded

- **(a) extend the enum** — rejected: puts non-English vocabulary in
  `ranting_core::grammar` and English forms in `src/language/english.rs` for
  words that have none; unbounded; and semver-major for every downstream
  exhaustive `match` (`SubjectPronoun` is re-exported and not
  `#[non_exhaustive]`).
- **(b) open pronoun channel** — rejected: trades a compile error for wrong
  output at five `unwrap_or(It)` sites plus two silent `false` answers, and
  reverses Phase 4 item 4's stated invariant invisibly. The `NounClass`
  precedent does not transfer, because the crate *reads* the subject label and
  never reads a noun class.
- **A per-addressee `Politeness`/`Honorific` channel alongside `subject`** —
  rejected as redundant: `Sie`/`vous` are pronoun slots, so the label carries
  the distinction already, and a second channel would need in-crate
  arbitration rules against the first.
- **Making `register` drive T-V in-crate** — rejected on granularity: a
  story-wide setting cannot express two addressees of different formality in
  one sentence, which is the common case, not the edge case.
- **Adding `#[non_exhaustive]` to `SubjectPronoun` pre-emptively** — rejected
  as unmotivated under (c): it costs a major version and weakens the in-crate
  exhaustiveness guarantee to buy flexibility only option (a) would use.

## Open questions for whoever picks this up next

- Should `is_first_person_subject` become a `Ranting` hook (defaulting to
  today's `matches!("I" | "we")`)? It is the one named gap with no fork-side
  workaround. Small, additive, English-preserving — a natural Phase 6 item 9
  or v1.3.x follow-up, but production code and therefore out of this item's
  scope.
- Does item 10's German lexicon want `du`/`Sie` in its worked example? It
  would be the first end-to-end exercise of the T-V precedence rule stated
  here, and the cheapest possible validation of this spike's central claim.
- Should `docs/EXTENSIBILITY.md` show a full non-English `Ranting` impl
  (subject label + five `PronounCase` arms + `inflect_verb_custom`), or point
  at `tests/ranting/grammatical_case.rs` and `noun_class.rs` and let those
  carry it? The former is the better teaching artifact; the latter cannot
  drift, since it compiles.

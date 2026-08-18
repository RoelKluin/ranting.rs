# Design spike: `ranting-if` (or similar) companion crate — Inform7-style object disambiguation

**Status**: design spike, PROPOSED only — **no code in this repository is changed by this
document**. ROADMAP.md Phase 9 item 5, citing *Post-v1.2: Future Directions*' "`ranting-if` (or
similar) Companion Crate" bullet, itself citing "proposed 2026-08-13, not scoped." This is the
least-scoped item on the Phase 9 list; the item's own text asks four questions rather than naming
a candidate shape, so this spike answers them in order: (a) a minimal first slice worth building,
(b) which dependency shape it needs (falsifier-style or dev-tool-style), (c) how it integrates
with an existing `ask!()`/`Answerable` call site, and (d) what a first version excludes.

## Background: what the item is reacting to

Interactive fiction (IF) parsers routinely receive free text like `"take sword"` where more than
one in-scope object could answer to "sword" — a rusty sword and a silver sword, say. Inform 7's
`Understand` rulebook resolves this by collecting every object whose grammar could match the
noun phrase, then picking among them with likelihood rules ("things preferred are more likely",
`Understand "sword" as the silver sword` outranking a same-named generic entry, and so on) —
falling back to asking the player to disambiguate only when the rules leave a genuine tie. That
mechanism — a *candidate registry* plus a *scoring pass* plus *author-written preference rules* —
has no equivalent anywhere in `ranting`. `ask!()` (Phase 5, `.claude/rules/heed-input-parsing.md`)
parses free text into captures and hands them to exactly one statically-known `Answerable`; it was
never designed to pick *which* object a capture like `{item}` refers to among several live
candidates. That's the gap this item names.

## (a) A minimal first slice

The full feature — arbitrary likelihood rules, scope reasoning (what's in the room, what's held,
what's visible), synonym grammar, disambiguation dialogue ("which sword do you mean, the rusty
sword or the silver sword?") — is a small IF parser in its own right. Inform 7's own `Understand`
implementation is one of the largest subsystems in its standard library. Scoping all of that in one
spike would produce a design nobody could evaluate in one sitting, which is exactly what the
ROADMAP item is flagging by calling this "largest and least-scoped." The question worth answering
first is not "what's the whole feature" but "what's the smallest slice that (1) does something an
IF author cannot already do with `ask!()` alone, and (2) is small enough to falsify or confirm
before committing further."

**Proposed first slice: flat candidate list + attribute-match scoring, no scope, no dialogue.**

1. **A candidate registry** — a `Vec<T>` (or any `IntoIterator<Item = &T>`) of objects the caller
   already has in hand; not a world model, not scope-aware, not queried by the crate. The caller
   decides what's a candidate for a given disambiguation the same way an `ask!()` caller already
   decides which `audience` to pass — by handing over a concrete collection.
2. **A scoring mechanism**, minimally: each candidate exposes a small set of matchable attributes
   (name, and a caller-supplied list of adjectives/synonyms — `Vec<&str>`, not a lexicon `ranting`
   understands), and the input's parsed noun-phrase tokens are matched against those attributes.
   Score = count of matched tokens, ties broken by "more specific wins" (a candidate matching
   *all* the input's descriptive words outranks one matching only the head noun) — this is the one
   piece of "likelihood" logic Inform 7's own examples lean on most, and it requires no
   world-modeling to implement. No numeric weight-tuning DSL in v1: a rule either matches a token
   or it doesn't, and match count is the whole scoring function.
3. **A rule-authoring surface that is a trait, not a new grammar.** Rather than inventing a
   sigil-based rule language (which would duplicate `ranting`'s own placeholder grammar for a
   different purpose — the project's stated design philosophy, per
   `.claude/rules/heed-input-parsing.md`'s "no tokenizer hook exists or will" precedent, cuts hard
   against a second parsed mini-language living in a sibling crate), a first version exposes
   disambiguation as an ordinary trait method, e.g. conceptually:

   ```
   trait Disambiguable {
       fn match_tokens(&self, tokens: &[&str]) -> u32; // score, 0 = no match
   }
   ```

   with a free function `disambiguate<'a, T: Disambiguable>(candidates: &'a [T], tokens: &[&str])
   -> DisambiguationResult<'a, T>` returning either a unique winner, a tie (the candidates that
   scored equally), or no match at all. "Rule authoring" in v1 *is* implementing
   `match_tokens` by hand per type — exactly as `Answerable::answer` today is "authoring" by hand,
   not by a rule DSL. This keeps the crate's first slice a library of plumbing around a
   caller-written function, not an interpreter.

This slice deliberately excludes anything Inform 7 calls scope, "likely" vs. "unlikely" graded
preference (only match-count ties), and multi-turn disambiguation dialogue. It is small enough to
be falsified quickly: either the match-count-with-specificity-tiebreak scoring is enough to
resolve realistic two-candidate cases correctly, or it demonstrably isn't and a real weighting
mechanism is needed — that finding is worth having before investing in rule syntax.

## (b) Falsifier-shaped or dev-tool-shaped?

**Dev-tool-shaped: it should depend on `ranting` and, if useful, `ranting_core` — like
`ranting_gaps`, not like `ranting_i18n`/`ranting_es`/`ranting_ar`/`ranting_ja`.** Three reasons,
by analogy to the two dependency patterns already in this repo (`.claude/rules/crate-layout.md`):

1. **The falsifier contract exists to answer one specific question — "is the public API enough
   signal for a non-English grammar?"** — and disambiguation is orthogonal to that question
   entirely. A disambiguation crate doesn't inflect anything in a new language; it resolves *which*
   noun a capture refers to before any `Ranting`/`say!()` call happens. Forcing it into the
   falsifier shape (depend on `ranting` alone, prove sufficiency) would be answering a question
   this crate was never designed to ask.
2. It is squarely the same shape as `ranting_gaps`/`ranting_es_gaps`: a tool that consumes
   `ranting`'s public surface (`Answerable`, `Ranting`, and `heed!()`'s capture-parsing machinery)
   from outside, to do a job the core crates don't do themselves. `ranting_gaps` depends on
   `ranting_core` specifically to reuse `ph_ext::parse` as an authoritative oracle for the closed
   pre-noun vocabulary rather than re-deriving it. If disambiguation ever needs to tokenize a noun
   phrase the same way `ranting`'s own placeholder grammar does (rather than a naive
   whitespace split, which the caller can supply itself for v1 — see (d)), the same argument
   applies: reuse `ranting_core::ph_ext`/`grammar` rather than hand-duplicating tokenization rules,
   exactly as `ranting_gaps` already does for a different purpose.
3. **It does not need to be a falsifier to be useful**, and forcing it to falsifier shape would
   cost real capability for no benefit: nothing about IF disambiguation is a claim about
   `ranting`'s language-signal sufficiency, so there's no finding to protect by keeping it to
   `ranting` alone.

Concretely, for the minimal slice in (a), a `ranting_core` dependency is not yet load-bearing —
whitespace-split tokens and a caller-supplied attribute list need nothing from `ranting_core`. The
recommendation is to start with `ranting` alone (simplest correct default) and add `ranting_core`
only if and when a concrete need appears (richer tokenization, reusing `PH_START`'s sentence
boundaries, etc.) — the same "add it when it's the finding" discipline `ranting_gaps` itself
documents. Either way, it is **not** bound by the falsifier contract and gaining a `ranting_core`
dependency later would not be a regression.

## (c) Integration with an existing `ask!()`/`Answerable` call site

`ask!(speaker, audience, template, input)` is deliberately one statically-known `audience` per call
site (`ranting_derive/src/lib.rs`'s `Ask` struct takes a single `audience: Expr`) — `.claude/rules/
heed-input-parsing.md`'s `ask!()` section and `src/answerable.rs`'s own doc comment both describe
this as fixed per implementor, "one audience answers questions of one arity." Disambiguation
doesn't need to — and under this proposal, does not — change that signature. Instead it runs
**before** `ask!()`, resolving which concrete value plays the role of `audience`, then hands that
value to an unmodified `ask!()` call exactly as today.

Using `tests/ranting/ask.rs`'s own fixtures as the worked example: suppose a caller has two
`Villager`-shaped NPCs in scope — a blacksmith and a farmer — and the player types `"ask
blacksmith about bone"`. Today's `ask!()` alone has no way to route to the right `Villager`
instance; the caller must already know which one to pass as `audience`. The proposed crate closes
exactly that gap:

```
// caller-side sketch, not a change to ranting/ranting_derive:
let candidates = vec![&blacksmith, &farmer]; // ordinary Vec<&Villager>, caller-owned
let tokens: Vec<&str> = input.split_whitespace().collect(); // e.g. from "blacksmith"
match ranting_if::disambiguate(&candidates, &tokens) {
    DisambiguationResult::Unique(villager) => {
        // now an ordinary, unmodified ask!() call:
        ask!(player, *villager, "about {topic}", input)
    }
    DisambiguationResult::Tie(_) => Some("Which one do you mean?".to_string()),
    DisambiguationResult::NoMatch => None,
}
```

Two things about this shape are load-bearing, not incidental:

- **`ask!()`'s template compiler and `Answerable` trait are untouched.** Disambiguation resolves
  the *audience value*, never the *template* or the *captures* — those stay exactly `heed!()`'s
  existing grammar, matched once, against the winning candidate. This is the same reason
  `elide_article_custom` runs at a separate, later splice point rather than folding into
  `inflect_article_custom` (`.claude/rules/extension-hooks.md`): a new concern gets its own step
  in the pipeline instead of widening an existing signature that other call sites already depend
  on being stable.
- **The candidate list and the `ask!()` audience share a type, but disambiguation only ever sees
  the type's `Disambiguable` half** (name/attribute matching), never its `Answerable` half
  (captures/answer logic) — the two traits are independent and a type can implement either, both,
  or neither. This mirrors how `Ranting` and `Answerable` are already two separate traits on the
  same `Villager` in `tests/ranting/ask.rs`, not one merged interface.

A second integration shape is also worth naming for completeness: disambiguation over the
*captured noun phrase itself*, e.g. `ask!(player, shop, "buy {item}", input)` where `{item}`
captures `"sword"` and the shop's inventory (not the `ask!()` audience) is what needs
disambiguating. That case resolves entirely inside `Shop::answer(&self, speaker, item: String)` —
the shop already receives the raw captured string and is free to call `ranting_if::disambiguate`
on its own inventory before formatting a response. No `ranting`/`ranting_derive` change is implied
by this shape either; it's ordinary caller code inside an existing `Answerable::answer` body.

## (d) Explicitly out of scope for a first version

- **Scope / world-model reasoning** ("is this object visible, held, in the same room"). The crate
  takes a candidate list as a parameter; it never discovers candidates itself. Building scope
  reasoning would mean building a room/inventory model, which has no relationship to `ranting`'s
  job of grammatical rendering and belongs, if anywhere, in an IF engine the crate could plug into
  later — not in the crate itself.
- **A rule-authoring DSL / new sigil grammar.** As argued in (a), v1 rule authoring is "implement
  a trait method," not a parsed mini-language. Introducing one would duplicate the design cost of
  `ranting`'s own placeholder grammar for an unrelated purpose, and nothing here requires it yet.
- **Graded likelihood weights** (Inform 7's "likely"/"unlikely"/numeric `Understand` priorities).
  v1 is match-count plus a specificity tiebreak only. Whether real disambiguation cases need finer
  grading than that is exactly the open question the minimal slice is meant to surface before more
  mechanism is built.
- **Multi-turn disambiguation dialogue** ("which one, the rusty sword or the silver sword?" →
  reprompt → re-resolve against the narrowed set). v1's `Tie` result hands the tied candidates back
  to the caller; turning that into a follow-up question-and-reparse loop is caller-side IF-engine
  logic, not something this crate should own, at least not before the single-shot case is proven
  out.
- **Synonym/vocabulary tables** (Inform 7's `Understand "blade" as "sword"`). v1 matches tokens
  against whatever attribute strings the caller already put on each candidate; it does not ship a
  synonym dictionary or stemmer. A caller wanting synonyms lists them as additional attributes
  themselves.
- **A `ranting_core`-derived tokenizer.** As covered in (b), v1 tokenizes with a caller-supplied
  `&[&str]` (in practice usually `str::split_whitespace` on `ask!()`'s already-captured phrase);
  reusing `ranting_core::ph_ext`/`grammar` for a more linguistically aware split is deferred until
  a concrete case motivates it, not built speculatively.
- **Any change to `ranting`, `ranting_core`, or `ranting_derive`.** Every integration point in (c)
  is caller-side code around an unmodified `ask!()`/`Answerable`. If a future iteration finds that
  insufficient — e.g. wanting `ask!()` itself to accept a candidate list and disambiguate inline —
  that would be a new, separately-scoped ROADMAP item, not part of this crate's first version.

## Summary

| Question | Answer |
|---|---|
| (a) Minimal first slice | Flat caller-supplied candidate list + match-count/specificity-tiebreak scoring + trait-based (not DSL-based) rule authoring — no scope, no dialogue, no weighting DSL |
| (b) Dependency shape | Dev-tool-shaped, like `ranting_gaps`/`ranting_es_gaps` — depends on `ranting` (and optionally `ranting_core` later, if a concrete tokenization need appears), **not** bound by the falsifier contract |
| (c) `ask!()` integration | Runs disambiguation *before* an unmodified `ask!()` call to resolve which concrete value plays `audience`; alternatively, resolves inventory-style ambiguity inside an existing `Answerable::answer` body over an already-captured string. Either way, `ask!()`'s one-audience-per-call-site design is unchanged |
| (d) Out of scope | Scope/world-model reasoning, a rule DSL, graded likelihood weights, multi-turn dialogue, synonym tables, a `ranting_core` tokenizer, and any change to the three core crates |

This is a scoping spike, not an implementation plan: it narrows "largest and least-scoped item on
the Phase 9 list" down to a slice small enough to build and falsify in one pass, and leaves the
maintainer to decide whether that slice is worth a new crate at all, or whether disambiguation
belongs outside this project's boundary entirely (an IF engine's own concern, with `ranting` and
`ask!()` as one dependency among several). Nothing here commits to either answer.

## What this spike does not do

- Does not create `ranting_if/`, any `.rs` file, or any `Cargo.toml`.
- Does not add, rename, or resignature any method on `Ranting`, `Answerable`, or any macro in
  `ranting_derive`.
- Does not touch `src/answerable.rs`, `ranting_derive/src/lib.rs`, `ranting_derive/src/heed.rs`, or
  any existing test.
- Does not decide whether this item should be built at all, only what a minimal first version would
  contain if the maintainer chooses to build it.

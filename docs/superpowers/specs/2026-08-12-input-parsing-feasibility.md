# Feasibility: a `scanf`-like input-parsing macro for Ranting

**Status**: feasibility exploration with v1 scope decided for feature A
(`heed!()`); feature B (`unsay!()`) explicitly not pursued. This is not yet
an implementation plan — decided *what* to build, not yet *how* in enough
detail to hand to `writing-plans`.

## Motivation

`say!()`/`ask!()` turn (template + values) into text. The requested feature
is roughly the inverse: (template + text) into values, in the spirit of
C's `scanf`, using the same placeholder syntax the crate already has.

That "inverse" framing hides two genuinely different problems with very
different feasibility profiles. This doc treats them separately, as agreed
before writing: **(A) command-parser matching** — extracting free-text
spans from user input against an expected phrase shape — and **(B) full
grammatical round-trip** — recovering the original values behind text that
`say!()`/`ask!()` itself produced, including reversing tense, pronoun
person, and pluralization. (A) is the practical, tractable feature. (B) is
a research-flavored stretch goal with real, unavoidable ambiguity.

## A. Command-parser matching (recommended scope)

**What it does**: match input text against a template containing bare
named captures and literal words, the way `"take {item}"` matched against
`"take sword"` yields `item = "sword"`. No grammar inversion involved —
only splitting text at literal boundaries.

### Mechanism

The pieces already exist to build this cheaply:

- `PH_START`/`PH_EXT` (`src/language/english_shared.rs`) already parse a
  placeholder template at compile time for `say!()`'s codegen. A parser for
  the *matching* macro can reuse the same "walk the literal text, find
  `{...}` spans" approach, but emit a compiled `regex::Regex` instead of a
  `format!()` call — `regex` is already a dependency (`PH_START` itself is
  one), so no new crate is needed for this half.
- Each bare `{name}` compiles to a named capture group. Literal text
  between placeholders compiles to `regex::escape()`d literal segments
  joined by `\s+`.
- At runtime, `.captures(input)` either returns `None` (no match) or a set
  of named strings — mirrors `sscanf`'s "returns number of fields matched,
  or `EOF`" without needing raw pointers, since Rust can just return
  `Option<(String, ...)>` or a generated struct.

### The one real design problem: capture width

`scanf`'s `%s` stops at whitespace; that's also the natural default here —
`{item}` compiles to `(\S+)`, one token. But natural-language templates
routinely need multi-word captures (`"take {item}"` matching `"take rusty
sword"`), and free-text captures are ambiguous by construction: given
`"give {item} to {target}"` and input `"give old rusty key to the
guard"`, there is no correct unassisted split of `item` vs `target` — regex
backtracking will find *a* match, not necessarily the *intended* one.

**Decided**: single-token capture by default (`\S+`); `{item...}` is the
opt-in "capture until the next literal or end of input" marker (a
`scanf`-style `%[^\n]` equivalent). Trailing-ellipsis was chosen over a
leading `*item` specifically to avoid colliding with `say!()`'s existing
`*` marker (which already means something else there) — the two macros'
grammars will appear side by side in docs, so a shared character with two
meanings was worth avoiding even though the macros are never used in the
same template. Multi-capture ambiguity (two adjacent open captures with no
literal between them) is a **compile-time error**, not a silent
greedy/lazy guess — this is knowable statically from the template alone,
the same way `say!()` already catches syntax errors at compile time (a
stated design goal per README: "Compile-time parsing... Catches syntax
errors early").

### Numeric captures (`#name`, `$name`)

`$name` (raw number) is trivial — `(\d+)` plus `str::parse`.

`#name` (number-as-words, e.g. "two") needs the reverse of
`english_numbers::convert_no_fmt` (the crate currently used for the
forward direction, see `src/lib.rs`'s `rant_convert_numbers`). That crate
appears to be format-only; a words→number direction would need either a
words-to-number crate (a few exist) or a hand-rolled parser bounded to
whatever range the forward direction actually supports.

**Decided**: out of v1. `{name}`/`{$name}` ship first; `{#name}` is a
fast-follow once a words-to-number dependency is picked (or a bounded
hand-rolled parser is written) — no need to block v1 on that decision.

### Explicitly out of scope for this feature

Grammar-aware markers — `=`/`@`/`` ` ``/`~` (pronoun case), articles,
tense markers — are not part of command-parser matching. A template like
`"{=who} take{s who} {item}"` mixes a literal capture (`item`) with
grammar that only makes sense on the *output* side. Matching input against
grammar markers is squarely feature B's problem, not this one's. The MVP
grammar for this macro should probably be a deliberately smaller subset of
the placeholder syntax than `say!()`'s: literal text, bare captures,
numeric captures, and nothing else — see "Naming" below for why keeping it
a visibly different (smaller) macro matters.

### Return shape — v1 tuple now, v2 derive-struct later

**v1 (decided)**: a plain expression macro, positional the same way
`say!()` is positional today ("Positional arguments only", per README) —
no new item-level codegen, just `(template_str, input_expr)` in,
`Option<...>` out:

```rust
let input = "give rusty sword to the guard";
if let Some((item, target)) = heed!("give {item...} to {target...}", input) {
    println!("item={item}, target={target}");
}
```

A single capture returns bare `Option<String>` (not a 1-tuple). This is
the whole v1 implementation surface: parse the template, compile a regex,
run it, hand back the captures. Ships fastest; the risk is that at 4+
captures a positional tuple stops being self-documenting and field
transposition (`item`/`target` swapped) is a silent bug, not a compile
error.

**v2 (decided, deferred)**: once the matching engine underneath v1 is
proven, add a derive macro mirroring the crate's existing
`#[derive_ranting]` precedent on the *output* side — a symmetric
`#[derive(Heed)]` on the *input* side:

```rust
#[derive(Heed)]
#[heed(template = "give {item...} to {target...}")]
struct GiveCommand {
    item: String,
    target: String,
}

let input = "give rusty sword to the guard";
if let Some(cmd) = GiveCommand::heed(input) {
    println!("item={}, target={}", cmd.item, cmd.target);
}
```

The derive reads the `#[heed(template = "...")]` attribute, cross-checks
each `{name}`/`{name...}` placeholder against a same-named struct field
(compile-time error on a mismatch either direction — same "catch it early"
philosophy as the rest of the crate), and generates an associated
`fn heed(input: &str) -> Option<Self>`. Call sites become self-documenting
(`cmd.item`, not `.0`) and field-name typos become compile errors instead
of silent tuple-position bugs — but this is a real derive macro (attribute
parsing, field↔placeholder cross-checking, associated-function codegen),
comparable in scope to `#[derive_ranting]` itself, not a small addition to
v1. Building v1's matcher as a reusable internal function (template →
compiled matcher → captures) rather than inlining it into the `heed!()`
expression macro's codegen is worth doing from the start, specifically so
v2's derive can call the same core logic instead of duplicating it.

## B. Full grammatical round-trip (`say!()` inverse)

**What it does**: given text that `say!()`/`ask!()` produced (or text
written to match their grammar), recover the values — including reversing
conjugated verbs, inflected pronouns, and pluralized nouns back to base
forms and a reconstructed `Noun`.

This is not one problem; it's a handful of sub-problems with very
different difficulty:

| Sub-problem | Difficulty | Why |
|---|---|---|
| Irregular noun singular↔plural | **Easy** | `IRREGULAR_SINGULARS`/`IRREGULAR_PLURALS` (`src/language/plurals.rs`) are already bidirectional lookup tables; `inflect_noun_irregular` already supports both directions. |
| Irregular verb past→base | **Easy** | `IRREGULAR_PAST`/`IRREGULAR_PAST_PARTICIPLE` are `(base, inflected)` pairs; a reverse index is a one-time `HashMap` build. |
| Auxiliary → tense marker | **Easy** | `conjugate_auxiliary` (`src/language/auxiliary.rs`) matches literal strings ("am"/"is"/"are", "was"/"were", "has"/"have", "had", "will"); the reverse (aux word → tense marker) is a small, exhaustive, already-enumerable match. |
| Regular verb inflected→base | **Medium** | `regular_past_form`/continuous-form rules (`src/language/verb_conjugate.rs`) are lossy in the reverse direction in edge cases (e.g. consonant-doubling for `-ing` forms: "running" → "run" needs to know whether to undouble, "reading" → "read" doesn't) — solvable, but needs its own reverse rule set and test suite, not a free inversion of the forward rules. |
| Pronoun subjective/objective/possessive → declared `subject` | **Ambiguous** | `SubjectPronoun::forms()` (`src/language/english.rs`) is a clean forward lookup, but the reverse is not injective in general: "they" alone cannot tell you whether the noun was declared `subject = "they"`, or was `he`/`she`/`it` pluralized via `{+noun}`, or (after this session's viewpoint feature) an `I`-declared narrator rendered through `Person::Third`. Multiple distinct original states collapse to the same surface text — this is a fundamental property of the forward grammar, not an implementation gap. |
| Custom `inflect_*_custom` hooks | **Not generally invertible** | These are arbitrary user-supplied closures (the whole point of the v1.1 extensibility work — see `docs/EXTENSIBILITY.md`). A pirate-speak or Scottish-dialect fork's custom pronoun/verb logic has no obligation to be a bijection, and the crate has no way to require one. |
| Noun *name* itself | **Not recoverable in general** | `{noun}` just calls `Display`/`Debug` (or `noun.inflect()`); free-text names with internal spaces (e.g. "Old man" from the README's own example) have no marked boundary in output text distinguishing the name from surrounding literal words, without the same delimiter-ambiguity problem as feature A, compounded by not knowing the name's *length* in tokens ahead of time. |

**Conclusion for B**: a *complete, always-correct* inverse of `say!()` is
not achievable — the forward grammar is deliberately lossy in several
places (that's a feature, not a bug: `say!()` exists to produce natural
prose, not a serialization format). A **best-effort, explicitly partial**
inverse is feasible for the "Easy"/"Medium" rows above, but any design
must document — prominently, in the same place users read about it, not
buried in a caveat — that pronoun-person and custom-hook reversal are
fundamentally ambiguous or unsupported, not "not implemented yet."

If this is pursued, recommend scoping v1 to verb tense/form reversal only
(the Easy/Medium rows), returning the *conjugated string as typed*
alongside a best-guess base form and tense, rather than promising a full
reconstructed value set. Pronoun/subject reversal should probably remain
permanently out of scope, documented as such, rather than "TODO."

## Naming

Whatever ships should read as clearly related to `say!()`/`ack!()`/
`nay!()`/`ask!()` without implying more power than it has — a name that
sounds like a full inverse of `say!()` would overpromise if only feature A
ships.

- **`heed!()`** (feature A, command-parser matching) — "pay heed to what
  was said," fits the existing terse, thematic naming (`say`, `ack`,
  `nay`, `ask`), and doesn't claim to be an inverse of anything.
- **`unsay!()`** (feature B, if ever pursued) — reads unambiguously as
  "the inverse of `say!()`," which is exactly the (partial, lossy)
  guarantee it would actually offer — the name itself is a hedge against
  overpromising, since "unsay" evokes "can't fully take it back" as much
  as it does "reverse."
- Ruled out: `scanf!()`/`sscanf!()` verbatim — reads as a promise of
  C-`scanf`-equivalent behavior (fixed-width format specifiers, strict
  field types), which this is not; `parse!()` — too generic, likely to
  collide with user expectations around `std::str::FromStr`.

## Recommendation

Feature A (`heed!()`) is a well-scoped, implementable feature using
mechanisms already in the codebase (the `regex` dependency, the existing
compile-time placeholder-parsing pattern from `ranting_derive`). Scope is
now decided:

- **v1**: expression macro `heed!(template, input) -> Option<...>`,
  positional tuple/bare-value return, single-token captures by default,
  `{item...}` for greedy multi-word captures, adjacent-open-capture
  ambiguity is a compile-time error, `{name}`/`{$name}` only (no `{#name}`
  word-numbers yet).
- **v2** (deferred, not scoped in detail yet): `#[derive(Heed)]` +
  `#[heed(template = "...")]` on a user struct, generating
  `fn heed(input: &str) -> Option<Self>`, built on top of v1's matching
  engine rather than duplicating it. `{#name}` word-number captures are
  also a candidate fast-follow, independent of v2's timing.

Feature B (`unsay!()`) is not recommended for v1.1. The ambiguity in the
pronoun/subject and custom-hook rows isn't an engineering gap that more
time closes — it's inherent to what `say!()` is for. If there's a concrete
use case that only needs the Easy/Medium rows (verb tense recovery), that
narrower feature could be scoped on its own later, but "invert `say!()`"
as stated is not a buildable spec.

## Open questions for whoever picks this up next

1. Is feature A worth a full spec now, or does it wait behind the other
   v1.1 items already in flight (reflexive forms, comparative/superlative,
   recursive type inflection)?
2. ~~Multi-word capture syntax~~ — resolved: `{item...}`, compile-time
   error on ambiguity.
3. ~~Numeric captures in v1~~ — resolved: `{name}`/`{$name}` only;
   `{#name}` deferred.
4. ~~Return shape~~ — resolved: positional tuple for v1;
   `#[derive(Heed)]` named-struct for v2, once v1's matcher is proven.

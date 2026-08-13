# Feasibility: word order and template slots for non-English `Ranting`

**Status**: design spike complete; conclusion is **don't build this** —
option (a), per-language template sets, recommended and documented as a
**permanent boundary** of the crate. No production code is proposed by this
document. ROADMAP.md Phase 6 item 1.

This is the second spike in the crate to conclude "the honest answer is
narrower than the ask"; `2026-08-12-input-parsing-feasibility.md` (feature B,
`unsay!()`) is the precedent.

## Motivation

The 2026-08-13 architecture review's German spike
(`docs/architecture-review-2026-08-13.md` §7) found and then closed one half
of the `ranting-i18n` gap: `inflect_article_custom` could not tell a subject
placeholder from an object placeholder, so `der`/`den` were indistinguishable.
That was fixed by threading `GrammaticalCase` (commit `11d531ed`).

The unclosed half is word order. In

```rust
say!("{=dog} {dog bark} at {@cat}")
```

the strings `" "`, `" at "` and the *relative positions* of the three
placeholders are English SVO, and they live in the literal template, not in
any placeholder. German verb-second with a clause-final element, Japanese SOV,
or Irish VSO cannot be reached from this template by any per-noun inflection
hook — **no hook can move text it does not own**. This document establishes
precisely what "does not own" means at the code level, scores the three
options ROADMAP.md item 1 lists, and states what stays impossible.

## What `handle_placeholder_impl` can and cannot reach

Everything below is a property of code as it stands today
(`src/lib.rs::handle_placeholder_impl`, `ranting_derive/src/lib.rs`'s `Say`
codegen), not a projection.

### The seam is `format!(lit, args…)`

`impl ToTokens for Say` (`ranting_derive/src/lib.rs`) emits exactly one
`format!()` call: a single string literal followed by comma-separated
argument expressions. The literal carries every character of inter-placeholder
text; each placeholder became one `{}` in that literal plus one
`handle_placeholder(noun, poss, nr, uc, spec)` argument expression.

Consequences, each of which independently blocks runtime reordering:

1. **The literal is frozen at compile time.** This is the
   "Compile-time parsing + runtime inflection" row of ROADMAP.md's
   architecture-decisions table, marked ✅ **Locked**. Inter-placeholder text
   is not data any runtime code receives; it is baked into the binary as a
   format string.
2. **Argument position is frozen with it.** `format!`'s Nth `{}` consumes the
   Nth argument. A permutation applied at runtime would have to permute the
   format string's holes, which is precisely the thing that is not a runtime
   value.
3. **A hook returns a `String` for one hole.** `handle_placeholder_impl`'s
   parameters are `(&R, poss: String, nr: String, uc: bool,
   spec: PlaceholderSpec, ctx: Option<&NarrationContext>)`. There is no
   reference to sibling placeholders, no reference to the surrounding literal,
   and no return channel other than the substituted string. A hook that wanted
   to emit "nothing here, and this word 40 characters later instead" has no
   expression for either half of that.

### Intra-placeholder order is hard-coded too

This is the sharper finding, and it is easy to miss: the limitation is not
only *between* placeholders. Inside `handle_placeholder_impl` the output is
assembled by a fixed sequence of `res.push_str` calls:

```text
pre (article, or pre-verb, plus chained article)   →
nr (the #var/$var count, unless `?`-hidden)        →
noun_space + the case-selected pronoun or name     →
post (possessive `'s`, post-verb, tense, degree)
```

A `_custom` hook is called *within* one of those steps and returns the text
for that step. It cannot swap two steps. So even a language whose reordering
need is entirely inside a single placeholder is out of reach — a **suffixed
definite article** (Romanian *câinele*, Norwegian *hunden*, Bulgarian
*кучето*) needs article-after-noun, and the article hook is invoked strictly
before the noun is pushed. The best a fork can do is return `""` from the
article hook and splice the article into the noun's own `Display`/`inflect()`
output, which fuses two grammatical decisions into one and loses the
`GrammaticalCase` threading that Phase 6 just added.

Option (b) therefore does not merely need cross-placeholder reordering; it
needs `handle_placeholder_impl`'s internal assembly order to become data too.

### `uc` is a compile-time positional decision

`uc` (uppercase-first) is decided by the macro from the placeholder's position
in the literal — "a placeholder at sentence start is uppercase by default"
(README) — and inside `handle_placeholder_impl` it is set to `false` after the
first non-empty emission, so only the first rendered fragment can be
capitalized. If slots were permuted at runtime, *which placeholder is
sentence-initial* would stop being statically knowable, and `uc`'s value would
be computed for the wrong slot. Any reordering design silently takes Phase 6
item 6 (orthography/capitalization hook) as a hard prerequisite, and needs
more from it than item 6 currently scopes: not just "how to capitalize," but
"who is first."

### One verb per placeholder, before or after — never both

`handle_placeholder_impl` contains

```rust
assert!(matches!(post_spec, PostSpec::None), "verb before and after?");
```

on the pre-verb path, and the pre/post verb split is the whole shape of the
grammar (`PostSpec::Verb`/`Tense`/`Degree` vs. a pre-noun verb, with
`singular_post_verb` derived from a `\bof\s+$` regex on the pre text). A
single verb that must surface in **two** positions in one clause — German
verb-second plus a clause-final separable prefix (*Ich sehe den Hund **an***)
or a clause-final participle (*Ich habe den Hund **gesehen***) — has no
representation in the placeholder grammar at any option level. This is not a
missing hook; it is a missing concept.

### What a hook *can* reach

For fairness, the ceiling is real and non-trivial: within its own braces a
hook fully controls article form, pronoun case, verb agreement and tense
word, number rendering, and the noun's own surface form, with
`GrammaticalCase`, `as_plural`, `uc` and (Phase 6 item 2) noun class as
signals. That is enough for **morphology**. It is categorically not enough for
**syntax**, and no amount of additional signal changes that, because signal is
not the binding constraint — text ownership is.

## Options, scored

| Option | Buildable? | Cost in `ranting` | What it actually buys | Verdict |
|---|---|---|---|---|
| **(a) Per-language template sets** — caller picks the template string by language; `ranting` inflects within it | **Yes, today** — needs zero crate changes | None | Full word-order freedom, at the cost of one template per language per sentence, authored by the caller | **Recommended** |
| **(b) Numbered slots + per-language reorder metadata** — templates declare roles (`{subj}`/`{verb}`/`{obj}`), a language module supplies a permutation | **No** at the current seam; only by replacing `format!()` codegen with a runtime assembler *and* making `handle_placeholder_impl`'s internal push order data | Very high, and breaks a ✅ Locked decision | Clause-level permutation for simple SVO↔SOV↔VSO cases; still no clause-final separable prefix, no suffixed article without further work | **Rejected — blocked by mechanism** |
| **(c) Syntax-tree API** — `sentence!(subject, verb, object)` rendered by a language module, no literal template | **Yes** — it sidesteps every limitation above by never having inter-placeholder literal text to begin with | Very high, and it is a second, parallel product surface | Genuine per-language syntax, including the constructions (b) can't reach | **Rejected — blocked by identity, not by feasibility** |

### (a) Per-language template sets — recommended

The caller holds a template per language and selects it before calling
`say!()`/`say_with!()`; `ranting` does what it already does well, inflecting
within the chosen template. Everything Phase 6 items 2 and 5–8 add — noun
class, adjective agreement, orthography, elision, numerals — composes with
this unchanged, because those are all intra-placeholder morphology and (a)
changes nothing about placeholders.

Cost: nothing in the crate. Cost to the caller: word order becomes a
per-language authoring task, i.e. the caller ends up maintaining something
shaped like a message catalogue. That is a real cost and it should be stated
in the same breath as the recommendation, not buried — see "What stays
impossible" below.

Score: **buildable today, zero crate cost, honest about its limit.**

### (b) Numbered slots + reorder metadata — rejected, blocked by mechanism

The idea: `say!("{subj} {verb} {obj}")` declares roles, and
`ranting-german`/`ranting-japanese` supply a permutation applied at render
time.

It fails at the seam, in three stacked ways, in increasing order of severity:

1. **The permutation has nowhere to apply.** The output is a `format!()` whose
   literal and argument order are compile-time constants. To permute at
   runtime, the codegen would have to stop emitting `format!()` and instead
   emit a vector of resolved fragments plus a runtime assembler that consults
   the language module. That is a direct reversal of the "Compile-time parsing
   + runtime inflection" ✅ Locked decision, and it deletes the property that
   decision exists to protect: syntax and argument errors caught by `format!()`
   at compile time.
2. **Inter-placeholder literal text has no role to be permuted with.** In
   `"{=dog} {dog bark} at {@cat}"` the word `at` is neither subject, verb, nor
   object; it is an English preposition that a German or Japanese rendering
   would delete, replace with a case ending, or move to a postposition. A
   permutation over slots is not expressive enough — the literal glue must be
   permuted, deleted and inserted too, which is no longer a permutation, it is
   translation. Phase 6 declares translation-catalogue machinery explicitly out
   of scope.
3. **Intra-placeholder order stays fixed anyway.** Per "Intra-placeholder order
   is hard-coded too" above, (b) also has to turn
   `handle_placeholder_impl`'s pre→nr→noun→post sequence into data, or it still
   cannot render a suffixed article. So (b) is not "a permutation table on top
   of today's engine"; it is a rewrite of both halves of the engine.

And it drags in prerequisites nobody has scoped: `uc`'s sentence-initial
determination (item 6, extended), and — because `heed!()`/`ask!()` compile the
*same* kind of literal template into an anchored regex
(`ranting_derive/src/heed.rs`) — a matching reorder story on the input side,
or the two directions stop being inverses.

A half-built (b) is worse than none: it would handle bare SVO↔SOV↔VSO
demonstrations, fail on every clause with a preposition, a separable prefix, or
a suffixed article, and read in the API docs as if word order were solved.

### (c) Syntax-tree API — rejected, blocked by identity

`sentence!(subject, verb, object)` handing a small tree to a language module
that renders it is **not infeasible**. It is the design that actually works:
with no literal template there is no text the crate doesn't own, so
clause-final prefixes, postpositions, VSO, suffixed articles and
question-final particles all become the language module's ordinary business.
It is what a from-scratch multilingual generator would look like.

It is rejected for what it costs, not for whether it works:

- It abandons the placeholder sigil grammar, which ROADMAP.md's
  architecture-decisions table marks ✅ **Locked** with "Sigil grammar is the
  crate's identity; keep it." `say!()`'s whole proposition is that an author
  writes the sentence they want and the crate inflects inside it. `sentence!()`
  inverts that: the author supplies parts, the crate composes the sentence.
- It would ship alongside `say!()` forever, since it cannot replace it without
  breaking every existing user — two macros, two mental models, two sets of
  docs, two test surfaces, and an inevitable stream of "why doesn't
  `sentence!()` support `{a !good thing}`" issues.
- The per-language rendering logic it needs is exactly the content Phase 6
  declares out of scope for `ranting` ("language-specific vocabulary and rules
  stay out of `ranting`"). A `sentence!()` that ships without any language
  module is an empty frame; one that ships with English rules inside it puts
  the English syntax rules back into the crate that just spent a phase getting
  them out.

If a genuinely tree-shaped generator is ever wanted, it belongs in a separate
crate that depends on `ranting` for morphology and owns syntax itself — the
same shape as the proposed `ranting-if`. It is not a Phase 6 item and this
document does not schedule it.

## Recommendation

**Adopt (a): per-language template sets, and document the word-order boundary
as permanent.**

Concretely, and with no code attached:

1. `ranting` inflects **within** a template. It does not, and will not,
   reorder across placeholders or reorder the fragments inside one placeholder.
2. A non-English caller supplies one template per language per sentence and
   selects it before the `say!()` call. `ranting-i18n` supplies morphology for
   the chosen template, not the template.
3. The boundary belongs in the extensibility documentation, in the same place
   a fork author reads about `inflect_*_custom` (exact placement is Open
   Question 1 below) — written as a boundary, not a TODO, matching how the
   input-parsing spec insisted
   pronoun/custom-hook reversal be documented as "fundamentally ambiguous," not
   "not implemented yet."
4. **For ROADMAP.md Phase 6 item 10**, which requires that an honest "German
   still needs per-language templates for word order" outcome be recorded in
   the companion crate's README *and in this spec*: it does. German verb-second
   and clause-final verb placement are **not** achievable through `ranting`'s
   hooks; `ranting-i18n`'s German reference lexicon must carry German word
   order in its own template strings, and any place it appears to need more is
   a word-order need, which item 1 has answered "no" to, not a hole in items
   2–9.

This is consistent with the phase's own framing — "Ranting inflects text a
program already composes; it is not a translation system" — rather than a
retreat from it. Word order is composition. Composition is the caller's.

## What stays impossible for `ranting-i18n` under this recommendation

Named constructions, so nobody has to rediscover them one at a time:

- **German verb-second with a clause-final element** — separable prefixes
  (*Ich sehe den Hund **an***) and perfect participles (*Ich habe den Hund
  **gesehen***). One verb, two positions; the placeholder grammar has one verb
  slot per placeholder and asserts against having both a pre- and a post-verb.
- **Japanese / Korean / Turkish SOV with postpositions** — the object precedes
  the verb and the particle follows the noun. An English template's word order
  and its prepositions are both wrong, and neither is text a hook owns.
- **VSO languages** (Irish, Welsh, Classical Arabic) — the verb precedes the
  subject. `{=dog} {dog bark}` cannot render verb-first.
- **Suffixed definite articles** (Romanian, Norwegian/Danish/Swedish,
  Bulgarian) — blocked *inside* a single placeholder by the fixed
  pre→nr→noun→post assembly order, independently of everything else.
- **Adjective position** — Romance post-nominal adjectives (*un chat noir*)
  when the template places the adjective before the noun. Phase 6 item 5's
  adjective hook gives agreement, i.e. the right *form*; it does not move the
  word. Note that item 5's worked example (*un chat noir* / *une robe noire* /
  *des chats noirs*) is post-nominal in both the French and the template, so it
  never exercises movement — agreement is all it delivers.
- **Sentence-final question particles** (Japanese *か*, Mandarin *吗*) and
  other clause-level particles that have no English counterpart to occupy.
- **Any construction where a word is deleted rather than inflected** — the
  English preposition that a case ending replaces has to be absent from the
  template, which means it is a different template.
- **Correspondingly on the input side**: `heed!()`/`ask!()` have no reorder
  analogue and gain none from this recommendation. Their templates are
  per-language too (and Phase 6 item 9 separately covers non-space-delimited
  scripts).

The single-sentence version, for the CLAUDE.md/README boundary note:
*`ranting` inflects words within a template; the order of those words is the
template's, and the template is the caller's — so a non-English application
needs one template per language, and no inflection hook will ever change that.*

## Rejected alternatives, recorded

| Rejected | Why |
|---|---|
| (b) numbered slots + reorder metadata | Blocked by mechanism: the `format!()` literal and its argument order are compile-time constants (✅ Locked decision), inter-placeholder glue words need deletion/insertion rather than permutation, and intra-placeholder assembly order is fixed as well. Also silently requires a sentence-initial-`uc` story and an input-side (`heed!()`) analogue. |
| (c) `sentence!()` syntax-tree API | Works, but abandons the sigil grammar the architecture-decisions table marks as the crate's identity, ships a permanent second product surface beside `say!()`, and reintroduces per-language syntax rules into the crate Phase 6 is keeping language-agnostic. Belongs in a separate downstream crate if ever wanted. |
| "(a) now, (c) later" hedge | Rejected as a *conclusion*: recording a deferred (c) leaves the boundary open in the docs and invites items 2–9 to be designed against a syntax-tree future they will never see. The value of this spike is a plain answer. |
| Extending the placeholder grammar with an explicit second verb position | Would address only the German separable-prefix case, adds grammar surface to every English user, and still leaves the other six rows of the impossible list. Rejected as a point fix for a category problem. |

## Open questions for whoever picks this up next

1. Where exactly the boundary statement lands — `README.md` extensibility
   section, `docs/EXTENSIBILITY.md`, `CLAUDE.md` "Non-obvious behaviors", or
   all three. (Recommendation: `docs/EXTENSIBILITY.md` in full, one-line
   pointers from the other two.) Not blocking on any Phase 6 item.
2. Whether Phase 6 item 6's orthography hook should, while it is being
   designed anyway, expose "is this placeholder sentence-initial" as an
   explicit signal rather than the implicit `uc: bool`. It is not needed for
   (a), but it is cheap there and would be the one piece of a hypothetical
   future (c)/downstream tree crate that `ranting` could usefully own.
3. Nothing else. This spike does not block items 2–9; it unblocks item 10 by
   telling it what not to attempt.

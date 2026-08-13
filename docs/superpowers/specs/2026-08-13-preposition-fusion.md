# Feasibility: preposition-article fusion and the closed pre-noun word list

**Status**: design spike complete; conclusion is **(c), leave it to the
caller's template — unchanged from items 1/20's word-order boundary — with
(b) named as the real fix should this ever get scheduled**. No production
code is proposed *by this document*. ROADMAP.md Phase 6 item 25; closes hole 7
of `ranting_i18n/README.md`, the last item-10 hole with no queued follow-up,
and the sole hole `ranting_es`'s independent Spanish lexicon (item 23) hits.

This is the fourth Phase 6 spike, following the same shape as
`2026-08-13-grammatical-case-inventory.md` (item 18) and
`2026-08-13-number-categories.md` (item 4): ground the question in the code
as it stands, score the options, state the residue plainly.

## Motivation

Several languages contract a preposition and the immediately following
definite article into one word:

| Language | Preposition + article | Fused form |
|---|---|---|
| German | `zu` + `dem` | `zum` |
| German | `in` + `dem` | `im` |
| German | `an` + `das` | `ans` |
| French | `de` + `le` | `du` |
| French | `à` + `le` | `au` |
| Spanish | `de` + `el` | `del` |
| Spanish | `a` + `el` | `al` |
| Italian | `di` + `il` | `del` |

`ranting_i18n`'s README already named this as hole 7 (belonging to item 7,
which built `elide_article_custom` for the *different* elision problem,
`le`+`homme`→`l'homme`, and recorded in passing that this one is out of
reach). `ranting_es/tests/holes.rs::hole_1_de_el_does_not_fuse_to_del` is now
the **only** hole the Spanish lexicon hits at all — item 23 built a complete,
correct Spanish implementation on top of every other hook (postnominal
adjective agreement, `tú`/`usted`, `¿`, `el agua`'s euphonic exception) and
still could not reach this one. Two structurally unrelated fork languages
converging on exactly the same unreachable gap, with nothing else standing
between either of them and full correctness, is why this spike treats it as
the single highest-value remaining item in the whole extensibility surface —
not a German curiosity that happens to also show up in Spanish.

## What the code does today

### The pre-noun slot is real, but closed, and lives *inside* the placeholder

`ranting_core::ph_ext::parse`'s `pre` capture group (the reference grammar is
restated in `ranting_core/src/ph_ext.rs`'s module doc comment) matches:

```text
(?P<pre>(?:
    \??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se|`[\w-]+|
    (?:[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
    (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
    (?:\s+(?:\??an?|\??some|\??the|th[eo]se|`[\w-]+))?
)(?:\s+[\w-]+)*?\s+)?+
```

In plain terms: the *first* word of the pre-noun slot must be an article
(`a`/`an`/`some`/`the`/`these`/`those`, each optionally `?`-prefixed) or one
of a fixed, hard-coded set of English auxiliary/modal verbs (`can`, `may`,
`shall`/`will`, `are`/`were`, `had`/`have`, `do`, `could`/`should`/`would`,
`must`/`might`, each optionally with `n't`). Once that first word matches,
arbitrary further words are accepted lazily before the noun — which is why
`"will `x"` is a valid pre-noun slot in the fuzz corpus
(`ranting_core/src/ph_ext.rs`'s `parity_fuzzed` test) but a bare preposition
is not: `"in"` matches neither branch, so `say!("{in the *=0}", haus)` fails
to parse at all — `expected article or verb` — before `handle_param`
(`ranting_derive/src/lib.rs:560`) ever runs. This is a **compile-time**
rejection, not a runtime one: there is no way to write a test that executes
past it, which is why both `ranting_i18n`'s hole 7 and `ranting_es`'s hole 1
record the fact in prose rather than as a second runtime assertion.

### `elide_article_custom` cannot reach it, and the reason is structural, not an oversight

`elide_article_custom` (item 7, `docs/EXTENSIBILITY.md` §2.7) runs *after*
the placeholder has been assembled, splicing over the article, the
separator, and the rendered *following* text — the byte span the call site
recorded starts at the article and ends after the noun. The preposition in
`"Vengo de {the *=0}."` is not in that span, or in any span the hook ever
sees, because it is template **literal** text that sits *before* the
placeholder opens — outside the `{...}` entirely. `handle_placeholder_impl`
only receives what `ph_ext::parse` captured from inside the braces; the `de`
in the format string is baked into the generated code as an ordinary string
literal `format!()` concatenates in, with no association to the placeholder
that follows it. CLAUDE.md's `elide_article_custom` bullet already states
this for French `de`+`le`→`du`; this spike confirms the same holds for every
row in the table above, German and Spanish included, and that no existing
hook parameter carries the missing word.

There is one place the macro *does* already look at the literal text
immediately preceding a placeholder: `parse_str_params`
(`ranting_derive/src/lib.rs:98-160`) uses `PH_START`'s `pre` capture (a
*different* `pre` than `ph_ext`'s — this one is the surrounding template
text, not the in-placeholder slot) to compute `at_sentence_start`, a single
boolean fed to `handle_param`. So the mechanism to inspect what comes before
a placeholder in the raw format string exists and is exercised today — but
it throws away everything except "does this look like sentence-initial
punctuation," never the actual word. Reaching the preposition itself would
mean extending that same call site to capture and forward the word, not
inventing a wholly new parsing pass.

## Options, scored

### (a) Open the pre-noun slot's *word list* — insufficient alone, rejected as a standalone fix

Loosen `ph_ext::parse`'s `pre` grammar so an arbitrary lowercase word (not
just the closed article/modal set) is accepted as the first pre-noun token —
i.e. make `say!("{de the *=0}", gato)` or `say!("{in the =0}", haus)` parse.

**What this actually buys**: nothing by itself. The `pre` capture's contents
are never inspected for meaning today — `handle_param` only checks whether
`pre` is present/absent and, for the uppercase-inheritance rule, its first
character's case. There is no code path that would take a newly-permitted
`"de"` and fuse it with the article that follows. Shipping (a) alone changes
a compile error into a placeholder that parses and renders `"Vengo de el
gato."` — no better than today's actual output, just reached a different way.

**What stays out of reach even after (a)**: the fusion itself. (a) is a
grammar change with no runtime consumer; it would need to be paired with (b)
to do anything, and paying (a)'s cost (a public grammar change, semver-major
for anyone whose code currently relies on `{de the ...}` being a compile
error, plus new ambiguity between "arbitrary pre-noun word" and the existing
modal-verb detection that `conjugate_pre_verb`-style logic downstream relies
on) without (b) attached buys literally nothing.

**Rejected as a standalone step.** Recorded here because it is the most
obvious-looking option and the one explicitly named in this item's own
framing; the reason it does not work in isolation is the finding worth
writing down.

### (b) A new hook fed the preceding literal word — the real fix, not scheduled

Extend `parse_str_params` to capture the literal word (or word run)
immediately before a placeholder — the same `PH_START` `pre` match already
computed for `at_sentence_start`, but forwarded as text instead of collapsed
to a bool — and thread it through to a new hook, something like
`inflect_preposition_custom(&self, preposition: &str, article: &str, case,
class, as_plural, uc) -> Option<String>`, called at the same point
`elide_article_custom` is today, given the chance to return a replacement
for *both* the preposition and the article (`"de"` + `"el"` → `"del"`,
consuming the literal word instead of leaving it in place). A fork whose
language has no such fusion (English, most of Spanish's own article set —
`de la`, `de los`, `de las` are all already correct without a hook) simply
returns `None` and nothing changes.

**What this closes**: exactly the gap this spike exists to name — both
`ranting_i18n`'s hole 7 and `ranting_es`'s hole 1, and by extension every row
in the fusion table above, since the mechanism is general (preposition +
article-rendering-outcome → replacement string), not hard-coded per
language.

**What stays out of reach even after (b)**: multi-word prepositions and
prepositions separated from the placeholder by more than whitespace (a
comma, an adverb) — `PH_START`'s `pre` capture is one literal run up to the
placeholder, mirroring the same single-token assumption `at_sentence_start`
already makes. Also out of reach: fusions that depend on more than the
immediately preceding word (none of the languages in the table need this,
but it is worth being honest that the design only reaches the adjacent-word
case).

**Cost, precisely**: this is a real hook-signature addition — a *ninth*
hook pair by the count `docs/superpowers/specs/2026-08-13-number-categories.md`
already tracks — plus a change to `parse_str_params`'s data flow (today
`pre` is destructured into a `bool` and discarded; it would need to survive
as a `&'static str` argument baked into the generated call, the same
bake-what-runtime-can't-re-derive shape `PostSpec::Tense`/`PostSpec::Degree`
already use). It is not a small change, but it is smaller than it first
looks, because the text it needs is already being read at the exact call
site that needs to read it — nothing new has to be parsed, only forwarded
and not discarded. Should this item ever get scheduled, it should be bundled
with any other owed hook-signature break (the number-categories spike's
`count` parameter, if it lands after item 5, is the other one on record)
rather than taken alone, per this repo's standing practice of not shipping
two separate breaking hook-signature changes back to back.

### (c) Leave it to the caller's template — recommended for now, unchanged from items 1/20

Do nothing to the grammar or the hook surface. A caller who needs `"del
gato"` writes it directly: either hardcodes the fused form as template
literal text when the noun's gender/number is known at the call site, or
branches on `noun.noun_class()`/`is_plural()` in ordinary Rust to select
between two templates (`"Vengo del {the *=0}."` vs. `"Vengo de la {the
*=0}."`), the same pattern item 22's template-selection spike
(`docs/superpowers/specs/2026-08-13-template-selection.md`, folded into
docs by item 27) already documents callers doing for per-language template
choice generally.

**The catch, stated precisely**: this is *not* free the way item 20's
word-order boundary is free. Word order is fixed at the call site once,
independent of the argument's runtime state. Preposition fusion is not — it
depends on the very same runtime-resolved gender/number/case that determines
which article `inflect_article_custom` renders, so a *single* template
cannot hardcode the fused form unless the caller is willing to accept it
being wrong whenever the noun's class or number varies across calls to the
same `say!()` site. Making it correct in general means the caller
re-implements, in ordinary Rust at every call site, exactly the
branch-on-grammatical-features logic the hook mechanism exists so callers
don't have to write. That is a real, ongoing cost this option accepts on
every fork's behalf, not a one-time documentation nicety — which is what
elevates this above a "document and move on" item 20/24-style close.

**What stays out of reach under (c)**: nothing is structurally blocked — a
caller can always get the right string by writing enough Rust — but nothing
is *ergonomic* either, and the branch a caller must write duplicates
information (`NounClass`, `is_plural()`) the hook system already computes
and hands to every other hook. `ranting_es` chose not to write that branch
at all; its `tests/holes.rs::hole_1_de_el_does_not_fuse_to_del` documents the
uncorrected `"de el gato"`/`"a el gato"` output rather than working around it
in caller code, which is itself evidence that (c) is a real, felt cost and
not merely a theoretical one.

## Recommendation

Ship **(c)** today — no code changes, this document is the deliverable — but
record explicitly, unlike a typical "leave it to the caller" close, that
**(b) is the option to pick up if this item is ever re-prioritized**, because
two independent fork languages hit exactly this gap with nothing else
standing in their way, and (b)'s implementation cost is lower than it first
appears (the preceding-literal-word text is already read at the one call
site that would need to forward it; nothing new needs parsing). (a) is
recorded as considered and rejected on its own — it is a plausible-looking
half-measure that a future contributor might reach for first, so the reason
it does not work alone is worth having written down rather than
rediscovered.

## Rejected alternatives, recorded

- **(a) alone** — see above: a grammar change with no consumer fixes nothing
  by itself and still costs a public, semver-relevant parser change.
- **Widening `elide_article_custom`'s existing span backward into the
  preceding literal text**, rather than adding a new hook. Rejected because
  `elide_article_custom`'s whole contract (`docs/EXTENSIBILITY.md` §2.7) is
  built on a *recorded byte span* inside the placeholder's own assembly —
  extending that span to reach text the macro emitted as an entirely
  separate, unassociated string literal before the placeholder even began
  would break the "no code path outside the placeholder's own rendering
  needs to know about the hook" invariant every other hook relies on, and
  would silently start also matching plain non-preposition words that
  happen to precede a placeholder (`"the dog and {the =0}"` — `"and"` is not
  a preposition to fuse against). A dedicated hook that only fires when
  `parse_str_params` actually captured a preceding literal word keeps the
  two concerns separate.
- **A crate-level preposition/article fusion table baked into `ranting`
  itself** (i.e. hard-coding German/French/Spanish/Italian fusion rules in
  the main crate rather than exposing a hook). Rejected on the same grounds
  every other hook in this crate is a hook and not a table: `ranting`
  doesn't know what language a `Ranting` impl represents, and a closed table
  keyed by literal preposition spelling would immediately collide across
  languages (`"a"` means different things in French and Spanish) with no
  way to disambiguate without per-entity language tagging that doesn't exist
  anywhere else in the crate.

## What stays impossible under this recommendation

- Every fusion in the motivation table renders unfused (`"de el gato"`, `"in
  dem Haus"`, `"an das Fenster"`) unless the caller writes per-branch
  template selection by hand.
- No amount of `elide_article_custom` or `inflect_article_custom` creativity
  reaches this — both are confined to the placeholder's own assembled span,
  and the preposition is provably outside it, not merely awkward to reach.
- The pre-noun slot stays a closed English word list either way, since (c)
  changes no grammar; `say!("{de the *=0}", gato)` keeps being a compile
  error until and unless (b) (or (a)+(b) together) is actually implemented.

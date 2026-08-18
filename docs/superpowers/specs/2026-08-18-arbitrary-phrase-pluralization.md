# Design spike: arbitrary-phrase pluralization

**Status**: design spike, PROPOSED only — **no code in this repository is changed by this
document**. ROADMAP.md Phase 9 item 4, citing *Post-v1.2: Future Directions*' "Pluralization of
entire phrases" bullet. Not yet scoped at all before this spike; the first question the item names
is whether the feature is in bounds given the word-order boundary (Key Architecture Decisions,
"Word order lives in the literal template") — that question is answered first, and only then does
this spike sketch a mechanism.

## What's already settled, so this item doesn't re-litigate it

Phase 8 item 6 (`docs/architecture-review-2026-08-15.md` §1.10) fixed one half of "phrase
pluralization" already: a multi-word **compound noun** passed as a single noun's name —
`attorney general`, `court martial`, `mother-in-law` — now pluralizes its head word instead of its
last word, via `compound_plural` (`src/language/plurals.rs:129`). That fix is gated behind two
closed lists, `PREPOSITIONS` and `POSTPOSED_ADJECTIVES` (`src/language/plurals.rs:130,132`), each
naming the *second* word of a two-word (or first-two-of-N-word) split. The ROADMAP entry that
opened this item draws the line explicitly: item 4 is "pluralizing an arbitrary rendered phrase,"
distinct from the compound-head defect already closed. This spike takes that line as given and
does not reopen §1.10.

## Question 1: is phrase pluralization in bounds at all?

`.claude/rules/extension-hooks.md`'s "word-order boundary" section and `docs/EXTENSIBILITY.md`
§2.12 both state the same permanent property: `ranting` inflects words *within* a template, and
the order of words — across placeholders, and the pre→nr→noun→post assembly *within* one
placeholder — is fixed at compile time. Nothing proposed here needs to move a word to a different
position, delete a word, or insert a new one. **English pluralization (and, per the survey below,
pluralization generally) is satisfied by re-inflecting one or more words in place** — never by
reordering. Concretely:

- `attorney general` → `attorneys general`: the *first* word changes form; both words stay where
  they are. Already true of the Phase 8 item 6 fix, and the reason it needed no new placeholder
  syntax.
- `man Friday` → `men Friday` (worked through below): same shape, first word changes form only.
- Grimm's-law style internal changes (`foot` → `feet`, Arabic broken plurals like `kitab` →
  `kutub`) are already reachable today, entirely inside `Ranting::inflect()`'s existing return
  value for a *single* word — nothing about a multi-word phrase changes that.
- Agreement spreading to a second word in the *same clause* (English "This is a big deal" → "These
  are big deals," which changes a demonstrative and a copula as well as the noun) does not move
  any word either — it re-inflects three words that are already sitting in their existing
  positions. If each of those three words is written as its own placeholder in the template (as
  `say!()` already requires for verb/pronoun agreement today), each already receives the correct
  plural form from the existing `as_plural`-threaded hooks, with no new mechanism at all. This
  case is called out explicitly in the "Out of scope" section below, because the one piece that's
  missing (a demonstrative-pronoun placeholder) is a different, unrelated gap, not this item's.

So: **phrase pluralization is in bounds.** The word-order boundary blocks *syntax* — changing which
word comes before which — and nothing pluralization needs, in English or in the languages surveyed
for other items in this repo (German, Spanish, Arabic, Japanese), requires reordering. This is a
narrower and more useful finding than "check the boundary and stop": the boundary was never the
actual obstacle for this feature, which is worth stating plainly since the ROADMAP item's own
phrasing ("first spike question is whether it's in bounds") suggested it might be.

## Question 2: given it's in bounds, what's actually left to build?

This is where the item gets much smaller than "pluralization of entire phrases" sounds. There are
three shapes a "phrase" can take at the point `ranting` sees it, and they need three different
answers.

### Shape A — the phrase is a single noun's own multi-word name (in bounds, small, mostly already built)

This is `compound_plural`'s territory. What Phase 8 item 6 left undone is not a missing mechanism,
but a **closed vocabulary**: `compound_plural` only recognizes a second word drawn from
`PREPOSITIONS`/`POSTPOSED_ADJECTIVES`. A phrase whose head-marking word isn't in either list falls
through to `regular_plural`, which pluralizes the *whole string* as if it were one word — wrong for
exactly the same reason `attorney general` was wrong before the fix.

**Concrete example.** `Noun::with_name("man Friday")`, rendered via `say!("{the +0}", noun)`:

| | Rendering |
|---|---|
| Current (wrong) | `"the man Fridays"` — `compound_plural` returns `None` (`"friday"` is in neither closed list), falls through to `regular_plural("man friday")`, which appends `-s` to the whole string |
| Desired (correct) | `"the men Friday"` — only the head word inflects |

`man Friday` is a genuinely open-vocabulary case: `Friday` is not a preposition and not one of the
four postposed adjectives already listed, so no amount of extending those two specific arrays
generalizes to it — the crate would have to either (a) keep growing closed, hand-picked word lists
per newly-discovered phrase (the existing, deliberate strategy — see `.claude/rules/
pluralization.md` point 6, "adding a rule means auditing what it now gets wrong"), or (b) give a
`Ranting` implementor a way to say, per instance, "the head of my own name is word N, don't guess."

(b) is the only piece of new mechanism this item motivates, and it's small:

```rust
/// Index of the word (0-based, split the same way `compound_plural` already splits on `-`/` `)
/// that carries this noun's plural inflection. `None` (the default) means "let the crate decide,"
/// i.e. today's `compound_plural` → `regular_plural` fallback chain, unchanged.
fn plural_head_index(&self) -> Option<usize> {
    None
}
```

This is the same defaulted-trait-method shape `is_mass()` already uses (`.claude/rules/
extension-hooks.md`'s `is_mass()` entry) — not a `_custom` hook (it needs no `NarrationContext`,
case, or count; it operates purely on the noun's own multi-word string), not a new placeholder
marker, and `None` reproduces every existing rendering byte-for-byte. A `Noun` built with
`Noun::with_name("man Friday")` and no override still renders `"man Fridays"` exactly as today;
only an implementor that opts in by returning `Some(0)` gets `"men Friday"`. Whether the crate
*also* wants to widen the closed lists case-by-case (the existing, cheaper strategy) or add this
escape hatch (a one-time cost that closes the open-vocabulary tail permanently) is the maintainer
decision this spike surfaces, not something to decide here.

**Rejected alternative for shape A: per-word marking inside the template.** E.g. a hypothetical
`` {the +man^0 Friday} `` where a marker on the literal word `Friday` says "not part of the plural
target." This was considered and rejected: `Friday` in that spelling is *literal template text*,
frozen at compile time and never passed through any hook (per the word-order-boundary docs, the
literal is a `format!()` string, not runtime data) — so marking a portion of it would require a new
kind of placeholder that spans into literal text, which is a materially bigger grammar change than
shape A actually needs. `attorney general` and `man Friday` are already each a *single* noun's
*own name* (one placeholder argument, e.g. `Noun::with_name("man Friday")`), not two words split
across a placeholder and literal text — so the phrase never needs to leave the noun's own string in
the first place, and `plural_head_index` operating on that string, entirely inside the existing
noun-inflection call, is sufficient.

### Shape B — the phrase's words are already separate placeholders in one template (in bounds, nothing to build)

Covered above under Question 1: if a caller writes `` say!("{=this} {is} a big deal", ...) ``
style templates — each inflectable word as its own placeholder — every existing `as_plural`/
`count`-threaded hook already agrees them correctly, because that's precisely what those hooks are
for. There is no new mechanism here; a caller who wants phrase-wide agreement already gets it by
using placeholders for each word that needs to change, exactly as verb/pronoun/article agreement
already works today for e.g. `` say!("{=dog} {dog bark}") ``. This shape isn't really "pluralizing
a phrase" as a distinct feature — it's ordinary multi-placeholder agreement, already shipped.

### Shape C — the phrase is unstructured free text with no placeholders at all (out of bounds for this crate)

A hypothetical `pluralize_phrase("the quick red fox") -> "the quick red foxes"` API, given only a
plain string with no markup at all, would need to first identify which word is the head noun of an
*arbitrary* English noun phrase — a part-of-speech and syntax problem, not an orthography problem.
`.claude/rules/pluralization.md` point 2 states the crate's existing pluralization rules are
"orthographic only, a function of spelling alone, so they need no lexicon" — deliberately, so they
generalize without maintaining a dictionary. Head-finding over unrestricted English text can't be
done that way; it needs either a closed grammar-role vocabulary (which is what shape A's
`PREPOSITIONS`/`POSTPOSED_ADJECTIVES` already are, at phrase-pair scale) or real POS tagging, which
is a different kind of dependency than anything else in this crate and is not proposed here. This
isn't blocked by the word-order boundary either — it's blocked by needing a syntax parser the crate
doesn't have and, per its own stated design philosophy, shouldn't grow one to get. Out of scope;
no further work proposed.

## Summary

| Shape | In bounds? | What's needed |
|---|---|---|
| A: multi-word compound as one noun's own name | **Yes** | Small: an optional `plural_head_index()` hook (sketched above) to close the open-vocabulary tail `compound_plural`'s closed lists don't reach. Maintainer decision: build the hook, or keep extending the closed lists case-by-case as today. |
| B: phrase already split across placeholders | **Yes** | Nothing — already works via existing agreement hooks. |
| C: unstructured free text, no placeholders | **No** | Needs a syntax/POS capability outside this crate's stated design; not proposed. |

The word-order boundary, which the ROADMAP item flagged as the thing to check first, turns out not
to be the actual constraint for any of the three shapes — pluralization never needs to reorder
words, only to correctly choose which word(s) to re-inflect. The real scoping question is narrower
and smaller than "is this in bounds": it's whether the crate wants to add one small opt-in hook
(shape A) to handle multi-word compound nouns the closed lists can't already reach. That decision
is left to the maintainer.

## What this spike does not do

- Does not add `plural_head_index`, or any other method, to `Ranting` or any impl of it.
- Does not touch `src/language/plurals.rs`, `compound_plural`, `PREPOSITIONS`, or
  `POSTPOSED_ADJECTIVES`.
- Does not touch any `.rs` or `Cargo.toml` file, and does not modify any existing test.
- Does not decide between extending the closed lists case-by-case versus adding the
  `plural_head_index` hook for shape A — both are legitimate under the crate's own precedent
  (`.claude/rules/pluralization.md` point 6); the choice is left to the maintainer.

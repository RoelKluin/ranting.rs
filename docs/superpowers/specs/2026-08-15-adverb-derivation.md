# Design spike: an adverb-derivation channel (quick → quickly)

**Status**: design spike complete; conclusion is **decline — no adverb channel
is added, and the item closes as decided-against rather than deferred**.
ROADMAP.md Phase 8 item 5. The item was scoped from the start to be *decided,
possibly declined*, and this spike is that decision. The sigil grammar is
Locked (ROADMAP.md's Key Architecture Decisions table) and no code in this
repository is changed by this document.

This is the fifth Phase 8 spike, following the shape of the others in this
directory (ground the question in the code as it stands, survey options
including the rejected ones, state a recommendation and what stays out of
reach under it).

## The question

`ranting` derives comparative and superlative adjective forms in place
(`{?w !good}` → `"Better"`, `{?w !!bad}` → `"worst"`), so quick→quickly looks
like exactly the kind of in-place word inflection the crate already does —
and it has no channel. Is one worth adding?

ROADMAP.md's item framing names the decisive question, and it is not
morphology: the `!`/`!!` degree slot is post-noun only, so the question is
**which sentence positions an English adverb actually occupies in a `say!()`
template, and how many of them a placeholder could reach without moving
words**. The word-order boundary is a locked decision
(`.claude/rules/extension-hooks.md`'s "The word-order boundary — permanent,
not a gap"; `docs/EXTENSIBILITY.md` §2.12; DONE.md Phase 6 item 20), cited
here rather than re-opened: agreement is form, never position, so every
option below must render the adverb where an existing slot already renders
text, or not at all.

## What the code does today: where an adjective-shaped word can render

The only adjective channel is the post-noun degree slot. `handle_param`'s
all-`!` marker arm (`ranting_derive/src/lib.rs`) resolves `!`/`!!` at compile
time via `ranting_derive/src/language/adjective.rs`'s
`to_comparative`/`to_superlative` — the irregular table generated from
`data/irregular_adjectives.txt` plus regular `-er`/`-est`-or-`more`/`most`
rules — and bakes the result as `PostSpec::Degree`, whose runtime arm
(`src/lib.rs:1152`) offers the *base* word to
`inflect_adjective_custom_with_context` (`src/lib.rs:1162`) before falling
back to the baked English form. Two structural facts carry the whole
analysis:

1. **A placeholder has exactly one post slot.** `PostSpec` is one variant per
   placeholder — verb *or* tense-marked verb *or* degree word *or*
   possessive-`s`, never two — and `handle_param` already rejects combining
   the degree marker with tense markers. A hypothetical adverb marker could
   therefore never co-occupy a placeholder with the verb it modifies.
2. **The assembly order inside a placeholder is a fixed `res.push_str`
   sequence** (article/pre-verb → number → noun-or-pronoun →
   possessive/post-verb/tense/degree — §2.12's third bullet). There is no
   slot between the noun and the post verb, and no hook can create one.

The useful existing idiom this spike leans on: a hidden-case noun carries
agreement without rendering, so `{?w !good}` renders just `"better"`
(pinned in `tests/ranting/comparative_adjectives.rs` and
`tests/ranting/adjective_agreement.rs`), and `{?w run}` renders just the
agreed verb.

## Reachability: the position inventory

The positions an English adverb occupies, checked one by one against the
grammar above. The mid-position and post-verb rows were verified against the
built crate (scratch crate on a path dependency, this repo's standard
substitute for a compile-fail/behavior harness), not asserted from memory.

| Position | Example | In a `say!()` template | Reachable by a placeholder channel? |
|---|---|---|---|
| Clause-final, after the verb — the default manner-adverb position | "She runs **quickly**." | verb occupies the placeholder's single post slot; the adverb falls after the closing brace | Not in the *same* placeholder (fact 1). Only via a trailing hidden-noun placeholder: `{=w run} {?w ~quick}.` |
| Mid-position, between subject and verb | "She **quickly** runs." | subject and verb share one placeholder; no slot between them (fact 2) | Not in the same placeholder. Expressible **today** by splitting: `say!("{=w} quickly {?w run}.", w)` → `"She quickly runs."` (verified) — adverb as literal text, verb agreement intact |
| Sentence-initial | "**Quickly**, she ran." | before any placeholder | Literal-text position by construction |
| Modifying an adjective | "a **truly** great idea" | the only adjective slot is post-noun `!`; a modifier would precede it *inside* `post` | No slot; unreachable without a grammar-level second post slot |
| Modifying another adverb | "**very** quickly" | no adverb slot exists to modify | Same as above |
| Bare post-noun — where a post-slot marker would naturally render | *"the dog quickly"* | the hypothetical marker's home position | Renderable, but it is not a grammatical English position for a bare manner adverb without a verb |

So the channel is not strictly unreachable: two positions (clause-final via a
trailing hidden-noun placeholder, mid-position via a split) can be reached
without moving any word. The ROADMAP item asks whether this is the same shape
as `ranting_i18n`'s prenominal-adjective hole (its README §4a: the German
`!`/`!!` output is always in the wrong place, so *no German sentence exists*
in which the hook's output is correct). It is close but not identical, and
the difference is what decides the item:

- **The German hole**: a channel with real runtime value (declension
  agreement) in a position no correct sentence can use.
- **An English adverb channel**: positions a correct sentence *can* use — but
  a derived form with **no runtime value in any of them**.

Both end the same way — the word is written as literal template text — but
for different reasons, and the second reason is terminal where the first was
a genuine loss.

## Why the derived form has no runtime value

Everything an adverb marker could emit is a compile-time constant the caller
could have typed:

- **The source adjective is a template literal**, exactly like the degree
  slot's — there is no runtime-variable adjective anywhere in this grammar to
  derive from.
- **The derived form is invariant.** English `quickly` has no person, number,
  tense, case or degree agreement; nothing about the entity in the
  placeholder can ever change one byte of it. The degree slot, which this
  item pattern-matched on, differs on both counts that matter: one base
  yields *two* forms selected by marker (`!` vs `!!`), and its hook offer is
  a real agreement channel for forks (`ranting_es` inflects `!negro` for
  gender — `.claude/rules/crate-layout.md`). An adverb marker yields one form
  from one base, always.
- **The fork story is empty too.** Derived adverbs are as invariant in the
  fork languages as in English — Spanish `-mente` forms and German adverbs
  (identical to the uninflected predicative adjective) do not agree with
  anything — so a tenth `inflect_adverb_custom` pair would be a hook whose
  every call site passes a constant, joining the never-overridden hooks the
  Phase 7 item 1 audit (`2026-08-14-unused-hook-audit.md`) exists to shrink,
  not grow.

`{=w} quickly {?w run}` versus a hypothetical `{=w} {?w ~quick} {?w run}`:
the second spends a scarce marker character and a placeholder to render the
identical constant the first spells out. There is no template in which the
marker form renders anything the literal cannot.

## The `-ly` rules, if it were ever built

Recorded so the morphology question never has to be re-derived; none of this
changes the recommendation. The rules are **orthographic** — a function of
spelling alone, needing no lexicon — so they would belong with the plural
rules in `src/language/plurals.rs`'s spirit (`.claude/rules/pluralization.md`
point 2's rules-versus-table split), though mechanically they would sit
beside `ranting_derive/src/language/adjective.rs`'s degree rules, since the
input is a compile-time literal:

1. Consonant + `y` → `-ily`: happy→happily, easy→easily. Monosyllables
   resist it (shy→shyly, sly→slyly), the same shape as the degree rules'
   consonant-`y` handling.
2. `-ic` → `-ically`: basic→basically, drastic→drastically. Exception:
   public→publicly — a table row.
3. Consonant + `-le` → replace the `e` with `y`: gentle→gently,
   simple→simply, terrible→terribly, subtle→subtly.
4. `-ue` → drop the `e`: true→truly, due→duly.
5. `-ll` → append `-y` only: full→fully.
6. Default: append `-ly`, final `e` kept (quick→quickly, nice→nicely).

And the irregular table it would need (point 6 of `pluralization.md`: adding
a rule means auditing what it now gets wrong): good→well; whole→wholly;
day→daily, gay→gaily (vowel-`y` words rule 1 misses); and — the
lexicon-shaped class spelling cannot decide, structurally the same as the
counterexample classes that keep singularization frozen
(`pluralization.md` point 4) — the flat adverbs and meaning-splitting pairs:
fast→fast, hard→hard (hardly means something else), late→late (lately means
something else), high→high/highly by sense. Those are not spelling rows; they
are per-word semantic decisions, which is the first sign this channel wants a
lexicon the crate deliberately does not have.

## Options, scored

### (a) Decline: the adverb stays literal template text — recommended

Zero cost, and per the reachability table zero loss: every grammatical adverb
position is either a literal-text position by construction (sentence-initial,
clause-final after an in-placeholder verb) or reachable today with the adverb
as literal text and all agreement intact (mid-position via
`{=w} quickly {?w run}`, verified). The two genuinely unreachable positions
(modifying an adjective or another adverb) are unreachable for *any* new
post-slot marker too, because they need a slot before the degree word that
the fixed assembly order does not have — declining loses nothing a marker
would have won.

### (b) A new post-slot adverb marker — rejected

E.g. `{?w ~quick}` → `"quickly"` on the hidden-noun idiom. Spends one of the
few clean marker characters (the verbatim-verb spike's taken-character
inventory: seventeen of the plausible ASCII punctuation characters already
mean something, and Phase 8 item 2 is already shopping the short remainder)
to emit a compile-time constant; cannot co-occupy a placeholder with the verb
it modifies (one post slot), so the flagship "runs quickly" case still ends
as two placeholders that literal text beats. All the parity costs of a
grammar change (`PH_EXT`/`ph_ext`/`handle_param`, three sites in lockstep)
for no template that renders anything new.

### (c) A tenth hook pair, `inflect_adverb_custom` — rejected

No agreement axis exists for it to carry, in English or in any current
falsifier's language; it would be born never-overridden. The hook-surface
budget discussion this repo already had (`extension-hooks.md`; the unused-
hook audit) points the other way.

### (d) Fold into the degree family (`!~` or a third `!`) — rejected

The degree arm is `handle_param`'s all-`!` compile-time branch and
`AdjectiveDegree` deliberately has no `Positive` variant because no marker
produces one (`extension-hooks.md`); grafting a non-degree derivation onto it
muddies a clean "degree only" contract and still inherits every objection to
(b).

## Recommendation

**Decline.** Adverb derivation is in-place word inflection, but of an
invariant form from a compile-time literal — a spelling macro, not an
agreement channel. Every sentence position a derived adverb could correctly
occupy is already served, byte-identically, by writing the adverb as literal
template text, which the word-order boundary already establishes as the home
of everything positional. The item closes as decided-against; re-opening it
should require what this spike shows to be missing — a runtime-variable
source word, or a language in whose grammar derived adverbs *agree* with
something a hook could carry (none of the four falsifiers' languages
qualifies).

## What stays impossible under this recommendation

- Nothing that was possible before. No template renders differently; no
  marker, hook, `PostSpec` variant or table is added.
- "a truly great idea"-style adverb-modifies-adjective remains literal text —
  as it would under every rejected option too.
- If a future language fork ever surfaces agreeing adverbs, that is a new
  finding in the falsifier tradition and re-opens this on its own evidence,
  not a gap this decline creates.

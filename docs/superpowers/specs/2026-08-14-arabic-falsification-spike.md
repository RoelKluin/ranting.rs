# Arabic falsification spike

**Status**: spike complete. Conclusion: **`ranting-ar` clears the item 4 bar**, on one axis that
no existing fork can reach — but not the axis the ROADMAP expected. The count channel item 14
shipped does **not** close the number-categories gap for the counted noun itself, because
`Ranting::inflect` was the one signature item 14 did not widen. `elide_article_custom`, by
contrast, comes through its first real test intact. ROADMAP.md Phase 7 item 2.

No production code is proposed by this document. Two changes it *recommends* have to be scheduled
as their own items; one defect it found is unrelated to Arabic and is filed separately.

## Method

Unlike the Phase 6 spikes, which reasoned from signatures, this one **ran the code**. A throwaway
crate (path-dependency on `ranting`, not committed — a spike is doc-only by ROADMAP scope)
implements a minimal `ArNoun` with the three number forms of كِتاب and the sun/moon-letter pair
شمس/قمر, and probes each ROADMAP question directly. Every rendered string quoted below is actual
output, not a prediction. That matters here: two of the five questions resolved the opposite way
from the ROADMAP's stated expectation, and one only because a probe was written to be *dishonest*
and see whether the dishonesty was detectable.

## 1. Dual with a numeral present — **not reachable**, contrary to expectation

The ROADMAP predicted that item 14's `PlaceholderCount` "*should* mean `say!("{$n kitab}", 2, book)`
can render Arabic dual by branching on `count.value == 2` in a fork's own hook." It cannot, and the
reason is one line:

```rust
fn inflect(&self, to_plural: bool, uc: bool, case: GrammaticalCase) -> String;
```

**The noun's own form is produced by `inflect`, and `inflect` receives no count.** Item 14 added
`count: Option<PlaceholderCount>` to five hook *pairs* — verb, pronoun, article, elision, adjective
— and item 26 designed a sixth with it. `Ranting::inflect` was widened in the same commit, but with
`case: GrammaticalCase`, not with a count. So every hook that *can* see the number 2 renders
something other than the counted noun, and the one call that renders the counted noun sees a
`bool`.

Concretely, `{$n kitab}` with `n = 2` calls `inflect(to_plural = true, ..)`, and `true` is all
there is: `kutub` (plural), never `kitābān` (dual).

**Agreement, however, *is* reachable.** The verb hook does receive the count, and dual verb
agreement works today:

```
{$n kitab are}, n=2  ->  ٢ kitābān humā [dual]      // verb from inflect_verb_custom
                                     ^^^^^^^^^^^ count.value == 2 branch, works
```

So Arabic dual under `ranting` is *half* expressible: everything that agrees with the counted noun
can be made dual, and the counted noun itself cannot. That is a worse failure mode than either
being fully reachable or fully unreachable, because the output is grammatical-looking and wrong in
exactly one word.

### The side-channel, and why it is not an answer

A fork *can* smuggle the count across: `inflect_numeral_custom` receives `count: Option<i64>` and
runs before the noun is pushed, so stashing it in a `Cell<i64>` and reading it back inside
`inflect` produces the right form. The probe did exactly that, and it works:

```
{$n kitab}, n=2  ->  ٢ kitābān      // via Cell
{$n kitab}, n=3  ->  ٣ kutub
```

It is not a workaround, for three reasons the probe also demonstrates:

1. **It contaminates later placeholders in the same template.** The stale value is still there:
   ```
   "{$n kitab} and {+kitab}", n=2  ->  ٢ kitābān and kitābān
                                                     ^^^^^^^ wrong: no numeral, should be kutub
   ```
2. It depends on undocumented call *order* between two hooks, which no test pins and no doc
   promises.
3. It requires interior mutability in a type whose whole trait surface is `&self`, i.e. it makes a
   logically-pure impl stateful and not thread-safe by construction.

A fork that shipped this would be shipping a latent bug. Recording it as "reachable with a trick"
would be false.

### Recommendation (needs its own item)

Add `count: Option<PlaceholderCount>` to `Ranting::inflect`, exactly as item 14 added it to the
five hook pairs — same type, same source (the placeholder's own `#var`/`$var`), `None` for a bare
placeholder. English ignores it, so output is byte-identical; the change is a signature break of
the same shape and size as item 14's, which took one commit.

This is also the correction the ROADMAP asked for to
`docs/superpowers/specs/2026-08-13-number-categories.md`, and it is sharper than the wording that
item requested. That spec's verdict should not become "reachable in principle, unverified" — it
should become **"the *agreement* half is reachable and verified; the *counted noun* half is
unreachable, because `inflect` has no count."**

## 2. Dual with no numeral — unreachable, and correctly so

Arabic marks dual on the noun with no numeral written (`kitābāni`, "two books"). There is no
placeholder marker for it: `match_nr` accepts only `[+-]|(#|\??\$)\w+`, so the number slot is
"singular / plural / take it from this variable", with no third literal. Confirmed:
`{+kitab}` → `kutub`, and there is nothing else to write.

Unlike §1 this is a *grammar* change, not a signature change — a new marker character in
`PH_EXT`/`ph_ext`, with all the priority-ordering hazards `.claude/rules/placeholder-grammar.md`
records. **It should not be made.** With §1's fix, `{$n kitab}` covers the numeral-present case,
and a fork wanting a bare dual can carry it as a distinct entity (a `dual: bool` on the noun, its
`inflect` returning the dual form) exactly the way `ranting_i18n` carries definiteness. That is
`ranting_i18n` hole 4b's shape, and it is an acceptable narrowing, not a hole: "dual is written
with its numeral, or carried by the entity."

## 3. Sun-letter assimilation — **reachable, and the hook's shape is right**

The ROADMAP's question was whether `elide_article_custom`'s two-string signature can look up which
of fourteen consonants triggers assimilation, or whether the consonant needs surfacing more
directly. It can, and it does not:

```
  [elide] article="The"  separator=" "  following="شمس"     -> ash-shams form
  [elide] article="The"  separator=" "  following="قمر"     -> al-qamar form
```

`following` is the noun alone, so the trigger consonant is `following.chars().next()` — one call,
no parsing, no ambiguity about what "the following word" means. This is the hook's **first real
consumer** (item 1's audit found it two-for-two unused), and it survives contact: the post-assembly
design, which exists so a fork can drop the separator, is exactly what Arabic needs, since `al-` is
written bound to its noun with no space.

One trap, hit while writing the probe and worth documenting for any fork: **the article arrives
capitalized** when the placeholder is sentence-initial (`"The"`, not `"the"`), so the natural
`match article { "the" => .. }` silently falls through to `None` and the assimilation just doesn't
happen. `.claude/rules/extension-hooks.md` records that the article is pre-capitalized; it does not
say that this is what makes a naive match fail. `docs/EXTENSIBILITY.md` §2.7 should carry a
one-line warning.

## 4. Root-and-pattern morphology — a fork's lookup problem, not a seam problem

Every `_custom` hook returns an opaque `String`, and `inflect` does too, so nothing mechanically
requires a stem-plus-affix model. The ROADMAP's real question was subtler: is "the base form" — the
notion `PostSpec::Degree.base` and `say_with!()`'s baked uninflected verb both rely on — still
well-formed for a language whose forms are a root mapped onto a pattern?

It is, because **the base form is whatever the template author wrote**, not something the crate
derives. `say_with!()` bakes the template's own word and hands it back; the fork maps `"kataba"` (or
`"k-t-b"`, if that is what its templates write) to a form through its own table. The seam carries a
string from the template to the hook without interpreting it, and that is all root-and-pattern
morphology needs.

The genuine consequence is elsewhere and is not a `ranting` gap: Arabic templates would be written
with citation forms that are themselves inflected words, so a `ranting-ar` lexicon is a *lookup
table keyed by citation form*, which is larger than German's or Spanish's but not differently
shaped.

## 5. Right-to-left rendering — out of scope, as predicted

`ranting` assembles `String`s with `format!()` and never emits direction-control characters or
layout markup. RTL rendering is a property of the text's Unicode script in the consumer's renderer.
Nothing to score.

Arabic-Indic digits *are* a `ranting` question and are already answered: `inflect_numeral_custom`
renders them fine (`٢`, `٣` in the probe output above), which is item 8's digit-system channel
working as designed, with a Devanagari precedent already pinned in `tests/ranting/numeral.rs`. Not
a new gap.

## What `ranting-ar` would falsify that German and Spanish structurally cannot

| Axis | German | Spanish | Arabic |
|---|---|---|---|
| Third morphological number | no | no | **yes — and it fails** |
| `elide_article_custom` used for real | no | no (contraction is preposition-side) | **yes — and it passes** |
| Non-concatenative morphology | no | no | yes (but no seam consequence) |
| Article written bound to the noun | no | no | **yes** |

Two of those are decisive. The dual finding is not reachable by any Indo-European lexicon, and it
is a *live defect in a shipped signature* rather than a documentation gap — the strongest kind of
falsification this apparatus has produced since `ranting_i18n`'s hole 1 found `say_with!()`
unreachable downstream.

## Recommendation for item 4

**Build it**, and build it *after* the `inflect`-count change, not before — a `ranting-ar` written
against today's signature would have to encode the `Cell` hack or omit the dual, and either choice
would make its `tests/holes.rs` a record of a workaround rather than of the gap. The item 4
synthesis should treat §1 as an implementation item that blocks item 5, not as a hole for the
lexicon to record.

Scope, unchanged from the ROADMAP's provisional sizing: a small closed noun set exercising sound
and broken plurals, the dual with a written numeral, the sun/moon letter split, and verb agreement
across person/number/gender.

## Residue

- ~~**§1's signature change is owed and is not scheduled.**~~ **Landed 2026-08-14** as ROADMAP.md
  Phase 7 item 11, scheduled by the item 4 addendum below. `Ranting::inflect` now takes
  `count: Option<PlaceholderCount>`, and this spike's own `ArNoun` is the acceptance test
  (`tests/ranting/third_number.rs`): `{$0 1}` with `n = 2` renders `kitaban`, and
  `"{$0 1} and {+1}"` renders the dual once rather than twice — the `Cell` hack's failure mode,
  pinned so it stays fixed. The crate's answer to "can a fork render a third number?" is now
  "yes, including on the noun".
- **Unrelated defect found while probing** (filed as `docs/architecture-review-2026-08-14.md`
  §1.5): `{?the noun}` — the documented "hide the article unless the entity wants one" syntax —
  renders literal garbage (`"?thes dog"`) for any noun whose `skip_article()` is `false`. It works
  only for `no_article = true` entities, which is the only case any test or doc example exercises.
- **`{?$n noun}` leaves a double space** (`"I see  boots"`), currently *pinned* by
  `tests/ranting/numeral.rs` rather than flagged. Recorded in the Japanese spike, where it matters
  more.

---

# Addendum: the item 4 build decision (2026-08-14)

ROADMAP.md Phase 7 item 4 is a synthesis step, not a spike, and item 4's own instruction is that
it be written as an addendum to whichever spike doc is richer rather than as a new document. This
is that addendum; the Japanese spike carries a pointer to it. It reads items 1, 2 and 3 together
and decides what gets built.

**Verdict: build both, in this order — the `Ranting::inflect` count change first, then
`ranting-ar`, then `ranting-ja`.** Neither spike's own recommendation is the decision, and both
survive being checked against the bar rather than adopted; the ordering below is the part item 4
actually settles, because the blocking relationship the Arabic spike asked for did not exist
anywhere in the plan.

## The bar, and how each language clears it

The bar items 10 and 23 set implicitly: build a language only when its spike found *falsification
value existing forks cannot supply* — not "another working example."

- **`ranting-ar` clears it on a live defect in a shipped signature.** `Ranting::inflect` takes
  `to_plural: bool`, so a counted noun cannot render a third morphological number even though
  item 14 gave every agreeing hook a `count`. No Indo-European lexicon can reach this: German and
  Spanish have no third number to ask for. It also becomes `elide_article_custom`'s first real
  user — item 1 found that hook overridden by neither fork.
- **`ranting-ja` clears it on two axes**, one of which needs stating carefully. The defect axis is
  §1's numeral-noun separator: a hard-coded space no hook can remove, with no workaround at all.
  The other is a *confirmation* — `NarrationContext.register` is read for real, and it passes —
  and a confirmation sits uncomfortably close to the "another working example" the bar rejects.
  What makes it clear the bar is item 1: `register` has been inert since Phase 3, and an audit
  from inside the repo structurally cannot settle whether an unused hook's shape is right. Only a
  fork that had to use it can.

**That third axis — item 1 — is what makes the timing non-arbitrary, and it applies to both.**
The audit's own framing: publishing freezes the trait, removing a method after 0.3.0 is a
breaking change, and right now it is free. Each lexicon converts a never-exercised surface into an
exercised one before the freeze. `ranting-ar` does it for `elide_article_custom`; `ranting-ja`
does it for `register`. Building neither does not leave the surface unjudged — it freezes it
unjudged.

Two of item 1's ten never-overridden methods stay never-overridden after both builds: the eight
`_with_context` twins as a class (a fork overriding only the twin is the documented sufficient
shape, so this is expected rather than alarming) and `is_first_person_subject_custom`, which
neither Arabic nor Japanese has a reason to reach. Phase 7's success criteria already admit that
outcome; it should be stated in the phase rather than discovered at freeze time.

## Why `ranting-ar` blocks on a fix and `ranting-ja` does not

This is the one judgment call worth making explicit, because it inverts the repo's own precedent.
The established order is *record the hole, then fix it* — `ranting_i18n`'s hole 1 recorded
`say_with!()` as unreachable downstream and item 12 closed it afterwards. Arabic asks for the
opposite, so the exception needs justifying rather than adopting silently.

The discriminator is **whether the gap has a workaround the lexicon would be forced to encode**:

| | Arabic dual | Japanese separator |
|---|---|---|
| Workaround available | yes — the `Cell` side-channel, or omit the dual | **none** |
| What `tests/holes.rs` would record | the workaround, or nothing | the gap, honestly |
| Blocks its build item | **yes** | no |

A hole test is only worth writing when it pins what the crate *actually renders* against a gap
that is really there. Arabic has two ways out and both poison that: the `Cell` hack contaminates
later placeholders in the same template and makes a `&self` trait stateful, and omitting the dual
records nothing at all. Japanese has no way out — the wrong character is simply in the output — so
its hole test says exactly what the gap is, and `ranting-ja` can be built against today's code.

The precedent therefore holds where it was set: a hole gets recorded first *when recording it is
honest*. It is the availability of a dishonest recording, not the mere existence of a fix, that
moves Arabic's fix in front of its build.

## Scope, confirmed

- **`ranting-ar`**: unchanged from the ROADMAP's provisional sizing.
- **`ranting-ja`**: **smaller** than the ROADMAP's provisional sizing. Japanese leaves six of the
  eight hook pairs untouched (§4), so scoping it like German's would be padding: a small noun set
  with classifiers, teineigo verb forms driven by `register`, and one `ask!()` audience over
  spaced command-style input to pin §3's narrowing. Its `README.md` should record the six unused
  pairs as a finding — clean degradation is the intended shape and Japanese is the evidence — not
  as holes.

## What this decision schedules

Four ROADMAP items, three of them new. Without them the decision above would name a blocking
change that exists nowhere in the plan.

1. **New item 11** — `count: Option<PlaceholderCount>` on `Ranting::inflect`. Blocks item 5.
2. **New item 12** — the numeral-side separator, matching `elide_article_custom`'s treatment.
   Does *not* block item 6; item 6's hole test is what justifies it.
3. **New item 13** — the two small residues both spikes leave: `{?$n noun}`'s double space, which
   is currently *pinned* by `tests/ranting/numeral.rs` and so reads as intended behavior, and
   `NarrationContext`'s "story-wide" wording, which the Japanese spike showed is a description of
   intended use that the type does not impose.
4. **Items 5 and 6** lose "provisional" and record the scope above.

Item 4 ends here. Neither implementation is started by this document.

# Audit: which `Ranting` hooks has a real fork ever used?

**Status**: complete, doc-only, no code changes. ROADMAP.md Phase 7 item 1.
Re-runnable via `scripts/hook_audit.sh`, which reads the method list from the
trait itself and discovers fork crates by globbing, so it does not go stale
when either changes.

## Why this is worth measuring

A hook that ships with an English-preserving default always compiles and always
passes its tests, whether or not its shape matches a real need. Nothing about
"it defaults correctly" is evidence that its parameters are the right ones. The
only real evidence is a fork that had to use it.

Scope is the two downstream falsifier crates, which depend on `ranting` alone.
`src/` and `tests/ranting/` are excluded on purpose: a main-crate test
exercising a hook proves the plumbing works, not that a language needed it.

**This audit is timely rather than routine.** Publishing freezes the trait:
removing a method after 0.3.0 is a breaking change, and right now it is free.

## Result 1: ten of twenty-three methods have never been overridden

```
Never overridden by any fork (10 of 23):
  capitalize_with_context
  elide_article_custom
  elide_article_custom_with_context
  inflect_adjective_custom_with_context
  inflect_article_custom_with_context
  inflect_numeral_custom_with_context
  inflect_preposition_custom_with_context
  inflect_pronoun_custom_with_context
  inflect_verb_custom_with_context
  is_first_person_subject_custom
```

That is **all eight `_with_context` methods**, plus `elide_article_custom` and
`is_first_person_subject_custom`.

> **Correction to ROADMAP.md item 1**, which says "any of the twelve
> `_with_context` methods". There are **eight**, not twelve — one per
> `_custom` pair (seven) plus `capitalize_with_context`. CLAUDE.md states
> eight correctly.

`elide_article_custom` does appear once outside `src/`, in
`ranting_i18n/tests/holes.rs` — but as a **negative probe**: a wrapper type
whose override would signal if it were reached, used to assert that the hook is
*not* called for a zero-length article. That is the opposite of a use.

## Result 2: the stronger finding is at the parameter level

```
  count            declared=16  ignored=14  used=2
  sentence_start   declared=1   ignored=1   used=0
  degree           declared=2   ignored=2   used=0
  case             declared=15  ignored=5   used=10
  class            declared=20  ignored=5   used=15
  as_plural        declared=16  ignored=6   used=10
  uc               declared=15  ignored=0   used=15
  style            declared=2   ignored=0   used=2
```

> **Re-run 2026-08-14, after Phase 7 item 12 added an eighth `_custom` pair: the
> list reads nine.** It grew by exactly one, and only by its `_with_context`
> twin — `elide_numeral_custom` itself was overridden by `ranting_ja` in the
> commit that created it, so it never spent a day unexercised. That is the
> discipline this audit argues for, applied prospectively: the hook was built
> *because* a fork had already shipped the wrong output for want of it, not on
> spec. The twin joining the list is the predicted class, not a new finding.

> **Re-run 2026-08-14, after `ranting_ja` (Phase 7 item 6): the list is eight.**
> `inflect_verb_custom_with_context` is off it — the **first `_with_context`
> twin any fork has ever overridden**, and the first real read of
> `NarrationContext::register`, which had been inert since Phase 3. Japanese
> keigo is the case this audit could not construct from inside the repo: German
> and Spanish politeness is a pronoun slot, so `register` had nothing to do in
> either. Result 1's "all eight `_with_context` methods" is therefore no longer
> true; seven remain, plus `is_first_person_subject_custom`.
>
> Note what the crate did *not* need: only the `_with_context` twin is
> overridden, and the plain hook's default delegation carries `say!()` calls
> unchanged. That is the documented sufficient shape, relied on by a real fork
> for the first time.

> **Re-run 2026-08-14, after `ranting_ar` (Phase 7 item 5) landed: the list is
> nine, not ten.** `elide_article_custom` is off it — this audit's headline
> example of "built for `l'homme`, never used by a language that needs it" now
> has a real consumer, and the hook's design came through intact (the sun-letter
> trigger is `following.chars().next()`, and dropping the separator is what the
> replace-all-three contract already allowed). That is item 4's stated
> justification for building at all, measured rather than predicted. The nine
> that remain are the eight `_with_context` twins as a class and
> `is_first_person_subject_custom`; ROADMAP.md item 4 says in advance why both
> are expected to stay unused, so the audit's question is now answered rather
> than open.
>
> The hook also turned out to be **unreachable in practice for a non-ASCII
> article** until the same day: it panicked. See
> `docs/architecture-review-2026-08-14.md` §1.7. A hook with no user is not
> merely unvalidated in its *shape* — it can be outright broken on a path no
> gate reaches, which is a stronger version of this audit's own thesis than it
> made.

> **Re-run 2026-08-14, after Phase 7 item 11** widened `Ranting::inflect` with a
> `count`: `count` reads `declared=20 ignored=18 used=2`, `case`
> `declared=19 ignored=8 used=11`, `uc` `declared=19 ignored=1 used=18`. The four
> new declarations are the four fork `inflect` overrides, all `_count`, which
> **strengthens** the finding below rather than dating it: item 11 was justified
> by a spike, not by a fork that had already reached for it, and the forks that
> now carry the parameter still have no use for it. Result 1's headline is
> unchanged — `inflect` was already overridden by both forks. The numbers above
> are kept as the reading item 4's decision was made on.

A never-reached hook and a reached-but-ignored parameter are different kinds of
evidence, and the second is stronger. A fork that never overrides
`elide_article_custom` simply had no elision to express. A fork that *does*
override `inflect_article_custom` and writes `_count` had the parameter in front
of it, in a hook it needed, and found nothing to do with it.

### `count` — item 14's ten-method signature break has zero uses

Phase 6 item 14 added `count: Option<PlaceholderCount>` to five hook pairs — ten
methods in one commit (`6819ef02`) — as the owed channel from the
number-categories spike. Across both forks, **all fourteen declarations of it in
those hooks are `_count`.**

The two live uses are both `inflect_numeral_custom`, which item 14 **deliberately
excluded** because it already carried its own richer `count: Option<i64>` from
item 8. So every count that is actually read came from item 8; item 14's own
break has not been used once.

This does not mean the channel is wrong — German's `два`/`две` example shows a
count *can* drive agreement. It means the shape has never been confirmed by a
fork that needed it, while costing a ten-method break plus updates to every call
site, both falsifiers, and every worked example in `docs/EXTENSIBILITY.md`.

### `sentence_start` and `degree` — declared once or twice, ignored

`sentence_start` (item 17) appears in exactly one fork override, German's
`capitalize`, as `_sentence_start`. `degree` (item 5) is ignored by both
adjective hooks — though this one was **predicted**: `docs/EXTENSIBILITY.md`
§2.5 already says an agreement-only fork ignores `degree`, and `ranting_es`'s
source comment says so explicitly. A documented, expected non-use is not the
same finding as `count`'s.

### What is earning its place

`uc` (15/15), `style` (2/2), `class` (15/20), `case` (10/15) and `as_plural`
(10/16) are all genuinely read. The core five — `name`, `subjective`,
`is_plural`, `inflect`, `skip_article` — are overridden by every noun type in
both forks. `noun_class`, and the article/pronoun/verb/adjective/numeral/
preposition `_custom` hooks, are overridden by both forks. The hook surface is
not uniformly speculative; the unused part is concentrated.

## Result 3: `capitalize` is used, by one fork, for the reason it was built

German overrides `capitalize` (`ranting_i18n/src/noun.rs:326`) to return
`OrthographyRole::Noun` words untouched, because German capitalizes nouns
wherever they stand. Spanish does not override it. This is the clearest example
in the audit of a hook whose shape was confirmed by a real need — and note that
it uses `role`, the parameter that is genuinely load-bearing, while ignoring
`sentence_start`.

## What this does and does not license

**It does not license deleting the `_with_context` methods.** They are default
methods delegating to their non-context twins; they cost nothing at runtime, and
their zero-override count has a mundane explanation — neither fork models
story-wide register or dialect, because neither was built to. Deleting them
would remove the only channel `NarrationContext` has into a fork.

**It does license three narrower conclusions:**

1. **The `_with_context` layer is unvalidated, and it is 35% of the trait.**
   Eight of twenty-three methods exist to carry a context object no fork has
   ever branched on. Before publishing freezes this, it is worth deciding
   whether that layer is a design commitment or an unexercised guess.
2. **`count` should not be widened further without a fork that needs it.** Its
   first ten-method break went unused; a second, larger one (a category rather
   than a count) would be guessing twice.
3. **Non-use has two causes, and only one is a defect.** `elide_article_custom`
   is unused because neither German nor Spanish elides — French does, and the
   hook was built for `le`+`homme`→`l'homme`. Its shape is untested, not wrong.
   That is a reason to weigh a candidate language by which unused hooks it would
   finally exercise, which is exactly the input items 2 and 3 asked for.

## Input to items 2 and 3 (Arabic, Japanese)

A candidate language earns priority partly by which unused hooks it would be the
first to exercise:

| Unused surface | Which candidate would exercise it |
|---|---|
| `count` on the five non-numeral pairs | **Arabic** — dual number is the case item 14 was built for |
| the eight `_with_context` methods | **Japanese** — register-driven honorifics are story-wide state, which is what `NarrationContext` is |
| `elide_article_custom` | neither; this wants French or Italian |
| `is_first_person_subject_custom` | either, if its lexicon labels first person natively |

That `count` and `_with_context` map onto the two candidates already drafted is
a point in favour of both spikes — each would be the first real test of a
distinct unvalidated layer.

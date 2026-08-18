# Design spike: an ordinal channel for the numeral slot

**Status**: design spike complete; conclusion is **a doubled numeral marker,
`##var`, baked as a new `NumeralKind::Ordinal` and mirrored into a new public
`NumeralStyle::Ordinal` variant — not implemented by this document**.
ROADMAP.md Phase 8 item 4. The sigil grammar is Locked (ROADMAP.md's Key
Architecture Decisions table) and `NumeralStyle` is a public, non-exhaustive-less
enum whose every downstream `match` is exhaustive, so this spike answers the
scoping questions a maintainer needs in order to rule on the change, and stops
there — no code in this repository is changed by it.

Second Phase 8 spike, following the shape of the Phase 6/7 spikes in this
directory and of its immediate sibling
`2026-08-15-verbatim-verb-marker.md` (ground the question in the code as it
stands, survey options including the rejected ones, score them, state a
recommendation and what stays out of reach under it).

## The question

`say!("This is {the #n attempt}.", 3, attempt)` renders `"This is the three
attempts."`. There is no way to write `"This is the third attempt."` — `#var`
spells cardinals and only cardinals. The caller's only option today is to keep
the ordinal out of the placeholder entirely (`say!("This is the third
{attempt}.")`), which works but hard-codes the number, defeating the point of
having a numeral slot at all.

This is pure word-form inflection with no word movement — the ordinal occupies
exactly the position the cardinal already occupies, between the article and the
noun — so it is squarely inside the crate's boundary (ROADMAP.md's "Word order
lives in the literal template", Phase 6 item 1). Nothing here re-opens that
decision.

### The two channels, and which one is at issue

English writes ordinals two ways: spelled (`third`) and digit-plus-suffix
(`3rd`). They map onto the crate's two existing numeral notations exactly —
`#var` spells, `$var` prints digits — so an ordinal channel is not one feature
but potentially two. This spike recommends **one** (the spelled ordinal, the
one Phase 8 item 4's own framing names) and records the digit form as an option
to bundle into the same semver break if a maintainer wants it. Adding it
*later* would be a second break on the same enum, which is the outcome to
avoid.

## What the code does today

### The numeral slot, end to end

The relevant path, read from source rather than from memory:

| Site | What it does |
|---|---|
| `ranting_core/src/grammar.rs:138` | `PH_EXT`'s `` (?P<nr>[+-]\|(?:\#\|\??\$)\w+\s+)?+ `` — the reference grammar for the numeral slot |
| `ranting_core/src/ph_ext.rs:603` | `match_nr`, the hand-written parser `ranting_derive` actually calls, mirroring the line above by hand |
| `ranting_derive/src/lib.rs:843-896` | bakes `NumeralSpec { kind, leading_space, hidden }` plus, for `#` only, `count_expr = Some(x as i64)` |
| `ranting_core/src/placeholder.rs:202` | `NumeralKind { Words, Digits }`, the compile-time type |
| `src/lib.rs:1705` | `NumeralStyle { Words, Digits }`, the public mirror, plus the `From` impl at `:1713` |
| `src/lib.rs:717-770` | the render: spell (`spell_count`) or reuse the formatted digits, hand both to `inflect_numeral_custom_with_context`, then claim the sentence capital (§1.11's fix) |
| `src/lib.rs:944` | `elide_numeral_custom`'s post-assembly splice at the numeral-noun boundary |

Two properties of that path matter for everything below.

**The count is real and already flows.** `#var`'s argument is cast to `i64` by
the macro (`ranting_derive/src/lib.rs:878`) and handed to
`handle_placeholder` as `count: Option<i64>`; `inflect_numeral_custom` receives
it directly, and every other hook receives it as `PlaceholderCount`
(`src/lib.rs:540`). An ordinal channel needs no new count plumbing whatsoever —
that is the single largest reason to build it on `#` rather than beside it.

**Spelling is a runtime concern, replaceable wholesale.** Since Phase 6 item 8
the macro bakes the count, not the word; `src/lib.rs:726` calls the crate's own
`spell_count` and `inflect_numeral_custom` may replace the entire resulting
string. An ordinal speller therefore lives on the same side of the seam as the
cardinal one, and a fork overrides it the same way.

**`#` and `$` are not symmetric.** `?` (hidden) prefixes `$` only —
`` \??\$ `` in `PH_EXT`, and `match_nr`'s explicit `starts_with('$')` check at
`ranting_core/src/ph_ext.rs:611-616`. There is no hidden `#` today, so there
would be no hidden ordinal either; making one would be a further widening and
is not recommended here.

### The taken-marker inventory

`.claude/rules/placeholder-grammar.md` is the map of this grammar's traps; this
is the literal inventory it asks for, enumerated from
`ranting_core/src/grammar.rs`'s `PH_EXT` and `ranting_core/src/ph_ext.rs`'s
matchers. It is the same inventory the verbatim-verb spike took, re-derived
here rather than cited, because the two spikes are competing for the same
scarce resource:

| Group | Characters | Meaning | Source |
|---|---|---|---|
| `uc` | `,` `^` | force-lowercase / force-uppercase | `uc_one_rep`, `ph_ext.rs:561` |
| `pre` | `a`/`an`/`some`/`the`/`these`/`those`, `` ` ``-prefixed word, modal words, `?` prefix, plus one arbitrary literal word (Phase 6 item 26) | article/modal detection, preposition-fusion input | `pre_one_rep`, `ph_ext.rs:491` |
| `nr` | `+` `-` `#` `$`, `?` as a prefix to `$` only | force-plural / force-singular / spelled numeral / digit numeral / hidden numeral | `match_nr`, `ph_ext.rs:603` |
| `case` | `` ` `` `=` `@` `~` `*` `?` `%`, plus the fused `` *` `` `*=` `*@` `*~` `*%` (Phase 6 item 19) | possessive substitution / subjective / objective / possessive-determiner / display-as-name / hidden / reflexive | `is_case_char`, `is_real_case_char`, `ph_ext.rs:635-644` |
| `post` (prefix) | `<` `=` `>` `%` `!` and the combos `<=`, `<%`, `!!` | tense (`TenseMarker`) or degree (`DegreeKind`) | `match_post`, `ph_ext.rs:671` |
| `post` (suffix) | `'` | possessive-`s` | same |

Union of characters already meaning something somewhere:
`` , ^ + - # $ ` = @ ~ * ? % < > ! ' `` — seventeen, several already carrying
two meanings depending on which side of the noun they appear.

Unclaimed and usable: `;` `|` `&` `/` `(` `)` `[` `]`. Disqualified or
awkward, for the reasons the sibling spike sets out in full: `{`/`}` (the
delimiters), `:` (structurally excluded by `PH_START`'s `[^{}:]*+` inner
capture, which reserves `:` for the `:fmt` spec), `_` (inside `\w`, so
ambiguous with any identifier), `"`/`\` (legal but need escaping inside the
Rust string literal a template is), `.` (free inside `PH_EXT` but the primary
sentence terminator in `PH_START`/`SENTENCE_TRIGGER_CHARS` just outside the
braces).

**The free list is short and there are two open claims on it.** The
verbatim-verb spike shortlists `;`/`|`/`&`/`/` and recommends `;`. This spike
deliberately does not compete for that character — see the recommendation
below, which spends none of the free list at all.

## Options for the syntax

### (a) A doubled numeral marker, `##var` — recommended

`##var` in the `nr` slot: same argument, same count, same slot, rendered as an
ordinal instead of a cardinal.

**It is backward-compatible by construction.** `match_nr` at
`ranting_core/src/ph_ext.rs:609-626` reads `#`, then requires
`leading_word_len` > 0 on what follows. `#` is not `\w`, so `##n ` fails to
match today, and it cannot match as *two* `nr` repetitions either: the first
repetition would itself have to consume `#` + word + whitespace, and it cannot.
So no template that compiles today changes meaning, and no template that
compiles today is rejected. This is the same property that made Phase 6 item
19's fused `*=` family safe.

**It composes with the notation axis instead of consuming a character.** The
ordinal is not a *different* channel: it is the same slot, the same argument,
the same count, the same post-assembly splice, differing only in how the
number is rendered. A standalone character would have to either pick one
notation (spelled or digits, arbitrarily) or spend *two* characters out of a
free list of eight that a sibling spike is already drawing from. Doubling
composes: `##` is the spelled ordinal and `$$` is the digit ordinal, should a
maintainer take the second.

There is a weak mnemonic available — `!`/`!!` already establishes doubling as
"same axis, next value" — but it is genuinely weak, since an ordinal is not
"more cardinal" the way a superlative is "more comparative". The argument
above is structural and does not lean on it.

### (b) A new standalone character in `nr` — rejected

`;`/`|`/`&`/`/` etc. as an ordinal marker, e.g. `` {the |n attempt} ``. Clean
by the disqualification criteria, and it would parse. Rejected on two counts:
it either abandons the digit ordinal permanently or costs two of the eight free
characters, and it obscures the relationship to `#`/`$` for a reader — the
inventory table above already shows what positional overload costs this grammar
in comprehensibility, and this would be the inverse mistake, spending a fresh
character on something that is a variant of an existing one.

### (c) A `:fmt`-style suffix — rejected, structurally impossible

`` {the #n:ord attempt} `` looks attractive because `:fmt` already exists. It
cannot work: `PH_START` splits `(?P<fmt>:.*?)?` off *before* `PH_EXT` sees the
placeholder, and `ranting_derive/src/lib.rs:805-826` partitions the fmt
segments into "number formatting" and "value formatting" and forwards them to
`format!()`. An `ord` segment is neither; it would have to be intercepted and
removed there, which puts a grammar decision inside the format-spec splitter,
and `#var` rejects number formatting outright anyway
(`ranting_derive/src/lib.rs:867-874`). Recorded because it is the first thing
anyone proposes.

### (d) Do nothing; the caller writes the ordinal as literal text — the status quo

`say!("This is the third {attempt}.")` works today and needs no crate change.
It is not a fix for the gap: the number becomes a compile-time constant, so
every ordinal a caller might render needs its own template, which is precisely
what the numeral slot exists to avoid. Recorded because it is the no-change
option every spike in this directory considers, and because for a caller with
one or two fixed ordinals it is genuinely the right answer.

## How it reaches `inflect_numeral_custom`

**Recommendation: a new `NumeralStyle::Ordinal` variant**, mirrored from a new
`NumeralKind::Ordinal`, carrying the same real `count: Option<i64>` the
cardinal channel already carries.

```rust
// ranting_core/src/placeholder.rs
pub enum NumeralKind { Words, Digits, Ordinal }

// src/lib.rs
pub enum NumeralStyle { Words, Digits, Ordinal }
```

Everything else about the hook is unchanged: same signature, same
`count`/`case`/`class`/`as_plural` parameters, same "return `Some` to replace
the whole rendered string" contract, same `uc` policy (the crate never passes
`uc` to a numeral hook; §1.11's fix capitalizes the crate's or the fork's
returned string afterwards, at `src/lib.rs:753`).

### This is a semver-visible break, and here is what it breaks

`NumeralStyle` is `pub`, re-exported from `ranting`, and is **not**
`#[non_exhaustive]` (`src/lib.rs:1704-1711`). Adding a variant is therefore a
breaking change for every downstream `match` on it, exactly as ROADMAP.md's Key
Architecture Decisions table already records for `SubjectPronoun` and
`GrammaticalCase`.

The blast radius is not hypothetical — **all four falsifier crates in this
repository match it exhaustively, with no wildcard arm**:

- `ranting_i18n/src/noun.rs:288-290`
- `ranting_es/src/noun.rs:209-214`
- `ranting_ar/src/noun.rs:254-257`
- `ranting_ja/src/noun.rs:142-149`

Each would stop compiling the moment the variant lands, with
`E0004: non-exhaustive patterns: NumeralStyle::Ordinal not covered`. That is
the *good* case — a compile error naming the exact site. Any downstream crate
outside this repository gets the same error, which is a major-version bump for
`ranting`, not a minor one.

Two mitigations exist and both are worse:

- **Marking the enum `#[non_exhaustive]` first.** Adding `#[non_exhaustive]`
  is itself a breaking change (it forces a wildcard arm on every existing
  exhaustive `match`), so it does not avoid the break — it only moves it
  earlier and permanently costs every fork a `_ => None` arm that silently
  swallows future variants. Silently swallowing is the failure mode this repo
  consistently rejects; the falsifier contract exists to make missing coverage
  *visible*.
- **A separate `ordinal: bool` parameter on the hook instead of a variant.**
  That is a hook-signature break, which is strictly larger: it breaks every
  implementor of the hook rather than only those matching on the style, and
  `.claude/rules/extension-hooks.md` records that this repo bundles signature
  breaks rather than shipping them piecemeal.

So the recommendation is the variant, taken as a deliberate major-version
change, with the four in-repo falsifiers updated in the same commit — which is
also how they earn their keep here, by turning an API decision into four
concrete compile errors at four real call sites.

**If the digit ordinal is wanted, it must land in the same break.** `$$var` →
`NumeralStyle::OrdinalDigits` (`"3rd"`) is a second variant and therefore a
second major bump if deferred. Decide both at once; take one or take two, but
do not take one now and one later.

## The English rules

Two distinct rule sets, one per notation.

### Spelled ordinals (the `##` channel)

The crate's cardinal speller is `spell_count` (`src/lib.rs:59`), which wraps
the `english-numbers` crate's `convert_no_fmt` (the only item this repo
re-exports from it, `src/lib.rs:52`) and prefixes `"minus "` for negatives
(§1.9's fix). An ordinal speller is crate-local either way: it spells the
cardinal, then rewrites the **last word** of the result.

| Cardinal ends in | Ordinal | Note |
|---|---|---|
| `one` | `first` | suppletive |
| `two` | `second` | suppletive |
| `three` | `third` | suppletive |
| `five` | `fifth` | stem change |
| `eight` | `eighth` | one `t`, not `eightth` |
| `nine` | `ninth` | drops the `e` |
| `twelve` | `twelfth` | stem change |
| `-y` (`twenty`, `thirty`, …) | `-ieth` (`twentieth`) | |
| anything else | `+th` (`fourth`, `sixth`, `hundredth`, `millionth`) | |

Applied to the last word, so `"one hundred one"` → `"one hundred first"` and
`"four"` → `"fourth"`.

**Inherited spelling quirk, named in ROADMAP.md item 4 itself.** Upstream
spells 21 as the unhyphenated single word `"twentyone"`, not `"twenty-one"`.
Because it is one token, the last-word rule sees `twentyone`, ends in `one`,
and yields `"twentyfirst"` — internally consistent with the cardinal spelling
and equally unhyphenated. An ordinal speller does not create this question and
should not try to fix it; whoever decides the hyphenation question decides it
for both channels at once.

**Negatives.** `spell_count(-3)` is `"minus three"`; the last-word rule gives
`"minus third"`, which is not English anyone writes. There is no correct
answer here — a negative ordinal is not a thing English has — so the sensible
behavior is the one already established for the negative cardinal: render
something deterministic and non-panicking, and let a fork's hook replace it
wholesale. `i64::MIN`'s pre-existing upstream panic is unchanged either way.

### Digit ordinals (the `$$` channel, if taken)

The suffix is chosen from the **last two digits**, not the last one — this is
the teens exception, and it is the rule most naive implementations get wrong:

1. If the last two digits are `11`, `12` or `13` → `th`
   (`11th`, `12th`, `13th`, `111th`, `212th`, `1013th`).
2. Otherwise, by the last digit: `1` → `st`, `2` → `nd`, `3` → `rd`, anything
   else → `th` (`1st`, `2nd`, `3rd`, `4th`, `21st`, `22nd`, `23rd`, `101st`,
   `111th`).

Checking the last digit alone gives `"11st"`, `"12nd"`, `"13rd"`. Note also
that the digits arrive as an already-formatted `String` on the `$` path
(`src/lib.rs:727`), with any `:fmt` width applied, and the count is recovered
by `parse::<i64>()` (`src/lib.rs:734`) — which returns `None` for a float or a
padded value. A digit-ordinal implementation has to decide what `{$$n}` does
when that parse fails; the honest answer is "render the digits unsuffixed",
matching the existing "`count` is `None`, agree from what we have" posture.

## Agreement: `as_pl` and `count` decouple

This is the design point most likely to be got wrong by analogy, and it
produces grammatical-looking wrong output rather than an error.

For a cardinal, the count decides the noun's number: `src/lib.rs:530`'s
`"#" => count != Some(1)`. For an ordinal it does not. `"the third attempt"` is
singular even though the count is 3; `"the third attempts"` is wrong. The
ordinal says *which* one, not *how many*.

So:

- **`as_pl` for `##` should fall through to `noun.is_plural()`** — the same
  arm a bare `{noun}` takes (`src/lib.rs:521`). The placeholder's numeral no
  longer speaks to number, so the entity's own declared plurality is the only
  remaining signal. Note that `+`/`-` are *alternatives* to `#`/`$` in the `nr`
  slot, not combinable with them, so a caller cannot override this with `{+…}`;
  if that turns out to matter, it is a separate grammar question, not part of
  this one.
- **`placeholder_count` must still carry the real value** (`src/lib.rs:540`).
  Spanish and Arabic ordinals agree in gender *and* number with their noun, and
  the value is also what a fork needs to spell the ordinal at all. Dropping it
  would make the ordinal useless to exactly the two forks that motivate the
  second half of Phase 8 item 4.

That decoupling — bool one way, count the other — is the reason the two
channels cannot share a single arm in either match.

## What the second constituency buys

ROADMAP.md item 4's second argument is that ordinals agree in gender in Spanish
and Arabic, so `ranting_es`/`ranting_ar` gain something to override. That is
right, and worth stating precisely, because the item's phrasing — "against the
never-exercised surface §4.1 records" — is looser than §4.1 itself.

**What §4.1 actually records** (`docs/architecture-review-2026-08-15.md:427`):
the nine never-overridden methods are exactly the eight `_with_context` twins
plus `is_first_person_subject_custom`. Plain `inflect_numeral_custom` is *not*
in that set — all four falsifiers override it, as the line references above
show. An ordinal channel therefore does **not** by itself drive a fork onto a
`_with_context` twin, and claiming it closes §4.1 would be wrong.

What it does buy is sharper than that, and is the same *kind* of gap:

- **A first gender-agreeing numeral.** Spanish cardinals barely agree —
  `uno`/`una`, and that is it, which is why `ranting_es`'s
  `lexicon::spell` takes `class` mainly for the `1` case
  (`ranting_es/src/noun.rs:211`). Spanish *ordinals* agree fully:
  `primero`/`primera`, `segundo`/`segunda`, `tercero`/`tercera`, plus plural
  forms. Arabic is the same shape and stronger: `أوّل`/`أولى`,
  `ثالث`/`ثالثة`, agreeing in gender and taking the definite article. So the
  `class: NounClass` parameter on `inflect_numeral_custom` — today exercised by
  one arm of one branch in Spanish and by the counter lookup in Japanese —
  finally gets a case where it is load-bearing across the whole numeral range,
  not just at `1`.
- **A first case where `count` and `as_plural` genuinely disagree.** Per the
  section above, an ordinal hands a hook `count = Some(3)` with
  `as_plural = false`. Nothing in the repo produces that combination today, and
  it is exactly the kind of shape the falsifier contract exists to test.
- **Spanish apocope, inside the existing hook.** `primero` → `primer` before a
  masculine singular noun (`el primer intento`), and `tercero` → `tercer`
  likewise. This is *not* a reason to expect `ranting_es` to become
  `elide_numeral_custom`'s second user: `inflect_numeral_custom` already
  receives `&self` (the noun the numeral counts), its `class`, and
  `as_plural`, so the apocope decision is fully determined inside that hook
  with no post-assembly splice needed. Worth recording precisely because the
  post-assembly hook looks like the natural home for it and is not.

None of this is a *new* hook. That is the point in its favor: the second
constituency is bought with zero new surface, only a new arm on a hook every
fork already implements.

## Cost: every site that must change, and which fail silently

The verbatim-verb spike's parity table has a direct analogue here, and it is
both bigger and sharper — because for `##` two of the sites produce *silently
wrong output* rather than a compile error. This is the honest cost of the
doubled-marker shape and it should be read before the recommendation is
accepted.

Each row states the consequence of missing *that* site while the others are
handled correctly; the sites interact, so a partial widening can reach the same
wrong output by a different route than the one named.

| # | Site | If missed |
|---|---|---|
| 1 | `ranting_core/src/grammar.rs:138` — `PH_EXT`'s `nr` group | `parity_fuzzed` catches drift against site 2 |
| 2 | `ranting_core/src/ph_ext.rs:603` — `match_nr` | as above; the parser is what `ranting_derive` calls |
| 3 | `ranting_derive/src/lib.rs:857` — `plurality.contains('#')` → `Words` | **silent**: `"##"` contains `'#'`, so it bakes a plain cardinal and `##` renders "three" |
| 4 | `ranting_derive/src/lib.rs:867` — `plurality == "#"` (exact) | **silent**: `"##"` takes the `else` branch, so `count_expr` stays `None` and `nr` is baked as formatted digits — the ordinal renders as digits and no hook ever sees the count. (Formatting is still rejected, by the *other* guard at `:886`'s `plurality != "$"`, so `` {##n:03 attempt} `` does error — just not from this line.) |
| 5 | `src/lib.rs:520` — `as_pl`'s `"#" =>` arm (exact) | **silent, and wrong English**: `"##"` falls to `_`, which reads `nr` — and `nr` is `String::new()` on the spelled path, so `"" != "1"` gives `as_pl = true` → "the third attempts." |
| 6 | `src/lib.rs:540` — `placeholder_count`'s `"#" =>` arm (exact) | **silent**: `"##"` falls to `_`, parses `""`, yields `None` — every agreeing hook silently loses the count, which is precisely what Spanish/Arabic ordinal gender agreement needs |
| 7 | `src/lib.rs:725` — the render `match kind` | compile error (non-exhaustive match) |
| 8 | `src/lib.rs:755` — §1.11's capital-claiming `match kind` | compile error, but note it is a *second, separate* match on the same value: miss its intent and a sentence-initial ordinal silently loses its capital |
| 9 | `ranting_core/src/placeholder.rs:202`, `src/lib.rs:1705`, `src/lib.rs:1713` | the two enums and the `From` impl between them |
| 10 | the four falsifiers' `match style` | compile error each — the good case |

Sites 3-6 are the same category the verbatim spike names at its own site 3:
downstream of `ph_ext::parse` having already matched, therefore outside
`parity_fuzzed`'s coverage entirely. There it was one site and one visibly
wrong render. Here it is four, two of which (5 and 6) produce output that looks
grammatical and is wrong — the failure mode this repo's own review history
(CLAUDE.md's "green gates are necessary, not sufficient") keeps rediscovering.

Two of the four are string comparisons that differ only in whether they are
`==` or `contains`, which is not a difference anyone reads carefully. **A
maintainer taking this should consider replacing the stringly-typed
`plurality` dispatch with the typed `NumeralKind` that is already baked into
the spec, as part of the same change** — the four sites exist because
`plurality` is a `&str` carrying what `NumeralKind` already carries as a type.
That is a refactor, not a feature, and it is the thing that makes the ordinal
channel's second and third implementations safe rather than only its first.

### One parser trap worth naming

`nr` goes through `star_candidates` (`ranting_core/src/ph_ext.rs:578`), the
same repeated-group engine `pre` uses — and
`.claude/rules/placeholder-grammar.md`'s point 6 is the standing warning:
**a new alternative in a repeated group is not local to that alternative**, and
the capture retains only the last repetition. That is exactly how the open
pre-word pass silently dropped `de` from `` {de the *=gato} ``. Whoever
implements this must check multi-repetition input (`` {##a ##b } ``-shaped),
not only the single-marker case, and decide whether `nr` should be restricted
to one repetition the way `parse_pass` restricts the open `pre` pass.

## Recommendation

Ship **(a)**: `##var` in the `nr` slot, baked as `NumeralKind::Ordinal` and
mirrored into a new public `NumeralStyle::Ordinal`, carrying the same
`count: Option<i64>` the cardinal channel already carries, with `as_pl` falling
through to `noun.is_plural()` and `placeholder_count` still carrying the real
value.

This spike does not implement it. The sigil grammar is Locked and
`NumeralStyle` is public with four exhaustive in-repo matchers, so this is a
maintainer's call. Three things are left for that sign-off:

1. **Whether the digit ordinal (`$$var` → `NumeralStyle::OrdinalDigits`,
   `"3rd"`) is taken.** If it is wanted at all it must land in the same major
   version; deferring it costs a second break on the same enum.
2. **Whether the stringly-typed `plurality` dispatch is retyped in the same
   change.** Four of the ten sites above exist only because it is a `&str`,
   and two of those four fail silently.
3. **Whether `nr` gains a one-repetition restriction** while its alternation is
   being widened, per the `star_candidates` trap above.

## What stays impossible under this recommendation, until implemented

- `say!("This is {the #n attempt}.", 3, attempt)` keeps rendering `"This is
  the three attempts."`; ordinals stay literal template text.
- `ranting_es`/`ranting_ar` keep having no gender-agreeing numeral case beyond
  Spanish's `uno`/`una`, and `class` on `inflect_numeral_custom` stays
  load-bearing only at `1`.
- No hidden ordinal (`?` prefixes `$` only), no ordinal under `:fmt`, and no
  change to any cardinal output — `#var` and `$var` render exactly as they do
  today, which is what keeps the English-byte-identity invariant intact for
  every template that does not opt in.

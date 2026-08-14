# Feasibility: number categories beyond `bool` for non-English `Ranting`

**Status**: design spike complete; conclusion is **(b), in a narrower form
than the ROADMAP scoped it** — keep `as_plural: bool` authoritative and
English-owned, and add a *count* channel (not a category channel) to the
inflection hooks, folded into Phase 6 item 5's already-planned signature
change. No production code is proposed *by this document*; the change it
recommends has to be scheduled as its own item. ROADMAP.md Phase 6 item 4.

This is the third Phase 6 spike. `2026-08-13-word-order-feasibility.md`
(item 1) and `2026-08-13-pronoun-inventory.md` (item 3) are the precedents for
the shape: ground the question in the code as it stands, score the options the
ROADMAP names, and state the residue plainly.

## Motivation

Number is a `bool` at every seam in the crate. Three phenomena the ROADMAP
names do not fit into one:

- **Arabic dual** — a third morphological number beside singular and plural
  (كِتاب / كِتابان / كُتُب), with its own agreement on verbs, adjectives and
  pronouns.
- **Slavic paucal** — Russian/Polish/Czech select a distinct form for the
  2–4 range (Russian *два дома*, not *два домов*), with the 12–14 exception,
  and the numeral additionally governs the noun's case — an axis that
  `GrammaticalCase` currently sources from the placeholder's own `CaseKind`,
  never from a numeral.
- **CLDR plural categories** — `zero`/`one`/`two`/`few`/`many`/`other`, a
  function of the *numeric value* (and, in several languages, of the visible
  fraction digits), not of a singular/plural flag. Arabic and Welsh use all
  six.

Welsh and Irish add number-triggered initial mutation on top, which is a
morphological consequence of the category rather than a separate axis.

The item exists as a spike and not as an implementation task because changing
the representation touches every public hook at once. This document establishes
exactly what "every" means, then scores the three options against the three
phenomena.

## Inventory: every place number is a `bool`

Everything below is code as it stands on `overnight/2026-08-13`, not a
projection.

### The public `Ranting` trait (`src/lib.rs`)

| Site | Signature |
|---|---|
| `src/lib.rs:1077` | `fn is_plural(&self) -> bool` |
| `src/lib.rs:1081` | `fn inflect(&self, to_plural: bool, uc: bool) -> String` |
| `src/lib.rs:1121` | `fn inflect_verb_custom(&self, subject, verb, _as_plural: bool, uc) -> Option<String>` |
| `src/lib.rs:1160` | `fn inflect_verb_custom_with_context(&self, subject, verb, as_plural: bool, uc, ctx)` |
| `src/lib.rs:1194` | `fn inflect_pronoun_custom(&self, subject, case, class, _as_plural: bool, uc)` |
| `src/lib.rs:1211` | `fn inflect_pronoun_custom_with_context(&self, subject, case, class, as_plural: bool, uc, ctx)` |
| `src/lib.rs:1261` | `fn inflect_article_custom(&self, article, noun_singular, case, class, _as_plural: bool, uc)` |
| `src/lib.rs:1279` | `fn inflect_article_custom_with_context(&self, article, noun_singular, case, class, as_plural: bool, uc, ctx)` |

That is the "six `_custom` hooks" the ROADMAP names, plus the two non-hook
trait methods that are the *source* of the bool rather than a consumer of it.
Phase 6 item 5 adds `inflect_adjective_custom`/`_with_context`, which its own
text says will receive "number" — making it **eight** hooks carrying whatever
number type is decided here. That is the concrete argument for deciding now
rather than after item 5 ships: the choice is between one coordinated
signature change and two.

### Where the bool is computed, exactly once

`handle_placeholder_impl` (`src/lib.rs:371-381`):

```rust
let as_pl = match plurality {
    ""  => noun.is_plural(),
    "+" => true,
    "-" => false,
    "#" => nr.trim_start() != "one",
    _   => { let s = nr.trim_start(); s != "1" && s.split('.').next() != Some("1") }
};
```

Every downstream consumer reads that one variable (or `pronoun_as_pl`,
`src/lib.rs:400`, which is `as_pl` with a `narration_person` override applied):

- `ArticleRenderCtx { as_pl, .. }` (`src/lib.rs:198`) → `get_article_or_so`
  → all three `inflect_article_custom_with_context` call sites and the
  `adapt_article` fallback;
- `conjugate_verb` (`src/lib.rs:294-310`) → `inflect_verb_custom_with_context`
  and the `inflect_verb` fallback, at the pre-verb, post-verb and both tense
  paths;
- the five `CaseKind` pronoun arms (`src/lib.rs:479-551`) →
  `inflect_pronoun_custom_with_context` and `inflect_subjective` /
  `inflect_objective` / `inflect_possessive` / `inflect_adjective` /
  `inflect_reflexive`;
- `noun.inflect(as_pl, uc)` for the `Name`/`Hidden` arms
  (`src/lib.rs:552`);
- `adapt_possesive_s(noun, as_pl)` (`src/lib.rs:562`, `:576`, defined at
  `src/lib.rs:862`), which picks `'` vs `'s`.

### The English implementation (`src/language/`)

`inflect_verb` (`english.rs:98`), `adapt_article` (`:140`),
`pluralize_pronoun` (`:164`), `inflect_adjective` (`:265`),
`inflect_subjective` (`:272`), `inflect_objective` (`:279`),
`inflect_possessive` (`:303`, **public**), `inflect_reflexive` (`:332`,
**public**), `inflect_noun_irregular` (`:347`, **public**) — every one takes
`as_plural`/`to_plural: bool`. `inflect_noun_irregular` dispatches on it to
`plurals::get_plural` (`plurals.rs:12`) or `plurals::get_singular` (`:22`);
there is no third table and no place to put one.

`is_subjective_plural` (`ranting_core/src/grammar.rs:130`, **public**,
re-exported at `src/lib.rs:56`) is the ultimate source of an entity's own
number: it maps a subject pronoun to a bool. `Noun` has **no number field** —
its number is derived from its `SubjectPronoun`, so a `Noun` cannot be
"dual" even in principle today.

`conjugate_auxiliary` (`auxiliary.rs:15`) is the exception worth naming: it
takes no number at all, matching on the *subject string* instead — so
`is`/`are`, `was`/`were`, `have`/`has` are selected by pronoun, and a fork's
number category would not reach them even if every other site had it.

### Generated code (`ranting_derive/`) — the expensive surface

- `ranting_derive/src/lib.rs:781` bakes `#expr.is_plural()` into **every**
  possessive-substitution call site, as an argument to
  `ranting::inflect_possessive`. Changing `is_plural`'s return type changes
  emitted code, not just a trait.
- `ranting_impl.rs:118-215` emits `is_plural`/`inflect` bodies in **two**
  branches — `get_plurality_fns` (`subject = "$"`, reading
  `ranting::is_subjective_plural(self.subject.as_str())`) and the
  literal-subject path (`:174-183`, folding in the `plural_you` attribute,
  `:52`, which is itself a `bool`).
- `ref_expr_ranting_trait` (`ranting_derive/src/lib.rs:210-230`) emits
  `is_plural`/`inflect` delegations for reference types.
- Consequence: a change to `is_plural`'s or `inflect`'s type is a **lockstep
  `ranting`↔`ranting_derive` version bump**. Generated code from an older
  `ranting_derive` stops compiling against the newer `ranting` — a failure
  mode with no analogue in items 2, 5–8, all of which only add parameters to
  hooks that macro-generated code never mentions.
- `src/collections.rs` carries **24** further delegations: 8 apiece for
  `Many`/`Maybe`/`Box` — the 6 hooks plus `is_plural` and `inflect`
  (`:98`–`:219`, `:277`–`:387`, `:404`–`:491`). `Many::is_plural` is
  `self.0.len() != 1` — the one place in the crate where an actual count
  exists and is immediately collapsed to a bool.

### The placeholder grammar (`+`/`-`, `#var`/`$var`)

`PlaceholderSpec.plurality` is **already a `&'static str`**
(`ranting_core/src/placeholder.rs:317`), baked by the macro
(`ranting_derive/src/lib.rs:688-690`, `:840`) — so the *spec* type tolerates
more than two values without any change. What does not tolerate it is the
grammar that produces it: `ph_ext::match_nr`
(`ranting_core/src/ph_ext.rs:562`) accepts exactly `[+-]` or
`(#|\??\$)\w+\s+`. A dual marker (`{2noun}` or similar) is a **compile-time
grammar change** in the parser and its `PH_EXT` differential-fuzz oracle, not
a hook change — and it is the only way an entity with no numeral in its
placeholder could ever be marked dual.

### The finding that discriminates between the options

**The number is gone by the time any hook runs.** `nr` reaches
`handle_placeholder` as an already-formatted `String`
(`ranting_derive/src/lib.rs:723-745`: `format!("{}", n)` for `$var`, or
`format!("{}", rant_convert_numbers(n as i64))` for `#var`, which has already
become English *words*). `handle_placeholder_impl` recovers a bool from it by
**string-sniffing** — `nr.trim_start() != "one"`, `s != "1" &&
s.split('.').next() != Some("1")` — and that string is never passed to a hook
at all. A `_custom` hook receives `as_plural: bool` and nothing else about the
count.

Two consequences that decide this spike:

1. A `plural_category()` channel **as literally scoped in option (b)** cannot
   be computed from what any hook receives today, by the fork *or* by the
   crate. CLDR categories are a function of the numeric value; the rendered
   form survives, the value does not. So (b)'s honest cost includes threading
   the numeral through the macro→runtime seam — which is most of (c)'s
   plumbing work, and needs saying before (b) is chosen for being cheap.
2. The `"1.0"` special-case in the sniff is the crate already conceding that
   *visible fraction digits* matter (English `1.0 inches` is CLDR `other`,
   not `one`). Any count payload that is a bare integer loses that
   distinction the existing code already tries to keep.

## The three phenomena, scored against the code

| Phenomenon | Needs | Reachable today? |
|---|---|---|
| **Arabic dual** with a numeral (`{$n book}`, n = 2) | the count at the hook | **No** — the count is stringified before the hook and never passed |
| **Arabic dual** on a bare `{+noun}` (no numeral) | a third value in the placeholder grammar *or* entity-side number | **No** — `match_nr` accepts `+`/`-` only; `Noun` has no number field, and `is_subjective_plural` is a bool |
| **Slavic paucal** (2–4, with the 12–14 exception) | the integer, and its last two digits | **No** — same reason; additionally the numeral governs case, which `GrammaticalCase` never sources from a numeral |
| **CLDR `zero/one/two/few/many/other`** | numeric value + visible fraction digits | **No** — value discarded; fraction digits survive only inside the rendered string |
| **Welsh/Irish number-triggered mutation** | the category, then a mutation on the *following* word | **No**, twice over — and the second half is Phase 6 item 7's "hook can't see what follows" problem, not this item's |

Note what this table does *not* say: there is no external-side-table
workaround here, of the kind item 2's `HashMap<&str, Gender>` was. A fork
cannot recover the count from anywhere. That is the difference between this
gap and the gender gap, and it is why (a) is not merely inelegant.

## Options, scored

| Option | English cost | Fork cost | What it buys | Verdict |
|---|---|---|---|---|
| **(a) Keep the bool; document non-English plural categories out of scope** | None | Permanent: dual/paucal/CLDR unreachable, with no workaround | Nothing; states a boundary | **Rejected** — contradicts a v1.3 success criterion the phase set for itself |
| **(b) Parallel channel alongside the bool, bool authoritative for English** | One parameter added to 6 (soon 8) hooks; zero behavioral change | Opt-in; ignore the parameter and nothing changes | Dual, paucal and CLDR **when a numeral is present** | **Recommended, in the "count, not category" form below** |
| **(c) Replace the bool with a `Number` enum in one coordinated breaking release** | Every hook, both non-hook trait methods, generated code, 24 wrapper delegations, 4 public free functions; lockstep macro↔runtime bump | Every impl rewritten | Same as (b), plus a tidier type — and *still* needs (b)'s numeral threading and a grammar change to be usable | **Rejected** — pays the full breaking cost for a strictly smaller increment than it appears to buy |

Scored against the three phenomena directly, since cost alone does not
separate them:

| Option | Arabic dual | Slavic paucal | CLDR `zero/one/two/few/many/other` |
|---|---|---|---|
| **(a)** | **No**, permanently — and with no side-table workaround, unlike item 2's gender gap | **No**, permanently | **No**, permanently |
| **(b)** narrowed | **Yes with a numeral** (`{$n book}`, n = 2); **no** on a bare `{+noun}` — that needs a grammar change or entity-side number, which fails through `Many`/`Maybe`/`Box` | **Yes** for form selection (the fork gets the integer and applies its own 2–4/12–14 rule); **no** for the numeral *governing the noun's case*, which `GrammaticalCase` never sources from a numeral | **Yes**, *iff* the count payload carries visible fraction digits — a bare `i64` cannot distinguish English `1.0` (`other`) from `1` (`one`), a distinction `src/lib.rs:379` already tries to make |
| **(c)** | Same as (b), and only after (b)'s numeral threading *plus* a `match_nr` grammar change — until then `Number::Dual` is unconstructible | Same as (b); the enum adds nothing the count does not | Same as (b); worse if `ranting` also ships the CLDR mapping, which pins the crate to a CLDR revision |

### (a) Keep the bool — rejected

The case *for* it is real and should be recorded: it costs nothing, it is what
item 1 concluded for word order, and "the honest answer is narrower than the
ask" is an established outcome in this crate.

It fails for one specific reason. ROADMAP.md's own **v1.3 Success Criteria**,
bullet 1: *"A non-English `Ranting` impl can obtain gender/noun class,
grammatical case, **number**, and register/dialect without an external
string-keyed side table."* Number is named there explicitly, and under (a) it
is the one item on that list a fork cannot obtain in any form — not via a side
table, not via `self`, not at all, because the count never leaves
`handle_placeholder_impl`.

So (a) is only available if that success criterion is reworded first. If a
future reader overturns this recommendation and takes (a) anyway, **the
success criterion must be amended in the same change**, to something like
*"...grammatical case, singular/plural number..."* with an explicit note that
categorial number (dual/paucal/CLDR) is out of scope for v1.3. Leaving the
criterion standing while shipping (a) is the one outcome this document rules
out entirely.

The unlock (a) genuinely does *not* block, and which weakens its cost
slightly: formal `Sie`-style plural-agreement-with-singular-reference is
already expressible via the singular-`they` precedent — item 3's spec makes
that point and records that item 4 "should not count T-V among its motivating
cases." It does not. Dual and paucal remain.

### (b) Parallel channel — recommended, narrowed

The ROADMAP scopes (b) as "add a parallel `plural_category()` channel". Two
amendments, both forced by the code:

**Amendment 1: a count, not a category.** `plural_category()` implies the
crate knows what the categories are. Mapping a number to `zero/one/two/few/
many/other` is a per-language function — Russian's rule and Arabic's rule and
Welsh's rule are three different functions of the same integer — and Phase 6's
framing puts language-specific rules in the companion crate, not in `ranting`.
Shipping a CLDR table in `ranting` would be the same mistake item 2 avoided by
making `NounClass` an open label instead of `enum { Masculine, Feminine,
Neuter }`: `ranting` should carry the *signal*, never the *interpretation*.
So: hand the fork the count and let it own the categorization. This also
sidesteps the version-skew problem of pinning `ranting` to a CLDR revision.

**Amendment 2: it is a hook parameter, not an entity method.** A
`plural_category()` on the trait would be entity-side, and the entity is the
wrong owner — the number in play is a property of the *placeholder occurrence*
(`{+noun}`, `{-noun}`, `{$n noun}`), not of the noun. This is exactly item 2's
`class`-parameter argument, and it has the same wrapper-shaped proof:
`Many::is_plural` is `self.0.len() != 1`, and when `Many` delegates a hook to
its single item, `self` inside that hook is the *inner* value — so anything
read off `self` reports the wrong thing for a wrapper. The count must be
threaded from the call site, like `class` and `case` before it.

The minimum viable payload, derived from the sniff at `src/lib.rs:371-381`:
the integer count **and** enough to reproduce the `1.0`-is-not-`one`
distinction the current code already makes — i.e. an integer plus visible
fraction digits, or the pre-conversion numeric value, carried in one small
public type with a `None`-equivalent for the "no numeral in this placeholder"
case (`{noun}`, `{+noun}`, `{-noun}`). The macro already has the numeric
expression in hand before rendering (`ranting_derive/src/lib.rs:723-731`,
where `#var` becomes `rant_convert_numbers(#n as i64)`), so the value exists
at exactly the point it is currently thrown away.

Additivity is mechanical and matches item 2 exactly: the bool stays, and stays
authoritative for every English fallback path, so `say!()`'s output is
byte-identical; an impl that ignores the new parameter behaves as it does
today; only hook *signatures* change, which is the cost Phase 6 has already
accepted twice (`GrammaticalCase` in item 2's predecessor, `NounClass` in item
2 — whose completion notes record "every pre-existing assertion
byte-identical; only hook signatures in those files changed"). Generated code
is untouched, because the macro never names the `_custom` hooks.

**Where it should land: inside Phase 6 item 5.** Item 5 already adds
`inflect_adjective_custom`/`_with_context` and already plans to pass number to
them. Adding the count parameter to the other six hooks in the same change
makes Phase 6 break hook signatures **once** instead of twice, and gives item
5's French worked example (*un chat noir* / *des chats noirs*) a natural place
to also demonstrate a count-driven form. If item 5 ships first without it,
this becomes a standalone item and the phase pays the signature break twice.

Score: **buys every case where a numeral is present, costs one parameter,
breaks no output, and keeps CLDR out of the crate.**

### (c) `Number` enum replacing the bool — rejected

The appeal is honest: one type, no redundant channel, no "which of these two
number values is authoritative" question for a fork author to get wrong.

It is rejected on cost-to-increment, not on taste:

1. **The breaking surface is the whole crate, in four kinds** — public trait
   signatures (8 methods incl. `is_plural`/`inflect`); four public free
   functions (`inflect_possessive`, `inflect_reflexive`,
   `inflect_noun_irregular`, `is_subjective_plural`); **generated code**
   (`ranting_derive/src/lib.rs:781`'s baked `#expr.is_plural()` and
   `ranting_impl.rs`'s two `is_plural`/`inflect` emission branches), which
   makes it a lockstep macro↔runtime version bump rather than a trait edit;
   and 24 wrapper delegations in `src/collections.rs`. Every `impl Ranting` in
   the ecosystem is rewritten, English-only ones included — the same
   "semver-major for users who get nothing from it" objection that sank
   option (a) in item 3's spike.
2. **It does not subsume (b)'s work.** A `Number` enum at the hooks still has
   to be *produced* from somewhere, and the only producers are
   `noun.is_plural()` (a bool, from a pronoun) and the numeral (discarded
   before the hook). So (c) needs the numeral threading anyway. It buys the
   tidier type and nothing else.
3. **Without a grammar change it is decorative.** `match_nr`
   (`ranting_core/src/ph_ext.rs:562`) accepts `+` and `-` only, and `Noun` has
   no number field, so nothing can ever *say* `Number::Dual`. A
   `Number { Singular, Plural, Dual, Paucal, … }` whose non-boolean variants
   are unconstructible is worse than a bool, because it advertises support
   that does not exist.
4. **`ranting` would have to interpret it.** Unlike `NounClass`, which the
   crate only forwards, number is *read* at every English site listed in the
   inventory. Every one of them would need a `Dual`/`Paucal` → plural
   collapse written into `src/language/english.rs`, i.e. English code
   answering questions about non-English categories — the precise objection
   item 3 raised against extending `SubjectPronoun` (`pronoun_forms`, an
   English module, being asked "what is `Sie`'s reflexive?").

The closed-vs-open question, for the record: were (c) ever revisited, item 2's
`NounClass` reasoning applies — a closed enum over English-adjacent categories
would be wrong on arrival, and the open form is `NounClass`'s label newtype,
which is what (b)'s count-plus-fork-owned-mapping already amounts to without
the breakage.

## Recommendation

**Adopt (b), narrowed to a count channel, and land it inside Phase 6 item 5's
signature change.**

1. `as_plural: bool` **stays**, stays authoritative for English, and its
   promise is documented (next section) rather than quietly widened.
2. The hooks additionally receive the placeholder's numeral, when it has one,
   as a small public value type with an explicit "no numeral here" state.
   `ranting` never maps it to a category.
3. Categorization (`zero/one/two/few/many/other`, dual, paucal) is the
   companion crate's, computed from that count with that language's rule.
4. Bare `{+noun}`/`{-noun}` dual — a categorial number with no numeral —
   stays **out of reach**, and is recorded as such below. Closing it needs a
   placeholder-grammar change, which this spike does not recommend: it would
   add grammar surface for every English user to serve one construction, the
   same shape of point fix item 1 rejected for the German second verb
   position.
5. If a future reader prefers (a), the v1.3 success criterion must be reworded
   in the same change — see (a) above.

## What `as_plural: bool` promises, and what it does not

This is the statement ROADMAP.md item 4 requires be recorded in both
ROADMAP.md and CLAUDE.md. It is a description of today's behavior and is true
under every option above.

**It promises**: *render the plural **agreement** form.* It is a two-valued
morphosyntactic switch over English's two-valued noun/verb/pronoun agreement
system, resolved per placeholder occurrence.

**It does not promise**: *the referent count is greater than one.* The crate
itself ships two counterexamples:

- `is_subjective_plural("they") == true` (`ranting_core/src/grammar.rs:130`),
  which is what makes singular *they* work — plural agreement, singular
  reference. `Many` with zero items is another: `len() != 1` is `true`, so
  "there **are** no items."
- `inflect_reflexive` (`src/language/english.rs:332`) special-cases
  `to_plural` to choose `yourself`/`yourselves` precisely *because* "you" is
  number-underspecified in every other form — the bool is carrying reference
  number there and agreement number everywhere else, in the same function.

**It also does not promise** to be derivable from anything else a hook
receives, and a fork must not try: the count is stringified before the hook
runs (`src/lib.rs:371-381`), and `#var` has already been converted to English
words by then. Under the recommendation above, the count arrives as its own
parameter; until then it does not arrive at all.

**For a fork author, concretely**: treat `as_plural` as "does English want the
plural form here", use it as the fallback when your language's category
computation has no numeral to work from, and expect it to answer `true` for
singular-reference plurals.

## Cross-item consequences to record

- **Item 5 (adjective agreement)** — its planned hook signature names
  "number". This spike fixes that as `as_plural: bool` **plus** the count
  parameter, and item 5 is the recommended landing site for the count on all
  eight hooks at once.
- **Item 8 (locale-aware numerals) — a live latent break, regardless of which
  option is chosen.** `"#" => nr.trim_start() != "one"` (`src/lib.rs:376`)
  string-sniffs the *English* word "one". The moment a fork's numeral hook
  spells `#var` in its own language, that comparison sees `"eins"`/`"un"`/
  `"один"` and reports plural for a count of one — silently, in the wrong
  direction, for every article, verb and pronoun in the placeholder. Item 8
  must either take the count from the same channel this spike recommends or
  explicitly document that overriding `#var` breaks number agreement. This is
  a prerequisite for item 8, not an optional cleanup.
- **Item 2 (`NounClass`)** — the precedent cited for "carry the signal, never
  the interpretation", and for a hook *parameter* rather than an entity method
  when wrappers are in play.
- **Item 3 (pronoun inventory)** — already recorded that T-V is not item 4's
  problem; confirmed here. Genuine dual/paucal are.
- **Item 7 (elision)** — Welsh/Irish number-triggered mutation needs both this
  item's category *and* item 7's "see the following word". Neither alone is
  enough; the pair is worth naming when item 7 is designed.
- **`conjugate_auxiliary`** (`src/language/auxiliary.rs:15`) takes no number
  at all and matches on the subject string. It is behind no hook. Whatever
  number type is decided, that function does not receive it — a fork
  overriding number agreement gets the English auxiliary anyway unless it
  returns a full form from `inflect_verb_custom`.

## Rejected alternatives, recorded

| Rejected | Why |
|---|---|
| (a) keep the bool, document categories out of scope | The only Phase 6 signal with **no** fork-side workaround of any kind (unlike item 2's side table), and it contradicts v1.3 success criterion 1, which names "number" explicitly. Available only if that criterion is reworded in the same change. |
| (c) `Number` enum replacing the bool | Breaks four kinds of surface at once — trait signatures, four public free functions, *generated code* (lockstep macro↔runtime bump), 24 wrapper delegations — for every user including English-only ones; still needs (b)'s numeral threading to be usable; needs a placeholder-grammar change before any non-boolean variant is even constructible; and forces `src/language/english.rs` to interpret non-English categories. |
| (b) as literally scoped — a `plural_category()` entity method | Two faults: it puts CLDR (a per-language, versioned table) inside `ranting`, against the phase's own scoping; and entity-sourced number is wrong for `Many`/`Maybe`/`Box`, where `self` inside a delegated hook is the inner value — item 2's exact argument for a parameter. |
| A dual/paucal marker in the placeholder grammar (`{2noun}`) | Would close the "categorial number with no numeral" case, but adds grammar surface (`ph_ext::match_nr` plus its `PH_EXT` fuzz oracle) for every English user to serve one construction. Same shape of point fix item 1 rejected for the German second verb position. Recorded as the *only* way that case could be closed, not scheduled. |
| Passing `nr: &str` (the rendered numeral) to the hooks as-is | Cheapest possible change, and rejected: the fork would have to parse back a string its own item-8 numeral hook may have written, in its own language, possibly with `{:...}` formatting applied. Sniffing strings is the bug this spike found, not the fix. |
| Deferring the decision until after item 5 | Costs Phase 6 a second hook-signature break, and item 5's hook would ship with a number type this spike would immediately change. |

## Correction, 2026-08-14: the counted noun itself was missed

> This section was added after `2026-08-14-arabic-falsification-spike.md` ran the recommendation
> below against real Arabic. Everything above is the design as scored on 2026-08-13; this is what
> shipping it and then testing it revealed.

The recommendation landed as ROADMAP.md Phase 6 item 14: `count: Option<PlaceholderCount>` on five
hook pairs, plus `inflect_numeral_custom`'s pre-existing `count: Option<i64>`, plus item 15's
`Many` length. **It does not make Arabic dual reachable, and this document's inventory is why it
was missed.**

The inventory above enumerated every place number is a `bool` and then scored the *hooks*.
`Ranting::inflect` is in that inventory — it takes `to_plural: bool` — but it is not a `_custom`
hook, so the recommendation's "add the count to the hooks" did not cover it. It was widened in item
14's own commit, with `case: GrammaticalCase`, not with a count.

The consequence is that Arabic dual is *half* reachable: verbs, pronouns, articles and adjectives
can all agree in the dual by branching on `count.value == 2`, and **the counted noun itself renders
plural**, because `inflect` still sees only a bool. Verified, not predicted — see the Arabic spike
§1, including why the obvious `Cell` side-channel is not a workaround (it contaminates every later
placeholder in the same template).

So the correct verdict for dual-with-a-numeral is neither "unreachable" (this document, before
item 14) nor "reachable" (item 14, assumed): it was **agreement reachable, head noun not**. Closing
it needed `count: Option<PlaceholderCount>` on `Ranting::inflect` — the same type, the same source,
one more signature.

**That change landed 2026-08-14 as ROADMAP.md Phase 7 item 11**, scheduled by the item 4 build
decision. Dual-with-a-numeral is now reachable end to end, verified by
`tests/ranting/third_number.rs` rather than assumed: `{$0 1}` renders `kitab`/`kitaban`/`kutub` at
counts 1/2/3, both numeral channels carry the count, and `None` stays distinguishable from a count
of one. The final verdict for option (b) is therefore **recommended and delivered**, in two
commits a day apart, with the second one found only by running the code. The bullets below are
unaffected — they are the cases no count channel reaches.

## What stays impossible under this recommendation

- **Categorial number with no numeral in the placeholder** — Arabic dual on a
  bare `{+noun}`, since `+`/`-` are the only markers `match_nr` accepts and
  `Noun` carries no number field. A fork can work around it *only* on its own
  types, by reading a number field off `self` in its hooks — which fails
  through `Many`/`Maybe`/`Box` for the reason given above.
- **Numeral-governed case** (Russian *два дома*, genitive singular after 2–4).
  `GrammaticalCase` is threaded from the placeholder's `CaseKind`; nothing
  lets a numeral override the case of the noun beside it.
- **Number-triggered initial mutation** (Welsh, Irish) — needs item 7 as well.
- **Auxiliary verbs under a fork's number rules** — see
  `conjugate_auxiliary` above.
- **`heed!()`/`ask!()` on the input side** — they have no number concept at
  all; `{$name}` parses digits to `u64` and nothing downstream agrees with it.

## Open questions for whoever picks this up next

1. The exact payload type for the count: bare `i64`, `Option<i64>`, or a small
   struct carrying visible fraction digits so the existing `1.0`-is-not-`one`
   behavior (`src/lib.rs:379`) is expressible to a fork. Recommendation: the
   struct, since the crate already needs the distinction and adding it later
   is a second signature change.
2. Whether the count parameter goes on all six existing hooks or only on the
   article/pronoun/adjective ones. Verb agreement in Arabic and Slavic *is*
   number-sensitive beyond singular/plural, so the current reading is "all of
   them" — unlike item 2's `NounClass`, which correctly skipped the verb hook
   because class is not a verb-agreement axis.
3. Whether `Many` should expose its `len()` as the count when a placeholder
   has no numeral (`{many_items}`) — it is the one entity that genuinely knows
   one. Cheap, and it is the only path to categorial number without a grammar
   change. Not blocking.

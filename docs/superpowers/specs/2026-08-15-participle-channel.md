# Design spike: the participle channel — passive voice, future perfect, perfect progressive

**Status**: design spike complete; conclusion is **five composed post-noun marker
spellings — `=%` / `<=%` (passive), `>%` (future perfect), `%=` / `<%=` (perfect
progressive) — landed as new `TenseMarker` variants, not new `PostSpec` shapes and
not a new marker character — not implemented by this document**. ROADMAP.md
Phase 8 item 1. The sigil grammar is Locked (ROADMAP.md's Key Architecture
Decisions table, "Placeholder syntax (full grammar support)"), so this spike
answers the scoping questions a maintainer needs to rule on a grammar change,
and stops there — no code in this repository is changed by it.

This is the second Phase 8 spike, following the shape of the others in this
directory (ground the question in the code as it stands, survey options
including the rejected ones, score them, state a recommendation and what stays
out of reach under it). Its sibling, `2026-08-15-verbatim-verb-marker.md`
(Phase 8 item 2), did the same exercise for a *new* marker character; this one
lands on the opposite conclusion — composition of already-taken characters —
and the comparison between the two costs is itself one of this spike's
findings.

## The question

`ranting_core::verb_conjugate::to_past_participle`
(`ranting_core/src/verb_conjugate.rs:43`) already exists, is already fed by the
generated `IRREGULAR_PAST_PARTICIPLE` table, and is already correct — but only
two markers reach it: `%` (present perfect, "has taken") and `<%` (past
perfect, "had taken"), via three call sites that all key off the same marker
strings:

- `ranting_derive/src/lib.rs:765-766` — `handle_param` baking the compile-time
  form for `say!()`;
- `src/narration.rs:188-189` and `:201` — `marker_and_form_for_tense` /
  `form_for_marker`, the `say_with!()` runtime-tense resolution;
- `src/lib.rs:1230-1239` — `handle_tense_marker`, which prepends the agreeing
  auxiliary (`conjugate_auxiliary(AuxiliaryVerb::HaveHas | Had, ..)`).

So a template cannot compose an auxiliary plus a participle for a *variable*
verb. Passive voice ("the sword **is taken**"), future perfect ("**will have
taken**") and perfect progressive ("**has been picking**") are all hand-written
today, for verb forms the crate already knows how to produce.

### What already works, precisely — the gap is narrower than "no passive"

The hand-written workaround is better than it first looks, and scoping the
item honestly requires saying so. A multi-word post-noun verb with no marker
bakes as `PostSpec::Verb` (`ranting_derive/src/lib.rs:706-711`), and since the
2026-08-15 §1.6 fix the runtime conjugates the **head** word and passes the
rest through verbatim. `are` is in `IrregularPluralVerb`
(`src/language/english.rs:44`) with full person agreement (`am`/`is`/`are`,
`was`/`were` via `Were`). So:

```rust
say!("{The sword are taken}.")   // "The sword is taken." — agreement works
say!("{The +swords are taken}.") // "The swords are taken."
```

already renders a correct short passive, *with* auxiliary agreement — provided
the caller hand-writes the participle. What is unreachable is the participle
of a **variable** verb: there is no way to write "conjugate `take` into its
participle here", because the only markers that invoke `to_past_participle`
also hard-wire the have-auxiliary. The same goes for "has been picking" (the
gerund is reachable only through `=`/`<=`, which hard-wire bare be, no
"been"). The gap is the *composition*, not the agreement and not the forms.

## The taken-character inventory, read from the source

Read from `ranting_core/src/grammar.rs` (`PH_EXT`, the reference regex) and
`ranting_core/src/ph_ext.rs` (the hand-written parser `ranting_derive`
actually calls), not from memory. `PH_EXT`, reformatted:

```text
^(?x)
    (?P<uc>[,^])?+
    (?P<pre>(?: ... ))?+
    (?P<nr>[+-]|(?:\#|\??\$)\w+\s+)?+
    (?P<case>\*[`=@~%]|[`=@~*?%])?+
    (?P<noun>[\w-]+)
    (?P<post>\s+[<=>%!]*(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$
```

| Group | Characters | Meaning |
|---|---|---|
| `uc` | `,` `^` | force-lowercase / force-uppercase override |
| `pre` | article/modal words, `` ` ``-prefixed word, `?` prefix, one literal word (Phase 6 item 26) | article/modal detection, preposition-fusion input |
| `nr` | `+` `-` `#` `$` (optionally `?`-prefixed) | plurality force, spelled-out numeral, digit numeral, hidden numeral |
| `case` | `` ` `` `=` `@` `~` `*` `?` `%`, plus fused `` *` `` `*=` `*@` `*~` `*%` (Phase 6 item 19) | pronoun case / display-as-name / hidden |
| `post` (marker prefix) | `<` `=` `>` `%` `!`, combined as `<=` `<%` `!!` | tense (`TenseMarker`) or degree (`DegreeKind`) |
| `post` (suffix form) | `'` | possessive-`s` |

Union of taken characters: `` , ^ + - # $ ` = @ ~ * ? % < > ! ' `` — seventeen,
unchanged since the verbatim-marker spike counted them. The unclaimed
shortlist (`;` `|` `&` `/`) is the same one that spike identified, and item 2's
recommendation is already eyeing `;` from it.

**The structural fact this spike turns on**: `post`'s marker slot is a *run* —
`[<=>%!]*` in `PH_EXT` (`ranting_core/src/grammar.rs:141`), a
`take_while(|c| matches!(c, '<' | '=' | '>' | '%' | '!'))` in both
`ph_ext::match_post` (`ranting_core/src/ph_ext.rs:682-686`) and
`ranting_derive`'s `handle_param` (`ranting_derive/src/lib.rs:701-704`). Any
run of those five characters already **parses**, at both the regex and the
hand-parser layer. Which runs *mean* something is decided strictly downstream,
in `handle_param`'s exact-string match (`ranting_derive/src/lib.rs:760-788`),
whose `_` arm makes every unrecognized run — including `=%`, `<=%`, `>%`, `%=`
and `<%=` today — a **compile error** ("unrecognized tense marker"). Two
consequences:

1. Assigning meaning to a currently-unrecognized run cannot change any
   existing template's output. Every template that compiles today keeps
   compiling with byte-identical output; the change converts compile errors
   into features. English byte-identity — the invariant CLAUDE.md states for
   every cross-language feature so far — is preserved *by construction*, not
   by care.
2. No character class changes anywhere. See "Parity cost" below for what that
   buys.

## The spellings, in the family the `*=`/`*@` work established

Phase 6 item 19's precedent is **enumerated fusion**: a fused spelling is an
exact multi-character string tried before the single-character fallback, each
constituent character keeping a recognizable echo of its standalone meaning —
not a productive algebra. `<=` and `<%` already work this way (`<` is not a
general "past-shift operator"; the two-character strings are enumerated
markers whose spelling is mnemonic). The proposed family follows suit:

| Marker | Construction | Auxiliary chain | Main verb form | `{the sword X take}` / `{=they X pick}` |
|---|---|---|---|---|
| `=%` | present passive | is/are/am (`AuxiliaryVerb::IsAre`) | participle | "the sword **is taken**" |
| `<=%` | past passive | was/were (`AuxiliaryVerb::WasWere`) | participle | "the sword **was taken**" |
| `>%` | future perfect | "will have" (invariant) | participle | "they **will have picked**" |
| `%=` | present perfect progressive | has/have + "been" (`HaveHas`) | gerund | "they **have been picking**" |
| `<%=` | past perfect progressive | "had been" (invariant) | gerund | "they **had been picking**" |

Mnemonics, stated so a maintainer can judge them rather than reverse-engineer
them: the passive pair reads "the be-auxiliary of `=`/`<=` (which already
select is/are and was/were for the continuous tenses), with `%`'s participle
instead of the gerund". The perfect chains read outer-to-inner in surface
order: `>%` is "will" + the `%` construction downshifted to its base ("have
taken"); `%=` is "has/have" + the `=` construction downshifted ("been
picking"); `<%=` is the `<%` past shift of the same. The two orderings `=%`
and `%=` denote different constructions, and in both the character order
matches the order of what renders — this is what makes the pair legible
rather than a trap. ROADMAP.md item 1's own suggested shapes (`=%verb` →
"is/are seen", `>%verb` → "will have seen") are the first and third rows.

Deliberately **not** proposed here, though the same reading reaches them:
future passive `>=%` ("will be taken"), future continuous `>=` ("will be
picking"), passive perfect `%=%` ("has been taken"), and the conditional
perfect (its "would" is already writable as `pre`-slot modal text, and `would`
is invariant — see the ranking below for why invariant-auxiliary
constructions gain the least). They are named so the maintainer can see the
family has room, and cut so the first change stays reviewable. Each would be
one more enumerated string in the same match sites, nothing structural.

No prefix-ambiguity risk: `handle_param` classifies the **entire** marker run
as one string (the run is extracted first, then matched), so `<=%` cannot be
misread as `<=` plus a stray `%` — there is no greedy-prefix matching step to
get wrong. The existing `!`-mixing rejection
(`ranting_derive/src/lib.rs:746-752`) already covers runs like `=%!`.

## Can the passive's auxiliary agreement reuse `AuxiliaryVerb` unchanged?

**Yes — for the passive, literally unchanged.** The passive's entire
agreement burden is is/are/am and was/were, and those are exactly
`AuxiliaryVerb::IsAre` and `AuxiliaryVerb::WasWere`
(`src/language/auxiliary.rs:19-25`), person tables and unrecognized-pronoun
defaults included. `handle_tense_marker` (`src/lib.rs:1207-1242`) gains two
arms that call `conjugate_auxiliary` with the existing variants and format
`"{aux} {participle}"` — the same shape as the existing `=`/`<=` arms with a
different main-verb form behind them.

The other two constructions need **no new agreement logic** either, only
invariant words:

- `>%`: "will have" is the same for every person. Either a new
  `AuxiliaryVerb::WillHave` variant returning the fixed string (fits the
  existing `-> &'static str` signature) or a literal in the
  `handle_tense_marker` arm; both are agreement-free.
- `%=`: `HaveHas` already carries the has/have agreement; "been" is invariant.
  The arm formats `"{aux} been {gerund}"`.
- `<%=`: "had been", fully invariant, same choice as `>%`.

So the answer to the spike's question is: the machinery is reused unchanged in
the strong sense (no new person/number logic anywhere); whether the two
invariant chains become enum variants or arm-local literals is a style call
for the implementer, invisible in output.

The main-verb forms are equally settled: `to_past_participle` for the three
participle rows, `to_continuous` for the two gerund rows — both existing, both
already irregular-table-backed, both already called from the exact functions
that would gain arms.

## `TenseMarker` or `PostSpec`? — `TenseMarker`, and the one real seam issue

These markers are tense/aspect/voice on a post-noun verb, and everything the
`PostSpec::Tense` arm does is exactly what they need: `say!()` bakes the
compile-time-conjugated form, `say_with!()` bakes the uninflected base so
`NarrationContext.tense` can re-resolve it
(`ranting_derive/src/lib.rs:757-768`), the runtime offers the form to
`inflect_verb_custom_with_context` and prepends the auxiliary via
`handle_tense_marker` keyed on `marker.as_marker_str()`
(`src/lib.rs:1079-1146`). Five new `TenseMarker` variants
(`ranting_core/src/placeholder.rs:81-127` — say `PresentPassive`,
`PastPassive`, `FuturePerfect`, `PresentPerfectProgressive`,
`PastPerfectProgressive`) ride all of that as-is: new arms in
`from_marker`/`as_marker_str`, in `handle_param`'s two matches, in
`narration::form_for_marker`, and in `handle_tense_marker`. All string-keyed
matches with explicit fallbacks; none `match` exhaustively on `TenseMarker`
itself except its own two methods. Per `.claude/rules/crate-layout.md`,
nothing in `ranting_core` is part of `ranting`'s public semver surface even
where re-exported, so growing the enum is not a semver event for downstream
matchers the way growing `GrammaticalCase` would be (that enum's lock is
precisely about downstream exhaustive matches; `TenseMarker` has none
outside the two crates that own the seam).

A new `PostSpec` variant instead (`PostSpec::Passive { .. }`) would duplicate
`Tense`'s four fields, its `handle_param` baking branch, and its entire
runtime arm — including re-implementing the `say_with!()` base-form protocol —
to express something that differs only in which auxiliary/form pair renders.
Rejected as pure seam duplication; `PostSpec`'s variants distinguish *kinds*
of post-noun content (verb vs. adjective vs. possessive-`'s`), not members of
the tense table.

**The one real design issue at the seam — the `ctx.tense` × voice
interaction.** `say_with!()`'s runtime override resolves the baked base verb
through `narration::marker_and_form_for_tense(t, word)`
(`src/lib.rs:1106-1112`, `src/narration.rs:181-191`), which maps a `Tense` to
a marker string *with no knowledge of the compile-time marker it is
overriding*. Extended naively, `NarrationContext { tense: Some(Tense::Past) }`
applied to `{the sword =%take}` would replace `=%` with `<` and render active
"took" — **silently stripping the voice the template wrote**. Voice is
orthogonal to tense; a tense override on a passive placeholder should move
along the be-chain ("is taken" → "was taken" → "will be taken"), and on a
perfect-progressive one along the have-chain. That means
`marker_and_form_for_tense` (or its caller) must become aware of the
compile-time marker's voice/aspect — a `pub(crate)` signature change, cheap,
but it must be *decided*, because the naive extension is silently wrong in
exactly the way this repo's review culture keeps finding
(grammatical-looking output, wrong in one word). Recommendation: the
override maps the tense axis only, voice/aspect preserved; `Tense` itself
(the public `NarrationContext` enum) stays at its seven variants — callers
who want to *switch* voice per call write two templates, which is the
word-order-boundary answer to template variation generally.

## What keeping `PH_EXT` and `ph_ext` in parity costs: nothing mandatory

This is the sharpest difference from the verbatim-marker spike, and the
strongest argument for composed spellings. The parity discipline
(`.claude/rules/placeholder-grammar.md`: `PH_EXT` is the reference grammar,
`ph_ext::parse` the implementation, `assert_parity`/`parity_fuzzed` the
drift-catcher) is only exercised by a change when a *character class or
capture structure* changes. Here none does:

- `PH_EXT`'s `post` group already matches `\s+[<=>%!]*…` — `who =%take`
  matches today, marker run and all.
- `ph_ext::match_post`'s `take_while` already consumes the same run.
- `assert_parity` already passes for these inputs, because both sides accept
  them with identical captures; the compile error lives downstream in
  `handle_param`, which the fuzzer does not (and need not) model.

So the three-site lockstep the verbatim spike had to budget for (`grammar.rs`
class, `ph_ext.rs` `take_while`, `ranting_derive` `take_while` — the third
outside fuzzer coverage) collapses to **zero regex/parser edits**. The only
sites that change are the downstream string matches listed in the previous
section, and the worthwhile-but-free additions: new rows in
`parity_curated_corpus` and the `parity_fuzzed` `post` option list
(`ranting_core/src/ph_ext.rs`, e.g. `" =%take"`, `" %=pick"`) to document
that these runs are intended, plus rendering tests in `tests/ranting/` — the
same coverage category every other marker family has. Choosing a *new*
character for any of these constructions would forfeit exactly this, which is
why the options section scores it down.

## The word-order boundary: not approached

Cite, don't re-litigate: ROADMAP.md Key Architecture Decisions, "Word order
lives in the literal template, not the placeholders" (Locked, Phase 6
item 1), and `.claude/rules/extension-hooks.md`'s "permanent, not a gap". The
proposed markers emit their auxiliaries **inside the placeholder's own verb
rendering**, at the exact position `>`/`%`/`=` already emit
"will"/"has"/"is" — `handle_tense_marker` formatting `"{aux} {verb}"` within
one slot. Nothing reorders across template words, and nothing new becomes
order-sensitive: the long passive's agent phrase ("taken **by the knight**")
is and remains caller template text after the placeholder, exactly where the
boundary says it lives. No part of this proposal needs, weakens, or tempts a
word-order exception.

One inherited (not new) limitation worth recording: the auxiliary is
invisible to `inflect_verb_custom_with_context` — the hook sees only the
main-verb form, and the auxiliary is prepended afterwards by
`handle_tense_marker`, English-only. That is already true of every existing
tense marker (`>`, `=`, `%`, …); the new markers neither widen nor narrow it.
A fork wanting a native passive (German *werden*-passive, say) overrides the
verb hook and gets the participle handed to it, but must render its own
auxiliary — the same deal `%` gives it today. If that ever becomes a finding,
it will come from a falsifier crate, and it belongs to the existing
"auxiliaries are not hook-visible" fact, not to this item.

## Which constructions land together — ranked by what hand-writing costs

The ranking criterion is not raw frequency alone but **whether the
hand-written workaround is merely verbose or actually wrong**. The
workaround's weak point is agreement: an invariant auxiliary can be
hand-written perfectly; a varying one either goes through the head-word
`PostSpec::Verb` path (works, see above) or is hard-coded wrong.

1. **Passive (`=%`, `<=%`) — the item's reason to exist.** Short passives are
   pervasive in the descriptive/expository prose this crate targets (ROADMAP
   item 1 says as much), the auxiliary varies by person *and* tense
   (am/is/are/was/were), and the missing piece — deriving the participle of a
   variable verb — has no workaround at all. Lands first in any slicing.
2. **Perfect progressive (`%=`, `<%=`) — worth riding along.** Common in
   narrative prose ("had been waiting"), auxiliary agreement varies
   (has/have), and both its forms (`HaveHas` agreement, `to_continuous`
   gerund) are already built. Marginal cost over the passive: two more arms
   in each of the same five matches.
3. **Future perfect (`>%`) — least urgent, cheapest.** Rare in ordinary
   prose, and its auxiliary chain ("will have") is fully invariant — so
   `{=they will have taken}`-style hand-writing (modal words are already
   legal `pre`-slot text, the participle hand-written) is *correct* today,
   not just possible. All it gains from a marker is deriving the participle.
   It lands because it costs one arm and ROADMAP's own item names it, not
   because prose demands it.

Recommendation on slicing: **land all five spellings in one change.** The
machinery is one shared set of match sites; slicing it would ship the
placeholder-grammar documentation (README marker table,
`.claude/rules/placeholder-grammar.md`, `docs/EXTENSIBILITY.md`) three times
for one table's worth of rows. If a maintainer wants a smaller first bite,
the passive pair alone is self-justifying and severable.

## Options, scored

### (a) Composed spellings as `TenseMarker` variants — recommended

Everything above. Zero grammar/parser edits, zero new characters spent,
byte-identity by construction (all five spellings are compile errors today),
auxiliary agreement reused unchanged, forms reused unchanged. Open cost: the
`ctx.tense` × voice decision, which exists under every option that ships
passive at all.

### (b) A new `PostSpec::Passive` variant — rejected

Duplicates the `Tense` arm's compile-time and runtime protocol (including the
`say_with!()` base-form baking) to express a fifth row of the same table.
`PostSpec` distinguishes kinds of post-noun content; these are tense-table
members.

### (c) A new marker character for voice (e.g. `&` from the free shortlist) — rejected

Spends one of four remaining clean characters (the verbatim spike's shortlist
`;`/`|`/`&`/`/`, of which item 2 already wants one) on something composition
expresses for free, and re-triggers the three-site character-class lockstep —
including the `ranting_derive` `take_while` site the parity fuzzer cannot
see — that option (a) avoids entirely. No expressiveness gain: a voice
character would still need to combine with tense markers, landing back at
fused spellings anyway.

### (d) Do nothing — rejected as leaving the one real hole open

The hand-written forms are better than folklore suggests (`{0 are taken}`
agrees correctly via the head-word `Verb` path), and that fact is recorded
above so nobody rediscovers it as a bug. But the participle of a variable
verb stays unreachable, which is precisely the gap ROADMAP item 1 names, and
no caller-side pattern recovers it.

## Recommendation

Ship **(a)**: the five enumerated spellings `=%`, `<=%`, `>%`, `%=`, `<%=` as
new `TenseMarker` variants, auxiliaries via the existing `conjugate_auxiliary`
table plus two invariant chains, forms via the existing
`to_past_participle`/`to_continuous`. This spike does not implement it — the
sigil grammar is Locked and a grammar change needs a maintainer's sign-off.
Three decisions are left open for that sign-off:

1. **The spellings themselves** — the mnemonic reading is argued above, but a
   maintainer who prefers different enumerated strings (or wants `>=%`-family
   rows included/excluded) can swap them without disturbing any structural
   conclusion here; nothing below the spelling layer changes.
2. **The `ctx.tense` × voice interaction** — recommended: override moves the
   tense axis only, voice/aspect preserved, `Tense` stays seven variants.
   The naive alternative silently strips voice and should be rejected out
   loud, not by default.
3. **Slicing** — all five together (recommended) versus the passive pair
   first.

## What stays impossible under this recommendation, until implemented

- `{the sword =%take}` and the other four spellings keep failing to compile
  ("unrecognized tense marker") until a maintainer signs off and the arms
  land.
- The passive of a variable verb stays hand-written-participle only
  (`{the sword are taken}` — which keeps working unchanged, before and
  after).
- No change to `%`/`<%` or any other existing marker; the seventeen taken
  characters keep their meanings, and the free shortlist stays four long for
  item 2's verbatim marker to draw on.
- Future passive, future continuous, passive perfect and the conditional
  perfect stay out of scope — named above as reachable by the same family,
  deliberately not proposed.

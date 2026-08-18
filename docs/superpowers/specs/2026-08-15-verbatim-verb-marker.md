# Design spike: a verbatim-verb marker for the subjunctive escape hatch

**Status**: design spike complete; conclusion is **a new post-noun marker
character, in the family alongside `<`/`=`/`>`/`%`/`!`, spelled `;` pending a
maintainer's own preference from the shortlist in "Which character" below —
not implemented by this document**. ROADMAP.md Phase 8 item 2. The defect is
`docs/architecture-review-2026-08-15.md` §1.5. The sigil grammar is Locked
(ROADMAP.md's Key Architecture Decisions table), so this spike answers the
scoping questions a maintainer needs to rule on a grammar change, and stops
there — no code in this repository is changed by it.

This is the first Phase 8 spike, following the shape of the Phase 6/7 spikes
in this directory (ground the question in the code as it stands, survey
options including the rejected ones, score them, state a recommendation and
what stays out of reach under it).

## The question

`say!("If {=i were} rich, …")` should render `"If I were rich, …"` — the
caller wrote a grammatically correct English subjunctive. It renders
`"If I was rich, …"` instead. §1.5 traces this precisely and the trace
matters for what follows, because the first-glance reading of the
placeholder is wrong in a way that changes where the fix has to live.

### Where "were" actually gets rewritten — not on the tense-marker path

In `{=i were}`, the `=` is **not** a tense marker. It is the `case` capture
(`ranting_core::grammar::PH_EXT`'s `` (?P<case>\*[`=@~%]|[`=@~*?%])? `` group,
the same slot `{the *=noun}` uses), asking for `CaseKind::Subjective` on the
noun `i` — it renders `"I"` in place of the entity's own name. `were` is the
`post` capture, and it starts with `w`, not one of `<=>%!`, so
`ranting_derive/src/lib.rs:701-704`'s `take_while(|c| matches!(c, '<' | '='
| '>' | '%' | '!'))` finds a zero-length marker run and bakes it as
`PostSpec::Verb("were")` (`ranting_derive/src/lib.rs:706-711`) — a plain,
unconjugated verb, exactly as captured. Nothing about *this* is wrong; a
present-tense verb reaches `handle_placeholder` the same way and gets
person/number agreement applied at runtime, which is the whole point of the
`PostSpec::Verb` arm.

The rewrite happens one level down, in that agreement step. `src/lib.rs`'s
`handle_placeholder_impl` calls `inflect_verb_custom_with_context`
(`src/lib.rs:2003-2012`, defaulting to `inflect_verb_custom`,
`src/lib.rs:1962-1972`) for every `PostSpec::Verb`, and the default
implementation is `src/language/english.rs`'s `inflect_verb`
(`src/language/english.rs:98`), which runs `IrregularPluralVerb::from_str(s)`
on the captured word and, for the `"I"` branch, applies `first_person()`
(`:70-80`) — `Were => Some("was")` at `:75` — unconditionally. The `"he" /
"she" / "it"` branch (`:114-124`) does the same via `third_person()`
(`:82-94`), `Were => Some("was")` at `:87`. Both mappings are correct for the
*indicative* (`"you were"` → `"he was"` is right, and is pinned by the
regression tuple `(IrregularPluralVerb::Were, Some("was"), Some("was"))` at
`:555`), and both fire regardless of mood, because nothing at this call site
carries mood. Indicative-vs-subjunctive is a property of the clause the
caller wrote (`if`, `wish`, `as though`, mandative `demand that`), which
lives in the template text surrounding the placeholder — `if` is a literal
word before `{=i were}`, not inside it — and is not recoverable from the verb
`were` itself. So the byte that has to be suppressed is this call to
`inflect_verb_custom_with_context` (or, more precisely, its default's read of
`IrregularPluralVerb::Were`), not anything on the `PostSpec::Tense` path.

### Why a smarter conjugator can't do it, and why the existing mapping can't just be deleted

`inflect_verb` cannot distinguish `"If {=i were} rich"` from `"Yesterday
{=i were} at the store"` (the latter being simply wrong English a caller
typed, which is not this crate's job to fix either) — both hand it the exact
same three arguments (`subject = "I"`, `verb = "were"`, `as_plural`/`uc`).
Deleting the `Were => Some("was")` arms to "fix" the subjunctive breaks the
indicative, which is the far more common case and is pinned at `:555`. The
fix has to be additive — a way for the caller to say "this word, exactly as
written" — not a change to what the existing mapping does.

## What the code does today: the full taken-marker inventory

`.claude/rules/placeholder-grammar.md` is the map of this grammar's traps;
this section is the literal inventory it asks for, read from
`ranting_core/src/grammar.rs`'s `PH_EXT` (the reference regex) and
`ranting_core/src/ph_ext.rs` (the hand-written parser `ranting_derive`
actually calls), not from memory.

`PH_EXT`, reformatted:

```text
^(?x)
    (?P<uc>[,^])?+
    (?P<pre>(?: ... ))?+
    (?P<nr>[+-]|(?:\#|\??\$)\w+\s+)?+
    (?P<case>\*[`=@~%]|[`=@~*?%])?+
    (?P<noun>[\w-]+)
    (?P<post>\s+[<=>%!]*(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$
```

Every character with a defined meaning inside a placeholder, by capture
group:

| Group | Characters | Meaning |
|---|---|---|
| `uc` | `,` `^` | force-lowercase / force-uppercase override |
| `pre` | `a`/`an`/`some`/`the`/`these`/`those`/`` ` ``-prefixed word, plus modal-verb words, plus (Phase 6 item 26) one arbitrary literal word before the placeholder | article/modal detection, preposition-fusion hook input — no bare *symbol* beyond `` ` `` and the `?` prefix |
| `nr` | `+` `-` `#` `$` (optionally `?`-prefixed) | plural/singular force, spelled-out numeral, digit numeral, hidden-numeral prefix |
| `case` | `` ` `` `=` `@` `~` `*` `?` `%`, plus the fused two-character forms `` *` `` `*=` `*@` `*~` `*%` (Phase 6 item 19) | possessive substitution / subjective / objective / possessive-determiner / display-as-name / hidden / reflexive |
| `post` (marker prefix) | `<` `=` `>` `%` `!` (and the two-character combos `<=`, `<%`, and the degree-only `!!`) | tense (`TenseMarker`) or degree (`DegreeKind`) |
| `post` (suffix form) | `'` | possessive-`s` |

Union of every character that already means something somewhere in this
grammar: `` , ^ + - # $ ` = @ ~ * ? % < > ! ' ``. Seventeen ASCII punctuation
characters, several (`=`, `%`) already carrying **two different meanings**
depending on whether they appear in `case` (pre-noun) or `post` (post-noun)
position — `.claude/rules/placeholder-grammar.md` documents this dual use for
the fused-marker family, and it is not free: a reader has to know which side
of the noun a character is on to know what it means.

What is *not* claimed, and why each is disqualified or awkward:

- `{` `}` — the placeholder delimiters themselves. Cannot be used.
- `:` — structurally excluded. `PH_START`'s outer match is
  `` \{(?:(?P<plain>\w*+)|(?P<ranting>[^{}:]*+))(?P<fmt>:.*?)?\} `` — the
  capture for everything *inside* the braces is `[^{}:]*+`, which excludes
  `:` by construction, because `:` starts the `:fmt` spec. A marker
  character can never be `:`; the outer grammar forbids it before `PH_EXT`
  is even reached.
- `_` — inside `\w`, so it is already part of every `[\w-]+` noun/verb/word
  match. Using it as a marker would be ambiguous with an identifier that
  happens to start or end with an underscore.
- `"` `\` — legal but expensive: both require escaping inside the Rust
  string literal `say!()`'s template argument actually is (`\"` and `\\`),
  an ergonomic tax on every use that none of the seventeen taken characters
  carry.
- `.` — free inside `PH_EXT`, but is the primary ASCII sentence-terminator
  character in `PH_START`/`SENTENCE_TRIGGER_CHARS`, just outside the braces.
  Not structurally forbidden, but reusing a sentence-final character as an
  in-placeholder marker invites exactly the kind of positional-overload
  confusion the `=`/`%` dual-use already costs.
- `;` `|` `&` `/` `(` `)` `[` `]` — free, unclaimed anywhere in the grammar,
  no escaping cost, no existing positional overload.

### Which character

The task in front of this spike is not to pick the one true character — a
Locked-grammar change is a maintainer call — but to narrow to a defensible
shortlist and state the tiebreak, so a maintainer who prefers a different
member of the shortlist can swap it in without redoing this analysis.

Shortlist, all clean by the criteria above: **`;`**, `|`, `&`, `/`. (The
bracket characters `(` `)` `[` `]` are technically clean too but read as
grouping/optionality syntax to anyone who has seen a regex or an EBNF
grammar, which this crate's own doc comments lean on constantly — reusing
that visual vocabulary for an unrelated meaning seems like an avoidable
readability tax.)

**Recommendation: `;`.** Tiebreak applied: unclaimed anywhere in `PH_EXT`,
no escaping cost inside a `say!()` string literal, no existing positional
overload, and (weakly) a semicolon reads as "stop; nothing more happens to
this clause," which is the actual contract. `{=i ;were}` would render
`"I were"`. This is a preference among equals, not a structural argument —
if a maintainer prefers `|` or `&` for a different mnemonic, nothing above
rules it out.

## Before or after the noun

The marker has to bind to the verb it protects, and the verb is always in
`post` — after the noun. A pre-noun marker (in `case`, alongside `` ` `` `=`
`@` `~` `*` `?` `%`) would have to reach forward across the noun to a word it
has no syntactic relationship to; nothing else in this grammar has a
pre-noun marker govern a post-noun word, and `case`'s marker governs how the
*noun itself* is displayed, an unrelated axis. So the marker belongs in
`post`, in the same character-class position as `<`/`=`/`>`/`%`/`!` —
immediately before the verb word it protects, e.g. `{=i ;were}`.

## What it actually is: a fifth `PostSpec` shape, not a sixth `TenseMarker`

This is the load-bearing scoping decision, easy to get wrong by pattern-
matching on "it's another character in the `[<=>%!]*` class, so it must be
another `TenseMarker`." It must not be. `TenseMarker::from_marker`
(`ranting_core/src/placeholder.rs:99-109`) maps a marker to a *different*
tense — `to_past`/`to_continuous`/`to_future`/`to_past_participle` — and the
whole point here is the opposite: apply **no** conjugation and **no**
person/number agreement, emit the captured word exactly as written. That is
a new `PostSpec` variant — `PostSpec::Verbatim(&'static str)` alongside the
existing `None`/`PossessiveS`/`Verb`/`Tense`/`Degree` — not a seventh
`TenseMarker` arm. Concretely, `ranting_derive/src/lib.rs:706-796`'s
marker-classification branch would need a fourth arm (today it is
plain-verb / degree / tense) before the fallback tense match, and
`;` would have to be excluded from `TenseMarker::from_marker`'s domain so
that function stays a total description of the six real tenses.

**What this touches beyond the subjunctive.** A verbatim marker suppresses
agreement for *any* post-noun verb, not just `were` — `{=0 ;have}` staying
`"have"` regardless of `0`'s person/number, archaic/dialectal forms
(`{=thou ;art}`), quoted direct speech reproduced verbatim inside a
placeholder, and any other case where a caller's hand-written verb form is
already correct and agreement would break it. This is the general "verbatim
escape hatch" ROADMAP.md's item framing already names — the subjunctive is
the motivating case, not the scope.

### Interaction with the rest of the `post` grammar

Adding a character to `[<=>%!]*` means it can appear next to the characters
already in that class, and the class already has combination semantics
(`<=`, `<%`) and explicit rejections (`handle_param`'s "degree marker
`!`/`!!` cannot be combined with tense markers", `ranting_derive/src/lib.rs:746-752`).
Three combinations need an answer before this is implementable, not just
namable:

- **`;` with a tense marker** (`{=i ;<were}` / `{=i <;were}`) — contradictory
  instructions ("apply no conjugation" and "conjugate to the past") on the
  same word. This should be a compile error, through the same
  `StrLitSlice::error` path the existing tense/degree conflict already uses
  (`ranting_derive/src/lib.rs:746-752`), not a silent pick of one meaning
  over the other.
- **`;` with a degree marker** (`{noun ;big}` vs. `{noun !big}`) — degree
  resolution (`to_comparative`/`to_superlative`) happens at compile time in
  `handle_param`'s `marker.chars().all(|c| c == '!')` arm
  (`ranting_derive/src/lib.rs:719-745`); verbatim means "resolve nothing."
  The two are incompatible for the same reason as tense: `;` should not be
  accepted inside the all-`!` degree arm, and mixing them should error the
  same way tense-plus-degree already does.
- **`;` under `say_with!()`'s runtime-tense override** — this is the one
  that actually threatens the escape hatch, not just a parser edge case.
  `PostSpec::Verb`/`Tense` bake the *base* form when `runtime_tense` is set
  (`ranting_derive/src/lib.rs:757-759`) so `NarrationContext.tense` can
  re-resolve them per call. If `PostSpec::Verbatim` were reachable by that
  same override, a caller's protected `were` could be silently rewritten by
  a per-call tense after all — reopening exactly the mood-varies-per-clause
  problem "Why a `NarrationContext` mood flag is the wrong shape" argues
  `NarrationContext` cannot correctly carry. `PostSpec::Verbatim` must stay
  inert under `say_with!()`'s tense machinery: `runtime_tense` is a
  `PostSpec::Tense`-only concern, and `Verbatim`'s word is fixed at compile
  time with no runtime override path, full stop.

That breadth raises one more design fork a maintainer has to rule on, not
just implementers: **does `PostSpec::Verbatim` bypass
`inflect_verb_custom_with_context` entirely, or still call it with a
signal meaning "don't touch"?** Every other `PostSpec::Verb` reaches the hook
(`src/lib.rs`'s `handle_placeholder_impl`, the `inflect_verb_custom_with_context`
call sites at `src/lib.rs:1091`/`1127`) so a fork's `Ranting` impl gets a say
over agreement in its own language. If verbatim bypasses the hook entirely,
it is a crate-level "no inflection happens here, full stop" — simpler, and
correct for English's own subjunctive, but it removes a fork's chance to do
something with the word (a language whose subjunctive is a *different*
verb form, not an unconjugated one, would want the hook called with a "this
is subjunctive" signal, not skipped). If verbatim still calls the hook,
one of two things must be true: the hook's default (`inflect_verb`) must
learn to special-case `Verbatim` too, or the hook's signature grows a
mood/verbatim parameter it did not have before — which is a real signature
break of the kind `.claude/rules/extension-hooks.md`'s "ninth hook pair"
discussion in the preposition-fusion spike flags as something this repo
bundles rather than ships piecemeal. This spike does not resolve that fork;
it names it as the second decision a maintainer needs to make alongside the
character.

## Cost to keep `PH_EXT` and `ph_ext` in parity

`.claude/rules/placeholder-grammar.md` states the rule this repo already
lives by: `PH_EXT` is the *reference grammar*, `ranting_core::ph_ext::parse`
is the hand-written implementation `ranting_derive` actually calls, and
`ph_ext.rs`'s own differential fuzzer (`assert_parity`, comparing
`Regex::new(PH_EXT)` against `parse_pass(.., PreWords::English)`, exercised
by `parity_fuzzed`) is what catches the two drifting. The `[<=>%!]*`
character class that would need to gain `;` exists in exactly three places,
verified by grep — no fourth hand-maintained copy exists:

1. `ranting_core/src/grammar.rs:141` — `PH_EXT`'s own `post` group, the
   reference regex text.
2. `ranting_core/src/ph_ext.rs:684` — the hand-written parser's
   `take_while(|c| matches!(c, '<' | '=' | '>' | '%' | '!'))`, which has to
   gain the same arm.
3. `ranting_derive/src/lib.rs:703` — `handle_param`'s own, separately
   maintained, identical `take_while`, which extracts the marker run at
   compile time to classify it into a `PostSpec` variant. This one is not
   fuzzed against `PH_EXT` at all (it is downstream of `ph_ext::parse`
   already having matched, and only re-derives the classification), so a
   maintainer editing site 1 and 2 but forgetting site 3 would not be
   caught by `parity_fuzzed` — it would surface as `;` parsing successfully
   at the `ph_ext` layer but the derive macro falling into the "no marker"
   `PostSpec::Verb` branch and emitting `";were"` verbatim as a literal
   word, silently wrong rather than a compile error. This is the one real
   parity risk worth naming precisely, because it sits outside the
   fuzzer's coverage.

Net: two of the three sites are protected by an existing, exercised
mechanism (`parity_fuzzed`), which is what makes this cost bounded rather
than a silent-drift risk; the third needs its own test — a
`;`-marker placeholder compiled and rendered, not just parsed — the same
category of coverage `tests/ranting/` already gives every other marker
family.

## Why a `NarrationContext` mood flag is the wrong shape

ROADMAP.md's Phase 8 item 2 framing already rules this out; the concrete
failure case is worth stating plainly. `NarrationContext` is documented
(`.claude/rules/extension-hooks.md`) as **per call, not per story** — one
`say_with!()` invocation, one context. But mood is a property of the
*clause*, and one template routinely mixes clauses of different mood in a
single call:

```rust
say_with!(&ctx, "If {=i were} rich, {=0 is} happy.", speaker, other)
```

The first clause is counterfactual (subjunctive `were`); the second is a
plain indicative observation (`is`) inside the very same `say_with!()` call.
A single `ctx.mood` field cannot be set to a value that is correct for both
placeholders at once — there is no per-call flag that resolves to the right
answer for two clauses with different moods in one call. `register` and
`dialect` get away with being story-wide precisely because both are
properties of the *telling*, constant across an utterance; mood is a
property of the individual clause, which is exactly what a per-placeholder
template-text marker captures and a per-call context field structurally
cannot. This is also the same "which axis does the signal actually vary
on" question `.claude/rules/extension-hooks.md` already answers for
`register` (T-V pronoun selection rides the addressee's own declared
subject label, not `NarrationContext`, for the same reason) — mood needs
its own answer, and the answer is the same shape: per-placeholder, not
per-call.

## Options, scored

### (a) Verbatim marker in `post` — recommended

Add a new `post`-position character (shortlisted above) meaning "this verb
form is final, apply no conjugation and no person/number agreement," baked
as a new `PostSpec::Verbatim(&'static str)` variant. Closes §1.5 exactly:
`say!("If {=i ;were} rich, …")` renders `"If I were rich, …"` with no change
to `{=i were}`'s existing (indicative-appropriate) output — English stays
byte-identical for every template that doesn't opt in. General beyond the
subjunctive, per "What it actually is" above. Cost: one new `PostSpec`
variant, one new marker-classification arm in `ranting_derive/src/lib.rs`,
three character-class sites in lockstep (bounded, two of three fuzzer-
protected, see above), and the hook-bypass-vs-hook-signal design fork
resolved one way or the other.

### (b) `NarrationContext` mood flag — rejected

See "Why a `NarrationContext` mood flag is the wrong shape" above: mood is
per-clause, `NarrationContext` is per-call, and a single realistic
`say_with!()` invocation can contain clauses of different mood. Rejected on
structural grounds, not merely inconvenience.

### (c) Do nothing; caller writes the whole clause as literal template text — rejected as a non-fix

A caller who wants `"If I were rich"` today can already get it by not using
a placeholder for the verb at all: `say!("If {=i} were rich, …")`. This
already works and needs no crate change. It is not a fix for §1.5, though,
because it only helps when the subject is written as a bare pronoun
placeholder with the verb as literal text immediately after — the defect is
specifically that `{=i were}` (verb *inside* the placeholder, which is the
natural way to write it, and the form every other verb-bearing placeholder
in this crate uses) silently corrupts correct input. Recorded because it is
the "no code change" option every spike in this directory considers, and
because it is worth being explicit that it is a workaround for the pattern
that trips people up, not a fix for the placeholder form the defect
report was filed against.

## Recommendation

Ship **(a)**: a new post-position marker character, `;` per the tiebreak
above (shortlist `;`/`|`/`&`/`/` if a maintainer prefers a different
member), baked as `PostSpec::Verbatim(&'static str)`, bypassing
person/number agreement for the marked word. This spike does not implement
it — the sigil grammar is Locked and a grammar change needs a maintainer's
sign-off, not a spike's. Two decisions are left open for that sign-off:
the exact character, and whether `Verbatim` bypasses
`inflect_verb_custom_with_context` entirely or still calls it with a
"don't touch" signal (see "What it actually is" above) — the latter costs
a hook-signature break this repo's own practice says to bundle with any
other owed one rather than ship alone.

## What stays impossible under this recommendation, until implemented

- `say!("If {=i were} rich, …")` keeps rendering `"If I was rich, …"` — §1.5
  stays open — until a maintainer picks a character and the marker actually
  ships.
- No change to indicative `were`→`was` agreement, which stays correct and
  pinned at `src/language/english.rs:555`.
- No change to any other marker family; the seventeen already-taken
  characters keep their existing meanings unchanged.

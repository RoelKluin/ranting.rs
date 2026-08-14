# Spike: modular authoring languages and modular output languages

**Status**: spike complete and **option 1 shipped** (2026-08-14). Output-language
modularity already worked; authoring-language modularity was blocked by one
grammar slot, and that block is now removed — a template may write its own
article keyword (`{el *=gato}`) and the language module inflects it
(`{el +*=gato}` → `los gatos`), with `ranting` itself knowing no non-English
vocabulary. Option 1's only cost — losing an accidental compile error on a
misspelled article — was accepted by the maintainer (see "Decision", below).

Implementation notes, and one limitation found while building it, are in
"As shipped" at the end of this document. The design discussion below is kept
as written, since it is what the implementation was chosen from.

## Motivation

The request: language should be modular on two axes. A developer should be able
to *write* templates in their own language, and to ship an application that
*renders* in several languages, assembling third-party language modules rather
than writing every rule personally.

Concretely: someone writing in Russian, whose application supports Russian,
English and Finnish, and who has those three modules available.

Two prior spikes bound the problem before this one starts.
`2026-08-13-word-order-feasibility.md` (Phase 6 item 1) makes word order a
permanent boundary: one template per language per sentence, written by the
caller. `2026-08-13-template-selection.md` (Phase 6 item 22) settles how the
caller picks between them — an ordinary `match`, option 1, no crate machinery.
So "supports three languages" always means at least three templates. This spike
does not reopen either. It asks what is left once those are granted.

## Axis B (output languages): already works — verified

This was tested rather than reasoned about, since two language implementations
already exist in-tree. A scratch binary depending on `ranting`, `ranting-i18n`
and `ranting-es` simultaneously, using the public API only:

```rust
struct Cat {
    de: ranting_i18n::GermanNoun,
    es: ranting_es::SpanishNoun,
}

fn describe(lang: Lang, cat: &Cat) -> String {
    match lang {
        Lang::De => say!("Ich sehe {the *@0}.", cat.de),
        Lang::Es => say!("Veo {the *=0}.", cat.es),
    }
}
```

Output:

```
DE: Ich sehe die Katze.
ES: Veo el gato.
```

Both modules coexist in one binary, both render correctly, and neither needed
`ranting_core` or `ranting_derive` — the falsifier invariant holds under
composition, which had not previously been checked (each falsifier had only
ever been built alone). Nothing in the crate prevents a third module joining
them; `Ranting` is an ordinary public trait and a language module is an
ordinary crate implementing it on its own noun type.

**So the user's scenario is feasible today on this axis.** Three module crates,
combined by an application neither of them knows about, is exactly what the
above demonstrates at N=2.

One qualifier on "works today", since the experiment proves *coexistence*
specifically: adding a fourth language costs one entity field and one `match`
arm per sentence, both in application code. It costs no change to `ranting` and
no cooperation between the modules — but the entity is not generic over
language, so this is "assemble N modules", not "drop in a module and the app
adapts". Making it the latter is the `Language`-trait question deferred below,
and it is ergonomics rather than feasibility.

### What a language module can and cannot supply

Worth stating precisely, because "modules available" can be read as more than
it is. Language is bound to the **type**: `ranting_i18n::GermanNoun` and
`ranting_es::SpanishNoun` are unrelated types, each carrying its own closed
lexicon (`SpanishNoun` holds an `entry: &'static NounEntry`, `ranting_es/src/noun.rs:15`).
Neither reads `NarrationContext::dialect`; language is not selected per call.

A module can therefore supply **rules** — article selection, agreement,
elision, case. Only the application knows that *its* `Cat` is `Katze` in German
and `gato` in Spanish, so per-language **lexical data per entity** stays the
caller's, in the same way templates do. The cost of the user's scenario is
three templates per sentence, plus three name/gender/plural datasets per
entity, plus three rule modules. The modules are the third of those and the
smallest — they are also the only part that can genuinely be someone else's
work, which is what the request actually asked for.

Note also that `say!()` passes `None` for context, so any future
language-selected-*by-context* design would make `say_with!()` the multilingual
macro and `say!()` the English fast path. That is already the de facto split
(Phase 6 item 12 existed because the `_with_context` hooks were otherwise
unreachable downstream).

## Axis A (authoring language): blocked, and the block is one regex

Today a non-English template must still be written with English keywords. This
renders correctly — the Spanish module's `inflect_article_custom` turns the
English keyword into Spanish output:

```
say!("X {the *=0}.", gato)   =>   "X el gato."
```

Writing the Spanish keyword instead fails. Measured, three distinct failures:

| Template | Result |
|---|---|
| `{the *=0}` | works — English keyword, Spanish output |
| `{el *=0}` | compile error: ``expected article or verb, found ` *=0` `` |
| `{el 0}`, `{el gato}` | compile error: `E0425: cannot find value 'el' in this scope` |

The second failure mode is the worse one: it names a variable the author never
wrote, because with no recognized pre-word the first word is taken as the noun.

### The single gate

Both closed vocabularies live in **one place** — the `pre` group of
`ranting_core::grammar::PH_EXT` (`ranting_core/src/grammar.rs:130`), mirrored by
hand in `ranting_core::ph_ext`'s recursive-descent parser:

```
(?P<pre>(?:
    \??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se|`[\w-]+|
    (?:[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
    (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
    ...
```

That is two English vocabularies, not one:

- **articles** — `a`/`an`/`some`/`the`/`these`/`those`
- **pre-noun verbs** — a closed set of English modals and auxiliaries:
  `can`, `may`, `shall`, `will`, `are`, `were`, `have`, `had`, `do`, `could`,
  `would`, `should`, `must`, `might`, each optionally `n't`

The verb half was not previously documented as closed. Verified: `{haven't =gato}`,
`{are =gato}` and `{do =gato}` all compile; `{walk =gato}` and `{sees =gato}`
do not — an ordinary English lexical verb is rejected exactly like a Spanish one.

Downstream of the regex, two smaller mirrors of the article list exist:
`article_kind_tokens` in `ranting_derive/src/lib.rs:886-888` (word → `ArticleKind`)
and `get_article_or_so`'s match in `src/lib.rs:262-342`.

### Why this is narrower than it looks

`ArticleKind` turns out to be a **role label, not an English leak.** Every arm of
`get_article_or_so` calls `inflect_article_custom_with_context` *first*, passing
the word as written, and only falls back to English (`adapt_article`,
`get_a_or_an`) when the hook returns `None`. The variant selects which fallback
shape and which singular-derivation strategy to use — nothing more.

The one exception is the gap: **`ArticleKind::Other` returns `None` without ever
calling the hook** (`src/lib.rs:342`). An unrecognized word is not offered to the
language module at all. That single arm, plus the regex that decides what counts
as recognized, is the entire mechanism standing between today's behavior and
native-language keywords.

Two further pieces of Axis A are **already solved** and need no work:

- **Verb conjugation** — `say_with!()`'s `runtime_tense` path bakes the
  uninflected base verb instead of an English-conjugated form
  (`ranting_derive/src/lib.rs:692-707`).
- **Adjective degree** — `PostSpec::Degree` carries `base` alongside the
  English `word`, so the hook receives the adjective as written.

So Axis A reduces to the pre-word slot alone.

## The hard constraint on any Axis A design

`say!()`'s literal is parsed at proc-macro expansion time, before the
surrounding crate exists as IR. This is the same ✅ **Locked** decision
`2026-08-13-template-selection.md` leaned on. The consequence here: **the
keyword vocabulary must be known at expansion time.** A runtime registry — a
language module registering its articles with `ranting` at startup — cannot
work, for the same reason a runtime template catalogue cannot. Whatever selects
the vocabulary is either compile-time configuration or nothing.

## Options, scored

| Option | Works with compile-time parsing? | Cost | Notes |
|---|---|---|---|
| **1. Widen the grammar; let the hook take first refusal** — accept any word in the pre-slot, classify unknown ones as `ArticleKind::Other`, and route `Other` to `inflect_article_custom` instead of returning `None` | **Yes** — purely a grammar and dispatch change, no registration anywhere | Small-to-moderate: `ph_ext`'s pre-slot, one runtime match arm, plus the `PH_EXT` oracle | **Prototyped end-to-end; see below.** No per-language registration at all, and it matches how the rest of Phase 6 works (hook first, English fallback). **Cost**: a typo'd article (`{teh gato}`) stops being a compile error and starts rendering literally — but see "the asymmetry" below, which is the strongest argument *for* option 1 |
| **2. Per-language cargo features** — `ranting = { features = ["es", "de"] }` extends the compile-time vocabulary | Yes | Moderate, and grows with every language | Features are additive and unify globally, so all-on is the normal case; vocabularies would collide across languages with no way to say which is meant. Also puts every language's keyword list inside `ranting`, which is precisely what modularity was meant to avoid |
| **3. Vocabulary declared at the call site or module** — e.g. an attribute naming the keyword set | Yes | Larger: a new configuration surface `say!()` does not have | Keeps vocabularies out of `ranting`, but proc macros cannot see other crates' items at expansion time, so the set must be spelled out locally — which is close to writing the words anyway |

Option 1 is the only one that preserves the modularity the request is about:
the language module supplies the vocabulary *by answering the hook*, and
`ranting` never learns any language's word list.

## Option 1 prototyped: it works, and two things about it are non-obvious

Option 1 was built as a throwaway (since reverted; the tree is clean) to check
whether the two halves are separable and whether existing templates survive.
Both answers are counterintuitive enough to record.

### Non-obvious 1: widening the grammar *alone* is worse than the status quo

With the pre-slot widened but `ArticleKind::Other` still returning `None`, the
native keyword parses and renders — but as **inert literal text**:

```
say!("Veo {el *=gato}.", gato)    =>  "Veo el gato."     -- looks right
say!("Veo {el +*=gato}.", gato)   =>  "Veo el gatos."    -- WRONG, no agreement
say!("Veo {the +*=gato}.", gato)  =>  "Veo los gatos."   -- English keyword, correct
```

The module never sees `el`, so it cannot inflect it to `los`. A compile error
was traded for silently wrong output. **The two halves are a package**; shipping
the grammar change without the dispatch change would be a regression.

With both halves, plus the module accepting its own keywords (three lines in
`ranting_es`), every case is correct:

```
{el *=g}  => "el gato"     {el +*=g} => "los gatos"
{la *=c}  => "la casa"     {la +*=c} => "las casas"
{un *=g}  => "un gato"     {the +*=g} => "los gatos"   -- English still works
```

`ranting` learned no Spanish in the process: the vocabulary lives entirely in
the module's `inflect_article_custom` match arm. That is the modularity the
request asked for, demonstrated.

### Non-obvious 2: the widened slot must be a *second pass*, not an extra candidate

The natural implementation — add "any word" to `pre_one_rep`'s candidate list at
lowest priority — **breaks 15 existing English call sites**. `pre` is a greedy
repeated group, so a longer `pre` wins over the noun-only parse regardless of
candidate order: `{w is}` starts parsing as `pre = "w "`, `noun = "is"`, and the
macro emits `cannot find value 'is' in this scope`. The widened pre-slot
collides with the post-noun verb slot, which is precisely the ambiguity the
closed English vocabulary was resolving.

The fix is to make widening a **fallback pass**: run the strict grammar first,
and retry with the open pre-slot only for templates the strict pass *rejects*.
Every template that compiles today takes pass 1 and is byte-identical by
construction. With that change:

| Crate | Result |
|---|---|
| `ranting` | 39 + 399 tests, 15 doctests — all pass |
| `ranting_derive` | 9 tests — pass |
| `ranting_i18n` | 31 tests — pass |
| `ranting_es` | 23 tests — pass |
| `ranting_core` | 25 pass, **2 fail** |

The two failures are `ph_ext::tests::parity_curated_corpus` and `parity_fuzzed`
— the differential fuzz comparing the hand-written parser against the `PH_EXT`
regex oracle, which the prototype deliberately left un-widened. That is a known
cost, not a surprise: a real implementation widens `PH_EXT` to match, or accepts
that the oracle now describes only the strict pass.

So the implementation cost of option 1 is roughly: one fallback pass in
`ph_ext::parse`, one widened matcher in `pre_one_rep`, one runtime match arm in
`get_article_or_so`, and a decision about the `PH_EXT` oracle.

### Non-obvious 3: the asymmetry — the post-noun slot is already wide open

The apparent cost of option 1 is losing a compile error on an unrecognized
pre-noun word. Measured against what the *post*-noun slot already does, that
cost is smaller than it looks, because the crate has already answered this
question the other way:

| Template | Today |
|---|---|
| `{teh gato}` | compile error — `cannot find value 'teh' in this scope` |
| `{=gato wlak}` | renders `él wlak` — unknown word accepted silently |
| `{=gato <wlak}` | renders `él wlaked` — unknown word accepted *and conjugated* |

An invented word after the noun is not merely tolerated, it is inflected: the
tense marker applies `to_past` to `wlak` and produces `wlaked` without
complaint. So "the crate catches your typos" is not a property `ranting` has;
it is a property the pre-noun slot alone has, as a side effect of that slot's
vocabulary being closed for parsing reasons rather than for diagnostic ones.

That reframes option 1's cost. It does not introduce a new permissive
philosophy — it makes the pre-noun slot consistent with the post-noun slot,
where the permissive choice was already made. The words losing protection are
the ~6 articles and ~14 modals/auxiliaries listed above.

### Decision: the lost typo check is not `ranting`'s job (maintainer, 2026-08-14)

Resolved rather than left open. **`ranting` is not a spelling corrector**;
correction is a separate concern and, if wanted, a separate plugin. Losing the
accidental check on `{teh gato}` is therefore accepted, not merely tolerated —
it removes a behavior the crate never intended to offer and offers inconsistently
(non-obvious 3), rather than sacrificing a feature.

Two consequences worth stating precisely, because "another plugin" is reachable
in one sense and not in the other:

- **A compile-time check cannot be a `ranting_derive` *plugin*, in the sense of
  third-party code.** A proc macro receives only a `TokenStream` and runs with
  its *own* dependency graph; it cannot call into a crate the downstream user
  chose. The dependency arrow points the wrong way — `ranting_derive` picks what
  it links against, and no downstream crate can inject itself into that. This
  is the same constraint that rules out options 2 and 3 above.
- **But it *can* be configuration, which is a real and under-appreciated
  mechanism.** Verified by instrumenting `say_with`'s expansion: a proc macro
  sees `CARGO_MANIFEST_DIR` pointing at **the downstream crate**, not at
  `ranting_derive` (probe output: `manifest_dir=".../polyglot" pkg="polyglot"`).
  A proc macro is ordinary code and may read files, so `ranting_derive` could
  read a `ranting.toml` from the user's crate root and check against a word list
  declared there. That is data supplied by the user, not code — a curated
  first-party check with user-supplied input, not an open extension point.
- **Runtime correction is already reachable**, and becomes more so under
  option 1. Once `ArticleKind::Other` routes to `inflect_article_custom`, the
  language module is handed the literal word — `"teh"` included — and may do
  whatever it likes with it: correct it, log it, or return `None` and let it
  render as written. Here a language module genuinely *is* the plugin: it is
  third-party code, chosen by the user, receiving the word.

So the honest split is: `ranting` parses and inflects; a language module — real
third-party code — may validate what it is handed at runtime; and a compile-time
check is possible but would be first-party code in `ranting_derive` driven by
user-supplied configuration, or an external lint reading the source. It cannot
be third-party code running at expansion time.

Note that the config-file mechanism does **not** reopen options 2 and 3 for
*vocabulary*. It could carry a word list, but doing so would make the
application restate vocabulary its language module already knows, duplicating it
in a second place that can drift. Option 1 needs no configuration at all, which
is why it still wins for vocabulary even though a configuration channel exists.

## What this spike does not decide
- **The `Language` trait question.** Refactoring the 23-method `Ranting` trait
  into an entity part and a delegating language part would let one entity render
  in several languages without one field per language. The experiment above
  shows the field-per-language shape works, so this is an ergonomics question,
  not a feasibility one. Deliberately out of scope.
- **`{el 0}`'s diagnostic.** Even if native keywords are never supported, the
  `E0425: cannot find value 'el'` error names a variable the author never wrote.
  That is a diagnostics defect worth fixing independently. *Investigated and
  partly closed by Phase 7 item 8 (2026-08-14): the message is rustc's and the
  shape is undecidable, so only the span could be narrowed. See the appendix.*

## Relation to Phase 7

This is a new item, orthogonal to items 1-4. Item 1 (unused-hook audit) feeds it
directly — a hook no module overrides should not gain new responsibilities.
Items 2 and 3 (Arabic, Japanese) ask whether *more* hooks are needed; this asks
how hooks are *packaged*.

---

## As shipped (2026-08-14)

Three changes, matching option 1 as scored, with the prototype's thread-local
replaced by a threaded parameter:

1. **`ranting_core::ph_ext::parse` runs two passes.** `parse_pass(s,
   PreWords::English)` first — the closed vocabulary the grammar has always had
   — and `parse_pass(s, PreWords::Open)` only if that fails, with the strict
   error returned on double failure so diagnostics are unchanged for genuinely
   malformed input. `PreWords` is threaded into `pre_one_rep` through the
   `star_candidates` closure rather than carried in thread-local state, since a
   differentially-fuzzed parser should not have invisible mode.
2. **`pre_one_rep`'s open arm replaces the vocabulary rather than extending it**
   — one word (`leading_word_or_apostrophe_len`, so `haven't` and `l'` still
   work), with the same trailing-whitespace requirement every English branch
   imposes via `finish_pre_candidates`.
3. **`get_article_or_so`'s `ArticleKind::Other` arm calls
   `inflect_article_custom_with_context`** instead of returning `None`, mapping
   `Some` to `custom + space` and leaving `None` to render the word as written.

### The open pass allows `pre` exactly one repetition

Found by testing after the first commit, and worth recording because the cause is
non-local. `pre` is a **repeated** group whose capture keeps only the *last*
repetition (see `ph_ext`'s module doc on `X?+`). An open pre-word slot therefore
matches `{de the *=gato}` as two repetitions — `de `, then `the ` — and the
capture retains only `the `. The runtime then rendered the article and **`de` was
silently dropped**: `say!("{de the *=0}", gato)` produced `"El gato"`.

Restricting the open *matcher* to one word does not fix this; the repetition
happens a level up, in `star_candidates`. The fix is in `parse_pass`: for the
open pass, skip any candidate whose `pre` span does not start at the group's own
start offset, which admits only a single repetition. `{de the *=gato}` and
`{de el *=gato}` are parse errors again, as they were before this pass existed —
the pre-noun-slot restriction `ranting_i18n`'s hole 7 documents is unchanged.

The general shape is worth remembering when touching this parser: **a new
alternative in a repeated group is not local to that alternative.** Pinned in
`open_pass_only_for_input_english_rejects`.

### `PH_EXT` stays the English grammar, and parity now targets the English pass

The plan was to widen `PH_EXT` to match. That turned out to be impossible in
the useful sense: a single regex has **one** preference order, so it cannot
express "prefer the English reading of `{w is}`, fall back to the open reading
only if there isn't one". Widening `PH_EXT`'s alternation would make it accept
the same *language* but disagree on *captures*, which is exactly what the
differential test checks.

So `assert_parity` now compares `PH_EXT` against `parse_pass(input,
PreWords::English)` rather than against `parse`. `PH_EXT` remains the exact
reference grammar for the English pass — which is what the differential fuzz was
protecting — and the open pass is pinned separately by
`open_pass_only_for_input_english_rejects`, which asserts both directions: that
accepted English templates parse identically through `parse`, and that the
newly-accepted inputs are ones the English pass rejects.

### Limitation found while building: the noun needs a marker

The open pass runs **only when the English pass fails**, and an unmarked
two-word placeholder does not fail — `{la casa}` parses as `noun = "la"`,
`post = " casa"`, the ordinary noun + post-noun-verb reading. So:

| Template | Result |
|---|---|
| `{el *=gato}`, `{el +*=gato}`, `{la *=casa}` | native article, inflected by the module |
| `{el gato}` | unchanged — still `noun = "el"`, hence `E0425: cannot find value 'el'` |

A non-English template therefore needs a case marker on the noun for its article
to be read as an article. That is not a regression (nothing worked before), and
it follows from the priority ordering that makes English byte-identical — the
two cannot both be had. Pinned by
`ph_ext::tests::open_pass_only_for_input_english_rejects` and
`tests/ranting/native_article_keyword.rs::unmarked_two_word_placeholder_keeps_the_english_reading`.

This also leaves the `{el 0}` diagnostic defect below open, and slightly
sharpens it: the confusing `E0425` is now the *only* remaining failure mode for
a native-keyword template, so improving that message is worth more than it was.
*Taken up as Phase 7 item 8 and found to be only partly fixable — see the
appendix's "The `{el 0}` diagnostic, as far as it goes" section.*

### Both falsifiers use it

The falsifier contract is that a gap is only closed when a real fork can reach
it through the public API, so both were updated rather than left writing English
keywords inside their own templates:

- `ranting_es` accepts `el`/`la`/`los`/`las`/`un`/`una`/`unos`/`unas`, pinned by
  `native_spanish_article_keywords` in `ranting_es/tests/spanish.rs`.
- `ranting_i18n` accepts `der`/`die`/`das`/`den`/`dem`/`des` and the `ein-`
  forms, pinned by `native_german_article_keywords` in
  `ranting_i18n/tests/german.rs`.

Both show the same property: the written form selects only the **paradigm**.
`{los *=0}` on a singular Spanish noun renders `el`, and `{der *@0}` on an
accusative German placeholder renders `den` — case, gender and number still pick
the form, exactly as they did for the English keyword. `ranting` gained no
vocabulary in either language; both word lists live in the forks'
`inflect_article_custom`.

### Verification

551 tests across all five crates pass, with `cargo fmt --check` and
`cargo clippy --all-targets -- -D warnings` clean in each. New coverage:
`tests/ranting/native_article_keyword.rs` (5 tests — native definite/indefinite
keywords, gender and number agreement, English byte-identity, and the unmarked
limitation), `ph_ext::tests::open_pass_only_for_input_english_rejects` (both
directions plus the one-repetition regression), and one native-keyword test in
each falsifier.

`heed!()`/`ask!()`/`#[derive(Heed)]` are unaffected: `ranting_derive/src/heed.rs`
never references `ph_ext`, so input matching does not share the changed parse
path.

## The `{el 0}` diagnostic, as far as it goes (2026-08-14, Phase 7 item 8)

The "still open" list above names the `E0425` message as a diagnostics defect
worth fixing independently. It was taken up, and it is **only partly fixable**.
The reason is worth recording, because the obvious fix is unavailable for two
independent reasons and neither is going to change.

### The message cannot be reworded

`E0425: cannot find value 'el' in this scope` is rustc's, emitted during name
resolution of an identifier the macro baked. A proc macro has no way to
intercept it or attach a note. The only way to replace it is to not bake the
identifier — i.e. to reject the template ourselves, with our own message.

That requires deciding, at expansion time, that `el` is *not* a variable. It is
not decidable. `` {el gato} `` and `` {person walk} `` are the same shape — noun
plus post-noun verb — and `` {person walk} `` is live syntax used in the test
suite today. The macro sees a word; whether a binding by that name exists is
rustc's knowledge, not the macro's, and arrives strictly later. Rejecting the
shape would break working English templates to improve the message on broken
Spanish ones.

Recognising a *list* of known non-English article words would decide it, and is
exactly the vocabulary-in-`ranting` that this whole spike exists to avoid.

### The span cannot be narrowed to the word

`StrLitSlice::error` already points at a substring of the template, which is why
`ph_ext`'s parse errors underline the offending characters. That machinery
depends on `proc_macro2::Literal::subspan`, which is nightly-only and returns
`None` on stable (rustc 1.97.1 confirmed) — hence the "At `<template>`" +
squiggle fallback those errors print instead. rustc's own error, though, gets no
fallback: it uses the `Span` the identifier carries, and on stable the finest
span available for a piece of a string literal is the whole literal.

### What did land

Two changes, both in `ranting_derive/src/lib.rs`:

1. **`path_from` takes the template literal's span instead of
   `Span::call_site()`.** The `E0425` caret moves from the whole
   `say!("Veo {el gato}.")` invocation to the `"Veo {el gato}."` literal, and the
   "this error originates in the macro `say`" note disappears. That is the entire
   stable-toolchain win, and on nightly it is also the hook a future
   `subspan`-based narrowing would use.

2. **`check_ident_path` guards `syn::Ident::new`, which was panicking.** This is
   the real defect found while investigating, and it is not the one the item was
   filed for. `ph_ext`'s word matcher admits `-` and `'`, so
   `` say!("X {gato-negro}.") `` parsed fine and then hit `Ident::new`'s panic —
   surfacing as a bare `error: proc macro panicked` / `help: message:
   "gato-negro" is not a valid identifier`, with the caret on the whole macro and
   no indication of which placeholder was at fault. It now returns an `Err`
   through the existing spanned-error path:

   ```
   error: `gato-negro` is not a valid Rust identifier, so it cannot name a
          variable. A placeholder's noun must be a variable in scope, a
          positional index (`{0}`), or a named argument (`say!("{x}", x = ..)`):
          At "X {gato-negro}."
                 ^^^^^^^^^^
   ```

   This is a case where a better message *is* available, precisely because it is
   decidable: no Rust variable can be named `gato-negro`, whatever is in scope.

One trap, caught by the existing suite rather than by reasoning: the guard must
mirror `Ident::new`'s rule, which is "a keyword **or** a legal variable name".
`syn::parse_str::<syn::Ident>` rejects keywords, and `` {self} ``/`` {=self do} ``
are live syntax throughout `tests/ranting/male_female_and_object.rs` — the first
version of the guard broke five call sites. `syn::Ident::parse_any` is the
correct predicate.

### Verification

553 tests across all five crates (2 new unit tests in `ranting_derive`, pinning
both directions of `check_ident_path`), `cargo fmt --check` and
`cargo clippy --all-targets -- -D warnings` clean in each. The two rendered
diagnostics above were verified by compiling a scratch crate against a path
dependency on `ranting`, not by assertion — this repo has no compile-fail
harness (no `trybuild`), and adding one is a new dev-dependency and a new testing
convention in a repo whose CLAUDE.md says "integration tests only", so it is left
as the maintainer's call.

### What stays open

`{el gato}`'s wording. It is not a bug with an unwritten fix; it is a consequence
of the noun slot being genuinely ambiguous. The honest mitigation is
documentation — `docs/EXTENSIBILITY.md` §2.3 now says a native article needs a
case marker and that the resulting error is rustc's — not a macro change.

# `ranting-ja` — a Japanese reference lexicon

This crate is [ROADMAP.md](../ROADMAP.md) **Phase 7 item 6**: the *fourth* acceptance test, after
German (Phase 6 item 10), Spanish (item 23) and Arabic (Phase 7 item 5).

It implements Japanese for a deliberately tiny closed vocabulary — four nouns across three
numeral classifiers, four verbs across the plain/polite split with sonkeigo substitution, and one
`ask!()` audience — **using only `ranting`'s public API**. Its `Cargo.toml` depends on `ranting`
and nothing else: no `ranting_core`, no `ranting_derive`, no `pub(crate)` item, and no fork of
`handle_placeholder_impl`.

Its purpose is falsification, not utility. Everywhere Japanese cannot reach something through the
public trait seam, that is a hole and it is written down here rather than worked around. Each hole
is pinned by a test in [`tests/holes.rs`](tests/holes.rs) that asserts what the crate *actually*
renders.

`cargo fmt --check`, `cargo clippy -- -D warnings` and `cargo test` are green here and in every
other directory of the repository.

## Why Japanese, after German, Spanish and Arabic

The Phase 7 item 3 spike scoped this crate and the item 4 build decision bought it for two things.
One is a defect; the other is a **confirmation**, which is unusual for a falsifier and needs
saying plainly.

**`NarrationContext::register` gets its first real consumer, and it passes.** `register` has been
inert since Phase 3 — the crate never interprets it, and the Phase 7 item 1 audit found that
neither German nor Spanish had ever read it. That is because in both of those, politeness *is* a
pronoun slot (T-V), so it rides the addressee's declared subject label and `register` has nothing
to do. Japanese keigo operates on the **verb**, with no pronoun anywhere:

```rust
say_with!(formal, "{0 are}", neko)   // 猫 です
say_with!(casual, "{0 are}", neko)   // 猫 だ
```

Nothing differs between those two lines except `register`. A confirmation is normally not worth a
crate — the item 4 bar rejects "another working example" — and what clears the bar is item 1's
framing: **publishing freezes the trait**, an unused hook is unvalidated in its *shape* rather
than merely unexercised, and an audit from inside the repo structurally cannot settle whether a
never-read parameter is the right one. Only a fork that had to read it can. Building nothing here
would not leave `register` unjudged; it would freeze it unjudged.

**A defect no other language reaches**: the numeral and its noun are joined by a hard-coded space
no hook can remove, so 「一匹の猫」 is unreachable and 「一匹の 猫」 is what renders. That is hole 1,
scheduled as Phase 7 item 12. Unlike Arabic's dual it has **no** workaround to encode, which is
precisely why the item 4 decision did not block this crate on the fix: the honest recording is a
hole test, so the crate could be built first.

## What works through the hooks alone

| Japanese | Reached via |
| --- | --- |
| Teineigo — です / だ, 食べます / 食べる — driven by the context alone, with no pronoun in the template and no entity state | `inflect_verb_custom_with_context` + `NarrationContext::register` |
| Sonkeigo *substitution* — 食べる → 召し上がる, 行く → いらっしゃる — for an honored referent | the same hook: it is a lookup keyed by verb, register and `&self`, all three already present |
| Numeral classifiers — 一匹の / 三人の / 二本の — including the sound changes (*ippiki* / *sanbiki*) that make them a table rather than a suffix | `inflect_numeral_custom`, reading the counter off the entity |
| `heed!()` / `ask!()` over spaced, command-style input (`取る 剣`) | the existing whitespace boundary, which is script-agnostic |

## Two of eight hook pairs are live, and that is the finding

A `ranting_ja` leaves `GrammaticalCase` unused (case is postpositional particles, which are
template text), `NounClass` at `UNSET` (see below), `inflect_pronoun_custom` unused (pro-drop, and
Japanese "pronouns" are ordinary nouns), `inflect_adjective_custom` unused (i-adjectives conjugate
for tense, negation and politeness, which the `!` slot's *degree* axis does not express — and they
are prenominal besides), `inflect_article_custom`/`elide_article_custom` unused (no articles),
`inflect_preposition_custom` unused (postpositions, not prepositions), `capitalize` unused
(caseless scripts) and `Ranting::inflect` an identity function (nouns do not inflect for number).

**That is a fine outcome, not a design smell.** Every hook defaults to English behavior and
generates no code when not overridden — an unclassed, unhooked impl is byte-identical to pre-v1.3
codegen. The cost of an unused hook to a fork is one line of documentation read and not acted on.
A surface sized for maximally-inflected languages degrading to near-nothing for a low-inflection
one is the *intended* shape, and this crate is the evidence that it degrades cleanly rather than
forcing a fork to fight it. The one genuine cost is discoverability — eight `_custom` pairs is a
lot to read to discover you need two — and that is `docs/EXTENSIBILITY.md`'s problem rather than
the API's.

Note especially that **Phase 7 item 11's `count` on `Ranting::inflect`, which Arabic needed, is
ignored here**. The two crates landed a day apart and disagree completely about which half of the
surface matters. That is the apparatus working.

## The `NounClass`-as-classifier question dissolves

The item 3 spike asked whether `inflect_numeral_custom`'s `class: NounClass` parameter can carry a
counter (匹 / 人 / 本), or whether reading a counter off a parameter documented as a gender label
is a misuse. **Neither, because the parameter is not needed**: the hook has `&self`, and which
counter a noun takes is a property of that noun. This crate reads `self.entry.counter` and leaves
`NounClass` at `UNSET`.

That is the same reasoning the crate already records for why `class` is a parameter *at all* — it
is redundant for a plain impl and exists for `Many`/`Maybe`/`Box`, where the call site reads the
class off the wrapper while `self` inside the hook is the inner value. A classifier would ride
that same path, which is also the one case where putting a counter in `NounClass` would be wrong:
a `Many` of mixed nouns has no single counter.

So item 2's open-ended `&'static str` design is **not falsified** by Japanese; it is simply not
exercised by it. The sharpest genericity test of `NounClass` remains unwritten — Bantu, not
Japanese, would be it.

## The holes

Five, numbered 1-5. Hole 1 has a sub-case (1b) with its own pinned test, since the escape hatch
that would work around it fails for a related reason; every other hole is one entry, one test.

### 1. The numeral cannot be bound to its noun

Japanese writes 「一匹の猫」 with no spaces. `handle_placeholder_impl` pushes a separator between
the rendered numeral and the noun and offers it to no hook, unlike the article separator, which
`elide_article_custom` receives explicitly and may drop. Returning the particle の from the hook
gets the particle in; the space still lands after it. There is nothing the hook can return that
removes it.

Every escape hatch is worse than the gap: `{?$n neko}` hides the numeral and leaves a **leading**
space (hole 1b); writing the numeral as template literal text makes `inflect_numeral_custom` dead
for that fork entirely; and squeezing spaces after `say!()` returns would corrupt any Latin text
in the same template. Scheduled as ROADMAP.md Phase 7 item 12. Pinned by
`hole_1_the_numeral_is_separated_from_its_noun_by_a_space` and `hole_1b_*`.

### 2. Unspaced prose cannot be parsed

Whitespace is the only word boundary in `heed!()`/`ask!()`, permanently (Phase 6 item 9). Natural
Japanese prose has none, so a template whose segments abut cannot match. The failure is **honest**
— `None`, never an invented split — which is the right behavior and the same don't-silently-guess
stance that makes two zero-gap captures a compile error. It still means this crate cannot parse a
natural sentence. The escape hatch is that an unspaced clause is one `\S+` token, so `{clause}`
hands the whole run back for the caller's own segmenter. Pinned by
`hole_2_unspaced_input_returns_none_rather_than_a_wrong_capture`.

### 3. `ask!()` degenerates to a function call on prose input

For `heed!()` the escape hatch above is fine: the caller wanted a string and got one. For `ask!()`
it is thinner — its pitch is "parse the input, then call `answer()` with the captures", and against
unspaced prose every template collapses to a single `{clause}`, so `answer()` receives the raw
utterance and does all the work.

A real narrowing, not a falsification. `ask!()` still routes to the right audience and still
returns `None` **without** calling `answer()` when a literal does not match, which is worth
something for command-style input — and games and CLIs write that with spaces anyway. The honest
summary is that `ask!()` is useful for Japanese *command* input and not for Japanese *prose*
input. Pinned by `hole_3_ask_degenerates_to_a_function_call_on_prose_input`.

### 4. Case particles are template text (a boundary, not a gap)

Japanese marks case with postpositional particles — が, を, に — separate morphemes after the noun,
so they live in the template's literal text and `GrammaticalCase` never comes into it. This is the
word-order boundary (`docs/EXTENSIBILITY.md` §2.12) reconfirmed from outside Indo-European by an
SOV language with postpositions. Recorded the way `ranting_i18n`'s hole 8 and `ranting_ar`'s hole 5
are.

It costs less than it looks like it should, which is worth recording because the first version of
this entry got it wrong. A verb has to hang off *some* placeholder, so a verb-final clause looks
like it needs a noun repeated at the end — but the hidden marker `?` solves it exactly:

```rust
say_with!(formal, "{0}が{1}を{?0 see}", neko, hon)   // 猫が本を見ます
```

The noun renders nothing and the verb still conjugates, giving idiomatic SOV with correct
politeness and no duplication. Pinned by
`hole_4_case_particles_are_template_text_not_grammatical_case`.

### 5. `Register` has three values; keigo has more levels

`Register` is a closed enum — Formal / Neutral / Casual — and Japanese politeness has more
distinctions than that (teineigo, sonkeigo and kenjougo are three *axes*, not three points on one).
This lexicon maps two of the three values onto the same form.

Not a design failure, and the precise statement matters: the escape hatch is
`NarrationContext::dialect`, an **open** `Option<&'static str>` the crate never interprets, which
a fork wanting five levels would use instead. What is unavailable is expressing them through
`register` itself. Worth stating; not worth changing. Pinned by
`hole_5_register_has_three_values_and_keigo_has_more_levels`.

## Also observed, not holes

- **"Story-wide" is the wrong word for `NarrationContext`** in the crate's own documentation. The
  ROADMAP nearly concluded from it that per-addressee variation is out of scope for `register`,
  which would have made keigo the wrong fit. It is not: the context is **per-call**, so a
  different one per utterance is ordinary usage, and
  `register_can_vary_per_utterance_within_one_scene` shows two in one scene. Scheduled as
  ROADMAP.md Phase 7 item 13.
- **Only the `_with_context` twin is overridden** for the verb hook, which is the documented
  sufficient shape — the non-context hook defaults to delegating to it. This crate is the first
  fork to rely on that, since the other three override the plain hooks instead.
- **An unmodelled verb declines** to `ranting`'s own conjugation, visibly, rather than being
  silently mis-inflected — the same contract the other three forks use.

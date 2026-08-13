# `ranting-i18n` — a German reference lexicon

This crate is [ROADMAP.md](../ROADMAP.md) **Phase 6 item 10**: the acceptance test for items 1–9.

It implements German for a deliberately tiny closed vocabulary — three nouns of differing gender
(`der Hund`, `die Katze`, `das Haus`), four verbs, three adjectives, the numerals `0..=12`, all
four cases, both articles, adjective agreement and verb agreement — **using only `ranting`'s
public API**. Its `Cargo.toml` depends on `ranting` and nothing else: no `ranting_core`, no
`ranting_derive`, no `pub(crate)` item, and no fork of `handle_placeholder_impl`.

Its purpose is falsification, not utility. Everywhere German cannot reach something through the
public trait seam, that is a hole items 1–9 failed to close, and it is written down here rather
than worked around. Each hole is pinned by a test in [`tests/holes.rs`](tests/holes.rs) that
asserts what the crate *actually* renders — so if a later change closes one, the test fails and
the entry gets struck rather than quietly rotting.

`cargo fmt --check`, `cargo clippy -- -D warnings` and `cargo test` are green here and in the
repository root.

## What works through the hooks alone

| German | Reached via |
| --- | --- |
| `der`/`die`/`das`/`den`/`dem`/`des`, `ein`/`eine`/`einen`/`einem`/`einer`/`eines` | `inflect_article_custom` + `NounClass` + `GrammaticalCase` |
| Present tense over all six persons, incl. `sein` and stem-changing `sehen`/`schlafen` | `inflect_verb_custom` + `subjective()` as an uninterpreted channel |
| Weak / mixed / strong attributive adjective endings, the complete table | `inflect_adjective_custom` |
| `ein Hund` / `eine Katze` / `zwei Hunde` / `zwölf Häuser` | `inflect_numeral_custom` |
| Nouns capitalized wherever they stand | `capitalize` + `OrthographyRole::Noun` |
| `er`/`ihn`/`ihm`, `sie`/`ihr`, `es`, `sich`, `mich`/`dich`/`uns`/`euch` — and, from the very same entity, `Der Hund`/`den Hund`/etc. via the fused `*=`/`*@` marker | `inflect_pronoun_custom` (see hole 5, closed) |
| Dative-plural `-n` on the noun (`den Hunden`, `den Häusern`) | `inflect` + entity-carried case (see hole 2) |

Two of these are worth calling out because they answer questions Phase 6 asked directly.
`NounClass` (item 2) does what it promised: gender is read off the entity, never off the display
string, and one code path produces all three genders. `inflect_numeral_custom` (item 8) does too:
German numerals are spelled by this crate, `1` agrees like an article, and anything outside the
closed range falls back to English rather than being invented. This is genuine morphological
difference, not the lexicon-level substitution `docs/EXTENSIBILITY.md`'s pirate/Scottish examples
demonstrate — it is what item 10 asked to prove.

## Word order

**German still needs per-language templates for word order.** Stating that plainly is a
requirement of item 10, and it is the honest outcome.

Phase 6 item 1 already settled this as a permanent boundary
(`docs/superpowers/specs/2026-08-13-word-order-feasibility.md`, option (a)): `ranting` inflects
*within* a template and never reorders across placeholders — nor within one, since the
pre→number→noun→post assembly order is fixed too. This crate confirms it from the other side:

- German verb-second is reachable **only** because the caller writes a German template.
  `say!("Heute {the *=0 schlafen}.", katze)` renders `"Heute die Katze schläft."`; getting
  `"Heute schläft die Katze."` means writing a different literal string
  (`"Heute {?0 schlafen} {the *=0}."`), not setting a different hook.
- A verb split across two positions — `"Der Hund macht die Tür auf"` — is not expressible at all.
  A placeholder cannot carry both a pre-noun and a post-noun verb (`handle_placeholder_impl`
  asserts it), and no hook can emit text at a position it does not own. The separable prefix has
  to be literal template text.

Both are pinned by `hole_8_*` in `tests/holes.rs`.

## The holes

Numbered as referenced from `src/`, `tests/holes.rs` and the ROADMAP.

### 1. ✅ Closed — `say_with!()` and `#[derive_ranting]` are now reachable from `ranting` alone
*Belongs to: Phase 3 items 3 & 4 (the `_with_context` mechanism), Phase 6 item 8 (locale via
`NarrationContext::dialect`); closed by Phase 6 item 12.*

Previously, `ranting` re-exported `say`, `ack`, `nay`, `heed`, `Heed`, `ask`,
`boxed_ranting_trait` and `ref_ranting_trait` — but not `say_with` and not `derive_ranting`. A
crate depending only on `ranting`'s public API could therefore construct a `NarrationContext` but
never deliver one, so overriding a `_with_context` hook was dead weight: `say!()` always passes
`None`. Phase 6 item 12 added the two missing re-exports to `ranting/src/lib.rs`, closing the gap
as a pure addition — no signature or behavior changed, `say!()` is still byte-identical.

`hole_1_*` in `tests/holes.rs` is kept under its original name (findable from this cross-
reference and the ROADMAP) but now asserts the fix: `say!()` still always passes `None` to a
`_with_context` hook, and `say_with!()` — written here with only `use ranting::*;`, no
`ranting_derive` dependency in `Cargo.toml` — now delivers `NarrationContext::dialect` to
`inflect_article_custom_with_context`. Register-driven wording and runtime tense are reachable the
same way; `dialect`-selected digit systems (named in `inflect_numeral_custom_with_context`'s own
docs as the intended home for a locale) are unaffected by this item and remain future work.

### 2. `Ranting::inflect` takes number but not a *reachable* case
*Belongs to: Phase 6 item 4 (the owed hook-signature break), closed partway by item 14's own
`Ranting::inflect` signature break — same signature-break site.*

German declines the *noun*, not only its article: dative plural `den Hunden`, genitive singular
`des Hauses`. Item 14 gave `inflect` a fourth parameter, `case: GrammaticalCase`, so the signature
gap this hole originally named is gone — and item 19 (which closed hole 5) narrowed the reachability
gap too: the *bare* real-case markers (`=`, `@`, `` ` ``, `~`) still switch the noun slot to a
*pronoun* and call `inflect_pronoun_custom` instead, never `inflect()`, but the *fused* `*=`/`*@`
form now reaches `inflect()` with the real `GrammaticalCase` — `case_for` no longer has to treat
every call as nominative. That still isn't enough to reach dative: `GrammaticalCase` has no dative
variant at all (hole 3), so the best the fused form can do is accusative, which happens to share
its plural noun form with nominative in this lexicon. The form still has to come off the entity:
`GermanNoun::in_case` is that carrier, and remains the only way to reach dative or genitive on the
noun's own form. The visible consequence is otherwise unchanged from before item 19:
`say!("Ich gebe {the *@0} etwas.", hund_plural)` renders `"die Hunde"` where German wants
`"den Hunden"` — the article is even wrong here, because `@` is read as accusative (hole 3), and
that marker still cannot reach dative on the noun form, whichever case it named.

### 3. `GrammaticalCase` has no dative, so a fork ends up ignoring it
*Belongs to: Phase 3 item 2's v1.3 `GrammaticalCase` bullet, and Phase 6 item 2.*

`GrammaticalCase`'s variants are `Name`, `Subjective`, `Objective`, `PossessiveDeterminer`,
`PossessivePronoun`, `Reflexive` and `Hidden` — English's inventory. German has four cases and
`@` means accusative-or-dative; `dem`/`der` are unreachable from any marker. The enum's own docs
say so ("English doesn't distinguish accusative from dative; neither does this"), but the effect
on a real fork is sharper than a missing variant:

**once the entity carries the case — which it must, to reach dative at all — the `case`
parameter becomes ignorable.** `say!("{the *=0}", dativ)` and `say!("{the *@0}", dativ)` produce
identical output (`hole_3_*`). That is the precise sense in which `GrammaticalCase` did not, on
its own, close the German article gap: it made two of German's four cases expressible, and a
lexicon that needs all four routes around it entirely.

Rejected workaround: smuggling the case through `NarrationContext.dialect`. Reaching it now needs
only `say_with!()` (hole 1, closed), but `dialect` is still story-wide state standing in for
per-placeholder information — that mismatch, not reachability, is why this is rejected, and it is
exactly the kind of papering-over item 10 asks not to do.

### 4. Attributive adjectives: wrong position, and declension class is not reported
*Belongs to: Phase 6 item 5 (adjective agreement), with the position half owned by item 1.*

Two distinct failures, both pinned.

**4a — position.** German attributive adjectives are prenominal: `der kleine Hund`. The `!`/`!!`
slot is post-noun only (`PostSpec::Degree`), so the endings come out right and the word comes out
in the wrong place: `say!("{the *=0 !klein}", hund)` → `"Der Hund kleine"`. There is no template
that fixes it, because item 1 settled that `ranting` will not move text. And German's *predicative*
adjectives — the one position that is post-verbal — are uninflected, so **there is no German
sentence in which this hook's output is correct German**. The hook proves the agreement
*mechanism* works; German cannot use it in production without writing the adjective as literal
template text, where no hook can inflect it.

Rejected workaround: writing the adjective in the pre-noun slot and abusing `inflect_verb_custom`
to inflect it. It fails twice over — the pre-noun slot only accepts an article or a hard-coded
English modal word (hole 7), and a placeholder that has a pre-noun verb may not also have a
post-noun one.

**4b — declension class.** German endings depend on which article precedes: weak after `der`
(`der kleine Hund`), mixed after `ein` (`ein kleiner Hund`), strong after none
(`kleiner Hund`). `inflect_adjective_custom` receives `degree`, `case`, `class`, `as_plural` and
`uc` — never the rendered article — and `self` cannot know it either, because the article is
template text chosen per placeholder. So the choice is carried on the entity
(`GermanNoun::with_article`); without it the lexicon must guess, and guesses weak.

### 5. ✅ Closed — case marking and pronoun display no longer share one hook
*Belongs to: Phase 3 item 2 (the `inflect_pronoun_custom` contract) and Phase 6 item 2; closed by
Phase 6 item 19.*

A case marker used to do two jobs at once: it told `inflect_article_custom` the noun's role *and*
switched the noun slot from the name to a pronoun. To render `"Der Hund bellt."` a fork had to make
`inflect_pronoun_custom` return the noun's name — which `tests/ranting/grammatical_case.rs` in the
main crate still demonstrates, and describes as what "a case-declining fork typically" does. The
consequence was not typical at all: that override then applied to *every* case-marked placeholder
for that entity, so genuine German pronouns (`er`/`ihn`/`ihm`) became unreachable for the same
noun. `say!("Ich sehe {@0}.", hund)` used to render `"Ich sehe Hund."`.

Item 19 closed it by reusing `*` — already a case-marker-position character, previously
synonymous with no marker at all — fused with a real case marker: `{the *=noun}`/`{the *@noun}`
case-mark the placeholder exactly as `{the =noun}`/`{the @noun}` do (`inflect_article_custom`
still sees the same `GrammaticalCase`) but render the noun's *name* instead of switching to a
pronoun. No new marker character was added, and the two forms are reachable for the same entity
in the same sentence: `GermanNoun::inflect_pronoun_custom` now always returns a real pronoun
(the `Render::Name`/`Render::Pronoun` flag and `GermanNoun::as_pronoun` this section used to
describe are gone), and `say!("Ich sehe {@0}.", hund)` renders `"Ich sehe ihn."` while
`say!("Ich sehe {the *@0}.", hund)` renders `"Ich sehe den Hund."`. See `tests/holes.rs`'s
`hole_5_closed_*` and, in the main crate, `tests/ranting/case_display_split.rs`.

### 6. An article that renders as nothing still emits its separator
*Belongs to: Phase 6 item 7 (the post-assembly splice) and item 2.*

German has no indefinite plural article: `Hunde bellen`. The only way `inflect_article_custom` can
express "no article here" is to return `""` — and the separator is emitted regardless, so the
placeholder renders with a leading space (`" Hunde bellen."`, a doubled space mid-sentence).

`elide_article_custom` cannot repair it: the post-assembly splice is skipped when the recorded
article span is empty, so the hook is never called for a zero-length article. `hole_6_*` proves
this with a probe whose elision hook would drop the separator if it ever ran.

`Ranting::skip_article` does suppress the article, but it is per-entity and unconditional — it
cannot mean "no article in the plural only", and it would swallow `der`/`die`/`das` too.

### 7. The pre-noun slot is a closed English word list, so preposition fusion stays unreachable
*Belongs to: Phase 6 item 7 (which stated the limitation) and item 1.*

`elide_article_custom` was designed for `le` + `homme` → `l'homme`, and item 7 already recorded
that preposition-article fusion across a placeholder boundary (`de` + `le` → `du`) is not
expressible. German's fusions are *all* of that kind — `in dem` → `im`, `zu dem` → `zum`,
`an das` → `ans` — so the hook has no German use at all, and this crate does not override it.

This crate adds one detail to item 7's statement. The obvious escape — writing the preposition
inside the placeholder, in the pre-noun verb slot, so the hook can see it — does not exist:
`say!("{in the *=0}", haus)` is a **compile error** (`expected article or verb`). The pre-noun slot
accepts an article (`a`/`an`/`some`/`the`/`these`/`those`) or one of a hard-coded list of English
modal words (`can`, `may`, `shall`, `will`, `are`, `were`, `had`, `have`, …), and nothing else. So
the pre-noun slot is not a general escape hatch for a non-English fork, and no hook ever sees a
German preposition.

### 8. Word order

See [Word order](#word-order) above. Not a hole in the sense of an unclosed item — a boundary,
already decided by item 1 and restated here because item 10 requires it in writing.

## Also observed, not holes

- **A partial lexicon degrades honestly.** Unknown verbs, adjectives and out-of-range numerals all
  return `None` and get `ranting`'s English rendering rather than an invented German one.
- **But an unrecognized `subjective()` degrades silently.** `subjective()` is an uninterpreted
  channel (the ROADMAP's "`SubjectPronoun` is a closed English enum" decision), which is what lets
  this crate declare `"er"`/`"wir"`. When a hook then declines, English's fallback does not
  recognize the label and takes the catch-all arm of `english::inflect_verb`, emitting the bare
  form: `"Der Hund walk."`, not `"walks"`. That is the cost the decision already names; this crate
  confirms it is real and confined to declined words.
- **`capitalize` (item 6) has nothing to do for German.** German capitalizes nouns always, and this
  crate's `name`/`inflect` already return them capitalized — so the override only has to promise
  not to undo it. The hook's real customers are Turkish `i`/`İ` and the caseless scripts, as its
  own documentation says.

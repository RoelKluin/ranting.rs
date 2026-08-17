# `ranting-es` — a Spanish reference lexicon

This crate is [ROADMAP.md](../ROADMAP.md) **Phase 6 item 23**: the *second* acceptance test,
after `ranting_i18n`'s German (item 10).

It implements Spanish for a deliberately tiny closed vocabulary — four nouns of differing gender
(`el gato`, `la casa`, `el problema`, `el agua`), four verbs, three adjectives, the numerals
`0..=12`, both articles, post-nominal adjective agreement, verb agreement across all six persons
(including the `tú`/`usted` contrast) and subject/object/possessive/reflexive pronouns — **using
only `ranting`'s public API**. Its `Cargo.toml` depends on `ranting` and nothing else: no
`ranting_core`, no `ranting_derive`, no `pub(crate)` item, and no fork of
`handle_placeholder_impl`.

Its purpose is falsification, not utility. Everywhere Spanish cannot reach something through the
public trait seam, that is a hole and it is written down here rather than worked around. Each hole
is pinned by a test in [`tests/holes.rs`](tests/holes.rs) that asserts what the crate *actually*
renders — so if a later change closes one, the test fails and the entry gets struck rather than
quietly rotting.

`cargo fmt --check`, `cargo clippy -- -D warnings` and `cargo test` are green here and in the
repository root.

## Why Spanish, after German

Item 10 found that German **structurally cannot use the adjective hook at all** in a correct
sentence: `ranting_i18n`'s hole 4a records that German attributive adjectives are prenominal
(`der kleine Hund`), the `!`/`!!` degree slot
([`Ranting::inflect_adjective_custom`](https://docs.rs/ranting/latest/ranting/trait.Ranting.html#method.inflect_adjective_custom))
is post-noun only, and German's post-verbal predicative adjectives are uninflected — so there is
no German sentence in which the hook's output is correct German, only a demonstration that the
agreement *mechanism* works.

Spanish attributive adjectives are post-nominal — `el gato negro`, `la casa negra`,
`los gatos negros` — exactly where the `!` slot renders. `tests/spanish.rs`'s
`postnominal_adjective_agreement_in_a_real_sentence` is the payoff: unlike German, there is no
position mismatch to record. This is complete, correct Spanish, not merely a mechanism
demonstration.

Spanish also has no grammatical case system for nouns at all (only pronouns take distinct
subject/object/possessive/reflexive forms, which `PronounCase` already covers in full). That
means two of `ranting_i18n`'s eight holes — hole 2 (`inflect`'s case parameter is unreachable
for dative) and hole 3 (`GrammaticalCase` has no dative variant) — simply don't reproduce here:
there is nothing case-shaped for a Spanish noun entity to carry that the existing hook signatures
don't already offer. See "Holes that do not reproduce here" below.

## What works through the hooks alone

| Spanish | Reached via |
| --- | --- |
| `el`/`la`/`los`/`las`, `un`/`una`/`unos`/`unas` | `inflect_article_custom` + `NounClass` |
| `el agua` / `un agua` (euphonic singular article on a feminine noun starting with stressed `a`), reverting to `las aguas` / `unas aguas` in the plural | `inflect_article_custom`, reading an entity-carried flag (see below) |
| Post-nominal adjective agreement in gender and number: `el gato negro` / `la casa negra` / `los gatos negros`, plus a gender-invariant adjective's `-es` plural (`azul` → `azules`) | `inflect_adjective_custom` |
| Present tense over all six persons, including `tú` vs `usted` (§ below) and irregular `ser` | `inflect_verb_custom` + `subjective()` as an uninterpreted channel |
| `un gato` / `una casa` / `dos gatos` / `doce casas`, `1` agreeing like the indefinite article (including the euphonic `un agua`) | `inflect_numeral_custom` |
| `él`/`ella`/`ellos`/`ellas` (subject), `lo`/`la`/`los`/`las` (object), `su`/`suyo` (possessive), `se` (reflexive) | `inflect_pronoun_custom` |
| `¿El gato es negro?` — sentence-initial capitalization triggered by the *opening* `¿` | already closed in `ranting_core` (ROADMAP.md Phase 6 item 17); this crate just exercises it in real Spanish |
| `del gato`, `al gato` — preposition fused with the masculine article that follows it | `inflect_preposition_custom` (see hole 1, closed) |

Two things are worth calling out because they answer questions item 23 asked directly. `NounClass`
does what it promised even against Spanish's sharpest gender trap: `problema` ends in `-a`, like
`casa`, but is masculine (`el problema`, `el sistema`, `el idioma` — the Greek-derived `-ma`
class), and `docs/EXTENSIBILITY.md` §4.3's own pre-`NounClass` example is written to get exactly
this wrong by guessing gender from the noun's last letter. `SpanishNoun::problema` renders
`"El problema"` correctly because gender comes off the entity, never off the display string.
Second, `tú` vs `usted`: `usted` is formal "you" but grammatically borrows
**third-person-singular** verb agreement (`usted habla`, identical to `el gato habla`), the same
solution to formal address that German `Sie` uses (`ranting_i18n::person::GermanPerson::SIE`) but
a *different* slot borrowed — German borrows third-person-**plural**. Both are reachable from the
same mechanism (`subjective()` as an uninterpreted channel feeding a per-language `Person` enum);
which slot gets borrowed is lexicon data, not a `ranting` concern.

## `el agua`: a real euphony rule, not a gender change

`agua` ("water") is grammatically feminine — its adjectives agree feminine
(`tests/spanish.rs::adjective_agreement_on_agua_stays_feminine_despite_the_el_article` pins
`"El agua pequeña"`, not `"pequeño"`) and its object pronoun is `la`, not `lo`
(`object_pronouns_by_gender_and_number`). But its *singular* definite and indefinite articles are
`el`/`un` — the masculine-looking forms — purely to avoid the vowel clash Spanish speakers find
awkward in `la agua`/`una agua`. The plural reverts to the expected `las aguas`/`unas aguas`
because the clash only exists in the singular.

This is reachable cleanly: `NounEntry::euphonic_el` is a `bool` carried on the entity (like
`GermanNoun::definiteness` for a different reason — see `ranting_i18n`'s hole 4b), read directly
by `SpanishNoun::inflect_article_custom` and `inflect_numeral_custom` via `self.entry`, not routed
through any hook parameter. It isn't a hole: nothing about the hook signature blocks it, and no
general phonological detector (stressed-vowel-onset sniffing) was needed for this closed
four-noun vocabulary — a real Spanish fork with a large lexicon would need one, but that is
ordinary Rust string logic outside `ranting`'s hooks entirely, not a gap in the API surface.

## Holes that do not reproduce here

Unlike `ranting_i18n`, this section lists findings from item 10 that Spanish's grammar makes
irrelevant, so a reader comparing the two crates doesn't have to wonder whether they were simply
missed.

- **Hole 2/3 (dative case unreachable) — N/A.** Spanish nouns don't decline by grammatical case
  at all. `GrammaticalCase`'s missing dative variant never comes up, because there is no Spanish
  noun form that would need it; `SpanishNoun::inflect`/`inflect_article_custom` ignore the `case`
  parameter entirely rather than working around a gap.
- **Hole 4a (adjective position) — N/A, by construction.** See "Why Spanish, after German" above.
- **Hole 4b (adjective declension class not reported) — N/A.** German's weak/mixed/strong
  adjective endings depend on which article was rendered, which no hook reports. Spanish
  adjective endings depend only on the noun's own gender and number — both of which
  `inflect_adjective_custom` already receives directly (`class`, `as_plural`) — so there is
  nothing to carry on the entity here at all.
- **Hole 6 (zero-length indefinite plural article) — N/A.** German has no indefinite plural
  article; Spanish does (`unos`/`unas`), so the empty-string/stray-separator case
  `ranting_i18n`'s hole 6 records (closed by item 11) never arises for this lexicon.

## The holes

### 1. ✅ Closed — preposition-article fusion (`de`+`el`→`del`, `a`+`el`→`al`)
*Belongs to: Phase 6 item 7; closed by Phase 6 item 26.*

Spanish contracts exactly two preposition+article pairs: `de` + `el` → `del`, `a` + `el` → `al`
(never `de`/`a` + `la`/`los`/`las`, which don't contract). `elide_article_custom` runs *after* the
article inside a placeholder has been rendered — designed for French `le` + `homme` → `l'homme` —
but `de`/`a` here are template literal text written *before* the placeholder even starts, so that
hook could never receive them: the identical structural gap `ranting_i18n`'s README recorded as its
own hole 7, restated for Spanish's own contraction pair. This was the *only* hole this crate's
independent Spanish lexicon hit at all — every other gap German found either doesn't reproduce in
Spanish's grammar (see "Holes that do not reproduce here" below) or was never a hole here to begin
with.

What closed it: `docs/superpowers/specs/2026-08-13-preposition-fusion.md`'s option (b), a dedicated
hook fed the literal word immediately before a placeholder — `ranting_derive::parse_str_params`
now captures it instead of discarding it, bakes it into `PlaceholderSpec::preposition`, and
`Ranting::inflect_preposition_custom` receives it alongside the rendered article, at the same
post-assembly point `elide_article_custom` runs at. `SpanishNoun::inflect_preposition_custom`
(`src/noun.rs`) answers exactly Spanish's two pairs and declines otherwise — `del`/`al` for
masculine `el`, `de la`/`a la`/`de los`/`a los`/`de las`/`a las` left alone, since none of those
contract. `tests/holes.rs`'s `hole_1_*` now pins `"Vengo del gato."` and `"Voy al gato."` instead
of the previous unfused forms.

The escape hatch of writing the preposition *inside* the placeholder, where an existing hook could
see it, still does not exist — `say!("{de the *=0}", gato)` is still a compile error ("expected
article or verb"), the same pre-noun-slot restriction `ranting_i18n`'s hole 7 documents. Item 26
did not touch the pre-noun grammar; it added a separate channel that never needed the preposition
to be inside the placeholder at all.

## Also observed, not holes

- **A partial lexicon degrades honestly.** Unknown verbs, adjectives and out-of-range numerals
  all return `None` and get `ranting`'s English rendering rather than an invented Spanish one —
  `an_unknown_verb_falls_through_to_english_rather_than_being_guessed`,
  `an_unknown_adjective_falls_through_to_the_english_degree_table`, and
  `a_numeral_outside_the_closed_range_falls_through_to_english` pin it.
- **ROADMAP.md Phase 8 item 4's `##var` ordinal channel is the second constituency it was built
  for.** `lexicon::ordinal` spells `1..=12` fully agreeing in gender (`primero`/`primera`) and
  apocopating `primero`/`tercero` before a masculine singular noun (`primer gato`, `tercer gato`)
  — genuinely load-bearing use of `class`, unlike the cardinals above, where it matters only at
  `1`. The digit-ordinal channel (`$$var`, English "3rd") has no Spanish notation modeled by this
  closed lexicon and falls through to English the same honest way an out-of-range cardinal does.
- **A numeral placeholder with no preceding article spends its sentence-initial capital on the
  numeral, not the noun.** `inflect_numeral_custom` deliberately has no `uc` parameter — the main
  crate applies capitalization on the crate side, to whatever the hook returns, rather than
  delegating the decision to it. `say!("{#0 1}", 1, gato)` renders `"Un gato"`. This was a
  documented defect (`docs/architecture-review-2026-08-15.md` §1.11 in the main crate) until it
  was fixed there; `ranting_i18n`'s `spelled_numerals_agree_like_an_article_at_one` test was
  updated the same way, since it is a property of the shared engine, not a per-language gap.
- **Adjective apocope (`bueno`→`buen`, `grande`→`gran`) is not modeled, and structurally can't
  come up.** Apocope — a short adjective form used immediately before a masculine singular noun —
  is a *prenominal*-only phenomenon in Spanish. The only position `inflect_adjective_custom` can
  render an adjective in is post-nominal (the `!`/`!!` degree slot), which never triggers apocope
  in real Spanish either. So this is the flip side of "why Spanish has no adjective-position
  hole": the one position the hook can use is also the one position that never needs the
  irregularity this lexicon left out.
- **Orthographic plural changes (`feliz`→`felices`, not `felizes`) are sidestepped by lexicon
  choice, not solved.** The three adjectives here (`negro`, `pequeño`, `azul`) were picked so the
  regular `-s`/`-es` pluralization rule needs no spelling-change exception. A larger Spanish fork
  would need one (`z`→`c` before `-es`); nothing about `ranting`'s hooks blocks writing it, this
  closed vocabulary just doesn't exercise it.
- **Possessive determiner/pronoun agree with the *possessed* noun, not this entity.** Spanish
  `su`/`suyo` change form for the gender and number of the thing possessed, not the possessor —
  information no hook carries, because the possessed noun is a different placeholder entirely (or
  isn't rendered at all). `SpanishNoun`/`SpanishPerson` return the closest honest single answer
  (`su`, invariant `suyo`) and go no further, the same stance `ranting_i18n::GermanNoun::possessive`
  takes for undeclined `sein`/`ihr`. Not filed as a numbered hole because it isn't one: no `ranting`
  signal is missing here that a same-entity German possessive needed either, it's inherent to what
  a possessive pronoun means.
- **`capitalize` has nothing to do for Spanish.** Unlike German (which always capitalizes nouns,
  needing an override to *not* undo it) Spanish orthography is capitalize-at-sentence-start, which
  is exactly the English default `capitalize`/`uc_1st_if` already implements. `SpanishNoun` and
  `SpanishPerson` don't override it at all.
- **Pro-drop (Spanish's habit of omitting the subject pronoun) needs no hook support.** `"Hablo
  español"` vs. `"Yo hablo español"` is a template choice — write the sentence with or without the
  `{=0}` placeholder — not a signal `ranting` is missing. Word order and word presence are both
  the caller's template, per ROADMAP.md Phase 6 item 1's permanent boundary
  (`docs/EXTENSIBILITY.md` §2.12).

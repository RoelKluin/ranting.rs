# `ranting-fr` — a French reference lexicon

This is the fifth falsifier, after German (`ranting_i18n`), Spanish (`ranting_es`), Arabic
(`ranting_ar`) and Japanese (`ranting_ja`) — and the first one chosen for adoption reach rather
than purely to close a documented gap: French is one of the most widely used languages a `ranting`
fork might target, and this crate exists partly as a showcase of what a real fork looks like.

It still earns its place the same way the other four did, by exercising something none of them
does — see "Why French, after German, Spanish and Arabic" below — and it holds to the same
contract: its `Cargo.toml` depends on `ranting` and nothing else, no `ranting_core`, no
`ranting_derive`, no `pub(crate)` item, and no fork of `handle_placeholder_impl`. Everywhere French
cannot reach something through the public trait seam, that is a hole and it is written down here
rather than worked around, pinned by a test in [`tests/holes.rs`](tests/holes.rs).

It implements French for a closed vocabulary of seven nouns spanning both genders, mass and count,
and every elision case (`le chat`, `la maison`, `l'arbre`, `l'école`, `l'homme`, `le héros`,
`de l'eau`); three verbs (`être`, `avoir`, `parler`) across `je`/`tu`/`il`/`elle`/`nous`/`vous`/
`ils`/`elles`; eleven adjectives, eight of them prenominal; the numerals `0..=20` plus the
vigesimal irregulars; and ordinals `1..=5`.

`cargo fmt --check`, `cargo clippy -- -D warnings` and `cargo test` are green here and in the
repository root.

## Why French, after German, Spanish and Arabic

French's two most obvious candidate gaps turn out to already be claimed by an earlier falsifier,
so this crate had to find something else to be about.

`Ranting::elide_article_custom` is documented, repo-wide, as built for exactly French's
`le`+`homme`→`l'homme` — the trait's own doc comment illustrates the hook with that example. But
`ranting_ar` is already its first real user, for sun/moon-letter assimilation, and
`.claude/rules/crate-layout.md`'s own stated criterion for a new falsifier is that it "exercise
different, complementary gaps rather than duplicate each other" — the same language used to
justify `ranting_ar` skipping `inflect_adjective_custom` once Spanish had already proven it.
Likewise, `de`+`le`→`du`, `à`+`le`→`au` fusion is structurally identical to `ranting_es`'s
already-closed hole 1 (`de`+`el`→`del`, `a`+`el`→`al`), which is what
`inflect_preposition_custom` was generalized from in the first place. Both hooks are still
implemented here — see "What works through the hooks alone" — for a correct, complete showcase;
neither is filed as a new finding.

Three things about French genuinely are new:

1. **Adjective position is lexically split, not categorical.** German is prenominal-only, so
   `inflect_adjective_custom` is structurally unreachable for the whole language (`ranting_i18n`'s
   hole 4a); Spanish and Arabic are postnominal-only, so the hook always fires correctly. French
   has a small closed set of very common adjectives (`grand`, `petit`, `beau`, `bon`, `vieux`,
   `nouveau`, `jeune`, `joli`) that go *before* the noun, while most others go after. No existing
   fork's lexicon has a per-word, not per-language, reachability split against the hook's single
   post-noun slot. This is the crate's one real hole — see below.
2. **`is_mass()`/the partitive article (`du`/`de la`) have zero prior exercise.** No other falsifier
   overrides `is_mass()` at all. `get_article_or_so` already resolves a mass noun's indefinite
   slot to `some` before falling back to English, so this crate can answer `du`/`de la` directly.
   A confirmation finding, not a hole — the hook signature was already sufficient.
3. **`h aspiré` vs. `h muet`** (`l'homme` elides, `le héros` does not, despite both spelling a
   plain `h`) gives `elide_article_custom` its first negative case: an entity-carried flag
   correctly declining to elide even though the surface string looks elidable. Also a
   confirmation — `None` was always documented as "keep it exactly as rendered", this is simply
   the first fork that needed that path for a word that only *looks* elidable.

## What works through the hooks alone

| French | Reached via |
| --- | --- |
| `le`/`la`/`les`, `un`/`une`/`des` | `inflect_article_custom` + `NounClass` |
| `du`/`de la` on a mass noun (`de l'eau`, not `une eau`) | `is_mass()` + `inflect_article_custom` |
| `l'arbre`, `l'école`, `l'homme` (elides); `le héros` (doesn't, `h aspiré`) | `elide_article_custom`, reading an entity-carried flag |
| Post-nominal adjective agreement in gender and number: `le chat noir` / `la maison noire` / `les chats noirs`, plus a gender-invariant adjective (`rouge`) | `inflect_adjective_custom` |
| Present tense over `je`/`tu`/`il`/`elle`/`nous`/`vous`/`ils`/`elles`, including `tu` vs `vous` (§ below) and irregular `être`/`avoir` | `inflect_verb_custom` + `subjective()` as an uninterpreted channel |
| `un chat`/`une maison`/`cinq chats`, `1` agreeing like the indefinite article | `inflect_numeral_custom` |
| `soixante-dix`/`quatre-vingts`/`quatre-vingt-dix` — genuine vigesimal irregularity, not just different spellings of the same algorithm | `inflect_numeral_custom` |
| `il`/`elle`/`ils`/`elles` (subject), `le`/`la`/`les` (object), `son` (possessive), `se` (reflexive) | `inflect_pronoun_custom` |
| `premier`/`première`, `deuxième`… — agrees in gender only at `1`, unlike Spanish where every ordinal agrees | `inflect_numeral_custom` |
| `du chat`, `au chat` — preposition fused with the masculine article that follows it (not a new finding, see above) | `inflect_preposition_custom` |

## `tu` vs `vous`

French has one fewer *distinct word* here than Spanish's six-way person system. Spanish's
informal second-person plural `vosotros` has no French equivalent: `vous` alone covers both formal
"you" and plural "you", and — unlike Spanish `usted` (a distinct word from `ustedes`, borrowing
third-person-singular agreement) or German `Sie` (a distinct word from `sie`, borrowing
third-person-**plural** agreement) — French's formal `vous` isn't just borrowing a slot, it *is*
the plural word, spelled identically and conjugated identically
(`FrenchPerson::VOUS_FORMAL`/`FrenchPerson::VOUS` share the same `subject` string and the same row
of `lexicon::VERBS`). `tests/french.rs::tu_and_vous_formal_take_the_same_verb_agreement_as_vous_plural`
pins it. The underlying mechanism — `subjective()` as an uninterpreted channel feeding a
per-language `Person` enum — is the same one Spanish and German already proved; only the specific
slot-sharing shape is new.

## Holes that do not reproduce here

- **`inflect_adjective_custom` unreachable — N/A, not the way German has it.** Unlike German,
  this hook *is* reachable, and correctly agrees every word in the lexicon. What's unreachable is
  *word order* for a closed eight-word subset — see "The holes" below for why that's a different
  finding, not the absence of one.
- **Preposition-article fusion (`de`+`le`→`du`, `à`+`le`→`au`) — not filed as a hole.** Implemented
  (see "What works through the hooks alone"), but structurally identical to `ranting_es`'s
  already-closed hole 1, so it isn't claimed as a new finding here.
- **Dative case unreachable (`ranting_i18n`'s holes 2/3) — N/A.** French nouns don't decline by
  grammatical case at all, the same as Spanish and Arabic.
- **Zero-length indefinite plural article (`ranting_i18n`'s hole 6) — N/A.** French has an
  indefinite plural article (`des`), the same as Spanish.

## The holes

### 1. Prenominal adjectives agree correctly, in the wrong position

`inflect_adjective_custom` agrees every adjective in this lexicon's closed set correctly —
`lexicon::ADJECTIVES` carries real masculine/feminine/singular/plural forms for all eleven words,
prenominal and postnominal alike. But the `!`/`!!` degree slot this hook is called from only ever
renders post-noun. For the eight prenominal words (`grand`, `petit`, `beau`, `bon`, `vieux`,
`nouveau`, `jeune`, `joli`), that means the agreed form comes out in the wrong place:
`say!("{the *=0 !grand}", FrenchNoun::maison())` renders `"La maison grande"` — correct gender,
correct number, wrong word order; real French is `"La grande maison"`.

This is the same shape German's whole-language finding takes (`ranting_i18n`'s hole 4a: the
mechanism works, the position doesn't), narrowed to a closed, lexically-determined subset rather
than covering every adjective in the language. It's a genuinely different-shaped finding from
either "the whole language can't reach it" (German) or "every adjective it knows reaches it
correctly" (Spanish, Arabic): here, *which* word determines whether the hook's output is usable at
all, and the hook has no way to find out.

Pinned by `tests/holes.rs::hole_1_prenominal_adjectives_agree_correctly_in_the_wrong_position`. The
only way to get correct French word order is to write the adjective as literal template text,
where no hook can inflect it — the same escape hatch (and the same limitation)
`ranting_i18n`'s hole 4a documents for German.

## Also observed, not holes

- **A partial lexicon degrades honestly.** An unknown verb, adjective or out-of-range numeral all
  return `None` and get `ranting`'s English rendering rather than an invented French one —
  `an_unknown_verb_falls_through_to_english_rather_than_being_guessed`,
  `an_unknown_adjective_falls_through_to_the_english_degree_table`, and
  `a_numeral_outside_the_closed_set_falls_through_to_english` pin it.
- **`is_mass()`/partitive and `h aspiré` are confirmations, not holes.** See "Why French, after
  German, Spanish and Arabic" above — both prove an existing hook signature was already
  sufficient, the same shape `ranting_ja`'s `register` finding takes.
- **Ordinal agreement is a different shape from Spanish's, not a smaller version of it.** Spanish
  ordinals fully agree in gender at every value 1..=12 (`primero`/`primera`). French ordinals in
  this closed `1..=5` range agree only at `1` (`premier`/`première`) — `deuxième` onward is
  already `-ième`-invariant across both genders. `lexicon::ordinal`'s doc comment has the detail.
- **Vigesimal numerals are a genuinely different algorithm, not just different vocabulary.**
  `soixante-dix` ("sixty-ten"), `quatre-vingts` ("four-twenties") and `quatre-vingt-dix`
  ("four-twenty-ten") don't have a Spanish or Arabic analogue in this repo — those languages'
  numeral tables are all base-ten. This closed lexicon models `0..=20` plus these three irregular
  decades (`70`/`80`/`90`, plus `71`/`81`/`91`) rather than a full `0..=99` table.
- **`capitalize` has nothing to do for French.** Same as Spanish: French orthography is
  capitalize-at-sentence-start, exactly what the English default already implements. `FrenchNoun`
  and `FrenchPerson` don't override it at all.
- **Possessive determiner/pronoun agree with the *possessed* noun, not this entity.** French
  `son`/`sa`/`ses` change form for the gender and number of the thing possessed, not the
  possessor — the same shape `SpanishNoun::su`/`ranting_i18n::GermanNoun::possessive` document.
  `FrenchNoun`/`FrenchPerson` return the closest honest single answer (`son`) and go no further.
  Not filed as a numbered hole for the same reason it isn't for Spanish or German: no `ranting`
  signal is missing that either of those needed either — it's inherent to what a possessive
  pronoun means. (French additionally prefers `son` over `sa` before a vowel-initial feminine
  noun purely for euphony — a second layer this closed lexicon doesn't attempt, for the same
  reason `SpanishNoun`'s adjective lexicon doesn't model apocope.)

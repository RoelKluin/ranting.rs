# `ranting-zh` — a Mandarin Chinese reference lexicon

This is the sixth falsifier, after German (`ranting_i18n`), Spanish (`ranting_es`), Arabic
(`ranting_ar`), Japanese (`ranting_ja`) and French (`ranting_fr`). Its `Cargo.toml` depends on
`ranting` and nothing else, no `ranting_core`, no `ranting_derive`, no `pub(crate)` item, and no
fork of `handle_placeholder_impl`. Everywhere Mandarin cannot reach something through the public
trait seam, that is a hole and it is written down here rather than worked around, pinned by a
test in [`tests/holes.rs`](tests/holes.rs).

It implements Mandarin for a closed vocabulary of three nouns with their classifiers (猫/只,
书/本, 人/个), three invariant verbs (吃, 是, 有), and the seven personal pronouns
(我/你/您/他/我们/你们/他们).

`cargo fmt --check`, `cargo clippy -- -D warnings` and `cargo test` are green here and in the
repository root.

## Why Mandarin, after German, Spanish, Arabic and Japanese

Two obvious candidate angles were considered and rejected as non-novel before this crate was
scoped.

**Classifiers/measure words** (一只猫, 一本书) are implemented here — see "What works through the
hooks alone" — but not claimed as a finding. If built the way `ranting_ja`'s counters are, they
are structurally the same mechanism: a lexical property read off `&self` inside
`inflect_numeral_custom`, spliced against the noun via `elide_numeral_custom`. Same
`NounClass`-not-needed conclusion, same "already closed" shape, no new signal extracted from the
API that Japanese's counters didn't already extract.

**Prenominal adjective position** (Mandarin's attributive words are prenominal stative verbs, not
a distinct word class) is not implemented at all. It would be a *third* restatement of
`ranting_i18n`'s hole 4a — German found the whole language unreachable through
`inflect_adjective_custom`; `ranting_fr` found a lexically-split version of the same thing; a
Mandarin fork saying it a third time adds nothing `crate-layout.md`'s own bar ("a second working
example adding no falsification") wouldn't already reject.

**What's genuinely new: the tense-marker pipeline is architecturally committed to composing an
English auxiliary+verb pair, and no hook can turn that off.** Mandarin verbs never conjugate for
person or tense — 吃 ("eat") is the same word for every subject and every time reference — and
instead of English-style tense (marked with auxiliary words: "will run", "has run", "was
running"), Mandarin marks **aspect** with particles suffixed directly onto the verb: 了
(perfective/completed), 过 (experiential), 着 (durative/continuous). This is a different
grammatical category from tense, and it turns out `ranting`'s tense-marker system has no seam
for it at all:

- `Ranting::inflect_verb_custom_with_context` — the only hook consulted anywhere in the
  tense-marker pipeline — never receives which of the eleven `TenseMarker` variants fired. Its
  signature carries `subject`, an already-partially-conjugated `verb` string, `as_plural`,
  `count`, `uc`, `ctx`; nothing that says "future" or "present perfect."
- Its return value is then unconditionally piped through `handle_tense_marker`, which composes
  `format!("{auxiliary} {verb}")` in plain English string formatting — **not a trait method
  call**. There is no hook there to override, suppress, or replace.

`MandarinNoun::inflect_verb_custom_with_context` substitutes the correct, invariant Mandarin verb
perfectly — proof the hook itself works exactly as designed. But
`say!("{?0 >eat}", MandarinNoun::mao())` still renders `"Will 吃"`, never a bare 了/过/着-suffixed
verb with no auxiliary, no matter what the hook returns. This is the same *shape* of gap
`ranting_i18n`'s hole 4a and `ranting_ar`'s hole 4/5 record — a hook whose output is well-formed
in isolation, but the surrounding machinery is architecturally committed to an English sentence
structure no override can escape — on an axis (tense vs. aspect) no existing fork touches.
`ranting_ja` never found this because its own tests never exercise a tense marker at all: its
README states only three of eight hook pairs are live, and none of the three is this one.

## What works through the hooks alone

| Mandarin | Reached via |
| --- | --- |
| 吃/是/有 substituted for any English spelling of the same verb (`eat`/`eats`/`ate`/`eaten`/`eating`, ...), invariant across every person and number | `inflect_verb_custom_with_context` |
| 三只猫, 零个人 — numeral + classifier fused directly against the noun, classifier read off the noun itself (not a new finding, see above) | `inflect_numeral_custom` + `elide_numeral_custom` |
| 我/你/您/他/我们/你们/他们 — subject pronouns across all persons, including 您 (formal, singular-only) and 他 covering the spoken-language gender-neutral third person | `inflect_pronoun_custom` on `MandarinPerson` |
| Identical word in subject and object position (我 is both "I" and "me") — no case distinction at all | `inflect_pronoun_custom` |
| 我的 — possessive via the regular particle 的, not a separate word | `inflect_pronoun_custom` |
| 自己 — invariant reflexive across every person | `inflect_pronoun_custom` |

## Holes that do not reproduce here

- **Dative case unreachable (`ranting_i18n`'s holes 2/3) — N/A.** Mandarin nouns and pronouns
  don't decline by grammatical case at all — pronouns don't even distinguish subject from object,
  a step further than Spanish/French/Arabic's "no case, but pronouns still have distinct forms."
- **`inflect_adjective_custom` unreachable — implemented as declined outright, not as a hole.**
  See "Why Mandarin, after..." above: this is a third restatement of an already-recorded finding,
  not a new one, so this crate doesn't implement the hook at all rather than filing a numbered
  hole for it.
- **Zero-length indefinite plural article (`ranting_i18n`'s hole 6) — N/A.** Mandarin has no
  articles of any kind, so there's no indefinite-plural slot to be empty or non-empty.

## The holes

### 1. The tense-marker pipeline always composes an English auxiliary

See "Why Mandarin, after German, Spanish, Arabic and Japanese" above for the full mechanism.
`say!("{?0 >eat}", MandarinNoun::mao())` renders `"Will 吃"`; `say!("{?0 %eat}", ...)` renders
`"Have 吃"` — every tense marker that implies an auxiliary hits the same wall, not just future.
(`%`'s auxiliary selection also falls to its "any other subject" default here, since
`subjective()` returns a Chinese pronoun that can never match `conjugate_auxiliary`'s closed
English pronoun set — a further, smaller symptom of the same root cause: the auxiliary-selection
machinery isn't just uncontrollable, its own person/number selection silently stops working the
moment the subject isn't English text.)

The only way to get correct Mandarin aspect marking is to avoid tense markers entirely: a bare,
unmarked verb placeholder never reaches `handle_tense_marker` at all
(`say!("{?0 eat}.", mao)` → `"吃."`), and the particle can then be written as literal trailing
text on that same slot (`say!("{?0 eat 了}.", mao)` → `"吃 了."`) — the same "word choice is the
caller's template" boundary `docs/EXTENSIBILITY.md` §2.12 already names for other languages'
clause-level particles, restated here for a third one.

Pinned by `tests/holes.rs::hole_1_a_tense_marker_always_composes_an_english_auxiliary` and
`tests/holes.rs::hole_1_the_only_way_to_write_real_aspect_is_to_avoid_tense_markers_entirely`.

## Also observed, not holes

- **A partial lexicon degrades honestly.** An unknown verb or an out-of-range numeral both fall
  through to English rather than being guessed —
  `an_unknown_verb_falls_through_to_english_rather_than_being_guessed` and
  `a_numeral_outside_the_closed_set_falls_through_to_english_while_the_noun_stays_chinese` pin it;
  the latter is a genuinely partial fallback, with the numeral in English and the noun still in
  Chinese, since the two are handled by separate hooks.
- **A bare pronoun placeholder on a plain noun falls through to English, not to `subjective()`'s
  own string.** `subjective()` is an uninterpreted channel — nothing renders it directly, only
  hooks that read it back do. Without an `inflect_pronoun_custom` override, `say!("{=0}",
  MandarinNoun::mao())` renders `ranting`'s English default ("It"), not the Chinese string
  `subjective()` returns. `MandarinPerson` overrides the hook and gets real Mandarin; a bare
  noun, matching `ranting_ja::JapaneseNoun` (also unoverridden), does not.
- **们-pluralization is a person-only, lexically-conditioned exception.** Standard Mandarin can
  suffix 们 onto pronouns and nouns referring to specific people (我们, 你们, 他们, 老师们
  "teachers") but not onto ordinary count nouns (猫们 for "cats" is not standard). This crate
  models the mechanism entirely through `MandarinPerson`'s pre-built pronoun forms rather than
  through any hook logic — the ordinary `is_plural`/`+` marker mechanism, nothing new — while
  `MandarinNoun`'s `inflect()` stays a true identity function regardless of number, matching
  `ranting_ja::JapaneseNoun::inflect` exactly.
- **您 has no plural.** Unlike French `vous`, which covers both formal-singular and plain-plural
  with the identical word (`ranting_fr`'s `tu_and_vous_formal_take_the_same_verb_agreement_as_vous_plural`),
  standard Mandarin's formal 您 is singular-only; 您们 is nonstandard and not modeled here.
- **他/她/它 are homophones.** Spoken Mandarin does not distinguish grammatical gender in the
  third person at all — 他 ("he"), 她 ("she") and 它 ("it") are pronounced identically and
  spelled differently only in writing, a fact about the script rather than the grammar. This
  lexicon picks one spelling (他) rather than modeling a distinction the spoken language doesn't
  make.
- **`capitalize` has nothing to do, and neither does `noun_class`.** Chinese script is caseless
  (already pre-documented in `docs/EXTENSIBILITY.md` §2.6 for Chinese specifically, before this
  crate existed) and Mandarin has no grammatical gender or noun class at all — matching
  `ranting_ja`, not the four gendered forks.

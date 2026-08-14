ranting-gaps
============

Point it at English text; it reports what `ranting` cannot inflect, ranked by how often the text
actually needs it.

```bash
cargo run --manifest-path ranting_gaps/Cargo.toml -- README.md docs/ *.md --out failures
```

Paths may be files or directories (recursed); anything that isn't UTF-8 is skipped. The result is
a `failures/` tree — see [the report layout](#the-report) below.

The point is not to list every quirk of English. It is to answer four questions per failure, with
evidence rather than assertion:

| Question | Where it is answered |
|---|---|
| What is the cause? | `## Cause` — in terms of `ranting`'s own code: which function, which fallback |
| Why does it fail? | `## Why it fails` — with the string `ranting` actually renders, obtained by running the inflection |
| How common is it? | The frequency-ranked case table, plus quoted source sentences |
| What does `ranting` need? | `## What ranting needs` — the concrete change, or, for a boundary, why there isn't one |

## What it found

Run against this repo's own 111k words of prose, on the first run:

| Cause | Kind | Occurrences |
|---|---|---|
| Prenominal adjectives have no placeholder slot | boundary | 1223 |
| No regular English pluralization rules | gap | 188 |
| Nouns colliding with the closed pre-noun vocabulary | gap | 70 |

The headline was the second one. `Ranting::inflect()`'s regular path appended the `plural_end`
attribute, which defaults to `"s"` — there was no `y`→`ies`, no `-es` after a sibilant, no
`f`→`ves`. Everything English does beyond append-`s` had to be a row in
`data/irregular_plurals.txt`, which is 63 lines long, so `{+entity}` rendered `"entitys"` and
`{+match}` rendered `"matchs"`. `src/english.rs` was the executable specification of the missing
rules, with the counterexamples (`day`/`days`, `roof`/`roofs`, `chief`/`chiefs`) pinned in its
tests.

**That gap is now closed** (ROADMAP.md Phase 7 item 10), along with compound heads
(`mothers-in-law`), and rerunning is how it was accepted: both causes drop out of the report
entirely, leaving the word-order boundary and the pre-word homographs. So a probe reporting
nothing now means *the rules are present and agree* — see [Probes](#probes).

## Two things it deliberately does *not* do

**It does not argue with decisions already taken.** `docs/EXTENSIBILITY.md` §2.12 established word
order as a permanent boundary. Prenominal adjectives are therefore reported as `kind: boundary` —
counted, never listed as work. The count is still the most interesting number in the report: it
says how much of ordinary English prose cannot be expressed as a single placeholder, which is
worth knowing before adopting the crate rather than after.

**It does not guess at part of speech.** There is no tagger. A word counts as a noun when a
determiner or numeral preceded it, and the plural findings additionally require the corpus to
*attest* the corrected form. That second filter is doing the tagger's job: the determiner cue
alone nominates `is`, `as` and `only` (markdown puts a determiner in front of all three), and the
rules then produce `ises`, `ases`, `onlies` — corrections no English text contains, so requiring
attestation drops them without anyone writing a stoplist and guessing. Pass `--unattested` to see
what that filter removes.

Each case carries a confidence label:

- **certain** — the tool ran `ranting`'s inflection and compared against a rule that is a function
  of spelling alone. No corpus judgement involved.
- **attested** — as *certain*, and the corpus independently writes the corrected form.
- **heuristic** — rests on the determiner cue being right about this word. Read the quoted
  sentences before acting.

## The report

```text
failures/
  README.md              index, causes ranked by corpus occurrences
  summary.json           the same data, for anything that wants to consume it
  <cause-id>/README.md   one directory per cause
```

One directory per **cause**, not per word. A single missing rule breaks thousands of words; a file
per broken word would bury the one fact worth acting on. The frequency-ranked table inside each
cause is the answer to "how common is this".

Rerunning replaces the tree — the output is generated, not edited.

## Probes

| Probe | Kind | Detects |
|---|---|---|
| `regular-plural-rules` | gap | Nouns whose plural `ranting` gets wrong — *closed; now a regression guard* |
| `compound-head-plural` | gap | `mother-in-laws` where English writes `mothers-in-law` — *closed; now a regression guard* |
| `pre-word-homograph` | gap | Nouns that are also articles or modals, so `{The can can}` misparses |
| `word-order-prenominal-adjective` | boundary | `the small dog` — measured, not actionable |

The first two probes are kept now that their findings are fixed, rather than deleted. Each
compares `ranting`'s real output against `src/english.rs`'s **independently written** copy of the
same rules, so an empty finding is a passing differential check. That copy must stay independent:
calling `ranting::inflect_noun_regular` from it would make the comparison tautological and the
probes would report nothing forever, whatever `ranting` did. Both files carry a note saying so.

Two probes were designed and dropped before implementation: **invariant plurals** (`sheep`,
`fish`) and **unlisted irregular verbs** (`slay`/`slew` absent from `data/irregular_verbs.txt`).
Both need to infer a word's correct inflection from attestation patterns rather than from
spelling, and both produce findings a human would have to hand-filter. A gap finder whose output
needs triage is not clearly better than none, so they wait for a lexicon or a reason.

## Not a falsifier

`ranting_i18n` and `ranting_es` depend on `ranting` alone by contract — their whole purpose is to
prove the *public* API is enough for a non-English fork, so a `ranting_core` dependency in either
would be the finding. This crate has the opposite job: it inspects `ranting` from outside and uses
`ranting_core::ph_ext::parse` as an oracle for the closed pre-noun vocabulary, because restating
that word list here would be exactly the hand-kept duplication `CLAUDE.md` already warns about for
`PH_START`/`SENTENCE_TRIGGER_CHARS`. Its `ranting_core` dependency is deliberate and is **not** a
precedent for the falsifiers.

Like every sibling crate here, it has its own `Cargo.lock` and its own `cargo fmt`/`clippy`/`test`
runs; `scripts/overnight_loop.sh`'s `gate_dirs` picks it up automatically.

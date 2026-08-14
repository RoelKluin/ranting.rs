# ranting-es-gaps

Point it at real Spanish text; it checks `ranting_es`'s closed lexicon against it and reports
disagreements with an independent Spanish grammar reference.

```bash
cargo run --manifest-path ranting_es_gaps/Cargo.toml -- some-spanish-text.txt --out failures
```

Paths may be files or directories (recursed); anything that isn't UTF-8 is skipped. The result is
a `failures/` tree — see [`ranting_gaps`](../ranting_gaps/README.md) for the general layout this
crate reuses.

## What this checks, and what it doesn't

`ranting_gaps` reads *arbitrary* English text and finds brand-new words `ranting`'s general
regular-pluralization rule gets wrong, because that rule is genuinely general. `ranting_es` has no
equivalent: its lexicon is a **closed set** — 4 nouns (`gato`, `casa`, `problema`, `agua`), 4 verbs
(`hablar`, `comer`, `vivir`, `ser`), 3 adjectives (`negro`, `pequeño`, `azul`), numerals `0..=12`
— every gender, plural, and conjugation hand-listed rather than suffix-generated. `problema` ends
in `-a` but is coded masculine specifically to prove there is no `-o`/`-a` gender-guessing
heuristic anywhere to test. So there is no general noun-gender, noun-pluralization, or
verb-conjugation rule in `ranting_es` for a tool like this one to check against new words.

This tool does two narrower things instead:

1. **Re-verifies agreement on the existing closed lexicon** against real, corpus-attested Spanish
   usage — article selection (`el`/`la`/`los`/`las`/`un`/`una`/`unos`/`unas`), post-nominal
   adjective agreement, present-tense verb agreement across all six persons, and the two
   preposition+article fusions (`de`+`el`→`del`, `a`+`el`→`al`).
2. **Probes the handful of general sub-rules `ranting_es` genuinely has** — the `-o`/`-a`
   adjective gender swap, the vowel-vs-consonant adjective plural suffix, and article/preposition
   selection — against an independent oracle in [`src/spanish.rs`](src/spanish.rs), the same
   differential-oracle arrangement `ranting_gaps/src/english.rs` uses relative to `ranting`.

**It cannot discover new-word inflection bugs.** It is not a Spanish coverage tool or a vocabulary
extender. Any Spanish word outside the closed 4-noun/4-verb/3-adjective/13-numeral lexicon falls
through to `ranting`'s English rendering — that is correct, pinned behavior
(`ranting_es/tests/spanish.rs`'s `an_unknown_verb_falls_through_to_english_rather_than_being_guessed`
and its adjective/numeral siblings), not a finding this tool reports. The `lexicon-coverage` cause
in the report measures how much of a given corpus's noun phrases fall outside the lexicon — that
is evidence about scope, not a bug list.

## The enumerate-then-attest model

`ranting_gaps` **nominates** candidate words from an open English vocabulary using a determiner
cue, and needs corpus attestation to *filter* the cue's false positives (a determiner-preceded
`is`/`as`/`only` would otherwise be nominated as a noun). This tool never nominates anything: every
comparison it makes is enumerated directly from `ranting_es`'s closed lexicon up front. The corpus
therefore only *grades* each enumerated comparison's confidence (`certain` vs. `attested`) — it
never decides whether a case exists, and nothing is ever hidden by a missing `--unattested` flag,
because there isn't one. See [`src/corpus.rs`](src/corpus.rs)'s module doc for the full rationale.

## Findings this tool must never report

Several `ranting_es` behaviors look surprising out of context but are documented, intentional
design decisions — see `ranting_es/README.md`'s "Also observed, not holes" section, restated here
as `probes::NOT_HOLES` in code:

- A partial lexicon degrading honestly to English for unknown words — correct, not a gap.
- A bare numeral placeholder's capital landing on the following noun, not the numeral — an
  engine-level property of `uc` allocation, not Spanish-specific.
- Adjective apocope (`bueno`→`buen`) being unmodeled — it's prenominal-only and structurally
  unreachable from the postnominal `!` slot this lexicon can render at all.
- Orthographic plural spelling changes (`feliz`→`felices`) being sidestepped by lexicon choice,
  not solved by a rule.
- `su`/`suyo` not truly agreeing with the possessed noun — no hook signal carries that information,
  for either language.
- `capitalize` not being overridden — Spanish orthography already matches the English default.
- Pro-drop — whether a subject pronoun is written at all is a template choice, and low
  pronoun+verb bigram attestation in real Spanish text is *expected*, which is why
  `verb_person`'s attestation keys off the bare conjugated form instead (see that probe's doc
  comment).

## Why there's no `word_order`-style boundary probe

`ranting_gaps` has a `word_order` probe measuring how often English's prenominal-adjective
boundary bites in real text. Spanish attributive adjectives are post-nominal — exactly where the
`!` degree slot renders (`ranting_es/README.md`'s "Why Spanish, after German") — so that boundary
never reproduces here. A probe built to always find zero cases would be dead code (`probes::run_all`
already drops empty-case findings), so this crate just states the non-applicability here instead
of shipping an inert probe.

## Not a falsifier

`ranting_i18n`, `ranting_es`, `ranting_ar` and `ranting_ja` depend on `ranting` alone by contract
— their whole purpose is to prove the *public* API is enough for a non-English fork, so a
`ranting_core` (or in `ranting_es`'s case, *any other*) dependency in one of them would be the
finding. This crate has the opposite job, exactly like `ranting_gaps` does for the root crate: it
inspects `ranting_es` from outside, so depending on `ranting_es` — and on `ranting` itself, for
the trait/type surface the probes call directly — is deliberate and is not a precedent for the
falsifiers. `ranting_es/Cargo.toml` still depends on `ranting` and nothing else; that fact is
unaffected by this crate existing.

One consequence worth knowing: `scripts/hook_audit.sh`'s fork-detection grep (`^ranting = `) *does*
match this crate — it depends on `ranting` directly (for the trait/type surface the probes call),
not only on `ranting_es`, so its `Cargo.toml` carries the same `ranting = { path = ".." }` line
`ranting_gaps` has. It therefore appears in that script's `FORKS` table, exactly like `ranting_gaps`
does, and contributes zero to every column — the probes *call* `Ranting` trait methods
(`entity.inflect_article_custom(...)`), they never *define* one, so the script's `fn $m\s*\(`
grep has nothing to match. A harmless all-zero row, not a misclassification: the script only
measures hook overrides, and this crate has none either way.

Like every sibling crate here, it has its own `Cargo.lock` and its own `cargo fmt`/`clippy`/`test`
runs; `scripts/overnight_loop.sh`'s `gate_dirs` picks it up automatically.

## Corpus

No corpus ships with this crate. A hand-curated Spanish sample written specifically to contain the
closed lexicon's words in agreeing forms would be circular — you'd write `la casa negra` *because*
you already know that's the expected output, destroying the independence a differential oracle
needs to mean anything. Point the tool at real Spanish text you already have: a
[Tatoeba](https://tatoeba.org/en/downloads) `spa.txt` sentence dump, a Spanish Wikipedia extract,
or your own prose. The small Spanish fixtures inside `#[cfg(test)]` blocks in this crate test
corpus-ingestion plumbing (accents, inverted punctuation, bigram counting) — not linguistic claims
— so they aren't circular in the same sense.

# Brainstorm: testing ranting's inflection patterns against real documents, across dominant languages

- Generated: 2026-08-16
- Repo context: quick scan (ORIENTATION.md was not present at brainstorm time — deleted earlier this
  session as stale; no `repo-orientations` entry exists yet for this repo either)

## Framing

`ranting_gaps` already does exactly this for English: it reads real text, nominates candidate
words via cheap syntactic cues (a determiner, a plural numeral), runs `ranting`'s actual inflection
against an independently hand-written "differential oracle" rule set, keeps only findings the
corpus itself attests (so a spurious nomination like "the is" never produces a false "ises"
finding), and ranks the survivors by frequency. The question this brainstorm answers: how far does
that pattern generalize — to the four non-English forks the repo already has (`ranting_i18n`
German, `ranting_es` Spanish, `ranting_ar` Arabic, `ranting_ja` Japanese), and to the world's most
widely spoken languages more broadly?

No related prior brainstorm exists for this repo on this subject (the one hit in
`research-brainstorms` was about `recounting`'s upstream feedback, unrelated). One relevant drift
pattern surfaced from a prior synthesis on this repo: a repo-grounded "no, the current structure
can't do X" answer can be scoped to the *current* code organization rather than to the goal itself
— worth remembering below, since it applies almost exactly to Direction A.

## Directions considered

### Direction A — A shared, generic gap-finder pipeline, instantiated per fork as its own non-falsifier sibling crate

Why it fits: `ranting_gaps`'s pipeline (ingest → nominate via cue → run real inflection → compare
to an independent oracle → require corpus attestation → rank by frequency) is mostly language-
generic — the corpus ingestion, frequency ranking, and report-writing machinery don't know
anything about English. What's English-specific is narrow and swappable: the cue word list
(determiners/plural markers), the tokenizer, and the oracle rule set. The falsifier contract
(`ranting_i18n`/`ranting_es`/`ranting_ar`/`ranting_ja` may never depend on `ranting_core`/
`ranting_derive`) looks at first like it blocks reusing `ranting_gaps` directly for a non-English
language, since `ranting_gaps` leans on `ranting_core::ph_ext` for its closed pre-noun vocabulary.
But — and this is where the stored drift pattern applies — that "no" is scoped to *reusing
`ranting_gaps` itself*, not to the goal. `ranting_gaps` already establishes the precedent that a
tool built *around* a crate, inspecting it from outside and declaring itself not a falsifier, may
depend on `ranting_core`. A `ranting_es_gaps`/`ranting_ar_gaps`-style sibling could do the same
thing around a fork instead of around `ranting` itself, carrying the same explicit "not a
falsifier" note `ranting_gaps/README.md` already carries, so it doesn't muddy what the falsifier
crates prove.

- Tradeoffs: real work per language, not a drop-in — each new tool needs its own tokenizer (see
  Direction D on Japanese), its own cue word list, and above all its own *independent* differential
  oracle. That last one is the crux: `ranting_gaps/src/english.rs` earns its keep by being written
  independently of `src/language/plurals.rs`, so agreement isn't tautological. A German/Spanish/
  Arabic gap-finder needs someone to independently write "how German/Spanish/Arabic plurals or
  verb conjugation actually work" a second time, in a form that doesn't call the fork's own code —
  that's linguistic labor, not just engineering, and it's the one piece a data-driven bundled
  corpus doesn't supply for free.
- Open questions: is a shared crate (generic pipeline + pluggable per-language modules) worth
  building before there's a second instantiation to generalize from, or should the first
  non-English gap-finder just be written standalone and factored out later once the shape is
  proven twice.
- Confidence: corroborated — repo-grounded architecture read agrees with the falsifier-contract
  text in `CLAUDE.md`/`crate-layout.md`, and the resolution pattern matches a previously-recorded
  drift lesson about scoping "no" answers to current structure.

### Direction B — Prioritize by typology: Spanish first, defer SOV/prenominal-adjective languages

Why it fits: the typology research angle (cross-checked against this repo's own empirically-
derived findings in `docs/EXTENSIBILITY.md`/`crate-layout.md`, which line up with WALS) shows the
world's most-spoken languages split cleanly into two groups for this purpose. **Postnominal-
adjective, SVO-compatible languages** — Spanish (already a confirmed-working falsifier),
Portuguese, Standard Arabic (VSO but noun-phrase-internal order is postnominal, already a confirmed
falsifier), and weakly Indonesian/French — are where a corpus-driven inflection check would
actually *find bugs*, because the template-supplies-word-order assumption holds well enough for
real sentences to exercise the inflection hooks meaningfully. **SOV or prenominal-adjective
languages** — Hindi, Urdu, Russian, Mandarin, and Japanese (already a confirmed falsifier) — hit
the same permanent word-order boundary German already demonstrates, so a naive corpus run there
would mostly just re-confirm "prenominal adjectives are unreachable" over and over rather than
surface new inflection defects, unless deliberately narrowed to slots that survive the boundary
(verb conjugation, article/case selection, numeral agreement — not the `!`/`!!` degree hook).
- Tradeoffs: "most spoken languages" and "most testable languages" are different lists; leading
  with testability means Hindi/Urdu/Russian/Mandarin — huge speaker populations — stay unaddressed
  by this kind of tool for longer, even though they're exactly where the architecture's real limits
  live and arguably most worth *documenting* precisely.
- Open questions: is a narrowed corpus check (verb/article/numeral only, skip adjective) worth
  building for an SOV language specifically to map how much of the surface *does* survive the
  boundary, rather than skipping such languages outright.
- Confidence: corroborated — external typological research (WALS-sourced) independently agrees
  with the repo's own hard-won findings from building the four existing forks, which the research
  agent flagged explicitly as a useful cross-check.

### Direction C — Bundle Universal Dependencies treebanks as the default corpus; Leipzig as an optional larger tier

Why it fits: a gap-finder needs real per-language sentence text, small enough to live in the repo
the way `ranting_gaps` currently runs against the repo's own 111k words of prose. UD treebanks are
sized right for that (hundreds of KB to a few MB of raw sentence text per language, e.g. UD
English-EWT: ~255k words), cover German/Spanish/Arabic/Japanese and most other major languages
directly via `UniversalDependencies/UD_<Language>-<Treebank>` on GitHub, and are CC BY-SA licensed
— redistributable with attribution, unlike the OSCAR/CC-100/raw-Wikipedia options. Leipzig Corpora
Collection (CC BY 4.0, 10k/100k/1M-sentence tiers per language) is the natural next tier for a
deeper, opt-in run rather than the bundled default, matching how `ranting_gaps` already treats
"point it at more text" as a parameter rather than a fixed corpus.
- Tradeoffs: UD treebank licenses vary per-treebank (mostly CC BY-SA 4.0, occasionally 3.0 or
  NC-SA) so each language's exact terms need checking before bundling, not assumed uniform. Tatoeba
  (CC BY 2.0 FR, some CC0) and hermitdave/FrequencyWords (MIT code, CC BY-SA 3.0 data) are smaller
  and cleaner-licensed alternatives worth comparing per language rather than defaulting to UD
  everywhere.
- Open questions: none of this was cross-checked against how `ranting_gaps` itself handles
  attribution/licensing for its own corpus today (it runs against this repo's own prose, so the
  question hasn't come up) — worth resolving before shipping any bundled third-party text.
- Confidence: corroborated for UD's existence/size/format; single-source-flagged on OSCAR/CC-100's
  exact copyright status (their own project pages, not independently corroborated), which matters
  only if a larger corpus tier is ever pursued, not for the UD-bundling default.

### Direction D — Japanese (and Mandarin-shaped languages generally) need a real tokenizer change, not just a new cue list

Why it fits: `ranting_gaps`'s tokenizer only splits on non-alphabetic runs — there is no
whitespace-boundary logic at all, so it's unusable as-is on unspaced Japanese text, independent of
`.claude/rules/heed-input-parsing.md`'s separate (and deliberately scoped) "whitespace is the only
word boundary" stance for `heed!()`/`ask!()`, which doesn't apply here since `ranting_gaps` doesn't
use that subsystem. A `ranting_ja_gaps`-style tool would need either a lightweight morphological
segmenter or a narrower approach that only analyzes particle-delimited runs, which is materially
more work than swapping a cue-word list and an oracle the way a German or Spanish version could.
- Tradeoffs: this pulls in a dependency class (segmentation) none of the rest of the repo has ever
  needed — worth being honest that it's a bigger lift than Direction A's "pluggable module" framing
  suggests for this one language specifically.
- Open questions: is a crude particle-boundary heuristic (だ/の/を/が-delimited runs) good enough for
  a first cut, given `ranting_ja`'s own hole inventory is already narrow (verb, numeral, and the
  numeral-elision hook it caused), or does a real tokenizer need to happen before this is worth
  attempting at all.
- Confidence: reasoned-only, grounded directly in reading `ranting_gaps`'s tokenizer code plus the
  existing documented script-segmentation stance elsewhere in the repo.

### Direction E — Keep the "require corpus attestation" filter; it looks like a genuinely underdocumented technique worth naming explicitly if this work is ever written up

Why it fits: the prior-art research angle searched specifically for "only report a rule divergence
if the corpus attests the corrected form" and found no matching documented practice in spell-
checking, morphology, or NLG literature — the closest adjacent hit was a different direction
entirely (validating synthetic error injection, not validating correction claims). Established
prior art clusters around two other things instead: gold-standard-corpus accuracy evaluation (UD,
UniMorph, spaCy) and frequency-list generation from corpora (Leipzig, wordfreq) — but not the
combination `ranting_gaps` already uses. Continuing to build on this pattern for new languages
means continuing to use something that, as far as this research could establish, isn't a named
technique anywhere else.
- Tradeoffs: none for the technique's use here — it already demonstrably works in `ranting_gaps`
  (this is not a proposal to change anything, just a flag that it's worth keeping and worth
  documenting/naming explicitly if the tool or its methodology is ever shared outside this repo).
- Open questions: is this worth a short writeup independent of any new language-specific tool,
  given it may be a genuinely reusable idea for anyone building a similar rule-vs-corpus checker.
- Confidence: single-source / unconfirmed — the research agent could not access the one paper
  (arXiv 2510.23131, Oct 2025) that comes closest to this idea beyond its abstract, so "no one has
  documented this" should be read as "no one this search could confirm has documented this," not
  as a settled negative.

## Contradictions surfaced

None between angles directly. One near-contradiction was pre-empted using stored research
memory: the repo-grounded angle's "the falsifier contract blocks reusing `ranting_gaps` for a
non-English language" is correct as stated but scoped to *reusing that specific crate* — it does
not block the underlying goal, which Direction A's non-falsifier-sibling-crate framing achieves
instead. Recorded before as a general pattern from a prior synthesis on this repo; this brainstorm
is a second confirmed instance of the same shape, so no new drift entry was added for it.

## Recommendation

No single direction supersedes the others — they compose. If asked to actually pick a starting
point: **Direction A's shape (a non-falsifier sibling crate, not a `ranting_gaps` extension) applied
first to Spanish (Direction B's highest-confidence testable language), using a bundled UD Spanish
treebank excerpt (Direction C) as the corpus, and keeping the attestation-filter technique
(Direction E) unchanged.** That combination reuses the most already-proven pieces (a working
falsifier, a typologically favorable language, a small clean-licensed real-sentence corpus, and a
technique with no counter-evidence against it) and defers the two genuinely open-ended pieces —
Japanese-class tokenization (Direction D) and any SOV/prenominal-language coverage (Direction B's
deferred half) — to a second pass once the pipeline's shape is proven once outside English.

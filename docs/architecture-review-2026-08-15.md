# Architecture / documentation review — 2026-08-15

Two-stage parallel audit (eight read-only investigators, one verification pass over 33 cited
claims: 24 confirmed, 4 corrected, 0 refuted). Claims the verifier could not confirm are not in
this document.

**Third in a series, none of which supersedes another.** `docs/architecture-review-2026-08-13.md`
covers pre-`ranting_core` history; `-08-14.md` covers the state after Phase 6 and the
ROADMAP→DONE split; this one covers the state after Phase 7 completed, and audits that phase's own
work. Read together, not instead of each other.

**Standing bias to declare**: five of the eight topics audited code committed the same day the
audit ran, and the two highest-severity findings below are defects in that code. The splice defect
(§1.1) was reproduced by the session lead independently of the report that raised it.

## 1. Code defects

### 1.1 The numeral-elision splice used byte offsets preposition fusion had invalidated — ✅ **FIXED 2026-08-15**

`src/lib.rs`, `handle_placeholder_impl`. Introduced 2026-08-14 by ROADMAP.md Phase 7 item 12,
fixed the day after this review recorded it.

The three post-assembly steps *used to* run in the order preposition fusion, numeral elision,
article elision. On success, fusion does `res.truncate(p_start)` and rebuilds, shifting every byte
after `p_start` by `fused.len() - (a_end - p_start)`. But `numeral_span` was recorded far earlier,
at the point the numeral is pushed.

The article splice guards on `!prep_fused`. **The numeral splice did not**, so after a successful
fusion it sliced `&res[start..end]` at displaced offsets and truncated at a displaced index.
Reproduced:

```
say!("Vengo de {the $0 1}", 2, n)     // fixture fuses "de"+"el" -> "del"
  hook receives  numeral="> g"  sep=""   following="ato"     // sliced out of "<2> gato"
say!("{the $0 1}", 2, n)              // same fixture, no preposition
  hook receives  numeral="<2>" sep=" "  following="gato"     // correct
```

In that fixture the final string still came out right by coincidence. With multibyte text the same
displaced index lands off a `char` boundary and **panics** — the identical failure mode as
`-08-14.md` §1.7, in code written the day after §1.7 was fixed. `end` can also exceed `res.len()`
when the fused form is shorter.

It was reachable rather than latent: `inflect_preposition_custom` is overridden by two existing
forks and is not on the never-overridden list. No test reached it, because it additionally needs an
`elide_numeral_custom` override in the same placeholder, and only `ranting_ja` has one.

The source comment was wrong twice over: it claimed the numeral splice "runs first" when fusion
preceded it, and "editing it leaves `article_span` untouched" named the wrong hazard — the hazard
ran the other way.

**Fixed** by making the numeral splice run *first of the three*, ahead of preposition fusion —
the innermost region of `[preposition][article][numeral][noun]`. Every byte it rewrites is then at
or after `article_span`'s end, so both spans the later splices depend on stay valid, and they see
the already-fused numeral+noun as their trailing text. That also makes the original comment's
reasoning true rather than merely plausible.

Pinned by two tests in `tests/ranting/preposition_fusion.rs`, **both verified to fail against the
old ordering** rather than only to pass against the new: one asserts the rendered string, the other
spies on the hook's inputs, since the old code could still produce a correct-looking result by
accident. A fixture overriding preposition fusion, a numeral hook and `elide_numeral_custom` at
once did not previously exist — which is why this was reachable but untested.

### 1.2 Two nouns rendered the wrong plural — fixed 2026-08-16

`data/irregular_plurals.txt` did **not** contain `hero`, `piano` or `quiz`, though
`.claude/rules/pluralization.md` point 2, `ROADMAP.md` and `src/language/plurals.rs:15-17` all
stated that it did, and gave exactly those words as the reason the table exists.

Verified consequences (as of 2026-08-15, before the fix):

| Input | Rendered | Correct | Path |
|---|---|---|---|
| `{+hero}` | `heros` | `heroes` | bare append, `plurals.rs:108` — reaches no sibilant arm |
| `{+quiz}` | `quizes` | `quizzes` | sibilant arm on `z`, `plurals.rs:102` |
| `{+piano}` | `pianos` | `pianos` | **correct** — not a defect |

**Fixed 2026-08-16**: `hero|heroes` and `quiz|quizzes` added to `data/irregular_plurals.txt`.
`piano` intentionally still has no row — the bare-`s` default already renders it correctly, and a
row would be a no-op. All eight crates' gates pass with the two new rows.

### 1.3 The `-f`/`-fe` "compounds only" claim is half true — ✅ **doc fixed 2026-08-16**

`pluralization.md` point 3, `plurals.rs:38-40` and `ROADMAP.md` all said the `-f`/`-fe`→`-ves` stem
lists "only ever fire for compounds", the bare words being table rows already. The verifier split
it:

- **`-fe` stems** (`knife`/`wife`/`life`) — rows at `data/irregular_plurals.txt:25-27`. Claim
  **holds**.
- **`-f` stems** — `leaf`/`loaf`/`wolf`/`thief`/`elf` are rows (`:28-32`), but `calf`, `half`,
  `shelf`, `self` are **not**. The `-f` rule fires for those bare words, and
  `plurals.rs:253-255` asserts exactly that (passing).

Output was correct either way; only the explanation was wrong. `.claude/rules/pluralization.md`
point 3 and `ROADMAP.md`'s Phase 7 item 10 entry now state the split explicitly instead of the
blanket "compounds only" claim.

### 1.4 `#[derive(Heed)]` on an empty braced struct — fixed 2026-08-16

`-08-14.md` §1.1. `ranting_derive/src/heed_derive.rs:136` branched on `field_idents.is_empty()`
rather than on `Fields::Unit` vs `Fields::Named`, emitting a bare `Self` — legal only for tuple
and unit structs — for an empty *braced* struct too, where `Self {}` was required. Fixed by
tracking `is_unit_struct` (from the `Fields` variant) separately from "has zero fields," and
generating `Self {}` for the braced-and-empty case. Pinned by
`tests/ranting/heed_derive.rs::zero_captures_empty_braced_struct_still_generates_heed`
(`struct Wait {}`). All eight crates' gates pass.

## 2. Documentation defects found and fixed on 2026-08-15

| # | Claim | Where | Reality |
|---|---|---|---|
| 2.1 | `NumeralSpec`'s `numeral` field doc said `None` covers a hidden numeral | `ranting_core/src/placeholder.rs:410-412` | Contradicted the type's own doc at `:213-214`, authoritative since the derive bakes `Some { hidden }`. Introduced 2026-08-14 by item 13; one of the two docs was updated and the other missed |
| 2.2 | "`ranting_derive`'s regex 1.6.0 and `ranting`'s regex 1.11 never need to match" | `.claude/rules/heed-input-parsing.md`, repeated at `src/heed.rs:6-7` | **Both declare `regex = "1.11"`.** The decoupling mechanism is real and verified; the version pair illustrating it was fiction |
| 2.3 | "Only two of the eight hook pairs are live" in `ranting_ja` | `.claude/rules/crate-layout.md:33` | Three: verb, numeral, and the `elide_numeral_custom` the crate itself caused to exist. The crate README already said three |
| 2.4 | "story-wide" for `NarrationContext` | 9 sites in `src/lib.rs`, 1 in `src/narration.rs`, plus `docs/API.md` and `docs/EXTENSIBILITY.md` | Phase 7 item 13 retired the phrase in two files and missed ten more sites. `lib.rs:1876` had become self-contradictory, calling the settings story-wide in a sentence that also said they vary per call |
| 2.5 | Two captures with "no literal text between them" is a compile error | `.claude/rules/heed-input-parsing.md` | Overstated: the check fires on a **zero-width** gap, so `{a} {b}` compiles, pinned by `ranting_derive/src/heed.rs`'s `whitespace_separated_captures_are_allowed` |
| 2.6 | `{name...}` "captures greedily" | same file | It is lazy (`.+?`) |
| 2.7 | `SENTENCE_TRIGGER_CHARS` has one reader | `.claude/rules/placeholder-grammar.md` | Two: `at_sentence_start` and the `preposition` filter (`ranting_derive/src/lib.rs:159,169`) |
| 2.8 | The append-exception preserves interior capitals | `.claude/rules/pluralization.md` point 5 | True but incomplete: additionally gated on `!is_all_caps`, which is what keeps `BOX` → `BOXES` |
| 2.9 | `ranting_core` "v1.2" | `.claude/rules/crate-layout.md:54` | Manifest says `0.1.0` |
| 2.10 | Phase 7 "In progress" and "Phase 7 is complete" | `ROADMAP.md`, two lines apart | Self-contradiction introduced by the same commit that completed the phase |
| 2.11 | "five crates" | `ROADMAP.md`, `-08-14.md:180` | Eight manifest directories since `ranting_ar`/`ranting_ja`. `CLAUDE.md`'s gate list was already correct — the reviews auditing it had gone stale first |
| 2.12 | Gates "missed real defects three times, each found by review" | `CLAUDE.md` | Count is now four, and "each found by review" was already wrong: §1.7 surfaced on `ranting_ar`'s first `cargo test`, i.e. a gate in a *new* directory reaching a line six existing gates could not |
| 2.13 | `ranting_i18n`'s `hole_8b` had no README entry | `ranting_i18n/README.md` | The only hole↔test parity gap across all four falsifier crates. Now named under hole 8 |
| 2.14 | `CLAUDE.md`'s record table cited only the 08-14 review | `CLAUDE.md` | The 08-13 review is a live companion, cited from `ROADMAP.md` and referenced by 08-14's own header — not superseded |

## 3. Verified accurate (recorded so the next audit can skip them)

- **The falsifier contract holds in all four crates.** `ranting_i18n`, `ranting_es`, `ranting_ar`
  and `ranting_ja` each declare exactly one dependency, `ranting = { path = ".." }`. No
  `ranting_core`/`ranting_derive` path in any manifest or source file; the only hits are prose and
  transitive `Cargo.lock` entries.
- **Hole↔test parity is complete in both directions** for `ranting_es`, `ranting_ar` and
  `ranting_ja`, and for `ranting_i18n` after 2.13. Closed holes assert corrected output, not the
  old broken strings.
- **Hook inventory**: 8 `_custom` pairs, 9 `_with_context` methods, 25 trait methods total, one
  unpaired (`is_first_person_subject_custom`). `Many`, `Maybe` and `Box<T>` each override **all
  25**, no fall-through. `-08-14.md` §3's "23"/"all 23" and its "exactly 12 `own_count` sites" are
  stale — the real figures are 25 and 15.
- **`hook_audit.sh`'s scope is wider than its header says** — it globs `*/Cargo.toml` and greps for
  a `ranting` dependency, excluding only `ranting_core`/`ranting_derive`, so it also audits
  `ranting_gaps`. That crate implements no `Ranting` method and contributes zero to every column,
  so **the published never-overridden counts are unaffected**; only the header is wrong.
- **The deliberate duplications are intact and both sides carry their notes**:
  `ranting_gaps/src/english.rs` vs `src/language/plurals.rs`, and `PH_EXT` vs `ph_ext` (parity
  enforced by `assert_parity` over a curated corpus plus a proptest).
- **All six `placeholder-grammar.md` claims** about the two-pass `ph_ext::parse` hold, including
  the one-repetition rule being enforced in `parse_pass` rather than in the matcher.
- **`failures/` is current**: two findings, `word-order-prenominal-adjective` (1250 occurrences)
  and `pre-word-homograph` (73), matching what `ROADMAP.md` claims the regenerated tree contains.
- **Both scripts glob** for `Cargo.toml`, so the two crates added on 2026-08-14 were gated and
  audited without either script being edited.

## 4. Left undone, and why

- **§1.2's missing table rows have since been added** (2026-08-16) — see the updated §1.2 above.
- **Four orphan top-level docs** — `ARGUMENT_PARSING_IMPROVEMENTS.md`, `DESIGN_REPORT_SUMMARY.md`,
  `PHASE_2_IMPLEMENTATION_PLAN.md`, `RECOUNTING_INTEGRATION.md` (1,281 lines) — appear in no index.
  Three carry self-supersession banners; `ARGUMENT_PARSING_IMPROVEMENTS.md` has none and
  contradicts itself on what error message the code produces. `DESIGN_REPORT_SUMMARY.md`'s own
  banner has drifted ("the repo now has five crates"). Deleting or bannering them is a maintainer
  decision, not an audit finding.
- **`README.md` mentions no sibling crate at all**, and still carries the `{*can can}` example that
  `ROADMAP.md` already tracks as demonstrating the marker where it changes nothing. Both are
  existing open items rather than new findings.
- **Trivia skipped**: test counts, line counts, a duplicate `goose|geese` row (harmless — first
  match wins), and `plurals.rs`'s "63-line table" comment against 51 data rows.

### 4.1 Nine hooks have never been exercised by a real fork (added 2026-08-16)

`scripts/hook_audit.sh`, re-run against all four falsifiers plus `ranting_gaps` (contributes zero
to every column — see §3): the never-overridden list is exactly the eight `_with_context` twins
of the eight `_custom` hook pairs, plus `is_first_person_subject_custom` — the one hook with no
`_with_context` twin at all. Every plain `_custom` hook and `inflect`/`capitalize` have at least
one real override somewhere; these nine have zero, across every fork, including `ranting_ar` and
`ranting_ja`, the two that added the furthest-reaching new mechanics (a third morphological
number, `elide_numeral_custom`).

This is the same shape of blind spot `-08-14.md` §4.7 describes for derive-generated `inflect()`:
the surface compiles, is reachable (every `_with_context` hook fires from `say_with!()`, and
`is_first_person_subject_custom` fires from ordinary `say!()`), and is covered only by this
repo's own tests asserting the *default* — "an unoverridden `_with_context` hook reproduces the
plain hook's output," "an unoverridden first-person check still recognizes `I`/`we`" — never by a
fork that needed different behavior and got it. No non-English fork has yet hit a case where
`NarrationContext.register`/`.dialect` need to change what a hook does mid-story rather than
which pronoun set it points at, and none has hit a first-person label other than `I`/`we` that the
hard-coded check would miss.

Not scheduled as a fix — there is no defect to correct and no test to write yet, since nothing has
ever exercised the path that would expose one. Left as an open item, in the same spirit as §4.7:
the falsifier contract is designed to surface exactly this kind of gap, and after four forks it
still hasn't fired on this corner of the hook surface.

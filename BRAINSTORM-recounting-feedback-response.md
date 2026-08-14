# Brainstorm: how `ranting` should respond to `recounting`'s upstream feedback

- Generated: 2026-08-14
- Repo context: `ORIENTATION.md` (fresh, stamped at current HEAD `cfbce06a`)

## Framing

`recounting` (a parser-interactive-fiction engine, sibling repo, depends on `ranting` via a path
dependency per `.claude/rules/01-project.md`-style pinning) sent three pieces of feedback, already
landed in this repo:

- `docs/heed-macro-evaluation.md` — an evaluation of `heed!()`/`#[derive(Heed)]` for
  `recounting-parse`'s grammar-line matcher (M4), concluding **recounting will hand-write its own
  matcher regardless**, but naming two gaps that would make `heed!()` worth reconsidering later.
- `docs/ranting-surface.md` — a table of exactly what `Ranting`/`Noun`/`say!()` surface
  `recounting-narrate` calls today, written after an undocumented `Ranting::inflect` signature
  change broke `recounting`'s CI with no local warning.
- `backups/recounting-all-2026-08-14.bundle` — a full snapshot of `recounting`'s repo, whose own
  `ROADMAP.md` "Upstream requirements (`ranting`)" section is the fuller source of the asks; it
  lists two more open items beyond the two docs above: a genuinely **blocking** stable-toolchain
  build failure (item 4) and an **open, M2-blocking** per-instance article-suppression gap
  (item 7).

So there are five distinct threads, not two: the two `heed!()` gaps, the stable-build/publish gap,
the article-suppression gap, and — a question this brainstorm added on its own initiative — whether
`ranting` should reciprocate `ranting-surface.md`'s pattern in its own repo.

No related prior brainstorm or `repo-orientations`/`research-brainstorm-drift` entries were found
for this repo or subject.

## Directions considered

### Direction A — Give `heed!()` structured match-failure info

- **Why it fits**: closes the first gap `heed-macro-evaluation.md` names — `HeedMatcher::match_input`
  returns a bare `Option<Vec<String>>`, so any consumer needing to distinguish *why* a match failed
  (recounting's `NotUnderstood`/`OutOfScope`/`Ambiguous` split) gets no help from `heed!()` today.
- **Tradeoffs**: not a small change. `HeedMatcher` flattens the whole template into one anchored
  `regex::Regex`; the `regex` crate itself has no partial-match/"how far did it get" API — confirmed
  by both the repo-grounded read of `src/heed.rs` and the external survey (`regex`'s public API is
  strictly boolean `is_match`/`find`/`captures`, all-or-nothing). Prior art across nom, winnow, pest,
  combine and chumsky converges on one shape closest to this problem: **pest's model** — track the
  furthest-matched position and the set of "expected" rules there, which pest can do cheaply because
  its PEG engine already walks alternatives one at a time. Applying that here means `HeedMatcher`
  would need to retain (or reconstruct) the segment structure (literal/capture, in order) that
  `compile_heed_template` already knows at compile time but currently discards once it's flattened
  into one pattern string, and `match_input` would need to walk segments instead of delegating to
  one regex call. That's a moderate, well-scoped redesign of the *matcher* (not the compiler) — not
  a one-line addition, but also not a ground-up rewrite.
- **Open questions**: is this worth doing when the one consumer who asked for it already decided to
  hand-roll its matcher regardless? `heed-macro-evaluation.md` is explicit that neither gap blocks
  `recounting` today. Doing this only pays off for a *future* `heed!()` consumer with recounting's
  same need.
- **Confidence**: corroborated (repo-grounded architecture read + 5-library external survey agree).

### Direction B — Add a small, duplicated runtime-constructible `HeedMatcher`

- **Why it fits**: closes the second `heed-macro-evaluation.md` gap — no public constructor exists
  today that takes a template known only at runtime (e.g. loaded from a vocabulary file).
- **Tradeoffs**: feasible as a genuinely small, additive change — a new
  `HeedMatcher::from_template(&str) -> Result<Self, HeedTemplateError>` in `ranting` itself,
  compiling an owned `String` pattern eagerly instead of a `const fn` over `&'static str`. But it
  requires **duplicating** (not reusing) the ~120-line segment-parsing/pattern-building algorithm,
  because that logic lives in `ranting_derive` — a `proc-macro = true` crate, which cannot be linked
  as a normal runtime dependency by `ranting` even though `ranting` is `ranting_derive`'s own
  reverse dependency. Nothing today pins the two copies (compile-time in `ranting_derive`, a new
  runtime one in `ranting`) to stay in sync; there's no shared test fixture between them.
- **Open questions**: is a second, un-synchronized copy of the algorithm an acceptable ongoing
  maintenance cost, or does it warrant the bigger restructuring in Direction C instead?
- **Confidence**: reasoned-only, grounded directly in the repo's crate-boundary structure (this is a
  Rust-toolchain constraint, not a judgment call — proc-macro crates genuinely cannot be depended on
  for runtime code).

### Direction C — Unify the engine instead: move pattern-building into `ranting`, make the macro a thin caller

- **Why it fits**: this direction wasn't proposed by either individual research angle — it falls out
  of combining them. The external prior-art survey found that `sqlx::query!`/`lazy-regex`'s
  `regex!()` **do not duplicate** their runtime engine: the macro is compile-time verification/sugar
  that expands to a call into the *same* ordinary runtime constructor (`sqlx::query()`,
  `regex::Regex::new()`) a direct runtime caller would also use — `phf` is the one clean
  counter-example, but its compile-time-only design is inherent to perfect-hashing's build cost, which
  doesn't apply to `heed!()`'s regex-based approach (regex compilation is already an ordinary,
  cached-at-runtime operation elsewhere in `ranting`, per `.claude/rules/heed-input-parsing.md`). Applied here: move
  `build_heed_pattern`/segment-parsing out of `ranting_derive` and into `ranting` as a public runtime
  function; `ranting_derive`'s macro stops building the pattern string itself and instead emits code
  that calls `ranting::HeedMatcher::compile(TEMPLATE_STR)` once, lazily (a `OnceLock`/`Lazy`-wrapped
  static instead of today's `const fn` literal). Both the macro-generated static path and a new
  direct runtime-string path would then call the identical function — no duplication, no drift risk.
- **Tradeoffs**: bigger architectural change than Direction B. Loses the `const fn`/pure-literal
  `static` construction `HeedMatcher::new` has today (replaced by a one-time lazy init, which is a
  negligible added cost since `Regex::new` is already deferred behind a `OnceLock` in the current
  design). It also changes the documented crate-decoupling rationale in `.claude/rules/heed-input-parsing.md`
  — today's split exists specifically so `ranting_derive`'s and `ranting`'s regex crate versions
  never need to match; moving pattern-building into `ranting` doesn't break that (the two versions
  still wouldn't need to match, since `ranting_derive` would no longer touch pattern-building at
  all), but it's a rationale the docs would need to be rewritten to reflect, not just extended.
- **Open questions**: does this redesign make sense to do *together* with Direction A (both touch
  `match_input`/the matcher's internals), as one combined `heed!()` overhaul, rather than three
  separate half-measures over time?
- **Confidence**: reasoned-only synthesis of two angles; the specific claim "regex compilation is
  already deferred/cached, so this cost is negligible" is grounded in this repo's own documented
  design (`.claude/rules/heed-input-parsing.md`), not speculation, but the overall tradeoff judgment is not
  independently corroborated by a third source.

### Direction D — Per-instance article suppression on `Noun`

- **Why it fits**: closes `recounting`'s ROADMAP item 7, explicitly filed as **open, blocking** its
  M2 milestone ("article suppression for proper-named entities") — the one concrete, currently-active
  ask among all five threads, not a "would be nice for the future" item like the `heed!()` gaps.
- **Tradeoffs**: `skip_article` is a plain trait method (no `_with_context` twin, confirmed against
  `extension-hooks.md`'s hook inventory), and `Noun` goes through the same `#[derive(Ranting)]`
  macro as user types — it isn't hand-written, so this isn't "add a field + edit one method." The
  actual blocker: `no_article` is a plain `bool` in `ranting_derive`'s `RantingOptions` with no
  `"$"`-sentinel runtime-field-read mode, unlike `name`/`subject`/`gender`, which already support
  exactly that pattern (`get_namefn_for`/`get_noun_class_fn`). The fix is mechanical and has three
  proven precedents to copy: teach `no_article` the same `"$"` sentinel, so
  `#[ranting(no_article = "$")]` reads a `bool` field at runtime instead of baking a literal; then
  add `Noun::with_skip_article(bool) -> Self`, following `with_noun_class`'s exact builder shape.
  `Many`/`Maybe`/`Box` delegation needs zero changes — already calls `skip_article()` dynamically at
  runtime regardless of what it does internally.
- **Open questions**: `no_article`'s attribute shape (`"true"` literal vs. `"$"` field-pointer) is an
  existing, documented, public attribute — widening its accepted values is a genuine (if narrow)
  shape decision on public API surface, worth explicit maintainer sign-off even though the
  implementation pattern is proven three times over already.
- **Confidence**: corroborated — repo-grounded, direct code read of `Noun`'s derive attributes,
  `RantingOptions`, and the three existing `"$"`-sentinel precedents.

### Direction E — Resolve the stable-toolchain build/publish gap

- **Why it fits**: `recounting`'s ROADMAP item 4 calls this the only thing blocking their CI
  entirely (not an MSRV nuance — a flat compile failure on any stable toolchain).
- **Tradeoffs**: the underlying *code* defect is already gone. `grep -rn "#!\[feature("` across
  every crate in this repo returns nothing; the nightly `#![feature(iter_intersperse)]` gate
  `recounting` hit was specific to the old `v0.2.1` git tag, not current HEAD. What remains is purely
  a **versioning/publishing gap**: the manifest still says `0.2.1`, `CHANGELOG.md` is already at
  `v1.3.0`, and `docs/architecture-review-2026-08-14.md` §4.1 records that mismatch as **flagged and
  left unresolved** — its own wording is that publishing "is the copyright holder's call, so no
  manifest was touched," which is the reviewer declining to act pending that call, not evidence a
  decision against publishing has actually been made. (Checked the newer
  `docs/architecture-review-2026-08-15.md`, one day fresher and stamped at the same HEAD this
  brainstorm cites — it doesn't revisit versioning at all, so this grounding is thin either way and
  the gap should be treated as genuinely open, not settled.) There is also no CI in this repo at all
  (`.github/workflows/` doesn't exist), so nothing currently verifies stable-toolchain builds
  automatically going forward either.
- **Open questions**: this is not an engineering task at all — it's a publishing decision, and an
  unresolved one, not a closed one. The actionable version of this direction is narrower than "fix
  the bug": it's "decide whether/when to cut and publish a release past 0.2.1," which `recounting`
  can act on today regardless (pin a git rev instead of a version string) without waiting on
  `ranting`.
- **Confidence**: corroborated — direct repo grep plus the architecture review's own explicit,
  dated record of the decision.

### Direction F — Reciprocate `ranting-surface.md` upstream (don't)

- **Why it fits**: `recounting`'s `ranting-surface.md` exists because an undocumented signature
  change broke their CI silently; the natural-sounding response is "shouldn't `ranting` keep a
  mirror of what its consumers depend on, so this is caught before publish?"
- **Tradeoffs**: reasoned analysis concludes **against** this. `ranting`'s own falsifier crates
  (`ranting_i18n`/`_es`/`_ar`/`_ja`) already give in-repo, red-on-breakage coverage the moment a
  public-API-shaped change lands, which is a superset of what a hand-maintained surface doc would
  catch, in the one dimension that matters (real trait impls compiling against the change, not a
  static name list). A mirror doc describing an *external* repo's usage, kept inside `ranting`'s own
  repo, would go stale the instant `recounting` adds a new call site nobody remembers to mirror
  upstream — worse than no doc, since a stale contract reads as false confidence. Semver discipline
  is the deciding factor: `ranting` hasn't published past 0.2.1 (Direction E), so there's no
  external semver boundary yet to defend this way — `recounting`'s own downstream-side test is
  already the right and sufficient defense for a moving-target dependency.
- **Open questions**: none blocking; this is presented as a closed recommendation, not an open
  choice.
- **Confidence**: reasoned-only, but the argument is self-consistent and doesn't rest on any
  single external source — it's a structural argument from this repo's own falsifier-contract
  design plus the already-established versioning gap (Direction E).

## Contradictions surfaced

One genuine tension, not a factual contradiction: **Direction B's repo-grounded conclusion**
("the runtime constructor requires duplicating the algorithm, because `ranting_derive` is
proc-macro-only and unreachable at runtime") **and the external prior-art angle's precedent**
(sqlx/lazy-regex avoid exactly this duplication) look at first like disagreement, but resolve
cleanly: the repo-grounded agent was reasoning within the *current* architecture (pattern-building
lives in `ranting_derive`), while the external precedent implies a different architecture (pattern-
building living in `ranting` instead) would avoid the duplication entirely. Direction C names that
resolution explicitly. This is presented as two directions (B: small/duplicated now, C: bigger/
unified later) rather than picking one, since which is right depends on whether `ranting`'s
maintainer wants to invest in the larger `heed!()` redesign at all — see Recommendation.

## Recommendation

No single direction dominates all five — they're genuinely different in urgency:

- **Direction D (article suppression)** and **Direction E (versioning/publish decision)** are the
  two *live* asks — items ROADMAP.md's recounting-side tracker marks "open, blocking" and
  "blocking," respectively, not speculative future-proofing. D is the more actionable of the two
  today: small, well-precedented, purely additive, needs only a scoped `ranting_derive` change plus
  maintainer sign-off on widening `no_article`'s attribute shape. E is not an engineering task —
  it's a publish/versioning decision only `ranting`'s maintainer can make; `recounting` already has
  a workaround (pin a git rev) that doesn't require `ranting` to act first.
- **Directions A, B, and C (the `heed!()` gaps)** are explicitly *not* blocking anything today —
  `recounting` already decided to hand-roll its matcher regardless of what `ranting` does here. They're
  worth keeping on record (as `heed-macro-evaluation.md` already does) rather than acting on now,
  unless another consumer surfaces the same need. If they ever are picked up, do A and C together as
  one `heed!()` redesign rather than B in isolation — B's quick win creates exactly the kind of
  unsynchronized duplicate that `.claude/rules/crate-layout.md`'s "deliberate duplication" notes are
  careful to flag and justify elsewhere in this repo; an unflagged, accidental duplicate would be a
  worse outcome than either committing to C's unification or not doing B at all.
- **Direction F**: don't do — the falsifier crates already serve this function, and there's no
  published semver boundary yet to protect.

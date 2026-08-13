# Architecture Review — 2026-08-13

Findings from a doc/code consistency audit (7 parallel investigations + a
verification pass) covering both crates' source, generated-code sharing
mechanisms, and the repo's in-progress-work tracking. This is a point-in-time
findings record, not a durable spec — see `CLAUDE.md` and `ROADMAP.md` for
current architecture and status; this doc is referenced from a couple of
spots where it has the full detail.

## 1. Irregular-plural lookup functions are dead code in both crates

`get_plural`/`get_singular`/`apply_case` exist in two places:

- `src/language/plurals.rs:12,23,35` (canonical, `ranting`)
- `ranting_derive/src/language/plurals.rs:14-52` (hand-duplicated, **not**
  generated via the usual `include!(OUT_DIR)` pattern the way
  `english_shared.rs`/`verb_conjugate.rs` are)

Both copies are genuinely unreachable dead code today:

- Zero callers of the `ranting` copy anywhere in `src/` or `tests/` outside
  its own module and unit tests.
- Zero callers of the `ranting_derive` copy anywhere in `ranting_derive/src/`
  (`lib.rs`, `ranting_impl.rs`, `str_lit.rs`, `heed.rs`) outside its own
  module.

The table-generation codegen itself (`data/irregular_plurals.txt` →
`$OUT_DIR/irregular_plurals_generated.rs` via `build.rs`) is complete and
correct — the gap is purely that nothing calls the lookup functions from an
actual noun-inflection path (`Noun::inflect()` doesn't use them).

**Why this matters**: `ranting_derive`'s copy currently escapes
`cargo clippy -D warnings` detection because that gate only hard-errors the
*primary* package being compiled — when `ranting_derive` builds as a path
dependency of `ranting`, its own dead code shows as a plain warning, not an
error. Running `cargo clippy` from inside `ranting_derive/` directly would
likely surface it.

**The two copies have also drifted** from each other, despite
`ranting_derive/src/language/plurals.rs:7`'s own comment that "both copies
must be kept in sync": the `ranting` copy has `#[allow(dead_code)]` and uses
`.is_some_and(...)`; the `ranting_derive` copy has neither and still uses
`.map_or(false, ...)`. There's no build-time check enforcing the sync.

**Also missing**: unlike `irregular_verbs.txt`, there is no
`ranting_derive/data/irregular_plurals.txt` symlink. A packaged (non-dev)
build would hit the packaged-build fallback path in `find_data_file` and
fail to locate this file unless one is added before the next publish.

## 2. A stale dead-code justification comment (now fixed)

`src/language/english_shared.rs`'s `Article`/`DemonstrativePronoun` enums had
an `#[allow(dead_code)]` comment (added in an earlier session pass while
unbreaking the clippy gate) claiming they were "kept for `ranting_derive`'s
compile-time article handling." That's not true — grep across
`ranting_derive/src/` finds zero references to either type; the derive
crate's real article logic works via string-literal matching
(`ranting_derive/src/lib.rs:215-216,493,500`), and `ranting`'s own real
article logic lives in a separate, actually-used `ArticleOrSo` enum
(`src/language/english.rs:12-18`). The comment has been corrected to state
they're unused in both crates rather than falsely justify them as
cross-crate-consumed.

## 3. No durable record of the overnight-loop stash situation

`git stash list` currently holds 10 entries (`stash@{0}`–`stash@{9}`) each
prefixed `"failed: ..."` by `scripts/overnight_loop.sh`'s automatic
stash-on-gate-failure behavior (`overnight_loop.sh:110-124`). Neither
`CLAUDE.md` nor `ROADMAP.md` mentions `git stash` anywhere.

Context (from direct investigation, not this audit's subagents): all 10 were
stashed because the CI-style gate (`cargo fmt --check && cargo clippy -D
warnings && cargo test`) was broken independently of any of the 12 tasks —
pre-existing dead code and two clippy style lints on the starting commit made
every task in that run unwinnable regardless of its own diff. That baseline
was fixed on `master` (commit `0fe23b24`) and merged into
`overnight/2026-08-12` (`76fac22e`). Rebuilding and retesting each stash's
actual diff against the fixed baseline showed all 12 build and pass tests
cleanly. Two (Licensing proposal, Repo hygiene) were replayed and committed;
the other 10 remain in `git stash list`, deliberately not replayed due to
heavy file-overlap between them (most touch `ROADMAP.md`; several touch
`src/lib.rs`). `overnight_loop.sh` now has a pre-flight gate check
(added same session) so a future run against a broken baseline fails in
minutes instead of stashing a whole night's work.

`tasks.txt` still lists these 10 items as pending work for a future
`overnight_loop.sh` run — that's the existing tracking mechanism, and it's
accurate. This section exists only because the *history* of why they failed
and what's already been verified about them had no other durable home.

## 4. Stale top-level docs

`PHASE_2_IMPLEMENTATION_PLAN.md` claimed "Status: In Progress" and described
a "walkeds" bug as unfixed, while `DONE.md`/`ROADMAP.md` confirm Phase 2 has
long since shipped complete, including that exact bug fixed. A superseded
banner was added pointing to `DONE.md`.

`DESIGN_REPORT_SUMMARY.md`, `RECOUNTING_INTEGRATION.md`, and
`ARGUMENT_PARSING_IMPROVEMENTS.md` are dated (or near-dated) 2026-08-1{1,2}
snapshots whose recommendations/concerns are now reflected as done in
`ROADMAP.md`/`README.md`. None of the three make an active "in progress"
claim the way `PHASE_2_IMPLEMENTATION_PLAN.md` did, and nothing in
`CLAUDE.md`/`ROADMAP.md` points to them, so they were left as-is —
orphaned historical artifacts rather than actively misleading ones.

# CLAUDE.md

`README.md` is the project overview and public-API tour. API docs are on docs.rs.

## Commands

```bash
cargo test                                  # integration + doctests
cargo test --test ranting                   # integration only (single target = tests/ranting/main.rs)
cargo test --test ranting singular_they     # one module/test by name filter
cargo test --doc
cargo test --features debug                 # show compile-time placeholder transforms
cargo clippy --fix
cargo fmt
```

**A green gate at the repo root proves nothing about half the repo.** This is not a cargo
workspace: `ranting` (root), `ranting_core`, `ranting_derive`, `ranting_i18n`, `ranting_es`,
`ranting_ar`, `ranting_ja` and `ranting_gaps` each have their own `Cargo.toml`/`Cargo.lock`, and
the commands above never compile or test the seven siblings. Before reporting anything as
passing, run all three gates in **every** directory that has a `Cargo.toml`:

```bash
for d in . ranting_core ranting_derive ranting_i18n ranting_es ranting_ar ranting_ja ranting_gaps; do
  cargo fmt --manifest-path $d/Cargo.toml --check
  cargo clippy --manifest-path $d/Cargo.toml --all-targets -- -D warnings
  cargo test --manifest-path $d/Cargo.toml
done
```

`scripts/overnight_loop.sh`'s `gate_dirs`/`run_gate` helpers do this by globbing for `Cargo.toml`,
so a future sibling crate is gated automatically without editing the script.

Green gates are necessary, not sufficient: they have missed real defects three times in this repo,
each found by review instead. See `docs/architecture-review-2026-08-14.md` §4.7 for the known
structural blind spot — narrowed 2026-08-14, still open for derive-generated `inflect()`.

## Where the record lives

| Question | File |
|---|---|
| What is already done, and why it was done that way | `DONE.md` (Phases 1-6, item by item) |
| What is next | `ROADMAP.md` (Phase 7 onward; also a redirect stub for older citations) |
| Known defects and open maintainer decisions | `docs/architecture-review-2026-08-14.md` |
| What a non-English fork can and can't do | `docs/EXTENSIBILITY.md` §2.x |
| Design spikes, including rejected options | `docs/superpowers/specs/` |

Citations of the form "ROADMAP.md Phase N item M" for N ≤ 6 point through the redirect stub to
`DONE.md` under the same phase and item number.

**Read `docs/architecture-review-2026-08-14.md` before "fixing"** the `CHANGELOG.md` v1.3.0 vs.
manifest 0.2.1 version mismatch, the absent `exclude` key, or the ~60 stale-looking ROADMAP
citations in the falsifier crates. Each is a decision already taken, not an oversight.

## Task-specific rules — read the matching file before you start

| Read this | Before |
|---|---|
| `.claude/rules/crate-layout.md` | adding a crate, moving code between crates, adding a build script, or "deduplicating" anything |
| `.claude/rules/placeholder-grammar.md` | touching `ph_ext`, `PH_EXT`, `PH_START`, `parse_str_params`, or any `say!()` compile error |
| `.claude/rules/extension-hooks.md` | adding, renaming or re-signing any `_custom` / `_with_context` hook, or touching `Many`/`Maybe`/`Box` delegation |
| `.claude/rules/pluralization.md` | changing `src/language/plurals.rs`, `data/irregular_plurals.txt`, `inflect_noun_regular`, or the `singular_end`/`plural_end` attributes |
| `.claude/rules/heed-input-parsing.md` | touching `heed!()`, `#[derive(Heed)]`, `ask!()` or their shared template compiler |

## Invariants that hold everywhere

- **The falsifier contract is absolute.** `ranting_i18n` (German), `ranting_es` (Spanish),
  `ranting_ar` (Arabic) and `ranting_ja` (Japanese) are downstream consumers that depend on
  `ranting` **alone**, exactly as an ecosystem fork would, to falsify the claim that the public
  API gives a non-English implementation enough signal. None may ever gain a `ranting_core` or
  `ranting_derive` dependency — the moment one needs it, *that is the finding*, and it gets
  recorded as a hole rather than worked around. (`ranting_gaps` does
  depend on `ranting_core`; it is a dev tool inspecting `ranting` from outside, not a falsifier, and
  is not precedent.)
- **English output stays byte-identical.** Every cross-language feature so far has been added by
  giving a hook a new parameter or a new default that reproduces today's English behavior exactly.
  A change that alters `say!()`'s existing output is a breaking change and needs saying out loud.
- **Word order is a permanent boundary, not a gap.** `ranting` inflects words within a template;
  their order is the template's, and the template is the caller's. No hook will ever change that.
- **Generated tables are never hand-edited.** `data/*.txt` are the sources of truth; three separate
  build scripts generate from them.
- **`failures/` is generated** by `ranting-gaps`, never hand-edited.
- **Some duplication is deliberate** — `ranting_gaps/src/english.rs` vs. `src/language/plurals.rs`,
  and `PH_EXT` vs. `ph_ext`, are differential oracles. Merging either makes the check agree by
  construction and report nothing forever. Both carry notes; see `crate-layout.md`.

## Testing conventions

- **Integration tests, via macros, in `tests/ranting/`.** Unit tests are sparse by design. A new
  test module must be registered in `tests/ranting/main.rs`.
- **Doctests for the proc-macro crate go in `ranting/src/lib.rs`** — `ranting_derive/src/lib.rs`
  can't easily run proc-macro examples.
- **There is no compile-fail harness** (no `trybuild`), so diagnostics are verified by compiling a
  scratch crate against a path dependency, and the pinned tests exercise the guard function
  directly rather than the rendered message.

## When adding grammar rules

1. **New irregular verb**: edit `data/irregular_verbs.txt` (`base|past|participle`). The build
   script regenerates the table.
2. **Runtime inflection logic** (`detect_tense` in `src/language/verb.rs`, pronoun/article rules in
   `src/language/english.rs`): edit under `src/language/`. `cargo test --features debug` shows the
   placeholder transforms.
3. **Compile-time inflection logic** (`handle_param`): edit `ranting_derive/src/lib.rs`.
4. Add integration tests in `tests/ranting/` demonstrating the new inflection.

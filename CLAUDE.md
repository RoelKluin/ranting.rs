# CLAUDE.md

See @README.md for project overview and API documentation on docs.rs.

## Commands

```bash
cargo test                                  # Run all tests (integration + doctests)
cargo test --test main                      # Run specific integration test
cargo test --doc                            # Run doctests only
cargo test --features debug                 # Show compile-time placeholder transforms
cargo clippy --fix
cargo fmt
```

## Architecture: Two Crates + Shared Code

**`ranting`** (main lib): Exports `Ranting` trait, `Noun` struct, `say!()` macro.  
**`ranting_derive`** (proc-macro): Implements `say!()`, `ack!()`, `nay!()`, `ask!()` macros.

**Key constraint**: `src/language/english_shared.rs` (repo root) is the single canonical source for grammar code shared by both crates. `ranting_derive/src/language/english_shared.rs` is a thin `include!(concat!(env!("OUT_DIR"), "/english_shared_generated.rs"))` wrapper; `ranting_derive/build.rs` copies the canonical file into `OUT_DIR` at build time (dev builds read `../src/language/english_shared.rs` directly; packaged builds read the dereferenced `ranting_derive/data/english_shared_source.rs` symlink — same fallback pattern as the verb-table codegen below). **Only ever edit the repo-root copy** — the derive crate's copy is generated and must not be hand-edited. Because one file compiles under both `ranting`'s `strum 0.27` and `ranting_derive`'s `strum 0.24`, any new derive or `#[strum(...)]` attribute added there must stay valid across both major versions.

**Verb conjugation codegen** (v1.1): `src/language/verb_conjugate.rs` (repo root) is the single canonical source for `to_past`/`to_continuous`/`to_future`/`to_past_participle`, used both by `ranting_derive` (compile-time `say!()` literal baking) and by `ranting` itself (runtime `say_with!()` tense resolution — a proc-macro crate can only export `#[proc_macro]` items, so `ranting` cannot call into `ranting_derive` at runtime; the conjugation engine had to become canonical in `ranting`, with `ranting_derive` consuming the generated copy, i.e. the *inverse* direction of the `english_shared.rs` pattern). Same build.rs-copy-into-`OUT_DIR` mechanism, with `ranting_derive/data/verb_conjugate_source.rs` as the packaged-build symlink fallback. **Only ever edit the repo-root copy.**

**Verb table codegen** (v1.0): The `IRREGULAR_PAST` table (118 entries, base→past) is generated at build time from `data/irregular_verbs.txt` (single source of truth). Both crates' build.rs scripts now emit `IRREGULAR_PAST` and `IRREGULAR_PAST_PARTICIPLE` to `$OUT_DIR/irregular_verbs_generated.rs` (the `ranting` crate started needing participles too, for `verb_conjugate::to_past_participle` at runtime). A symlink at `ranting_derive/data/irregular_verbs.txt` → `../../data/irregular_verbs.txt` enables independent packaging while maintaining single-source truth. **Do not manually edit the verb tables in `src/` or `ranting_derive/src/language/verb.rs`** — regenerate by editing `data/irregular_verbs.txt` and rebuilding.

**Macro flow**: Derive crate parses placeholders at compile-time, generates `format!()` calls. Runtime: `handle_placeholder()` applies final inflections via `Ranting` trait. `say_with!()` (v1.1) takes a `NarrationContext` as its first argument and calls `handle_placeholder_with_context()` instead — placeholders with tense markers (`<`,`=`,`>`,`<=`,`%`,`<%`) bake the *uninflected base verb* rather than a compile-time-conjugated form, so the marker can be overridden at runtime by `context.tense`. `say!()`'s codegen and output are untouched by this — it still bakes fully-conjugated literals and calls the original `handle_placeholder()`.

## Non-obvious behaviors

- **Positional arguments only** — `say!("{=x}", x)` works, named args do not yet.
- **Empty placeholders don't work** — `{}` is skipped; must name the variable.
- **Shared inflection code**: `english_shared.rs` is generated for `ranting_derive` from the repo-root `src/language/english_shared.rs` at build time (see "Key constraint" above) — edit only the repo-root copy; no manual sync needed.
- **Verb table codegen**: `data/irregular_verbs.txt` is the single source of truth. Changing it triggers rebuilds via `cargo:rerun-if-changed`. Do not edit `IRREGULAR_PAST` or `IRREGULAR_PAST_PARTICIPLE` in the Rust source files — they are generated.
- **Runtime tense selection (`say_with!()`)**: without a `NarrationContext.tense` override, `say_with!()` reproduces `say!()`'s output exactly for the same placeholder (falls back to the placeholder's own marker).
- **Runtime viewpoint selection (`say_with!()`)**: `NarrationContext.narration_person` (`Person::First/Second/Third`) overrides which pronoun set and verb agreement render — but only for nouns declared first-person (`subject` is exactly `"I"` or `"we"`). Nouns declared `you`/`he`/`she`/`it`/`they`/etc. are never in scope, so a first-person narrator can be retold in third person while other characters in the same placeholder call keep their own declared pronouns unchanged (see `narration::resolve_viewpoint` in `src/narration.rs`). Third-person rendering always falls back to singular "they" — there's no gender data on a first-person-declared noun to render a gendered pronoun instead; a noun that wants gendered third-person output should declare that `subject` directly rather than relying on this override. Note also that `we` overridden to `Person::Second` renders "you" the same as `I` would — the original number can't be recovered from `Person::Second` alone, so this is a one-way rendering, not a round-trip.
- **Doctests in proc-macro crate**: `ranting_derive/src/lib.rs` can't run proc-macro examples easily; test in `ranting/src/lib.rs` instead.
- **Integration tests only**: Unit tests are sparse by design; test via macros in `tests/ranting/`.

## When adding grammar rules

1. **For new verb conjugation tables**: Edit `data/irregular_verbs.txt` (one line per verb: `base|past|participle`). The build scripts will regenerate the Rust tables at build time.
2. **For runtime inflection logic** (e.g., `detect_tense`): Edit `src/language/english.rs`. Test with `cargo test --features debug` to see placeholder transforms.
3. **For compile-time inflection logic** (e.g., `handle_param` in the derive macro): Edit `ranting_derive/src/lib.rs`. Both crates' code changes are independent after codegen separation.
4. Add doctests or integration tests demonstrating the new inflection in `tests/ranting/`.

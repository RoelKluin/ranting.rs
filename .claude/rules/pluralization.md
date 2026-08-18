# Noun pluralization: the rules, the table, and the `plural_end` contract

Read before changing `src/language/plurals.rs`, `data/irregular_plurals.txt`,
`inflect_noun_regular`, or the `singular_end`/`plural_end` attributes.

ROADMAP.md Phase 7 item 10, 2026-08-14. Until this landed there were *no* regular rules —
`Ranting::inflect()` looked the noun up in `IRREGULAR_PLURALS` and otherwise appended `plural_end`
(default `"s"`) verbatim, so `{+entity}` rendered `"entitys"` and `{+box}` rendered `"boxs"`.
`src/language/plurals.rs` now has `regular_plural`/`compound_plural`, reached through the public
`ranting::inflect_noun_regular`, which is what the four derive-generated `inflect()` fallback sites
in `ranting_derive/src/ranting_impl.rs::get_plurality_fns` call.

1. **Which path runs is decided by whether the `singular_end`/`plural_end` attributes were
   *written*, and that is the compatibility contract.** Absent, the English orthographic rules
   apply; a struct that *sets* either one has declared its own rule and still gets the literal
   strip-and-append — that is what stops a non-English impl using `plural_end` as an escape hatch
   from silently acquiring English spelling. They arrive as runtime values (not resolved in the
   macro) because `= "$"` reads them from the struct's own fields.

   **Written, not valued.** The first cut tested `singular_end.is_empty() && plural_end == "s"`,
   which made `#[ranting(plural_end = "s")]` — literal append-`s`, no orthography, exactly what a
   German/Dutch/Danish *loanword* plural needs (`Partys`, `Babys`) — indistinguishable from the
   default, so it silently got the English rules and there was no opt-out short of a decoy
   `singular_end`. Consonant + `y` is where the two paths diverge, and the one class the rules made
   *previously-correct* output wrong for. So `ranting_derive`'s `RantingOptions.singular_end`/
   `plural_end` are `Option<String>` and `inflect_noun_regular` takes `Option<&str>`, defaulting to
   `""`/`"s"` only inside the literal path. `= "$"` reads the field through the public
   `ranting::DeclaredEnding` trait, which is what lets `String` (the documented field shape, always
   "declared") and `Option<String>` (additionally able to say "unset" at runtime) both work —
   `Noun` uses the latter for `with_plural_end`/`with_singular_end`, since it has no attributes to
   declare and would otherwise be the one `Ranting` impl in the crate with no way out of English
   spelling.
2. The rules are **orthographic only**, a function of spelling alone, so they need no lexicon;
   `hero`/`piano`, Latin `-us` vs. `bus`, and `quiz`→`quizzes` (consonant doubling conditioned by
   stress, not letters) stay `data/irregular_plurals.txt` rows. **The split defines what that table
   is for.**
3. The `-f`/`-fe`→`-ves` stem lists look redundant with the table but aren't identically so. The
   `-fe` stems (`knife`/`wife`/`life`) and some `-f` stems (`leaf`/`loaf`/`wolf`/`thief`, plus
   `elf`) are already rows, so for *those* words the rule only ever fires for **compounds** the
   table's exact-match lookup misses (`bookshelf`→`bookshelves`). But `calf`/`half`/`shelf`/`self`
   have no row — the rule fires on the bare word too, not just compounds built on it. Their order
   matters — `shelf` before `self` before `elf`, each being a suffix of the previous.
4. **Singularization is deliberately unchanged** and still strips `plural_end`: every inverse rule
   has a counterexample class spelling can't separate from its positive class (`-ies`→`-y` fixes
   `cities` but breaks `movies`→`"movy"`, which the naive `-s` strip gets right; a `-ves`→`-f`
   suffix rule turns `olives` into `"olife"`), so `{-cities}` still renders `"citie"`. Pinned by
   `plurals::tests::singularization_is_deliberately_unchanged`; reopening it needs a lexicon, not a
   rule.
5. The rules run **wholly on a lowercased copy** and restore the caller's case afterwards, via the
   same `apply_case` the irregular path uses — except when the rule merely appends *and the name is
   not all-caps*, where the name is left as written so interior capitals survive (`iPhone` →
   `iPhones`, which `apply_case` would lowercase). The `!is_all_caps` half of that gate is what
   keeps `BOX` → `BOXES` rather than `BOXes`; both halves are pinned in `plurals.rs`'s own tests. Matching on the lowercased form and slicing the *original* is not equivalent and was
   the first cut's bug: it rendered `CITY` as `"CITIes"` and panicked outright when lowercasing
   changed the byte length (`\u{212A}nife`, Kelvin sign → `knife`).
   `tests/ranting/property_based.rs::prop_inflect_no_panic` guards the panic class.
6. The spelling-only scope has a **concrete cost in `data/irregular_plurals.txt`**: the
   `-ch`-pronounced-/k/ words (`stomach`, `epoch`, `monarch`, …) needed rows, since the sibilant
   rule would otherwise say `"stomaches"` — before the rules landed they were right by accident, via
   bare append-`s`. `bus` is there for the same kind of reason on the singular side. **Adding a rule
   to the engine means auditing what it now gets wrong.**
7. **`ranting_gaps/src/english.rs` keeps its own copy of these rules on purpose** — it is the
   differential oracle the `regular-plural-rules`/`compound-head-plural` probes compare `ranting`
   against, so routing it through `ranting::inflect_noun_regular` would make them agree by
   construction and report zero findings forever. This is the `PH_EXT`-versus-`ph_ext` arrangement,
   not hand-kept duplication; both files carry the note.

Tests: `tests/ranting/regular_plurals.rs`. Fork-facing docs: `docs/EXTENSIBILITY.md` §2.15.

**Blind spot worth knowing** (`docs/architecture-review-2026-08-14.md` §4.7): both falsifiers
hand-write `inflect()`, so *nothing in this repo exercises the derive-generated `inflect()`
fallback against non-English input* — the path a fork gets by default. Six green gates exclude it,
which is why point 1's defect survived them.

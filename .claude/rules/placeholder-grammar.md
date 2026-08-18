# Placeholder grammar: `ph_ext`, `PH_EXT`, `PH_START`, and macro diagnostics

Read before touching `ranting_core/src/ph_ext.rs`, `PH_EXT`/`PH_START` in
`ranting_core/src/grammar.rs`, `ranting_derive`'s `parse_str_params`, or any `say!()` compile error.

## A template may write its own language's article keyword (2026-08-14)

`docs/superpowers/specs/2026-08-14-language-modularity.md` is the spec. `` {el *=gato} `` renders
`"el gato"` and `` {el +*=gato} `` renders `"los gatos"` — the *module* picks the form, so the
article agrees. `ranting` still knows no non-English vocabulary; the word list lives in the fork's
`inflect_article_custom`. Six things are non-obvious.

1. **`ph_ext::parse` runs two passes**: `parse_pass(s, PreWords::English)` with the closed
   vocabulary the grammar has always had, then `parse_pass(s, PreWords::Open)` **only if that
   fails**, returning the strict error on double failure. That ordering is what makes every
   existing template byte-identical, and it is not optional — an open pre-word slot competing at
   the same priority makes `` {w is} `` parse as `pre="w "`, `noun="is"`, which broke 15 English
   call sites when tried.
2. `PreWords` is threaded through `star_candidates`'s closure into `pre_one_rep`, deliberately
   *not* a thread-local: this parser is differentially fuzzed and must not have invisible mode.
3. **`PH_EXT` was not widened to match, and cannot be.** A single regex has one preference order,
   so it cannot express "prefer the English reading, fall back to the open one" — widening its
   alternation would accept the same language but disagree on captures, which is what the
   differential test checks. `assert_parity` therefore compares `PH_EXT` against `parse_pass(..,
   English)`, not against `parse`; `PH_EXT` remains the exact reference grammar for the English
   pass, and the open pass is pinned by `ph_ext::tests::open_pass_only_for_input_english_rejects`.
4. **`get_article_or_so`'s `ArticleKind::Other` arm calls `inflect_article_custom_with_context`**
   instead of returning `None`. Both halves are required: with the grammar widened but `Other`
   still returning `None`, the native word renders as *inert literal text* and `` {el +*=gato} ``
   gives `"el gatos"` — a compile error traded for silently wrong output. English impls return
   `None` by default, which is what leaves the word as written.
5. **The noun needs a case marker.** The open pass runs only when the English pass *fails*, and
   `` {la casa} `` does not fail — it parses as `noun="la"`, `post=" casa"`, the ordinary noun +
   post-noun-verb reading. So `` {el gato} `` is unchanged and still errors with `E0425: cannot
   find value 'el'`; only `` {el *=gato} ``-shaped placeholders reach the open pass. That follows
   from the priority ordering and cannot be had alongside English byte-identity.
6. **The open pass allows `pre` exactly one repetition**, enforced in `parse_pass` rather than in
   the matcher — `pre` is a *repeated* group whose capture keeps only the last repetition, so
   `` {de the *=gato} `` otherwise matched as two reps (`de `, `the `), retained only `the `, and
   silently **dropped** `de` (rendering `"El gato"`). Restricting the open *matcher* to one word
   does not fix this; the repetition happens a level up in `star_candidates`. General lesson for
   this parser: **a new alternative in a repeated group is not local to that alternative.**

Both falsifiers use the feature, so it is exercised by real forks and not only by a synthetic
fixture. `heed!()`/`ask!()`/`#[derive(Heed)]` are **unaffected** — `ranting_derive/src/heed.rs`
never references `ph_ext`. See `tests/ranting/native_article_keyword.rs`.

## `check_ident_path`, `path_from`'s span, and why `{el gato}`'s message is permanent

ROADMAP.md Phase 7 item 8. A placeholder's noun is baked as a `syn::Ident`, and two separate things
were wrong with that.

1. **`syn::Ident::new` panics on a non-identifier**, and `ph_ext`'s word matcher admits `-` and
   `'`, so `` say!("X {gato-negro}.") `` parsed fine and then surfaced as a bare `error: proc macro
   panicked`. `check_ident_path` now guards it and returns an `Err` through the existing
   `StrLitSlice::error` path. The guard must use `syn::Ident::parse_any`, **not**
   `syn::parse_str::<syn::Ident>`: `Ident::new`'s rule is "a keyword *or* a legal variable name",
   and `` {self} ``/`` {=self do} `` are live syntax throughout
   `tests/ranting/male_female_and_object.rs` — the strict predicate broke five call sites.
2. **`path_from` takes the template literal's span** instead of `Span::call_site()`, which moves
   rustc's `E0425` caret onto the literal and drops the `= note: this error originates in the macro
   \`say\`` line.

That is the *only* improvement available for `E0425`: the message is rustc's and cannot be
intercepted by a proc macro, and the macro cannot reject the template instead, because
`` {el gato} `` and `` {person walk} `` are the same shape — which is which is name-resolution
knowledge that arrives strictly after expansion. Narrowing the span to the individual word needs
`proc_macro2::Literal::subspan`, which is nightly-only and returns `None` on stable (confirmed on
1.97.1) — that is also why `ph_ext`'s parse errors print the "At `<template>`" + squiggle fallback
rather than a real underline. **Do not re-open this as a fixable diagnostic**; the full reasoning is
in `docs/superpowers/specs/2026-08-14-language-modularity.md`'s "The `{el 0}` diagnostic, as far as
it goes" appendix.

This repo has **no compile-fail harness** — no `trybuild` — so both diagnostics were verified by
compiling a scratch crate against a path dependency, and the pinned tests exercise
`check_ident_path` directly rather than the rendered output.

## Sentence detection beyond Latin punctuation

ROADMAP.md Phase 6 item 17. `PH_START` recognizes ASCII `.`/`?`/`!`, Greek's question mark
(U+037E), the CJK full-width terminators (`。`/`！`/`？`, which take no following space), Urdu's full
stop (U+06D4), and Spanish's opening `¿`/`¡` (which mark sentence-initial from *before* the
placeholder). ASCII/Greek/Urdu terminators require `\s+` after them; CJK needs none; Spanish's
opening marks take `\s*+`.

`SENTENCE_TRIGGER_CHARS` lists the same characters for `ranting_derive`, which has **two** readers
of it: `at_sentence_start` and the `preposition` filter. **The two are not structurally coupled** — `PH_START` is a `concat!` of string
literals with the trigger characters hard-coded in its own character class, and a `concat!` cannot
interpolate a `&[char]` const. Changing one means changing the other. See
`tests/ranting/sentence_detection.rs`.

## The fused `*=`/`*@`/`` *` ``/`*~`/`*%` markers

ROADMAP.md Phase 6 item 19. `PH_EXT`'s `case` capture tries the fused two-character form first
(`` \*[`=@~%] ``), falling back to the single-character class; `ph_ext::case_one_rep` mirrors it by
hand. `` *? `` and `**` are deliberately not accepted **as fused case markers** (`?` already means
hidden; a second `*` has no defined meaning) — but they are not rejected outright: each parses as
two ordinary single-character reps, so `` {*?who} `` and `` {**who} `` compile and render. See
`extension-hooks.md` for what the fused markers *do*.

## Where the rest lives

- **Auxiliary verbs** (`>` future/`will`, `=` continuous/`is`-`are`, `<=` `have`-`has`, `%`
  `was`-`were`, `<%` `had`): `src/language/auxiliary.rs`'s `AuxiliaryVerb`/`conjugate_auxiliary`,
  called from `handle_placeholder` in `src/lib.rs`.
- **Empty placeholders don't work** — `{}` is skipped; you must name the variable.
- **Named arguments work** — `say!("{=x}", x)` and `say!("{=x}", x = val)` both do; see
  `tests/ranting/argument_edge_cases.rs`.

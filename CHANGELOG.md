# Changelog

## Unreleased

## v2.0.0 — English Grammar Depth

Phase 8's goal was the reverse of Phases 6-7's: those asked whether the hook surface carries
enough signal for a non-English implementation; this one asks whether the English the crate
ships can carry a complex sentence. A grammarian's end-to-end review of the placeholder surface
found five missing channels and seven defects; this release lands the participle channel and
passive voice, a subjunctive/verbatim escape hatch, agreeing quantifiers and the mass/count
distinction, ordinal numerals, and all seven defect fixes. The `NumeralStyle` enum gained two
variants (`OrdinalWords`/`OrdinalDigits`), which is semver-breaking for any downstream exhaustive
match on it — the reason this release is a major version bump rather than a minor one.

### Deprecated

- **`uc_1st_if` is renamed `capitalize_if`.** The old name broke Rust naming
  conventions twice over — an unexplained `uc` abbreviation and a numeral inside
  the identifier — and did not share a vocabulary with the
  `Ranting::capitalize` hook it is the default for. `uc_1st_if` remains as a
  `#[deprecated]` alias that forwards to the new name, so nothing downstream
  breaks; it will be removed in 2.0. Every call site in this repo, the four
  falsifier crates included, and `docs/EXTENSIBILITY.md` now use
  `capitalize_if`. Behavior is identical.

### Changed

- **Five new composed post-noun tense markers add passive voice, future perfect and perfect
  progressive: `=%` (present passive), `<=%` (past passive), `>%` (future perfect), `%=`
  (present perfect progressive), `<%=` (past perfect progressive).** `say!("{The sword =%take}.")`
  now renders "The sword is taken." — previously a compile error ("unrecognized tense marker"),
  since the only markers reaching `verb_conjugate::to_past_participle` were `%`/`<%`. Closes
  ROADMAP.md Phase 8 item 1 (`docs/superpowers/specs/2026-08-15-participle-channel.md`). Each
  spelling composes already-taken `post` characters — no grammar/parser change, and every
  existing template is byte-identical, since all five were compile errors before this change.
  Baked as five new `ranting_core::placeholder::TenseMarker` variants; auxiliary agreement reuses
  `AuxiliaryVerb::IsAre`/`WasWere`/`HaveHas` unchanged. Under `say_with!()`, a `NarrationContext`
  `tense` override on one of these five moves only the tense axis (present/past/future) — the
  marker's voice/aspect is preserved, so `{=%take}` overridden to `Tense::Past` renders "was
  taken", never active "took"; the six pre-existing markers keep their unchanged full-table
  override behavior.

- **A new post-noun marker, `;`, renders a verb exactly as written, bypassing person/number
  agreement entirely.** `say!("If {=i were} rich, …")` still renders "If I was rich, …" —
  unchanged, since indicative `were` → `was` agreement is correct and stays pinned — but a
  caller who wants the subjunctive can now write `say!("If {=i ;were} rich, …")` to get
  "If I were rich, …". Closes `docs/architecture-review-2026-08-15.md` §1.5 (ROADMAP.md Phase 8
  item 2): mood is a property of the surrounding clause, not recoverable from the verb, so the
  fix is an escape hatch rather than a smarter conjugator. Baked as a new
  `ranting_core::placeholder::PostSpec::Verbatim` variant; `Ranting::inflect_verb_custom_with_context`
  is never called for it. `;` is new grammar (parsed by both `PH_EXT` and `ph_ext::parse`, in
  lockstep — `.claude/rules/placeholder-grammar.md`) but every existing template is
  byte-identical, since it was previously a syntax error to write `;` inside a placeholder.
  Combining `;` with a tense or degree marker, or repeating it, is a compile error.

- **A bare `-ing` form in an unmarked verb slot is now a compile error instead
  of rendering ungrammatical text.** `say!("{=0 walking}")` used to compile and
  render `"She walking"` — the runtime correctly leaves a non-present form
  untouched, but nothing ever supplies the auxiliary, so the writer error
  failed silently into user-visible output
  (`docs/architecture-review-2026-08-15.md` §1.8, ROADMAP.md Phase 8 item 6).
  The macro now rejects the template with a message naming both intended
  spellings: `{=0 walk}` for the present, `{=0 =walk}` for the progressive.
  No grammatical template changes meaning or output — this only turns a
  previously-accepted malformed template into an error at compile time.
  - Bare *past* forms are untouched: `{=0 walked}` and `{=0 went}` render
    grammatically without an auxiliary and stay pinned as intended output in
    `tests/ranting/verb_tense.rs`.
  - Base verbs that merely end in "ing" (`{0 sing}`, `{0 bring}`, `{0 ping}`,
    `{0 cling}`) still compile — the guard checks irregular-table bases and
    stem shape, not the raw suffix. Live in `ranting_ar`/`ranting_ja`, which
    both use `{0 sing}`.
  - Pinned by unit tests on `check_unmarked_verb_slot` in
    `ranting_derive/src/lib.rs` (this repo has no trybuild harness — same
    arrangement as `check_ident_path`).

### Changed (breaking)

- **`pre`'s closed article vocabulary gains ten more reserved words: `no`, `every`, `all`,
  `each`, `either`, `neither`, `much`, `many`, `less`, `fewer`** (ROADMAP.md Phase 8 item 3,
  `docs/superpowers/specs/2026-08-15-quantifier-determiners.md`). Six new `ArticleKind`
  variants — `No`, `EveryAll`, `Each`, `EitherNeither`, `MuchMany`, `LessFewer` — reach
  `inflect_article_custom_with_context` through new `get_article_or_so` arms carrying the
  identical `GrammaticalCase`/`NounClass`/`count`/`uc`/`ctx` signal set the pre-existing article
  arms already pass, so a fork overrides a quantifier with zero new hook surface. **This is the
  same reservation class `the`/`some`/the fourteen modal words already occupy**: a placeholder
  whose only variable named one of these ten words (`{no ...}` reading `no` as a noun) now
  reparses as the quantifier instead — the same hazard a variable named `some` or `the` already
  carries, not a new kind of one.
  - `` {no item} ``-shaped templates that used to be `E0425` (no vocabulary slot for `no`) now
    compile. `` {no +item} ``/`` {no $n item} `` (the pre-existing open-pass accident) render
    differently: plural spellings are byte-identical ("No items"), but the singular spelling
    changes from **"Noes 1 item"** to **"No 1 item"** — a defect fix
    (`docs/superpowers/specs/2026-08-15-quantifier-determiners.md` recorded the "no" falling
    through to the pre-noun *verb* path and getting conjugated), but formally an output change
    for input that already compiled, so it is called out here rather than folded into
    byte-identity.
  - Resolving the word list's cut line (left open by the spike): ship exactly the six named
    pairs now; `both`, `all`-as-a-keyword-beyond-`every`'s-pair, `such`, `enough`, `several`,
    `most` and `any` stay out — `any` in particular interacts with polarity and needs its own
    look, per the spike's own recommendation.
  - `much`/`many` and `less`/`fewer` select on `Ranting::is_mass()` (see `### Added` below), not
    on number agreement — landing them required the mass/count flag to exist first, since the
    only available proxy before it (`as_pl`) guesses wrong on exactly the nouns these words exist
    for.
  - `each`/`either`/`neither` force singular agreement, baked at compile time by
    `ranting_derive`'s `article_kind_tokens` exactly as a written `-` marker would be. A written
    `+` directly contradicts that and is now a **compile error** naming the quantifier
    (`` {each +item} ``) — the repo's "don't silently guess" stance, the same one a doubled `;`
    verbatim marker or a tense/degree conflict already takes. A `#`/`$`-numeral's own plurality is
    left untouched by this bake: the runtime count decides agreement there, and there is no
    *static* contradiction for the macro to catch.
  - `` {are no ?$n item} `` (the zero-count idiom already documented in ROADMAP.md item 3, and the
    idiom-spelling correction the spike made: `` {?#n +items} `` never parsed) keeps rendering
    "There are no items."/"There is no item." exactly as before — but only because
    `ranting_core::ph_ext`'s `match_nested_article_candidates` (the modal's "nested article"
    matcher, already used for `` {do the thing} ``-shaped chains) gained the same ten words.
    Without that, reserving `no` as an independent top-level `pre` atom made `star_candidates`'
    greedy "more repetitions first" search prefer a second, competing repetition starting at
    `no` over the correct single-repetition "are no " capture — silently dropping "are"/"is"
    from the output. Exactly the "a new alternative in a repeated group is not local to that
    alternative" trap `.claude/rules/placeholder-grammar.md` already names for the
    language-modularity change; caught here before release, not after.

- **Two new numeral markers, `##var` (spelled ordinal, "third") and `$$var` (digit ordinal,
  "3rd"), and `NumeralStyle`/`NumeralKind` each gain two new variants, `Ordinal`/`OrdinalDigits`,
  for them.** `say!("This is {the ##n attempt}.", 3, attempt)` now renders "This is the third
  attempt." — previously a compile error, since `match_nr` required a word character directly
  after `#`/`$` and rejected a second one (ROADMAP.md Phase 8 item 4,
  `docs/superpowers/specs/2026-08-15-ordinal-numerals.md`). Every existing template is
  byte-identical: `##`/`$$` could not parse before this change. **`NumeralStyle` is public,
  re-exported, and not `#[non_exhaustive]`, so this is a semver-major break** — every downstream
  `match style { ... }` on it, with no wildcard arm, now needs the two new arms; all four
  falsifier crates in this repo (`ranting_i18n`, `ranting_es`, `ranting_ar`, `ranting_ja`) needed
  exactly that fix, which is what the falsifier contract is for.
  - Agreement decouples from the ordinal itself: an ordinal says *which* one, not *how many*, so
    `as_pl` falls through to `noun.is_plural()` — "the third attempt", never "attempts", no
    matter how large the count. `placeholder_count` still carries the real value through to
    `inflect_numeral_custom`, which is what gender-agreeing ordinals (Spanish, Arabic) need.
  - `ranting_core::placeholder::PlaceholderSpec::plurality` is retyped from `&'static str` to a
    new `Plurality` enum in the same change, closing two sites the design spike found that failed
    *silently* rather than refusing to compile (a `contains('#')` check that could not tell `##`
    from `#`, and an exact `== "#"` check that could not tell "cardinal" from "no marker at all").
  - `##`/`$$` inherit the ordinal speller's English rules verbatim: suppletive `one`/`two`/`three`
    → `first`/`second`/`third`, stem-change `five`/`eight`/`nine`/`twelve`, `-y` → `-ieth`,
    otherwise `+th`; the digit suffix is chosen from the last *two* digits, so 11-13 (and
    111-113, ...) take `"th"` regardless of the last digit alone. Both inherit
    `english_numbers`' unhyphenated compound spelling verbatim ("twentyone", not "twenty-one").
  - `nr`'s alternation gained a one-repetition restriction in the same change, mirroring the open
    `pre` pass's existing one — `ph_ext::parse`'s generic repeated-group engine otherwise allows a
    second numeral-shaped run to silently displace the first, the same trap that already bit the
    open-pass `pre` widening.
  - `ranting_es::lexicon::ordinal`/`ranting_ar::lexicon::ordinal` spell `##var` with real gender
    agreement (Spanish additionally apocopating `primero`/`tercero` before a masculine singular
    noun) — the "second constituency" the ROADMAP item named. `ranting_i18n`/`ranting_ja` fall
    through to English for both new variants, an honest gap recorded in each README.
  - `ranting`/`ranting_core`/`ranting_derive` bumped to `2.0.0` (version-locked, per CLAUDE.md).
  - See `tests/ranting/ordinal_numerals.rs`, `ranting_es/tests/spanish.rs`, and
    `ranting_ar/tests/arabic.rs`.

- **`Ranting::inflect` takes a fifth parameter,
  `count: Option<PlaceholderCount>`** (ROADMAP.md Phase 7 item 11). Every
  hand-written `Ranting` impl must add it; derive-generated impls are
  regenerated and need no change. **Rendered output is unchanged** — nothing in
  the crate reads the new parameter, and English has no form for it to select.
  - Why: `inflect` renders *the counted noun itself* and was the one call Phase
    6 item 14 did not give a count, so a language with a third morphological
    number could agree in that number everywhere except on the noun. Arabic
    `{$n kitab}` with `n = 2` gave every agreeing hook
    `PlaceholderCount { value: 2, .. }` and gave the noun the plural `kutub`
    rather than the dual `kitābān` — output that looks grammatical and is wrong
    in one word.
  - `None` means the placeholder wrote no numeral, which is **not** the same as
    a count of one. `Many` substitutes its own length when the placeholder
    supplied none, never overriding an explicit numeral.
  - See `docs/EXTENSIBILITY.md` §2.16 and `tests/ranting/third_number.rs`.

- **A plural proper name now takes the bare possessive apostrophe instead of
  `'s`.** `say!("{the 0's} house", joneses)` on a `Noun` named `"Joneses"` with
  subject `"they"` used to render `"the Joneses's house"`; it now renders
  `"the Joneses' house"` (`docs/architecture-review-2026-08-15.md` §1.7,
  ROADMAP.md Phase 8 item 6). **This changes `say!()`'s rendered output for
  every plural proper name run through `{noun's}`/`{noun'}`** — the one case
  CLAUDE.md's byte-identity invariant requires calling out explicitly.
  - `adapt_possesive_s` used to pick `'s` for any capitalized noun regardless
    of number (`is_name`, keyed off the first character alone), which is only
    correct for a *singular* name ending in `s`. It now picks the bare
    apostrophe whenever the noun is plural — matching the same rule already
    applied to plural common nouns — and `'s` otherwise, with no name check at
    all.
  - `"Myles's"` (a singular name ending in `s`) is unaffected: singular nouns
    always took `'s` before this change, independent of `is_name`, and still
    do. Pinned by the doctest above `adapt_possesive_s` in `src/lib.rs` and by
    `tests/ranting/possessive_apostrophe.rs`.
  - See `tests/ranting/possessive_apostrophe.rs` for the plural-proper-name,
    singular-name-ending-in-`s`, and plural-common-noun cases side by side.

- **A phrasal or compound verb now takes the third-person `-s` on its head
  word, not its last word.** `say!("{=0 pick up} the sword.")` used to render
  `"He pick ups the sword."`; it now renders `"He picks up the sword."`
  (`docs/architecture-review-2026-08-15.md` §1.6, ROADMAP.md Phase 8 item 6).
  **This changes `say!()`'s rendered output for every bare-present-tense
  phrasal or compound verb placeholder** — the case CLAUDE.md's byte-identity
  invariant requires calling out explicitly.
  - The split was in `src/lib.rs`'s `PostSpec::Verb` handling, not in
    `inflect_verb` itself: it used to cut the placeholder's post-noun text at
    its *last* whitespace, push everything before that as literal text, and
    hand only the trailing word to `inflect_verb` — so which suffix branch
    fired was decided by the spelling of whatever word happened to be last,
    not by the verb: `` {=0 stick to} `` conjugated "to" on the sibilant
    branch ("stick toes") and `` {=0 get by} `` conjugated "by" on the
    consonant-`y` branch ("get bies"). It now splits at the *first*
    whitespace instead, conjugating the head word through the unchanged
    `inflect_verb`/`conjugate_verb` path and appending the remainder
    (including the separating whitespace) after the conjugated form instead
    of before it.
  - A single-word verb has no whitespace to split on, so it is byte-identical
    to before — this only changes multi-word verbs.
  - Tense-marked forms (`` {<0 pick up} `` and friends) were already correct
    and are untouched: they conjugate through
    `ranting_core::verb_conjugate`, which the macro already applies to the
    head word only.
  - See `tests/ranting/verb_tense.rs` ("pick up", "stick to", "get by", and a
    single-word control, each in first, second and third person).

- **A space-separated head-first compound noun now pluralizes on its head, not
  its tail.** `say!("{,+0}", Noun::new("attorney general", "it"))` used to
  render `"attorney generals"`, and `"court martial"` rendered
  `"court martials"` (`docs/architecture-review-2026-08-15.md` §1.10,
  ROADMAP.md Phase 8 item 6); they now render `"attorneys general"` and
  `"courts martial"`, matching the hyphenated spellings `attorney-general` and
  `court-martial`, which were already correct. **This changes `say!()`'s
  rendered output for any space-separated noun whose second word is in the
  closed `PREPOSITIONS`/`POSTPOSED_ADJECTIVES` list** in
  `src/language/plurals.rs::compound_plural` — the case CLAUDE.md's
  byte-identity invariant requires calling out explicitly.
  - `compound_plural` used to split on `-` only and return `None` for anything
    without a hyphen, so a head-first compound written with a space fell
    through to `regular_plural` and pluralized its last word like an ordinary
    noun. It now also splits on a single space, gated behind the same closed
    lists the hyphenated form already used — an ordinary modifier + head
    phrase (`"red house"` → `"red houses"`, `"post office"` →
    `"post offices"`, `"fire engine"` → `"fire engines"`) is far more common
    than a postposed-head compound written with a space, so the split only
    fires when the second word is a known preposition or postposed adjective.
  - See `tests/ranting/regular_plurals.rs` and
    `src/language/plurals.rs`'s own tests.

- **A sentence-initial numeral now takes the placeholder's capital, instead of
  the noun several words later.** `say!("{#n item} fell.", n = 2)` used to
  render `"two Items fell."`; it now renders `"Two items fell."`.
  `say!("{$n item} fell.", n = 2)` used to render `"2 Items fell."`; it now
  renders `"2 items fell."` (`docs/architecture-review-2026-08-15.md` §1.11,
  ROADMAP.md Phase 8 item 6). **This changes `say!()`'s rendered output for any
  sentence-initial `#var`/`$var` placeholder with no preceding article or
  verb** — the case CLAUDE.md's byte-identity invariant requires calling out
  explicitly. A placeholder with a preceding article (`` {the #n item} ``) or
  not at sentence start is unaffected.
  - The two channels differ, so the fix does too: a spelled `#var` is a word
    and can be capitalized (`capitalize_if`), so it now claims the capital and
    stops it reaching the noun; a digit `$var` can't be capitalized, so the
    capital is dropped outright rather than carried on.
  - `inflect_numeral_custom` still takes no `uc` parameter — capitalization
    stays entirely on the crate side, applied to whatever the hook returns (or
    to the English fallback), rather than delegated to the hook. Its own doc
    is corrected to say so; it used to cite "the crate never capitalizes the
    numeral" as the reason for the missing parameter, which was the bug
    described as policy.
  - Gated on `uc && sentence_start`, not `uc` alone, so a mid-sentence
    forced-uppercase placeholder (`` {^#n item} ``, the same shape
    `.claude/rules/extension-hooks.md` gives for `` {The 0} ``) is
    byte-identical to before.
  - `ranting_i18n`'s `spelled_numerals_agree_like_an_article_at_one` test
    pinned the identical pre-fix shape for German (`"ein Hund"`) and is
    updated to `"Ein Hund"`, since it is a property of the shared engine, not
    a per-language gap.
  - See `tests/ranting/numeral.rs`.

### Fixed

- **A negative `#var` spelled the non-word "negativeone".**
  `say!("I see {#0 boot}", -1)` rendered `"I see negativeone boots"`
  (`docs/architecture-review-2026-08-15.md` §1.9, ROADMAP.md Phase 8 item 6):
  `english_numbers::convert_no_fmt` writes a negative as one unbroken run. The
  sign is now a word of its own — `-1` is `"minus one"`, `-21` is
  `"minus twentyone"` — spelled by a guard in `ranting`'s own code rather than
  upstream. Non-negative counts are byte-identical, so this is not a breaking
  change. Three notes:
  - The magnitude keeps upstream's spelling exactly: positive 21 renders
    `"twentyone"` today, so the negative is `"minus twentyone"` and not
    `"minus twenty-one"` — hyphenating would change what non-negative counts
    render.
  - `"minus "` is part of the single numeral string handed to
    `inflect_numeral_custom`, so the hook still replaces the whole numeral
    wholesale and `NumeralSpec::leading_space` is untouched.
  - `i64::MIN` is unchanged: its magnitude is not representable and upstream
    already panicked on it.
  - Agreement is unaffected — it is decided from the count, never from the
    spelled word, so `-1` still takes the plural (`"minus one boots"`, as in
    "minus one degrees").

- **`{?$n noun}` left a stray space.** `say!("I see {?$0 boot}", 2)` rendered
  `"I see  boots"` (ROADMAP.md Phase 7 item 13). A hidden numeral sits between
  two separators — its own leading one and the noun's — and with nothing
  rendered between them the pair now collapses to one, the same way a
  zero-length article's does. The leading separator is the one kept, which is
  what leaves `{The ?$n noun}` rendering `"The raven"`. Cosmetic in English;
  it was on the critical path of `ranting_ja`'s only candidate workaround for
  the numeral separator. It had been *pinned* by `tests/ranting/numeral.rs`
  rather than flagged, so it read as intended behavior for two phases.

- **Elision panicked on a non-ASCII article.** `split_at_find_end` advanced one
  *byte* past the byte index `rfind` returned, so the post-assembly elision
  splice sliced mid-codepoint whenever the rendered article's last character was
  multibyte: `say!("{the 0}", ..)` on an entity whose `inflect_article_custom`
  returns `ال`, `этот` or `τό` and which overrides `elide_article_custom` panicked
  with *"end byte index N is not a char boundary"*. Nothing about it was
  language-specific — it was byte arithmetic — and it survived because
  `elide_article_custom` had no real-world user until `ranting_ar`, both existing
  reference lexicons' articles being ASCII. Pinned by
  `tests/ranting/property_based.rs::elision_does_not_panic_on_a_multibyte_article`.

- **`{?article noun}` rendered literal garbage** unless the entity's
  `skip_article()` was `true`. The `?` marker (README's "display depends on
  `no_article`", e.g. `say!("{?the 0} was great!", activity)`) was not stripped
  before the word was classified, so `?the` was taken for a pre-noun *verb* and
  conjugated: `say!("{?the dog}")` rendered `"?thes dog"` and `say!("{?a dog}")`
  rendered `"?as dog"`, with no error at compile time or run time. `?the` is now
  simply `the`, which is the documented reading. The `no_article = true` half —
  the only half any test or example exercised, and why this survived — is
  unchanged.

- **Regular English pluralization** (ROADMAP.md Phase 7 item 10). `{+noun}` on a
  noun absent from `data/irregular_plurals.txt` used to append the `plural_end`
  attribute (default `"s"`) verbatim — there were no rules at all — so
  `{+entity}` rendered `"entitys"`, `{+box}` rendered `"boxs"` and
  `{+mother-in-law}` rendered `"mother-in-laws"`. English's regular orthographic
  rules now apply: consonant + `y` → `ies`, `-es` after `s`/`x`/`z`/`ch`/`sh`,
  the `-f`/`-fe` → `-ves` stems (which fire for compounds like `bookshelf`, the
  bare words being table rows already), and head pluralization for hyphenated
  compounds.
  - **This changes rendered output.** A struct that declares `singular_end` or
    `plural_end` is unaffected: declaring either states a rule of its own and
    still gets the literal strip-and-append, which is what keeps a non-English
    impl from acquiring English orthography by accident. What counts is that the
    attribute was *written*, not what value it was given — `plural_end = "s"` is
    a genuine opt-out (bare append, no orthography), which is what a German,
    Dutch or Danish loanword plural needs: `Party` → `Partys`, where the rules
    say `Parties`. Names ending in a consonant + `y` are the class where the two
    paths differ, and the only class the rules made *previously-correct* output
    wrong for.
  - Singularization is deliberately unchanged — no spelling rule separates
    `cities` → `city` from `movies` → `movie`, so `{-cities}` still renders
    `"citie"`.
  - `data/irregular_plurals.txt` gained the `-ch`-as-/k/ words (`stomach`,
    `epoch`, `monarch`, …) the spelling-only rules cannot recognize, plus `bus`.

### Added

- **`Ranting::elide_numeral_custom` / `_with_context`** (ROADMAP.md Phase 7
  item 12) — an eighth `_custom` hook pair, the numeral-side twin of
  `elide_article_custom`: same post-assembly splice, same
  replace-all-three-or-decline contract. It fuses a rendered numeral with the
  noun after it, which is what Japanese 「一匹の猫」 needs — written as one run
  with no space. Until this existed the separator was pushed by
  `handle_placeholder` and offered to no hook, so `一匹の 猫` was the best a fork
  could do; unlike a missing distinction, that is a wrong character in the
  output with no workaround. Runs *before* `elide_article_custom`, since
  `[article][numeral][noun]` makes it the inner of the two boundaries, and is
  not called for a hidden numeral. English output is unchanged — the default
  returns `None`. First and only user: `ranting_ar`'s sibling `ranting_ja`.

- **`Ranting::is_mass() -> bool`, declared via `#[ranting(mass)]` or `Noun::with_mass()`**
  (ROADMAP.md Phase 8 item 3, part (b)). Defaulted `false`, so no existing type's rendering
  changes. Orthogonal to `NounClass` by design — a word can be both mass and gendered (German
  *das Wasser* is neuter and mass) — so `ranting` never folds it into the class label.
  `ranting` itself reads it in exactly two places: the `a`/`an`/`some` article slot renders the
  unstressed `some` on a mass noun's singular instead of guessing `a`/`an` from the noun's first
  letter/sound (`` {a 0} `` on "information" used to render **"An information"**; now renders
  "Some information" for any noun declaring itself mass — mass-`some` was previously unreachable
  even though the word was already in the vocabulary, since `adapt_article` discarded it in favor
  of the computed a/an), and the new `much`/`many`/`less`/`fewer` quantifier pair (see
  `### Changed (breaking)` above) picks its mass-noun member. `#[ranting(mass)]` mirrors
  `gender`'s attribute-present-or-not contract but is bare-boolean-shaped like `no_article`
  rather than string-shaped like `gender`, since there is no open-ended label to carry; a
  private `MassAttr` type in `ranting_derive` gives it both the bare-word shape (`bool`'s own
  `FromMeta`) and `gender`'s `"$"` field-read sentinel, which is what lets `Noun` (which has no
  attribute value of its own to declare) read a real `mass: bool` field via
  `#[ranting(mass = "$")]` and offer `Noun::with_mass()`. `Many`/`Maybe`/`Box`/`&dyn Trait`
  delegate it the same one-item-or-single-value rule `noun_class()` already uses.
- `ranting::inflect_noun_regular`, the public entry point derive-generated
  `inflect()` impls use once the irregular table misses. Its `singular_end`/
  `plural_end` parameters are `Option<&str>`, `None` meaning "no rule declared".
- `Noun::with_plural_end` / `Noun::with_singular_end`, chaining off
  `new`/`try_new` like `with_noun_class`. `Noun` has no `#[ranting(..)]`
  attributes to write, so these are its opt-out from the regular rules.
- `ranting::DeclaredEnding`, the trait `#[ranting(singular_end = "$")]` /
  `#[ranting(plural_end = "$")]` read their field through. A `String` field
  (the documented shape) always counts as declared; an `Option<String>` one can
  additionally say "unset" at runtime, which is how `Noun` keeps the English
  rules for every noun that never calls `with_plural_end`.

## v1.3.1 — Internationalization Foundations (republish)

Identical in content to v1.3.0 below. `1.3.0` was burned on crates.io: `ranting_core`
published successfully, but the version-locked `ranting_derive`/`ranting` publish
stalled on an unrelated crates.io ownership/token issue, and crates.io versions are
immutable once uploaded — so the rest of the locked group had to move to `1.3.1`
instead of retrying `1.3.0`.

## v1.3.0 — Internationalization Foundations

Phase 6's goal was narrow: make a non-English `Ranting` implementation
*buildable* — not build one and ship it. This release lands the signals a
fork needs and could not previously obtain (gender, grammatical case, degree,
orthographic role, numeral style, a length-derived count) as new hooks and
types on `Ranting`, defaulting everywhere to today's English behavior, and
then spends two reference lexicons — German (`ranting_i18n`) and Spanish
(`ranting_es`) — proving the set is close to sufficient. It is not a
translation system: no vocabulary, message catalogue, or word-order engine
shipped in `ranting` itself. Several Phase 6 items were doc-only design
spikes that changed no code at all (see "Deliberately not done" below) —
this release is smaller in code than the number of ROADMAP items suggests.

### Added

**Six new/extended `_custom` hook pairs on `Ranting`** (each hook has an
`_with_context` twin taking an extra `ctx: Option<&NarrationContext>`; all
default to `None`/today's English output, so no existing impl needs to
change to keep compiling and rendering identically):

- `inflect_adjective_custom`/`_with_context` — runtime adjective agreement
  for the `{noun !adj}`/`{noun !!adj}` degree slot (previously resolved
  entirely at compile time). Receives the adjective as written,
  `AdjectiveDegree`, `GrammaticalCase`, `NounClass`, `as_plural`, `count` and
  `uc`.
- `elide_article_custom`/`_with_context` — a post-assembly hook for
  phonological elision/fusion (French `le`+vowel → `l'`), receiving the
  rendered `article`, `separator` and `following` text.
- `inflect_numeral_custom`/`_with_context` — locale-aware numeral rendering
  for `#var`/`$var`, receiving `NumeralStyle` (words vs. digits), `count`,
  case, class and `as_plural`.
- `inflect_preposition_custom`/`_with_context` — fuses a literal pre-noun
  preposition with the rendered article (German `zu`+`dem`→`zum`, Spanish
  `de`+`el`→`del`), fed the preposition text the macro previously discarded.
- `inflect_article_custom`/`_with_context` and `inflect_pronoun_custom`/
  `_with_context` — both extended (not new) with a `class: NounClass`
  parameter, and later a `count` parameter (see Breaking Changes).

Plus a new fallback-taking-over hook, not part of the `_custom`/`None`
convention above since there's nothing to decline into:

- `capitalize`/`capitalize_with_context` — routes every sentence-position
  capitalization decision through an overridable hook (`OrthographyRole` +
  `uc` + `sentence_start`), instead of calling `uc_1st_if` directly at each
  call site.

**New public types**:

- `NounClass` — an open-ended lexical-gender/noun-class label (`Noun` gets
  one via `#[ranting(gender = "...")]`), read by `noun_class()` and threaded
  into the article/pronoun/adjective/preposition hooks. Not a closed
  `enum { Masculine, Feminine, Neuter }` — deliberately, since Bantu
  languages have a dozen-plus classes and Danish has common/neuter.
- `GrammaticalCase` — the noun's grammatical role at a given placeholder
  occurrence (`Name`/`Subjective`/`Objective`/`Possessive`/`Reflexive`),
  mirrored from `ranting_core::placeholder::CaseKind`. Threaded into
  `inflect_article_custom`, `elide_article_custom`, `inflect_preposition_custom`
  and (new in this release) `Ranting::inflect` itself.
- The fused `*=`/`*@`/`` *` ``/`*~`/`*%` marker forms — `*` was already a
  case-marker-position character (meaning "no case marker, but mark this as
  the placeholder's Ranting element"); fused with a real case marker it now
  case-marks the placeholder exactly like the bare marker but renders the
  noun's *name* instead of switching to a pronoun (`display_as_name: bool`
  on `PlaceholderSpec`). Lets a fork whose `inflect_pronoun_custom` always
  returns a real pronoun still get a case-correct article with the name
  shown, for the same entity, in the same sentence a bare `` {@noun} ``
  renders a real pronoun for. See `docs/EXTENSIBILITY.md` §2.11.
- `AdjectiveDegree` — mirrors `ranting_core::placeholder::DegreeKind`
  (`Comparative`/`Superlative`) for `inflect_adjective_custom`.
- `OrthographyRole` — which call site is asking `capitalize` to decide
  (`Article`/`Verb`/`Pronoun`/`Noun`/`Adjective`).
- `NumeralStyle` — `Words` (`#var`) vs. `Digits` (`$var`) for
  `inflect_numeral_custom`.
- `PlaceholderCount` — `{ value: i64, fraction_digits: u32 }`, the count
  channel threaded into five hook pairs (see Breaking Changes).

**Behavioral additions**:

- `is_first_person_subject_custom(&self, subject: &str) -> bool` —
  `narration::resolve_viewpoint`'s first-person check (previously
  hard-coded to `matches!(subject, "I" | "we")`) is now overridable, so a
  fork whose first-person labels are e.g. `ich`/`wir` can make
  `NarrationContext.narration_person` retelling work instead of silently
  no-op'ing.
- `Many<T>` now substitutes its own `Vec`'s length as the `count` for the
  five count-carrying hook pairs when the placeholder itself carried no
  numeral (`count.or_else(|| self.own_count())`) — only at exactly one item;
  an explicit placeholder numeral, and a `Many` of zero or 2+ items, are
  unaffected.
- Sentence detection widened beyond ASCII `.`/`?`/`!`: `PH_START` now also
  recognizes Greek's question mark (U+037E), Urdu's full stop (U+06D4),
  CJK full-width terminators (`。`/`！`/`？`, which take no following space),
  and Spanish's opening `¿`/`¡` (which mark sentence-initial from *before*
  the placeholder). The single source of truth for "is this character a
  sentence trigger" is the new `ranting_core::grammar::SENTENCE_TRIGGER_CHARS`.
- `say_with!()` and `derive_ranting` are now re-exported from `ranting` —
  previously a crate depending on `ranting`'s public API alone could never
  construct a call carrying a `NarrationContext`, making every
  `_with_context` hook unreachable in practice from outside this repo.

### Breaking changes

Every one of the five signature breaks below is additive in *behavior*
(English `say!()`/`say_with!()` output is byte-identical before and after —
verified by the full pre-existing test suite passing unchanged) but is a
**source break for any downstream override** of the named hook. If your
`Ranting` impl overrides any of these methods, it needs updating to match
the new parameter list before it compiles again.

**1. `class: NounClass` added to the article and pronoun hooks:**

```rust
// Before
fn inflect_article_custom(&self, article: &str, noun_singular: &str,
    case: GrammaticalCase, as_plural: bool, uc: bool) -> Option<String>
fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase,
    as_plural: bool, uc: bool) -> Option<String>

// After
fn inflect_article_custom(&self, article: &str, noun_singular: &str,
    case: GrammaticalCase, class: NounClass, as_plural: bool,
    count: Option<PlaceholderCount>, uc: bool) -> Option<String>
fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase,
    class: NounClass, as_plural: bool, count: Option<PlaceholderCount>,
    uc: bool) -> Option<String>
```

(`class` landed first; `count` landed later in the same phase — see below.
Both are shown together since anyone updating an override has to handle
both regardless of which commit they're diffing against.)

**2. `count: Option<PlaceholderCount>` added to five hook pairs, plus
`case: GrammaticalCase` added to `Ranting::inflect` — the "owed" signature
break from the number-category design spike, done once:**

```rust
// Before
fn inflect_verb_custom(&self, subject: &str, verb: &str,
    as_plural: bool, uc: bool) -> Option<String>
fn elide_article_custom(&self, article: &str, separator: &str,
    following: &str, case: GrammaticalCase, class: NounClass,
    as_plural: bool) -> Option<String>
fn inflect_adjective_custom(&self, adjective: &str, degree: AdjectiveDegree,
    case: GrammaticalCase, class: NounClass, as_plural: bool,
    uc: bool) -> Option<String>
fn inflect(&self, to_plural: bool, uc: bool) -> String

// After
fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool,
    count: Option<PlaceholderCount>, uc: bool) -> Option<String>
fn elide_article_custom(&self, article: &str, separator: &str,
    following: &str, case: GrammaticalCase, class: NounClass,
    as_plural: bool, count: Option<PlaceholderCount>) -> Option<String>
fn inflect_adjective_custom(&self, adjective: &str, degree: AdjectiveDegree,
    case: GrammaticalCase, class: NounClass, as_plural: bool,
    count: Option<PlaceholderCount>, uc: bool) -> Option<String>
fn inflect(&self, to_plural: bool, uc: bool, case: GrammaticalCase) -> String
```

(`inflect_article_custom`/`inflect_pronoun_custom` also gained `count` here,
on top of the `class` parameter shown above.) All ten affected methods
(five hooks × the plain form and its `_with_context` twin) changed in one
commit. `inflect_numeral_custom` was deliberately **not** touched by this
break — it already carries its own, differently-typed `count: Option<i64>`
from when it landed.

**3. `sentence_start: bool` added to `capitalize`:**

```rust
// Before
fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool) -> String

// After
fn capitalize(&self, word: &str, role: OrthographyRole, uc: bool,
    sentence_start: bool) -> String
```

`uc` and `sentence_start` can disagree in both directions — `uc` also folds
in the `,`/`^` markers and an uppercase pre-text word, `sentence_start` is
the raw compile-time-known position signal alone.

**Not breaking, for context**: `inflect_adjective_custom`,
`elide_article_custom`, `inflect_numeral_custom`, `inflect_preposition_custom`
and `capitalize` were all themselves *new* in this release (see Added
above) — they only became a compatibility concern for anyone who adopted
them mid-phase. `Many`'s count substitution (item 15) and the fused `*=`
markers (item 19) required no hook signature change at all.

### Deliberately not done

Several Phase 6 items concluded "change nothing, document it" after scoring
concrete alternatives — these are permanent boundaries, not gaps awaiting a
future release:

- **Word order stays in the caller's template.** `ranting` inflects words
  within a template; the order of those words is the template's. No hook
  can move text it doesn't own — German verb-second, Japanese/Korean SOV,
  VSO languages, and suffixed definite articles are all out of reach by
  construction, not by omission. See `docs/EXTENSIBILITY.md` §2.12 and
  `docs/superpowers/specs/2026-08-13-word-order-feasibility.md`.
- **`GrammaticalCase` stays at English's five-marker inventory.** It scopes
  to "which of `say!()`'s five markers did this occurrence use," not to a
  general syntactic-case representation — German's four cases cross-cut
  that split, so no re-slicing of the existing variants recovers a clean
  match, and a fork needing more (dative, genitive) carries the real case
  on the entity instead. See `docs/EXTENSIBILITY.md` §2.3.1 and
  `docs/superpowers/specs/2026-08-13-grammatical-case-inventory.md`.
- **Per-language template selection stays caller-side.** `say!()` parses
  its literal as a `syn::LitStr` at compile time, before any runtime value
  exists, so a runtime catalogue lookup is a compile error, not a slow
  path — selecting a template by language costs languages × sentences of
  source text no matter how it's spelled. See `docs/EXTENSIBILITY.md` §2.12
  and `docs/superpowers/specs/2026-08-13-template-selection.md`.
- **Whitespace stays `heed!()`'s only word boundary**, for `heed!()`,
  `ask!()` and `#[derive(Heed)]` alike, permanently — not an ASCII/Latin
  restriction (it's script-agnostic), but a template whose segments abut
  without whitespace, in any script, returns an honest `None` rather than a
  silently wrong capture. See the README's "Whitespace is the only word
  boundary" section.

### Known gaps (not this release's to close)

The `ranting_i18n` (German) and `ranting_es` (Spanish) falsifier crates each
document, in their own `README.md`, the specific constructions their
language still cannot reach through `ranting`'s public API even after this
release — including German's inability to reach `inflect_adjective_custom`
in a grammatically correct sentence at all (prenominal attributive
adjectives vs. the post-noun-only `!` slot), unreported adjective-declension
class, and (for both languages, closed together by item 26 above) what
remained of preposition-article fusion before this release landed the
`inflect_preposition_custom` hook. Read `ranting_i18n/README.md` and
`ranting_es/README.md` before assuming a construction works — this phase
closes the gaps it names, not every gap a non-English language might hit.

### See also

- `docs/EXTENSIBILITY.md` — the extension-point reference for every hook
  above, with worked examples per language.
- `docs/superpowers/specs/2026-08-13-*.md` — the design spikes behind the
  "Deliberately not done" decisions, each with its scored alternatives.
- `ROADMAP.md` Phase 6 — the full item-by-item implementation log this
  changelog summarizes for a crate user rather than a roadmap reader.

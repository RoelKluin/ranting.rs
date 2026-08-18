# The `Ranting` customization hooks: traps and invariants

`docs/EXTENSIBILITY.md` §2.x is the fork-facing reference for *what each hook is*. This file is the
part that isn't in it, or is easy to miss: why each hook has the shape it has, and what breaks if
you change it. Read before adding, renaming or re-signing any `_custom` hook.

There are **eight `_custom` hook pairs** (each hook plus a `_with_context` twin) plus
`capitalize`/`capitalize_with_context` — nine `_with_context`-suffixed methods in total.

## The `_with_context` mechanism

`NarrationContext` carries `tense`, `narration_person`, `register: Option<Register>` and
`dialect: Option<&'static str>`; **the crate never interprets `register` or `dialect`** — they are
inert unless a `Ranting` impl reads them. Every verb/pronoun/article/adjective/elision/preposition/
numeral call site calls the `_with_context` form, which takes `ctx: Option<&NarrationContext>` as a
plain parameter (`say!()` passes `None`, `say_with!()` passes `Some(ctx)`) and defaults to
delegating to the pre-existing non-context hook — so **overriding only the `_with_context` hook is
enough**. `ctx` is never sourced from `self`/`subject`: that was deliberately rejected, to keep an
entity's own `subject` separate from settings of the telling.

**`NarrationContext` is per *call*, not per story**, and calling it "story-wide" (as the docs did
until Phase 7 item 13) misleads: a different context per utterance is ordinary usage, which is what
makes `register` the right axis for Japanese keigo, where politeness varies per addressee inside
one scene. Setting it once for a story is the common case, not a constraint.

Without a `NarrationContext.tense` override, `say_with!()` reproduces `say!()`'s output exactly.

`say_with!()` and `#[derive_ranting]` are **re-exported from `ranting`** (Phase 6 item 12, closing
hole 1 of item 10): before that, a downstream crate depending on `ranting` alone could never
construct a `say_with!()` call, so every `_with_context` hook was unreachable in practice.
`tests/ranting/reexports.rs` pins it by writing only `use ranting::*;`.

## What each added signal is for, and the mirroring rule

**The rule**: a type is mirrored from `ranting_core` *iff* the corresponding thing is written in a
placeholder. `GrammaticalCase` mirrors `placeholder::CaseKind` and `AdjectiveDegree` mirrors
`placeholder::DegreeKind` (both are markers a template writes). `NounClass` and `OrthographyRole`
are defined in `ranting` alone (a noun class is carried by the entity; a call-site role is never
written anywhere). `ranting_core` types are not part of `ranting`'s public semver surface, which is
why nothing is exposed directly.

- **`GrammaticalCase`** on `inflect_article_custom` — threaded from the placeholder's own
  `CaseKind`; a bare `` {the noun} `` reports `GrammaticalCase::Name`, since English has nothing
  more specific. Added because a German-fork spike found every call looked identical, so
  `der`/`den`/`dem` was unpickable. It still only carries English's five-marker inventory, which is
  why it does **not** by itself close `ranting_i18n`'s hole 2 (dative/genitive).
- **`NounClass`** — a newtype over `&'static str`, not an `enum { Masculine, Feminine, Neuter }`,
  because Bantu has a dozen-plus classes and Danish has common/neuter; `ranting` never interprets
  the label. The `class` parameter is *redundant for a plain impl* (the hook has `self`, which has
  `noun_class()`) — but not for `Many`/`Maybe`/`Box`, where the call site reads the class off the
  *wrapper* while `self` inside the delegated-to hook is the *inner* value. `#[ranting(gender =
  "...")]` generates **no** `noun_class()` override when absent, so an unclassed impl is
  byte-identical to pre-v1.3 codegen.
- **`is_mass()`** (Phase 8 item 3) is a defaulted trait method — `false` by default, same shape as
  `noun_class()`/`skip_article()` — declared via `#[ranting(mass)]` or `Noun::with_mass()`. It is
  **not** a hook parameter: no `inflect_article_custom`/`_with_context` signature changed. Instead
  `get_article_or_so` reads `self.is_mass()` directly to pick between `a`/`an`/`some` and between
  `much`/`many`/`less`/`fewer`, then hands the *already-resolved word* to the existing hook exactly
  as it always has for `the`/`a`. So a fork gets mass-noun-aware `a`/`an`/`some`/`much`/`many`/
  `less`/`fewer` selection for free by implementing `is_mass()` alone, with no `_custom` hook
  involved — the one place in this file where `ranting` itself, not a hook, reads a customization
  point.
- **`inflect_adjective_custom`** is called only from the `PostSpec::Degree` arm, i.e. only for
  `{noun !adj}`/`{noun !!adj}` — `PostSpec::Verb` is deliberately *not* routed to it, since handing
  a real verb to an adjective hook would break English. So `!` is the only adjective channel there
  is, and a fork whose adjectives merely *agree* (French `noir`/`noire`) writes `!` for the positive
  degree and ignores `degree`; there is no `AdjectiveDegree::Positive` because no marker produces
  one. `PostSpec::Degree` carries **both** `base` and `word` (the compile-time English form) because
  the English form is not reversible back into the base; the hook gets `base`, and `word` is emitted
  only on the `None` fallback path.
- **`capitalize`** returns `String`, not `Option<String>`, and so is deliberately *not* named
  `_custom`: it is the fallback, not a chance to decline one. Its default is exactly
  `uc_1st_if(word, uc)`. Four of the five `OrthographyRole`s get an uncapitalized word and a
  truthful `uc`; **`OrthographyRole::Noun` is passed `uc: false`**, because the name has already
  been through `inflect()` — a derive-generated `name()` reads `uc == true` as "as written", not
  "force uppercase", so routing `uc` through the hook there would silently start capitalizing
  `#[ranting(name = "designer")]`. Guard:
  `tests/ranting/orthography.rs::lowercase_name_attribute_still_renders_lowercase`. The hook decides
  what is *done* with `uc`, never what `uc` *is*: sentence position and the `,`/`^` markers stay
  compile-time, and `apply_case` in `src/language/plurals.rs` is not routed through it.
- **`sentence_start: bool`** on `capitalize` is baked into `PlaceholderSpec` (unlike `uc`, which is
  not a `PlaceholderSpec` field at all — the macro passes it as a separate argument), because `uc`
  conflates "sentence-initial" with "forced uppercase by a `^`/`,` marker or an uppercase pre-text
  word": `` {The 0} `` mid-sentence has `uc == true, sentence_start == false`, and `` {,noun} ``
  right after a period has the reverse.
- **`elide_article_custom`** is the only hook that runs *after* the placeholder is assembled. It is
  a separate hook rather than a `following: &str` parameter on `inflect_article_custom` because at
  `get_article_or_so` time *the following text does not exist yet*; the post-assembly shape also
  lets a fork drop the separator, which a parameter could not. The call site records the **byte
  span** of the last article pushed into `res` and splices after the noun, so
  `get_article_or_so`'s signature is untouched and English is byte-identical by construction. It
  takes **no `uc`** — the article arrives already capitalized and `uc` has been reset by the splice
  point.
- **`elide_numeral_custom`** (Phase 7 item 12) is `elide_article_custom`'s numeral-side twin:
  same post-assembly splice, same replace-all-three contract, same no-`uc`. It runs **first of the three**
  post-assembly steps — ahead of preposition fusion and article elision — because
  `[preposition][article][numeral][noun]` makes the numeral-noun boundary the innermost: every byte
  it rewrites is at or after `article_span`'s end, so both later splices' spans stay valid and each
  sees the already-fused numeral+noun as its trailing text. Running it *second*, after fusion, is
  what `docs/architecture-review-2026-08-15.md` §1.1 records — fusion truncates and rebuilds, and
  the numeral's pre-fusion offsets then slice a displaced window. Not
  called for a hidden numeral. It exists because `ranting_ja` found the asymmetry the hard way:
  「一匹の猫」 is written as one run, the separator was pushed by `handle_placeholder` and offered
  to nothing, and unlike a missing distinction a wrong character is simply in the output. Pinned
  from both sides — `ranting_ja` uses it, and `ranting_ar/tests/arabic.rs` asserts the two splices
  do not disturb each other, which is the only place in the repo where both can fire.
- **`inflect_preposition_custom`** (item 26) closed what §2.7 used to call a permanent gap
  (`de` + `le` → `du`): `PH_START`'s `pre` capture was widened to also match a single literal word,
  baked into `PlaceholderSpec::preposition`. It runs at the same splice point as
  `elide_article_custom` and is tried **first**: `Some` consumes both and skips elision; `None`
  leaves the word exactly as written. Only fires when the preposition is directly adjacent to the
  article. Widening `PH_START` surfaced a latent quirk in this crate's `regex` version: **`X?+` is
  not a possessive single-optional the way PCRE's is, empirically behaving as `(X?)+`
  (repeatable)**, which would have chained the new word branch across every preceding word
  (`"Vengo de "` instead of `"de "`) had the outer `pre` group been left as `?+`.
- **`inflect_numeral_custom`** covers both numeral channels — `#var` (spelled out) and `$var`
  (digits) — via `NumeralStyle`. **`#var` moved from compile time to runtime**: the macro used to
  bake a finished English word, and now bakes the *count*, which is why `handle_placeholder` grew a
  `count: Option<i64>` parameter (a runtime value, so it could not go in the `Copy`
  `PlaceholderSpec`). `$var` was *not* moved: its argument need only be `Display`, so it is still
  rendered by `format!()` with the `:fmt` spec, and its count is recovered by `parse::<i64>()` on
  that string — baking `as i64` would fail to compile code that works today. The number's leading
  space lives in `NumeralSpec::leading_space` so a replacement can't eat or duplicate it, and a
  `:fmt` width spec is *not* re-applied to a hook's return value. It takes **no `uc`**: the crate
  never capitalizes a numeral.

## What `as_plural: bool` promises

Exactly one thing: *render the plural **agreement** form*, resolved per placeholder occurrence at
`src/lib.rs`'s `as_pl` match. It does **not** promise the referent count is greater than one:
`is_subjective_plural("they")` is `true` (singular *they*), an empty `Many` is plural ("there
**are** no items"), and `inflect_reflexive` special-cases the same bool to pick
`yourself`/`yourselves` because "you" is number-underspecified everywhere else.

It also does not let a fork recover the count: `$var`'s `nr` is already a formatted `String` by the
time `handle_placeholder_impl` runs and the bool is recovered from it by string-sniffing. Phase 6
item 14 therefore added `count: Option<PlaceholderCount>` to the five then-count-less pairs (verb,
pronoun, article, elision, adjective) in one signature change; `inflect_numeral_custom` was left
alone since it already had a richer `count: Option<i64>`, and `inflect_preposition_custom` was
designed with the parameter from the start. So every non-numeral pair now carries both, and only
`$var`'s own recovery remains string-sniffed. CLDR categories (Arabic dual, Slavic paucal) stay
**out** of the crate deliberately — see `docs/superpowers/specs/2026-08-13-number-categories.md`.

Item 14 also gave `Ranting::inflect` a fourth parameter, `case: GrammaticalCase`.

**Phase 7 item 11 gave it a fifth, `count: Option<PlaceholderCount>`** — the same type, from the
same source. Item 14 had left `inflect` the one count-less call *that renders the counted noun*,
so a fork could agree in a third number everywhere except on the noun: `{$n kitab}` with `n = 2`
gave every agreeing hook `PlaceholderCount { value: 2, .. }` and gave the noun the plural.
Grammatical-looking output, wrong in one word. Three things about it:

- **`None` is not a count of one.** A placeholder that wrote no numeral is distinguishable from
  `{$n noun}` with `n = 1`.
- **The `Cell` side-channel is not the alternative**, and was tried: smuggling the count from
  `inflect_numeral_custom` into `inflect` contaminates every later placeholder in the same
  template, depends on undocumented hook call order, and makes a `&self` trait stateful.
- **A bare `{noun}` never reaches `inflect` at all** — no marker means the macro renders through
  `Display`. Pre-existing, unchanged by item 11, and the reason
  `tests/ranting/third_number.rs` writes `{+noun}` where it wants an uncounted `inflect` call.

`2026-08-13-number-categories.md`'s inventory missed this originally because `inflect` is not a
`_custom` hook. `tests/ranting/third_number.rs` is the acceptance test, and is also the first
thing in the repo to exercise `inflect` against non-English input at all (see `pluralization.md`'s
blind-spot note).

## Wrapper delegation rules (`src/collections.rs`)

`Many` delegates a hook to its single item **only at `len() == 1`** — the same rule for
`noun_class()`, `capitalize()`, `is_first_person_subject_custom`, `inflect` and every `_custom`
pair. A 0-or-2+
`Many` reports `NounClass::UNSET` and does not delegate at all, so there is no hook invocation to
ride along with (`tests/ranting/elision.rs::many_with_two_items_does_not_elide` pins this).

When `Many` *does* delegate, it substitutes `count.or_else(|| self.own_count())` before forwarding
— its `Vec`'s length is a count no placeholder numeral supplied. It only fills the gap, never
overrides. Item 11 extended the same substitution to `inflect`, which is not a `_custom` pair; the
value it can supply there is always `1`, since delegation happens only at `len() == 1`. `Maybe`/`Box` are untouched: each holds at most one value with no alternative count to
offer.

## Other viewpoint/case behavior

- **`narration_person`** overrides which pronoun set renders, but **only for nouns declared
  first-person** (`subject` is exactly `"I"` or `"we"`), so a first-person narrator can be retold in
  third person while other characters keep their declared pronouns. Third-person rendering always
  falls back to singular "they" — there's no gender data on a first-person-declared noun. `we`
  overridden to `Person::Second` renders "you" the same as `I` would: a one-way rendering, not a
  round-trip.
- **`is_first_person_subject_custom`** (item 16) exists because `resolve_viewpoint` used to gate on
  a hard-coded `matches!(subject, "I" | "we")`, so a fork whose first-person labels are `ich`/`wir`
  got a silent no-op.
- **The fused `*=`/`*@` markers** (item 19) split case marking from pronoun display: they
  case-mark exactly like the bare marker (`inflect_article_custom` sees the identical
  `GrammaticalCase`) but render the name via `noun.inflect()` instead of calling
  `inflect_pronoun_custom`. `PlaceholderSpec::display_as_name` is `false` for every pre-existing
  placeholder, so `say!()`'s output is byte-identical by construction; when set,
  `handle_placeholder_impl` calls `noun.inflect(as_pl, uc, case.into())` with the *real*
  grammatical role rather than the always-`Name` that `CaseKind::Name`/`Hidden` pass.
  `ranting_i18n` dropped its `Render`/`GermanNoun::as_pronoun` workaround once this landed.

## The word-order boundary — permanent, not a gap

`ranting` inflects words within a template; the order of those words is the template's, and the
template is the caller's — so a non-English application needs one template per language, and **no
inflection hook will ever change that**. Agreement is *form*, never *position*. See
`docs/EXTENSIBILITY.md` §2.12 for the named list of unreachable constructions.

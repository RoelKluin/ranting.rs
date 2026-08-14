# Japanese falsification spike

**Status**: spike complete. Conclusion: **`ranting-ja` clears the item 4 bar, narrowly, and for a
different reason than the ROADMAP expected.** `NarrationContext.register` finds its first real
consumer and turns out to be the *right* shape for keigo. `NounClass` does **not** need to stretch
to classifiers — that question dissolves. What a Japanese lexicon actually falsifies is something
no existing spike names: **the numeral and the noun are joined by a hard-coded space that no hook
can remove**, so 「一匹の猫」 is unreachable and 「一匹の 猫」 is what renders. ROADMAP.md Phase 7
item 3.

No production code is proposed by this document.

## Method

Same as the Arabic spike (`2026-08-14-arabic-falsification-spike.md`): a throwaway
path-dependency crate, not committed, implementing a minimal `JaNoun` and probing each ROADMAP
question against real output. Every rendered string below is actual output. Two of the four
questions resolved differently from the ROADMAP's framing, and the finding that matters most was
not one of the four.

## 1. Numeral classifiers — the `NounClass` question dissolves; a spacing question replaces it

The ROADMAP asked whether `inflect_numeral_custom`'s `class: NounClass` parameter can carry a
classifier (匹/人/本), "or whether reading the counter off `class` is a misuse of a parameter
documented as a gender/lexical-class label."

**Neither, because the parameter is not needed.** The hook has `&self`, and *which counter a noun
takes is a property of that noun* — so a fork reads `self.classifier` and never touches `class` at
all. The probe renders correctly this way:

```
{$n neko}, n=1  ->  一匹の …      // self.classifier == 匹
{$n hito}, n=3  ->  三人の …      // self.classifier == 人
```

This is the same reasoning `.claude/rules/extension-hooks.md` already records for why `class` is a
parameter *at all*: it is redundant for a plain impl and exists for `Many`/`Maybe`/`Box`, where the
call site reads the class off the wrapper while `self` inside the hook is the inner value. A
classifier would ride that same path if a fork wanted `Many<JaNoun>` to count its items — which is
the only case where "put the counter in `NounClass`" would be tempting, and also the case where it
would be wrong, since a `Many` of mixed nouns has no single counter.

So item 2's open-ended `&'static str` design is *not* falsified by Japanese; it is simply not
exercised by it. That is a negative result on the ROADMAP's stated question, and it means the
sharpest genericity test of `NounClass` remains unwritten (Bantu, not Japanese, would be it).

### What replaced it: the numeral-noun separator

Japanese counts as 「一匹の猫」 — numeral, classifier, the particle の, then the noun, **with no
spaces anywhere**. The hook renders the numeral, and the crate then pushes a space and the noun:

```
{$n neko}, n=1  ->  "一匹の 猫"
                          ^ this space cannot be removed from any hook
```

Returning `"一匹の"` from `inflect_numeral_custom` gets the particle in, and the space still lands
after it. The separator is pushed by `handle_placeholder_impl` between the numeral and the noun; no
hook is offered it, unlike the article separator, which `elide_article_custom` receives explicitly
and may drop (`.claude/rules/extension-hooks.md`, item 7 point 1 — the post-assembly design exists
*precisely* so a fork can drop a separator).

The escape hatches are all worse than the disease:

- `{?$n neko}` hides the numeral — and renders `" 猫"`, keeping a leading space (see §5).
- Writing the numeral outside the placeholder (`一匹の{neko}`) works, but then the count is
  template-literal text and `inflect_numeral_custom` is dead — a fork that does this has no use for
  item 8 at all.
- Post-processing the returned `String` to squeeze spaces is not available: `say!()` returns the
  finished string, and squeezing spaces globally would corrupt any Latin text in the same template.

**This is the finding.** It is exactly parallel to the Arabic sun-letter case — an article written
bound to its noun — except that Arabic's got a hook in item 7 and the numeral's did not. Recommended
follow-up (needs its own item): give the numeral the same treatment, either by passing the
separator to `inflect_numeral_custom` and honoring an empty return, or by a numeral-side splice
matching `elide_article_custom`'s.

## 2. Register / keigo — **`register` is the right shape, and this is its first real consumer**

Item 3's pronoun-inventory spike concluded T-V politeness rides the addressee's declared subject
label rather than `NarrationContext.register`, because in German and Spanish politeness *is* a
pronoun slot. The ROADMAP's question was whether keigo — which operates on the verb, largely
independent of whether a pronoun is present — vindicates `register`'s design or reveals it as the
wrong axis for the same reason T-V was.

**It vindicates it.** The probe drives teineigo off `register` alone, with no pronoun anywhere in
the template and no entity state involved:

```
say_with!(ctx_formal, "{neko are}")  ->  猫 です
say_with!(ctx_casual, "{neko are}")  ->  猫 だ
```

The ROADMAP worried that keigo might vary per-addressee within one scene (formal to a stranger,
plain to a friend), which would make it addressee-scoped like T-V rather than story-scoped. It
does vary that way — but **that is not a problem, because `NarrationContext` is per-call, not
per-story.** Nothing prevents constructing a different context for each utterance; the two probe
lines above are two contexts in one scene. The word "story-wide" in the crate's own documentation
is a description of the *intended* use, not a constraint the type imposes.

Two consequences worth recording:

1. `register`'s design is confirmed by a language it was not designed for, which is what item 3
   said it lacked. `ranting_i18n`/`ranting_es` leave it inert; Japanese would be the first fork
   with a reason to read it.
2. The doc wording should be softened. Calling `NarrationContext` "story-wide" invites a fork to
   conclude, as the ROADMAP nearly did, that per-addressee variation is out of scope for it.

Sonkeigo/kenjougo (honorific verb *substitution* — 食べる → 召し上がる) is a lexical lookup keyed by
verb plus register, so it lands in the same hook with no additional signal needed. The one thing
`register`'s three-value enum cannot express is the finer honorific gradation Japanese actually
uses, but `Register` is a closed enum in `ranting` — a fork wanting five levels would use
`dialect: Option<&'static str>` (uninterpreted, open) instead. Worth stating; not worth changing.

## 3. `heed!()` against natural Japanese — the boundary holds, and the escape hatch is usable

Item 9 declared whitespace the only word boundary and proved it script-agnostic with *spaced* gloss
examples. Against genuinely natural, unspaced input:

```
heed!("{item}を取る", "剣を取る")  ->  None            // honest failure, no invented split
heed!("{clause}",     "剣を取る")  ->  Some("剣を取る")  // escape hatch: whole clause
heed!("取る {item}",   "取る 剣")  ->  Some("剣")        // the gloss-style example, unchanged
```

Exactly as documented. The question the ROADMAP actually posed was harder — whether "capture the
clause and segment it yourself" leaves `ask!()`'s value proposition intact. It does, with a
caveat worth stating plainly:

- For `heed!()`, the escape hatch is fine. The caller wanted a string; they get the string, and
  their tokenizer takes it from there.
- For `ask!()`, it is thinner. `ask!()`'s pitch is "parse the input, then call `answer()` with the
  captures" — and for unspaced Japanese, every template collapses to a single `{clause}` capture,
  so `answer()` receives the raw utterance and does *all* the work. `ask!()` degenerates into "call
  `answer()` with the input string", which is a function call.

That is a real narrowing but not a falsification: `ask!()` still routes to the right audience and
still returns `None` without calling `answer()` when the template's *literal* parts don't match,
which is worth something for command-style input (`"取る 剣"` written with a space, as game input
and CLI input often are). The honest summary is that `ask!()` is useful for Japanese *command*
input and not for Japanese *prose* input, and item 9's permanent-boundary decision stands.

## 4. Design load for a low-inflection language — fine, not a smell

A `ranting-ja` would leave `GrammaticalCase` unused (case is postpositional particles — が/を/に —
which are template literal text under the word-order boundary), `NounClass` at `UNSET` (§1),
`inflect_pronoun_custom` near pass-through (pervasive pro-drop), `inflect_adjective_custom` unused
(i-adjectives conjugate for tense/negation/politeness, and the `!` slot is degree, not tense), and
`inflect` an identity function (nouns don't decline for number).

That leaves two of the eight hook pairs live: verb (keigo) and numeral (classifiers). The ROADMAP
asked whether that is a fine outcome or a design smell.

**Fine.** Every hook defaults to English behavior and generates no code when not overridden — an
unclassed, unhooked impl is byte-identical to pre-v1.3 codegen, which item 2 established
deliberately. The cost of an unused hook to a fork is one line of documentation read and not acted
on. A surface sized for maximally-inflected languages degrading to near-nothing for a
low-inflection one is the *intended* shape, and Japanese is the proof that it degrades cleanly
rather than forcing a fork to fight it.

The one genuine cost is discoverability, and it is `docs/EXTENSIBILITY.md`'s problem rather than
the API's: eight `_custom` pairs is a lot to read through to discover that you need two.

## What `ranting-ja` would falsify that German and Spanish structurally cannot

| Axis | German | Spanish | Japanese |
|---|---|---|---|
| `NarrationContext.register` used for real | no | no | **yes — and it passes** |
| Numeral bound to noun without a space | no | no | **yes — and it fails** |
| Non-Indo-European, near-zero inflection | no | no | yes (degrades cleanly) |
| Unspaced input through `heed!()`/`ask!()` | no | no | yes (boundary holds; `ask!()` narrows) |

## Recommendation for item 4

**Build it, at lower priority than `ranting-ar`.** Its two decisive findings are one confirmation
(`register`) and one defect (§1's separator), against Arabic's one defect in a shipped signature
plus one confirmation. The confirmation is worth having precisely because it is the only evidence
`register` is not dead weight — item 1's audit could not settle that from inside the repo.

Scope should be smaller than the ROADMAP's provisional sizing suggests, because §4 means most of
the hook surface would be untouched: a small noun set with classifiers, teineigo verb forms driven
by `register`, and an `ask!()` audience over command-style spaced input to pin §3's narrowing.

## Residue

- **§1's separator gap is unscheduled**, and unlike the Arabic dual it has no partial workaround at
  all — it is a wrong character in the output, not a missing distinction.
- **`{?$n noun}` renders a double space** (`"I see  boots"`, `"есть  стол"`). This is *pinned* by
  `tests/ranting/numeral.rs` rather than flagged, so it currently reads as intended behavior. For
  English it is cosmetic; for Japanese it is on the critical path of the only workaround §1 has.
  Worth a decision either way.
- **"Story-wide" is the wrong word** for `NarrationContext` in the docs (§2).

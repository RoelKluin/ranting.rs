# Spike: how does the caller actually select a per-language template?

**Status**: design spike complete; conclusion is **leave it to the caller,
documented** — no new macro, no new type. ROADMAP.md Phase 6 item 22.

## Motivation

`2026-08-13-word-order-feasibility.md` (Phase 6 item 1) recommends option
(a) — the caller holds one template per language per sentence and selects
the right one before calling `say!()`/`say_with!()` — and states that as the
crate's permanent word-order boundary. It does not say *how* the selection
happens. This spike closes that gap: it is not a new grammar problem, it is
an ordinary Rust ergonomics question, but it has one hard constraint that
rules out the most natural-looking answer.

## The constraint: `say!()`'s literal must be a `StrLit` the macro can parse

`ranting_derive/src/lib.rs`'s `Say` (`struct Say { lit_str: String, params:
Vec<Expr> }`, `impl Parse for Say`, `impl ToTokens for Say`) parses its
first argument as a `StrLit` token *at proc-macro expansion time* — before
the surrounding crate is even compiled to IR, let alone run. `parse_params`
and `parse_str_params` walk that literal's placeholder syntax (`{=noun}`,
`{`noun}`, tense/degree markers, etc.) and bake a `PlaceholderSpec` per
placeholder plus a `format!()` literal with one `{}` per placeholder. This
is the "Compile-time parsing + runtime inflection" row of ROADMAP.md's
architecture-decisions table, marked ✅ **Locked**, and the word-order spike
already leaned on it (its "The literal is frozen at compile time" point).

The consequence for template *selection* specifically: `say!()`'s first
argument cannot be a `String` computed at runtime — not a `HashMap<Lang,
&str>` lookup, not a `match` returning `&str`, not a function call. All
three fail to parse as a `syn::LitStr`, so `say!(select_template(lang),
noun)` is a compile error, not a slow path. Whatever selects the template
has to produce, syntactically, a string literal for *each* language variant,
and each of those literals goes through its own independent `say!()`
expansion — there is no way to expand `say!()` once and vary its literal
input at runtime.

That rules out "runtime lookup in a translation catalogue" categorically,
not just as a style preference. The real question this spike scores is:
given that each language's literal needs its own `say!()` call, what's the
least repetitive way to write that at the call site.

## Options, scored

| Option | Preserves ✅ Locked compile-time-parsing decision? | Buildable today? | What it costs |
|---|---|---|---|
| **1. Leave it to the caller, documented** — caller writes `match lang { Lang::De => say!(...), Lang::En => say!(...) }` inline at each call site | **Yes** — every arm is its own ordinary `say!()` expansion | Yes, zero crate changes | Scales as languages × sentences, spelled out at every call site; no crate machinery to maintain |
| **2. A macro that takes a template set and expands the match** — e.g. `say_lang!(lang, { En => "...", De => "..." })` expanding to the same `match` | **Yes**, if implemented as pure syntactic sugar over N independent `say!()` expansions (one per arm) | Yes — a new `macro_rules!`/proc-macro, moderate crate work | Still languages × sentences in *source text* (every arm still needs its own literal, still needs its own translator to write it), but removes the boilerplate of writing `match`/`Lang::` by hand each time; adds a macro to learn, document, and keep in sync with `say!()`'s own grammar/error messages |
| **3. A per-language template-set type declared once, with a macro selecting from it by a runtime language value** — e.g. a `TemplateSet` trait/struct generated once per sentence, indexed by `Lang` at runtime | **No** — indexing "by a runtime language value" means the selected template's *identity* is a runtime fact, and something has to turn that runtime fact into a specific `say!()` expansion. Either the indexing happens *before* macro expansion (which collapses back to option 2, decided at compile time, no runtime win) or it happens *after* (which requires calling into a runtime-dispatched closure/function-pointer table whose entries were each pre-baked by their own `say!()`, i.e. exactly option 2 with an extra indirection layer, or — the tempting shortcut — storing pre-*formatted* strings and interpolating them at runtime, which is the translation-catalogue design item 1 already rejected because argument order and placeholder inflection can't survive being reduced to a runtime string template) | No, without also reintroducing runtime string-template resolution | Whichever sub-design it decays into, it either buys nothing over option 2 or reopens the door option 1's own spike shut |

### Why option 3 collapses rather than being a genuine third point

Option 3 sounds like it should be strictly better than option 2 — declare
the set once, index at runtime, done. It doesn't survive contact with the
constraint above. "Index by a runtime language value" bifurcates into
exactly two implementations, and both are already covered:

- If the *set itself* is a compile-time construct (a struct with one field
  per language, each field's value produced by its own `say!()` call written
  out in source), then "indexing by a runtime `Lang`" is just a `match`
  over which field to read — that *is* option 2, wearing a struct instead of
  a bare `match` arm list. No new capability, only new vocabulary.
- If the indexing is meant to happen *without* re-listing every language's
  `say!()` call at the point of use (the actual appeal of "declared once"),
  the only way to make that true is for the set to hold something that is
  no longer a `say!()` expansion at all — a closure, a function pointer, or
  a pre-rendered string — selected at runtime. A closure/fn-pointer table is
  still N `say!()` calls in source, just moved to the declaration site
  instead of the call site — no reduction in languages × sentences, only in
  where the multiplication is spelled out, and it adds a layer of indirection
  and a type (`TemplateSet`) to design, name, and version. A pre-rendered
  string table is the runtime-catalogue design the constraint section above
  already rules out, because it can't carry `say!()`'s placeholder
  inflection semantics — a catalogue entry is a finished `String`, not a
  literal `say!()` can parse and bake `PlaceholderSpec`s from.

So option 3 is not a real fourth architecture; it is option 2 relabeled, or
it is the rejected runtime catalogue relabeled, depending on which horn of
the bifurcation is picked. It earns a "No" in the preservation column
because the version of it that would actually be *new* (the second horn) is
the version that breaks the Locked decision.

## Recommendation

**Leave it to the caller, documented.** Concretely:

1. `say!()`/`say_with!()` gain no new macro and no new type. The per-language
   `match` (or equivalent `if`/`enum` dispatch) that item 1 already implies
   is written by the caller, inline, exactly where they'd write it anyway.
2. Document the scaling cost plainly, in the same place item 1's word-order
   boundary is documented (`docs/EXTENSIBILITY.md` §2.12 per that spike's
   Open Question 1): selecting a per-language template is languages ×
   sentences of source text, full stop, and no crate-level indirection
   changes that number — it only changes whose hand writes the
   multiplication and how it's spelled.
3. Option 2 (a sugar macro over the `match`) is not adopted, but it is not
   ruled impossible either — it's a legitimate future ergonomics-only
   addition if and when the boilerplate is felt to be a real pain point by
   an actual downstream fork (e.g. `ranting-i18n` growing enough sentences
   that hand-written `match` blocks get unwieldy). It is not scheduled as a
   Phase 6 item because nothing today demonstrates the need — `ranting-i18n`
   currently exercises single-language holes, not a multi-language dispatch
   table — and building sugar ahead of a demonstrated pain point is exactly
   the kind of premature abstraction the rest of this phase has avoided.

This keeps the spike's own instruction — "leave it to the caller, documented"
is stated as an acceptable outcome — and avoids inventing crate surface for
a cost that turns out to be irreducible: no design considered here makes
languages × sentences smaller, because each language's placeholder
inflection genuinely needs its own compile-time-parsed literal. The only
thing an outcome could change is where that multiplication is *written*, and
the plainest place to write it is the call site, in the caller's own code,
where it's also the easiest to review against the caller's actual set of
supported languages.

## Rejected alternatives, recorded

| Rejected | Why |
|---|---|
| 2. Sugar macro expanding a template set into a `match` | Not infeasible, but not adopted now — reduces call-site boilerplate only, doesn't reduce languages × sentences, and there is no demonstrated downstream pain point motivating it yet (see Recommendation point 3). Left open as a future ergonomics addition, not scheduled. |
| 3. Per-language template-set type indexed at runtime | Rejected as not a real third option — see "Why option 3 collapses" above. Its only genuinely-new variant (runtime-dispatched pre-rendered templates) reopens the translation-catalogue design item 1's spike already rejected, because a catalogue entry can't carry `say!()`'s placeholder-inflection semantics. |
| Runtime lookup in a translation catalogue (the premise this spike was asked to rule out) | `say!()`'s first argument is parsed as a `syn::LitStr` at macro-expansion time (`ranting_derive/src/lib.rs`'s `Say`/`impl Parse for Say`), before any runtime value exists to look up. A `HashMap<Lang, &str>`/function-call/`match`-returning-`&str` argument to `say!()` fails to parse, full stop — this isn't a slow path, it's a compile error. |

## What this spike does not touch

- It does not revisit item 1's conclusion — this document assumes "per-
  language template sets, selected before the call" as settled and asks only
  how selection is spelled in source.
- It proposes no production code. `ranting_derive/src/lib.rs`'s `Say`
  parsing, `format!()` codegen, and `PlaceholderSpec` baking are read here
  only to establish the constraint in "The constraint" above, not modified.
- `heed!()`/`ask!()` share the same compile-time-literal constraint (their
  own `compile_heed_template` also consumes a `StrLit`), so the same answer
  — caller-side dispatch, no runtime catalogue — applies to them without a
  separate spike; this document doesn't re-derive it for the input-parsing
  direction since `2026-08-12-input-parsing-feasibility.md` already
  establishes that side owns its own template per language too.

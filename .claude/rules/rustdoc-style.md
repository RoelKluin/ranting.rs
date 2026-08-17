# Rustdoc style: what a `docs.rs` reader should see

Read before writing or editing any `///`/`//!` doc comment, and before adding a doctest.
The procedure for sweeping existing docs against these rules is the `rustdoc-janitor` skill;
this file is the rules themselves.

## The one governing question

**What will the user actually see on the rendered page, and is it relaxed for the eyes?**
Every rule below is an instance of it. Judge doc comments from the rendered output
(`cargo doc --no-deps`, open the HTML), never from how the source reads — rustdoc merges
`///` blocks, hides `//` lines, renders backticks as grey boxes, and shows only the first
sentence in item listings.

## Audience, per surface

- `///` on `pub` items in the root `ranting` crate → **crate consumers on docs.rs.** Two
  personas: template writers using `say!()`/`heed!()`, and fork authors implementing
  `Ranting`. Everything here is written for one of those two — if a sentence serves
  neither, it doesn't belong in `///`.
- `//` comments → **maintainers only.** This is where provenance, history, and
  "why not the obvious alternative" go when they're worth keeping at the item
  (rustdoc still merges surrounding `///` blocks across an interleaved `//` line, so
  this is safe mid-doc).
- `ranting_core`/`ranting_derive` rustdoc is maintainer-facing (neither is part of the
  public semver surface); the same rules apply but at lower priority — **except for anything
  `ranting` re-exports.** A macro's doc in `ranting_derive/src/lib.rs` is what a docs.rs
  reader sees on `ranting`'s own `macro.*.html` page, so those blocks are front-line user
  prose and get full priority. Two consequences: a `///` on the `pub use` renders *above*
  the original's doc rather than replacing it, so a summary there is read twice — leave the
  re-export undocumented when the original is documented; and fixing one half of such a page
  leaves it half-fixed, so edit both crates in the same pass (and run `ranting_derive`'s own
  gates, prose-only — its doctests can't compile against `ranting`).

## History belongs in the record, not the docs

No `ROADMAP.md` phase/item numbers, `docs/architecture-review-*.md` citations,
`docs/superpowers/specs/*` paths, or "this was added in v1.x because…" narratives in `///`.
The repo already has a place for each of those (CLAUDE.md's "Where the record lives" table).

- If the **rationale itself** still helps a user ("why a struct and not a bare `i64`"),
  state the rationale without the archaeology.
- If only the **provenance** matters, move it verbatim into a `//` comment at the item.
- Precedents: `PlaceholderCount` (`src/lib.rs`) and `NarrationContext` (`src/narration.rs`),
  both converted 2026-08-15 — use their shape.

## Backtick discipline

Backticks render as grey inline-code boxes; too many makes a paragraph flicker grey/white
and reads worse than plain prose. Use a code span **only for things that are code**:

- an identifier referred to *as* an identifier (`inflect_verb_custom`, `NounClass`),
- template/placeholder syntax the reader must type exactly (`{=x will}`, `plural_end = "$"`),
- a literal value where the exact spelling is the point (`Some(Register::Neutral)`).

Do **not** backtick:

- generic nouns used descriptively, even when a parameter shares the name — "the input
  string", "the template", not "the `input`";
- `None`/`Some`/`true` when the sentence means "nothing / no match / yes" rather than the
  literal value — "returns nothing on a failed match" vs. "returns `None`" (keep the
  backticks when documenting the actual return value; drop them when the word is just
  standing in for a concept mid-prose);
- a term already backticked earlier in the same paragraph, unless ambiguity returns.

When in doubt, render the page: if a paragraph looks striped, it has too many spans.

## Necessity, wording, placement, form

Per sentence, in order:

1. **Necessary?** Does it change what the reader does or understands? Self-justification,
   defended design decisions, and "note that this differs from an internal alternative you've
   never heard of" serve the author, not the reader — cut or demote to `//`.
2. **Worded right?** Prefer the common case first, the exception after. Short declarative
   sentences over qualifier chains.
3. **Placed right?** Concept shared by several items → module doc (`//!`) or the trait doc,
   linked, not repeated. Fork-facing depth → `docs/EXTENSIBILITY.md`, with the item doc
   carrying one line and a pointer. Field detail → the field, not the struct.
4. **Best form?** An enumerable mapping is a table (the `Tense` marker table in
   `src/narration.rs` is the house example). Behavior is an example/doctest before it is a
   paragraph. Parallel items are a list; non-parallel prose stays prose.

## Doctests are user-visible prose too

- Example sentences must be **natural English** — `"They are out of stock."`, never
  grammatically-technically-correct output a native speaker wouldn't write
  (precedent: the `"There they are no items."` fix in `src/collections.rs`).
- Doctest assertions are pinned behavior. Changing an example sentence means updating any
  mirrored integration test (`src/collections.rs` ↔ `tests/ranting/recursive_inflection.rs`)
  and never changes what `say!()` renders — only which template is demonstrated.

## Verification

After any doc edit: `cargo doc --no-deps` and read the rendered item; `cargo test --doc`;
`cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`. Doc edits in sibling
crates get that crate's own gates (see CLAUDE.md — the root gate compiles none of them).

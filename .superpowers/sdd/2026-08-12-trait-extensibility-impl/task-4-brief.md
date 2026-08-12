# Task 4: Write EXTENSIBILITY.md Documentation

**Goal:** Create comprehensive user-facing documentation explaining the trait-based inflection extensibility feature with working examples (Pirate, Scottish, Spanish) and best practices.

**Files:**
- Create: `docs/EXTENSIBILITY.md`

**Interfaces:**
- Consumes: 
  - `PronounCase` enum definition and API reference
  - Three trait methods: `inflect_verb_custom()`, `inflect_pronoun_custom()`, `inflect_article_custom()`
  - Working example implementations from Task 3
- Produces: User-facing documentation with examples, API reference, and best practices

## Document Structure

### Section 1: Header and Quick Start

**Content:**
- Title: "# Trait-Based Inflection Extensibility"
- Intro (2-3 sentences): Explain that Ranting v1.1 enables custom grammar rules via trait method overrides for ecosystem forks (ranting-spanish, ranting-pirate, ranting-elvish).
- Quick Start code example showing PirateNoun with pirate verb customization and say!() output

### Section 2: Extension Points (API Reference)

Three subsections, one per custom method:

**2.1 Verb Inflection: `inflect_verb_custom()`**
- Signature: `fn inflect_verb_custom(&self, subject: &str, verb: &str, as_plural: bool, uc: bool) -> Option<String>`
- Parameters explained (subject, verb, as_plural, uc)
- Return values (Some(String) or None)
- Example: Pirate English (be/have/do customization)
- Best practice: return None for verbs you don't customize

**2.2 Pronoun Inflection: `inflect_pronoun_custom()`**
- Signature: `fn inflect_pronoun_custom(&self, subject: &str, case: PronounCase, as_plural: bool, uc: bool) -> Option<String>`
- `PronounCase` enum with 4 variants:
  - Subjective (I, you, he, she, it, we, they)
  - Objective (me, you, him, her, it, us, them)
  - PossessiveDeterminer (my, your, his, her, its, our, their)
  - PossessivePronoun (mine, yours, his, hers, its, ours, theirs)
- Example: Formal French "vous" for plural "you"
- Best practice: case routing — match on PronounCase to handle specific forms

**2.3 Article Inflection: `inflect_article_custom()`**
- Signature: `fn inflect_article_custom(&self, article: &str, noun_singular: &str, as_plural: bool, uc: bool) -> Option<String>`
- Parameters (article, noun_singular, as_plural, uc)
- Note: `noun_singular` is the singular inflected form (useful for gender/vowel detection)
- Example: Spanish gendered articles (la/el/los/las based on noun gender)
- Best practice: examine noun_singular for vowel/gender patterns

### Section 3: Partial Customization

**Content:**
- Explain that you don't need to implement all three methods
- Show example: implement only verb customization, leave pronouns and articles as default None
- Highlight: returning None automatically triggers English fallback

### Section 4: Full Examples

Three complete, runnable examples:

**4.1 Pirate Dialect**
- Struct definition and full impl Ranting with inflect_verb_custom
- Usage example: `say!("{=0 be} fearless!", pirate)`
- Output: "You be fearless."

**4.2 Scottish Highland English**
- Combine verb and pronoun customization
- ScottishHighlander struct with both methods
- Usage: "Laddie be brave"
- Shows case routing for pronouns

**4.3 Spanish with Gendered Articles and Verbs**
- Article customization (la/los/las for definite articles)
- Verb customization (Spanish "ser": "es"/"son")
- Usage: "{the =0 be} hermosa" → "La cosa es hermosa"
- Demonstrates noun_singular parameter use

### Section 5: Best Practices

**Content (5 bullet points):**
1. Partial customization is fine—return None to use English rules
2. Use `uc_1st_if()` for capitalization (handles contractions correctly)
3. Test your overrides with integration tests
4. Document your dialect/language in your ecosystem fork's README
5. Keep custom methods fast (called for every placeholder)

### Section 6: Performance Notes

**Content:**
- Custom method dispatch is a zero-cost abstraction
- If method returns None, English fallback is used without overhead
- If method returns Some(String), you pay string creation cost (unavoidable)
- No additional function call overhead beyond the Option check

### Section 7: Contributing Custom Rules

**Content:**
- Encourage users to open GitHub issues for English inflection bugs
- Suggest creating companion crates for new language modules (ranting-french, ranting-spanish)
- Mention that these crates would depend on ranting and export pre-built customized types

## Implementation Notes

- Use Rust code blocks with triple backticks (```rust) for all code examples
- Mark deferred/non-runnable examples with `ignore` comment inside block: ```rust (note: doesn't actually use ```ignore)
- Link to docs.rs for trait reference
- Cross-reference the examples in Quick Start and Full Examples sections
- Maintain clarity for users new to the library (some may not be familiar with trait design)

## Verification Steps

### Step 1: Create documentation file

Create `docs/EXTENSIBILITY.md` with all sections as defined above.

### Step 2: Verify file is readable and well-formatted

```bash
cat docs/EXTENSIBILITY.md | head -100
```

Expected: First 100 lines display correctly with clear markdown formatting.

### Step 3: Check for links and references

Verify that:
- All code examples compile (mentally verify signatures match trait definition)
- All trait method signatures match Task 1 definitions exactly
- `PronounCase` enum variants are correct (Subjective, Objective, PossessiveDeterminer, PossessivePronoun)
- Example struct names match actual implementations from tests (PirateNoun, ScottishHighlander, SpanishNoun)

### Step 4: Commit

```bash
git add docs/EXTENSIBILITY.md
git commit -m "docs: add EXTENSIBILITY.md with dialect examples and API reference"
```

## Self-Review Checklist

- [ ] All 7 sections present (Header, API Reference, Partial Customization, Full Examples, Best Practices, Performance, Contributing)
- [ ] API reference covers all three custom methods with signatures, parameters, return values, and examples
- [ ] PronounCase enum documented with all 4 variants
- [ ] 3 full working examples (Pirate, Scottish, Spanish) with usage and output
- [ ] Best practices section has 5 concrete bullet points
- [ ] Performance notes explain zero-cost abstraction
- [ ] Contributing section encourages ecosystem participation
- [ ] All code examples use correct Rust syntax and match trait definitions
- [ ] Markdown formatting is clean and readable
- [ ] File is saved to `docs/EXTENSIBILITY.md`
- [ ] Commit created with message: "docs: add EXTENSIBILITY.md with dialect examples and API reference"

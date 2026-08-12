# moved from ROADMAP.md

✅ **Working**:
- Core `Ranting` trait and `Noun` struct
- Basic subject/object/possessive pronouns (I, you, he, she, it, we, they)
- Simple present tense (do/does, am/are/is)
- Articles (a, an, some, the) with conditional display
- Optional article logic
- `say!()`, `ack!()`, `nay!()` macros
- Partial `ask!()` macro (recently added)
- Positional arguments only
- Integration tests and doctests

### Phase 1: **v0.3.0** — Foundation & Ergonomics
*Goal: Make Ranting more ergonomic and feature-complete for present-tense usage.*

**Named Arguments & Empty Placeholders**:
- Implement named argument parsing: `say!("{=person}", person = my_var)`
- Support empty placeholders: `say!("{}", my_var)` using positional/named order
- This is the highest-value feature—users expect `format!()`-like ergonomics
- **Tradeoff**: Slight macro complexity increase for significantly better UX

**Improve Error Messages**:
- Add compile-time validation for placeholder syntax
- Report mismatched argument counts with helpful messages
- Point users to specific placeholder issues in source
- **Tradeoff**: Larger error handling code vs. developer productivity

**Gender-Neutral Pronouns**:
- Expand `Noun` to support singular they/them
- Document inclusive language patterns
- Add examples showing they/them inflection
- **Tradeoff**: Minimal; they already work with current architecture

**Test Coverage**:
- Aim for >80% code coverage
- Add tests for edge cases in argument parsing
- Test error conditions thoroughly
- Use/Convert to table-driven tests, where possible

### Phase 2: **v0.4.0** — Grammar Depth
*Goal: Handle past, future, conditional, and continuous tenses.*

**Verb Tense Support**:
- Past tense: was/were, had, did
- Future/conditional: will, would, shall, should, may, might, can, could
- Continuous/progressive: -ing forms (is running, was running, will be running)
- **Implementation strategy**:
  - Extend placeholder syntax: `{=person do}` for present → `{=person did}` for past
  - Add `to_past()`, `to_future()` methods or similar to trait
  - use prefix markers: `{=person @past do}`
  - **Tradeoff**: Syntax clarity vs. flexibility


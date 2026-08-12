# Task 2: Implement Custom Method Hooks in handle_placeholder()

**Goal:** Wire the three custom trait methods into the runtime inflection pipeline by modifying `handle_placeholder()` to check custom methods before falling back to English module functions.

**Files:**
- Modify: `src/lib.rs:169-308` (`handle_placeholder()` function)

**Interfaces:**
- Consumes: 
  - `PronounCase` enum (from Task 1, now available in scope)
  - Three trait methods: `inflect_verb_custom()`, `inflect_pronoun_custom()`, `inflect_article_custom()` (from Task 1)
  - Existing English module functions: `inflect_verb()`, `inflect_subjective()`, `inflect_objective()`, `inflect_possesive()`, `inflect_adjective()`, `adapt_article()`
- Produces: Modified `handle_placeholder()` that checks custom methods before English functions

## Implementation Steps

### Step 1: Modify verb inflection call (around line 218)

**Find the line:**
```rust
let verb = inflect_verb(subjective, p.as_str(), as_pl, uc);
```

**Replace with:**
```rust
let verb = if let Some(custom) = noun.inflect_verb_custom(subjective, p.as_str(), as_pl, uc) {
    custom
} else {
    inflect_verb(subjective, p.as_str(), as_pl, uc)
};
```

### Step 2: Modify pronoun inflection calls (around lines 247-250)

**Find the match statement:**
```rust
let s = match case {
    "=" => inflect_subjective(subjective, as_pl, uc),
    "@" => inflect_objective(subjective, as_pl, uc),
    "`" => inflect_possesive(subjective, as_pl, uc),
    "~" => inflect_adjective(subjective, as_pl, uc),
    _ => noun.inflect(as_pl, uc),
};
```

**Replace with:**
```rust
let s = match case {
    "=" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::Subjective, as_pl, uc) {
            custom
        } else {
            inflect_subjective(subjective, as_pl, uc)
        }
    }
    "@" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::Objective, as_pl, uc) {
            custom
        } else {
            inflect_objective(subjective, as_pl, uc)
        }
    }
    "`" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::PossessiveDeterminer, as_pl, uc) {
            custom
        } else {
            inflect_possesive(subjective, as_pl, uc)
        }
    }
    "~" => {
        if let Some(custom) = noun.inflect_pronoun_custom(subjective, PronounCase::PossessivePronoun, as_pl, uc) {
            custom
        } else {
            inflect_adjective(subjective, as_pl, uc)
        }
    }
    _ => noun.inflect(as_pl, uc),
};
```

**Key Mapping Clarification:**
- `"="` marker → `PronounCase::Subjective` (subject pronouns: I, you, he, she, it, we, they)
- `"@"` marker → `PronounCase::Objective` (object pronouns: me, you, him, her, it, us, them)
- `` ` `` marker → `PronounCase::PossessiveDeterminer` (possessive determiners: my, your, his, her, its, our, their)
- `"~"` marker → `PronounCase::PossessivePronoun` (possessive pronouns: mine, yours, his, hers, its, ours, theirs)

### Step 3: Modify article inflection in get_article_or_so() (around lines 149-166)

**Find the function:**
```rust
fn get_article_or_so<R>(noun: &R, s: &str, space: &str, as_pl: bool, uc: bool) -> Option<String>
where
    R: Ranting,
{
    if noun.skip_article() && !s.starts_with('!') && !matches!(s, "these" | "those") {
        return Some("".to_string());
    }
    match s.trim_start_matches('!') {
        "the" => Some(uc_1st_if(s, uc)),
        "a" | "an" | "some" => {
            let singular = noun.inflect(false, false);
            let a_or_an = uc_1st_if(get_a_or_an(&singular), uc);
            Some(ranting::adapt_article(&a_or_an, s, space, as_pl, uc))
        }
        "these" | "those" => Some(ranting::adapt_article(s, s, space, as_pl, uc)),
        _ => None,
    }
}
```

**Replace with:**
```rust
fn get_article_or_so<R>(noun: &R, s: &str, space: &str, as_pl: bool, uc: bool) -> Option<String>
where
    R: Ranting,
{
    if noun.skip_article() && !s.starts_with('!') && !matches!(s, "these" | "those") {
        return Some("".to_string());
    }
    let article_form = s.trim_start_matches('!');
    let singular = noun.inflect(false, false);
    match article_form {
        "the" => {
            if let Some(custom) = noun.inflect_article_custom("the", &singular, as_pl, uc) {
                Some(custom + space)
            } else {
                Some(uc_1st_if("the", uc) + space)
            }
        }
        "a" | "an" | "some" => {
            if let Some(custom) = noun.inflect_article_custom(article_form, &singular, as_pl, uc) {
                Some(custom + space)
            } else {
                let a_or_an = uc_1st_if(get_a_or_an(&singular), uc);
                Some(ranting::adapt_article(&a_or_an, s, space, as_pl, uc))
            }
        }
        "these" | "those" => {
            if let Some(custom) = noun.inflect_article_custom(article_form, &singular, as_pl, uc) {
                Some(custom + space)
            } else {
                Some(ranting::adapt_article(s, s, space, as_pl, uc))
            }
        }
        _ => None,
    }
}
```

**Note:** The article custom method receives:
- `article_form` (e.g., "the", "a", "some", "these", "those")
- `singular` form of the noun (for gender/vowel detection)
- `as_pl` boolean
- `uc` boolean

## Verification Steps

### Step 4: Verify code compiles

```bash
cargo check
```

Expected: No errors. The custom method calls should compile with the new trait methods.

### Step 5: Run full test suite

```bash
cargo test --all
```

Expected: All existing tests pass (custom methods return `None` by default, so behavior is unchanged).

### Step 6: Commit

```bash
git add src/lib.rs
git commit -m "feat: integrate custom inflection hooks into handle_placeholder()"
```

## Self-Review Checklist

- [ ] Verb inflection hook added at line ~218 (checks custom before English)
- [ ] Pronoun inflection hooks added for all 4 cases (= @ ` ~) at lines ~247-250
- [ ] PronounCase enum variants used correctly in pronoun matches
- [ ] Article inflection hooks added in get_article_or_so() for "the", "a"/"an"/"some", "these"/"those"
- [ ] Custom method returns checked with `if let Some(...) { custom } else { english_fallback }`
- [ ] Article custom method receives correct parameters (article_form, singular noun form, as_pl, uc)
- [ ] `cargo check` passes with no errors
- [ ] `cargo test --all` passes with all existing tests (188+ total)
- [ ] Commit created with message: "feat: integrate custom inflection hooks into handle_placeholder()"

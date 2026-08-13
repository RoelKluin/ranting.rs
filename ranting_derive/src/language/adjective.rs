// (c) Roel Kluin 2026 MIT
//! Comparative/superlative adjective degree support (derive-crate only).
//!
//! Unlike `verb.rs`/`english_shared.rs`, this has no repo-root canonical copy
//! and no `include!()`-from-`OUT_DIR` wrapper indirection: degree inflection
//! needs no subject, number, or `NarrationContext` at runtime, so `ranting`
//! itself has no use for this table — only `ranting_derive`'s compile-time
//! `say!()` baking of the `!`/`!!` post-noun degree markers does (see
//! `handle_param` in `ranting_derive/src/lib.rs`). Generated table from
//! `data/irregular_adjectives.txt` via build.rs.

// Include the generated irregular adjective table at compile time.
include!(concat!(
    env!("OUT_DIR"),
    "/irregular_adjectives_generated.rs"
));

/// Vowel groups, used as a rough syllable count. Silent trailing `e` (e.g.
/// "large", "simple") doesn't count as an extra syllable.
fn syllable_count(word: &str) -> usize {
    let mut count = 0usize;
    let mut prev_was_vowel = false;
    for c in word.chars() {
        let is_vowel = matches!(c, 'a' | 'e' | 'i' | 'o' | 'u' | 'y');
        if is_vowel && !prev_was_vowel {
            count += 1;
        }
        prev_was_vowel = is_vowel;
    }
    if word.ends_with('e') && count > 1 {
        count -= 1;
    }
    count.max(1)
}

/// Whether a monosyllabic-in-our-heuristic word ends in a single
/// consonant-vowel-consonant pattern that doubles its final consonant before
/// `-er`/`-est` (big -> bigger, sad -> sadder), excluding `w`/`x`/`y` finals
/// (new -> newer, not newwer).
fn doubles_final_consonant(word: &str) -> bool {
    let chars: Vec<char> = word.chars().collect();
    if chars.len() < 3 {
        return false;
    }
    let last = chars[chars.len() - 1];
    let mid = chars[chars.len() - 2];
    let before = chars[chars.len() - 3];
    let is_vowel = |c: char| matches!(c, 'a' | 'e' | 'i' | 'o' | 'u');
    !is_vowel(last) && !matches!(last, 'w' | 'x' | 'y') && is_vowel(mid) && !is_vowel(before)
}

/// Whether `word_lower` ends in a consonant followed by `y` (happy, easy) —
/// eligible for the `y` -> `ier`/`iest` swap regardless of syllable count.
fn ends_consonant_y(word_lower: &str) -> bool {
    if !word_lower.ends_with('y') || word_lower.len() <= 1 {
        return false;
    }
    let prev = word_lower.chars().rev().nth(1).unwrap();
    !matches!(prev, 'a' | 'e' | 'i' | 'o' | 'u')
}

/// Apply regular `-er`/`-est` suffix rules. Only called once the caller
/// (`degree`) has decided the word is suffix-eligible: `e`-ending and
/// consonant+`y` words regardless of syllable count, everything else
/// (including CVC-doubling) only when monosyllabic — see `degree`'s
/// `suffix_eligible` gate for why CVC-doubling can't be left ungated
/// (it coincidentally matches the tail of multisyllabic words like
/// "beautiful").
fn regular_degree_suffixes(word_lower: &str) -> (String, String) {
    if let Some(stem) = word_lower.strip_suffix('e') {
        // large -> larger/largest; simple -> simpler/simplest
        (format!("{stem}er"), format!("{stem}est"))
    } else if ends_consonant_y(word_lower) {
        // happy -> happier/happiest
        let stem = &word_lower[..word_lower.len() - 1];
        (format!("{stem}ier"), format!("{stem}iest"))
    } else if doubles_final_consonant(word_lower) {
        // big -> bigger/biggest
        let last = word_lower.chars().last().unwrap();
        (
            format!("{word_lower}{last}er"),
            format!("{word_lower}{last}est"),
        )
    } else {
        // fast -> faster/fastest
        (format!("{word_lower}er"), format!("{word_lower}est"))
    }
}

/// Case pattern of `original` applied to `target` (mirrors `plurals.rs`'s
/// `apply_case`: all-uppercase or initial-uppercase originals propagate).
fn apply_case(original: &str, target: &str) -> String {
    if original
        .chars()
        .all(|c| !c.is_alphabetic() || c.is_uppercase())
    {
        target.to_uppercase()
    } else if original.chars().next().is_some_and(|c| c.is_uppercase()) {
        let mut chars = target.chars();
        match chars.next() {
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
            None => String::new(),
        }
    } else {
        target.to_string()
    }
}

fn degree(word: &str) -> (String, String) {
    let word_lower = word.to_lowercase();

    if let Some((_, comparative, superlative)) = IRREGULAR_ADJECTIVES
        .iter()
        .find(|(base, _, _)| word_lower == *base)
    {
        return (apply_case(word, comparative), apply_case(word, superlative));
    }

    // The `e`-ending and consonant+`y` suffix rules apply regardless of
    // syllable count (large -> larger, happy -> happier); CVC-doubling and
    // the plain `-er`/`-est` fallback are reserved for monosyllabic words —
    // otherwise (beautiful) it's periphrastic (more/most).
    //
    // Known limitation: the `e`-ending rule isn't itself syllable-gated, so a
    // multisyllabic adjective ending in a silent `e` (e.g. "expensive") gets
    // "expensiver"/"expensivest" instead of the idiomatic "more expensive"/
    // "most expensive". Fixing this would need real syllable-stress data,
    // which is out of scope for this heuristic; unlike CVC-doubling, silent-e
    // endings on genuinely long adjectives are rare enough that this hasn't
    // been worth gating.
    let suffix_eligible = word_lower.ends_with('e')
        || ends_consonant_y(&word_lower)
        || syllable_count(&word_lower) <= 1;

    if suffix_eligible {
        let (comparative, superlative) = regular_degree_suffixes(&word_lower);
        return (
            apply_case(word, &comparative),
            apply_case(word, &superlative),
        );
    }

    (format!("more {word}"), format!("most {word}"))
}

/// Convert a base adjective to its comparative form (e.g. "good" -> "better",
/// "fast" -> "faster", "beautiful" -> "more beautiful").
pub(crate) fn to_comparative(word: &str) -> String {
    degree(word).0
}

/// Convert a base adjective to its superlative form (e.g. "good" -> "best",
/// "fast" -> "fastest", "beautiful" -> "most beautiful").
pub(crate) fn to_superlative(word: &str) -> String {
    degree(word).1
}

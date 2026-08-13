// (c) Roel Kluin 2024 MIT
//! Verb conjugation helpers, used both at compile time (`say!()` literal
//! baking in `ranting_derive`) and at runtime (`say_with!()` runtime tense
//! resolution and `detect_tense` in `ranting`). Converts base verbs to
//! past/continuous/future forms using regular and irregular rules.
//!
//! `IRREGULAR_PAST`/`IRREGULAR_PAST_PARTICIPLE` are generated at build time
//! from `data/irregular_verbs.txt` (see `build.rs`) — this is now the single
//! place that table is generated; `ranting` and `ranting_derive` both read
//! it from here instead of each running their own copy of the codegen.

include!(concat!(env!("OUT_DIR"), "/irregular_verbs_generated.rs"));

fn regular_past_form(verb_lower: &str) -> String {
    let base = if let Some(stripped) = verb_lower.strip_suffix('e') {
        stripped.to_string()
    } else if verb_lower.ends_with('y') && verb_lower.len() > 1 {
        let prev = verb_lower.chars().rev().nth(1).unwrap();
        if !matches!(prev, 'a' | 'e' | 'i' | 'o' | 'u') {
            format!("{}i", &verb_lower[..verb_lower.len() - 1])
        } else {
            verb_lower.to_string()
        }
    } else {
        verb_lower.to_string()
    };

    format!("{}ed", base)
}

/// Convert a base verb to its past tense form.
pub fn to_past(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();

    if let Some((_, past)) = IRREGULAR_PAST.iter().find(|(base, _)| verb_lower == *base) {
        return past.to_string();
    }

    regular_past_form(&verb_lower)
}

/// Convert a base verb to its past participle form.
pub fn to_past_participle(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();

    if let Some((_, participle)) = IRREGULAR_PAST_PARTICIPLE
        .iter()
        .find(|(base, _)| verb_lower == *base)
    {
        return participle.to_string();
    }

    regular_past_form(&verb_lower)
}

/// Convert a base verb to its continuous (present participle) form (-ing).
pub fn to_continuous(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();

    let base = if verb_lower.ends_with("ie") {
        // "lie" → "lying", "tie" → "tying" (check this first, before 'e' rule)
        format!("{}y", &verb_lower[..verb_lower.len() - 2])
    } else if verb_lower.ends_with('e') {
        // "make" → "making", "like" → "liking"
        verb_lower[..verb_lower.len() - 1].to_string()
    } else if verb_lower.len() > 2 {
        // Consonant doubling: "run" → "running", "sit" → "sitting"
        let last_char = verb_lower.chars().last().unwrap();
        // Only double true consonants (not y, which acts as a vowel at word end)
        let is_doubling_consonant =
            last_char.is_alphabetic() && !matches!(last_char, 'a' | 'e' | 'i' | 'o' | 'u' | 'y');

        if is_doubling_consonant {
            let chars: Vec<char> = verb_lower.chars().collect();
            if chars.len() >= 2 {
                let second_last = chars[chars.len() - 2];
                if matches!(second_last, 'a' | 'e' | 'i' | 'o' | 'u') {
                    // Short vowel + consonant: double the consonant
                    format!("{}{}", verb_lower, last_char)
                } else {
                    verb_lower.clone()
                }
            } else {
                verb_lower.clone()
            }
        } else {
            verb_lower.clone()
        }
    } else {
        verb_lower.clone()
    };

    format!("{}ing", base)
}

/// Convert a verb to its future form (base form, since future tense is "will [base]").
pub fn to_future(verb: &str) -> String {
    // Future tense in English is always "will [base verb]", so just return the base.
    verb.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    // Load irregular verb tables for filtering
    fn load_irregular_bases() -> std::collections::HashSet<String> {
        IRREGULAR_PAST
            .iter()
            .map(|(base, _)| base.to_lowercase())
            .collect()
    }

    proptest! {
        #[test]
        fn prop_to_past_regular_ends_with_ed(base in r"[a-z]{2,12}") {
            let irregular_bases = load_irregular_bases();
            // Skip if the base is in the irregular table (would not follow the regular rule)
            prop_assume!(!irregular_bases.contains(&base));
            let result = to_past(&base);
            prop_assert!(result.ends_with("ed"), "to_past('{}') = '{}' does not end with 'ed'", base, result);
        }

        #[test]
        fn prop_to_past_participle_regular_ends_with_ed(base in r"[a-z]{2,12}") {
            let irregular_bases = load_irregular_bases();
            prop_assume!(!irregular_bases.contains(&base));
            let result = to_past_participle(&base);
            prop_assert!(result.ends_with("ed"), "to_past_participle('{}') = '{}' does not end with 'ed'", base, result);
        }

        #[test]
        fn prop_to_continuous_ends_with_ing(base in r"[a-z]{2,12}") {
            let result = to_continuous(&base);
            prop_assert!(result.ends_with("ing"), "to_continuous('{}') = '{}' does not end with 'ing'", base, result);
        }

        #[test]
        fn prop_to_future_identity(verb in r"[a-z]{2,12}") {
            let result = to_future(&verb);
            prop_assert_eq!(result, verb.clone(), "to_future('{}') should return '{}'", verb, verb);
        }

        #[test]
        fn prop_to_past_no_panic(verb in any::<String>()) {
            let _ = to_past(&verb);
        }

        #[test]
        fn prop_to_past_participle_no_panic(verb in any::<String>()) {
            let _ = to_past_participle(&verb);
        }

        #[test]
        fn prop_to_continuous_no_panic(verb in any::<String>()) {
            let _ = to_continuous(&verb);
        }

        #[test]
        fn prop_to_future_no_panic(verb in any::<String>()) {
            let _ = to_future(&verb);
        }
    }

    #[test]
    fn to_past_irregular_examples() {
        assert_eq!(to_past("go"), "went");
        assert_eq!(to_past("be"), "was");
        assert_eq!(to_past("do"), "did");
    }

    #[test]
    fn to_past_regular_examples() {
        assert_eq!(to_past("walk"), "walked");
        assert_eq!(to_past("play"), "played");
        assert_eq!(to_past("like"), "liked");
    }

    #[test]
    fn to_continuous_examples() {
        assert_eq!(to_continuous("run"), "running");
        assert_eq!(to_continuous("make"), "making");
        assert_eq!(to_continuous("lie"), "lying");
    }
}

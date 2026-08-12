// (c) Roel Kluin 2024 GPL v3
//! Compile-time verb conjugation helpers for the say!() macro.
//! Converts base verbs to past/continuous/future forms using regular and irregular rules.
//! Irregular verb tables are generated at build time from data/irregular_verbs.txt.

include!(concat!(env!("OUT_DIR"), "/irregular_verbs_generated.rs"));

fn regular_past_form(verb_lower: &str) -> String {
    let base = if verb_lower.ends_with('e') {
        verb_lower[..verb_lower.len() - 1].to_string()
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
/// Used at compile time in the say!() macro to conjugate verbs.
pub(crate) fn to_past(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();

    if let Some((_, past)) = IRREGULAR_PAST.iter().find(|(base, _)| verb_lower == *base) {
        return past.to_string();
    }

    regular_past_form(&verb_lower)
}

/// Convert a base verb to its past participle form.
/// Used at compile time in the say!() macro to conjugate verbs for perfect tenses.
pub(crate) fn to_past_participle(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();

    if let Some((_, participle)) = IRREGULAR_PAST_PARTICIPLE.iter().find(|(base, _)| verb_lower == *base) {
        return participle.to_string();
    }

    regular_past_form(&verb_lower)
}

/// Convert a base verb to its continuous (present participle) form (-ing).
/// Used at compile time in the say!() macro to conjugate verbs.
pub(crate) fn to_continuous(verb: &str) -> String {
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
        let is_doubling_consonant = last_char.is_alphabetic()
            && !matches!(last_char, 'a' | 'e' | 'i' | 'o' | 'u' | 'y');

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
/// Used at compile time in the say!() macro to conjugate verbs.
pub(crate) fn to_future(verb: &str) -> String {
    // Future tense in English is always "will [base verb]", so just return the base.
    verb.to_string()
}

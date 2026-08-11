// (c) Roel Kluin 2024 GPL v3
//! Verb tense classification and detection.
//! Supports detection of past, continuous, and present tenses.
//! Built-in for Phase 2 grammar depth — to avoid introducing new trait methods,
//! these functions live as free functions in src/language/, consistent with
//! inflect_verb, inflect_possesive, and other existing inflection functions.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Tense {
    Present,
    Past,
    Continuous,
}

static IRREGULAR_PAST: &[(&str, &str)] = &[
    ("am", "was"),
    ("are", "were"),
    ("be", "was"),
    ("beat", "beat"),
    ("become", "became"),
    ("begin", "began"),
    ("bend", "bent"),
    ("bet", "bet"),
    ("bite", "bit"),
    ("bleed", "bled"),
    ("blow", "blew"),
    ("break", "broke"),
    ("breed", "bred"),
    ("bring", "brought"),
    ("build", "built"),
    ("burn", "burnt"),
    ("burst", "burst"),
    ("buy", "bought"),
    ("catch", "caught"),
    ("choose", "chose"),
    ("come", "came"),
    ("cost", "cost"),
    ("cut", "cut"),
    ("deal", "dealt"),
    ("dig", "dug"),
    ("do", "did"),
    ("draw", "drew"),
    ("dream", "dreamt"),
    ("drink", "drank"),
    ("drive", "drove"),
    ("eat", "ate"),
    ("fall", "fell"),
    ("feed", "fed"),
    ("feel", "felt"),
    ("fight", "fought"),
    ("find", "found"),
    ("fly", "flew"),
    ("forget", "forgot"),
    ("forgive", "forgave"),
    ("freeze", "froze"),
    ("get", "got"),
    ("give", "gave"),
    ("go", "went"),
    ("grow", "grew"),
    ("have", "had"),
    ("hear", "heard"),
    ("hide", "hid"),
    ("hit", "hit"),
    ("hold", "held"),
    ("hurt", "hurt"),
    ("keep", "kept"),
    ("kneel", "knelt"),
    ("know", "knew"),
    ("lay", "laid"),
    ("lead", "led"),
    ("learn", "learnt"),
    ("leave", "left"),
    ("lend", "lent"),
    ("let", "let"),
    ("lie", "lay"),
    ("light", "lit"),
    ("lose", "lost"),
    ("make", "made"),
    ("mean", "meant"),
    ("meet", "met"),
    ("pay", "paid"),
    ("put", "put"),
    ("quit", "quit"),
    ("read", "read"),
    ("ride", "rode"),
    ("ring", "rang"),
    ("rise", "rose"),
    ("run", "ran"),
    ("say", "said"),
    ("see", "saw"),
    ("seek", "sought"),
    ("sell", "sold"),
    ("send", "sent"),
    ("set", "set"),
    ("shake", "shook"),
    ("shine", "shone"),
    ("shoot", "shot"),
    ("show", "showed"),
    ("shut", "shut"),
    ("sing", "sang"),
    ("sink", "sank"),
    ("sit", "sat"),
    ("sleep", "slept"),
    ("slide", "slid"),
    ("speak", "spoke"),
    ("spend", "spent"),
    ("spin", "spun"),
    ("split", "split"),
    ("spread", "spread"),
    ("stand", "stood"),
    ("steal", "stole"),
    ("stick", "stuck"),
    ("sting", "stung"),
    ("stink", "stunk"),
    ("strike", "struck"),
    ("string", "strung"),
    ("swear", "swore"),
    ("sweep", "swept"),
    ("swim", "swam"),
    ("swing", "swung"),
    ("take", "took"),
    ("teach", "taught"),
    ("tear", "tore"),
    ("tell", "told"),
    ("think", "thought"),
    ("throw", "threw"),
    ("understand", "understood"),
    ("wear", "wore"),
    ("weave", "wove"),
    ("weep", "wept"),
    ("win", "won"),
    ("wind", "wound"),
    ("write", "wrote"),
];

/// Detect the tense of a verb by checking for irregular forms, -ed suffix, or -ing suffix.
pub(crate) fn detect_tense(verb: &str) -> Tense {
    let verb_lower = verb.to_lowercase();

    // Check irregular past table
    if IRREGULAR_PAST
        .iter()
        .any(|(_, past)| verb_lower == *past)
    {
        return Tense::Past;
    }

    // Check regular past: -ed suffix
    if verb_lower.ends_with("ed") && verb.len() > 2 {
        return Tense::Past;
    }

    // Check continuous: -ing suffix
    if verb_lower.ends_with("ing") && verb.len() > 3 {
        return Tense::Continuous;
    }

    // Default to present
    Tense::Present
}

/// Convert a verb to its past tense form.
/// Uses the irregular verb table for irregulars; adds -ed for regular verbs.
pub(crate) fn to_past(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();
    let is_capitalized = verb.chars().next().map_or(false, |c| c.is_uppercase());

    // Check irregular past table
    if let Some((_, past)) = IRREGULAR_PAST.iter().find(|(base, _)| verb_lower == *base) {
        let result = if is_capitalized {
            let s = past.to_string();
            if let Some(c) = s.chars().next() {
                let rest = &s[c.len_utf8()..];
                c.to_uppercase().to_string() + rest
            } else {
                s
            }
        } else {
            past.to_string()
        };
        return result;
    }

    // Regular past: add -ed with common rules
    let base = if verb_lower.ends_with('e') {
        verb_lower[..verb_lower.len() - 1].to_string()
    } else if verb_lower.ends_with('y') && verb_lower.len() > 1 {
        let prev = verb_lower.chars().rev().nth(1).unwrap();
        if !matches!(prev, 'a' | 'e' | 'i' | 'o' | 'u') {
            // consonant + y: change y to i, then add -ed
            format!("{}i", &verb_lower[..verb_lower.len() - 1])
        } else {
            verb_lower.clone()
        }
    } else {
        verb_lower.clone()
    };

    let past_form = format!("{}ed", base);
    if is_capitalized {
        if let Some(c) = past_form.chars().next() {
            let rest = &past_form[c.len_utf8()..];
            c.to_uppercase().to_string() + rest
        } else {
            past_form
        }
    } else {
        past_form
    }
}

/// Convert a verb to its continuous (present participle) form (-ing).
/// Handles common rules: silent e, consonant doubling, ie → y.
pub(crate) fn to_continuous(verb: &str) -> String {
    let verb_lower = verb.to_lowercase();
    let is_capitalized = verb.chars().next().map_or(false, |c| c.is_uppercase());

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

    let ing_form = format!("{}ing", base);
    if is_capitalized {
        if let Some(c) = ing_form.chars().next() {
            let rest = &ing_form[c.len_utf8()..];
            c.to_uppercase().to_string() + rest
        } else {
            ing_form
        }
    } else {
        ing_form
    }
}

/// Convert a verb to its future form (base form, since future tense is "will [base]").
pub(crate) fn to_future(verb: &str) -> String {
    // Future tense in English is always "will [base verb]", so just return the base.
    verb.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_past_regular() {
        let test_cases = vec!["walked", "talked", "wanted", "played", "watched"];
        for verb in test_cases {
            assert_eq!(detect_tense(verb), Tense::Past, "Failed for: {}", verb);
            // Case insensitive
            assert_eq!(
                detect_tense(&verb.to_uppercase()),
                Tense::Past,
                "Failed for uppercase: {}",
                verb
            );
        }
    }

    #[test]
    fn detect_past_irregular() {
        let test_cases = vec![
            "went", "saw", "took", "made", "got", "came", "gave", "knew", "thought", "found",
            "was", "were", "had", "did", "said",
        ];
        for verb in test_cases {
            assert_eq!(detect_tense(verb), Tense::Past, "Failed for: {}", verb);
        }
    }

    #[test]
    fn detect_continuous() {
        let test_cases = vec!["walking", "running", "going", "talking", "playing"];
        for verb in test_cases {
            assert_eq!(
                detect_tense(verb),
                Tense::Continuous,
                "Failed for: {}",
                verb
            );
            // Case insensitive
            assert_eq!(
                detect_tense(&verb.to_uppercase()),
                Tense::Continuous,
                "Failed for uppercase: {}",
                verb
            );
        }
    }

    #[test]
    fn detect_present() {
        let test_cases = vec!["walk", "run", "go", "is", "have", "do", "say", "see"];
        for verb in test_cases {
            assert_eq!(detect_tense(verb), Tense::Present, "Failed for: {}", verb);
        }
    }

    #[test]
    fn irregular_table_coverage() {
        // Ensure all entries in the irregular table are correctly classified as Past
        for (_, past) in IRREGULAR_PAST {
            assert_eq!(
                detect_tense(past),
                Tense::Past,
                "Irregular verb table entry '{}' not detected as Past",
                past
            );
        }
    }

    #[test]
    fn to_past_regular() {
        assert_eq!(to_past("walk"), "walked");
        assert_eq!(to_past("talk"), "talked");
        assert_eq!(to_past("play"), "played");
        assert_eq!(to_past("watch"), "watched");
        assert_eq!(to_past("try"), "tried");
        assert_eq!(to_past("like"), "liked");
    }

    #[test]
    fn to_past_irregular() {
        assert_eq!(to_past("go"), "went");
        assert_eq!(to_past("see"), "saw");
        assert_eq!(to_past("take"), "took");
        assert_eq!(to_past("make"), "made");
        assert_eq!(to_past("come"), "came");
        assert_eq!(to_past("give"), "gave");
        assert_eq!(to_past("run"), "ran");
        assert_eq!(to_past("do"), "did");
    }

    #[test]
    fn to_past_case_preserving() {
        assert_eq!(to_past("Walk"), "Walked");
        assert_eq!(to_past("Go"), "Went");
        assert_eq!(to_past("Try"), "Tried");
    }

    #[test]
    fn to_continuous_basic() {
        assert_eq!(to_continuous("walk"), "walking");
        assert_eq!(to_continuous("talk"), "talking");
        assert_eq!(to_continuous("play"), "playing");
    }

    #[test]
    fn to_continuous_silent_e() {
        assert_eq!(to_continuous("make"), "making");
        assert_eq!(to_continuous("like"), "liking");
        assert_eq!(to_continuous("come"), "coming");
        assert_eq!(to_continuous("take"), "taking");
    }

    #[test]
    fn to_continuous_consonant_doubling() {
        assert_eq!(to_continuous("run"), "running");
        assert_eq!(to_continuous("sit"), "sitting");
        assert_eq!(to_continuous("stop"), "stopping");
        assert_eq!(to_continuous("bat"), "batting");
    }

    #[test]
    fn to_continuous_ie_to_y() {
        assert_eq!(to_continuous("lie"), "lying");
        assert_eq!(to_continuous("tie"), "tying");
    }

    #[test]
    fn to_continuous_case_preserving() {
        assert_eq!(to_continuous("Walk"), "Walking");
        assert_eq!(to_continuous("Run"), "Running");
        assert_eq!(to_continuous("Make"), "Making");
    }

    #[test]
    fn to_future_returns_base() {
        // Future tense is always "will [base]", so the function returns the base form
        assert_eq!(to_future("walk"), "walk");
        assert_eq!(to_future("go"), "go");
        assert_eq!(to_future("Walk"), "Walk");
    }
}

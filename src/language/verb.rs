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
}

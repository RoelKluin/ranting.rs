// (c) Roel Kluin 2024 GPL v3
//! Compile-time verb conjugation helpers for the say!() macro.
//! Converts base verbs to past/continuous/future forms using regular and irregular rules.
//! These are duplicated from src/language/verb.rs in the runtime crate.

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

static IRREGULAR_PAST_PARTICIPLE: &[(&str, &str)] = &[
    ("am", "been"),
    ("are", "been"),
    ("be", "been"),
    ("beat", "beaten"),
    ("become", "become"),
    ("begin", "begun"),
    ("bend", "bent"),
    ("bet", "bet"),
    ("bite", "bitten"),
    ("bleed", "bled"),
    ("blow", "blown"),
    ("break", "broken"),
    ("breed", "bred"),
    ("bring", "brought"),
    ("build", "built"),
    ("burn", "burnt"),
    ("burst", "burst"),
    ("buy", "bought"),
    ("catch", "caught"),
    ("choose", "chosen"),
    ("come", "come"),
    ("cost", "cost"),
    ("cut", "cut"),
    ("deal", "dealt"),
    ("dig", "dug"),
    ("do", "done"),
    ("draw", "drawn"),
    ("dream", "dreamt"),
    ("drink", "drunk"),
    ("drive", "driven"),
    ("eat", "eaten"),
    ("fall", "fallen"),
    ("feed", "fed"),
    ("feel", "felt"),
    ("fight", "fought"),
    ("find", "found"),
    ("fly", "flown"),
    ("forget", "forgotten"),
    ("forgive", "forgiven"),
    ("freeze", "frozen"),
    ("get", "gotten"),
    ("give", "given"),
    ("go", "gone"),
    ("grow", "grown"),
    ("have", "had"),
    ("hear", "heard"),
    ("hide", "hidden"),
    ("hit", "hit"),
    ("hold", "held"),
    ("hurt", "hurt"),
    ("keep", "kept"),
    ("kneel", "knelt"),
    ("know", "known"),
    ("lay", "laid"),
    ("lead", "led"),
    ("learn", "learnt"),
    ("leave", "left"),
    ("lend", "lent"),
    ("let", "let"),
    ("lie", "lain"),
    ("light", "lit"),
    ("lose", "lost"),
    ("make", "made"),
    ("mean", "meant"),
    ("meet", "met"),
    ("pay", "paid"),
    ("put", "put"),
    ("quit", "quit"),
    ("read", "read"),
    ("ride", "ridden"),
    ("ring", "rung"),
    ("rise", "risen"),
    ("run", "run"),
    ("say", "said"),
    ("see", "seen"),
    ("seek", "sought"),
    ("sell", "sold"),
    ("send", "sent"),
    ("set", "set"),
    ("shake", "shaken"),
    ("shine", "shone"),
    ("shoot", "shot"),
    ("show", "shown"),
    ("shut", "shut"),
    ("sing", "sung"),
    ("sink", "sunk"),
    ("sit", "sat"),
    ("sleep", "slept"),
    ("slide", "slid"),
    ("speak", "spoken"),
    ("spend", "spent"),
    ("spin", "spun"),
    ("split", "split"),
    ("spread", "spread"),
    ("stand", "stood"),
    ("steal", "stolen"),
    ("stick", "stuck"),
    ("sting", "stung"),
    ("stink", "stunk"),
    ("strike", "struck"),
    ("string", "strung"),
    ("swear", "sworn"),
    ("sweep", "swept"),
    ("swim", "swum"),
    ("swing", "swung"),
    ("take", "taken"),
    ("teach", "taught"),
    ("tear", "torn"),
    ("tell", "told"),
    ("think", "thought"),
    ("throw", "thrown"),
    ("understand", "understood"),
    ("wear", "worn"),
    ("weave", "woven"),
    ("weep", "wept"),
    ("win", "won"),
    ("wind", "wound"),
    ("write", "written"),
];

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

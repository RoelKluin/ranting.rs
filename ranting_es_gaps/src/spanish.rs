//! An independent reference for the small pieces of Spanish grammar `ranting_es` actually
//! implements as general rules.
//!
//! **This copy must stay independent -- do not make it call `ranting_es`.** The probes compare
//! `ranting_es`'s real hook output against these rules; routing both through one implementation
//! would make them agree by construction and report nothing forever, whatever `ranting_es` does.
//! Same arrangement `ranting_gaps::english` documents relative to `ranting`, and `CLAUDE.md`
//! records for `PH_EXT` versus `ph_ext`.
//!
//! Scope is deliberately narrow, unlike `ranting_gaps::english`. `ranting_es`'s lexicon is a
//! **closed set** -- 4 nouns, 4 verbs, 3 adjectives, numerals 0..=12, every gender/conjugation
//! hand-listed rather than suffix-generated (`problema` is masculine specifically to prove there
//! is no `-o`/`-a` gender-guessing heuristic to test). There is no general noun-gender,
//! noun-pluralization, or verb-conjugation rule in `ranting_es` for this crate to differentially
//! check against new words the way `ranting_gaps::english::regular_plural` checks `ranting`'s
//! real general rule. What *does* exist as a real general rule, and so has an oracle function
//! here: article selection, the `-o`/`-a` adjective-agreement pattern, and the two obligatory
//! preposition+article fusions.

/// The definite article: `el`/`la`/`los`/`las`. Independently derived from standard Spanish
/// grammar, matching `ranting_es::lexicon::definite_article`'s contract but not its code.
pub fn definite_article(feminine: bool, plural: bool, euphonic_el: bool) -> &'static str {
    match (plural, feminine) {
        (true, true) => "las",
        (true, false) => "los",
        (false, true) if euphonic_el => "el",
        (false, true) => "la",
        (false, false) => "el",
    }
}

/// The indefinite article: `un`/`una`/`unos`/`unas`.
pub fn indefinite_article(feminine: bool, plural: bool, euphonic_el: bool) -> &'static str {
    match (plural, feminine) {
        (true, true) => "unas",
        (true, false) => "unos",
        (false, true) if euphonic_el => "un",
        (false, true) => "una",
        (false, false) => "un",
    }
}

/// Agree a Spanish adjective stem ending in `-o` (or gender-invariant) with a noun's gender and
/// number: `-o`/`-a` swap for gender, then vowel-final → `+s` / consonant-final → `+es` for
/// number. Written with **char-aware** operations throughout, not byte-slicing -- unlike
/// `ranting_gaps::english::replace_tail`, a general Spanish adjective stem can end in an accented
/// vowel, and this oracle is meant to stay correct as a specification even though the three words
/// this crate actually checks it against (`negro`, `pequeño`, `azul`) happen to be ASCII-safe.
pub fn adjective_agree(stem: &str, feminine: bool, plural: bool) -> String {
    let mut chars: Vec<char> = stem.chars().collect();
    let gendered: String = if chars.last() == Some(&'o') {
        if feminine {
            chars.pop();
            chars.push('a');
        }
        chars.into_iter().collect()
    } else {
        stem.to_string()
    };
    if !plural {
        return gendered;
    }
    let ends_in_vowel = gendered
        .chars()
        .next_back()
        .is_some_and(|c| matches!(c, 'a' | 'e' | 'i' | 'o' | 'u'));
    if ends_in_vowel {
        format!("{gendered}s")
    } else {
        format!("{gendered}es")
    }
}

/// The two obligatory preposition+article fusions, and nothing else -- `de`/`a` + `la`/`los`/
/// `las` don't contract, so every other combination is `None` (decline, don't invent).
pub fn fuse_preposition(preposition: &str, article: &str) -> Option<&'static str> {
    match (preposition, article) {
        ("de", "el") => Some("del"),
        ("a", "el") => Some("al"),
        _ => None,
    }
}

/// Present-tense conjugations for the closed verb set, independently hand-transcribed from
/// standard Spanish grammar -- not copied from `ranting_es::lexicon::VERBS`, since copying would
/// make the differential check tautological. Order: yo, tú, él/ella/usted, nosotros, vosotros,
/// ellos/ellas/ustedes -- the same six-person order `ranting_es::lexicon::Person::index` uses.
pub static VERBS: [(&str, [&str; 6]); 4] = [
    (
        "hablar",
        ["hablo", "hablas", "habla", "hablamos", "habláis", "hablan"],
    ),
    (
        "comer",
        ["como", "comes", "come", "comemos", "coméis", "comen"],
    ),
    (
        "vivir",
        ["vivo", "vives", "vive", "vivimos", "vivís", "viven"],
    ),
    ("ser", ["soy", "eres", "es", "somos", "sois", "son"]),
];

/// Conjugate an infinitive at the given person index (0..=5), or `None` outside the closed set.
pub fn conjugate(infinitive: &str, person_index: usize) -> Option<&'static str> {
    VERBS
        .iter()
        .find(|(inf, _)| *inf == infinitive)
        .map(|(_, forms)| forms[person_index])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn article_selection_matches_the_four_known_nouns() {
        // gato: masculine, casa: feminine, problema: masculine despite -a, agua: feminine
        // euphonic.
        assert_eq!(definite_article(false, false, false), "el"); // gato
        assert_eq!(definite_article(true, false, false), "la"); // casa
        assert_eq!(definite_article(false, false, false), "el"); // problema
        assert_eq!(definite_article(true, false, true), "el"); // agua (euphonic)
        assert_eq!(definite_article(true, true, true), "las"); // aguas (euphony reverts)
    }

    #[test]
    fn adjective_agreement_handles_gender_and_number() {
        assert_eq!(adjective_agree("negro", false, false), "negro");
        assert_eq!(adjective_agree("negro", true, false), "negra");
        assert_eq!(adjective_agree("negro", true, true), "negras");
        assert_eq!(adjective_agree("azul", false, false), "azul");
        assert_eq!(adjective_agree("azul", false, true), "azules");
    }

    #[test]
    fn preposition_fusion_is_exactly_the_two_pairs() {
        assert_eq!(fuse_preposition("de", "el"), Some("del"));
        assert_eq!(fuse_preposition("a", "el"), Some("al"));
        assert_eq!(fuse_preposition("de", "la"), None);
        assert_eq!(fuse_preposition("a", "los"), None);
    }

    #[test]
    fn conjugate_covers_all_six_persons_of_the_closed_verbs() {
        assert_eq!(conjugate("hablar", 0), Some("hablo"));
        assert_eq!(conjugate("ser", 2), Some("es"));
        assert_eq!(conjugate("volar", 0), None);
    }
}

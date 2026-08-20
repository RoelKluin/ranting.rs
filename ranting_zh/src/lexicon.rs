//! The closed Mandarin vocabulary: three nouns with their classifiers, three verbs, and the
//! seven personal pronouns. Nothing here touches `ranting` — [`crate::noun`] and
//! [`crate::person`] are the only files that do.
//!
//! Mandarin has no grammatical gender, no noun plural marking, no case, no articles and no
//! verb conjugation for person or tense — see the crate README for what that leaves this
//! lexicon to actually exercise.

/// One closed-vocabulary noun.
pub struct NounEntry {
    pub word: &'static str,
    /// The classifier (measure word) this noun takes: 只 for animals, 本 for bound volumes, 个
    /// for people (and the default, general-purpose classifier). Read off the noun, not off
    /// `NounClass` — the same "`&self` is already enough" shape `ranting_ja`'s counters use, and
    /// not a new finding here; see the README.
    pub classifier: &'static str,
}

/// 猫 (māo) "cat" — counted with 只.
pub const MAO: NounEntry = NounEntry {
    word: "猫",
    classifier: "只",
};

/// 书 (shū) "book" — counted with 本.
pub const SHU: NounEntry = NounEntry {
    word: "书",
    classifier: "本",
};

/// 人 (rén) "person" — counted with 个, and unlike `MAO`/`SHU`, can additionally pluralize with
/// the 们 suffix when referring to specific people rather than a bare count — see
/// [`crate::person`] and the README's "也/们-plural is a person-only, lexically-conditioned
/// exception" note.
pub const REN: NounEntry = NounEntry {
    word: "人",
    classifier: "个",
};

/// Numerals `0..=5`, spelled out in Chinese numerals. Digit style (`$n`) needs no table:
/// Mandarin writes the same Arabic digits English does in ordinary running text.
static NUMERALS: [&str; 6] = ["零", "一", "二", "三", "四", "五"];

/// Spell a count in Chinese, immediately followed by the noun's classifier, or `None` outside
/// the closed `0..=5` range — the same decline-rather-than-guess contract every other lexicon in
/// this repo uses.
pub fn counted(count: i64, classifier: &str) -> Option<String> {
    let word = *NUMERALS.get(usize::try_from(count).ok()?)?;
    Some(format!("{word}{classifier}"))
}

/// One entry in the closed verb table: every English spelling `ranting`'s compile-time tense
/// baking might hand to the hook (base, third-person, past, participle, gerund — see the
/// crate README's "why so many English spellings for one Chinese word" note, which is itself
/// part of this crate's headline finding, not incidental noise) mapped to the single invariant
/// Mandarin word. Mandarin verbs never conjugate for person, number or tense at all.
static VERBS: [(&[&str], &str); 3] = [
    (&["eat", "eats", "ate", "eaten", "eating"], "吃"),
    (&["be", "is", "are", "was", "were", "been", "being"], "是"),
    (&["have", "has", "had", "having"], "有"),
];

/// Substitute the invariant Mandarin verb for any English spelling of it in the closed set, or
/// `None` when the written word isn't one of these three — the same fall-through-to-English
/// contract every lexicon in this repo uses for an unknown word.
pub fn verb_form(word: &str) -> Option<&'static str> {
    let lower = word.to_lowercase();
    VERBS
        .iter()
        .find(|(spellings, _)| spellings.contains(&lower.as_str()))
        .map(|(_, zh)| *zh)
}

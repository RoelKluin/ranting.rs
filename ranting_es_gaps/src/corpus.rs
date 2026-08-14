//! Reading arbitrary Spanish text and reducing it to what the probes need: a word/bigram index
//! with enough context to quote a sentence back.
//!
//! **This tool does not nominate candidates the way `ranting_gaps` does.** `ranting_gaps` reads
//! an open English vocabulary, so it needs a determiner-cue heuristic to guess which words are
//! nouns, and attestation exists to filter the heuristic's false positives (the determiner cue
//! alone nominates `is`/`as`/`only` as nouns; requiring the corpus to attest the "corrected" form
//! drops them without a part-of-speech tagger). `ranting_es`'s lexicon is closed and tiny -- 4
//! nouns, 4 verbs, 3 adjectives, numerals 0..=12 -- so every comparison this crate can make is
//! enumerated directly from that lexicon, with no guessing step. The corpus's only remaining job
//! is to say which of those enumerated forms real Spanish text actually contains, which grades a
//! case `Attested` instead of `Certain` -- see `crate::finding::Confidence`. Nothing is ever
//! filtered out for lack of attestation.
//!
//! No corpus ships with this crate -- see README.md. Point it at any Spanish text you have.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Words that mark whatever follows as a Spanish noun phrase (definite/indefinite articles,
/// demonstratives, possessives, `cada`). Used only by `probes::lexicon_coverage` to measure how
/// much of the corpus's noun phrases the closed lexicon covers -- no probe here relies on this
/// cue to *decide* whether a word is a noun, since every noun this tool checks is already known
/// from `ranting_es::lexicon` directly.
const DETERMINERS: &[&str] = &[
    "el", "la", "los", "las", "un", "una", "unos", "unas", "este", "esta", "estos", "estas", "ese",
    "esa", "esos", "esas", "aquel", "aquella", "aquellos", "aquellas", "mi", "mis", "tu", "tus",
    "su", "sus", "nuestro", "nuestra", "nuestros", "nuestras", "cada",
];

/// Determiners that additionally mark the following noun as plural.
const PLURAL_DETERMINERS: &[&str] = &[
    "los", "las", "estos", "estas", "esos", "esas", "aquellos", "aquellas", "mis", "tus", "sus",
    "nuestros", "nuestras", "varios", "varias", "algunos", "algunas",
];

/// One occurrence of a word in the corpus, with enough context to quote it back.
#[derive(Debug, Clone)]
pub struct Occurrence {
    pub file: PathBuf,
    pub line: usize,
    pub sentence: String,
}

/// What the corpus knows about one lowercase word form.
#[derive(Debug, Default, Clone)]
pub struct WordFacts {
    /// Times seen at all.
    pub total: usize,
    /// Times seen directly after a determiner -- the noun-position cue, used only by
    /// `lexicon_coverage`.
    pub as_noun: usize,
    /// Up to [`MAX_EXAMPLES`] sentences, kept for quoting in the report.
    pub examples: Vec<Occurrence>,
}

/// How many example sentences to retain per word.
pub const MAX_EXAMPLES: usize = 3;

impl WordFacts {
    fn note(&mut self, occurrence: &Occurrence) {
        self.total += 1;
        if self.examples.len() < MAX_EXAMPLES {
            self.examples.push(occurrence.clone());
        }
    }
}

/// Every word the corpus contains, plus adjacent-pair counts the agreement probes attest against.
#[derive(Debug, Default)]
pub struct Corpus {
    pub words: HashMap<String, WordFacts>,
    pub files: usize,
    pub total_words: usize,
    /// Adjacent word pairs, lowercase. This is the whole attestation mechanism for
    /// `article_agreement`/`adjective_agreement`/`preposition_fusion` (e.g. `("el", "gato")`,
    /// `("gato", "negro")`, `("del", "gato")`) -- no trigram type is needed, since Spanish's two
    /// fusible preposition+article pairs (`de`+`el`→`del`, `a`+`el`→`al`) already tokenize as one
    /// word once fused.
    pub bigrams: HashMap<(String, String), usize>,
}

impl Corpus {
    /// True when the corpus contains this exact word form at all. Used by this crate's own
    /// ingestion tests; the probes use [`Corpus::word_count`]/[`Corpus::attests_bigram`] instead,
    /// since they need a count to grade confidence, not a bare yes/no.
    #[cfg(test)]
    pub fn attests(&self, word: &str) -> bool {
        self.words.contains_key(&word.to_lowercase())
    }

    /// How many times the corpus writes `a` immediately followed by `b`, both lowercased. The
    /// grading signal every agreement probe uses.
    pub fn attests_bigram(&self, a: &str, b: &str) -> usize {
        self.bigrams
            .get(&(a.to_lowercase(), b.to_lowercase()))
            .copied()
            .unwrap_or(0)
    }

    /// Total times the corpus writes this exact word form. Used by `preposition_fusion` to grade
    /// a fused form (`del`, `al`) directly, since it tokenizes as one word rather than a bigram.
    pub fn word_count(&self, word: &str) -> usize {
        self.words
            .get(&word.to_lowercase())
            .map(|f| f.total)
            .unwrap_or(0)
    }

    #[cfg(test)]
    pub fn facts(&self, word: &str) -> Option<&WordFacts> {
        self.words.get(&word.to_lowercase())
    }

    /// Words seen in noun position at least `min` times, most frequent first. Used only by
    /// `lexicon_coverage` to report how much of the corpus's noun phrases fall outside the
    /// closed lexicon.
    pub fn nouns_by_frequency(&self, min: usize) -> Vec<(&str, &WordFacts)> {
        let mut out: Vec<_> = self
            .words
            .iter()
            .filter(|(_, f)| f.as_noun >= min)
            .map(|(w, f)| (w.as_str(), f))
            .collect();
        out.sort_by(|a, b| b.1.as_noun.cmp(&a.1.as_noun).then(a.0.cmp(b.0)));
        out
    }
}

/// Collect every readable text file under `paths` (recursing into directories) into one corpus.
pub fn read(paths: &[PathBuf]) -> std::io::Result<Corpus> {
    let mut corpus = Corpus::default();
    let mut files = Vec::new();
    for path in paths {
        gather(path, &mut files)?;
    }
    for file in files {
        let Ok(text) = std::fs::read_to_string(&file) else {
            continue;
        };
        corpus.files += 1;
        ingest(&text, &file, &mut corpus);
    }
    Ok(corpus)
}

fn gather(path: &Path, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    if path.is_dir() {
        let mut entries: Vec<_> = std::fs::read_dir(path)?
            .filter_map(Result::ok)
            .map(|e| e.path())
            .collect();
        entries.sort();
        for entry in entries {
            gather(&entry, out)?;
        }
    } else if path.is_file() {
        out.push(path.to_path_buf());
    }
    Ok(())
}

fn ingest(text: &str, file: &Path, corpus: &mut Corpus) {
    for (line_idx, line) in text.lines().enumerate() {
        let line = strip_markup(line);
        for sentence in split_sentences(&line) {
            let words = words_of(sentence);
            if words.is_empty() {
                continue;
            }
            let occurrence = Occurrence {
                file: file.to_path_buf(),
                line: line_idx + 1,
                sentence: sentence.trim().to_string(),
            };
            for (i, word) in words.iter().enumerate() {
                corpus.total_words += 1;
                let facts = corpus.words.entry(word.clone()).or_default();
                facts.note(&occurrence);
                let prev = i.checked_sub(1).map(|p| words[p].as_str());
                // Plural determiners are checked first: several entries (`los`, `las`, ...)
                // appear in both lists, and a plain-determiner arm above would shadow them.
                match prev {
                    Some(p) if PLURAL_DETERMINERS.contains(&p) => facts.as_noun += 1,
                    Some(p) if DETERMINERS.contains(&p) => facts.as_noun += 1,
                    _ => {}
                }
                if let Some(p) = prev {
                    *corpus
                        .bigrams
                        .entry((p.to_string(), word.clone()))
                        .or_default() += 1;
                }
            }
        }
    }
}

/// Remove the markup this tool is most likely to be pointed at. Identical to
/// `ranting_gaps::corpus::strip_markup` -- markdown syntax is not language-specific.
fn strip_markup(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '`' | '*' | '_' | '#' | '|' | '[' | ']' | '<' | '>' => out.push(' '),
            '\\' => {
                chars.next();
                out.push(' ');
            }
            _ => out.push(c),
        }
    }
    out
}

/// Split on sentence-final punctuation, plus Spanish's opening `¿`/`¡`, which mark a sentence
/// boundary from *before* the clause rather than after -- without this, `Hola. ¿Qué tal?` would
/// tokenize `.` correctly but a lone `¿Qué tal?` preceded by no `.`/`!`/`?` would merge with
/// whatever ran before it.
fn split_sentences(line: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let mut start = 0;
    let mut chars = line.char_indices().peekable();
    while let Some((i, c)) = chars.next() {
        let byte_idx = i + c.len_utf8();
        if matches!(c, '.' | '!' | '?' | ';' | ':')
            && chars.peek().is_none_or(|(_, n)| n.is_whitespace())
        {
            out.push(&line[start..byte_idx]);
            start = byte_idx;
        } else if matches!(c, '¿' | '¡') && i > start {
            out.push(&line[start..i]);
            start = i;
        }
    }
    if start < line.len() {
        out.push(&line[start..]);
    }
    out
}

/// Lowercase alphabetic words -- Rust's `char::is_alphabetic()` already covers Spanish's
/// accented vowels and `ñ`, so no change from `ranting_gaps::corpus::words_of` is needed beyond
/// treating `¿`/`¡` as punctuation (handled by `split_sentences`, not here). Hyphens and
/// apostrophes are kept for symmetry with the English tool, though nothing in this crate's
/// closed lexicon currently needs them.
fn words_of(sentence: &str) -> Vec<String> {
    sentence
        .split(|c: char| !(c.is_alphabetic() || c == '-' || c == '\''))
        .filter_map(|raw| {
            let w = raw.trim_matches(|c| c == '-' || c == '\'');
            let ok = w.len() > 1
                && w.chars().any(|c| c.is_alphabetic())
                && w.chars()
                    .all(|c| c.is_alphabetic() || c == '-' || c == '\'');
            ok.then(|| w.to_lowercase())
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn corpus_of(text: &str) -> Corpus {
        let mut c = Corpus::default();
        ingest(text, Path::new("t.txt"), &mut c);
        c
    }

    #[test]
    fn accented_and_enye_words_are_ingested() {
        let c = corpus_of("El niño pequeño comió mañana.");
        assert!(c.attests("niño"));
        assert!(c.attests("mañana"));
        assert!(c.attests("comió"));
    }

    #[test]
    fn inverted_punctuation_marks_a_sentence_boundary() {
        let c = corpus_of("Hola. ¿Qué tal? Bien.");
        // "tal" and "hola" must not be counted as an adjacent bigram across the ¿ boundary.
        assert_eq!(c.attests_bigram("hola", "qué"), 0);
    }

    #[test]
    fn bigrams_are_counted_across_the_corpus() {
        let c = corpus_of("El gato negro. El gato negro duerme.");
        assert_eq!(c.attests_bigram("gato", "negro"), 2);
        assert_eq!(c.attests_bigram("el", "gato"), 2);
    }

    #[test]
    fn determiner_marks_the_following_word_as_a_noun() {
        let c = corpus_of("El gato duerme. Los gatos duermen.");
        assert_eq!(c.facts("gato").expect("gato").as_noun, 1);
        assert_eq!(c.facts("gatos").expect("gatos").as_noun, 1);
    }

    #[test]
    fn markup_and_identifiers_do_not_become_words() {
        let c = corpus_of("El **gato** come `snake_case` y x2 y un 5.");
        assert!(c.attests("gato"), "markup around a word is stripped");
        assert!(!c.attests("snake_case"), "identifiers are not words");
        assert!(!c.attests("x2"), "digit-bearing tokens are not words");
    }
}

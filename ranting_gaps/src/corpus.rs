//! Reading arbitrary text and reducing it to the two things every probe needs: a stream of
//! words tagged with the syntactic cue that preceded them, and a way back to the sentence they
//! came from.
//!
//! There is deliberately **no part-of-speech tagger** here. A probe that needs to know "is this
//! word a noun?" gets one signal: was it preceded by a determiner or a numeral. That is a
//! heuristic with known holes (it misses bare plurals, "dogs bark"), but it is a *conservative*
//! one -- it under-reports rather than inventing nouns -- and every finding it feeds carries the
//! sentence it came from so a human can check. A real tagger would be a dependency, a model, and
//! a new class of wrongness for a tool whose whole value is being trustworthy about what fails.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Words that mark whatever follows as a noun phrase. A numeral counts too, handled separately.
const DETERMINERS: &[&str] = &[
    "a", "an", "the", "this", "that", "these", "those", "my", "your", "his", "her", "its", "our",
    "their", "every", "each", "another", "no",
];

/// Determiners that additionally say the following noun is **plural**. Used by the plural probes
/// to decide whether a corpus token is attesting a plural form.
const PLURAL_DETERMINERS: &[&str] = &["these", "those", "many", "several", "both", "few"];

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
    /// Times seen directly after a determiner -- the noun-position cue.
    pub as_noun: usize,
    /// Times seen directly after a *plural* determiner or a numeral greater than one.
    pub as_plural_noun: usize,
    /// Times seen directly after `to` -- the bare-infinitive cue, i.e. a verb.
    pub as_verb: usize,
    /// Up to [`MAX_EXAMPLES`] sentences, kept for quoting in the report.
    pub examples: Vec<Occurrence>,
}

/// How many example sentences to retain per word. Enough to judge a finding, few enough that the
/// report stays readable and `summary.json` stays a reasonable size.
pub const MAX_EXAMPLES: usize = 3;

impl WordFacts {
    fn note(&mut self, occurrence: &Occurrence) {
        self.total += 1;
        if self.examples.len() < MAX_EXAMPLES {
            self.examples.push(occurrence.clone());
        }
    }
}

/// Every word the corpus contains, plus the corpus-wide totals a report header needs.
#[derive(Debug, Default)]
pub struct Corpus {
    pub words: HashMap<String, WordFacts>,
    pub files: usize,
    pub total_words: usize,
    /// Adjacent word pairs, lowercase, for the probes that need two-token context (a determiner
    /// followed by an adjective followed by a noun, say). Counted, not stored per-occurrence.
    pub bigrams: HashMap<(String, String), usize>,
}

impl Corpus {
    /// True when the corpus contains this exact word form at all. Probes use it to tell an
    /// *attested* correction ("the corpus really does write `flies`") from a merely computed one.
    pub fn attests(&self, word: &str) -> bool {
        self.words.contains_key(&word.to_lowercase())
    }

    #[cfg(test)]
    pub fn facts(&self, word: &str) -> Option<&WordFacts> {
        self.words.get(&word.to_lowercase())
    }

    /// Words seen in noun position at least `min` times, most frequent first. The ordering is
    /// what makes the report "how common is this" rather than "here is a pile".
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
            // Not UTF-8 -- a binary, a gzipped changelog. Skipping is correct: the alternative is
            // lossy decoding, which invents words that were never written.
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
                // Plural determiners are checked first: `these`/`those` are in *both* lists,
                // and a plain-determiner arm placed above would shadow them and silently lose
                // every plural attestation they carry.
                match prev {
                    Some(p) if PLURAL_DETERMINERS.contains(&p) => {
                        facts.as_noun += 1;
                        facts.as_plural_noun += 1;
                    }
                    Some(p) if DETERMINERS.contains(&p) => facts.as_noun += 1,
                    Some(p) if is_plural_numeral(p) => {
                        facts.as_noun += 1;
                        facts.as_plural_noun += 1;
                    }
                    Some("to") => facts.as_verb += 1,
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

/// A numeral that forces a plural noun. `one` deliberately excluded; so is a bare `0`, which
/// English writes with a plural ("zero items") but which is not worth the ambiguity here.
fn is_plural_numeral(word: &str) -> bool {
    const SPELLED: &[&str] = &[
        "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve",
        "twenty", "hundred", "thousand", "million",
    ];
    if SPELLED.contains(&word) {
        return true;
    }
    matches!(word.parse::<u64>(), Ok(n) if n > 1)
}

/// Remove the markup this tool is most likely to be pointed at, so that `**word**` and `` `word` ``
/// are counted as `word` rather than as three different tokens. Intentionally shallow: anything
/// cleverer starts guessing at document structure, and every probe already tolerates noise by
/// requiring a frequency threshold.
fn strip_markup(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            // Inline code and fenced-code markers: the contents are identifiers, not English, but
            // dropping the delimiters is enough -- the word filter below rejects the identifiers.
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

/// Split on sentence-final punctuation. Abbreviations ("e.g.", "Dr.") will over-split; that costs
/// a probe nothing, since no probe reasons across a sentence boundary -- it only quotes them.
fn split_sentences(line: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let mut start = 0;
    let bytes = line.as_bytes();
    for (i, b) in bytes.iter().enumerate() {
        if matches!(b, b'.' | b'!' | b'?' | b';' | b':')
            && bytes.get(i + 1).is_none_or(|n| n.is_ascii_whitespace())
        {
            out.push(&line[start..=i]);
            start = i + 1;
        }
    }
    if start < line.len() {
        out.push(&line[start..]);
    }
    out
}

/// Lowercase alphabetic words, hyphens and apostrophes kept (they are exactly what two of the
/// probes are about). Anything containing a digit or an underscore is an identifier, not a word.
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
    fn determiner_marks_the_following_word_as_a_noun() {
        let c = corpus_of("The fly landed. I want to fly home.");
        let fly = c.facts("fly").expect("fly is in the corpus");
        assert_eq!(fly.total, 2);
        assert_eq!(fly.as_noun, 1, "only the determiner-preceded one");
        assert_eq!(fly.as_verb, 1, "only the `to`-preceded one");
    }

    #[test]
    fn numerals_and_plural_determiners_attest_a_plural() {
        let c = corpus_of("Three flies buzzed. These wolves howled. One wolf slept.");
        assert_eq!(c.facts("flies").expect("flies").as_plural_noun, 1);
        assert_eq!(c.facts("wolves").expect("wolves").as_plural_noun, 1);
        assert_eq!(
            c.facts("wolf").expect("wolf").as_plural_noun,
            0,
            "`one` must not attest a plural"
        );
    }

    #[test]
    fn markup_and_identifiers_do_not_become_words() {
        let c = corpus_of("The **box** holds `snake_case_ident` and x2 and a 5.");
        assert!(c.attests("box"), "markup around a word is stripped");
        assert!(!c.attests("snake_case_ident"), "identifiers are not words");
        assert!(!c.attests("x2"), "digit-bearing tokens are not words");
    }

    #[test]
    fn hyphens_and_apostrophes_survive_because_two_probes_need_them() {
        let c = corpus_of("The mother-in-law arrived. The children's toys broke.");
        assert!(c.attests("mother-in-law"));
        assert!(c.attests("children's"));
    }

    #[test]
    fn nouns_are_returned_most_frequent_first() {
        let c = corpus_of("The cat sat. The cat ran. The dog sat.");
        let ranked = c.nouns_by_frequency(1);
        assert_eq!(ranked.first().map(|(w, _)| *w), Some("cat"));
    }
}

// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.
use super::english_shared::{inflect_verb, SubjectPronoun};
use super::roman_shared::ExtCased;

// sentence always captures: to obtain the placeholder offset.
pub(crate) static PH_START: &str =
    r"(?P<pre>(?:^|[.?!]\s+|\{\{)?+)\{(?:(?P<plain>\w*+)|(?P<ranting>[^{}:]*+))(?P<fmt>:.*?)?\}";

// TODO: do not capture space separate but split off from parts.
// regex to capture the placholders or sentence ends
// useful: https://regex101.com/r/Ly7O1x/3/
/// The components captured in a Ranting trait placeholder are defined here.
pub(crate) static PH_EXT: &str = r"^(?x)
    (?P<uc>[,^])?+
    (?P<pre>(?:\??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se|
    '[rv]e|[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
    (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
    \s+)?+
    (?P<etc1>[\w-]+(?:\s+[\w-]+)*\s+?)??
    (?P<nr>[+-]|\??\#\w+\s+)?+
    (?P<case>(?:[`=@~*?]|<[^>]*>))?+
    (?P<noun>[\w-]+)
    (?P<etc2>(?:\s+[\w-]+)+?)??
    (?P<post>\s+(?:[\w-]+')?[\w-]+|'\w*)?$";

/// Return the case for a character.
pub(crate) fn get_case_from_str(s: &str) -> Option<&str> {
    match s {
        "=" => Some("subjective"),
        "@" => Some("objective"),
        "`" => Some("possesive"),
        "~" => Some("adjective"),
        "*" => None,
        _ => Some(s.trim_start_matches('<')),
    }
}

/// Return whether a word is one of `some` `a` `an` `the` `these` `those`
pub fn is_article_or_so(word: &str) -> bool {
    matches!(word, "some" | "a" | "an" | "the" | "these" | "those")
}

// In English singular possesive s i always the same.
pub(crate) fn adapt_possesive_s_wo_subj(c: char) -> Option<&'static str> {
    (c == '-').then_some("'s")
}

// In English verbs are the same if 1st, 2nd or 3rd person plural.
pub(crate) fn inflect_verb_wo_subj(verb: &str, c: char, uc: bool) -> Option<ExtCased> {
    (c == '+').then_some(inflect_verb(SubjectPronoun::We, verb, true, uc))
}

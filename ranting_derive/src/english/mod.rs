// (c) Roel Kluin 2022 GPL v3
//
// regex to capture the placholders or sentence ends
// useful: https://regex101.com/r/Ly7O1x/3/
/// The components captured in a Ranting trait placeholder are defined here.
pub(crate) static RANTING_PLACEHOLDER: &str = r"(?x)
(?P<sentence>(?:\.\s+)?+)  # sentence always captures: to obtain the placeholder offset.
\{
    (?P<uc>[,^])?+
    (?:
        (?P<pre>[aA]n?|[sS]ome|[tT]h(?:[eo]s)?e|
        '[rv]e|[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
        (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)?+
        (?P<etc1>(?:\s+[\w-]+)+?)??
        (?P<sp1>\s+)
    )?+
    (?P<plurality>[+-]|(?:\??\#|\#\?)\w+(?P<sp2>\s+))?+
    (?P<case>[`:@~*?])?+
    (?P<noun>[\w-]+)
    (?P<etc2>(?:\s+[\w-]+)+?)??
    (?P<post>(?:\s+\w+)?'\w*|\s+[\w-]+)?
    (?P<fmt>:[^}]+)?+
\}";

/// Return the case for a character.
pub(crate) fn get_case_from_str(s: &str) -> Option<&'static str> {
    match s {
        ":" => Some("subjective"),
        "@" => Some("objective"),
        "`" => Some("possesive"),
        "~" => Some("adjective"),
        "*" => None,
        x => panic!("Unsupported case {x}"),
    }
}

/// Return whether a word is one of `some` `a` `an` `the` `these` `those`
pub fn is_article_or_so(word: &str) -> bool {
    match word {
        "some" | "a" | "an" | "the" | "these" | "those" => true,
        _ => false,
    }
}

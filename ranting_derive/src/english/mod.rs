// (c) Roel Kluin 2022 GPL v3
//
/// The components captured in a Ranting trait placeholder are defined here.
pub(crate) static RANTING_PLACEHOLDER: &str = r"(?x)(?P<sentence>(?:\.\s+)?+)\{  # NOTE: always captures on purpose!
    (?P<uc>[,^])?+
    (?:(?P<pre>
        [aA]n?|[sS]ome|[tT]h(?:[eo]s)?e|
        '[rv]e|'d|[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
        (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?
    )(?P<s_pre>\s+))?
    (?P<etc1>\s+(?:[\w-]+\s+)+?)??
    (?:(?P<plurality>[+-]|\#\??\w+)(?P<s_nr>\s*))?+
    (?P<case>[`:@~])?+
    (?:(?P<noun>\??[\w-]+)(?P<s_noun>\s*)?+)
    (?P<etc2>(?:[\w-]+\s+)*?)??
    (?:(?P<post>(?:\w+')?+[\w-]+))?
    (?P<fmt>:[^}]+)?+
\}";

/// Return the case for a character.
pub(crate) fn get_case_from_str(s: &str) -> Option<&'static str> {
    match s {
        ":" => Some("subjective"),
        "@" => Some("objective"),
        "`" => Some("possesive"),
        "~" => Some("adjective"),
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



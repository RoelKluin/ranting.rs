// (c) Roel Kluin 2022 GPL v3
use std::str::FromStr;
use strum_macros::EnumString;

// sentence always captures: to obtain the placeholder offset.
#[allow(dead_code)]
pub(crate) static PH_START: &str =
    r"(?P<pre>(?:^|[.?!]\s+|\{\{)?+)\{(?:(?P<plain>\w*+)|(?P<ranting>[^{}:]*+))(?P<fmt>:.*?)?\}";

// TODO: do not capture space separate but split off from parts.
// regex to capture the placholders or sentence ends
// useful: https://regex101.com/r/Ly7O1x/3/
/// The components captured in a Ranting trait placeholder are defined here.
#[allow(dead_code)]
pub(crate) static PH_EXT: &str = r"^(?x)
    (?P<uc>[,^])?+
    (?P<pre>(?:
        \??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se|`[\w-]+|
        (?:[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
        (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
        (?:\s+(?:\??an?|\??some|\??the|th[eo]se|`[\w-]+))?
    )(?:\s+[\w-]+)*?\s+)?+
    (?P<nr>[+-]|(?:\#|\??\$)\w+\s+)?+
    (?P<case>[`=@~*?])?+
    (?P<noun>[\w-]+)
    (?P<post>\s+(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$";

#[allow(dead_code)]
pub(crate) static ASK: &str = r"^(?x)
    (?P<pre>(?:
        (?:[cC]an(?:'t)?|[mM]ay|(?:[sS]ha|[wW]i)ll|
        (?:(?:[aA]|[wW]e)re|[hH]a(?:d|ve)|[dD]o|(?:[cCwW]|[sS]h)ould|[mM](?:us|igh)t)(?:n't)?+)
        (?:\s+(?:\??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se))?
        |(?:\??[aA]n?|\??[sS]ome|\??[tT]he|[Tt]h[eo]se)
    )(?:\s+[\w-]+)*?\s+)?+
    (?P<noun>[\w-]+)
    (?P<post>\s+(?:[\w-]+\s+)*?(?:[\w-]+')?[\w-]+|'\w*)?$";

/// An enum with pronouns in subjective form.
#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub(crate) enum SubjectPronoun {
    #[strum(serialize = "I")]
    I,
    You,
    Thou,
    He,
    She,
    It,
    We,
    Ye,
    They,
}

/// return whether the given `&str` is a valid subject
pub fn is_subject(subject: &str) -> bool {
    SubjectPronoun::from_str(subject).is_ok()
}

/// Returns whether the subjective is plural. You is assumed singular; a plural_you
/// ranting attribute should already be considered before this call.
pub fn is_subjective_plural(subject: &str) -> bool {
    (SubjectPronoun::from_str(subject).expect("not a subject") as usize) >= 6
}

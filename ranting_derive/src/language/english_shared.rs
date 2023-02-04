// (c) Roel Kluin 2022 GPL v3
use super::roman_shared::{uc_1st_if, ExtCased};
use std::str::FromStr;
use strum_macros::EnumString;

/// An enum with pronouns in subjective form.
#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum SubjectPronoun {
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

#[derive(EnumString, PartialEq, Eq, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub(super) enum ArticleOrSo {
    The,
    #[strum(serialize = "a", serialize = "an", serialize = "some")]
    A,
    These,
    Those,
}

#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub(super) enum IrregularPluralVerb {
    Are,
    Were,
    #[strum(serialize = "'re")]
    Re,
    #[strum(serialize = "'ve")]
    Ve,
    Have,
    Do,
    #[strum(serialize = "'d")]
    D,
    Had,
    Could,
    Would,
    Should,
    Might,
    Must,
    Can,
    May,
    Shall,
    Will,
    Ca, // for can't
    Wo, // for won't
}

static ARTICLE_OR_SO: [&str; 8] = ["the", "some", "these", "those", "a", "an", "this", "that"];

#[allow(dead_code)]
static IRREGULAR_VERBS_1ST: [&str; 4] = ["am", "aint", "was", "'m"];
#[allow(dead_code)]
static IRREGULAR_VERBS_3RD: [&str; 5] = ["is", "was", "'s", "has", "does"];

#[allow(dead_code)]
pub(super) struct Rant {
    uc: bool,
    pre: String,
    sp1: String,
    etc1: String,
    sp2: String,
    plurality: String,
    sp3: String,
    case: String,
    noun: String,
    sp4: String,
    etc2: String,
    post: String,
}

#[allow(dead_code)]
// XXX: Should be pub only for ranting_derive
/// (for internal use) Given an article, the default, a requested one, inflect and to_upper() it as specified.
pub fn adapt_article(
    mut s: String,
    requested: &str,
    ws: &str,
    as_plural: bool,
    uc: bool,
) -> String {
    if !s.is_empty() {
        let art = match ArticleOrSo::from_str(requested).expect("Not an article") {
            t if t == ArticleOrSo::The || as_plural => ARTICLE_OR_SO[t as usize],
            ArticleOrSo::A => s.as_str(),
            t => ARTICLE_OR_SO[(t as usize) + 4],
        };
        s = uc_1st_if(art, uc) + ws
    }
    s
}

/// Returns true if the subjective is plural. You is assumed singular. A Ranting
pub fn is_subjective_plural(subjective: SubjectPronoun) -> bool {
    (subjective as usize) >= 6
}

/// Inflect a subject pronoun to singular or plural and uppercase first character as indicated
pub(crate) fn pluralize_pronoun(subject: SubjectPronoun, as_plural: bool) -> SubjectPronoun {
    if as_plural == is_subjective_plural(subject) {
        subject
    } else if as_plural {
        match subject {
            SubjectPronoun::I => SubjectPronoun::We,
            SubjectPronoun::Thou => SubjectPronoun::Ye,
            SubjectPronoun::He | SubjectPronoun::She | SubjectPronoun::It => SubjectPronoun::They,
            x => x,
        }
    } else {
        match subject {
            SubjectPronoun::We => SubjectPronoun::I,
            SubjectPronoun::Ye => SubjectPronoun::Thou,
            SubjectPronoun::They => SubjectPronoun::It,
            x => x,
        }
    }
}

#[allow(dead_code)]
/// Given a subject and a verb, inflect it and to_upper() as specified.
pub fn inflect_verb(subject: SubjectPronoun, verb: &str, as_plural: bool, uc: bool) -> ExtCased {
    let verb = verb.trim();

    let (mut s, mut ext) = verb
        .strip_suffix("n't")
        .map_or((verb, ""), |start| (start, "n't"));

    match pluralize_pronoun(subject, as_plural) {
        SubjectPronoun::I => match IrregularPluralVerb::from_str(s) {
            Ok(IrregularPluralVerb::Are) if ext != "n't" => {
                s = IRREGULAR_VERBS_1ST[0];
            }

            Ok(e) => {
                s = IRREGULAR_VERBS_1ST.get((e as usize) + 1).unwrap_or(&s);
            }
            _ => {}
        },
        SubjectPronoun::He | SubjectPronoun::She | SubjectPronoun::It => {
            if let Ok(mut val) = IrregularPluralVerb::from_str(s).map(|e| e as usize) {
                if val >= IrregularPluralVerb::Ve as usize {
                    val -= 1;
                }
                s = IRREGULAR_VERBS_3RD.get(val).unwrap_or(&s);
            } else if s.ends_with(&['s', 'o', 'x']) || s.ends_with("ch") || s.ends_with("sh") {
                ext = "es";
            } else if let Some(p) = s
                .strip_suffix('y')
                .filter(|p| !p.ends_with(&['a', 'e', 'u', 'o']))
            {
                s = p;
                ext = "ies";
            } else {
                ext = "s";
            }
        }
        _ => {}
    }
    ExtCased { s, ext, uc }
}

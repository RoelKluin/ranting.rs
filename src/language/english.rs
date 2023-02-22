// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.
use super::english_shared::SubjectPronoun;
use crate::is_subjective_plural;
use crate::uc_1st_if;
use std::str::FromStr;
use strum_macros::EnumString;

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

static IRREGULAR_VERBS_1ST: [&str; 4] = ["am", "aint", "was", "'m"];
static IRREGULAR_VERBS_3RD: [&str; 5] = ["is", "was", "'s", "has", "does"];

/// Given a subject and a verb, inflect it and to_upper() as specified.
pub(crate) fn inflect_verb(subject: &str, verb: &str, as_plural: bool, uc: bool) -> String {
    let verb = verb.trim();

    let (mut s, ext) = verb
        .strip_suffix("n't")
        .map_or((verb, ""), |start| (start, "n't"));

    match pluralize_pronoun(subject, as_plural) {
        "I" => {
            match IrregularPluralVerb::from_str(s) {
                Ok(IrregularPluralVerb::Are) if ext != "n't" => s = IRREGULAR_VERBS_1ST[0],
                Ok(e) => s = IRREGULAR_VERBS_1ST.get((e as usize) + 1).unwrap_or(&s),
                _ => {}
            }
            uc_1st_if(s, uc) + ext
        }
        "he" | "she" | "it" => {
            if let Ok(mut val) = IrregularPluralVerb::from_str(s).map(|e| e as usize) {
                if val >= IrregularPluralVerb::Ve as usize {
                    val -= 1;
                }
                uc_1st_if(IRREGULAR_VERBS_3RD.get(val).unwrap_or(&s), uc) + ext
            } else if s.ends_with(&['s', 'o', 'x']) || s.ends_with("ch") || s.ends_with("sh") {
                uc_1st_if(s, uc) + "es"
            } else if let Some(p) = s
                .strip_suffix('y')
                .filter(|p| !p.ends_with(&['a', 'e', 'u', 'o']))
            {
                uc_1st_if(p, uc) + "ies"
            } else {
                uc_1st_if(s, uc) + "s"
            }
        }
        _ => uc_1st_if(s, uc) + ext,
    }
}

/// (for internal use) Given an article, the default, a requested one, inflect and to_upper() it as specified.
pub(crate) fn adapt_article(
    s: &str,
    requested: &str,
    ws: &str,
    as_plural: bool,
    uc: bool,
) -> String {
    if s.is_empty() {
        s.to_string()
    } else {
        let art = match ArticleOrSo::from_str(requested).expect("Not an article") {
            t if t == ArticleOrSo::The || as_plural => ARTICLE_OR_SO[t as usize],
            ArticleOrSo::A => s,
            t => ARTICLE_OR_SO[(t as usize) + 4],
        };
        uc_1st_if(art, uc) + ws
    }
}

/// Inflect a subject pronoun to singular or plural and uppercase first character as indicated
pub(crate) fn pluralize_pronoun(subject: &str, as_plural: bool) -> &str {
    if as_plural == is_subjective_plural(subject) {
        subject
    } else if as_plural {
        match subject {
            "I" => "we",
            "thou" => "ye",
            "he" | "she" | "it" => "they",
            x => x,
        }
    } else {
        match subject {
            "we" => "I",
            "ye" => "thou",
            "they" => "it",
            x => x,
        }
    }
}

/// Inflect possesive pronoun as to_plural indicates. The first character capitalized with uc set.
pub(crate) fn inflect_adjective(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let nr = SubjectPronoun::from_str(pluralized).expect("Not a subject") as usize;
    uc_1st_if(ADJECTIVE_PRONOUN[nr], uc)
}

/// singular-/pluralize subjective with as to_plural and set uc to capitalize first character
pub(crate) fn inflect_subjective(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let nr = SubjectPronoun::from_str(pluralized).expect("Not a subject") as usize;
    uc_1st_if(SUBJECTIVE_PRONOUN[nr], uc)
}

/// Inflect objective pronoun as to_plural indicates. The first character capitalized with uc set.
pub(crate) fn inflect_objective(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let nr = SubjectPronoun::from_str(pluralized).expect("Not a subject") as usize;
    uc_1st_if(OBJECTIVE_PRONOUN[nr], uc)
}

/// Inflect possesive pronoun as to_plural indicates. The first character capitalized with uc set.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// # fn main() {
///     let boots = Noun::new("shoe", "it");
///     let man = Noun::new("Old man", "he");
///
/// assert_eq!(say!("{can `man pair of #0 boots remain} singular?", 2),
///     "Can his pair of two shoes remain singular?".to_string());
/// # }
/// ```
pub fn inflect_possesive(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let nr = SubjectPronoun::from_str(pluralized).expect("Not a subject") as usize;
    uc_1st_if(POSSESIVE_PRONOUN[nr], uc)
}

static OBJECTIVE_PRONOUN: [&str; 9] =
    ["me", "you", "thee", "him", "her", "it", "us", "you", "them"];

static POSSESIVE_PRONOUN: [&str; 9] = [
    "my", "your", "thy", "his", "her", "its", "our", "your", "their",
];

static ADJECTIVE_PRONOUN: [&str; 9] = [
    "mine", "yours", "thine", "his", "hers", "its", "ours", "yours", "theirs",
];

static SUBJECTIVE_PRONOUN: [&str; 9] = ["I", "you", "thou", "he", "she", "it", "we", "ye", "they"];

#[cfg(test)]
mod tests {
    use ranting::*;
    #[test]
    fn singular_subjective() {
        for subject in ["I", "you", "thou", "she", "he", "it"].into_iter() {
            assert!(!is_subjective_plural(subject));
        }
    }
    #[test]
    fn plural_subjective() {
        for subject in ["we", "ye", "they"].into_iter() {
            assert!(is_subjective_plural(subject));
        }
    }
    #[test]
    fn pluralize_pronoun() {
        assert_eq!(
            ["I", "you", "she", "he", "it", "we", "they"]
                .iter()
                .map(|s| {
                    let word = Noun::new(format!("subject {s}").as_str(), s);
                    say!("{=0 are} - for {0}.", word)
                })
                .collect::<String>(),
            "I am - for subject I.\
         You are - for subject you.\
         She is - for subject she.\
         He is - for subject he.\
         It is - for subject it.\
         We are - for subject we.\
         They are - for subject they."
                .to_string()
        );
    }
    #[test]
    fn verbs_allow() {
        assert_eq!(
            ["I", "you", "he", "we", "they"]
                .iter()
                .map(|s| {
                    let w = Noun::new("", s);
                    say!("{=w're} {can ?w} {?w see} {?w may} {do =w}, {=w've}, {=w were}. ")
                })
                .collect::<String>(),
            "I'm can see may do I, I've, I was. \
     You're can see may do you, you've, you were. \
     He's can sees may does he, he's, he was. \
     We're can see may do we, we've, we were. \
     They're can see may do they, they've, they were. "
                .to_string()
        );
    }
    #[test]
    fn verbs_deny() {
        assert_eq!(
            ["I", "you", "it", "we", "they"]
                .iter()
                .map(|s| {
                    let w = Noun::new("", s);
                    say!("{=w don't}, {=w can't}, {=?w won't}, {=?w mustn't}, {=?w haven't}, {=?w weren't}, or {weren't =w}? ")
                })
                .collect::<String>(),
            "I don't, I can't, won't, mustn't, haven't, wasn't, or wasn't I? \
     You don't, you can't, won't, mustn't, haven't, weren't, or weren't you? \
     It doesn't, it can't, won't, mustn't, hasn't, wasn't, or wasn't it? \
     We don't, we can't, won't, mustn't, haven't, weren't, or weren't we? \
     They don't, they can't, won't, mustn't, haven't, weren't, or weren't they? "
                .to_string()
        );
    }
    #[test]
    fn article_or_so() {
        let ball = Noun::new("ball", "it");
        assert_eq!(
            say!("{a 0}, {the 0}, {these 0}, {those 0}", ball),
            "A ball, the ball, this ball, that ball".to_string()
        )
    }
    #[test]
    fn upper() {
        assert_eq!(["I", "you", "she", "they"]
     .iter()
     .map(|s| {
         let w = Noun::new("one", s);
         say!("{=?w're} {the w}? {=?w'd} say for {`w}self! {=?w've} got here all of {@w}. ")
        })
     .collect::<String>(),
     "'M the one? 'D say for myself! 'Ve got here all of me. \
     'Re the one? 'D say for yourself! 'Ve got here all of you. \
     'S the one? 'D say for herself! 'S got here all of her. \
     'Re the one? 'D say for theirself! 'Ve got here all of them. "
     .to_string());
    }
}

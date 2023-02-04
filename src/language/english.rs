// (c) Roel Kluin 2022 GPL v3
//!
//! Functions used by [Ranting](../ranting_derive/index.html) trait placeholders.
use super::english_shared::{pluralize_pronoun, SubjectPronoun};
use super::roman_shared::Cased;
use strum_macros::EnumString;

static OBJECTIVE_PRONOUN: [&str; 9] =
    ["me", "you", "thee", "him", "her", "it", "us", "you", "them"];

static POSSESIVE_PRONOUN: [&str; 9] = [
    "my", "your", "thy", "his", "her", "its", "our", "your", "their",
];

static ADJECTIVE_PRONOUN: [&str; 9] = [
    "mine", "yours", "thine", "his", "hers", "its", "ours", "yours", "theirs",
];

static SUBJECTIVE_PRONOUN: [&str; 9] = ["I", "you", "thou", "he", "she", "it", "we", "ye", "they"];

#[derive(EnumString, Copy, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum IrregularPluralVerb {
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

/// Return the adjective for a subject or panic.
pub(crate) fn adjective<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = ADJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
}

/// singular-/pluralize subjective with as_plural and set uc to capitalize first character
pub fn inflect_subjective<'a>(subject: SubjectPronoun, as_plural: bool, uc: bool) -> Cased<'a> {
    let subject = pluralize_pronoun(subject, as_plural);
    let s = SUBJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
}

/// Return the objective for a subjective. Can panic. see pluralize_pronoun().
pub(crate) fn objective<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = OBJECTIVE_PRONOUN[subject as usize];
    Cased { s, uc }
}

/// Return the objective for a subjective. Can panic. see pluralize_pronoun().
pub(crate) fn possesive<'a>(subject: SubjectPronoun, uc: bool) -> Cased<'a> {
    let s = POSSESIVE_PRONOUN[subject as usize];
    Cased { s, uc }
}

#[cfg(test)]
mod tests {
    use crate::language::english_shared::{adapt_article, inflect_verb};
    use ranting::*;
    use std::str::FromStr;
    #[test]
    fn singular_subjective() {
        for subject in ["I", "you", "thou", "she", "he", "it"]
            .into_iter()
            .map(SubjectPronoun::from_str)
        {
            assert!(!is_subjective_plural(subject.unwrap()));
        }
    }
    #[test]
    fn plural_subjective() {
        for subject in ["we", "ye", "they"]
            .into_iter()
            .map(SubjectPronoun::from_str)
        {
            assert!(is_subjective_plural(subject.unwrap()));
        }
    }
    fn get_subject(word: Noun) -> String {
        say!("{=word are} - for {word}.")
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
    fn article_or_so() {
        let ball = Noun::new("ball", "it");
        assert_eq!(
            say!("{a 0}, {the 0}, {these 0}, {those 0}", ball),
            "A ball, the ball, this ball, that ball".to_string()
        )
    }
    fn upper() {
        assert_eq!(["I", "you", "she", "they"]
     .iter()
     .map(|s| {
         let w = Noun::new("", s);
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

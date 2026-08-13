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

#[deny(clippy::wildcard_enum_match_arm)]
impl ArticleOrSo {
    fn plural_or_definite(self) -> &'static str {
        match self {
            ArticleOrSo::The => "the",
            ArticleOrSo::A => "some",
            ArticleOrSo::These => "these",
            ArticleOrSo::Those => "those",
        }
    }

    fn singular_demonstrative(self) -> Option<&'static str> {
        // no wildcard arms: exhaustiveness is the invariant here
        match self {
            ArticleOrSo::These => Some("this"),
            ArticleOrSo::Those => Some("that"),
            ArticleOrSo::The | ArticleOrSo::A => None,
        }
    }
}

#[derive(EnumString, Copy, Clone, PartialEq, Eq, Debug, strum_macros::EnumIter)]
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

#[deny(clippy::wildcard_enum_match_arm)]
impl IrregularPluralVerb {
    fn first_person(self) -> Option<&'static str> {
        use IrregularPluralVerb::*;
        // no wildcard arms: exhaustiveness is the invariant here
        match self {
            Are => Some("am"),
            Were => Some("was"),
            Re => Some("'m"),
            Ve | Have | Do | D | Had | Could | Would | Should | Might | Must | Can | May
            | Shall | Will | Ca | Wo => None,
        }
    }

    fn third_person(self) -> Option<&'static str> {
        use IrregularPluralVerb::*;
        // no wildcard arms: exhaustiveness is the invariant here
        match self {
            Are => Some("is"),
            Were => Some("was"),
            Re | Ve => Some("'s"),
            Have => Some("has"),
            Do => Some("does"),
            D | Had | Could | Would | Should | Might | Must | Can | May | Shall | Will | Ca
            | Wo => None,
        }
    }
}

/// Given a subject and a verb, inflect it and to_upper() as specified.
pub(crate) fn inflect_verb(subject: &str, verb: &str, as_plural: bool, uc: bool) -> String {
    let verb = verb.trim();

    let (mut s, ext) = verb
        .strip_suffix("n't")
        .map_or((verb, ""), |start| (start, "n't"));

    match pluralize_pronoun(subject, as_plural) {
        "I" => {
            if let Ok(verb_enum) = IrregularPluralVerb::from_str(s)
                && let Some(form) = verb_enum.first_person()
            {
                s = form;
            }
            uc_1st_if(s, uc) + ext
        }
        "he" | "she" | "it" => {
            if let Ok(verb_enum) = IrregularPluralVerb::from_str(s) {
                if let Some(form) = verb_enum.third_person() {
                    return uc_1st_if(form, uc) + ext;
                }
                // Modal verb not in irregular table — return unchanged
                uc_1st_if(s, uc) + ext
            } else if super::verb::detect_tense(s) != super::verb::Tense::Present {
                // Verb is past or continuous (e.g., "walked", "running") — return as-is
                uc_1st_if(s, uc) + ext
            } else if s.ends_with(['s', 'o', 'x']) || s.ends_with("ch") || s.ends_with("sh") {
                uc_1st_if(s, uc) + "es"
            } else if let Some(p) = s
                .strip_suffix('y')
                .filter(|p| !p.ends_with(['a', 'e', 'u', 'o']))
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
        let t =
            ArticleOrSo::from_str(requested).expect("requested string should be a valid article");
        let art = if t == ArticleOrSo::The || as_plural {
            t.plural_or_definite()
        } else if t == ArticleOrSo::A {
            s
        } else {
            t.singular_demonstrative().unwrap_or(s)
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

struct PronounForms {
    subjective: &'static str,
    objective: &'static str,
    possessive: &'static str,
    adjective: &'static str,
    reflexive: &'static str,
}

impl SubjectPronoun {
    fn forms(self) -> PronounForms {
        use SubjectPronoun::*;
        match self {
            I => PronounForms {
                subjective: "I",
                objective: "me",
                possessive: "my",
                adjective: "mine",
                reflexive: "myself",
            },
            You => PronounForms {
                subjective: "you",
                objective: "you",
                possessive: "your",
                adjective: "yours",
                reflexive: "yourself",
            },
            Thou => PronounForms {
                subjective: "thou",
                objective: "thee",
                possessive: "thy",
                adjective: "thine",
                reflexive: "thyself",
            },
            He => PronounForms {
                subjective: "he",
                objective: "him",
                possessive: "his",
                adjective: "his",
                reflexive: "himself",
            },
            She => PronounForms {
                subjective: "she",
                objective: "her",
                possessive: "her",
                adjective: "hers",
                reflexive: "herself",
            },
            It => PronounForms {
                subjective: "it",
                objective: "it",
                possessive: "its",
                adjective: "its",
                reflexive: "itself",
            },
            We => PronounForms {
                subjective: "we",
                objective: "us",
                possessive: "our",
                adjective: "ours",
                reflexive: "ourselves",
            },
            Ye => PronounForms {
                subjective: "ye",
                objective: "you",
                possessive: "your",
                adjective: "yours",
                reflexive: "yourselves",
            },
            They => PronounForms {
                subjective: "they",
                objective: "them",
                possessive: "their",
                adjective: "theirs",
                reflexive: "themselves",
            },
        }
    }
}

/// Inflect possesive pronoun as to_plural indicates. The first character capitalized with uc set.
pub(crate) fn inflect_adjective(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let forms = SubjectPronoun::from_str(pluralized)
        .expect("Not a subject")
        .forms();
    uc_1st_if(forms.adjective, uc)
}

/// singular-/pluralize subjective with as to_plural and set uc to capitalize first character
pub(crate) fn inflect_subjective(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let forms = SubjectPronoun::from_str(pluralized)
        .expect("Not a subject")
        .forms();
    uc_1st_if(forms.subjective, uc)
}

/// Inflect objective pronoun as to_plural indicates. The first character capitalized with uc set.
pub(crate) fn inflect_objective(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let forms = SubjectPronoun::from_str(pluralized)
        .expect("Not a subject")
        .forms();
    uc_1st_if(forms.objective, uc)
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
    let forms = SubjectPronoun::from_str(pluralized)
        .expect("Not a subject")
        .forms();
    uc_1st_if(forms.possessive, uc)
}

/// Inflect reflexive pronoun (myself, yourself, himself, herself, itself, thyself,
/// ourselves, yourselves, themselves) as to_plural indicates. The first character
/// capitalized with uc set.
///
/// `pluralize_pronoun` leaves "you" as "you" regardless of number (English doesn't
/// distinguish singular/plural "you" in its subjective/objective/possessive forms),
/// so unlike the other `inflect_*` functions here, reflexive "you" is special-cased
/// on `to_plural` directly to choose between "yourself" and "yourselves".
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// # fn main() {
///     let alex = Noun::new("Alex", "they");
///
/// assert_eq!(say!("{alex hurt} {%alex}."), "Alex hurt themselves.".to_string());
/// # }
/// ```
pub fn inflect_reflexive(subject: &str, to_plural: bool, uc: bool) -> String {
    let pluralized = pluralize_pronoun(subject, to_plural);
    let reflexive = if pluralized == "you" {
        if to_plural { "yourselves" } else { "yourself" }
    } else {
        SubjectPronoun::from_str(pluralized)
            .expect("Not a subject")
            .forms()
            .reflexive
    };
    uc_1st_if(reflexive, uc)
}

/// Look up singular/plural form in irregular noun table.
/// Returns the inflected form if found in table, or None to trigger fallback to regular rules.
/// Lookup is case-insensitive; caller is responsible for applying capitalization via uc_1st_if.
pub fn inflect_noun_irregular(noun_form: &str, to_plural: bool) -> Option<String> {
    use super::plurals::{IRREGULAR_PLURALS, IRREGULAR_SINGULARS};

    let key_lower = noun_form.to_lowercase();
    if to_plural {
        IRREGULAR_PLURALS
            .iter()
            .find(|(s, _)| s.to_lowercase() == key_lower)
            .map(|(_, p)| p.to_string())
    } else {
        IRREGULAR_SINGULARS
            .iter()
            .find(|(p, _)| p.to_lowercase() == key_lower)
            .map(|(_, s)| s.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::super::english_shared::SubjectPronoun;
    use super::{IrregularPluralVerb, inflect_noun_irregular};
    use ranting::*;

    #[test]
    fn test_inflect_noun_irregular_plurals() {
        // Test a few common irregular plurals
        assert_eq!(
            inflect_noun_irregular("child", true),
            Some("children".to_string())
        );
        assert_eq!(
            inflect_noun_irregular("person", true),
            Some("people".to_string())
        );
        assert_eq!(
            inflect_noun_irregular("mouse", true),
            Some("mice".to_string())
        );
    }

    #[test]
    fn test_inflect_noun_irregular_singulars() {
        // Test reverse lookup
        assert_eq!(
            inflect_noun_irregular("children", false),
            Some("child".to_string())
        );
        assert_eq!(
            inflect_noun_irregular("people", false),
            Some("person".to_string())
        );
        assert_eq!(
            inflect_noun_irregular("mice", false),
            Some("mouse".to_string())
        );
    }

    #[test]
    fn test_inflect_noun_irregular_not_found() {
        // Regular nouns should return None (fallback to suffix rules)
        assert_eq!(inflect_noun_irregular("book", true), None);
        assert_eq!(inflect_noun_irregular("books", false), None);
    }

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
        assert_eq!(
            ["I", "you", "she", "they"]
                .iter()
                .map(|s| {
                    let w = Noun::new("one", s);
                    say!("{=?w're} {the w}? {=?w'd} say for {%w}! {=?w've} got here all of {@w}. ")
                })
                .collect::<String>(),
            "'M the one? 'D say for myself! 'Ve got here all of me. \
     'Re the one? 'D say for yourself! 'Ve got here all of you. \
     'S the one? 'D say for herself! 'S got here all of her. \
     'Re the one? 'D say for themselves! 'Ve got here all of them. "
                .to_string()
        );
    }

    #[test]
    fn irregular_verb_forms_match_expected_table() {
        use strum::IntoEnumIterator;
        #[rustfmt::skip]
        const EXPECTED_VERB_FORMS: &[(IrregularPluralVerb, Option<&str>, Option<&str>)] = &[
            (IrregularPluralVerb::Are, Some("am"), Some("is")),
            (IrregularPluralVerb::Were, Some("was"), Some("was")),
            (IrregularPluralVerb::Re, Some("'m"), Some("'s")),
            (IrregularPluralVerb::Ve, None, Some("'s")),
            (IrregularPluralVerb::Have, None, Some("has")),
            (IrregularPluralVerb::Do, None, Some("does")),
            (IrregularPluralVerb::D, None, None),
            (IrregularPluralVerb::Had, None, None),
            (IrregularPluralVerb::Could, None, None),
            (IrregularPluralVerb::Would, None, None),
            (IrregularPluralVerb::Should, None, None),
            (IrregularPluralVerb::Might, None, None),
            (IrregularPluralVerb::Must, None, None),
            (IrregularPluralVerb::Can, None, None),
            (IrregularPluralVerb::May, None, None),
            (IrregularPluralVerb::Shall, None, None),
            (IrregularPluralVerb::Will, None, None),
            (IrregularPluralVerb::Ca, None, None),
            (IrregularPluralVerb::Wo, None, None),
        ];
        assert_eq!(
            IrregularPluralVerb::iter().count(),
            EXPECTED_VERB_FORMS.len(),
            "variant count mismatch"
        );
        for (v, expected_first, expected_third) in EXPECTED_VERB_FORMS {
            assert_eq!(
                v.first_person(),
                *expected_first,
                "first_person mismatch for {:?}",
                v
            );
            assert_eq!(
                v.third_person(),
                *expected_third,
                "third_person mismatch for {:?}",
                v
            );
        }
    }

    #[test]
    fn pronoun_forms_match_expected_table() {
        use strum::IntoEnumIterator;
        #[rustfmt::skip]
        const EXPECTED: &[(SubjectPronoun, &str, &str, &str, &str, &str)] = &[
            (SubjectPronoun::I, "I", "me", "my", "mine", "myself"),
            (SubjectPronoun::You, "you", "you", "your", "yours", "yourself"),
            (SubjectPronoun::Thou, "thou", "thee", "thy", "thine", "thyself"),
            (SubjectPronoun::He, "he", "him", "his", "his", "himself"),
            (SubjectPronoun::She, "she", "her", "her", "hers", "herself"),
            (SubjectPronoun::It, "it", "it", "its", "its", "itself"),
            (SubjectPronoun::We, "we", "us", "our", "ours", "ourselves"),
            (SubjectPronoun::Ye, "ye", "you", "your", "yours", "yourselves"),
            (SubjectPronoun::They, "they", "them", "their", "theirs", "themselves"),
        ];
        assert_eq!(
            SubjectPronoun::iter().count(),
            EXPECTED.len(),
            "variant count mismatch"
        );
        for (p, expected_subj, expected_obj, expected_poss, expected_adj, expected_refl) in EXPECTED
        {
            let f = p.forms();
            assert_eq!(
                (
                    f.subjective,
                    f.objective,
                    f.possessive,
                    f.adjective,
                    f.reflexive
                ),
                (
                    *expected_subj,
                    *expected_obj,
                    *expected_poss,
                    *expected_adj,
                    *expected_refl
                ),
                "pronoun forms mismatch for {:?}",
                p
            );
        }
    }

    #[test]
    fn inflect_reflexive_all_pronouns() {
        use super::inflect_reflexive;
        // pass each pronoun's own natural plurality through, mirroring how
        // handle_placeholder derives `to_plural` from the noun/subject itself.
        assert_eq!(
            ["I", "you", "thou", "he", "she", "it", "we", "ye", "they"]
                .iter()
                .map(|s| inflect_reflexive(s, is_subjective_plural(s), false))
                .collect::<Vec<_>>(),
            vec![
                "myself",
                "yourself",
                "thyself",
                "himself",
                "herself",
                "itself",
                "ourselves",
                "yourselves",
                "themselves",
            ]
        );
    }

    #[test]
    fn inflect_reflexive_you_pluralizes_by_flag() {
        use super::inflect_reflexive;
        assert_eq!(inflect_reflexive("you", false, false), "yourself");
        assert_eq!(inflect_reflexive("you", true, false), "yourselves");
    }

    #[test]
    fn inflect_reflexive_uc() {
        use super::inflect_reflexive;
        assert_eq!(inflect_reflexive("they", true, true), "Themselves");
        assert_eq!(inflect_reflexive("it", false, true), "Itself");
    }
}

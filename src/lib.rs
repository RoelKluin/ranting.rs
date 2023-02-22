// (c) Roel Kluin 2022 GPL v3
//!
//! Functions to handle [Ranting](https://docs.rs/ranting_derive/0.2.1/ranting_derive/) trait placeholders.
//!
//! ## Feature flags
#![doc = document_features::document_features!()]

extern crate self as ranting;

mod language;
use lazy_static::lazy_static;
use regex::Regex;

#[doc(hidden)]
pub use english_numbers::convert_no_fmt as rant_convert_numbers;

// required for ranting_derive
#[doc(hidden)]
pub use strum_macros as rant_strum_macros;

use in_definite::get_a_or_an;
pub use language::english::inflect_possesive;
use language::english::{
    adapt_article, inflect_adjective, inflect_objective, inflect_subjective, inflect_verb,
};
pub use language::english_shared::{is_subject, is_subjective_plural};

// TODO: make this a feature:
//pub(crate) use strum_macros;

/// A wrapper for `return Ok(say!())`
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, ack, Ranting};
/// fn question(harr: Noun, friends: Noun, lad: Noun) -> Result<String, String> {
///     ack!("{harr shall} {+=friends do} with {the drunken *lad}?");
/// }
///
/// # fn main() {
/// let harr = Noun::new("what", "it");
/// let friends = Noun::new("crew", "we");
/// let lad = Noun::new("sailor", "he");
///
/// assert_eq!(
///     question(harr, friends, lad),
///     Ok("What shall we do with the drunken sailor?".to_string())
/// );
/// # }
/// ```
pub use ranting_derive::ack;

/// A wrapper for `return Err(say!())`
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, nay, Ranting};
/// fn home(p: Noun) -> Result<String, String> {
///     nay!("{=p can't} get in {`p} house.")
/// }
///
/// # fn main() {
/// assert_eq!(
///     home(Noun::new("Jo", "she")),
///     Err("She can't get in her house.".to_string())
/// );
/// # }
/// ```
pub use ranting_derive::nay;

/// Functions like `format!()` for normal placeholders, but allows extended placeholders including
/// e.g. articles or verbs beside a Noun or a variable with the Ranting trait. These are inflected
/// accordingly, and adjustable by punctuation prefixes.
///
/// # Examples
///
/// ```rust
/// # use ranting::{Noun, say, Ranting};
/// fn inflect(with: Noun) -> String {
///     let n = Noun::new("noun", "it");
///     say!("{some n} with {0} {?n inflect} as {=0}, {@0}, {`0} and {~0}.", with)
/// }
///
/// # fn main() {
///
/// assert_eq!(["I", "you", "he", "she", "it", "we", "they"]
///     .iter()
///     .map(|s| inflect(Noun::new(format!("subject {s}").as_str(), s)))
///     .collect::<String>(),
///     "A noun with subject I inflects as I, me, my and mine.\
///     A noun with subject you inflects as you, you, your and yours.\
///     A noun with subject he inflects as he, him, his and his.\
///     A noun with subject she inflects as she, her, her and hers.\
///     A noun with subject it inflects as it, it, its and its.\
///     A noun with subject we inflects as we, us, our and ours.\
///     A noun with subject they inflects as they, them, their and theirs."
///     .to_string());
/// # }
/// ```
pub use ranting_derive::say;

/// If you want to implement Ranting on a `Box<&dyn Trait>` where Trait has Ranting
pub use ranting_derive::boxed_ranting_trait;

/// If you want to implement Ranting on a `&'_ dyn Trait` where Trait has Ranting
pub use ranting_derive::ref_ranting_trait;

fn get_article_or_so<R>(noun: &R, s: &str, space: &str, as_pl: bool, uc: bool) -> Option<String>
where
    R: Ranting,
{
    if noun.skip_article() && !s.starts_with('!') && !matches!(s, "these" | "those") {
        return Some("".to_string());
    }
    match s.trim_start_matches('!') {
        "the" => Some(uc_1st_if(s, uc)),
        "a" | "an" | "some" => {
            let singular = noun.inflect(false, false);
            let a_or_an = uc_1st_if(get_a_or_an(&singular), uc);
            Some(ranting::adapt_article(&a_or_an, s, space, as_pl, uc))
        }
        "these" | "those" => Some(ranting::adapt_article(s, s, space, as_pl, uc)),
        _ => None,
    }
}

/// The say macro parses placeholders and passes captures to this function which returns a string.
pub fn handle_placeholder<R>(
    noun: &R,
    poss: String,
    nr: String,
    mut uc: bool,
    caps: [&str; 5],
) -> String
where
    R: Ranting,
{
    lazy_static! {
        static ref OF: Regex = Regex::new(r"\bof\s+$").unwrap();
    }
    let [mut pre, plurality, noun_space, case, mut post] = caps;
    let has_possesive = pre.contains('`');
    let singular_post_verb = OF.is_match(pre); // e.g. "{a set of $ten are} still singular"

    let as_pl = match plurality {
        "" => noun.is_plural(),
        "+" => true,
        "-" => false,
        // A bit hackish but should work also for e.g. 1.0%
        "#" => nr.trim_start() != "one",
        _ => {
            let s = nr.trim_start();
            s != "1" && s.split('.').next() != Some("1")
        }
    };
    let pre_string = pre.replace('`', poss.as_str());

    let mut space;
    (pre, space) = split_at_find_end(&pre_string, |c: char| !c.is_whitespace())
        .unwrap_or((pre_string.as_str(), ""));

    let mut etc1;
    (pre, etc1) = split_at_find_start(pre, |c| c.is_whitespace()).unwrap_or((pre, ""));

    let subjective = noun.subjective();
    let mut res = String::new();

    // This may be an article or certain verbs that can occur before the noun:
    if !pre.is_empty() {
        let p = pre.to_lowercase();
        if let Some(a) = get_article_or_so(noun, p.as_str(), space, as_pl, uc) {
            res.push_str(&a);
        } else if has_possesive {
            res.push_str(&uc_1st_if(pre, uc));
        } else {
            assert!(post.is_empty(), "verb before and after?");
            let verb = inflect_verb(subjective, p.as_str(), as_pl, uc);
            res.push_str(&verb);
            if !etc1.is_empty() {
                let art_space;
                (art_space, etc1) =
                    split_at_find_start(etc1, |c| !c.is_whitespace()).unwrap_or(("", etc1));
                res.push_str(art_space);
                let s;
                (s, etc1) = split_at_find_start(etc1, |c| c.is_whitespace()).unwrap_or((etc1, ""));
                if let Some(a) = get_article_or_so(noun, s, space, as_pl, false) {
                    res.push_str(&a);
                } else {
                    res.push_str(s);
                }
            }
        }
        res.push_str(etc1);
        res.push_str(space);
        uc = false;
    }
    if !plurality.contains('?') {
        res.push_str(&nr);
    }

    (space, post) = split_at_find_start(post, |c: char| !c.is_whitespace()).unwrap_or(("", post));

    if case != "?" {
        res.push_str(noun_space);
        let s = match case {
            "=" => inflect_subjective(subjective, as_pl, uc),
            "@" => inflect_objective(subjective, as_pl, uc),
            "`" => inflect_possesive(subjective, as_pl, uc),
            "~" => inflect_adjective(subjective, as_pl, uc),
            _ => noun.inflect(as_pl, uc),
        };
        res.push_str(&s);
        res.push_str(space);
        uc = false;
    }
    let etc2;
    (etc2, post) = split_at_find_end(post, |c| c.is_whitespace()).unwrap_or(("", post));

    res.push_str(etc2);
    if !post.is_empty() {
        match post {
            "'" | "'s" => {
                res.push_str(adapt_possesive_s(noun, as_pl));
            }
            v => {
                let verb = inflect_verb(subjective, v, !singular_post_verb && as_pl, uc);
                res.push_str(&verb);
            }
        }
    }
    res
}

/// upper cases first character if uc is true, or second in a contraction.
pub fn uc_1st_if(s: &str, uc: bool) -> String {
    if uc {
        let mut c = s.chars();
        c.next()
            .map(|t| match t {
                '\'' => {
                    t.to_string()
                        + &c.next()
                            .map(|c| c.to_uppercase().collect::<String>())
                            .unwrap_or_default()
                }
                _ => t.to_uppercase().collect::<String>(),
            })
            .unwrap_or_default()
            + c.as_str()
    } else {
        s.to_string()
    }
}

fn split_at_find_start(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.find(fun).map(|u| s.split_at(u))
}

fn split_at_find_end(s: &str, fun: fn(char) -> bool) -> Option<(&str, &str)> {
    s.rfind(fun).map(|u| s.split_at(u + 1))
}

/// Has the Ranting trait. Often you may want to `#[derive(Ranting)]` and sometimes override some
/// of the trait functions.
#[derive(ranting_derive::Ranting)]
// By setting name and subject to "$", these must come from the struct.
#[ranting(name = "$", subject = "$")]
pub struct Noun {
    name: String,
    subject: String,
}
impl Noun {
    pub fn new(name: &str, subject: &str) -> Self {
        assert!(is_subject(subject), "not a subject");
        Noun {
            name: name.to_string(),
            subject: subject.to_string(),
        }
    }
}

/// convert to `'s` or `'` as appropriate for singular or plural of a noun.
///
/// # Examples
///
/// ```rust
/// # use ranting::*;
/// # fn main() {
///
/// let school = Noun::new("school", "it");
/// let principal = Noun::new("principal", "she");
/// let myles = Noun::new("Myles", "he");
///
/// assert_eq!(say!("{the school'} {principal are} also {myles'}, but only one of all {the +school's} {+principal} in town."),
///     "The school's principal is also Myles's, but only one of all the schools' principals in town.".to_string());
/// # }
/// ```
// a combined plural may require some tricks: "The star and cross' design was pattented by Bob."
fn adapt_possesive_s(noun: &dyn Ranting, asked_plural: bool) -> &str {
    if asked_plural && !is_name(noun) {
        "'"
    } else {
        "'s"
    }
}
fn is_name(noun: &dyn Ranting) -> bool {
    noun.name(false)
        .trim_start_matches('\'')
        .starts_with(|c: char| c.is_uppercase())
}

/// The trait required for a struct or enum to function as a noun in a placeholder, derived with `#[derive_ranting]`.
/// Functions are used in `say!()` placeholders replacements.
///
/// # Examples
///
/// ```
/// # use std::str::FromStr;
/// # use ranting::*;
/// # use ranting_derive::*;
/// #[derive_ranting]
/// #[ranting(subject = "you", plural_you = true)]
/// struct OpponentTeam {}
///
/// #[derive_ranting]
/// #[ranting(subject = "he")]
/// struct ChessPlayer {}
///
/// fn big_words_to<T: Ranting>(who: T) -> String {
///     say!("I will grant {@0} {`0} fight, but {=0 are} going to lose today.", who)
/// }
///
/// # fn main() {
/// let team = OpponentTeam {};
/// assert_eq!(big_words_to(team),
///     "I will grant you your fight, but you are going to lose today.");
///
/// let magnus = ChessPlayer {};
/// assert_eq!(big_words_to(magnus),
///     "I will grant him his fight, but he is going to lose today.");
/// # }
/// ```
// By overriding functions one can adapt default behavior, which affects the
// [placeholder](https://docs.rs/ranting_derive/0.2.1/ranting_derive/) behavior.
pub trait Ranting: std::fmt::Display {
    /// return the name, which is struct name or the `#{ranting(name = "..")]` value, or self.name
    /// if the name attribute was set to "$"
    fn name(&self, uc: bool) -> String;
    /// return the subject: "it" or the `#{ranting(subject = "..")]` value; self.subject if "$".
    fn subjective(&self) -> &str;
    /// return if plural (the subject, or if you, the `#{ranting(plural_you = "true/false")]` value,
    /// default false
    // if the subject can be "you" in both forms, you may want to override the function.
    fn is_plural(&self) -> bool;
    /// return the singular or plural form as configured, starting with capital if uc is set.
    /// use `#{ranting(singular_end = "..", plural_end = "..")]` if not plural = singular + "s"
    // if name can change this should be overridden to lookup each singular_end and plural_end:
    fn inflect(&self, to_plural: bool, uc: bool) -> String;
    /// If an article is only required when emphasizing, set `#{ranting(no_article = "true")]`,
    /// and this function will return accordingly (used by placeholders).
    // examples: Names, languages, elements, food grains, meals (unless particular), sports.
    // if name can change and sometimes goes without article (e.g. a sport) lookup & override:
    fn skip_article(&self) -> bool;
}

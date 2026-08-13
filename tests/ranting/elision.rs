// (c) Roel Kluin 2026 MIT
// Phonological elision / contraction (ROADMAP.md Phase 6 item 7).
//
// `a`/`an` is the only article choice in the crate that depends on the *word
// after* the article, and it is hard-coded English phonology inside
// `get_article_or_so`. `inflect_article_custom` cannot express its equivalent in
// any other language, for a structural reason rather than a missing parameter:
// it returns its string *before* the following text has been rendered. French
// `l'homme` vs `le chien`, Italian `lo`/`il`/`l'` and Portuguese article fusion
// all need the two words side by side.
//
// `Ranting::elide_article_custom` is the post-assembly answer: it is handed the
// rendered article, the whitespace between, and the rendered following text, and
// returns one fused string replacing all three — or `None` to keep them exactly
// as they were, which is what leaves English byte-identical.
use ranting::*;
use std::cell::RefCell;
use std::fmt;

/// Does this word begin with a vowel sound, for French elision purposes?
///
/// French elides before vowels and before *mute* h (`l'homme`), but not before
/// aspirate h (`le héros`) — a lexical property, not a phonological one, which
/// is exactly why the crate must not try to decide it. Here the test lexicon
/// carries it per-word.
fn starts_with_vowel(word: &str) -> bool {
    word.chars()
        .next()
        .is_some_and(|c| "aeiouâàéèêîïôûùAEIOUÂÀÉÈÊÎÏÔÛÙ".contains(c))
}

/// A French noun: a word, a gender, and whether it elides a preceding article.
struct FrenchNoun {
    word: &'static str,
    plural: &'static str,
    class: NounClass,
    /// `true` for a vowel or mute h (`homme`), `false` for aspirate h (`héros`).
    elides: bool,
}

impl FrenchNoun {
    fn new(word: &'static str, plural: &'static str, gender: &'static str) -> Self {
        FrenchNoun {
            word,
            plural,
            class: NounClass::new(gender),
            elides: starts_with_vowel(word),
        }
    }
    /// A word beginning with `h` whose elision behavior must be declared: mute h
    /// elides (`l'homme`), aspirate h does not (`le héros`).
    fn with_h(word: &'static str, plural: &'static str, gender: &'static str, mute: bool) -> Self {
        FrenchNoun {
            word,
            plural,
            class: NounClass::new(gender),
            elides: mute,
        }
    }
}

impl fmt::Display for FrenchNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.word)
    }
}

impl Ranting for FrenchNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.word, uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        uc_1st_if(if to_plural { self.plural } else { self.word }, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }
    fn noun_class(&self) -> NounClass {
        self.class
    }

    /// le / la / les — gender and number, no elision yet: at this point the noun
    /// has not been rendered, so `homme` and `chien` are indistinguishable here.
    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        if article != "the" {
            return None;
        }
        let form = match (as_plural, class.as_str()) {
            (true, _) => "les",
            (false, "feminine") => "la",
            (false, _) => "le",
        };
        Some(uc_1st_if(form, uc))
    }

    /// …and here it has been. `le`/`la` + vowel or mute h → `l'`, no space.
    fn elide_article_custom(
        &self,
        article: &str,
        _separator: &str,
        following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        if !self.elides {
            return None;
        }
        match article {
            "le" | "la" => Some(format!("l'{following}")),
            "Le" | "La" => Some(format!("L'{following}")),
            _ => None,
        }
    }
}

/// The worked example the roadmap item asks for.
#[test]
fn french_elision_l_apostrophe_versus_le() {
    let homme = FrenchNoun::with_h("homme", "hommes", "masculine", true);
    let chien = FrenchNoun::new("chien", "chiens", "masculine");
    assert_eq!(say!("Voici {the homme}."), "Voici l'homme.".to_string());
    assert_eq!(say!("Voici {the chien}."), "Voici le chien.".to_string());
}

/// Gender still selects the article; elision then overrides both forms alike.
#[test]
fn french_elision_applies_to_both_genders() {
    let ecole = FrenchNoun::new("école", "écoles", "feminine");
    let voiture = FrenchNoun::new("voiture", "voitures", "feminine");
    assert_eq!(say!("Voici {the ecole}."), "Voici l'école.".to_string());
    assert_eq!(
        say!("Voici {the voiture}."),
        "Voici la voiture.".to_string()
    );
}

/// Aspirate h keeps the full article: the elision decision is lexical, carried
/// by the entity, and the hook is free to decline per-noun.
#[test]
fn french_aspirate_h_does_not_elide() {
    let heros = FrenchNoun::with_h("héros", "héros", "masculine", false);
    assert_eq!(say!("Voici {the heros}."), "Voici le héros.".to_string());
}

/// Plural `les` never elides in this lexicon, and the hook sees the plural noun.
#[test]
fn french_plural_article_is_untouched() {
    let homme = FrenchNoun::with_h("homme", "hommes", "masculine", true);
    assert_eq!(say!("Voici {the +homme}."), "Voici les hommes.".to_string());
}

/// Sentence-initial: the article arrives at the hook already capitalized, which
/// is why the hook takes no `uc` — there is nothing left for it to decide.
#[test]
fn elision_receives_the_article_already_capitalized() {
    let homme = FrenchNoun::with_h("homme", "hommes", "masculine", true);
    assert_eq!(say!("{the homme} arrive."), "L'homme arrive.".to_string());
}

// ---------------------------------------------------------------------------
// Italian lo / il / l' — the item's other stated motivation. One hook body,
// three outputs, chosen from the rendered noun.
// ---------------------------------------------------------------------------

struct ItalianNoun(&'static str);

impl fmt::Display for ItalianNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Ranting for ItalianNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.0, uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(&self, _to_plural: bool, uc: bool) -> String {
        uc_1st_if(self.0, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        (article == "the").then(|| uc_1st_if("il", uc))
    }

    /// `il` → `l'` before a vowel, `lo` before s+consonant, z, gn, ps.
    fn elide_article_custom(
        &self,
        article: &str,
        _separator: &str,
        following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        if article != "il" {
            return None;
        }
        let lo = following.starts_with('z')
            || following.starts_with("gn")
            || following.starts_with("ps")
            || (following.starts_with('s')
                && following
                    .chars()
                    .nth(1)
                    .is_some_and(|c| !"aeiou".contains(c)));
        if starts_with_vowel(following) {
            Some(format!("l'{following}"))
        } else if lo {
            Some(format!("lo {following}"))
        } else {
            None
        }
    }
}

#[test]
fn italian_lo_il_l_apostrophe() {
    let libro = ItalianNoun("libro");
    let studente = ItalianNoun("studente");
    let amico = ItalianNoun("amico");
    let zaino = ItalianNoun("zaino");
    assert_eq!(say!("Ecco {the libro}."), "Ecco il libro.".to_string());
    assert_eq!(
        say!("Ecco {the studente}."),
        "Ecco lo studente.".to_string()
    );
    assert_eq!(say!("Ecco {the amico}."), "Ecco l'amico.".to_string());
    assert_eq!(say!("Ecco {the zaino}."), "Ecco lo zaino.".to_string());
}

// ---------------------------------------------------------------------------
// What the hook is actually handed, pinned.
// ---------------------------------------------------------------------------

thread_local! {
    static CALLS: RefCell<Vec<(String, String, String, bool)>> = const { RefCell::new(Vec::new()) };
}

/// Records every elision call and always declines, so output is unchanged and
/// the recording alone is what the assertions read.
struct Probe;

impl fmt::Display for Probe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "chien")
    }
}

impl Ranting for Probe {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("chien", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        uc_1st_if(if to_plural { "chiens" } else { "chien" }, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn elide_article_custom(
        &self,
        article: &str,
        separator: &str,
        following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        as_plural: bool,
    ) -> Option<String> {
        CALLS.with(|c| {
            c.borrow_mut().push((
                article.to_string(),
                separator.to_string(),
                following.to_string(),
                as_plural,
            ))
        });
        None
    }
}

fn probe_calls() -> Vec<(String, String, String, bool)> {
    CALLS.with(|c| c.borrow().clone())
}

#[test]
fn declining_hook_leaves_output_untouched() {
    let p = Probe;
    assert_eq!(say!("{the p} barks."), "The chien barks.".to_string());
    assert_eq!(say!("I hear {a p}."), "I hear a chien.".to_string());
}

/// What `separator` and `following` actually contain, including the case where
/// a number is rendered between the article and the noun — `following` is the
/// *rendered* text of everything between the separator and the post-noun slot,
/// number included.
#[test]
fn probe_records_what_the_hook_is_handed() {
    CALLS.with(|c| c.borrow_mut().clear());
    let p = Probe;
    let n = 2;
    let _ = say!("{the p}");
    let _ = say!("{the $n p}");
    let _ = say!("{The p} barks.");
    assert_eq!(
        probe_calls(),
        vec![
            (
                "The".to_string(),
                " ".to_string(),
                "chien".to_string(),
                false
            ),
            (
                "The".to_string(),
                " ".to_string(),
                "2 chiens".to_string(),
                true
            ),
            (
                "The".to_string(),
                " ".to_string(),
                "chien".to_string(),
                false
            ),
        ]
    );
}

/// A hidden noun (`{?the p}`) renders nothing to elide against, so the hook is
/// not called at all — the same reachability boundary as `de` + `le` → `du`.
#[test]
fn hidden_noun_does_not_reach_the_hook() {
    CALLS.with(|c| c.borrow_mut().clear());
    let p = Probe;
    let _ = say!("{?the p}");
    assert!(probe_calls().is_empty());
}

// ---------------------------------------------------------------------------
// English must be byte-identical: `a`/`an` never routes through the hook.
// ---------------------------------------------------------------------------

#[test]
fn english_a_an_is_unchanged() {
    let apple = Noun::new("apple", "it");
    let pear = Noun::new("pear", "it");
    let hour = Noun::new("hour", "it");
    assert_eq!(say!("I see {an apple}."), "I see an apple.".to_string());
    assert_eq!(say!("I see {a pear}."), "I see a pear.".to_string());
    assert_eq!(say!("I see {an hour}."), "I see an hour.".to_string());
    assert_eq!(say!("I see {the +apple}."), "I see the apples.".to_string());
    assert_eq!(
        say!("I see {these +pear}."),
        "I see these pears.".to_string()
    );
    assert_eq!(
        say!("I see {those +apple}."),
        "I see those apples.".to_string()
    );
    // …and sentence-initially, where the article carries the capital.
    assert_eq!(say!("{an apple} fell."), "An apple fell.".to_string());
}

// ---------------------------------------------------------------------------
// Wrappers: Many (one-item rule), Maybe, Box, and the composed Many<Box<T>>.
// ---------------------------------------------------------------------------

#[test]
fn wrappers_delegate_elision() {
    let boxed = Box::new(FrenchNoun::with_h("homme", "hommes", "masculine", true));
    assert_eq!(say!("Voici {the boxed}."), "Voici l'homme.".to_string());

    let maybe = Maybe(Some(FrenchNoun::with_h(
        "homme",
        "hommes",
        "masculine",
        true,
    )));
    assert_eq!(say!("Voici {the maybe}."), "Voici l'homme.".to_string());

    let one = Many(vec![FrenchNoun::with_h(
        "homme",
        "hommes",
        "masculine",
        true,
    )]);
    assert_eq!(say!("Voici {the one}."), "Voici l'homme.".to_string());

    let composed = Many(vec![Box::new(FrenchNoun::with_h(
        "homme",
        "hommes",
        "masculine",
        true,
    ))]);
    assert_eq!(say!("Voici {the composed}."), "Voici l'homme.".to_string());
}

/// A 2+ item `Many` keeps the English default — the same one-item rule as
/// `noun_class()`/`capitalize()`, for the same reason: `following` is then the
/// joined phrase, whose members may elide differently. Note the article is
/// English `the` as well: the *article* hook follows the same rule, so there is
/// nothing French left to elide.
#[test]
fn many_with_two_items_does_not_elide() {
    let two = Many(vec![
        FrenchNoun::with_h("homme", "hommes", "masculine", true),
        FrenchNoun::new("chien", "chiens", "masculine"),
    ]);
    assert_eq!(
        say!("Voici {the two}."),
        "Voici the homme and chien.".to_string()
    );
}

/// The chained article after a pre-noun verb (`{are the homme}`) is the one
/// adjacent to the noun, so it is the one elision applies to.
#[test]
fn chained_article_after_a_verb_elides() {
    let homme = FrenchNoun::with_h("homme", "hommes", "masculine", true);
    assert_eq!(
        say!("Voici {are the homme}."),
        "Voici is l'homme.".to_string()
    );
}

// (c) Roel Kluin 2026 MIT
// Preposition-article fusion (ROADMAP.md Phase 6 item 26,
// docs/superpowers/specs/2026-08-13-preposition-fusion.md option (b)).
//
// German `zu dem` -> `zum`, `in dem` -> `im`; Spanish `de el` -> `del`, `a el`
// -> `al` — a preposition and the article rendered immediately after it
// contract into one word. The preposition is template *literal* text sitting
// outside the placeholder's `{...}`, so `inflect_article_custom` (which
// renders before the following text exists) and `elide_article_custom`
// (whose span starts at the article, never before it) could not reach this,
// which is why `ranting_i18n`'s README recorded it as hole 7 and
// `ranting_es`'s as hole 1. `ranting_derive`'s `parse_str_params` now
// captures that literal word and forwards it as data
// (`PlaceholderSpec::preposition`) instead of baking it as inert text, so
// `Ranting::inflect_preposition_custom` can see it.
use ranting::*;
use std::cell::RefCell;
use std::fmt;

/// A German noun fixed to the dative case's article ("dem"), just enough to
/// exercise fusion — full case declension is `tests/ranting/grammatical_case.rs`'s
/// concern, not this file's.
struct GermanHaus;

impl fmt::Display for GermanHaus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Haus")
    }
}

impl Ranting for GermanHaus {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("Haus", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(&self, _to_plural: bool, uc: bool, _case: GrammaticalCase) -> String {
        uc_1st_if("Haus", uc)
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
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        (article == "the").then(|| uc_1st_if("dem", uc))
    }

    /// `zu` + `dem` -> `zum`, `in` + `dem` -> `im`, `an` + `dem` -> `am`. Any
    /// other preposition (or a `"dem"` this lexicon didn't produce) declines.
    fn inflect_preposition_custom(
        &self,
        preposition: &str,
        article: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        match (preposition, article) {
            ("zu", "dem") => Some("zum".to_string()),
            ("in", "dem") => Some("im".to_string()),
            ("an", "dem") => Some("am".to_string()),
            _ => None,
        }
    }
}

#[test]
fn german_zu_dem_fuses_to_zum() {
    let haus = GermanHaus;
    assert_eq!(say!("zu {the haus}.", haus), "zum Haus.".to_string());
}

#[test]
fn german_in_dem_fuses_to_im() {
    let haus = GermanHaus;
    assert_eq!(say!("in {the haus}.", haus), "im Haus.".to_string());
}

/// A preposition this lexicon doesn't fuse leaves the article, separator and
/// noun exactly as rendered — the same "decline, don't guess" contract every
/// other `_custom` hook has.
#[test]
fn unmapped_preposition_leaves_output_untouched() {
    let haus = GermanHaus;
    assert_eq!(say!("über {the haus}.", haus), "über dem Haus.".to_string());
}

// ---------------------------------------------------------------------------
// Spanish de + el -> del, a + el -> al — the item's other stated motivation.
// ---------------------------------------------------------------------------

struct SpanishGato;

impl fmt::Display for SpanishGato {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "gato")
    }
}

impl Ranting for SpanishGato {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("gato", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(&self, _to_plural: bool, uc: bool, _case: GrammaticalCase) -> String {
        uc_1st_if("gato", uc)
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
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        (article == "the").then(|| uc_1st_if("el", uc))
    }

    fn inflect_preposition_custom(
        &self,
        preposition: &str,
        article: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        match (preposition, article) {
            ("de", "el") => Some("del".to_string()),
            ("a", "el") => Some("al".to_string()),
            _ => None,
        }
    }
}

#[test]
fn spanish_de_el_fuses_to_del() {
    let gato = SpanishGato;
    assert_eq!(
        say!("Vengo de {the gato}.", gato),
        "Vengo del gato.".to_string()
    );
}

#[test]
fn spanish_a_el_fuses_to_al() {
    let gato = SpanishGato;
    assert_eq!(say!("Voy a {the gato}.", gato), "Voy al gato.".to_string());
}

// ---------------------------------------------------------------------------
// English must be byte-identical: no template literal word ever routes
// through the hook unless a Ranting impl chooses to answer it.
// ---------------------------------------------------------------------------

#[test]
fn english_prepositions_are_never_fused() {
    let house = Noun::new("house", "it");
    assert_eq!(say!("in {the house}.", house), "in the house.".to_string());
    assert_eq!(say!("of {the house}.", house), "of the house.".to_string());
}

/// A placeholder with no preceding literal word at all is unaffected —
/// `PlaceholderSpec::preposition` is `None`, so the fusion attempt never
/// happens, exactly like the pre-existing `at_sentence_start` boolean it sits
/// alongside.
#[test]
fn placeholder_with_no_preceding_word_is_unaffected() {
    let haus = GermanHaus;
    assert_eq!(
        say!("{the haus} steht dort.", haus),
        "Dem Haus steht dort.".to_string()
    );
}

// ---------------------------------------------------------------------------
// What the hook is actually handed, pinned.
// ---------------------------------------------------------------------------

thread_local! {
    static CALLS: RefCell<Vec<(String, String, bool)>> = const { RefCell::new(Vec::new()) };
}

/// Records every preposition-fusion call and always declines, so output is
/// unchanged and the recording alone is what the assertions read.
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
    fn inflect(&self, to_plural: bool, uc: bool, _case: GrammaticalCase) -> String {
        uc_1st_if(if to_plural { "chiens" } else { "chien" }, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_preposition_custom(
        &self,
        preposition: &str,
        article: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        CALLS.with(|c| {
            c.borrow_mut()
                .push((preposition.to_string(), article.to_string(), as_plural))
        });
        None
    }
}

fn probe_calls() -> Vec<(String, String, bool)> {
    CALLS.with(|c| c.borrow().clone())
}

#[test]
fn probe_records_what_the_hook_is_handed() {
    CALLS.with(|c| c.borrow_mut().clear());
    let p = Probe;
    assert_eq!(say!("de {the p}.", p), "de the chien.".to_string());
    assert_eq!(
        probe_calls(),
        vec![("de".to_string(), "the".to_string(), false)]
    );
}

/// The `?`-before-noun case marker (`{the ?p}`, hiding the *noun*, not the
/// preposition) renders no noun to fuse against — same reachability boundary
/// `elide_article_custom` already has, since the fusion attempt sits inside
/// the same `case != Hidden` block.
#[test]
fn hidden_noun_does_not_reach_the_hook() {
    CALLS.with(|c| c.borrow_mut().clear());
    let p = Probe;
    assert_eq!(say!("de {the ?p}", p), "de the".to_string());
    assert!(probe_calls().is_empty());
}

// ---------------------------------------------------------------------------
// Wrappers: Many (one-item rule), Maybe, Box.
// ---------------------------------------------------------------------------

#[test]
fn wrappers_delegate_preposition_fusion() {
    let boxed = Box::new(GermanHaus);
    assert_eq!(say!("zu {the boxed}.", boxed), "zum Haus.".to_string());

    let maybe = Maybe(Some(GermanHaus));
    assert_eq!(say!("zu {the maybe}.", maybe), "zum Haus.".to_string());

    let one = Many(vec![GermanHaus]);
    assert_eq!(say!("zu {the one}.", one), "zum Haus.".to_string());
}

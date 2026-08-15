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
        capitalize_if("Haus", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        _to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if("Haus", uc)
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
        (article == "the").then(|| capitalize_if("dem", uc))
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
        capitalize_if("gato", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        _to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if("gato", uc)
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
        (article == "the").then(|| capitalize_if("el", uc))
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
        capitalize_if("chien", uc)
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        capitalize_if(if to_plural { "chiens" } else { "chien" }, uc)
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

// ---------------------------------------------------------------------------
// The three post-assembly splices, in one placeholder
// ---------------------------------------------------------------------------

/// Overrides all three post-assembly hooks at once — preposition fusion, numeral elision and
/// article elision. No such fixture existed before `docs/architecture-review-2026-08-15.md` §1.1,
/// which is why the ordering defect it records was reachable but untested.
struct AllThree;

impl std::fmt::Display for AllThree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("gato")
    }
}

impl Ranting for AllThree {
    fn name(&self, _uc: bool) -> String {
        "gato".to_string()
    }
    fn subjective(&self) -> &str {
        "it"
    }
    fn is_plural(&self) -> bool {
        false
    }
    fn inflect(
        &self,
        _to_plural: bool,
        _uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        "gato".to_string()
    }
    fn skip_article(&self) -> bool {
        false
    }
    fn inflect_article_custom(
        &self,
        _article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        Some("el".to_string())
    }
    fn inflect_preposition_custom(
        &self,
        _preposition: &str,
        _article: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        Some("del".to_string())
    }
    fn inflect_numeral_custom(
        &self,
        _numeral: &str,
        count: Option<i64>,
        _style: NumeralStyle,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        // Deliberately multibyte: the defect's worst form was a panic off a `char` boundary, and
        // an ASCII numeral would only have produced silently wrong text.
        Some(format!("«{}»", count?))
    }
    fn elide_numeral_custom(
        &self,
        numeral: &str,
        _separator: &str,
        following: &str,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
    ) -> Option<String> {
        Some(format!("{numeral}{following}"))
    }
}

#[test]
fn all_three_splices_in_one_placeholder() {
    // `docs/architecture-review-2026-08-15.md` §1.1. The numeral splice runs first, on the
    // innermost boundary, so preposition fusion and article elision both still see valid spans
    // and pick up the already-fused numeral+noun as their trailing text.
    let n = AllThree;
    assert_eq!(say!("Vengo de {the $0 1}", 2, n), "Vengo del «2»gato");

    // Each splice on its own, so a future failure says which one moved.
    assert_eq!(say!("{the $0 1}", 2, n), "el «2»gato");
    assert_eq!(say!("Vengo de {the 0}", n), "Vengo del gato");
}

#[test]
fn the_numeral_splice_sees_its_own_text_not_a_displaced_window() {
    // The defect's signature: after preposition fusion shifted every later byte, the numeral
    // splice sliced a window out of the middle of the rendered text — the hook received `"> g"`
    // out of `"<2> gato"`. With multibyte output the same displaced index panics instead, which
    // is why this fixture's numeral is `«2»`. Asserting the hook's *inputs* rather than only the
    // final string, since the old code could still produce a correct-looking result by accident.
    use std::cell::RefCell;

    thread_local! {
        static SEEN: RefCell<Vec<(String, String, String)>> = const { RefCell::new(Vec::new()) };
    }

    struct Spy;
    impl std::fmt::Display for Spy {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("gato")
        }
    }
    impl Ranting for Spy {
        fn name(&self, _uc: bool) -> String {
            "gato".to_string()
        }
        fn subjective(&self) -> &str {
            "it"
        }
        fn is_plural(&self) -> bool {
            false
        }
        fn inflect(
            &self,
            _t: bool,
            _u: bool,
            _c: GrammaticalCase,
            _n: Option<PlaceholderCount>,
        ) -> String {
            "gato".to_string()
        }
        fn skip_article(&self) -> bool {
            false
        }
        fn inflect_article_custom(
            &self,
            _a: &str,
            _s: &str,
            _c: GrammaticalCase,
            _cl: NounClass,
            _p: bool,
            _n: Option<PlaceholderCount>,
            _uc: bool,
        ) -> Option<String> {
            Some("el".to_string())
        }
        fn inflect_preposition_custom(
            &self,
            _p: &str,
            _a: &str,
            _c: GrammaticalCase,
            _cl: NounClass,
            _pl: bool,
            _n: Option<PlaceholderCount>,
            _uc: bool,
        ) -> Option<String> {
            Some("del".to_string())
        }
        fn inflect_numeral_custom(
            &self,
            _num: &str,
            count: Option<i64>,
            _s: NumeralStyle,
            _c: GrammaticalCase,
            _cl: NounClass,
            _p: bool,
        ) -> Option<String> {
            Some(format!("«{}»", count?))
        }
        fn elide_numeral_custom(
            &self,
            numeral: &str,
            separator: &str,
            following: &str,
            _c: GrammaticalCase,
            _cl: NounClass,
            _p: bool,
            _n: Option<PlaceholderCount>,
        ) -> Option<String> {
            SEEN.with(|s| {
                s.borrow_mut().push((
                    numeral.to_string(),
                    separator.to_string(),
                    following.to_string(),
                ))
            });
            None
        }
    }

    let _ = say!("Vengo de {the $0 1}", 2, Spy);
    SEEN.with(|s| {
        let seen = s.borrow();
        assert_eq!(seen.len(), 1, "the numeral hook should be called once");
        assert_eq!(
            seen[0],
            ("«2»".to_string(), " ".to_string(), "gato".to_string())
        );
    });
}

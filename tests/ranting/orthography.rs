// (c) Roel Kluin 2026 MIT
// Orthography & capitalization hook (ROADMAP.md Phase 6 item 6).
//
// `uc_1st_if` and the sentence-start-uppercase default are English orthographic
// assumptions: German capitalizes every noun wherever it stands, Japanese/Chinese/
// Arabic/Hebrew have no letter case at all so `uc` is meaningless, and Turkish
// needs `i`→`İ` which `char::to_uppercase` gets wrong for a Turkish locale.
// `Ranting::capitalize`/`capitalize_with_context` is the single place those
// decisions are now made; its default is exactly `uc_1st_if(word, uc)`, so English
// output is unchanged unless a fork overrides it.
use ranting::*;
use ranting_derive::{derive_ranting, say_with};
use std::cell::RefCell;
use std::fmt;

// ============================================================================
// A German-style noun: capitalized wherever it stands, not just sentence-initially
// ============================================================================

/// "Hund" (dog), masculine — deliberately stored *lowercase* so that every
/// capital in the rendered output has to come from the hook, not from the data.
#[derive(Clone, Copy)]
struct Hund;

impl fmt::Display for Hund {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "hund")
    }
}

impl Ranting for Hund {
    fn name(&self, _uc: bool) -> String {
        "hund".to_string()
    }
    fn subjective(&self) -> &str {
        "he"
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
        "hund".to_string()
    }
    fn skip_article(&self) -> bool {
        false
    }
    fn noun_class(&self) -> NounClass {
        NounClass::new("masculine")
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if article != "the" {
            return None;
        }
        Some(uc_1st_if(
            match case {
                GrammaticalCase::Objective => "den",
                _ => "der",
            },
            uc,
        ))
    }

    /// The German rule: nouns are capitalized regardless of sentence position.
    /// Every other role keeps English's sentence-initial-only behavior.
    fn capitalize(
        &self,
        word: &str,
        role: OrthographyRole,
        uc: bool,
        _sentence_start: bool,
    ) -> String {
        match role {
            OrthographyRole::Noun => uc_1st_if(word, true),
            _ => uc_1st_if(word, uc),
        }
    }
}

/// Byte-identical to `Hund` except that it does not override `capitalize` — the
/// control that shows the capital comes from the hook and not from the data.
#[derive(Clone, Copy)]
struct HundOhneHook;

impl fmt::Display for HundOhneHook {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "hund")
    }
}

impl Ranting for HundOhneHook {
    fn name(&self, _uc: bool) -> String {
        "hund".to_string()
    }
    fn subjective(&self) -> &str {
        "he"
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
        "hund".to_string()
    }
    fn skip_article(&self) -> bool {
        false
    }
}

#[test]
fn german_noun_is_capitalized_mid_sentence() {
    // Mid-sentence is the point: `uc` is false here, so English capitalizes
    // nothing and only an always-capitalize hook can produce "Hund".
    assert_eq!(say!("Heute bellt {the 0}.", Hund), "Heute bellt der Hund.");
    assert_eq!(
        say!("Heute bellt {the 0}.", HundOhneHook),
        "Heute bellt the hund."
    );
}

#[test]
fn german_noun_capital_does_not_leak_into_other_roles() {
    // Sentence-initial: the article gets English's `uc` (via the noun's own
    // article hook), the noun gets its unconditional capital, and the two are
    // independent decisions made under different `OrthographyRole`s.
    assert_eq!(say!("{The 0} bellt.", Hund), "Der Hund bellt.");
}

// ============================================================================
// A caseless script: the hook as a no-op
// ============================================================================

/// Stands in for Japanese/Chinese/Arabic/Hebrew: there is no letter case, so
/// sentence-initial uppercasing is not a thing that can be done. Latin letters
/// are used only so the assertions stay readable in this file.
#[derive(Clone, Copy)]
struct Neko;

impl fmt::Display for Neko {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "neko")
    }
}

impl Ranting for Neko {
    fn name(&self, _uc: bool) -> String {
        "neko".to_string()
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
        "neko".to_string()
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn capitalize(
        &self,
        word: &str,
        _role: OrthographyRole,
        _uc: bool,
        _sentence_start: bool,
    ) -> String {
        word.to_string()
    }
}

#[test]
fn caseless_language_hook_suppresses_sentence_start_uppercase() {
    // Article and verb both sit on fallback paths that would call `uc_1st_if`.
    // (Verbs are written in the plural in a placeholder and inflect from there.)
    assert_eq!(say!("{a 0 are} here.", Neko), "a neko is here.");
    // Pronoun path, likewise.
    assert_eq!(say!("{=0 are} here.", Neko), "it is here.");
}

// ============================================================================
// Which role each site reports
// ============================================================================

thread_local! {
    static SEEN: RefCell<Vec<(OrthographyRole, String, bool)>> = const { RefCell::new(Vec::new()) };
}

#[derive(Clone, Copy)]
struct Probe;

impl fmt::Display for Probe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "thing")
    }
}

impl Ranting for Probe {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("thing", uc)
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
        uc_1st_if(if to_plural { "things" } else { "thing" }, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn capitalize(
        &self,
        word: &str,
        role: OrthographyRole,
        uc: bool,
        _sentence_start: bool,
    ) -> String {
        SEEN.with(|s| s.borrow_mut().push((role, word.to_string(), uc)));
        uc_1st_if(word, uc)
    }
}

fn drain_seen() -> Vec<(OrthographyRole, String, bool)> {
    SEEN.with(|s| s.borrow_mut().drain(..).collect())
}

#[test]
fn each_site_reports_its_own_role() {
    let _ = drain_seen();
    assert_eq!(say!("{The 0 are} here.", Probe), "The thing is here.");
    let seen = drain_seen();
    let roles: Vec<OrthographyRole> = seen.iter().map(|(r, _, _)| *r).collect();
    assert_eq!(
        roles,
        vec![
            OrthographyRole::Article,
            OrthographyRole::Noun,
            OrthographyRole::Verb
        ]
    );
    // `uc` truthfully reports the sentence-initial position for the article.
    assert_eq!(seen[0].1, "the");
    assert!(seen[0].2);
    // The noun is the one site whose word arrives already capitalized-or-not by
    // `inflect()` (which takes `uc` itself), so it is passed `uc: false` — see
    // `Ranting::capitalize`'s docs. Here the article in front of it had already
    // spent the sentence's `uc` anyway, so the name is plain "thing".
    assert_eq!(seen[1].1, "thing");
    assert!(!seen[1].2);

    let _ = drain_seen();
    assert_eq!(say!("{=0} is here.", Probe), "It is here.");
    assert_eq!(
        drain_seen().iter().map(|(r, _, _)| *r).collect::<Vec<_>>(),
        vec![OrthographyRole::Pronoun]
    );

    let _ = drain_seen();
    assert_eq!(say!("{0 !!good} of them.", Probe), "Thing best of them.");
    assert_eq!(
        drain_seen().iter().map(|(r, _, _)| *r).collect::<Vec<_>>(),
        vec![OrthographyRole::Noun, OrthographyRole::Adjective]
    );

    // A tense marker's auxiliary+verb phrase is a Verb too.
    let _ = drain_seen();
    assert_eq!(say!("{=0 >walk} home.", Probe), "It will walk home.");
    assert_eq!(
        drain_seen().iter().map(|(r, _, _)| *r).collect::<Vec<_>>(),
        vec![OrthographyRole::Pronoun, OrthographyRole::Verb]
    );
}

// ============================================================================
// A custom `inflect_*_custom` form still owns its own `uc`
// ============================================================================

/// Records into `SEEN` exactly like `Probe`, but also answers
/// `inflect_article_custom` — so its article never takes the fallback path.
#[derive(Clone, Copy)]
struct ProbeWithCustomArticle;

impl fmt::Display for ProbeWithCustomArticle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "thing")
    }
}

impl Ranting for ProbeWithCustomArticle {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("thing", uc)
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
        uc_1st_if(if to_plural { "things" } else { "thing" }, uc)
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
        uc: bool,
    ) -> Option<String> {
        Some(uc_1st_if("yon", uc))
    }

    fn capitalize(
        &self,
        word: &str,
        role: OrthographyRole,
        uc: bool,
        _sentence_start: bool,
    ) -> String {
        SEEN.with(|s| s.borrow_mut().push((role, word.to_string(), uc)));
        uc_1st_if(word, uc)
    }
}

#[test]
fn custom_inflection_forms_bypass_the_hook() {
    // A hook that returns `Some` owns its own `uc` and never reaches
    // `capitalize` — the pre-existing fallback-path-only contract, unchanged.
    // The noun and verb beside it still route normally.
    let _ = drain_seen();
    assert_eq!(
        say!("{The 0 are} here.", ProbeWithCustomArticle),
        "Yon thing is here."
    );
    let roles: Vec<OrthographyRole> = drain_seen().iter().map(|(r, _, _)| *r).collect();
    assert!(!roles.contains(&OrthographyRole::Article));
    assert_eq!(roles, vec![OrthographyRole::Noun, OrthographyRole::Verb]);
}

// ============================================================================
// `capitalize_with_context`: a locale can live in `NarrationContext::dialect`
// ============================================================================

#[derive(Clone, Copy)]
struct Kedi; // Turkish "kedi" (cat)

impl fmt::Display for Kedi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "it")
    }
}

impl Ranting for Kedi {
    fn name(&self, _uc: bool) -> String {
        "it".to_string()
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
        "it".to_string()
    }
    fn skip_article(&self) -> bool {
        false
    }

    /// Turkish dotted/dotless `i`: `i` uppercases to `İ`, not to `I`. Only the
    /// context-aware hook is overridden, which also shows that overriding one of
    /// the pair is enough — the plain `capitalize` keeps the English default.
    fn capitalize_with_context(
        &self,
        word: &str,
        role: OrthographyRole,
        uc: bool,
        sentence_start: bool,
        ctx: Option<&NarrationContext>,
    ) -> String {
        if uc && ctx.and_then(|c| c.dialect) == Some("tr") {
            let mut chars = word.chars();
            if let Some('i') = chars.next() {
                return format!("İ{}", chars.as_str());
            }
        }
        self.capitalize(word, role, uc, sentence_start)
    }
}

#[test]
fn dialect_selects_turkish_uppercase_i() {
    let tr = NarrationContext::new().dialect("tr");
    assert_eq!(say_with!(tr, "{=0} burada.", Kedi), "İt burada.");

    // Without the dialect the English default applies, and `say!()` — which has
    // no context at all — is unaffected either way.
    let plain = NarrationContext::new();
    assert_eq!(say_with!(plain, "{=0} burada.", Kedi), "It burada.");
    assert_eq!(say!("{=0} burada.", Kedi), "It burada.");
}

// ============================================================================
// Plumbing through Many / Maybe / Box
// ============================================================================

#[test]
fn many_delegates_to_its_single_item_only() {
    let one = Many(vec![Hund]);
    assert_eq!(say!("Heute bellt {the 0}.", one), "Heute bellt der Hund.");

    // Two items are one joined phrase whose members may disagree, so `Many`
    // keeps the English default — the same rule as `noun_class()`. The join
    // itself is unchanged: uppercase first char only, and only when `uc`.
    let two = Many(vec![Hund, Hund]);
    assert_eq!(
        say!("Heute bellen {the 0}.", two),
        "Heute bellen the hund and hund."
    );
    assert_eq!(say!("{The 0} bellen.", two), "The hund and hund bellen.");

    // An empty `Many` has no member to speak for it either (and skips its
    // article, as it did before this hook existed). Since ROADMAP.md Phase 6
    // item 11, a skipped article also swallows the separator that would have
    // followed it, so only one space remains here — the empty noun's own,
    // not the article's.
    let none: Many<Hund> = Many(vec![]);
    assert_eq!(say!("Heute bellen {the 0}.", none), "Heute bellen .");
}

#[test]
fn maybe_and_box_delegate_to_the_inner_value() {
    assert_eq!(
        say!("Heute bellt {the 0}.", Maybe(Some(Hund))),
        "Heute bellt der Hund."
    );
    // `Maybe(None)` skips its article too; see the `Many` test above for why only
    // one space remains after ROADMAP.md Phase 6 item 11 (the empty noun's, not
    // the article's).
    let nothing: Maybe<Hund> = Maybe(None);
    assert_eq!(say!("Heute bellt {the 0}.", nothing), "Heute bellt .");
    assert_eq!(
        say!("Heute bellt {the 0}.", Box::new(Hund)),
        "Heute bellt der Hund."
    );
    // Composed, as documented for the wrappers generally.
    assert_eq!(
        say!("Heute bellt {the 0}.", Many(vec![Box::new(Hund)])),
        "Heute bellt der Hund."
    );
}

// ============================================================================
// `sentence_start`: an explicit signal separate from `uc` (ROADMAP.md Phase 6
// item 17, closing open question 2 of
// docs/superpowers/specs/2026-08-13-word-order-feasibility.md)
// ============================================================================
//
// `uc` conflates "this placeholder is sentence-initial" with "forced uppercase
// by a `^`/`,` marker or an uppercase pre-text word". `sentence_start` is the
// raw signal alone. The two normally agree, but a `,`/`^` marker or an
// uppercase pre-text word can force `uc` independently of sentence position —
// these tests pin the two disagreeing in both directions.

thread_local! {
    static SENTENCE_SEEN: RefCell<Vec<(bool, bool)>> = const { RefCell::new(Vec::new()) };
}

#[derive(Clone, Copy)]
struct SentenceProbe;

impl fmt::Display for SentenceProbe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "thing")
    }
}

impl Ranting for SentenceProbe {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("thing", uc)
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
        uc_1st_if(if to_plural { "things" } else { "thing" }, uc)
    }
    fn skip_article(&self) -> bool {
        false
    }

    fn capitalize(
        &self,
        word: &str,
        _role: OrthographyRole,
        uc: bool,
        sentence_start: bool,
    ) -> String {
        SENTENCE_SEEN.with(|s| s.borrow_mut().push((uc, sentence_start)));
        uc_1st_if(word, uc)
    }
}

fn drain_sentence_seen() -> Vec<(bool, bool)> {
    SENTENCE_SEEN.with(|s| s.borrow_mut().drain(..).collect())
}

#[test]
fn forced_lowercase_marker_keeps_sentence_start_true() {
    // A `,` marker forces `uc == false` even though the placeholder really is
    // sentence-initial (right after ". ") -- `sentence_start` still reports
    // `true`, which `uc` alone could never distinguish from "mid-sentence".
    let _ = drain_sentence_seen();
    assert_eq!(
        say!("Hello. {,=0} is nice.", SentenceProbe),
        "Hello. it is nice."
    );
    let seen = drain_sentence_seen();
    assert!(seen.contains(&(false, true)), "{seen:?}");
}

#[test]
fn uppercase_pre_word_does_not_imply_sentence_start() {
    // "The" starting with an uppercase letter forces `uc == true` on the
    // fallback noun-capitalization site regardless of sentence position; the
    // placeholder here is mid-sentence (no `.`/`?`/`!` before it), so
    // `sentence_start` reports `false` even though `uc` is `true`.
    let _ = drain_sentence_seen();
    assert_eq!(
        say!("meanwhile {The 0} arrived.", SentenceProbe),
        "meanwhile The thing arrived."
    );
    let seen = drain_sentence_seen();
    assert!(seen.contains(&(true, false)), "{seen:?}");
}

#[test]
fn ordinary_sentence_start_agrees_on_both_signals() {
    let _ = drain_sentence_seen();
    assert_eq!(
        say!("{The 0} arrived.", SentenceProbe),
        "The thing arrived."
    );
    let seen = drain_sentence_seen();
    assert!(seen.contains(&(true, true)), "{seen:?}");
}

// ============================================================================
// Regression guards: English output is byte-identical
// ============================================================================

#[derive_ranting]
#[ranting(name = "designer", subject = "they")]
struct Designer {}

#[test]
fn lowercase_name_attribute_still_renders_lowercase() {
    // A derive-generated `name()` reads `uc == true` as "as written", not "force
    // uppercase", so an explicitly lowercase `name` attribute stays lowercase
    // even sentence-initially. That is why the noun site passes `uc` down into
    // `inflect()` and hands the hook `uc: false` rather than routing `uc`
    // through it — routing would silently start capitalizing this.
    let d = Designer {};
    assert_eq!(say!("{+d} arrived."), "designer arrived.");
    assert_eq!(say!("{the d} arrived."), "The designer arrived.");
}

#[test]
fn default_english_output_is_unchanged() {
    let jane = Noun::new("Jane", "she");
    let cat = Noun::new("cat", "it");
    assert_eq!(say!("{=jane} pets {a cat}."), "She pets a cat.".to_string());
    assert_eq!(say!("{The cat are} hers."), "The cat is hers.".to_string());
    assert_eq!(say!("{=cat won't} move."), "It won't move.".to_string());
    // The pre-noun possessive-substitution path (`OrthographyRole::Noun`, and the
    // other site whose word arrives pre-capitalized).
    assert_eq!(say!("{`jane cat} sleeps."), "Her cat sleeps.".to_string());
    assert_eq!(say!("{a cat's} bowl."), "A cat's bowl.".to_string());
    assert_eq!(say!("{%jane} pets {@cat}."), "Herself pets it.".to_string());
}

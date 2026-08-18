// (c) Roel Kluin 2026 MIT
// ROADMAP.md Phase 8 item 3, part (b): `Ranting::is_mass()` is the mass/count flag, orthogonal
// to `NounClass` -- declared with `#[ranting(mass)]` (the derive), or `Noun::with_mass()` (the
// runtime builder). Mirrors `gender` -> `noun_class()`'s mechanism: unset means the trait's own
// `false` default, and nothing renders differently for a type that never declares it.
//
// This is also what fixes the `{a 0}` -> "An information" defect the design spike recorded
// (docs/superpowers/specs/2026-08-15-quantifier-determiners.md): once a noun declares itself
// mass, the `a`/`an`/`some` article slot renders the unstressed `some` on a singular instead of
// guessing `a`/`an` from the noun's first letter/sound.
use ranting::*;

#[test]
fn trait_default_is_false() {
    struct Probe;
    impl std::fmt::Display for Probe {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "dog")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            capitalize_if("dog", uc)
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
            capitalize_if("dog", uc)
        }
        fn skip_article(&self) -> bool {
            false
        }
    }
    assert!(!Probe.is_mass());
}

#[test]
fn derived_mass_attribute_sets_the_flag() {
    #[derive_ranting]
    #[ranting(subject = "it", name = "information", mass)]
    struct Info {}

    // No `mass` attribute at all: the trait default applies, unchanged.
    #[derive_ranting]
    #[ranting(subject = "it", name = "item")]
    struct Item {}

    assert!(Info {}.is_mass());
    assert!(!Item {}.is_mass());
}

#[test]
fn noun_carries_mass_via_with_mass() {
    let info = Noun::new("information", "it").with_mass();
    assert!(info.is_mass());

    // Unset by default.
    let plain = Noun::new("dog", "it");
    assert!(!plain.is_mass());

    // Additivity, from the rendering side: a non-mass noun renders byte-identically to before
    // this channel existed.
    assert_eq!(say!("{a plain}"), "A dog".to_string());
}

#[test]
fn wrappers_delegate_mass_of_their_inner_value() {
    let info = || Noun::new("information", "it").with_mass();

    assert!(Box::new(info()).is_mass());
    assert!(Maybe(Some(info())).is_mass());
    assert!(Many(vec![info()]).is_mass());

    // Nothing to report: an absent `Maybe`, and a `Many` whose members may disagree.
    assert!(!Maybe::<Noun>(None).is_mass());
    assert!(!Many(vec![info(), Noun::new("boot", "it")]).is_mass());
    assert!(!Many::<Noun>(vec![]).is_mass());
}

/// The item's own motivating example, and the recorded hazard: `{a 0}` on "information" used to
/// render "An information" regardless of `is_mass()`, because the `AAnSome` arm ran `get_a_or_an`
/// unconditionally. A mass noun now renders the unstressed `some` instead.
#[test]
fn mass_noun_singular_renders_some_not_a_or_an() {
    let info = Noun::new("information", "it").with_mass();
    assert_eq!(say!("{a info}"), "Some information".to_string());
    assert_eq!(say!("{an info}"), "Some information".to_string());
    assert_eq!(say!("{some info}"), "Some information".to_string());
}

/// A non-mass noun keeps guessing a/an as before -- the fix is additive, only for types that
/// newly declare mass.
#[test]
fn non_mass_noun_singular_is_unaffected() {
    let apple = Noun::new("apple", "it");
    let dog = Noun::new("dog", "it");
    assert_eq!(say!("{a apple}"), "An apple".to_string());
    assert_eq!(say!("{a dog}"), "A dog".to_string());
}

/// A mass noun's plural agreement was already correct (`ArticleOrSo::A`'s `plural_or_definite`
/// already renders "some" for every spelling, singular or plural) -- unaffected by this change.
/// `is_mass()` governs the article slot only; it does not suppress `inflect()`'s own regular
/// pluralization, which is unaware of it, exactly as `NounClass` is.
#[test]
fn mass_noun_plural_is_unaffected() {
    let info = Noun::new("information", "it").with_mass();
    assert_eq!(say!("{a +info}"), "Some informations".to_string());
}

/// The hook is still offered the word first, exactly as the pre-existing article kinds do --
/// `is_mass()` changes only the English fallback behind a declined hook.
#[test]
fn a_fork_can_override_before_the_mass_fallback_runs() {
    struct MassNoun;
    impl std::fmt::Display for MassNoun {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "eau")
        }
    }
    impl Ranting for MassNoun {
        fn name(&self, uc: bool) -> String {
            capitalize_if("eau", uc)
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
            capitalize_if("eau", uc)
        }
        fn skip_article(&self) -> bool {
            false
        }
        fn is_mass(&self) -> bool {
            true
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
            (article == "a" || article == "an" || article == "some")
                .then(|| capitalize_if("quelque", uc))
        }
    }
    assert_eq!(say!("{a 0}", MassNoun), "Quelque eau".to_string());
}

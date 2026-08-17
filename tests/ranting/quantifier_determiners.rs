// (c) Roel Kluin 2026 MIT
// ROADMAP.md Phase 8 item 3, part (a): six agreeing quantifier word/pairs on the existing
// `ArticleKind`/`ArticleOrSo` pair -- `no`, `every`/`all`, `each`, `either`/`neither`,
// `much`/`many`, `less`/`fewer`. See docs/superpowers/specs/2026-08-15-quantifier-determiners.md.
use ranting::*;

fn item() -> Noun {
    Noun::new("item", "it")
}

fn info() -> Noun {
    Noun::new("information", "it").with_mass()
}

/// `no` is number-transparent: it renders itself unchanged whether the noun agreement is
/// singular or plural, unlike every other quantifier here.
#[test]
fn no_is_number_transparent() {
    assert_eq!(say!("{no 0}", item()), "No item".to_string());
    assert_eq!(say!("{no +0}", item()), "No items".to_string());
}

/// The recorded hazard: before this item, `{no $n item}` at `n = 1` rendered "Noes 1 item" --
/// the open-pass word fell through to the pre-noun *verb* path and got conjugated. `no` is now
/// a real `ArticleKind`, so it renders as an invariant word instead.
#[test]
fn no_with_a_numeral_no_longer_conjugates_as_a_verb() {
    assert_eq!(say!("{no $0 1}", 1, item()), "No 1 item".to_string());
    assert_eq!(say!("{no $0 1}", 3, item()), "No 3 items".to_string());
}

/// `every` swaps to the suppletive plural `all` on plural agreement, the same mechanism
/// `these`/`those` -> `this`/`that` already uses.
#[test]
fn every_swaps_to_all_on_plural_agreement() {
    assert_eq!(say!("{every 0}", item()), "Every item".to_string());
    assert_eq!(say!("{every +0}", item()), "All items".to_string());
    // Writing `all` selects the same pair -- the written word picks the paradigm, not the form,
    // same as `these`/`those`.
    assert_eq!(say!("{all 0}", item()), "Every item".to_string());
    assert_eq!(say!("{all +0}", item()), "All items".to_string());
}

/// `each`/`either`/`neither` force singular agreement, baked at compile time exactly as a
/// written `-` marker would be -- so a bare, unmarked placeholder already renders singular.
#[test]
fn each_either_neither_force_singular_agreement() {
    assert_eq!(say!("{each 0}", item()), "Each item".to_string());
    assert_eq!(say!("{either 0}", item()), "Either item".to_string());
    assert_eq!(say!("{neither 0}", item()), "Neither item".to_string());
    // Explicitly writing `-` is redundant, not a contradiction -- still forces singular.
    assert_eq!(say!("{each -0}", item()), "Each item".to_string());
}

/// `much`/`many` and `less`/`fewer` are picked by `is_mass()`, not by number agreement -- the
/// dependency on part (b) that made shipping them before it wrong.
#[test]
fn much_many_pick_by_mass_not_number() {
    assert_eq!(say!("{much 0}", info()), "Much information".to_string());
    assert_eq!(say!("{many 0}", item()), "Many item".to_string());
    assert_eq!(say!("{much +0}", item()), "Many items".to_string());
}

#[test]
fn less_fewer_pick_by_mass_not_number() {
    assert_eq!(say!("{less 0}", info()), "Less information".to_string());
    assert_eq!(say!("{fewer 0}", item()), "Fewer item".to_string());
    assert_eq!(say!("{less +0}", item()), "Fewer items".to_string());
}

/// The zero-count idiom the item names, corrected per the design spike: `` {?#n +items} `` never
/// parsed (`?` is legal only before `$`). The idiom that works is `` {are no ?$n item} ``, with
/// `no` as an inert extra pre word chained onto the modal and the hidden numeral carrying
/// agreement -- and it keeps working exactly as before this item, including through the
/// modal + quantifier chaining fix this item needed (see `ranting_core::ph_ext`'s
/// `match_nested_article_candidates`).
#[test]
fn zero_count_idiom_still_works() {
    assert_eq!(
        say!("There {are no ?$0 1}.", 0, item()),
        "There are no items.".to_string()
    );
    assert_eq!(
        say!("There {are no ?$0 1}.", 1, item()),
        "There is no item.".to_string()
    );
}

/// Every new word reaches `inflect_article_custom_with_context` first, with the same signal set
/// the pre-existing article arms carry -- a fork overrides a quantifier with zero new hook
/// surface, exactly as `.claude/rules/extension-hooks.md`'s `inflect_article_custom` paragraph
/// promises.
#[test]
fn a_fork_can_override_a_quantifier_before_the_english_fallback_runs() {
    struct Probe;
    impl std::fmt::Display for Probe {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "chat")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            capitalize_if("chat", uc)
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
            capitalize_if("chat", uc)
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
            (article == "no").then(|| capitalize_if("aucun", uc))
        }
    }
    assert_eq!(say!("{no 0}", Probe), "Aucun chat".to_string());
}

/// English output for the pre-existing vocabulary is unaffected -- none of this item's changes
/// touch the `the`/`a`/`an`/`some`/`these`/`those` arms except the mass-only `some` fix, which is
/// pinned separately in `tests/ranting/mass_count.rs`.
#[test]
fn pre_existing_articles_are_unaffected() {
    let noun = item();
    assert_eq!(say!("I see {the 0}.", noun), "I see the item.");
    assert_eq!(say!("I see {a 0}.", noun), "I see an item.");
    assert_eq!(say!("I see {these +0}.", noun), "I see these items.");
}

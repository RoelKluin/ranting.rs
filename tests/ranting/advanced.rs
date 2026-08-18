// (c) Roel Kluin 2026 MIT
//
// Advanced examples -- backed tests for docs/ADVANCED.md.
// Each test verifies one scene's code example compiles and produces its documented output.
// Unlike the Cookbook (one feature per recipe), each scene here composes several placeholder
// markers in one passage.

use ranting::*;

#[test]
fn scene_1_introduce_once_refer_invisibly() {
    let jane = Noun::new("Jane", "she");
    assert_eq!(
        say!(
            "{jane}, {the ?jane !!good} in class, {?jane <%receive} a bad mark.",
            jane
        ),
        "Jane, the best in class, had received a bad mark."
    );
}

#[test]
fn scene_2_a_merchants_inventory() {
    let merchant = Noun::new("Merchant", "she");
    let sword = Noun::new("sword", "it");
    assert_eq!(
        say!(
            "{=merchant have} {$n sword} for sale. If you buy them all, {=merchant >give} you a discount.",
            merchant,
            n = 3,
            sword
        ),
        "She has 3 swords for sale. If you buy them all, she will give you a discount."
    );
}

#[test]
fn scene_3_nothing_left_but_plenty_of_gold() {
    let item = Noun::new("item", "it");
    let merchant = Noun::new("Merchant", "she");
    let gold = Noun::new("gold", "it").with_mass();
    assert_eq!(
        say!(
            "There {are no ?$n item} left in the shop. {=merchant have} {much gold}, though.",
            n = 0i64,
            item,
            merchant,
            gold
        ),
        "There are no items left in the shop. She has much gold, though."
    );

    // The idiom's other side: a nonzero count still agrees singular.
    assert_eq!(
        say!(
            "There {are no ?$n item} left in the shop. {=merchant have} {much gold}, though.",
            n = 1i64,
            item,
            merchant,
            gold
        ),
        "There is no item left in the shop. She has much gold, though."
    );
}

#[test]
fn scene_4_only_they_can_decide() {
    let tarzan = Noun::new("Tarzan", "he");
    let jane = Noun::new("Jane", "she");
    let alex = Noun::new("Alex", "they");
    assert_eq!(
        say!(
            "{=tarzan are} {?jane !strong} than {jane}, but {=alex decide} for {%alex} what is {?alex !!good} for {`alex} own future.",
            tarzan,
            jane,
            alex
        ),
        "He is stronger than Jane, but they decide for themselves what is best for their own future."
    );
}

#[test]
fn scene_5_third_times_not_the_charm() {
    let tarzan = Noun::new("Tarzan", "he");
    let attempt = Noun::new("attempt", "it");
    assert_eq!(
        say!(
            "This is {`tarzan} {##n attempt}. If {=tarzan fail} again, this will be {`tarzan} {##m attempt}.",
            tarzan,
            n = 3i64,
            attempt,
            m = 4i64
        ),
        "This is his third attempt. If he fails again, this will be his fourth attempt."
    );
}

#[test]
fn scene_6_the_joneses_menagerie() {
    let joneses = Noun::new("Joneses", "they");
    let owl = Noun::new("owl", "it");
    let goose = Noun::new("goose", "it");
    assert_eq!(
        say!(
            "{the 0's} house has {an owl} and {a +2}.",
            joneses,
            owl,
            goose
        ),
        "The Joneses' house has an owl and some geese."
    );
}

#[test]
fn scene_7_seeing_isnt_always_seeing() {
    enum Sense {
        Sight,
        Insight,
        Judgment,
    }

    struct Watcher {
        noun: Noun,
        sense: Sense,
    }

    impl std::fmt::Display for Watcher {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.noun)
        }
    }

    impl Ranting for Watcher {
        fn subjective(&self) -> &str {
            self.noun.subjective()
        }
        fn name(&self, uc: bool) -> String {
            self.noun.name(uc)
        }
        fn is_plural(&self) -> bool {
            self.noun.is_plural()
        }
        fn skip_article(&self) -> bool {
            self.noun.skip_article()
        }
        fn inflect(
            &self,
            as_pl: bool,
            uc: bool,
            case: GrammaticalCase,
            count: Option<PlaceholderCount>,
        ) -> String {
            self.noun.inflect(as_pl, uc, case, count)
        }

        fn inflect_verb_custom(
            &self,
            subject: &str,
            verb: &str,
            _as_plural: bool,
            _count: Option<PlaceholderCount>,
            uc: bool,
        ) -> Option<String> {
            if verb != "see" {
                return None;
            }
            let synonym = match self.sense {
                Sense::Sight => return None,
                Sense::Insight => "perceive",
                Sense::Judgment => "regard",
            };
            let conjugated = match subject {
                "he" | "she" | "it" => format!("{synonym}s"),
                _ => synonym.to_string(),
            };
            Some(capitalize_if(&conjugated, uc))
        }
    }

    let oracle = Watcher {
        noun: Noun::new("Oracle", "she"),
        sense: Sense::Insight,
    };
    assert_eq!(
        say!("{=oracle see} the truth.", oracle),
        "She perceives the truth."
    );

    let judge = Watcher {
        noun: Noun::new("Judge", "he"),
        sense: Sense::Judgment,
    };
    assert_eq!(
        say!("{=judge see} the case fairly.", judge),
        "He regards the case fairly."
    );

    let guard = Watcher {
        noun: Noun::new("Guard", "they"),
        sense: Sense::Sight,
    };
    assert_eq!(say!("{=guard see} the gate.", guard), "They see the gate.");
}

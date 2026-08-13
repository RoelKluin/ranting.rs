// (c) Roel Kluin 2026 MIT
// The adjective-agreement runtime hook (ROADMAP.md Phase 6 item 5).
//
// English resolves the `!`/`!!` degree markers entirely at compile time
// (`ranting_derive/src/language/adjective.rs`) and needs no agreement, so before
// this hook `ranting` had no runtime adjective path at all. Romance and Germanic
// adjectives agree with their noun in gender, number and (German) case — none of
// which the macro can know — so `inflect_adjective_custom`/`_with_context` get
// the adjective *as written* plus `AdjectiveDegree`, `GrammaticalCase`,
// `NounClass` and the plural bool, and only when they decline does the
// compile-time-baked English degree form get emitted.
//
// The worked example the roadmap item asks for is below: `un chat noir`, `une
// robe noire` and `des chats noirs` from one template and one hook body.
//
// Note the wart this example demonstrates as much as it demonstrates agreement:
// `!` is the *only* post-noun adjective slot the placeholder grammar has (an
// unmarked post-noun word is parsed as a verb, and an adjective written outside
// the placeholder is literal text no hook can reach), so a French fork writes
// `!` for a plain positive-degree adjective and ignores `degree`. See
// docs/EXTENSIBILITY.md §2.5.
use ranting::*;
use std::fmt;

/// One French noun type for both genders and both numbers — deliberately one
/// struct with one hook body, so the three renderings below are provably one
/// code path.
struct FrenchNoun {
    singular: &'static str,
    plural: &'static str,
    class: NounClass,
    is_plural: bool,
}

impl FrenchNoun {
    fn new(singular: &'static str, plural: &'static str, class: &'static str) -> Self {
        FrenchNoun {
            singular,
            plural,
            class: NounClass::new(class),
            is_plural: false,
        }
    }
    fn plural(mut self) -> Self {
        self.is_plural = true;
        self
    }
    fn word(&self) -> &'static str {
        if self.is_plural {
            self.plural
        } else {
            self.singular
        }
    }
}

impl fmt::Display for FrenchNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.word())
    }
}

impl Ranting for FrenchNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.word(), uc)
    }
    fn subjective(&self) -> &str {
        if self.is_plural { "they" } else { "it" }
    }
    fn is_plural(&self) -> bool {
        self.is_plural
    }
    fn inflect(&self, to_plural: bool, uc: bool) -> String {
        uc_1st_if(
            if to_plural {
                self.plural
            } else {
                self.singular
            },
            uc,
        )
    }
    fn skip_article(&self) -> bool {
        false
    }
    fn noun_class(&self) -> NounClass {
        self.class
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        let form = match (article, class.as_str(), as_plural) {
            ("a" | "an" | "some", _, true) => "des",
            ("a" | "an" | "some", "feminine", false) => "une",
            ("a" | "an" | "some", _, false) => "un",
            ("the", _, true) => "les",
            ("the", "feminine", false) => "la",
            ("the", _, false) => "le",
            _ => return None,
        };
        Some(uc_1st_if(form, uc))
    }

    fn inflect_pronoun_custom(
        &self,
        _subject: &str,
        _case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        // As in tests/ranting/noun_class.rs: keep showing the noun's own name
        // after the article instead of collapsing to an English pronoun.
        Some(uc_1st_if(self.word(), uc))
    }

    fn inflect_adjective_custom(
        &self,
        adjective: &str,
        degree: AdjectiveDegree,
        _case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        uc: bool,
    ) -> Option<String> {
        // Agreement: -e for feminine, -s for plural — the whole of regular
        // French adjective inflection, off `class` and `as_plural` alone. The
        // adjective arrives as written (`noir`), never as English's resolved
        // degree form, which is not reversible back into it.
        let mut form = adjective.to_string();
        if class.as_str() == "feminine" {
            form.push('e');
        }
        if as_plural {
            form.push('s');
        }
        // Degree in French is periphrastic and the article agrees too; `!` is
        // the plain positive here, since it is the only adjective slot there is.
        let form = match degree {
            AdjectiveDegree::Comparative => form,
            AdjectiveDegree::Superlative => {
                let determiner = match (class.as_str(), as_plural) {
                    (_, true) => "les",
                    ("feminine", false) => "la",
                    (_, false) => "le",
                };
                format!("{determiner} plus {form}")
            }
        };
        Some(uc_1st_if(&form, uc))
    }
}

#[test]
fn one_template_renders_un_chat_noir_une_robe_noire_des_chats_noirs() {
    let chat = FrenchNoun::new("chat", "chats", "masculine");
    let robe = FrenchNoun::new("robe", "robes", "feminine");
    let chats = FrenchNoun::new("chat", "chats", "masculine").plural();

    // One template. Gender comes off the entity (item 2's `NounClass`), number
    // off the entity's own `is_plural()`, and the adjective agrees with both.
    assert_eq!(say!("J'ai vu {a 0 !noir}.", chat), "J'ai vu un chat noir.");
    assert_eq!(
        say!("J'ai vu {a 0 !noir}.", robe),
        "J'ai vu une robe noire."
    );
    assert_eq!(
        say!("J'ai vu {a 0 !noir}.", chats),
        "J'ai vu des chats noirs."
    );
}

#[test]
fn feminine_plural_agrees_on_both_axes_at_once() {
    let robes = FrenchNoun::new("robe", "robes", "feminine").plural();
    assert_eq!(
        say!("J'ai vu {a 0 !noir}.", robes),
        "J'ai vu des robes noires."
    );
}

#[test]
fn the_degree_marker_reaches_the_hook() {
    // `!` vs `!!` is the one thing about the adjective English *does* decide at
    // compile time, and it is passed through rather than consumed: a fork needs
    // it because French superlatives are periphrastic and take their own article.
    let chat = FrenchNoun::new("chat", "chats", "masculine");
    let robe = FrenchNoun::new("robe", "robes", "feminine");

    assert_eq!(say!("{the 0 !noir}", chat), "Le chat noir");
    assert_eq!(say!("{the 0 !!noir}", chat), "Le chat le plus noir");
    assert_eq!(say!("{the 0 !!noir}", robe), "La robe la plus noire");
}

#[test]
fn the_hook_receives_the_written_adjective_and_the_agreement_inputs() {
    // A probe rather than an assertion on rendered text: what matters is that
    // `adjective` is the placeholder's own word (not English's "more noir"),
    // and that case/class/number arrive alongside it.
    #[derive(Debug, PartialEq, Eq)]
    struct Seen {
        adjective: String,
        degree: AdjectiveDegree,
        case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
    }
    thread_local! {
        static SEEN: std::cell::RefCell<Vec<Seen>> = const { std::cell::RefCell::new(Vec::new()) };
    }

    struct Probe;
    impl fmt::Display for Probe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "chat")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            uc_1st_if("chat", uc)
        }
        fn subjective(&self) -> &str {
            "it"
        }
        fn is_plural(&self) -> bool {
            false
        }
        fn inflect(&self, _to_plural: bool, uc: bool) -> String {
            uc_1st_if("chat", uc)
        }
        fn skip_article(&self) -> bool {
            true
        }
        fn noun_class(&self) -> NounClass {
            NounClass::new("masculine")
        }
        fn inflect_adjective_custom(
            &self,
            adjective: &str,
            degree: AdjectiveDegree,
            case: GrammaticalCase,
            class: NounClass,
            as_plural: bool,
            _uc: bool,
        ) -> Option<String> {
            SEEN.with(|s| {
                s.borrow_mut().push(Seen {
                    adjective: adjective.to_string(),
                    degree,
                    case,
                    class,
                    as_plural,
                })
            });
            None // decline, so the English degree form is emitted unchanged
        }
    }

    // "good" is irregular, so the compile-time form ("better") shares no prefix
    // with the written word — the hook still gets "good".
    // (`Probe::inflect` ignores `to_plural`, so the name stays "chat"; `+` is
    // there to prove the plural bool reaches the hook, not to inflect the noun.)
    assert_eq!(say!("the {+0 !good} one", Probe), "the chat better one");
    SEEN.with(|s| {
        assert_eq!(
            s.borrow().as_slice(),
            &[Seen {
                adjective: "good".to_string(),
                degree: AdjectiveDegree::Comparative,
                case: GrammaticalCase::Name,
                class: NounClass::new("masculine"),
                as_plural: true,
            }]
        )
    });
}

#[test]
fn declining_the_hook_leaves_english_degree_output_untouched() {
    // Additivity: `Noun` never overrides the hook, so every `!`/`!!` placeholder
    // renders exactly as it did before this channel existed (the same forms
    // tests/ranting/comparative_adjectives.rs asserts).
    let w = Noun::new("thing", "it");
    assert_eq!(say!("{?w !good} than that.", w), "Better than that.");
    assert_eq!(say!("{?w !!good} in class", w), "Best in class");
    assert_eq!(say!("a {w !large} one", w), "a thing larger one");
}

#[test]
fn a_custom_form_owns_its_own_capitalization() {
    // Mirrors the article/pronoun hooks: `uc` is passed in and the caller's
    // uppercase-first-char pass applies only on the fallback path.
    let chat = FrenchNoun::new("chat", "chats", "masculine");
    assert_eq!(say!("{?0 !noir} est le mot.", chat), "Noir est le mot.");
}

#[test]
fn the_with_context_hook_is_the_one_called() {
    // Same sentinel shape as tests/ranting/narration_context_threading.rs:
    // overriding only `_with_context` is enough, because that is the call site.
    struct Probe;
    impl fmt::Display for Probe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "chose")
        }
    }
    impl Ranting for Probe {
        fn name(&self, uc: bool) -> String {
            uc_1st_if("chose", uc)
        }
        fn subjective(&self) -> &str {
            "it"
        }
        fn is_plural(&self) -> bool {
            false
        }
        fn inflect(&self, _to_plural: bool, uc: bool) -> String {
            uc_1st_if("chose", uc)
        }
        fn skip_article(&self) -> bool {
            true
        }
        fn inflect_adjective_custom_with_context(
            &self,
            adjective: &str,
            _degree: AdjectiveDegree,
            _case: GrammaticalCase,
            _class: NounClass,
            _as_plural: bool,
            uc: bool,
            ctx: Option<&NarrationContext>,
        ) -> Option<String> {
            // Register is inert in the crate; a fork is free to read it here.
            let form = match ctx.and_then(|c| c.register) {
                Some(Register::Formal) => format!("fort {adjective}"),
                _ => adjective.to_string(),
            };
            Some(uc_1st_if(&form, uc))
        }
    }

    // say!() reaches the `_with_context` hook with ctx: None ...
    assert_eq!(say!("c'est {?0 !beau}", Probe), "c'est beau");
    // ... and say_with!() hands it the context.
    let ctx = NarrationContext {
        register: Some(Register::Formal),
        ..Default::default()
    };
    assert_eq!(say_with!(ctx, "c'est {?0 !beau}", Probe), "c'est fort beau");
}

#[test]
fn wrappers_delegate_the_adjective_hook() {
    // `Many`/`Maybe`/`Box` forward the hook to their single inner value, like
    // every other `_custom` hook (src/collections.rs).
    let boxed = Box::new(FrenchNoun::new("robe", "robes", "feminine"));
    assert_eq!(
        say!("J'ai vu {a 0 !noir}.", boxed),
        "J'ai vu une robe noire."
    );

    let one = Many(vec![FrenchNoun::new("robe", "robes", "feminine")]);
    assert_eq!(say!("J'ai vu {a 0 !noir}.", one), "J'ai vu une robe noire.");

    let some = Maybe(Some(FrenchNoun::new("chat", "chats", "masculine")));
    assert_eq!(say!("J'ai vu {a 0 !noir}.", some), "J'ai vu un chat noir.");

    // A multi-item `Many` has no single item to delegate to, so the hook is not
    // called and English's compile-time form stands.
    let two = Many(vec![
        FrenchNoun::new("chat", "chats", "masculine"),
        FrenchNoun::new("robe", "robes", "feminine"),
    ]);
    // English's degree table, applied to a French word — which is exactly what
    // the hook exists to avoid, and what a fork gets when it can't be reached.
    assert_eq!(say!("J'ai vu {?0 !noir}.", two), "J'ai vu noirer.");
}

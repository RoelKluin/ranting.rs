use ranting::*;
use std::fmt;

// ============================================================================
// Test 1: Custom Verb (Pirate) — `test_custom_verb_pirate`
// ============================================================================

#[derive(Clone, Copy)]
struct PirateNoun;

impl fmt::Display for PirateNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pirate")
    }
}

impl Ranting for PirateNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("pirate", uc)
    }

    fn subjective(&self) -> &str {
        "ye"
    }

    fn is_plural(&self) -> bool {
        true
    }

    fn inflect(
        &self,
        to_plural: bool,
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        if to_plural {
            uc_1st_if("pirates", uc)
        } else {
            uc_1st_if("pirate", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            "have" | "has" => Some(uc_1st_if("have", uc)),
            "do" | "does" => Some(uc_1st_if("do", uc)),
            _ => None,
        }
    }
}

#[test]
fn test_custom_verb_pirate() {
    let pirate = PirateNoun;
    let result = say!("{=0 be} a scallywag.", pirate);
    // Note: subject "ye" displays as "Ye" in English, not "You"
    // The custom verb implementation returns "be" for all forms
    assert_eq!(result, "Ye be a scallywag.".to_string());
}

// ============================================================================
// Test 2: Partial Verb Customization — `test_custom_verb_partial`
// ============================================================================

/// Same hook as `PirateNoun`, but a *singular third person* subject.
///
/// This matters: with a plural subject like "ye", English adds no `-s`, so a hook returning the
/// bare form is indistinguishable from no hook at all. With "he", English would produce "bes" /
/// "has" / "walks", so the hook's effect (and its absence) is observable.
#[derive(Clone, Copy)]
struct PirateCaptain;

impl fmt::Display for PirateCaptain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "captain")
    }
}

impl Ranting for PirateCaptain {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("captain", uc)
    }

    fn subjective(&self) -> &str {
        "he"
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
        if to_plural {
            uc_1st_if("captains", uc)
        } else {
            uc_1st_if("captain", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            "have" | "has" => Some(uc_1st_if("have", uc)),
            "do" | "does" => Some(uc_1st_if("do", uc)),
            _ => None,
        }
    }
}

#[test]
fn test_custom_verb_partial() {
    let captain = PirateCaptain;
    // "have" is customized; without the hook English would conjugate "he has".
    let result = say!("{=0 have} treasure.", captain);
    assert_eq!(result, "He have treasure.".to_string());

    // "be" likewise: English would render "bes" for a third person singular subject.
    let result = say!("{=0 be} a scallywag.", captain);
    assert_eq!(result, "He be a scallywag.".to_string());
}

// ============================================================================
// Test 3: Verb Fallback (None) — `test_custom_verb_fallback`
// ============================================================================

#[test]
fn test_custom_verb_fallback() {
    let captain = PirateCaptain;
    // "walk" is not customized, so the hook returns None and English inflection runs.
    // The "-s" is the proof that the fallback path, not the hook, produced this.
    let result = say!("{=0 walk} forward.", captain);
    assert_eq!(result, "He walks forward.".to_string());
}

// ============================================================================
// Test 4: Custom Pronoun (Formal) — `test_custom_pronoun_formal`
// ============================================================================

#[derive(Clone, Copy)]
struct Dignitary;

impl fmt::Display for Dignitary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dignitary")
    }
}

impl Ranting for Dignitary {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("dignitary", uc)
    }

    fn subjective(&self) -> &str {
        "you"
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
        if to_plural {
            uc_1st_if("dignitaries", uc)
        } else {
            uc_1st_if("dignitary", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if subject == "you" && case == PronounCase::Objective {
            return Some(uc_1st_if("your majesty", uc));
        }
        None
    }
}

#[test]
fn test_custom_pronoun_formal() {
    let dignitary = Dignitary;
    let result = say!("I see {@0}.", dignitary);
    assert_eq!(result, "I see your majesty.".to_string());
}

// ============================================================================
// Test 5: Pronoun Case Routing — `test_custom_pronoun_case_routing`
// ============================================================================

#[test]
fn test_custom_pronoun_case_routing() {
    let dignitary = Dignitary;

    // Objective case should use custom form
    let result = say!("I see {@0}.", dignitary);
    assert_eq!(result, "I see your majesty.".to_string());

    // Subjective case should fall back to English
    let result = say!("{=0 are} here.", dignitary);
    assert_eq!(result, "You are here.".to_string());
}

// ============================================================================
// Test 6: Custom Article (Gendered) — `test_custom_article_gendered`
// ============================================================================

#[derive(Clone, Copy)]
struct SpanishFeminine;

impl fmt::Display for SpanishFeminine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cosa")
    }
}

impl Ranting for SpanishFeminine {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("cosa", uc)
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
        if to_plural {
            uc_1st_if("cosas", uc)
        } else {
            uc_1st_if("cosa", uc)
        }
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
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if article == "the" {
            let form = if as_plural { "las" } else { "la" };
            return Some(uc_1st_if(form, uc));
        }
        None
    }
}

#[test]
fn test_custom_article_gendered() {
    let cosa = SpanishFeminine;
    let result = say!("{the 0}", cosa);
    // Plural articles are capitalized at sentence start
    assert_eq!(result, "La cosa".to_string());

    let result = say!("{the +0}", cosa);
    assert_eq!(result, "Las cosas".to_string());
}

// ============================================================================
// Test 7: Article Fallback — `test_custom_article_fallback`
// ============================================================================

#[test]
fn test_custom_article_fallback() {
    let cosa = SpanishFeminine;
    // "a" is not customized, should use English a/an logic
    let result = say!("{a 0}", cosa);
    assert_eq!(result, "A cosa".to_string());
}

// ============================================================================
// Test 8: Combined Customization (Verb + Pronoun) — `test_custom_combined_verb_pronoun`
// ============================================================================

#[derive(Clone, Copy)]
struct ScottishHighlander;

impl fmt::Display for ScottishHighlander {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "highlander")
    }
}

impl Ranting for ScottishHighlander {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("highlander", uc)
    }

    fn subjective(&self) -> &str {
        "he"
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
        if to_plural {
            uc_1st_if("highlanders", uc)
        } else {
            uc_1st_if("highlander", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        match verb {
            "be" | "is" | "am" | "are" => Some(uc_1st_if("be", uc)),
            _ => None,
        }
    }

    fn inflect_pronoun_custom(
        &self,
        subject: &str,
        case: PronounCase,
        _class: NounClass,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if subject == "he" && case == PronounCase::Subjective {
            return Some(uc_1st_if("he lad", uc));
        }
        None
    }
}

#[test]
fn test_custom_combined_verb_pronoun() {
    let highlander = ScottishHighlander;
    let result = say!("{=0 be} brave.", highlander);
    // Custom pronoun "he lad" combined with custom verb "be". The verb sits *after* the noun,
    // so this only holds because the custom hook is wired into the post-noun path too.
    // This is the same example as docs/EXTENSIBILITY.md section 4.2.
    assert_eq!(result, "He lad be brave.".to_string());
}

// ============================================================================
// Test 9: Zero Customization (All Fallback) — `test_zero_customization`
// ============================================================================

#[derive(Clone, Copy)]
struct PlainNoun;

impl fmt::Display for PlainNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "thing")
    }
}

impl Ranting for PlainNoun {
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
        if to_plural {
            uc_1st_if("things", uc)
        } else {
            uc_1st_if("thing", uc)
        }
    }

    fn skip_article(&self) -> bool {
        false
    }

    // All custom methods return None — test that fallback works
}

#[test]
fn test_zero_customization() {
    let thing = PlainNoun;
    // Every hook returns None, so article and verb both come from English.
    // Verbs are written in their plural form ("are"), which English conjugates to "is" for a
    // third person singular subject.
    let result = say!("{the 0 are} red.", thing);
    assert_eq!(result, "The thing is red.".to_string());
}

// ============================================================================
// Test 10: Hook reaches the post-noun verb path — `test_verb_hook_called_on_post_noun_verb`
// ============================================================================

/// Negative control. The hook returns a sentinel for *every* verb, so if any verb call site
/// ever stops consulting `inflect_verb_custom()`, the English form leaks through and this fails.
#[derive(Clone, Copy)]
struct Sentinel;

impl fmt::Display for Sentinel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sentinel")
    }
}

impl Ranting for Sentinel {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("sentinel", uc)
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
        uc: bool,
        _case: GrammaticalCase,
        _count: Option<PlaceholderCount>,
    ) -> String {
        uc_1st_if("sentinel", uc)
    }

    fn skip_article(&self) -> bool {
        false
    }

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        _verb: &str,
        _as_plural: bool,
        _count: Option<PlaceholderCount>,
        _uc: bool,
    ) -> Option<String> {
        Some("XXHOOKCALLEDXX".to_string())
    }
}

#[test]
fn test_verb_hook_called_on_post_noun_verb() {
    let sentinel = Sentinel;
    // Post-noun verb: without the hook wired here, English would render "is".
    let result = say!("{=0 are} here.", sentinel);
    assert_eq!(result, "He XXHOOKCALLEDXX here.".to_string());

    // Pre-noun verb path, for contrast — this site was already wired.
    let result = say!("{are =0} here?", sentinel);
    assert_eq!(result, "XXHOOKCALLEDXX he here?".to_string());
}

// ============================================================================
// Test 11: Hook reaches the tense-marker path — `test_verb_hook_with_tense_marker`
// ============================================================================

// ============================================================================
// Test 12: docs/EXTENSIBILITY.md section 4.3 — `test_doc_spanish_article_and_verb`
// ============================================================================

/// Mirrors the Spanish example in docs/EXTENSIBILITY.md section 4.3.
///
/// That file is not `include_str!`'d, so its examples are never compiled as doctests. This is
/// the only thing keeping the documented output honest — and its verb sits after the noun, so
/// it only holds while the custom hook stays wired into the post-noun path. Gender is declared
/// on the entity via `NounClass`, not guessed from the noun's spelling: "problema" ends in "-a"
/// like "casa" but is masculine (the Greek-derived "-ma" class), which is exactly the trap an
/// `ends_with('a')` heuristic falls into — see `ranting_es/README.md`'s note on this same word.
#[derive(Clone, Copy)]
struct SpanishNoun {
    singular: &'static str,
    plural: &'static str,
    class: NounClass,
}

impl SpanishNoun {
    const fn new(singular: &'static str, plural: &'static str, class: &'static str) -> Self {
        SpanishNoun {
            singular,
            plural,
            class: NounClass::new(class),
        }
    }
}

impl fmt::Display for SpanishNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.singular)
    }
}

impl Ranting for SpanishNoun {
    fn name(&self, uc: bool) -> String {
        uc_1st_if(self.singular, uc)
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

    fn inflect_verb_custom(
        &self,
        _subject: &str,
        verb: &str,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        match verb {
            // Spanish "ser": "es" (singular) or "son" (plural)
            "be" => Some(uc_1st_if(if as_plural { "son" } else { "es" }, uc)),
            _ => None,
        }
    }

    fn inflect_article_custom(
        &self,
        article: &str,
        _noun_singular: &str,
        _case: GrammaticalCase,
        class: NounClass,
        as_plural: bool,
        _count: Option<PlaceholderCount>,
        uc: bool,
    ) -> Option<String> {
        if article == "the" {
            // Gender comes from the entity's own declared class — never from
            // noun_singular's spelling, which "problema" would get wrong.
            let form = match (class.as_str(), as_plural) {
                ("feminine", true) => "las",
                ("feminine", false) => "la",
                (_, true) => "los",
                (_, false) => "el",
            };
            return Some(uc_1st_if(form, uc));
        }
        None
    }
}

#[test]
fn test_doc_spanish_article_and_verb() {
    let casa = SpanishNoun::new("casa", "casas", "feminine");
    // The regular case: "casa" ends in "-a" and is feminine.
    assert_eq!(
        say!("{the 0 be} bonita.", casa),
        "La casa es bonita.".to_string()
    );

    let problema = SpanishNoun::new("problema", "problemas", "masculine");
    // "problema" also ends in "-a", but is masculine — declaring class on the
    // entity renders it correctly instead of falling into the spelling trap.
    assert_eq!(
        say!("{the 0 be} grave.", problema),
        "El problema es grave.".to_string()
    );
    assert_eq!(
        say!("{the +0 be} graves.", problema),
        "Los problemas son graves.".to_string()
    );
}

#[test]
fn test_verb_hook_with_tense_marker() {
    // A tense marker composes an auxiliary with the main verb. The hook gets a say over the
    // main verb, but the auxiliary composition must survive intact.
    let sentinel = Sentinel;
    let result = say!("{=0 >walk} home.", sentinel);
    assert_eq!(result, "He will XXHOOKCALLEDXX home.".to_string());

    // Without a hook, the marker path must not double-inflect: "will walk", never "will walks".
    let plain = PlainNoun;
    let result = say!("{=0 >walk} home.", plain);
    assert_eq!(result, "It will walk home.".to_string());
}

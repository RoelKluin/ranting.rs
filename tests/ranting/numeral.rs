// (c) Roel Kluin 2026 MIT
// The numeral-rendering runtime hook (ROADMAP.md Phase 6 item 8).
//
// `#var` used to be spelled out by `english-numbers` inside the macro-baked
// `format!()` argument, and `$var` is the argument's own `Display` output — two
// hard-coded English/ASCII choices with no way in. Every other language needs
// its own speller, several agree the numeral itself with the noun's gender
// (Russian `два стола` vs. `две книги`), and several scripts have their own
// digits.
//
// `#var` is therefore spelled at *runtime* now, from a count the macro bakes,
// with `rant_convert_numbers` as the fallback — so English output is unchanged
// (the guards at the bottom of this file assert that) while
// `inflect_numeral_custom` can replace the speller wholesale. `$var` still
// arrives pre-rendered with its `:fmt` spec applied, and the hook gets that
// string plus a count parsed back out of it when it is a plain integer.
use ranting::*;
use ranting_derive::say_with;
use std::fmt;

/// A Russian noun: enough of one to show gender agreement on the numeral and a
/// non-English speller, from one hook body.
struct RussianNoun {
    singular: &'static str,
    plural: &'static str,
    class: NounClass,
}

impl RussianNoun {
    fn new(singular: &'static str, plural: &'static str, class: &'static str) -> Self {
        RussianNoun {
            singular,
            plural,
            class: NounClass::new(class),
        }
    }
}

impl fmt::Display for RussianNoun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.singular)
    }
}

impl Ranting for RussianNoun {
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
        true
    }
    fn noun_class(&self) -> NounClass {
        self.class
    }

    fn inflect_numeral_custom(
        &self,
        numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        _case: GrammaticalCase,
        class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        match style {
            // A speller of its own: nothing of English's "one"/"two" survives,
            // and 1 and 2 agree with the noun's gender — the agreement the
            // roadmap item names, driven off `class` alone.
            NumeralStyle::Words => Some(
                match (count?, class.as_str()) {
                    (1, "feminine") => "одна",
                    (1, _) => "один",
                    (2, "feminine") => "две",
                    (2, _) => "два",
                    (3, _) => "три",
                    (5, _) => "пять",
                    _ => return Some(numeral.to_string()),
                }
                .to_string(),
            ),
            // Digits: a transcription of what English rendered, which is all a
            // digit-system fork needs.
            NumeralStyle::Digits => Some(
                numeral
                    .chars()
                    .map(|c| match c {
                        '0'..='9' => char::from_u32(c as u32 - '0' as u32 + 0x966).unwrap_or(c),
                        other => other,
                    })
                    .collect(),
            ),
        }
    }
}

#[test]
fn words_numeral_agrees_with_the_nouns_gender() {
    let stol = RussianNoun::new("стол", "стола", "masculine");
    let kniga = RussianNoun::new("книга", "книги", "feminine");

    // Same template, same count, different gender on the entity: два / две.
    assert_eq!(say!("есть {#0 1}", 2, stol), "есть два стола");
    assert_eq!(say!("есть {#0 1}", 2, kniga), "есть две книги");
    assert_eq!(say!("есть {#0 1}", 1, stol), "есть один стол");
    assert_eq!(say!("есть {#0 1}", 1, kniga), "есть одна книга");
}

#[test]
fn words_numeral_is_spelled_by_the_hook_not_by_english() {
    let stol = RussianNoun::new("стол", "стола", "masculine");
    assert_eq!(say!("есть {#0 1}", 5, stol), "есть пять стола");
    // A count the hook doesn't special-case falls back to the string it was
    // handed — the English rendering, which is what `None` would have kept.
    assert_eq!(say!("есть {#0 1}", 4, stol), "есть four стола");
}

#[test]
fn a_count_of_one_still_takes_singular_agreement_through_the_hook() {
    // The prerequisite ROADMAP.md item 8 called out: number agreement used to be
    // decided by comparing the *rendered* numeral against the literal English
    // word "one", so a non-English speller silently got plural agreement for a
    // count of one. It is decided from the count now, before the hook runs, so
    // "один" is singular exactly as "one" was.
    let stol = RussianNoun::new("стол", "стола", "masculine");
    assert_eq!(say!("есть {#0 1}", 1, stol), "есть один стол");
    assert_eq!(say!("есть {#0 1}", 2, stol), "есть два стола");
}

#[test]
fn digits_numeral_is_transcribed_by_the_hook() {
    let stol = RussianNoun::new("стол", "стола", "masculine");
    assert_eq!(say!("есть {$0 1}", 2, stol), "есть २ стола");
    assert_eq!(say!("есть {$0 1}", 12, stol), "есть १२ стола");
    // Plurality still comes off the *rendered English* digits for `$var`
    // (unchanged behavior), so a count of one is singular here too.
    assert_eq!(say!("есть {$0 1}", 1, stol), "есть १ стол");
}

#[test]
fn digits_numeral_hook_sees_the_fmt_spec_applied() {
    let stol = RussianNoun::new("стол", "стола", "masculine");
    // `$var`'s `:fmt` spec is applied before the hook sees the number, so the
    // padding is part of the string handed over — and a hook that returns a
    // fresh string owns whatever padding its output has.
    assert_eq!(say!("есть {$0 1:>3}", 2, stol), "есть   २ стола");
}

#[test]
fn hidden_numbers_do_not_reach_the_hook() {
    // `{?$n noun}` renders no number at all, so there is nothing to customize —
    // but the count still governs agreement.
    let stol = RussianNoun::new("стол", "стола", "masculine");
    assert_eq!(say!("есть {?$0 1}", 1, stol), "есть  стол");
    assert_eq!(say!("есть {?$0 1}", 2, stol), "есть  стола");
}

#[test]
fn a_placeholder_without_a_number_does_not_reach_the_hook() {
    let stol = RussianNoun::new("стол", "стола", "masculine");
    assert_eq!(say!("есть {0}", stol), "есть стол");
    assert_eq!(say!("есть {+0}", stol), "есть стола");
}

/// A noun that overrides *only* the `_with_context` form of the hook, and reads
/// the story-wide dialect to pick a digit system. This is what pins the call
/// site: if `handle_placeholder_impl` called `inflect_numeral_custom` directly,
/// every other test in this file would still pass and this one would not.
struct LocalizedCount;

impl fmt::Display for LocalizedCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("item")
    }
}

impl Ranting for LocalizedCount {
    fn name(&self, uc: bool) -> String {
        uc_1st_if("item", uc)
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
        uc_1st_if(if to_plural { "items" } else { "item" }, uc)
    }
    fn skip_article(&self) -> bool {
        true
    }

    fn inflect_numeral_custom_with_context(
        &self,
        numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
        ctx: Option<&NarrationContext>,
    ) -> Option<String> {
        // `dialect` is inert to the crate — a hook is the only thing that reads
        // it, which is exactly the point of the `_with_context` pair.
        match ctx.and_then(|c| c.dialect)? {
            "de" if style == NumeralStyle::Words => Some(match count? {
                1 => "eins".to_string(),
                2 => "zwei".to_string(),
                n => n.to_string(),
            }),
            "ar" if style == NumeralStyle::Digits => Some(
                numeral
                    .chars()
                    .map(|c| match c {
                        '0'..='9' => char::from_u32(c as u32 - '0' as u32 + 0x660).unwrap_or(c),
                        other => other,
                    })
                    .collect(),
            ),
            _ => None,
        }
    }
}

#[test]
fn say_with_reaches_the_with_context_numeral_hook() {
    let de = NarrationContext::new().dialect("de");
    assert_eq!(
        say_with!(de, "ich sehe {#0 1}", 2, LocalizedCount),
        "ich sehe zwei items"
    );
    let ar = NarrationContext::new().dialect("ar");
    assert_eq!(
        say_with!(ar, "I see {$0 1}", 2, LocalizedCount),
        "I see ٢ items"
    );
}

#[test]
fn say_with_without_a_dialect_keeps_the_english_numeral() {
    // The hook declines, and `say_with!()` reproduces `say!()` exactly — the
    // same no-override guarantee the rest of the crate makes.
    let plain = NarrationContext::new();
    assert_eq!(
        say_with!(plain, "I see {#0 1}", 2, LocalizedCount),
        "I see two items"
    );
    assert_eq!(
        say_with!(plain, "I see {$0 1}", 2, LocalizedCount),
        "I see 2 items"
    );
    assert_eq!(say!("I see {#0 1}", 2, LocalizedCount), "I see two items");
}

// ---------------------------------------------------------------------------
// Byte-identical-English guards. `Noun` overrides nothing, so every one of
// these must render exactly as it did before the hook existed — including the
// two shapes the refactor moved code around most: `#var`, whose English word is
// now produced by `rant_convert_numbers` at runtime rather than baked by the
// macro, and the number's leading space, which moved out of the baked string
// into `NumeralSpec::leading_space`. Every expectation below was captured by
// running the same templates against the pre-change sources.
// ---------------------------------------------------------------------------

#[test]
fn english_words_numerals_are_unchanged() {
    let boot = Noun::new("boot", "it");
    assert_eq!(say!("I see {#0 boot}", 1), "I see one boot");
    assert_eq!(say!("I see {#0 boot}", 2), "I see two boots");
    assert_eq!(say!("I see {#0 boot}", 0), "I see zero boots");
    assert_eq!(say!("I see {#0 boot}", 21), "I see twentyone boots");
    assert_eq!(say!("I see {a #0 boot}", 3), "I see some three boots");
    // Sentence-initial: `uc` is spent on the noun, never on the numeral.
    assert_eq!(say!("{#0 boot}", 1), "one Boot");
}

#[test]
fn english_digit_numerals_are_unchanged() {
    let boot = Noun::new("boot", "it");
    assert_eq!(say!("I see {$0 boot}", 1), "I see 1 boot");
    assert_eq!(say!("I see {$0 boot}", 2), "I see 2 boots");
    assert_eq!(say!("I see {$0 boot}", 1.0), "I see 1 boot");
    assert_eq!(say!("I see {$0 boot:>4}", 2), "I see    2 boots");
    assert_eq!(say!("{$0 boot's} laces", 2), "2 Boots' laces");
    // A hidden number renders nothing — but note it leaves the noun's own
    // leading space, exactly as before (the `?` drops the *number's* space,
    // which is why `leading_space` living in `NumeralSpec` is unobservable
    // here): "I see " + " boots".
    assert_eq!(say!("I see {?$0 boot}", 2), "I see  boots");
    assert_eq!(say!("I see {?$0 boot}", 1), "I see  boot");
}

#[test]
fn english_numeral_verb_and_article_agreement_is_unchanged() {
    let boot = Noun::new("boot", "it");
    let sock = Noun::new("sock", "it");
    assert_eq!(say!("I see {the #0 boot are}", 1), "I see the one boot is");
    assert_eq!(
        say!("I see {the #0 boot are}", 2),
        "I see the two boots are"
    );
    assert_eq!(
        say!("I see {a set of $0 boot are}", 2),
        "I see some set of 2 boots are"
    );
    assert_eq!(
        say!("I see {a set of #0 boot are}", 2),
        "I see some set of two boots are"
    );
    // Two numerals, two styles, one call.
    assert_eq!(
        say!("I have {#0 boot}, {$1 sock}", 3, 4),
        "I have three boots, 4 socks"
    );
}

// (c) Roel Kluin 2026 MIT
// ROADMAP.md Phase 8 item 4: the ordinal channels, `##var` (spelled, "third") and `$$var`
// (digits with an English suffix, "3rd") -- the doubled siblings of the existing `#var`/`$var`
// cardinal markers. See docs/superpowers/specs/2026-08-15-ordinal-numerals.md.
use ranting::*;

#[test]
fn spelled_ordinal_renders_english_words() {
    let attempt = Noun::new("attempt", "it");
    let n = 1;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the first attempt."
    );
    let n = 2;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the second attempt."
    );
    let n = 3;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the third attempt."
    );
    // The teens are all `+th`, no suppletion.
    let n = 11;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the eleventh attempt."
    );
    let n = 12;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the twelfth attempt."
    );
    let n = 13;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the thirteenth attempt."
    );
}

#[test]
fn digit_ordinal_renders_the_english_suffix() {
    let attempt = Noun::new("attempt", "it");
    let n = 1;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 1st attempt."
    );
    let n = 2;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 2nd attempt."
    );
    let n = 3;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 3rd attempt."
    );
    // The teens exception: checked against the last *two* digits, not the last one, so 11-13
    // (and 111-113, ...) all take "th" regardless of what a naive last-digit check would give.
    let n = 11;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 11th attempt."
    );
    let n = 12;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 12th attempt."
    );
    let n = 13;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 13th attempt."
    );
    // Past the teens, the last-digit rule resumes.
    let n = 21;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 21st attempt."
    );
    let n = 22;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 22nd attempt."
    );
    let n = 23;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 23rd attempt."
    );
    let n = 101;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 101st attempt."
    );
    let n = 111;
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 111th attempt."
    );
}

/// ROADMAP.md Phase 8 item 4: agreement decouples from the ordinal itself -- an ordinal says
/// *which* one, not *how many*, so a singular-declared noun stays singular no matter how large
/// the count is. This is the exact silent-failure site the design spike named: before the
/// `plurality` retype, `{##n attempt}` fell through to the string match's catch-all arm (which
/// reads `nr`, empty for the spelled path) and rendered "attempts".
#[test]
fn ordinal_does_not_pluralize_the_noun_from_its_count() {
    let attempt = Noun::new("attempt", "it");
    let n = 100;
    // `english_numbers` spells cardinals as one fused, unhyphenated run with no interior space
    // at all ("onehundred", not "one hundred") -- inherited verbatim by the ordinal speller, see
    // `spelled_ordinal_inherits_the_unhyphenated_compound_spelling` below.
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the onehundredth attempt."
    );
    assert_eq!(
        say!("This is {the $$n attempt}."),
        "This is the 100th attempt."
    );
}

/// The noun's own declared plurality, not the ordinal's count, decides agreement -- unchanged
/// even at a count of 1. A `Noun` whose subject is plural ("they") renders its declared `name`
/// text as-is when asked for plural agreement (its own declared plurality), so the name is given
/// already in its plural spelling, exactly as a caller would write it for `{+noun}`.
#[test]
fn ordinal_agreement_follows_the_nouns_own_plurality() {
    let attempts = Noun::new("attempts", "they");
    let n = 1;
    assert_eq!(
        say!("This is {the ##n attempts}."),
        "This is the first attempts."
    );
    assert_eq!(
        say!("This is {the $$n attempts}."),
        "This is the 1st attempts."
    );
}

/// The unhyphenated compound spelling `english_numbers` gives cardinals ("twentyone", not
/// "twenty-one") is inherited by the ordinal speller verbatim -- see
/// docs/superpowers/specs/2026-08-15-ordinal-numerals.md's "The English rules" section.
#[test]
fn spelled_ordinal_inherits_the_unhyphenated_compound_spelling() {
    let attempt = Noun::new("attempt", "it");
    let n = 21;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the twentyfirst attempt."
    );
    let n = 100;
    assert_eq!(
        say!("This is {the ##n attempt}."),
        "This is the onehundredth attempt."
    );
}

/// A `Ranting` implementor's `inflect_numeral_custom` still receives the real `count` for an
/// ordinal placeholder -- the signal Spanish/Arabic ordinal gender agreement needs (ROADMAP.md
/// Phase 8 item 4's "second constituency") -- alongside the correct `NumeralStyle::Ordinal`/
/// `NumeralStyle::OrdinalDigits` classification, distinct from the plain cardinal styles.
struct OrdinalSpy;

impl std::fmt::Display for OrdinalSpy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("attempt")
    }
}

impl Ranting for OrdinalSpy {
    fn name(&self, uc: bool) -> String {
        capitalize_if("attempt", uc)
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
        capitalize_if(if to_plural { "attempts" } else { "attempt" }, uc)
    }
    fn skip_article(&self) -> bool {
        true
    }

    fn inflect_numeral_custom(
        &self,
        _numeral: &str,
        count: Option<i64>,
        style: NumeralStyle,
        _case: GrammaticalCase,
        _class: NounClass,
        _as_plural: bool,
    ) -> Option<String> {
        match style {
            NumeralStyle::Ordinal => Some(format!("ord#{}", count?)),
            NumeralStyle::OrdinalDigits => Some(format!("orddig#{}", count?)),
            NumeralStyle::Words | NumeralStyle::Digits => None,
        }
    }
}

#[test]
fn ordinal_hooks_receive_the_real_count_and_style() {
    let spy = OrdinalSpy;
    let n = 3;
    // Sentence-initial: the numeral claims the capital, same as `#var`/`$var` do.
    assert_eq!(say!("{##n spy}"), "Ord#3 attempt");
    assert_eq!(say!("{$$n spy}"), "orddig#3 attempt");
}

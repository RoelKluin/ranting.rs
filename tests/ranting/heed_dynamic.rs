// (c) Roel Kluin 2026 MIT
//
// heed!() requires its template as a string literal — the macro bakes the
// template into a compile-time regex, and its typed return shape (Option<T>,
// Option<(T1, T2, ...)>, {$name} -> u64) is derived from the template's own
// text at compile time. That leaves no path for a template that only exists
// at runtime: read from a config file, typed by a user, or otherwise built
// as a String rather than written as a literal at the call site.
//
// HeedMatcher::from_template closes that gap by exposing the same template
// compiler heed!() itself uses (ranting_core::heed_template, shared via the
// macro/runtime seam) as a runtime constructor. Every capture comes back as
// a plain String, in HeedMatcher::capture_names()'s order — the same
// always-String shape ask!()'s Answerable::Captures already uses for the
// identical reason (a fixed shape must work for every template that reaches
// it, so it cannot vary a {$name} capture's type case by case).
//
// These are HeedMatcher::from_template's first integration tests.

use ranting::*;

#[test]
fn compiles_and_matches_a_template_built_at_runtime() {
    // Simulates a template read from disk or typed by a user: built as an
    // owned String at runtime, never written as a literal anywhere.
    let template: String = ["take", "{item}"].join(" ");
    let matcher = HeedMatcher::from_template(&template).expect("valid template");
    assert_eq!(
        matcher.match_input("take sword"),
        Some(vec!["sword".to_string()])
    );
    assert_eq!(matcher.match_input("drop sword"), None);
}

#[test]
fn capture_names_reports_every_capture_in_match_order() {
    let template = String::from("give {item...} to {target}, {$count} gold");
    let matcher = HeedMatcher::from_template(&template).expect("valid template");
    assert_eq!(matcher.capture_names(), &["item", "target", "count"]);
    assert_eq!(
        matcher.match_input("give a rusty sword to smith, 12 gold"),
        Some(vec![
            "a rusty sword".to_string(),
            "smith".to_string(),
            "12".to_string(),
        ])
    );
}

#[test]
fn zero_capture_template_matches_the_literal_text_only() {
    let matcher = HeedMatcher::from_template("look around").expect("valid template");
    assert_eq!(matcher.capture_names(), &[] as &[&str]);
    assert_eq!(matcher.match_input("look around"), Some(vec![]));
    assert_eq!(matcher.match_input("look elsewhere"), None);
}

#[test]
fn malformed_template_reports_a_structured_error_instead_of_panicking() {
    let err = HeedMatcher::from_template("take {}").expect_err("empty capture name is invalid");
    assert!(err.to_string().contains("identifier"));

    let err = HeedMatcher::from_template("take {item").expect_err("unterminated brace is invalid");
    assert!(err.to_string().contains("unterminated"));
    // The error carries the byte range of the problem within the template,
    // so a caller building its own diagnostic (e.g. for a config file) can
    // point at the exact offending text without re-parsing.
    assert_eq!(&"take {item"[err.range()], "{item");

    let err =
        HeedMatcher::from_template("{a}{b}").expect_err("two adjacent captures are ambiguous");
    assert!(err.to_string().contains("ambiguous"));

    let err = HeedMatcher::from_template("take {a{b}}").expect_err("nested captures are rejected");
    assert!(err.to_string().contains("nested"));
}

// A runtime template gives ask!()'s Answerable::Captures the same thing
// HeedMatcher::match_input always has: an ordered Vec<String>, never a typed
// tuple. ask!() itself still needs a compile-time-literal template (the same
// reason heed!() does), but nothing new is needed to reach Answerable::answer
// with a runtime one — it is a plain public trait method, so a caller can
// match with HeedMatcher::from_template and call it directly.
struct Trader;
impl Answerable for Trader {
    type Captures = (String, String);
    fn answer(&self, _speaker: &dyn Ranting, (item, price): (String, String)) -> String {
        let price: u32 = price.parse().unwrap_or(0);
        format!("I'll give you {price} gold for that {item}.")
    }
}

#[test]
fn a_runtime_template_can_still_reach_answerable_by_hand() {
    let player = Noun::new("Jo", "she");
    let trader = Trader;
    let template = String::from("sell {item} for {price}");
    let matcher = HeedMatcher::from_template(&template).expect("valid template");

    let mut caps = matcher
        .match_input("sell sword for 12")
        .expect("input matches the template");
    let price = caps.pop().expect("two captures");
    let item = caps.pop().expect("two captures");

    assert_eq!(
        trader.answer(&player, (item, price)),
        "I'll give you 12 gold for that sword."
    );
    // Same answer ask!() itself would give for the equivalent literal template.
    assert_eq!(
        ask!(
            player,
            trader,
            "sell {item} for {price}",
            "sell sword for 12"
        ),
        Some("I'll give you 12 gold for that sword.".to_string())
    );
}

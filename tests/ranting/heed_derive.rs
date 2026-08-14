// (c) Roel Kluin 2026 MIT
// #[derive(Heed)] — v2 of heed!() (ROADMAP.md Phase 3 item 8). Same
// template grammar and matching engine as heed!(), but produces a typed
// `fn heed(input: &str) -> Option<Self>` instead of a positional tuple.
use ranting::Heed;

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "take {item}")]
struct Take {
    item: String,
}

#[test]
fn single_field_matches() {
    assert_eq!(
        Take::heed("take sword"),
        Some(Take {
            item: "sword".to_string()
        })
    );
}

#[test]
fn no_match_returns_none() {
    assert_eq!(Take::heed("drop sword"), None);
}

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "give {item} to {target}")]
struct Give {
    item: String,
    target: String,
}

#[test]
fn multiple_fields_populate_from_captures() {
    assert_eq!(
        Give::heed("give sword to guard"),
        Some(Give {
            item: "sword".to_string(),
            target: "guard".to_string(),
        })
    );
}

// Field declaration order need not match template capture-appearance order —
// the derive maps by name, not position (mirrors heed!()'s own
// returns_captures_in_names_order behavior).
#[derive(Heed, Debug, PartialEq)]
#[heed(template = "give {item} to {target}")]
struct GiveReordered {
    target: String,
    item: String,
}

#[test]
fn field_order_independent_of_template_order() {
    assert_eq!(
        GiveReordered::heed("give sword to guard"),
        Some(GiveReordered {
            target: "guard".to_string(),
            item: "sword".to_string(),
        })
    );
}

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "trade {$count} {item} for {target}")]
struct Trade {
    count: u64,
    item: String,
    target: String,
}

#[test]
fn numeric_capture_parses_to_u64_field() {
    assert_eq!(
        Trade::heed("trade 3 sword for shield"),
        Some(Trade {
            count: 3,
            item: "sword".to_string(),
            target: "shield".to_string(),
        })
    );
}

#[test]
fn numeric_capture_overflow_returns_none() {
    assert_eq!(
        Trade::heed("trade 99999999999999999999 sword for shield"),
        None
    );
}

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "take {item...}")]
struct TakeGreedy {
    item: String,
}

#[test]
fn greedy_capture_spans_multiple_words() {
    assert_eq!(
        TakeGreedy::heed("take rusty old sword"),
        Some(TakeGreedy {
            item: "rusty old sword".to_string()
        })
    );
}

#[derive(Heed, Debug, PartialEq)]
#[heed(template = "look around")]
struct LookAround;

#[test]
fn zero_captures_still_generates_heed() {
    assert_eq!(LookAround::heed("look around"), Some(LookAround));
    assert_eq!(LookAround::heed("look elsewhere"), None);
}

// Empty-braced struct (`struct Foo {}`), distinct from the true unit struct `LookAround`
// above: `Fields::Named` with zero fields, not `Fields::Unit`. Must construct `Self {}`, not
// bare `Self` — the latter doesn't typecheck for a struct declared with braces. Previously
// mishandled because the codegen branched on "zero fields" rather than "which `Fields`
// variant" (docs/architecture-review-2026-08-14.md §1.1 / -08-15.md §1.4).
#[derive(Heed, Debug, PartialEq)]
#[heed(template = "wait")]
struct Wait {}

#[test]
fn zero_captures_empty_braced_struct_still_generates_heed() {
    assert_eq!(Wait::heed("wait"), Some(Wait {}));
    assert_eq!(Wait::heed("go"), None);
}

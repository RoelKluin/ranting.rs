// (c) Roel Kluin 2026 GPL v3
use ranting::heed;

#[test]
fn single_word_capture() {
    assert_eq!(
        heed!("take {item}", "take sword"),
        Some("sword".to_string())
    );
}

#[test]
fn no_match_returns_none() {
    assert_eq!(heed!("take {item}", "drop sword"), None);
}

#[test]
fn multi_capture_returns_positional_tuple() {
    assert_eq!(
        heed!("give {item} to {target}", "give sword to guard"),
        Some(("sword".to_string(), "guard".to_string()))
    );
}

#[test]
fn greedy_capture_spans_multiple_words() {
    assert_eq!(
        heed!("take {item...}", "take rusty old sword"),
        Some("rusty old sword".to_string())
    );
}

#[test]
fn greedy_capture_before_trailing_literal_and_capture() {
    assert_eq!(
        heed!(
            "give {item...} to {target}",
            "give rusty old sword to guard"
        ),
        Some(("rusty old sword".to_string(), "guard".to_string()))
    );
}

#[test]
fn numeric_capture_parses_to_u64() {
    assert_eq!(heed!("take {$count} gold", "take 42 gold"), Some(42u64));
}

#[test]
fn zero_captures_matches_literal_only() {
    assert_eq!(heed!("look around", "look around"), Some(()));
    assert_eq!(heed!("look around", "look elsewhere"), None);
}

#[test]
fn tolerates_surrounding_and_extra_whitespace() {
    assert_eq!(
        heed!("take {item}", "  take   sword  "),
        Some("sword".to_string())
    );
}

#[test]
fn three_captures_returns_three_tuple() {
    assert_eq!(
        heed!(
            "trade {$count} {item} for {target}",
            "trade 3 sword for shield"
        ),
        Some((3u64, "sword".to_string(), "shield".to_string()))
    );
}

#[test]
fn punctuation_literal_matches_end_to_end() {
    assert_eq!(
        heed!(
            "give {item} to {target}, {$count} gold",
            "give sword to guard, 5 gold"
        ),
        Some(("sword".to_string(), "guard".to_string(), 5u64))
    );
    // No whitespace is required or allowed before a trailing punctuation-only
    // literal segment, so a space before the comma must not match.
    assert_eq!(
        heed!(
            "give {item} to {target}, {$count} gold",
            "give sword to guard , 5 gold"
        ),
        None
    );
}

#[test]
fn numeric_capture_overflowing_u64_returns_none() {
    assert_eq!(
        heed!("take {$n} gold", "take 99999999999999999999 gold"),
        None
    );
}

#[test]
fn owned_string_input_compiles_and_matches() {
    let input = String::from("take sword");
    assert_eq!(heed!("take {item}", input), Some("sword".to_string()));
}

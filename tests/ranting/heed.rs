// (c) Roel Kluin 2026 GPL v3
use ranting::heed;

#[test]
fn single_word_capture() {
    assert_eq!(heed!("take {item}", "take sword"), Some("sword".to_string()));
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

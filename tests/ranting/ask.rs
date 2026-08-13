// (c) Roel Kluin 2026 GPL v3
//
// Phase 5: ask!() reworked from a duck-typed `audience.answer(speaker, String)`
// call built from a say!()-style rendered question, to a heed!()-shaped
// `ask!(speaker, audience, template, input)` that parses `input` against
// `template` and forwards the captures to `audience`'s `Answerable::answer`.
// These tests are ask!()'s first coverage in the repo.

use ranting::*;

struct Dog;
impl Answerable for Dog {
    type Captures = ();
    fn answer(&self, _speaker: &dyn Ranting, _captures: ()) -> String {
        "Woof!".to_string()
    }
}

struct Stone;
impl Answerable for Stone {
    type Captures = String;
    fn answer(&self, _speaker: &dyn Ranting, _topic: String) -> String {
        "The stone remains silent.".to_string()
    }
}

struct Villager;
impl Answerable for Villager {
    type Captures = String;
    fn answer(&self, speaker: &dyn Ranting, topic: String) -> String {
        match topic.as_str() {
            "bone" => format!("{} not looking for bones here.", speaker.subjective()),
            _ => "I don't know anything about that.".to_string(),
        }
    }
}

struct Trader;
impl Answerable for Trader {
    type Captures = (String, String);
    fn answer(&self, _speaker: &dyn Ranting, (item, price): (String, String)) -> String {
        let price: u32 = price.parse().unwrap_or(0);
        format!("I'll give you {price} gold for that {item}.")
    }
}

#[test]
fn zero_capture_audience_ignores_input_text() {
    let player = Noun::new("Jo", "she");
    let dog = Dog;
    assert_eq!(
        ask!(player, dog, "pet dog", "pet dog"),
        Some("Woof!".to_string())
    );
}

#[test]
fn single_capture_forwarded_as_plain_string() {
    let player = Noun::new("Jo", "she");
    let stone = Stone;
    assert_eq!(
        ask!(player, stone, "about {topic}", "about weather"),
        Some("The stone remains silent.".to_string())
    );
}

#[test]
fn single_capture_drives_response_and_sees_speaker() {
    let player = Noun::new("Jo", "she");
    let villager = Villager;
    assert_eq!(
        ask!(player, villager, "about {topic}", "about bone"),
        Some("she not looking for bones here.".to_string())
    );
    assert_eq!(
        ask!(player, villager, "about {topic}", "about weather"),
        Some("I don't know anything about that.".to_string())
    );
}

#[test]
fn two_captures_forwarded_as_a_tuple() {
    let player = Noun::new("Jo", "she");
    let trader = Trader;
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

#[test]
fn no_match_returns_none_without_calling_answer() {
    let player = Noun::new("Jo", "she");
    let dog = Dog;
    assert_eq!(ask!(player, dog, "pet dog", "kick dog"), None);
}

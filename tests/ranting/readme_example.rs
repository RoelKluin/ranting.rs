use ranting::*;
use ranting_derive::*;

fn say_name(who: Noun) -> String {
    say!("{=who do} say {`who} name is {who}.")
}

#[test]
fn test_name() {
    assert_eq!(
        say_name(Noun::new("Jane", "I")),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        say_name(Noun::new("Tarzan", "he")),
        "He does say his name is Tarzan.".to_string()
    );
}

fn state<T: Ranting>(who: T, liberty: &str) -> String {
    say!("{haven't =who} a {liberty} to say {a who's} land is {~who}?")
}

#[derive_ranting]
#[ranting(subject = "he")]
struct Earl {}

#[derive_ranting]
#[ranting(subject = "they")]
struct Farmers {}

#[test]
fn test_state() {
    assert_eq!(
        state(Earl {}, "right"),
        "Hasn't he a right to say an earl's land is his?".to_string()
    );
    assert_eq!(
        state(Farmers {}, "right"),
        "Haven't they a right to say some farmers' land is theirs?".to_string()
    );
}

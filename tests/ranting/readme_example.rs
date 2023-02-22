use ranting::*;
use ranting_derive::*;

fn say_this(who: Noun, title: &Noun) -> String {
    say!("{=who do} say {`who title are} {who}.")
}

#[test]
fn test_name() {
    let title = Noun::new("name", "it");
    assert_eq!(
        say_this(Noun::new("Jane", "I"), &title),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        say_this(Noun::new("Tarzan", "he"), &title),
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

use ranting::*;

fn name(who: Noun) -> String {
    say!("{:who do} say {`who} name is {who}.")
}

#[test]
fn test_name() {
    assert_eq!(
        name(Noun::new("Jane", "I")),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        name(Noun::new("Tarzan", "he")),
        "He does say his name is Tarzan.".to_string()
    );
}

fn state(who: Noun, liberty: &str) -> String {
    say!("{haven't :who} a {liberty} to say {a who's} land is {~who}?")
}

#[test]
fn test_state() {
    assert_eq!(
        state(Noun::new("earl", "he"), "right"),
        "Hasn't he a right to say an earl's land is his?".to_string()
    );
    assert_eq!(
        state(Noun::new("farmers", "they"), "right"),
        "Haven't they a right to say some farmers' land is theirs?".to_string()
    );
}

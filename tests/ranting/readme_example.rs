use ranting::*;
use ranting_derive::*;

// by setting name and subject empty, these must come from the
#[derive_ranting]
#[ranting(name = "", subject = "", lc = "true")]
struct Person {
    name: String,
    subject: SubjectPronoun,
}

impl Person {
    fn new(name: &str, subject: &str) -> Self {
        Person {
            name: String::from(name),
            subject: SubjectPronoun::try_from(subject).unwrap(),
        }
    }
}

fn say_name<T: Ranting>(who: T) -> String {
    say!("{:who do} say {`who} name is {who}.")
}

#[test]
fn test_name() {
    assert_eq!(
        say_name(Person::new("Jane", "I")),
        "I do say my name is Jane.".to_string()
    );
    assert_eq!(
        say_name(Person::new("Tarzan", "he")),
        "He does say his name is Tarzan.".to_string()
    );
}

fn state<T: Ranting>(who: T, liberty: &str) -> String {
    say!("{haven't :who} a {liberty} to say {a who's} land is {~who}?")
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

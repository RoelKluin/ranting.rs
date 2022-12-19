use ranting::Ranting;
use ranting_derive::*;

#[derive(Ranting)]
struct Named {
    name: String,
    subject: String,
}
impl Named {
    fn new(name: &str, subject: &str) -> Self {
        Named {
            name: name.to_string(),
            subject: subject.to_string(),
        }
    }
    fn subjective(&self) -> &str {
        self.subject.as_str()
    }
}

fn say_want_to_send(sender: &dyn Ranting, receiver: &dyn Ranting, message: &dyn Ranting) -> String {
    say!(
        "{0 want} to send {some message}, {`0} secret {message}, to {receiver}.",
        sender
    )
}

fn say_we_know_message(
    sender: &dyn Ranting,
    receiver: &dyn Ranting,
    message: &dyn Ranting,
) -> String {
    say!("Now {:receiver know} of {these message} that {:message are} really {~sender}.")
}

#[test]
fn readme_example() {
    let alice = Named::new("Alice", "she");
    let bob = Named::new("Bob", "he");
    let packages = Named::new("packages", "they");

    // with Alice as sender: packages
    assert_eq!(
        say_want_to_send(&alice, &bob, &packages),
        "Alice wants to send some packages, her secret packages, to Bob.".to_string()
    );
    assert_eq!(
        say_we_know_message(&alice, &bob, &packages),
        "Now he knows of these packages that they are really hers.".to_string()
    );

    let email = Named::new("email", "it");

    // With Bob as sender: email
    assert_eq!(
        say_want_to_send(&bob, &alice, &email),
        "Bob wants to send an email, his secret email, to Alice.".to_string()
    );
    assert_eq!(
        say_we_know_message(&bob, &alice, &email),
        "Now she knows of this email that it is really his.".to_string()
    );

    // With Email as sender: packages to Alice, even kind of works:
    assert_eq!(
        say_want_to_send(&email, &alice, &packages),
        "Email wants to send some packages, its secret packages, to Alice.".to_string()
    );
    assert_eq!(
        say_we_know_message(&email, &alice, &packages),
        "Now she knows of these packages that they are really its.".to_string()
    );

    // With Packages as sender: email to Bob:
    assert_eq!(
        say_want_to_send(&packages, &bob, &email),
        "Packages want to send an email, their secret email, to Bob.".to_string()
    );
    assert_eq!(
        say_we_know_message(&packages, &bob, &email),
        "Now he knows of this email that it is really theirs.".to_string()
    );
}

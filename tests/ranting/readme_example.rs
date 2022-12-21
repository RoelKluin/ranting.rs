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
}
fn send(sender: &Named, receiver: &Named, msg: &Named) -> String {
    say!(
        "{0 want} to send {some msg}, {`0} secret {msg}, to {1}.",
        sender,
        receiver
    )
}

fn receive<S, R, M>(sender: &S, receiver: &R, msg: &M) -> String
where
    S: Ranting,
    R: Ranting,
    M: Ranting,
{
    say!("Now {:receiver know} {these msg}, {:msg are} {~sender}.")
}

#[test]
fn readme_example() {
    let alice = Named::new("Alice", "she");
    let bob = Named::new("Bob", "he");
    let packages = Named::new("packages", "they");

    // with Alice as sender: packages
    assert_eq!(
        send(&alice, &bob, &packages),
        "Alice wants to send some packages, her secret packages, \
        to Bob."
            .to_string()
    );
    assert_eq!(
        receive(&alice, &bob, &packages),
        "Now he knows these packages, they are hers.".to_string()
    );
    let email = Named::new("email", "it");

    // With Bob as sender: email
    assert_eq!(
        send(&bob, &alice, &email),
        "Bob wants to send an email, his secret email, \
        to Alice."
            .to_string()
    );
    assert_eq!(
        receive(&bob, &alice, &email),
        "Now she knows this email, it is his.".to_string()
    );

    // With Email as sender: packages to Alice, even kind of works:
    assert_eq!(
        send(&email, &alice, &packages),
        "Email wants to send some packages, its secret packages, \
        to Alice."
            .to_string()
    );
    assert_eq!(
        receive(&email, &alice, &packages),
        "Now she knows these packages, \
        they are its."
            .to_string()
    );

    // With Packages as sender: email to Bob:
    assert_eq!(
        send(&packages, &bob, &email),
        "Packages want to send an email, their secret email, \
        to Bob."
            .to_string()
    );
    assert_eq!(
        receive(&packages, &bob, &email),
        "Now he knows this email, it is theirs.".to_string()
    );
}


Generate `Ranting` trait implementations
```rust
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
            subject: subject.to_string()
        }
    }
    fn subjective(&self) -> &str {
        self.subject.as_str()
    }
}
```
The struct should contain at least a name String. The trait provides the following
functions of which you may want to override the subjective function:

```rust
pub trait Ranting: std::fmt::Display {
    fn subjective(&self) -> &str;
    fn name(&self) -> &str;
    fn is_plural(&self) -> bool;
    fn a_or_an(&self, uc: bool) -> &str;
}
```
Then these can be referenced in the say!() nay!() and ack!() macros.

The say!() macro produces a String, a bit similar to format!(), but with extended
formatting options for arguments with Ranting traits.

```rust
fn say_want_to_send(sender: &dyn Ranting, receiver: &dyn Ranting, message: &dyn Ranting) -> String {
    say!("{0 want} to send {some message}, {'0} secret {message}, to {receiver}.", sender)
}

fn say_we_know_message(sender: &dyn Ranting, receiver: &dyn Ranting, message: &dyn Ranting) -> String {
    say!("Now {:receiver know} of {these message} that {:message are} really {~sender}.")
}

fn main() {
    let alice = Named::new("Alice","she");
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
```
Or see a more elaborate [example](tests/ranting/male_female_and_object.rs).

Ranting trait objects as arguments to say!() are displayed either as name (by default)
or as pronoun and/or inflected, dependent on provided markers. Alongside the ranting
variable, articles and verbs can be included in the curly braces, that are inflected
accordingly. The verb should be specified in the plural form.

Any struct with the Ranting trait should contain a name and subject variable. The name
should preferrably just be a noun. The subject variable determines the default case,
use `I`, `you`, `he`, `she`, `it`, `we`, `they`, `thou` or `ye`.

Beside ranting trait variables, also variables can be included as normal, that have a
Display or Debug trait. Then just Normal formatting specifiers apply, but for ranting
trait arguments normal formatting specifiers apply to specific parts of the content.

A placeholder to display a Ranting variable has this structure:

`{[,^]?(<article> |<verb> )?([+-]|#<var> )?[':@~]?<noun>( <verb>):<fmt>}`

With `,` and `^` lower- and uppercase are enforced, but a sentence start is assumed
to be uppercase - the start of a string or after `. ` - also an article or verb with
an uppercase enforces uppercase.

To force plurality use `+`, for a singular use `-`. If prependeded by a `#<var>` where
`<var>` is an integer in scope, plurality is adapted to the count, singular if 1,
otherwise plural. A verb or article is inflected along with the specified or default
plurality.

By default the name of a variable is displayed, but a pronoun with formatting markes:
subject with `:`, or `@` for object, `` ` ``: possesive, `~`: adjective. If a verb is
included in the placeholder, the noun is assumed to be subject.

Both a var and the noun can be prepended with a question mark, such as `#?<var>`,
to indicates that inflection rules occur accordingly, but the var or noun is not
displayed. The inflection may apply to the article and / or verb alongside.

The article can be one of `a`, `an`, `some` `the` `those` or `these`. If capitalized
this is retained. `These` and `those` are converted to `this` and `that` if the
pronoun is singular.

Positional argument and numeric references are supported, but not named arguments,
currently.

ack!() and nay!() are convenience wrappers respectively for 

```rust
return Ok(say!())
// and
return Err(say!())
```

ack!() and nay!() are not intended for normalerror handling; placeholders obfuscate
where an error originates from, rendering a simple string search less effective.
Instead use these as a allow / deny response where multiple actors could be involved.

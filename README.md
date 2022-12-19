
Generate `Ranting` trait implementations
```rust
#[derive(Ranting)]
struct Named {
    name: String,
    subject: String,
}
impl Named {
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
fn say_want_to_send(sender: &Named, receiver: &Named, message: &Named) -> String {
    say!("{0 want} to send {some message} of {'0} to {receiver}.", sender)
}

fn say_we_know_message(sender: &Named, receiver: &Named, message: &Named) -> String {
    say!("Now {:receiver know} that {these message are} really {~sender}.")
}

fn main() {
    let alice = Named {
        name: "Alice".to_string(),
        subject: "she".to_string(),
    };
    let bob = Named {
        name: "Bob".to_string(),
        subject: "he".to_string(),
    };
    let email = Named {
        name: "message".to_string(),
        subject: "it".to_string(),
    };

    // with Alice as sender
    assert_eq!(
        say_want_to_send(alice, bob, email)
        "Alice wants to send a message of her to Bob.".to_string()
    );
    assert_eq!(
        say_we_know_message(alice, bob, email)
        "Now he knows that this message is really hers.".to_string()
    );
    
    // With Bob as sender
    assert_eq!(
        say_want_to_send(bob, alice, email)
        "Bob wants to send a message of his to Alice.".to_string()
    );
    assert_eq!(
        say_we_know_message(alice, bob, email)
        "Now she knows that this message is really his.".to_string()
    );
}
```
For a more elaborate example see tests/ranting/male_female_and_object.rs

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
subject with `:`, or `@` for object, `'`: possesive, `~`: adjective. If a verb is
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

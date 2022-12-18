
Generates the `Ranting` trait implementations
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

    let msg = say!("{0 want} to send {some email} of {'0} to {bob}.", alice);
    assert_eq!(
        msg,
        "Alice wants to send a message of her to Bob.".to_string()
    );

    let msg = say!("Now {:bob know} that {these email are} really {~alice}.", alice);
    assert_eq!(
        msg,
        "Now he knows that this message is really hers.".to_string()
    );
    assert_eq!(
        say("{some email arrive} in {'alice} inbox."),
        "an email arrives in her inbox.".to_string()
    );
}
```
for a more elaborate example see tests/ranting/male_female_and_object.rs

Ranting trait objects as arguments to say!() are displayed either as name (by default)
or as pronoun and/or inflected, dependent on provided markers. Alongside the ranting
variable, articles and verbs can be included in the curly braces, that are inflected
accordingly.

Articles and verbs provided should have the plural form.


{[,^]?(<article> |<verb> )?([+-]|#var )?[':@~]?\??<noun>( <post_verb>)}

To force a plural form, use '+', to force singular use '-'. If prependeded by a
`#var `, where var is an integer in scope, the plurality is adapted to the variable
count, singular if 1, otherwise plural.

`:` gives a subject, `@` an object, `'` a possesive and `~` an adjective form of the
pronoun. Without the name is printed, not its pronoun. To force a plural form,
plural, 

With `:n` or `:N` the name is printed and with `:m` or `:M` the plural thereof.
With `:d` or `:D` a verb is reflected but the name is replaced by 'there', so that
e.g. `{self are:d}` becomes `there is` if singular, or `there are` if plural.

when prepended with `a` or `an`, this indefinite article is adapted to the name.
When capitalized this is preserved. Also `the`, `these` and `those` can occur before.
Ranting always uses the 1st plural form. `These` and `those` are converted to `this`
and `that` if the pronoun is singular.

In absence of upper or lower format specifiers, a noun at the start of a sentence or
after a dot will start with a capital.

If prepended with `#var` where var is a numeric variable, then the noun is inflected
accordingly, plural unless the value of var is 1. However, var should be an identifier,
not a numeric positional.

A verb after, again in the first plural form, is also inflected to the pronoun's case.
The Ranting object enclosed before a verb is assumed to be the subject in the sentence.

Positional argument and numeric references are supported, but not named arguments,
currently.

nay!() and ack!() are convenience wrappers respectively for 

```rust
return Err(say!())
// and
return Ok(say!())
```

ack!() and nay!() are not intended for normalerror handling; the usage of more placeholders
can obfuscate where an error originates from. They hinder a simple string search.
Instead they are intended to allow a diversity of reponses when different actors can be
involved.

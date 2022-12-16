
Generates the `Ranting` trait implementations
```rust
#[derive(Ranting)]
struct Named {
    name: String,
    pronoun: String,
}
impl Named {
    fn pronoun(&self) -> &str {
        self.pronoun.as_str()
    }
}
```
The struct should contain at least a name String. The trait provides the following
functions of which you may want to override the pronoun function:

```rust
pub trait Ranting: std::fmt::Display {
    fn pronoun(&self) -> &str;
    fn name(&self) -> &str;
    fn is_plural(&self) -> bool;
    fn a_or_an(&self, uc: bool) -> &str;
    fn subject(&self, uc: bool) -> &str;
    fn object(&self, uc: bool) -> &str;
    fn possesive(&self, uc: bool) -> &str;
    fn adjective(&self, uc: bool) -> &str;
    fn plural(&self, uc: bool) -> String;
    fn verb(&self, verb: &str) -> String;
    fn verb_for(&self, verb: &str, pronoun: &str) -> String;
}
```
Then these can be referenced in the say!() nay!() and ack!() macros.

The say!() macro produces a String, a bit similar to format!(), but with extended
formatting options for arguments with Ranting traits.

```rust
fn main() {
    let alice = Named {
        name: "Alice".to_string(),
        pronoun: "she".to_string(),
    };
    let bob = Named {
        name: "Bob".to_string(),
        pronoun: "he".to_string(),
    };
    let email = Named {
        name: "secret message".to_string(),
        pronoun: "it".to_string(),
    };

    let msg = say!("{0 want} to send {an email} of {0:p} to {bob}.", alice);
    assert_eq!(
        msg,
        "Alice wants to send a secret message of her to Bob.".to_string()
    );

    let msg = say!("Now {bob know} that {these email are} really {alice:a}.", alice);
    assert_eq!(
        msg,
        "Now Bob knows that this secret message is really hers.".to_string()
    );
}
```
for a more elaborate example see tests/ranting/male_female_and_object.rs

Ranting trait objects as arguments to say!()  are displated by name by
default, but by pronoun with the formatting extensions.

`:s` gives a subject, `:o` an object, `:p` the possesive and `:a` the adjective
form of the pronoun. With a capital, e.g. `:S`, the pronoun form is capitalized.

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

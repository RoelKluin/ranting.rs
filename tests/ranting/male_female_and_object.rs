// (c) Roel Kluin 2022 GPL v3

use ranting::Ranting;
use ranting_derive::*;
use std::collections::HashMap;

#[derive(Ranting)]
struct Object {
    name: String,
    pronoun: String,
}

impl Object {
    fn new(name: &str, pronoun: &str) -> Self {
        Object {
            name: name.to_string(),
            pronoun: pronoun.to_string(),
        }
    }
}

#[derive(Ranting)]
struct Person {
    name: String,
    pronoun: String,
    inventory: HashMap<String, usize>,
}

impl Person {
    fn new(name: &str, pronoun: &str) -> Self {
        Person {
            name: name.to_string(),
            pronoun: pronoun.to_string(),
            inventory: HashMap::new(),
        }
    }
    fn pronoun(&self) -> &str {
        self.pronoun.as_str()
    }
    fn respond_to<T, U>(
        &mut self,
        actor: &T,
        act: &str,
        nr_obj: Option<(usize, &U)>,
    ) -> Result<String, String>
    where
        T: Ranting,
        U: Ranting,
    {
        match (act, nr_obj) {
            ("give", Some((_, trash))) if trash.name() == "trash" => {
                // Verbs are always in 1st plural form.
                nay!("{The trash} from {actor} is not something that {self do} accept.")
            }
            ("give", Some((nr, coin))) if coin.name() == "coin" => match nr {
                0 => nay!("{actor don't:S} seem able to give zero {coin:m} to {self}."),
                n => {
                    let ent = self.inventory.entry(coin.name().to_string()).or_default();
                    *ent += nr;
                    ack!("{self thank:S} {0:o}, {0}, for {0:p} {#n coin}.", actor)
                }
            },
            ("receive", Some((nr, coin))) if coin.name() == "coin" && nr > 0 => {
                let ent = self.inventory.entry(coin.name().to_string()).or_default();
                if nr <= *ent {
                    ack!("Reluctantly {actor give:s} {#nr coin} for {self:o}");
                } else {
                    nay!("{actor do:S} not have {#nr coin} to give to {self:o}");
                }
            }
            (act, Some((nr, item))) => nay!("{actor can:S} not {act} {nr} {item}s to {self}"),
            (act, None) => nay!("{actor shouldn't:S} {act} {self}."),
        }
    }
}

#[test]
fn male_female_and_object() {
    let mut anna = Person::new("Anna", "I");
    let mut bob = Person::new("Bob", "he");
    let rubbish = Object::new("trash", "they");
    let coin = Object::new("coin", "it");

    let ret = anna.respond_to(&bob, "give", Some((1, &rubbish)));
    assert_eq!(
        ret,
        Err("The trash from Bob is not something that I do accept.".to_string())
    );

    let ret = bob.respond_to(&anna, "give", Some((1, &rubbish)));
    assert_eq!(
        ret,
        Err("The trash from Anna is not something that he does accept.".to_string())
    );

    let ret = anna.respond_to(&bob, "give", Some((0, &coin)));
    assert_eq!(
        ret,
        Err("He doesn't seem able to give zero coins to Anna.".to_string())
    );

    let ret = bob.respond_to(&anna, "give", Some((0, &coin)));
    assert_eq!(
        ret,
        Err("I don't seem able to give zero coins to Bob.".to_string())
    );

    let ret = bob.respond_to(&anna, "give", Some((1, &coin)));
    assert_eq!(ret, Ok("He thanks me, Anna, for my 1 coin.".to_string()));

    let ret = anna.respond_to(&bob, "give", Some((4, &coin)));
    assert_eq!(ret, Ok("I thank him, Bob, for his 4 coins.".to_string()));

    let ret = anna.respond_to::<Person, Person>(&bob, "push", None);
    assert_eq!(ret, Err("He shouldn't push Anna.".to_string()));
}

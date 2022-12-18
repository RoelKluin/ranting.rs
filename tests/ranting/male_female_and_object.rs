// (c) Roel Kluin 2022 GPL v3

use ranting::Ranting;
use ranting_derive::*;
use std::collections::HashMap;

#[derive(Ranting)]
struct Object {
    name: String,
    subject: String,
}

#[derive(Ranting)]
struct Meadowers {
    name: String,
    count: u32,
}

impl Meadowers {
    fn new(name: &str, count: u32) -> Self {
        Meadowers {
            name: name.to_string(),
            count,
        }
    }
    fn subjective(&self) -> &str {
        if self.count == 1 {
            "it"
        } else {
            "they"
        }
    }
    fn count(&self) -> String {
        let count = self.count;
        say!("Now there {?self are} {#count self} in the meadow.")
    }
    fn join(&mut self, newcomer: Meadowers) -> String {
        let s = say!("{The newcomer join} {the +self} in the meadow.");
        self.count += newcomer.count;
        s
    }
}

impl Object {
    fn new(name: &str, subject: &str) -> Self {
        Object {
            name: name.to_string(),
            subject: subject.to_string(),
        }
    }
}

#[derive(Ranting)]
struct Person {
    name: String,
    subject: String,
    inventory: HashMap<String, usize>,
}

impl Person {
    fn new(name: &str, subject: &str) -> Self {
        Person {
            name: name.to_string(),
            subject: subject.to_string(),
            inventory: HashMap::new(),
        }
    }
    fn subject(&self) -> &str {
        self.subject.as_str()
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
            ("give", Some((_, trash))) if trash.name(false).as_str() == "trash" => {
                // Verbs are always in 1st plural form.
                nay!("{The trash} from {actor} is not something that {:self do} accept.")
            }
            ("give", Some((nr, coin))) if coin.name(false).as_str() == "coin" => match nr {
                0 => nay!("{actor don't} seem able to give zero {+coin} to {self}. {actor frown} at {self}."),
                n => {
                    let ent = self
                        .inventory
                        .entry(coin.name(false).to_string())
                        .or_default();
                    *ent += nr;
                    ack!("{self thank} {@0}, {0}, for {'0} {#n coin}.", actor)
                }
            },
            ("receive", Some((nr, coin))) if coin.name(false).as_str() == "coin" && nr > 0 => {
                let ent = self.inventory.entry(coin.name(false)).or_default();
                if nr <= *ent {
                    ack!("Reluctantly, {actor give} {#nr coin} to {@self}");
                } else {
                    nay!("{actor do} not have {#nr coin} to give to {@self}");
                }
            }
            (act, Some((nr, item))) => nay!("{actor can} not {act} {#nr item} to {self}"),
            (act, None) => nay!("{actor shouldn't} {act} {self}."),
        }
    }
}

#[test]
fn male_female_and_object() {
    let mut anna = Person::new("Anna", "I");
    let mut bob = Person::new("Bob", "he");

    let mut pack = Meadowers::new("animal", 0);
    assert_eq!(pack.count(), "Now there are 0 animals in the meadow.");

    assert_eq!(
        pack.join(Meadowers::new("raven", 1)),
        "The raven joins the animals in the meadow."
    );
    assert_eq!(pack.count(), "Now there is 1 animal in the meadow.");

    assert_eq!(
        pack.join(Meadowers::new("sheep", 3)),
        "The sheep join the animals in the meadow."
    );
    assert_eq!(pack.count(), "Now there are 4 animals in the meadow.");

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
        Err("Bob doesn't seem able to give zero coins to Anna. He frowns at Anna.".to_string())
    );

    let ret = bob.respond_to(&anna, "give", Some((0, &coin)));
    assert_eq!(
        ret,
        Err("I, Anna, don't seem able to give zero coins to Bob. I frown at Bob.".to_string())
    );

    let ret = bob.respond_to(&anna, "give", Some((1, &coin)));
    assert_eq!(ret, Ok("He thanks me, Anna, for my 1 coin.".to_string()));

    let ret = anna.respond_to(&bob, "give", Some((4, &coin)));
    assert_eq!(ret, Ok("I thank him, Bob, for his 4 coins.".to_string()));

    let ret = anna.respond_to::<Person, Person>(&bob, "push", None);
    assert_eq!(ret, Err("He shouldn't push Anna.".to_string()));
}

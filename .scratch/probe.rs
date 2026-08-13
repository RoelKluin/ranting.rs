use ranting::say;
use ranting_es::{SpanishNoun, SpanishPerson};

fn main() {
    let gato = SpanishNoun::gato();
    let casa = SpanishNoun::casa();
    let problema = SpanishNoun::problema();
    let agua = SpanishNoun::agua();

    println!("{:?}", say!("{the *=0 !negro}", gato));
    println!("{:?}", say!("{the *=0 !negro}", casa));
    println!("{:?}", say!("{the +*=0 !negro}", gato));
    println!("{:?}", say!("{the *=0 ser} negro.", gato));
    println!("{:?}", say!("{the *=0}", problema));
    println!("{:?}", say!("{the *=0}", agua));
    println!("{:?}", say!("{the +*=0}", agua));
    println!("{:?}", say!("{a *=0}", agua));
    println!("{:?}", say!("Vengo de {the *=0}.", gato));
    println!("{:?}", say!("Voy a {the *=0}.", gato));
    println!("{:?}", say!("¿{the *=0 ser} negro?", gato));
    println!("{:?}", say!("{=0 hablar}.", SpanishPerson::TU));
    println!("{:?}", say!("{=0 hablar}.", SpanishPerson::USTED));
    println!("{:?}", say!("{=0 hablar}.", SpanishPerson::USTEDES));
    println!("{:?}", say!("{=0 hablar}.", SpanishPerson::VOSOTROS));
    println!("{:?}", say!("{#0 1}", 1, gato));
    println!("{:?}", say!("{#0 1}", 1, agua));
    println!("{:?}", say!("{#0 1}", 2, gato));
    println!("{:?}", say!("{#0 1}", 12, casa));
    println!("{:?}", say!("{#0 1}", 40, gato));
    println!("{:?}", say!("Vi {@0}.", gato));
    println!("{:?}", say!("Vi {@0}.", casa));
    println!("{:?}", say!("Vi {@0}.", agua));
    println!("{:?}", say!("Veo {#0 1}.", 1, gato));
    println!("{:?}", say!("{the *=0 hablar}.", gato));
    println!("{:?}", say!("{=0 hablar}.", gato));
    println!("{:?}", say!("{=0}.", gato));
    println!("{:?}", say!("{the *=0 !good}", gato));
    println!("{:?}", say!("{the *=0 correr}.", gato));
    println!("{:?}", say!("{`0} gato.", SpanishPerson::TU));
    println!("{:?}", say!("{=0 hablar} {%0}.", SpanishPerson::NOSOTROS));
}

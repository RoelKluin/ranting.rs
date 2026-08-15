use ranting::say;
use ranting::Noun;
#[test]
fn spot() {
    let item = Noun::new("item", "it");
    let n: i64 = 2;
    println!("A [{}]", say!("{#n item} fell.", n = n));
    println!("B [{}]", say!("{the #n item} fell.", n = n));
    println!("C [{}]", say!("{$n item} fell.", n = n));
    println!("D [{}]", say!("I see {#n item}.", n = n));
    println!("E [{}]", say!("{#n item} {=item are} here.", n = n));
    let one: i64 = 1;
    println!("F [{}]", say!("{#n item} fell.", n = one));
}

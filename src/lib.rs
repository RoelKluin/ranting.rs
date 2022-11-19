// (c) RoelKluin 2022 GPL v3

pub use in_definite;

#[rustfmt::skip]
pub trait Ranting: std::fmt::Display {
    fn pronoun(&self) -> &str;
    fn name(&self) -> &str;
    fn a_or_an(&self) -> &str;
    fn subject(&self, uc: bool) -> &str;
    fn object(&self, uc: bool) -> &str;
    fn possesive(&self, uc: bool) -> &str;
    fn adjective(&self, uc: bool) -> &str;
    fn verb(&self, verb: &str) -> String;
}

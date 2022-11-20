// (c) Roel Kluin 2022 GPL v3

pub use in_definite;
pub use inflector::Inflector;

#[rustfmt::skip]
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
}

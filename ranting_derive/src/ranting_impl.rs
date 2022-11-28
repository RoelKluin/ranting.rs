// (c) RoelKluin 2022 GPL v3

use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(object))]
pub(crate) struct RantingOptions {
    pub(crate) pronoun: Option<String>,
}

/// An abstract thing, which may be a person and have a gender
pub(crate) fn ranting_q(opts: RantingOptions, ident: &Ident) -> TokenStream {
    let pronoun = opts.pronoun.unwrap_or_else(|| "it".to_string());
    quote! {
        impl Ranting for #ident {
            fn pronoun(&self) -> &str {
                #pronoun
            }
            fn name(&self, uc: bool) -> String {
                if uc {
                    use ranting::Inflector;
                    self.name.to_sentence_case()
                } else {
                    self.name.to_string()
                }
            }
            fn is_plural(&self) -> bool {
                match self.pronoun() {
                    "we" | "they" => true,
                    _ => false,
                }
            }
            fn a_or_an(&self, uc: bool) -> &str {
                if self.is_plural() {
                    if uc {"Some"} else {"some"}
                } else {
                    match ranting::in_definite::get_a_or_an(self.name.as_str()) {
                        "a" if uc => "A",
                        "an" if uc => "An",
                        alt => alt,
                    }
                }
            }
            fn plural(&self, uc: bool) -> String {
                use ranting::Inflector;
                self.name(uc).as_str().to_plural()
            }
            fn subject(&self, uc: bool) -> &str {
                if uc {
                    match self.pronoun() {
                        "I" => "I",
                        "you" => "You",
                        "he" => "He",
                        "she" => "She",
                        "we" => "We",
                        "they" => "They",
                        "it" => "It",
                        p => panic!("Unimplemented: subjective for '{}'", p),
                    }
                } else {
                    self.pronoun()
                }
            }
            fn object(&self, uc: bool) -> &str {
                match self.pronoun() {
                    "I" => if uc {"Me"} else {"me"},
                    "you" => if uc {"Your"} else {"your"},
                    "he" => if uc {"Him"} else {"him"},
                    "she" => if uc {"Her"} else {"her"},
                    "it" => if uc {"It"} else {"it"},
                    "we" => if uc {"Us"} else {"us"},
                    "they" => if uc {"Them"} else {"them"},
                    p => panic!("Unimplemented: objective for '{}'", p),
                }
            }
            fn possesive(&self, uc: bool) -> &str {
                match self.pronoun() {
                    "I" => if uc {"My"} else {"my"},
                    "you" => if uc {"Your"} else {"your"},
                    "he" => if uc {"His"} else {"his"},
                    "she" => if uc {"Her"} else {"her"},
                    "it" => if uc {"Its"} else {"its"},
                    "we" => if uc {"Our"} else {"our"},
                    "they" => if uc {"Their"} else {"their"},
                    p => panic!("Unimplemented: possesive for '{}'", p),
                }
            }
            fn adjective(&self, uc: bool) -> &str {
                match self.pronoun() {
                    "I" => if uc {"Mine"} else {"mine"},
                    "you" => if uc {"Yours"} else {"yours"},
                    "he" => if uc {"His"} else {"his"},
                    "she" => if uc {"Hers"} else {"hers"},
                    "it" => if uc {"Its"} else {"its"},
                    "we" => if uc {"Ours"} else {"ours"},
                    "they" => if uc {"Theirs"} else {"theirs"},
                    p => panic!("Unimplemented adjective for '{}'", p),
                }
            }
            fn verb(&self, verb: &str) -> String {
                match self.pronoun() {
                    "I" => match verb {
                        "'re" => "'m".to_string(),
                        " are" => " am".to_string(),
                        " were" => " was".to_string(),
                        " aren't" => " am not".to_string(),
                        " weren't" => " wasn't".to_string(),
                        v => v.to_string()
                    },
                    "you" | "we" | "they" => verb.to_string(),
                    _ => {
                        match verb {
                            "'re" | "'ve" => "'s".to_string(),
                            " are" => " is".to_string(),
                            " have" => " has".to_string(),
                            " were" => " was".to_string(),
                            " do" => " does".to_string(),
                            " aren't" => " isn't".to_string(),
                            " weren't" => " wasn't".to_string(),
                            " don't" => " doesn't".to_string(),
                            " could" | " would" | " can" | " may" | " might" | " must" | " should" | " shall" | " will" | " had" |
                            " couldn't" | " wouldn't" | " can't" | " mightn't" | " mustn't" | " shouldn't" | " hadn't" => verb.to_string(),
                            v => {
                                if v.ends_with(&['s', 'o', 'x']) || v.ends_with("ch") || v.ends_with("sh") {
                                    format!("{}es", v)
                                } else if let Some(p) = v.strip_suffix('y').filter(|p| !p.ends_with(&['a', 'e', 'u', 'o'])) {
                                    format!("{}ies", p);
                                } else {
                                    format!("{}s", v)
                                }
                            }
                        }
                    },
                }
            }

        }
        impl std::fmt::Display for #ident {
            fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "{}", self.name(false))
            }
        }
    }
}

// (c) RoelKluin 2022 GPL v3

mod ranting_impl;

#[path = "../language/english.rs"]
#[allow(dead_code)]
mod english;
use english as language;

use darling::{FromDeriveInput, ToTokens};
use english as language;
use lazy_static::lazy_static;
use proc_macro::{self, TokenStream as TokenStream1};
use proc_macro2::{Punct, Spacing, Span, TokenStream};
use quote::{quote, TokenStreamExt};
use ranting_impl::*;
use regex::{Captures, Regex};
use syn::{
    self,
    parse::{Parse, ParseStream, Parser},
    parse_macro_input,
    punctuated::Punctuated,
    DeriveInput, Error, Expr, LitStr, Token,
};

// TODO: replace Span::mixed_site() with more precise location.

struct Say {
    lit_str: String,
    params: Vec<Expr>,
}

impl Parse for Say {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Err(syn::Error::new(Span::call_site(), "missing format string"));
        }
        let lit = input.parse::<LitStr>()?;

        let params_in = if input.is_empty() {
            vec![]
        } else {
            input.parse::<Token![,]>()?;

            input
                .parse_terminated::<_, Token![,]>(syn::Expr::parse)?
                .into_iter()
                .collect()
        };
        lazy_static! {
            static ref RE: Regex = Regex::new(language::RANTING_PLACEHOLDER).unwrap();
        }
        let mut err = None;
        #[cfg(feature = "debug")]
        eprintln!("{}", lit.value());
        let mut params = vec![];

        let lit_str = RE
            .replace_all(&lit.value(), |caps: &Captures| {
                match handle_param(caps, &params_in, &mut params) {
                    Ok(s) => s,
                    Err(e) => {
                        err = Some(e);
                        String::new()
                    }
                }
            })
            .to_string();

        match err {
            Some(e) => Err(e),
            None => Ok(Say { lit_str, params }),
        }
    }
}

impl ToTokens for Say {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut macro_tokens = TokenStream::new();
        let expr = get_lit_str(self.lit_str.as_str());

        expr.to_tokens(&mut macro_tokens);
        for param in self.params.iter() {
            macro_tokens.append(Punct::new(',', Spacing::Alone));
            param.to_tokens(&mut macro_tokens);
        }
        let mut segments = Punctuated::new();
        segments.push(syn::PathSegment {
            ident: syn::Ident::new("format", Span::call_site()),
            arguments: syn::PathArguments::None,
        });
        let mac = Expr::Macro(syn::ExprMacro {
            attrs: vec![],
            mac: syn::Macro {
                path: syn::Path {
                    leading_colon: None,
                    segments,
                },
                bang_token: syn::Token![!]([Span::mixed_site()]),
                delimiter: syn::MacroDelimiter::Paren(syn::token::Paren {
                    span: Span::mixed_site(),
                }),
                tokens: macro_tokens,
            },
        });
        mac.to_tokens(tokens);
        #[cfg(feature = "debug")]
        eprintln!("{}", tokens.to_string());
    }
}

fn get_lit_bool(value: bool) -> syn::Expr {
    Expr::Lit(syn::ExprLit {
        attrs: vec![],
        lit: syn::Lit::Bool(syn::LitBool {
            value,
            span: Span::mixed_site(),
        }),
    })
}
fn get_lit_str(value: &str) -> syn::Expr {
    Expr::Lit(syn::ExprLit {
        attrs: vec![],
        lit: syn::Lit::Str(syn::LitStr::new(value, Span::mixed_site())),
    })
}

fn get_lit_int(repr: &str) -> syn::Expr {
    Expr::Lit(syn::ExprLit {
        attrs: vec![],
        lit: syn::Lit::Int(syn::LitInt::new(repr, Span::mixed_site())),
    })
}

fn path_from_segs<S: AsRef<str>>(segs: &[S]) -> syn::Expr {
    let mut segments = Punctuated::new();
    for seg in segs.iter() {
        segments.push(syn::PathSegment {
            ident: syn::Ident::new(seg.as_ref(), Span::call_site()),
            arguments: syn::PathArguments::None,
        });
    }
    Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments,
        },
    })
}

fn fn_call_from_path(path: Expr, mut arg_vec: Vec<Expr>) -> syn::Expr {
    let mut args = Punctuated::new();
    for arg in arg_vec.drain(..) {
        args.push(arg);
    }
    Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: std::boxed::Box::new(path),
        paren_token: syn::token::Paren {
            span: Span::mixed_site(),
        },
        args,
    })
}

fn fn_call_from_segs<S: AsRef<str>>(segs: &[S], arg_vec: Vec<Expr>) -> syn::Expr {
    fn_call_from_path(path_from_segs(segs), arg_vec)
}

fn get_method_call(path: Expr, method: &str, mut arg_vec: Vec<Expr>) -> syn::Expr {
    let mut args = Punctuated::new();
    for arg in arg_vec.drain(..) {
        args.push(arg);
    }
    Expr::MethodCall(syn::ExprMethodCall {
        attrs: vec![],
        receiver: Box::new(path),
        dot_token: syn::Token![.]([Span::mixed_site()]),
        method: syn::Ident::new(method, Span::mixed_site()),
        paren_token: syn::token::Paren {
            span: Span::mixed_site(),
        },
        args,
        turbofish: None,
    })
}

fn get_caps_expr(caps: &Captures, given: &[Expr], name: &str) -> Result<Expr, Error> {
    if let Some(part) = caps.name(name).map(|s| s.as_str()) {
        let expr = if let Ok(u) = part.parse::<usize>() {
            given
                .get(u)
                .ok_or_else(|| {
                    Error::new(
                        Span::mixed_site(),
                        format!("for {name} {part}, positional {u} is out of bounds"),
                    )
                })?
                .clone()
        } else {
            path_from_segs(&[part])
        };
        Ok(expr)
    } else {
        Err(Error::new(
            Span::mixed_site(),
            format!("The {name} is missing in a placeholder."),
        ))
    }
}

fn res_pos_push(res: &mut String, pos: &mut Vec<Expr>, expr: Expr, ofmt: Option<&str>) {
    res.push('{');
    res.push_str(pos.len().to_string().as_str());
    if let Some(fmt) = ofmt {
        res.push_str(fmt);
    }
    res.push('}');
    pos.push(expr);
}

// arguments become positionals
fn handle_param(caps: &Captures, given: &[Expr], pos: &mut Vec<Expr>) -> Result<String, Error> {
    let mut is_plain_placeholder = true;
    let mut nr: Option<Expr> = None;
    let mut noun_space = caps.name("sp1");
    let mut post_space = caps.name("sp3").map_or("", |m| m.as_str());
    let post = caps.name("post1").or_else(|| caps.name("post2"));
    let ofmt = caps.name("fmt").map(|s| s.as_str());

    // could be a struct or enum with a Ranting trait or just be a regular
    //  placeholder if withou all other SayPlaceholder elements.
    let noun = get_caps_expr(caps, given, "noun")?;

    let is_pl = if let Some(plurality) = caps.name("plurality") {
        is_plain_placeholder = false;
        match plurality.as_str().chars().next().unwrap() {
            '+' => get_lit_bool(true),
            '-' => get_lit_bool(false),
            c => {
                noun_space = caps.name("sp2");
                let expr = get_caps_expr(caps, given, "nr")?;
                // "#", "#?" or "?#" are captured in RE but not "??" or "##".
                if c != '?' {
                    nr = Some(expr.clone()); // nr not assigned!
                }
                Expr::Binary(syn::ExprBinary {
                    attrs: vec![],
                    left: Box::new(expr),
                    op: syn::BinOp::Ne(syn::Token![!=]([Span::mixed_site(); 2])),
                    right: Box::new(get_lit_int("1")),
                })
            }
        }
    } else {
        get_method_call(noun.clone(), "is_plural", vec![])
    };
    let mut res = caps.name("sentence").map_or("", |s| s.as_str()).to_owned();

    // uppercase if 1) noun has a caret ('^'), otherwise if not lc ('.') is specified
    // 2) uc if article or so is or 3) the noun is first or after start or `. '
    let mut uc = if let Some(m) = caps.name("uc") {
        is_plain_placeholder = false;
        m.as_str() == "^"
    } else {
        // or if article has uc or the noun is first or at new sentence
        caps.name("pre")
            .filter(|s| s.as_str().starts_with(|c: char| c.is_uppercase()))
            .is_some()
            || caps
                .name("sentence")
                .filter(|m| m.start() == 0 || m.as_str() != "")
                .is_some()
    };

    // This may be an article or certain verbs that can occur before the noun:
    if let Some(pre) = caps.name("pre") {
        is_plain_placeholder = false;
        let p = pre.as_str().to_lowercase();
        let call = if language::is_article_or_so(p.as_str()) {
            fn_call_from_segs(
                &["ranting", "adapt_article"],
                vec![
                    get_method_call(
                        noun.clone(),
                        "indefinite_article",
                        vec![get_lit_bool(false)],
                    ),
                    get_lit_str(p.as_str()),
                    is_pl.clone(),
                    get_lit_bool(uc),
                ],
            )
        } else {
            assert!(post.is_none(), "verb before and after?");
            fn_call_from_segs(
                &["ranting", "inflect_verb"],
                vec![
                    get_method_call(noun.clone(), "subjective", vec![]),
                    get_lit_str(p.as_str()),
                    is_pl.clone(),
                    get_lit_bool(uc),
                ],
            )
        };
        res_pos_push(&mut res, pos, call, None);
        res.push_str(caps.name("s_pre").map_or("", |m| m.as_str()));
        uc = false;
    }

    if let Some(etc1) = caps.name("etc1") {
        is_plain_placeholder = false;
        res.push_str(etc1.as_str());
    }
    if let Some(expr) = nr {
        res.push_str(caps.name("sp1").map_or("", |m| m.as_str()));
        res_pos_push(&mut res, pos, expr, ofmt);
    }
    // also if case is None, the noun should be printed.
    let opt_case = caps.name("case").map(|m| m.as_str());
    if opt_case != Some("?") {
        res.push_str(noun_space.map_or("", |m| m.as_str()));
        match opt_case.and_then(language::get_case_from_str) {
            Some(case) if case.ends_with("ive") => {
                let segs = &["ranting".to_string(), format!("inflect_{case}")];
                let arg_vec = vec![
                    get_method_call(noun.clone(), "subjective", vec![]),
                    is_pl.clone(),
                    get_lit_bool(uc),
                ];
                res_pos_push(&mut res, pos, fn_call_from_segs(segs, arg_vec), None);
            }
            Some(word_angular) => {
                let word = word_angular.trim_end_matches('>');
                let segs = &["ranting", "inflect_noun"];
                let arg_vec = vec![
                    get_method_call(noun.clone(), "mut_name", vec![get_lit_str(word)]),
                    get_method_call(noun.clone(), "is_plural", vec![]),
                    is_pl.clone(),
                    get_lit_bool(uc),
                ];
                res_pos_push(&mut res, pos, fn_call_from_segs(segs, arg_vec), None);
            }
            None if is_plain_placeholder && caps.name("etc2").is_none() && post.is_none() => {
                res_pos_push(&mut res, pos, noun.clone(), ofmt);
            }
            None => {
                let segs = &["ranting", "inflect_noun"];
                let arg_vec = vec![
                    get_method_call(noun.clone(), "name", vec![get_lit_bool(false)]),
                    get_method_call(noun.clone(), "is_plural", vec![]),
                    is_pl.clone(),
                    get_lit_bool(uc),
                ];
                res_pos_push(&mut res, pos, fn_call_from_segs(segs, arg_vec), None);
            }
        }
        uc = false;
    } else if noun_space.is_none() {
        post_space = "";
    }
    if let Some(etc2) = caps.name("etc2") {
        res.push_str(etc2.as_str());
    }
    if let Some(post) = post.map(|m| m.as_str()) {
        res.push_str(post_space);
        let call = match post {
            "'" | "'s" => fn_call_from_segs(
                &["ranting", "adapt_possesive_s"],
                vec![
                    get_method_call(noun.clone(), "name", vec![get_lit_bool(false)]),
                    get_method_call(noun, "is_plural", vec![]),
                    is_pl,
                ],
            ),
            verb => fn_call_from_segs(
                &["ranting", "inflect_verb"],
                vec![
                    get_method_call(noun, "subjective", vec![]),
                    get_lit_str(verb),
                    is_pl,
                    get_lit_bool(uc),
                ],
            ),
        };
        res_pos_push(&mut res, pos, call, None);
    }
    Ok(res)
}

#[proc_macro]
pub fn ack(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Say);
    quote!(return Ok(#input)).into()
}

#[proc_macro]
pub fn nay(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Say);
    quote!(return Err(#input)).into()
}

#[proc_macro]
pub fn say(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Say);
    quote!(#input).into()
}

/// Implies `#[derive(Ranting)]` and includes `name` and `subject` in structs.
/// For an enum `"it"` and the variant's name are assumed.
#[proc_macro_attribute]
pub fn derive_ranting(_args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            if let syn::Fields::Named(fields) = &mut struct_data.fields {
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { name: String })
                        .unwrap(),
                );
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { subject: String })
                        .unwrap(),
                );
            }

            quote! {
                #[derive(Ranting)]
                #ast
            }
            .into()
        }
        syn::Data::Enum(_) => quote! {
            #[derive(ranting::strum_macros::Display, Ranting)]
            #ast
        }
        .into(),
        _ => panic!("`add_field` has to be used with structs or enums"),
    }
}

/// Above macros inflect Ranting elements within a placeholder. Structs require a `name` and `subject` String.
#[proc_macro_derive(Ranting, attributes(ranting))]
pub fn inner_derive_ranting(input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input);
    let mut options =
        RantingOptions::from_derive_input(&input).expect("Invalid Thing trait options");
    if let syn::Data::Enum(_) = &input.data {
        options.is_enum = true;
    }
    ranting_q(options, &input.ident).into()
}

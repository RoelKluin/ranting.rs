// (c) Roel Kluin 2026 MIT
// #[derive(Heed)] — v2 of heed!() (ROADMAP.md Phase 3 item 8): generates
// `fn heed(input: &str) -> Option<Self>` on a struct from a
// `#[heed(template = "...")]` attribute, reusing heed!()'s own template
// compiler (`crate::heed::compile_heed_template`) rather than duplicating
// the matching engine. Every template capture must have a same-named field
// (and vice versa) — this is a one-to-one mapping, not a partial one, so a
// stale field or a renamed capture is a compile error instead of a silent
// `Default::default()`-style gap.

use crate::heed::{HeedCapture, HeedCaptureKind, compile_heed_template};
use crate::str_lit::StrLit;
use darling::FromDeriveInput;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident, Type};

#[derive(FromDeriveInput)]
#[darling(attributes(heed))]
struct HeedOptions {
    template: String,
}

fn expected_type_name(kind: HeedCaptureKind) -> &'static str {
    match kind {
        HeedCaptureKind::Word | HeedCaptureKind::Greedy => "String",
        HeedCaptureKind::Number => "u64",
    }
}

fn type_name(ty: &Type) -> String {
    quote!(#ty).to_string().replace(' ', "")
}

pub(crate) fn derive_heed(input: DeriveInput) -> TokenStream {
    let ident = input.ident.clone();

    let opts = match HeedOptions::from_derive_input(&input) {
        Ok(opts) => opts,
        Err(e) => return e.write_errors(),
    };

    let is_unit_struct = matches!(&input.data, Data::Struct(s) if matches!(s.fields, Fields::Unit));

    let named: Vec<_> = match &input.data {
        Data::Struct(s) => match &s.fields {
            Fields::Named(named) => named.named.iter().collect(),
            Fields::Unit => Vec::new(),
            Fields::Unnamed(_) => {
                return syn::Error::new_spanned(
                    &ident,
                    "#[derive(Heed)] does not support tuple structs — use named fields",
                )
                .to_compile_error();
            }
        },
        _ => {
            return syn::Error::new_spanned(
                &ident,
                "#[derive(Heed)] only supports structs, not enums or unions",
            )
            .to_compile_error();
        }
    };

    let template_lit = syn::LitStr::new(&opts.template, ident.span());
    let (pattern, captures): (String, Vec<HeedCapture>) =
        match compile_heed_template(&StrLit::new(template_lit)) {
            Ok(r) => r,
            Err(e) => return e.to_compile_error(),
        };

    let mut remaining: std::collections::HashMap<String, HeedCapture> =
        captures.into_iter().map(|c| (c.name.clone(), c)).collect();

    let mut field_idents = Vec::new();
    let mut capture_names = Vec::new();
    let mut convert_exprs = Vec::new();

    for field in named {
        let field_ident: &Ident = field.ident.as_ref().expect("named field has an ident");
        let name = field_ident.to_string();

        let capture = match remaining.remove(&name) {
            Some(c) => c,
            None => {
                return syn::Error::new_spanned(
                    field_ident,
                    format!(
                        "field `{name}` has no matching `{{{name}}}` capture in the #[heed(template = ...)] pattern"
                    ),
                )
                .to_compile_error();
            }
        };

        let expected = expected_type_name(capture.kind);
        if type_name(&field.ty) != expected {
            return syn::Error::new_spanned(
                &field.ty,
                format!(
                    "field `{name}` must be `{expected}` to match its `{{{marker}{name}}}` capture, found `{found}`",
                    marker = if capture.kind == HeedCaptureKind::Number { "$" } else { "" },
                    found = type_name(&field.ty),
                ),
            )
            .to_compile_error();
        }

        let convert = match capture.kind {
            HeedCaptureKind::Word | HeedCaptureKind::Greedy => quote! {
                __v.next().expect("heed!() matched capture count mismatch")
            },
            HeedCaptureKind::Number => quote! {
                __v.next().expect("heed!() matched capture count mismatch").parse::<u64>().ok()?
            },
        };

        field_idents.push(field_ident.clone());
        capture_names.push(name);
        convert_exprs.push(convert);
    }

    if let Some(orphan) = remaining.keys().next() {
        return syn::Error::new_spanned(
            &ident,
            format!(
                "capture `{{{orphan}}}` in the #[heed(template = ...)] pattern has no matching field"
            ),
        )
        .to_compile_error();
    }

    // A true unit struct (`struct Foo;`) must be constructed as bare `Self`, not `Self {}` —
    // the latter is only valid for a struct declared with (possibly empty) braces, so an
    // empty-but-braced struct (`struct Foo {}`) needs `Self {}`, not `Self`. Likewise, an
    // unused `__v` binding when there are no fields to pop from it would warn under
    // `-D warnings`.
    let (bind_caps, construct) = if field_idents.is_empty() {
        let ctor = if is_unit_struct {
            quote! { Self }
        } else {
            quote! { Self {} }
        };
        (quote! { let _ = __ranting_heed_caps; }, ctor)
    } else {
        (
            quote! { let mut __v = __ranting_heed_caps.into_iter(); },
            quote! { Self { #(#field_idents: #convert_exprs,)* } },
        )
    };

    quote! {
        impl #ident {
            /// Matches `input` against the `#[heed(template = "...")]` pattern and,
            /// on a match, constructs `Self` from the named captures. Generated by
            /// `#[derive(Heed)]`.
            pub fn heed(input: &str) -> Option<Self> {
                static __RANTING_HEED_MATCHER: ranting::HeedMatcher =
                    ranting::HeedMatcher::new(#pattern, &[#(#capture_names),*]);
                let __ranting_heed_caps = __RANTING_HEED_MATCHER.match_input(input)?;
                #bind_caps
                Some(#construct)
            }
        }
    }
}

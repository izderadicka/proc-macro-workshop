use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{spanned::Spanned as _, Data, Ident, Type};

fn gen_type(ty: &Type, name: &str) -> Option<Type> {
    match ty {
        Type::Path(path)
            if path.path.segments.len() == 1 && path.path.segments[0].ident == name =>
        {
            match &path.path.segments[0].arguments {
                syn::PathArguments::AngleBracketed(inner) => {
                    inner.args.iter().next().and_then(|arg| {
                        if let syn::GenericArgument::Type(ty) = arg {
                            Some(ty.clone())
                        } else {
                            None
                        }
                    })
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn parse_key_value(tokens: proc_macro2::TokenStream) -> Option<(String, String)> {
    let mut iter = tokens.into_iter();
    let key = iter.next().and_then(|t| match t {
        proc_macro2::TokenTree::Ident(ident) => Some(ident),
        _ => None,
    });
    if key.is_none() {
        return None;
    }

    let punct = iter.next().and_then(|t| match t {
        proc_macro2::TokenTree::Punct(punct) if punct.as_char() == '=' => Some(punct),
        _ => None,
    });
    if punct.is_none() {
        return None;
    }

    iter.next().and_then(|t| match t {
        proc_macro2::TokenTree::Literal(lit) => Some(lit.to_string())
            .and_then(|s| {
                s.strip_prefix("\"")
                    .and_then(|s| s.strip_suffix("\"").map(|s| s.to_string()))
            })
            .map(|s| (key.unwrap().to_string(), s)),
        _ => None,
    })
}

fn attr_value(
    attrs: &Vec<syn::Attribute>,
    name: &str,
    key: &str,
) -> Result<Option<String>, syn::Error> {
    let res = attrs
        .iter()
        .filter_map(|attr| {
            if matches!(attr.style, syn::AttrStyle::Outer) && attr.path().is_ident(name) {
                if let syn::Meta::List(a) = &attr.meta {
                    return parse_key_value(a.tokens.clone()).map(|(k, v)| (k, v, a.tokens.span()));
                }
            }

            None
        })
        .next()
        .map(|(k, v, span)| {
            if k == key {
                Ok(v)
            } else {
                let msg = format!("expected `{name}({key} = \"...\")`");
                Err(syn::Error::new(span, msg))
            }
        });
    res.transpose()
}

struct FieldInfo<'a> {
    name: &'a syn::Ident,
    ty: syn::Type,
    optional: bool,
    each_method_name: Result<Option<String>, syn::Error>,
    vec_type: Option<syn::Type>,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;
    let builder_name = format_ident!("{}Builder", name);

    if let Data::Struct(data) = input.data {
        let fields = data
            .fields
            .iter()
            .filter_map(|field| match &field.ident {
                Some(ident) => {
                    let ty = gen_type(&field.ty, "Option");
                    let optional = ty.is_some();
                    let ty = ty.unwrap_or(field.ty.clone());

                    let attr = attr_value(&field.attrs, "builder", "each");
                    let vec_type = gen_type(&ty, "Vec");

                    Some(FieldInfo {
                        name: ident,
                        ty,
                        optional,
                        each_method_name: attr,
                        vec_type,
                    })
                }
                None => None,
            })
            .collect::<Vec<_>>();

        let builder_fields_def = fields.iter().map(|f| {
            let name = f.name;
            let ty = &f.ty;
            quote! {
                pub #name: std::option::Option<#ty>,
            }
        });

        let builder_fields_init = fields.iter().map(|f| {
            let name = f.name;
            if f.vec_type.is_some() {
                quote! {
                    #name: std::option::Option::Some(Vec::new()),
                }
            } else {
                quote! {
                    #name: std::option::Option::None,
                }
            }
        });

        let builder_methods = fields.iter().map(|f| {
            let name = f.name;
            let ty = &f.ty;
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        });

        let builder_checks = fields
            .iter()
            .filter(|f| !f.optional && f.vec_type.is_none())
            .map(|f| {
                let name = f.name;
                let name_string = name.to_string();
                quote! {
                    if self.#name.is_none() {
                        return std::result::Result::Err(std::format!("Field {} is not set", #name_string).into());
                    }
                }
            });

        let struct_fields = fields.iter().map(|f| {
            let name = f.name;
            if f.optional {
                quote! {
                    #name: self.#name.take(),
                }
            } else {
                quote! {
                    #name: self.#name.take().unwrap(),
                }
            }
        });

        let builder_extra_functions = fields.iter().filter_map(|f| {
            match (&f.each_method_name, &f.vec_type) {
                (Ok(Some(name)), Some(ty)) => {
                    let name = Ident::new(name, Span::call_site());
                    if fields.iter().any(|f| f.name == &name) {
                        return None;
                    }
                    let field_name = f.name;
                    let method = quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            if self.#field_name.is_none() {
                                self.#field_name= std::option::Option::Some(std::vec::Vec::new());
                            }
                            self.#field_name.as_mut().unwrap().push(#name);
                            self
                        }
                    };

                    Some(method)
                }
                _ => None,
            }
        });

        let compile_errors = fields.iter().filter_map(|f| match &f.each_method_name {
            Err(e) => Some(e.to_compile_error()),
            _ => None,
        });

        // ///////////////////////////////////////////////////////////////////////////////////////////////////////

        let tokens = quote! {

        #(#compile_errors)*

        pub struct #builder_name {
            #(#builder_fields_def)*

        }

        impl #builder_name {
            #(#builder_methods)*

            #(#builder_extra_functions)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #(#builder_checks)*

                std::result::Result::Ok(#name {
                    #(#struct_fields)*
                })

            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_fields_init)*

                }
            }

        }
        };

        tokens.into()
    } else {
        panic!("Only structs are supported");
    }
}

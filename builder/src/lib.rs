use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, spanned::Spanned};

struct Field<'f> {
    name: &'f syn::Ident,
    ty: &'f syn::Type,
    is_optional: bool,
    is_repeated: std::option::Option<syn::Ident>,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse input token into an AST
    let input = parse_macro_input!(input as syn::DeriveInput);

    // Get struct name and Builder name
    let struct_name = &input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);
    let struct_fields = get_fields(&input);

    // If there is an error in the parsing of fields return the compiler error
    if let Err(e) = struct_fields {
        return TokenStream::from(e.to_compile_error());
    }
    // The error was handled so unwrapping struct_fields is safe
    let struct_fields = struct_fields.unwrap();

    // Generate Builder struct definition
    let builder_fields: Vec<_> = builder_fields(&struct_fields);

    // Generate Builder functions
    let builder_funcs: Vec<_> = builder_setter_funcs(&struct_fields);
    let builder_build = build_function(struct_name, &struct_fields);

    // Generate builder function of struct
    let builder_body: Vec<_> = struct_fields
        .iter()
        .map(|f| {
            let name = &f.name;
            if f.is_repeated.is_some() {
                quote! {
                    #name : vec![]
                }
            } else {
                quote! {
                    #name : None
                }
            }
        })
        .collect();

    // Put all generated code together
    let expended = quote! {
        pub struct #builder_name {
            #(#builder_fields,)*
        }

        impl #builder_name {
            #(#builder_funcs)*
            #builder_build
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_body,)*
                }
            }
        }
    };

    TokenStream::from(expended)
}

// Create a list of fields with their caracteristics (optional, repreatable...)
fn get_fields<'f>(input: &'f syn::DeriveInput) -> std::result::Result<Vec<Field<'f>>, syn::Error> {
    match input.data {
        syn::Data::Struct(ref d) => match d.fields {
            syn::Fields::Named(ref fields) => {
                return fields
                    .named
                    .iter()
                    .map(|f| {
                        let is_optional = get_is_optional(&f.ty);
                        let identifier = f.ident.as_ref().unwrap();
                        // Errors must be passed on to the derive function
                        let repetition = get_is_repeated(&f)?;
                        let ftype = if is_optional { &get_type(&f.ty) } else { &f.ty };
                        Ok(Field {
                            name: identifier,
                            ty: ftype,
                            is_optional: is_optional,
                            is_repeated: repetition,
                        })
                    })
                    .collect();
            }
            syn::Fields::Unnamed(_) | syn::Fields::Unit => unimplemented!(),
        },
        syn::Data::Enum(_) | syn::Data::Union(_) => unimplemented!(),
    };
}

// Check if the field can be repeated
// Return the field identifer of the function to repeat a field if it is repeated, else return None
fn get_is_repeated(
    f: &syn::Field,
) -> std::result::Result<std::option::Option<syn::Ident>, syn::Error> {
    // If the syn::Field has no attribute it is not repeated
    if f.attrs.is_empty() {
        return Ok(None);
    }
    // If there are attributs check repetability
    for attr in f.attrs.iter() {
        // Check for a builder attibute meta that is a List
        let m = attr.parse_meta()?;
        if let syn::Meta::List(m) = m {
            if m.path.get_ident().unwrap() == "builder" {
                // Check the nested meta
                for n in m.nested.iter() {
                    if let syn::NestedMeta::Meta(n) = n {
                        // Check the named value "each"
                        if let syn::Meta::NameValue(v) = n {
                            if v.path.get_ident().unwrap() == "each" {
                                if let syn::Lit::Str(s) = &v.lit {
                                    return Ok(Some(format_ident!("{}", s.value())));
                                } else {
                                    return Err(syn::Error::new(
                                        v.eq_token.spans[0],
                                        "Wrong type for naming function",
                                    ));
                                }
                            } else {
                                return Err(syn::Error::new(
                                    attr.path.span().join(attr.tokens.span()).unwrap(),
                                    "expected `builder(each = \"...\")`",
                                ));
                            }
                        } else {
                            return Err(syn::Error::new(
                                attr.path.span().join(attr.tokens.span()).unwrap(),
                                "expected `builder(each = \"...\")`",
                            ));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

// Return true only if the input type is an std::option::Optional
fn get_is_optional(t: &syn::Type) -> bool {
    match t {
        syn::Type::Path(t) => match t.path.segments.first() {
            Some(t) => t.ident == "Option",
            _ => false,
        },
        _ => false,
    }
}

// Return the type wrapped inside a container type (std::option::Option, Vec, Array...)
fn get_type(ty: &syn::Type) -> &syn::Type {
    if let syn::Type::Path(ty) = ty {
        if let Some(ty) = ty.path.segments.first() {
            if let syn::PathArguments::AngleBracketed(ty) = &ty.arguments {
                if let Some(ty) = ty.args.first() {
                    if let syn::GenericArgument::Type(ty) = ty {
                        return ty;
                    }
                }
            }
        }
    }
    unimplemented!();
}

fn builder_setter_funcs(fields: &Vec<Field>) -> Vec<proc_macro2::TokenStream> {
    fields
        .iter()
        .map(|f| {
            let name = &f.name;
            let type_name = &f.ty;
            if let Some(rep_name) = &f.is_repeated.as_ref() {
                let global = if rep_name != name {
                    quote! {
                        fn #name (&mut self, #name : #type_name) -> &mut Self {
                            self.#name = #name;
                            self
                        }
                    }
                } else {
                    proc_macro2::TokenStream::new()
                };
                let rep_type = get_type(type_name);
                quote! {
                    #global
                    fn #rep_name (&mut self, #rep_name : #rep_type) -> &mut Self {
                        self.#name.push(#rep_name);
                        self
                    }
                }
            } else {
                quote! {
                    fn #name (&mut self, #name : #type_name) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
        })
        .collect()
}

// Create the body of the builder struct definition
fn builder_fields(fields: &Vec<Field>) -> Vec<proc_macro2::TokenStream> {
    fields
        .iter()
        .map(|f| {
            let name = f.name;
            let ty = f.ty;
            if f.is_repeated.is_some() {
                quote! {
                    #name : #ty
                }
            } else {
                quote! {
                    #name : std::option::Option<#ty>
                }
            }
        })
        .collect()
}

// Create the build() function that consums the builder to create the desired structure wrapped in result
fn build_function(struct_name: &syn::Ident, fields: &Vec<Field>) -> proc_macro2::TokenStream {
    // For each field generate the assignment an raise an error if no value was given to a mendatory field
    let fields_assignment: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.name;
            let err_msg = format!("Field '{}' is missing", name);
            if !f.is_optional {
                if f.is_repeated.is_some() {
                    quote! {
                        #name : self.#name.clone()
                    }
                } else {
                    quote! {
                        #name : self.#name.take().ok_or(#err_msg)?
                    }
                }
            } else {
                quote! {
                    #name : self.#name.take()
                }
            }
        })
        .collect();

    quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
            std::result::Result::Ok(#struct_name {
                #(#fields_assignment),*
            })
        }
    }
}

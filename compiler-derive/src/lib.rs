use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::ToTokens;
use syn::{Data, DeriveInput, Fields, Ident, LitStr, parse_macro_input};

#[proc_macro_derive(BuiltinStruct, attributes(module_id, struct_name, generic, array))]
pub fn builtin_struct(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    let mut module_id = String::new();
    let mut struct_name = String::new();
    let mut generic_type = None;

    for attribute in &input.attrs {
        if attribute.path().is_ident("module_id") {
            let args: LitStr = attribute.parse_args().unwrap();

            module_id = args.value();
        } else if attribute.path().is_ident("struct_name") {
            let args: LitStr = attribute.parse_args().unwrap();

            struct_name = args.value();
        } else if attribute.path().is_ident("generic") {
            let type_: Ident = attribute.parse_args().unwrap();
            generic_type = Some(type_.to_string());
        }
    }

    let Data::Struct(struct_) = input.data else {
        panic!("This macro can only be applied to structs.");
    };

    let Fields::Named(fields) = struct_.fields else {
        panic!("This macro can only be applied to structs with named fields");
    };

    let mut fields_gen = vec![];
    let mut declarations = vec![];

    for (i, field) in fields.named.into_iter().enumerate() {
        let is_array = field
            .attrs
            .first()
            .map(|x| x.path().is_ident("array"))
            .unwrap_or(false);
        let field_name = field.ident.unwrap().to_string();
        let field_type_lng = to_lng_type(&field.ty, generic_type.as_deref());

        let field_name_lit = Literal::string(&field_name);

        let field_type_lng = if is_array {
            let field_type_name = quote::format_ident!("field_type__{}", i);
            declarations.push(quote::quote! {
                let #field_type_name = type_store.add(#field_type_lng);
            });

            quote::quote! {
                type_store.add(crate::types::Type::new_generic(
                    crate::types::TypeKind::Array(#field_type_name),
                    crate::types::generics::TypeArguments::new_empty(),
                ))
            }
        } else {
            quote::quote! { type_store.add(#field_type_lng) }
        };

        fields_gen.push(quote::quote! { crate::types::structs::StructField {
            struct_id,
            name: crate::identifier::Identifier::parse(#field_name_lit),
            type_: #field_type_lng,
            static_: false
        }});
    }

    let generics = if let Some(generic_type) = generic_type {
        let generic_type_lit = Literal::string(&generic_type);

        quote::quote! {
            let type_argument = crate::types::generics::TypeArgument::new(crate::identifier::Identifier::parse(#generic_type_lit));
            let type_arguments = crate::types::generics::TypeArguments::new(vec![type_argument.clone()]);
            let generic_argument_type = crate::types::Type::new_generic(
                crate::types::TypeKind::Generic(type_argument),
                type_arguments.clone(),
            );
        }
    } else {
        quote::quote! {
            let type_arguments:crate::types::generics::TypeArguments = crate::types::generics::TypeArguments::new_empty();
        }
    };

    let module_id_lit = Literal::string(&module_id);
    let struct_name_lit = Literal::string(&struct_name);

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let name = input.ident;
    quote::quote! {
        impl #impl_generics #name #type_generics #where_clause {
            pub(crate) fn struct_id() -> crate::types::structs::StructId {
                let module_id = crate::types::modules::ModuleId::parse(#module_id_lit);
                let struct_name = crate::identifier::Identifier::parse(#struct_name_lit);
                crate::types::structs::StructId::InModule(module_id, struct_name)
            }
        }

        pub fn describe_structure(type_store: &mut dyn crate::types::store::TypeStore) -> crate::types::structs::Struct {
            #generics
            #(#declarations);*

            let module_id = crate::types::modules::ModuleId::parse(#module_id_lit);
            let struct_name = crate::identifier::Identifier::parse(#struct_name_lit);
            let struct_id = crate::types::structs::StructId::InModule(module_id, struct_name);

            crate::types::structs::Struct {
                id: struct_id,
                type_id: type_store.add(crate::types::Type::new_generic(
                    crate::types::TypeKind::Struct(
                        crate::types::structs::InstantiatedStructId::new(
                            struct_id,
                            crate::types::generics::TypeArguments::new_empty()
                        )
                    ),
                    type_arguments.clone(),
                )),
                instance_type: type_store.add(crate::types::Type::new_generic(
                    crate::types::TypeKind::Object(
                        crate::types::structs::InstantiatedStructId::new(struct_id, crate::types::generics::TypeArguments::new_empty())
                    ),
                    type_arguments
                )),
                fields: vec![#(#fields_gen),*],
                impls: vec![],
                implemented_interfaces: std::collections::HashMap::new(),
            }
        }
    }
    .into()
}

fn to_lng_type(type_: &syn::Type, generic_type: Option<&str>) -> impl ToTokens {
    match type_ {
        syn::Type::Ptr(type_ptr) => {
            let inner_type = to_lng_type(&type_ptr.elem, generic_type);

            quote::quote! {crate::types::Type::new(crate::types::TypeKind::Pointer(std::boxed::Box::new(#inner_type)))}
        }
        syn::Type::Path(path) => {
            if let Some(generic_type_) = generic_type {
                if path.path.is_ident(&generic_type_) {
                    return quote::quote! {generic_argument_type};
                }
            }

            if path.path.is_ident("u64") {
                return quote::quote! {crate::types::Type::u64()};
            } else if path.path.is_ident("u8") {
                return quote::quote! {crate::types::Type::u8()};
            }

            quote::quote! {
                crate::types::Type::new(
                    crate::types::TypeKind::Object(
                        crate::types::structs::InstantiatedStructId::new(
                            #path::struct_id(),
                            crate::types::generics::TypeArguments::new_empty(),
                        )
                    )
                )
            }
        }
        syn::Type::BareFn(_fn_) => {
            // TODO this is a hack, replace this with a callable type perhaps, once the language
            // actually does support callables
            quote::quote! {crate::types::Type::u8()}
        }
        _ => todo!(),
    }
}

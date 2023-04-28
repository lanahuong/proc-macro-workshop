use proc_macro::{Literal, TokenStream, TokenTree};
use quote::{format_ident, quote};
use syn::{parse::Parse, parse::ParseStream, parse::Result, parse_macro_input, Expr, Token};

struct Sequence {
    i: String,
    start: u32,
    stop: u32,
    handle: TokenStream,
}

impl Parse for Sequence {
    fn parse(input: ParseStream) -> Result<Self> {
        let i = input.parse::<syn::Ident>()?;
        let i = i.to_string();
        input.parse::<Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?;
        let start = start.base10_parse::<u32>()?;
        input.parse::<Token![..]>()?;
        let stop = input.parse::<syn::LitInt>()?;
        let stop = stop.base10_parse::<u32>()?;
        let handle = input.parse::<Expr>()?;
        let handle = TokenStream::from(quote! {#handle});
        Ok(Sequence {
            i,
            start,
            stop,
            handle,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Sequence);

    let expanded = replace(input.i, 1, input.handle);

    TokenStream::from(expanded)
}

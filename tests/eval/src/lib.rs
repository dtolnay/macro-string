use macro_string::MacroString;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro]
pub fn eval(input: TokenStream) -> TokenStream {
    let MacroString(value) = parse_macro_input!(input);
    TokenStream::from(quote!(#value))
}

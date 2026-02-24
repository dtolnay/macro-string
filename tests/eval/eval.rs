use macro_string::MacroString;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro]
pub fn eval(input: TokenStream) -> TokenStream {
    let macro_string = parse_macro_input!(input as MacroString);
    TokenStream::from(match macro_string.eval() {
        Ok(value) => quote!(#value),
        Err(err) => err.into_compile_error(),
    })
}

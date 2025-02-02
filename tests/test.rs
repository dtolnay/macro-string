use macro_string::MacroString;
use syn::parse_quote;

#[test]
fn test_concat_stringify() {
    let MacroString(value) = parse_quote! {
        concat!("x", stringify!(y z))
    };
    assert_eq!(value, "xy z");
}

#[test]
fn test_env() {
    let MacroString(value) = parse_quote! {
        env!("CARGO_PKG_NAME")
    };
    assert_eq!(value, "macro-string");
}

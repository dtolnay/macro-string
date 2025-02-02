use macro_string::MacroString;
use macro_string_eval::eval;
use syn::parse_quote;

#[test]
fn test_eval() {
    macro_rules! test {
        ($expr:expr) => {{
            const EVAL: &str = eval!($expr);
            const STD: &str = $expr;
            assert_eq!(EVAL, STD);
        }};
    }

    test!(concat!("ru", "st"));
}

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

#[test]
fn test_include_str() {
    let MacroString(value) = parse_quote! {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/example.str"))
    };
    assert_eq!(value, "success\n");
}

#[test]
fn test_include() {
    let MacroString(value) = parse_quote! {
        include!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/example.expr"))
    };
    assert_eq!(value, "123");
}

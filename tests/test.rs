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

    #[rustfmt::skip]
    test!(concat!(
        // Lit::Str
        "rust",
        // Lit::Char
        ' ',
        // Lit::Int
        10,
        -10,
        0x10,
        10u8,
        10f32,
        // Lit::Float
        10.0,
        10e1,
        10.0f32,
        // Lit::Bool
        false,
        true,
    ));
}

#[test]
fn test_concat_stringify() {
    let macro_string: MacroString = parse_quote! {
        concat!("x", stringify!(y z))
    };
    assert_eq!(macro_string.eval().unwrap(), "xy z");
}

#[test]
fn test_env() {
    let macro_string: MacroString = parse_quote! {
        env!("CARGO_PKG_NAME")
    };
    assert_eq!(macro_string.eval().unwrap(), "macro-string");
}

#[test]
fn test_include_str() {
    let macro_string: MacroString = parse_quote! {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/example.str"))
    };
    assert_eq!(macro_string.eval().unwrap(), "success\n");
}

#[test]
fn test_include() {
    let macro_string: MacroString = parse_quote! {
        include!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/example.expr"))
    };
    assert_eq!(macro_string.eval().unwrap(), "123");
}

#[test]
fn test_macro_lit() {
    macro_rules! do_test_macro_lit {
        ($str:expr) => {
            eval!(concat!("//", $str))
        };
    }

    let value = do_test_macro_lit!("...");
    assert_eq!(value, "//...");
}

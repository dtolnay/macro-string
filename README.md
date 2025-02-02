macro-string
============

This crate is a helper library for procedural macros to perform eager evaluation
of standard library string macros like `concat!` and `env!` in macro input.

For example, to implement a macro such as the following:

```rust
// Parses JSON at compile time and expands to a serde_json::Value.
let j = include_json!(concat!(env!("CARGO_MANIFEST_DIR"), "/manifest.json"));
```

the implementation of `include_json!` will need to parse and eagerly evaluate
the two macro calls within its input tokens.

```rust
use macro_string::MacroString;
use proc_macro::TokenStream;
use proc_macro2::Span;
use std::fs;
use syn::parse_macro_input;

#[proc_macro]
pub fn include_json(input: TokenStream) -> TokenStream {
    let MacroString(path) = parse_macro_input!(input);

    let content = match fs::read(&path) {
        Ok(content) => content,
        Err(err) => {
            return TokenStream::from(syn::Error::new(Span::call_site(), err).to_compile_error());
        }
    };

    let json: serde_json::Value = match serde_json::from_slice(&content) {
        Ok(json) => json,
        Err(err) => {
            return TokenStream::from(syn::Error::new(Span::call_site(), err).to_compile_error());
        }
    };

    /*TODO: print serde_json::Value to TokenStream*/
}
```

<br>

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

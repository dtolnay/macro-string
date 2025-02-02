use proc_macro2::TokenStream;
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket, Paren};
use syn::{LitStr, Token};

mod kw {
    syn::custom_keyword!(concat);
    syn::custom_keyword!(env);
    syn::custom_keyword!(include);
    syn::custom_keyword!(include_str);
    syn::custom_keyword!(stringify);
}

enum Expr {
    Lit(LitStr),
    Concat(Concat),
    Env(Env),
    Include(Include),
    IncludeStr(IncludeStr),
    Stringify(Stringify),
}

struct Concat {
    name: kw::concat,
    bang_token: Token![!],
    delimiter: MacroDelimiter,
    args: Punctuated<Expr, Token![,]>,
}

struct Env {
    name: kw::env,
    bang_token: Token![!],
    delimiter: MacroDelimiter,
    arg: Box<Expr>,
    trailing_comma: Option<Token![,]>,
}

struct Include {
    name: kw::include,
    bang_token: Token![!],
    delimiter: MacroDelimiter,
    arg: Box<Expr>,
    trailing_comma: Option<Token![,]>,
}

struct IncludeStr {
    name: kw::include_str,
    bang_token: Token![!],
    delimiter: MacroDelimiter,
    arg: Box<Expr>,
    trailing_comma: Option<Token![,]>,
}

struct Stringify {
    name: kw::stringify,
    bang_token: Token![!],
    delimiter: MacroDelimiter,
    tokens: TokenStream,
}

enum MacroDelimiter {
    Paren(Paren),
    Brace(Brace),
    Bracket(Bracket),
}

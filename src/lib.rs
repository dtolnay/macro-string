use proc_macro2::TokenStream;
use syn::parse::{Error, Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket, Paren};
use syn::{braced, bracketed, parenthesized, LitStr, Token};

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

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            input.parse().map(Expr::Lit)
        } else if lookahead.peek(kw::concat) {
            input.parse().map(Expr::Concat)
        } else if lookahead.peek(kw::env) {
            input.parse().map(Expr::Env)
        } else if lookahead.peek(kw::include) {
            input.parse().map(Expr::Include)
        } else if lookahead.peek(kw::include_str) {
            input.parse().map(Expr::IncludeStr)
        } else if lookahead.peek(kw::stringify) {
            input.parse().map(Expr::Stringify)
        } else {
            Err(lookahead.error())
        }
    }
}

macro_rules! macro_delimiter {
    ($var:ident in $input:ident) => {{
        let lookahead = $input.lookahead1();
        if $input.peek(Paren) {
            MacroDelimiter::Paren(parenthesized!($var in $input))
        } else if $input.peek(Brace) {
            MacroDelimiter::Brace(braced!($var in $input))
        } else if $input.peek(Bracket) {
            MacroDelimiter::Bracket(bracketed!($var in $input))
        } else {
            return Err(lookahead.error());
        }
    }};
}

impl Parse for Concat {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Concat {
            name: input.parse()?,
            bang_token: input.parse()?,
            delimiter: macro_delimiter!(content in input),
            args: content.call(Punctuated::parse_terminated)?,
        })
    }
}

impl Parse for Env {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Env {
            name: input.parse()?,
            bang_token: input.parse()?,
            delimiter: macro_delimiter!(content in input),
            arg: content.parse()?,
            trailing_comma: content.parse()?,
        })
    }
}

impl Parse for Include {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Include {
            name: input.parse()?,
            bang_token: input.parse()?,
            delimiter: macro_delimiter!(content in input),
            arg: content.parse()?,
            trailing_comma: content.parse()?,
        })
    }
}

impl Parse for IncludeStr {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(IncludeStr {
            name: input.parse()?,
            bang_token: input.parse()?,
            delimiter: macro_delimiter!(content in input),
            arg: content.parse()?,
            trailing_comma: content.parse()?,
        })
    }
}

impl Parse for Stringify {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Stringify {
            name: input.parse()?,
            bang_token: input.parse()?,
            delimiter: macro_delimiter!(content in input),
            tokens: content.parse()?,
        })
    }
}

use proc_macro2::TokenStream;
use quote::ToTokens;
use std::env;
use std::fs;
use syn::parse::{Error, Parse, ParseBuffer, ParseStream, Result};
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

impl Expr {
    fn eval(&self) -> Result<String> {
        match self {
            Expr::Lit(lit) => Ok(lit.value()),
            Expr::Concat(expr) => {
                let mut concat = String::new();
                for arg in &expr.args {
                    concat += &arg.eval()?;
                }
                Ok(concat)
            }
            Expr::Env(expr) => {
                let key = expr.arg.eval()?;
                match env::var(&key) {
                    Ok(value) => Ok(value),
                    Err(err) => Err(Error::new_spanned(expr, err)),
                }
            }
            Expr::Include(expr) => {
                let path = expr.arg.eval()?;
                match fs::read_to_string(&path) {
                    Ok(content) => {
                        let inner: Expr = syn::parse_str(&content)?;
                        inner.eval()
                    }
                    Err(err) => Err(Error::new_spanned(expr, err)),
                }
            }
            Expr::IncludeStr(expr) => {
                let path = expr.arg.eval()?;
                match fs::read_to_string(&path) {
                    Ok(content) => Ok(content),
                    Err(err) => Err(Error::new_spanned(expr, err)),
                }
            }
            Expr::Stringify(expr) => Ok(expr.tokens.to_string()),
        }
    }
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
            let lit: LitStr = input.parse()?;
            if !lit.suffix().is_empty() {
                return Err(Error::new(
                    lit.span(),
                    "unexpected suffix on string literal",
                ));
            }
            Ok(Expr::Lit(lit))
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

impl ToTokens for Expr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Expr::Lit(expr) => expr.to_tokens(tokens),
            Expr::Concat(expr) => expr.to_tokens(tokens),
            Expr::Env(expr) => expr.to_tokens(tokens),
            Expr::Include(expr) => expr.to_tokens(tokens),
            Expr::IncludeStr(expr) => expr.to_tokens(tokens),
            Expr::Stringify(expr) => expr.to_tokens(tokens),
        }
    }
}

macro_rules! macro_delimiter {
    ($var:ident in $input:ident) => {{
        let (delim, content) = $input.call(macro_delimiter)?;
        $var = content;
        delim
    }};
}

fn macro_delimiter<'a>(input: ParseStream<'a>) -> Result<(MacroDelimiter, ParseBuffer<'a>)> {
    let content;
    let lookahead = input.lookahead1();
    let delim = if input.peek(Paren) {
        MacroDelimiter::Paren(parenthesized!(content in input))
    } else if input.peek(Brace) {
        MacroDelimiter::Brace(braced!(content in input))
    } else if input.peek(Bracket) {
        MacroDelimiter::Bracket(bracketed!(content in input))
    } else {
        return Err(lookahead.error());
    };
    Ok((delim, content))
}

impl MacroDelimiter {
    fn surround<F>(&self, tokens: &mut TokenStream, f: F)
    where
        F: FnOnce(&mut TokenStream),
    {
        match self {
            MacroDelimiter::Paren(delimiter) => delimiter.surround(tokens, f),
            MacroDelimiter::Brace(delimiter) => delimiter.surround(tokens, f),
            MacroDelimiter::Bracket(delimiter) => delimiter.surround(tokens, f),
        }
    }
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

impl ToTokens for Concat {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.bang_token.to_tokens(tokens);
        self.delimiter
            .surround(tokens, |tokens| self.args.to_tokens(tokens));
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

impl ToTokens for Env {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.bang_token.to_tokens(tokens);
        self.delimiter.surround(tokens, |tokens| {
            self.arg.to_tokens(tokens);
            self.trailing_comma.to_tokens(tokens);
        });
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

impl ToTokens for Include {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.bang_token.to_tokens(tokens);
        self.delimiter.surround(tokens, |tokens| {
            self.arg.to_tokens(tokens);
            self.trailing_comma.to_tokens(tokens);
        });
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

impl ToTokens for IncludeStr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.bang_token.to_tokens(tokens);
        self.delimiter.surround(tokens, |tokens| {
            self.arg.to_tokens(tokens);
            self.trailing_comma.to_tokens(tokens);
        });
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

impl ToTokens for Stringify {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.bang_token.to_tokens(tokens);
        self.delimiter
            .surround(tokens, |tokens| self.tokens.to_tokens(tokens));
    }
}

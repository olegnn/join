//!
//! `Handler` implementation.
//!

use std::convert::TryFrom;
use syn::parse::ParseStream;
use syn::Expr;
use syn::Token;

///
/// map => expr,
/// then => expr,
/// and_then => expr
///
pub enum Handler {
    Map(Expr),
    Then(Expr),
    AndThen(Expr),
}

mod keywords {
    syn::custom_keyword!(map);
    syn::custom_keyword!(then);
    syn::custom_keyword!(and_then);
}

impl Handler {
    ///
    /// Returns `true` if handler is `Map`.
    ///
    pub fn is_map(&self) -> bool {
        matches!(self, Self::Map(_))
    }

    ///
    /// Returns `true` if handler is `Then`.
    ///
    pub fn is_then(&self) -> bool {
        matches!(self, Self::Then(_))
    }

    ///
    /// Returns `true` if handler is `AndThen`.
    ///
    pub fn is_and_then(&self) -> bool {
        matches!(self, Self::AndThen(_))
    }

    ///
    /// Returns `true` if next value in input `ParseStream` is the definition of `map` `Handler`.
    ///
    fn peek_map_handler(input: ParseStream<'_>) -> bool {
        input.peek(keywords::map) && input.peek2(Token![=>])
    }

    ///
    /// Returns `true` if next value in input `ParseStream` is the definition of `then` `Handler`.
    ///
    fn peek_then_handler(input: ParseStream<'_>) -> bool {
        input.peek(keywords::then) && input.peek2(Token![=>])
    }

    ///
    /// Returns `true` if next value in input `ParseStream` is the definition of `and_then` `Handler`.
    ///
    fn peek_and_then_handler(input: ParseStream<'_>) -> bool {
        input.peek(keywords::and_then) && input.peek2(Token![=>])
    }

    ///
    /// Returns `true` if next value in input `ParseStream` is the definition of `Handler`.
    ///  
    pub fn peek_handler(input: ParseStream<'_>) -> bool {
        Self::peek_then_handler(input)
            || Self::peek_and_then_handler(input)
            || Self::peek_map_handler(input)
    }

    ///
    /// Extracts inner expr.
    ///
    pub fn extract_expr(&self) -> &Expr {
        match self {
            Self::Map(expr) | Self::Then(expr) | Self::AndThen(expr) => expr,
        }
    }
}

impl<'a> TryFrom<ParseStream<'a>> for Handler {
    type Error = syn::Error;
    ///
    /// Attempts to parse input as a handler.
    ///
    fn try_from(input: ParseStream<'a>) -> syn::Result<Self> {
        let res = if Self::peek_then_handler(input) {
            input.parse::<keywords::then>()?;
            input.parse::<Token![=>]>()?;
            Self::Then(input.parse()?)
        } else if Self::peek_and_then_handler(input) {
            input.parse::<keywords::and_then>()?;
            input.parse::<Token![=>]>()?;
            Self::AndThen(input.parse()?)
        } else if Self::peek_map_handler(input) {
            input.parse::<keywords::map>()?;
            input.parse::<Token![=>]>()?;
            Self::Map(input.parse()?)
        } else {
            return Err(syn::Error::new(input.span(), "Failed to parse `Handler`"));
        };
        input.parse::<Option<Token![,]>>()?;

        Ok(res)
    }
}

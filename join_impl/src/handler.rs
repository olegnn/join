//!
//! `Handler` defines handler function of 3 possible types: `map`, `and_then` and `then`.
//! `map` and `and_then` will be evaluted in case of all successful results and `then` will be evaluated in any case,
//! which allows user to define its own handlers for every error.
//!

use syn::parse::ParseStream;
use syn::Token;
use syn::{self, Expr};

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
    /// Checks if input `ParseStream` next value is a `Handler` and then if it's true, attempts to parse it, otherwise returns `None`.
    /// Will return Err if `ParseStream` must contain `Handler` but it can't be parsed.
    ///
    pub fn new(input: ParseStream<'_>) -> syn::Result<Option<Self>> {
        let result = if Self::peek_then_handler(input) {
            input.parse::<keywords::then>()?;
            input.parse::<Token![=>]>()?;
            Some(Self::Then(input.parse()?))
        } else if Self::peek_and_then_handler(input) {
            input.parse::<keywords::and_then>()?;
            input.parse::<Token![=>]>()?;
            Some(Self::AndThen(input.parse()?))
        } else if Self::peek_map_handler(input) {
            input.parse::<keywords::map>()?;
            input.parse::<Token![=>]>()?;
            Some(Self::Map(input.parse()?))
        } else {
            None
        };

        if result.is_some() {
            input.parse::<Option<Token![,]>>()?;
        }

        Ok(result)
    }

    ///
    /// Returns true if handler is `Map`.
    ///
    pub fn is_map(&self) -> bool {
        match self {
            Self::Map(_) => true,
            _ => false,
        }
    }

    ///
    /// Returns true if handler is `Then`.
    ///
    pub fn is_then(&self) -> bool {
        match self {
            Self::Then(_) => true,
            _ => false,
        }
    }

    ///
    /// Returns true if handler is `AndThen`.
    ///
    pub fn is_and_then(&self) -> bool {
        match self {
            Self::AndThen(_) => true,
            _ => false,
        }
    }

    ///
    /// Returns true if next value in input `ParseStream` is the definition of `map` `Handler`.
    ///
    fn peek_map_handler(input: ParseStream<'_>) -> bool {
        input.peek(keywords::map) && input.peek2(Token![=>])
    }

    ///
    /// Returns true if next value in input `ParseStream` is the definition of `then` `Handler`.
    ///
    fn peek_then_handler(input: ParseStream<'_>) -> bool {
        input.peek(keywords::then) && input.peek2(Token![=>])
    }

    ///
    /// Returns true if next value in input `ParseStream` is the definition of `and_then` `Handler`.
    ///
    fn peek_and_then_handler(input: ParseStream<'_>) -> bool {
        input.peek(keywords::and_then) && input.peek2(Token![=>])
    }

    ///
    /// Returns true if next value in input `ParseStream` is the definition of `Handler`.
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

//!
//! Parser which takes expression chains and puts them into `branches` field,
//! and handler (one of `map`, `and_then`, `then`) and puts it into `handler` field.
//! Handler can be either defined once or not defined.
//!

use syn::parenthesized;
use syn::parse::{Parse, ParseStream};
use super::super::expr_chain::ExprChainWithDefault;
use super::super::handler::Handler;
use super::Join;

mod keywords {
    syn::custom_keyword!(futures_crate_path);
}

impl Parse for Join {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut join = Join {
            branches: Vec::new(),
            handler: None,
            futures_crate_path: None,
        };

        if input.peek(keywords::futures_crate_path) {
            input.parse::<keywords::futures_crate_path>()?;
            let content;
            parenthesized!(content in input);
            join.futures_crate_path = Some(content.parse()?);
        }

        while !input.is_empty() {
            if Handler::is_handler(&input) {
                if join.handler.is_some() {
                    return Err(input.error("Multiple `handler` cases found, only one allowed. Please, specify one of `map`, `and_then`, `then`."));
                }
                let handler = Handler::new(input)?.expect(
                    "join: Handler `is_handler` check failed. This's a bug, please report it.",
                );
                join.handler = Some(handler);
            } else {
                let expr_chain = ExprChainWithDefault::new(input, Box::new(Handler::is_handler))?;
                if let Some(expr_chain) = expr_chain {
                    join.branches.push(Box::new(expr_chain));
                }
            };
        }

        if join.branches.is_empty() {
            Err(input.error("join must contain at least 1 branch."))
        } else {
            Ok(join)
        }
    }
}


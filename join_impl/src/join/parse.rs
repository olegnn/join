//!
//! Parser which takes expression chains and puts them into `branches` field,
//! and handler (one of `map`, `and_then`, `then`) and puts it into `handler` field.
//! Handler can be either defined once or not defined.
//!
//!
use super::super::expr_chain::group::GroupDeterminer;
use super::super::expr_chain::{ActionExprChain, ActionExprChainGenerator};
use super::super::handler::Handler;
use super::JoinDefault;
use syn::parenthesized;
use syn::parse::{Parse, ParseStream};
use syn::Token;

#[cfg(feature = "full")]
use std::sync::Arc;

mod keywords {
    syn::custom_keyword!(futures_crate_path);
    syn::custom_keyword!(n);
}

///
/// Default `GroupDeterminer`'s definitions.
///
pub const DEFAULT_GROUP_DETERMINERS: &[GroupDeterminer] = &crate::define_instant_and_deferred_determiners! {
    UNWRAP => Token![<], Token![<], Token![<] => 3,
    Collect => Token![=], Token![>], syn::token::Bracket => 3,
    Map => Token![|], Token![>] => 2,
    Then => Token![->] => 2,
    AndThen => Token![=>] => 2,
    Or => Token![<], Token![|] => 2,
    OrElse => Token![<=] => 2,
    Dot => Token![>], Token![.] => 2,
    Dot => Token![..] => 2,
    MapErr => Token![!], Token![>] => 2,
    Chain => Token![>], Token![@], Token![>] => 3,
    Inspect => Token![?], Token![?] => 2,
    Filter => Token![?], Token![>] => 2,
    FindMap => Token![?], Token![|], Token![>], Token![@] => 4,
    FilterMap => Token![?], Token![|], Token![>] => 3,
    Enumerate => Token![|], keywords::n, Token![>] => 3,
    Partition => Token![?], Token![&], Token![!], Token![>] => 4,
    Flatten => Token![^], Token![^], Token![>] => 3,
    Fold => Token![^], Token![@] => 2,
    TryFold => Token![?], Token![^], Token![@] => 3,
    Find => Token![?], Token![@] => 2,
    Zip => Token![>], Token![^], Token![>] => 3,
    Unzip => Token![<], Token![-], Token![>] => 3
};

///
/// `GroupDeterminer` used to determine deferred group.
///
pub const DEFERRED_DETERMINER: &GroupDeterminer = &crate::define_determiner_with_no_group! {
    Token![~] => 1
};

///
/// `GroupDeterminer` used to determine wrapper group.
///
pub const WRAP_DETERMINER: &GroupDeterminer = &crate::define_determiner_with_no_group! {
    Token![>], Token![>], Token![>] => 3
};

#[cfg(feature = "static")]
::lazy_static::lazy_static! {
    ///
    /// Static `GroupDeterminer`s definition which will be used if `static` feature is enabled.
    ///
    pub static ref DEFAULT_GROUP_DETERMINERS_STATIC: Arc<&'static [GroupDeterminer]> = Arc::new(DEFAULT_GROUP_DETERMINERS);
}

impl Parse for JoinDefault {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut join = JoinDefault {
            branches: Vec::new(),
            handler: None,
            futures_crate_path: None,
        };

        #[cfg(not(feature = "static"))]
        let action_expr_chain_generator = ActionExprChainGenerator::new(
            DEFAULT_GROUP_DETERMINERS,
            DEFERRED_DETERMINER,
            WRAP_DETERMINER,
        );

        #[cfg(feature = "static")]
        let action_expr_chain_generator = ActionExprChainGenerator::new(
            DEFAULT_GROUP_DETERMINERS_STATIC.clone(),
            DEFERRED_DETERMINER,
            WRAP_DETERMINER,
        );

        if input.peek(keywords::futures_crate_path) {
            input.parse::<keywords::futures_crate_path>()?;
            let content;
            parenthesized!(content in input);
            join.futures_crate_path = Some(content.parse()?);
        }

        while !input.is_empty() {
            if Handler::peek_handler(&input) {
                if join.handler.is_some() {
                    return Err(input.error("Multiple `handler` cases found, only one allowed. Please, specify one of `map`, `and_then`, `then`."));
                }
                let handler = Handler::new(input)?.expect(
                    "join: Handler `peek_handler` check failed. This's a bug, please report it.",
                );
                join.handler = Some(handler);
            } else {
                let expr_chain = ActionExprChain::new(input, &action_expr_chain_generator)?;
                if let Some(expr_chain) = expr_chain {
                    join.branches.push(expr_chain);
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

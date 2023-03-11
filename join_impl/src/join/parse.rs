//!
//! Parser which takes expression chains and puts them into `branches` field,
//! and handler (one of `map`, `and_then`, `then`) and puts it into `handler` field.
//! Handler can be either defined once or not defined.
//!
//!
use super::JoinInputDefault;
use crate::{
    action_expr_chain::ActionExprChainBuilder,
    chain::{group::GroupDeterminer, parse_chain::ParseChain},
    handler::Handler,
};
use alloc::vec::Vec;
use core::convert::TryFrom;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    LitBool, Token,
};

mod keywords {
    syn::custom_keyword!(futures_crate_path);
    syn::custom_keyword!(transpose_results);
    syn::custom_keyword!(lazy_branches);
    syn::custom_keyword!(custom_joiner);
    syn::custom_keyword!(n);
}

///
/// Default `GroupDeterminer`'s definitions.
///
pub const DEFAULT_GROUP_DETERMINERS: &[GroupDeterminer] = &crate::define_group_determiners! {
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
pub const WRAPPER_DETERMINER: &GroupDeterminer = &crate::define_determiner_with_no_group! {
    Token![>], Token![>], Token![>] => 3
};

impl Parse for JoinInputDefault {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut join = Self {
            branches: Vec::new(),
            handler: None,
            futures_crate_path: None,
            custom_joiner: None,
            transpose_results: None,
            lazy_branches: None,
        };

        let action_expr_chain_builder = ActionExprChainBuilder::new(
            DEFAULT_GROUP_DETERMINERS,
            DEFERRED_DETERMINER,
            WRAPPER_DETERMINER,
        );

        for _ in 0..4 {
            if input.peek(keywords::futures_crate_path) {
                input.parse::<keywords::futures_crate_path>()?;
                let content;
                parenthesized!(content in input);
                if join.futures_crate_path.is_some() {
                    return Err(input.error("futures_crate_path specified twice"));
                }
                join.futures_crate_path = Some(content.parse()?);
            }

            if input.peek(keywords::custom_joiner) {
                input.parse::<keywords::custom_joiner>()?;
                let content;
                parenthesized!(content in input);
                if join.custom_joiner.is_some() {
                    return Err(input.error("custom_joiner specified twice"));
                }
                join.custom_joiner = Some(content.parse()?);
            }

            if input.peek(keywords::transpose_results) {
                input.parse::<keywords::transpose_results>()?;
                let content;
                parenthesized!(content in input);
                if join.transpose_results.is_some() {
                    return Err(input.error("transpose_results specified twice"));
                }
                join.transpose_results = Some(content.parse::<LitBool>()?.value);
            }

            if input.peek(keywords::lazy_branches) {
                input.parse::<keywords::lazy_branches>()?;
                let content;
                parenthesized!(content in input);
                if join.lazy_branches.is_some() {
                    return Err(input.error("lazy_branches specified twice"));
                }
                join.lazy_branches = Some(content.parse::<LitBool>()?.value);
            }
        }

        while !input.is_empty() {
            if Handler::peek_handler(input) {
                if join.handler.is_some() {
                    return Err(input.error("Multiple `handler` cases found, only one allowed. Please, specify one of `map`, `and_then`, `then`."));
                }
                join.handler = Some(Handler::try_from(input)?);
            } else {
                let expr_chain = action_expr_chain_builder.build_from_parse_stream(input)?;
                join.branches.push(expr_chain);
            };
        }

        if join.branches.is_empty() {
            Err(input.error("join must contain at least 1 branch."))
        } else {
            Ok(join)
        }
    }
}

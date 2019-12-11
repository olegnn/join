//!
//! Defines `Join` trait and `Join` type to generate output of the `join!` macro based on input and given config.
//!

pub mod config;
pub mod join_output;
pub mod name_constructors;
pub mod parse;

pub use config::Config;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::Path;

use super::expr_chain::{ActionExprChain, Chain};
use super::handler::Handler;
use join_output::Join;
use syn::parse_quote;

///
/// Result of parsing `join!` macro input in trait form.
///
pub trait JoinInput {
    ///
    /// Object with implementation of `Chain` trait used to generate macro output.
    ///
    type Chain: Chain;

    ///
    /// Optional `join!` macro handler.
    ///
    type Handler;

    ///
    /// Returns custom futures_crate_path taken from macro input if exists.
    ///
    fn get_futures_crate_path(&self) -> Option<&Path>;

    ///
    /// Returns branches, each of branches is an object implemented `Chain` trait.
    ///
    fn get_branches(&self) -> &[Self::Chain];

    ///
    /// Returns `Handler` if exists.
    ///
    fn get_handler(&self) -> Option<&Self::Handler>;

    ///
    /// Returns custom joiner if exists.
    ///
    fn get_joiner(&self) -> Option<&TokenStream>;

    ///
    /// Returns transpose results configuration if specified.
    ///
    fn get_transpose_results_option(&self) -> Option<bool>;

    ///
    /// Returns lazy branches configuration if provided.
    ///
    fn get_lazy_branches_option(&self) -> Option<bool>;
}

///
/// Default struct which represents result of parsing `join!` macro input.
///
pub struct JoinInputDefault {
    pub futures_crate_path: Option<Path>,
    pub custom_joiner: Option<TokenStream>,
    pub transpose_results: Option<bool>,
    pub lazy_branches: Option<bool>,
    pub branches: Vec<ActionExprChain>,
    pub handler: Option<Handler>,
}

impl JoinInput for JoinInputDefault {
    type Chain = ActionExprChain;
    type Handler = Handler;

    fn get_futures_crate_path(&self) -> Option<&Path> {
        self.futures_crate_path.as_ref()
    }

    fn get_branches(&self) -> &[Self::Chain] {
        &self.branches
    }

    fn get_handler(&self) -> Option<&Self::Handler> {
        self.handler.as_ref()
    }

    fn get_joiner(&self) -> Option<&TokenStream> {
        self.custom_joiner.as_ref()
    }

    fn get_transpose_results_option(&self) -> Option<bool> {
        self.transpose_results.as_ref().copied()
    }

    fn get_lazy_branches_option(&self) -> Option<bool> {
        self.lazy_branches.as_ref().copied()
    }
}

///
/// Generates output of the `join!` macro based on parsed input and given config.
///
pub fn generate_join<T: JoinInput<Chain = ActionExprChain, Handler = Handler>>(
    join: &T,
    config: Config,
) -> TokenStream {
    let default_futures_crate_path = parse_quote! { ::futures };

    Join::new(
        join.get_branches(),
        join.get_handler(),
        if let Some(futures_crate_path) = join.get_futures_crate_path() {
            Some(futures_crate_path)
        } else if config.is_async {
            Some(&default_futures_crate_path)
        } else {
            None
        },
        join.get_joiner(),
        join.get_transpose_results_option(),
        join.get_lazy_branches_option(),
        config,
    )
    .unwrap()
    .into_token_stream()
}

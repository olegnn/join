//!
//! `JoinInput` trait used to generate output of the `join!` macro based on input and given config.
//!

pub mod config;
pub mod join_output;
pub mod name_constructors;
pub mod parse;

pub use config::Config;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::Path;

use super::handler::Handler;
use crate::action_expr_chain::ActionExprChain;
use crate::chain::Chain;
use join_output::JoinOutput;
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
    fn futures_crate_path(&self) -> Option<&Path>;

    ///
    /// Returns branches, each of branches is an object implemented `Chain` trait.
    ///
    fn branches(&self) -> &[Self::Chain];

    ///
    /// Returns `Handler` if exists.
    ///
    fn handler(&self) -> Option<&Self::Handler>;

    ///
    /// Returns custom joiner if exists.
    ///
    fn joiner(&self) -> Option<&TokenStream>;

    ///
    /// Returns transpose results configuration if specified.
    ///
    fn transpose_results_option(&self) -> Option<bool>;

    ///
    /// Returns lazy branches configuration if provided.
    ///
    fn lazy_branches_option(&self) -> Option<bool>;
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

    fn futures_crate_path(&self) -> Option<&Path> {
        self.futures_crate_path.as_ref()
    }

    fn branches(&self) -> &[Self::Chain] {
        &self.branches
    }

    fn handler(&self) -> Option<&Self::Handler> {
        self.handler.as_ref()
    }

    fn joiner(&self) -> Option<&TokenStream> {
        self.custom_joiner.as_ref()
    }

    fn transpose_results_option(&self) -> Option<bool> {
        self.transpose_results.as_ref().copied()
    }

    fn lazy_branches_option(&self) -> Option<bool> {
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

    JoinOutput::new(
        join.branches(),
        join.handler(),
        if let Some(futures_crate_path) = join.futures_crate_path() {
            Some(futures_crate_path)
        } else if config.is_async {
            Some(&default_futures_crate_path)
        } else {
            None
        },
        join.joiner(),
        join.transpose_results_option(),
        join.lazy_branches_option(),
        config,
    )
    .unwrap()
    .into_token_stream()
}

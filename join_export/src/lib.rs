//!
//! Exports of the `join!`, `join_async!`, `join_spawn!`, `join_async_spawn!`, `async_spawn!` macros.
//!

extern crate proc_macro;
extern crate syn;
extern crate join_impl;

use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;

use join_impl::{generate_join, Config, Join};

fn join_impl(join: Join, config: Config) -> TokenStream {
    TokenStream::from(generate_join(join, config))
}

#[proc_macro_hack]
pub fn join(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Join);

    join_impl(
        parsed,
        Config {
            is_async: false,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn join_async(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Join);

    join_impl(
        parsed,
        Config {
            is_async: true,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn join_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Join);

    join_impl(
        parsed,
        Config {
            is_async: false,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Join);

    join_impl(
        parsed,
        Config {
            is_async: false,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn join_async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Join);

    join_impl(
        parsed,
        Config {
            is_async: true,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Join);

    join_impl(
        parsed,
        Config {
            is_async: true,
            spawn: true,
        },
    )
}

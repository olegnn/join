//!
//! Exports of the `join!`, `join_async!`, `join_spawn!`, `join_async_spawn!`, `async_spawn!`, `try_join!`, `try_join_async!`, `try_join_spawn!`, `try_join_async_spawn!`, `try_async_spawn!` macros.
//!

extern crate join_impl;
extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;

use join_impl::{generate_join, Config, JoinDefault};

fn join_impl(join: JoinDefault, config: Config) -> TokenStream {
    TokenStream::from(generate_join(&join, config))
}

#[proc_macro_hack]
pub fn join(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: false,
            is_spawn: false,
            is_try: false,
        },
    )
}

#[proc_macro_hack]
pub fn join_async(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: true,
            is_spawn: false,
            is_try: false,
        },
    )
}

#[proc_macro_hack]
pub fn join_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: false,
            is_spawn: true,
            is_try: false,
        },
    )
}

#[proc_macro_hack]
pub fn spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: false,
            is_spawn: true,
            is_try: false,
        },
    )
}

#[proc_macro_hack]
pub fn join_async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: true,
            is_spawn: true,
            is_try: false,
        },
    )
}

#[proc_macro_hack]
pub fn async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: true,
            is_spawn: true,
            is_try: false,
        },
    )
}

#[proc_macro_hack]
pub fn try_join(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: false,
            is_spawn: false,
            is_try: true,
        },
    )
}

#[proc_macro_hack]
pub fn try_join_async(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: true,
            is_spawn: false,
            is_try: true,
        },
    )
}

#[proc_macro_hack]
pub fn try_join_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: false,
            is_spawn: true,
            is_try: true,
        },
    )
}

#[proc_macro_hack]
pub fn try_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: false,
            is_spawn: true,
            is_try: true,
        },
    )
}

#[proc_macro_hack]
pub fn try_join_async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: true,
            is_spawn: true,
            is_try: true,
        },
    )
}

#[proc_macro_hack]
pub fn try_async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as JoinDefault);

    join_impl(
        parsed,
        Config {
            is_async: true,
            is_spawn: true,
            is_try: true,
        },
    )
}

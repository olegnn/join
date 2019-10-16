//!
//! Exports of the `union!`, `union_async!`, `asyncion!`, `union_spawn!`, `union_async_spawn!`, `async_spawn!` macros.
//!

extern crate proc_macro;
extern crate syn;
extern crate union_impl;

use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;

use union_impl::{generate_union, Config, Union};

fn union_impl(union: Union, config: Config) -> TokenStream {
    TokenStream::from(generate_union(union, config))
}

#[proc_macro_hack]
pub fn union(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: false,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn union_async(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn asyncion(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn union_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: false,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: false,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn union_async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: true,
        },
    )
}

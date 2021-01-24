//!
//! Implementation of the `join!` macro.
//!
#![allow(clippy::large_enum_variant)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

pub mod action_expr_chain;
pub mod chain;
pub mod common;
pub mod handler;
pub mod join;
pub mod parse;

pub use crate::join::{generate_join, Config, JoinInputDefault};

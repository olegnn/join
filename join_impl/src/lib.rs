//!
//! Implementation of the `join!` macro.
//!
//!

extern crate proc_macro;
extern crate proc_macro2;
extern crate proc_macro_hack;
extern crate quote;
extern crate syn;

pub mod expr_chain;
pub mod handler;
pub mod join;

pub use crate::join::*;

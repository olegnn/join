//!
//! Name constructors for code generation by `join!` macro.
//!

use proc_macro2::{Ident, Span};
use quote::format_ident;

///
/// Constructs name for variable with given index. For internal usage.
///
pub fn construct_var_name(index: impl Into<usize>) -> Ident {
    format_ident!("__value_{}", index.into())
}
///
/// Constructs step result name using given index. For internal usage.
///
pub fn construct_step_result_name(index: impl Into<usize>) -> Ident {
    format_ident!("__step_{}_result", index.into())
}

///
/// Constructs result name with given index. For internal usage.
///
pub fn construct_result_name(index: impl Into<usize>) -> Ident {
    format_ident!("__result_{}", index.into())
}

///
/// Constructs thread builder name with given index. For internal usage.
///
pub fn construct_thread_builder_name(index: impl Into<usize>) -> Ident {
    format_ident!("__join_thread_builder_{}", index.into())
}

///
/// Constructs inspect function name. For internal usage.
///
pub fn construct_inspect_fn_name() -> Ident {
    Ident::new("__inspect", Span::call_site())
}

///
/// Constructs `tokio::spawn` wrapper function name. For internal usage.
///
pub fn construct_spawn_tokio_fn_name() -> Ident {
    Ident::new("__spawn_tokio", Span::call_site())
}

///
/// Constructs result wrapper in order to be used when expression is block. For internal usage.
///
pub fn construct_expr_wrapper_name(
    index: impl Into<usize>,
    expr_index: impl Into<usize>,
    internal_index: impl Into<usize>,
) -> Ident {
    format_ident!(
        "__expr_wrapper_{}_{}_{}",
        index.into(),
        expr_index.into(),
        internal_index.into()
    )
}

///
/// Constructs thread name with given index. This name will be displayed as result of thread::current().name().unwrap() or
/// in case of thread's panic.
/// ```
/// extern crate join;
///
/// use std::thread;
/// use join::join_spawn;
///
/// fn main() {
///     join_spawn! {
///         Ok::<_, ()>("hello world") |> |value| {
///             println!("{}", thread::current().name().unwrap()); // main_join_0
///             value
///         }
///     };
/// }
/// ```
/// In runtime thread's name will be constructed from {(name of parent thread (if it's `Some`) + `_`) or `` (if it's None)}join_{index of branch (starting from 0)}.
///
/// Example code with many branches.
/// ```
/// extern crate join;
///
/// use std::thread;
///
/// use join::try_join_spawn;
///
/// fn get_current_thread_name() -> String {
///     thread::current().name().unwrap().to_owned()
/// }
///
/// fn print_branch_thread_name(index: &Result<usize, ()>) {
///     println!("Branch: {}. Thead name: {}.", index.unwrap(), get_current_thread_name());
/// }
///
/// fn main() {
///     let _ = try_join_spawn! {
///         Ok(0) ?? print_branch_thread_name,
///         Ok(1) ?? print_branch_thread_name,
///         try_join_spawn! {
///             Ok(2) ?? print_branch_thread_name,
///             try_join_spawn! {
///                 Ok(3) ?? print_branch_thread_name,
///             }
///         }
///     }.unwrap();
/// }
///
/// // Branch: 0. Thead name: main_join_0.
/// // Branch: 1. Thead name: main_join_1.
/// // Branch: 2. Thead name: main_join_2_join_0.
/// // Branch: 3. Thead name: main_join_2_join_1_join_0.
/// // Order could be different.
/// ```
///
pub fn construct_thread_name(index: impl Into<usize>) -> String {
    format!("join_{}", index.into())
}

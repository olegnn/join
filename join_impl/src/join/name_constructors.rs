//!
//! Name constructors for code generation by `join!` macro.
//!

use proc_macro2::{Ident, Span};
use quote::format_ident;

///
/// Constructs name for variable with given index.
///
pub fn construct_var_name(index: impl Into<usize>) -> Ident {
    format_ident!("__value_{}", index.into() as u16)
}
///
/// Constructs step result name using given index.
///
pub fn construct_step_results_name(index: impl Into<usize>) -> Ident {
    format_ident!("__step_{}_results", index.into() as u16)
}

///
/// Constructs result name with given index.
///
pub fn construct_result_name(index: impl Into<usize>) -> Ident {
    format_ident!("__result_{}", index.into() as u16)
}

///
/// Constructs thread builder name with given index.
///
pub fn construct_thread_builder_name(index: impl Into<usize>) -> Ident {
    format_ident!("__join_thread_builder_{}", index.into() as u16)
}

///
/// Constructs inspect function name.
///
pub fn construct_inspect_fn_name() -> Ident {
    Ident::new("__inspect", Span::call_site())
}

///
/// Constructs `tokio::spawn` wrapper function name.
///
pub fn construct_spawn_tokio_fn_name() -> Ident {
    Ident::new("__spawn_tokio", Span::call_site())
}

///
/// Constructs results name.
///
pub fn construct_results_name() -> Ident {
    Ident::new("__results", Span::call_site())
}

///
/// Constructs handler name.
///
pub fn construct_handler_name() -> Ident {
    Ident::new("__handler", Span::call_site())
}

///
/// Constructs internal value name with no index.
///
pub fn construct_internal_value_name() -> Ident {
    Ident::new("__value", Span::call_site())
}

///
/// Constructs result wrapper in order to be used when expression is block.
///
pub fn construct_expr_wrapper_name(
    index: impl Into<usize>,
    expr_index: impl Into<usize>,
    internal_index: impl Into<usize>,
) -> Ident {
    format_ident!(
        "__expr_wrapper_{}_{}_{}",
        index.into() as u16,
        expr_index.into() as u16,
        internal_index.into() as u16
    )
}

///
/// Constructs thread builder fn name. This function will generate thread builder with specified name.
/// This name will be displayed as result of thread::current().name().unwrap() or
/// in case of thread's panic.
///
/// # Example:
/// ```
/// extern crate join;
///
/// use std::thread;
/// use join::join_spawn;
///
/// join_spawn! {
///     Ok::<_, ()>("hello world") |> |value| {
///         println!("{}", thread::current().name().unwrap()); // main_join_0
///         value
///     }
/// };
/// ```
/// In runtime thread's name will be constructed from name of parent thread and join_%branch_index%.
///
/// # Example with several branches:
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
///     println!("Branch: {}. Thread name: {}.", index.unwrap(), get_current_thread_name());
/// }
///
/// let _ = try_join_spawn! {
///     Ok(0) ?? print_branch_thread_name,
///     Ok(1) ?? print_branch_thread_name,
///     try_join_spawn! {
///         Ok(2) ?? print_branch_thread_name,
///         try_join_spawn! {
///             Ok(3) ?? print_branch_thread_name,
///         }
///     }
/// }.unwrap();
///
/// // Branch: 0. Thread name: main_join_0.
/// // Branch: 1. Thread name: main_join_1.
/// // Branch: 2. Thread name: main_join_2_join_0.
/// // Branch: 3. Thread name: main_join_2_join_1_join_0.
/// // Order could be different.
/// ```
///
pub fn construct_thread_builder_fn_name() -> Ident {
    Ident::new("__construct_thread_builder", Span::call_site())
}

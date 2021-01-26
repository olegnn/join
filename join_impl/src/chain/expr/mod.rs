//!
//! All expression definitions and `InnerExpr` trait.
//!

use syn::Expr;

mod action_expr;
mod err_expr;
mod initial_expr;
mod process_expr;
mod types;

pub use action_expr::{Action, ActionExpr};
pub use err_expr::ErrExpr;
pub use initial_expr::InitialExpr;
pub use process_expr::ProcessExpr;
pub use types::*;

///
/// Provide functionality to get or replace inner `Expr`(s).
///
pub trait InnerExpr
where
    Self: Sized,
{
    ///
    /// Extracts `Expr`(s) from given value if applicable.
    ///
    fn get_inner_exprs(&self) -> Option<&[Expr]>;

    ///
    /// Replaces current expr by given `Expr` if it's possible, returning Some(`Self`) with given `Expr`(s),
    /// otherwise returns `None`.
    ///
    fn replace_inner_exprs(self, expr: &[Expr]) -> Option<Self>;

    ///
    /// Checks if expr can be replaced.
    ///
    fn is_replaceable(&self) -> bool {
        true
    }
}

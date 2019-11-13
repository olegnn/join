//!
//! Contains definition of `InnerExpr` traits.
//!

use syn::Expr;

mod action_expr;
mod err_expr;
mod initial_expr;
mod process_expr;

pub use action_expr::{ActionExpr, ErrActionExpr, ProcessActionExpr};
pub use err_expr::ErrExpr;
pub use initial_expr::InitialExpr;
pub use process_expr::ProcessExpr;

pub enum AnyExpr {
    Process(ProcessExpr),
    Initial(InitialExpr),
    Err(ErrExpr),
}

///
/// Provides functionality to get or replace inner `Expr`.
///
pub trait InnerExpr
where
    Self: Sized,
{
    ///
    /// Extracts `Expr` from given valuem if applicable.
    ///
    fn extract_inner(&self) -> Option<Vec<&Expr>>;

    ///
    /// Replaces current expr by given `Expr` if it's possible, returning Some(new `Self`) with given `Expr`,
    /// otherwise returns None.
    ///
    fn replace_inner(&self, expr: &mut Vec<Expr>) -> Option<Self>;

    ///
    /// Can expr be replaced or not.
    ///
    fn is_replaceable(&self) -> bool {
        true
    }
}

//!
//! Contains all definition of expression + `ExtractExpr` and `ReplaceExpr` traits.
//!

use syn::Expr;

mod action_expr;
mod default_expr;
mod process_expr;

pub use action_expr::{ActionExpr, DefaultActionExpr, ProcessActionExpr};
pub use default_expr::DefaultExpr;
pub use process_expr::ProcessExpr;

///
/// Trait which allows to extract `Expr` and `InnerExpr` (which can be any superset of `Expr`).
///
pub trait ExtractExpr {
    ///
    /// Defines superset of `Expr` used by given struct.
    ///
    type InnerExpr;

    ///
    /// Extracts `Expr` from given value.
    ///
    fn extract_expr(&self) -> &Expr;

    ///
    /// Extracts `InnerExpr`. If `InnerExpr` is `Expr`, acts as `extract_expr`.
    ///
    fn extract_inner_expr(&self) -> &Self::InnerExpr;
}

///
/// Provides functionality to replacee inner `Expr` and return new `Self` with given `Expr`.
///
pub trait ReplaceExpr
where
    Self: Sized,
{
    ///
    /// Replaces current expr by given `Expr`, returning new `Self` with given `Expr`.
    ///
    fn replace_expr(&self, expr: Expr) -> Self;
}

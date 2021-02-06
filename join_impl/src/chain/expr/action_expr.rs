//!
//! Enum of all expression types.
//!

use super::InnerExpr;
use super::{ErrExpr, InitialExpr, ProcessExpr};
use syn::Expr;

///
/// One of either `Process`, `Err` or `Initial` expression.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ActionExpr {
    Process(ProcessExpr),
    Err(ErrExpr),
    Initial(InitialExpr),
}

impl InnerExpr for ActionExpr {
    fn replace_inner_exprs(self, exprs: &[Expr]) -> Option<Self> {
        Some(match self {
            ActionExpr::Process(expr) => ActionExpr::Process(expr.replace_inner_exprs(exprs)?),
            ActionExpr::Err(expr) => ActionExpr::Err(expr.replace_inner_exprs(exprs)?),
            ActionExpr::Initial(expr) => ActionExpr::Initial(expr.replace_inner_exprs(exprs)?),
        })
    }

    fn inner_exprs(&self) -> Option<&[Expr]> {
        match self {
            ActionExpr::Process(expr) => expr.inner_exprs(),
            ActionExpr::Err(expr) => expr.inner_exprs(),
            ActionExpr::Initial(expr) => expr.inner_exprs(),
        }
    }
}

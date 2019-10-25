//!
//! `Unit` module describes one unit of expression parsing which contains current `Expr`
//! and optional next `ActionGroup`
//!

use syn::Expr;

use super::super::group::ActionGroup;

///
/// `Unit` defines one unit of expression parsing.
///
pub struct Unit {
    pub expr: Expr,
    pub next_group_type: Option<ActionGroup>,
}

pub type UnitResult = syn::Result<Unit>;

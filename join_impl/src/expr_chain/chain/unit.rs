//!
//! `Unit` module describes one unit of expression parsing which contains current `Expr`
//! and optional next `ActionGroup`
//!
use super::super::group::ActionGroup;

///
/// `Unit` defines one unit of expression parsing.
///
pub struct Unit<T> {
    pub expr: T,
    pub next_group_type: Option<ActionGroup>,
}

pub type UnitResult<T> = syn::Result<Unit<T>>;

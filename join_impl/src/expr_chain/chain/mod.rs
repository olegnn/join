//!
//! Contains `Chain` trait definition.
//!
use syn::PatIdent;

mod unit;

pub use unit::{Unit, UnitResult};

///
/// `Chain` trait describes any chain with members of type `Member` and optional `Pat`.
///
pub trait Chain {
    type Member: Sized;
    ///
    /// Returns vec of self members.
    ///
    fn get_members(&self) -> &[Self::Member];

    ///
    /// Returns optional `Pat` associated with chain.
    ///
    fn get_pat(&self) -> Option<&PatIdent>;
}

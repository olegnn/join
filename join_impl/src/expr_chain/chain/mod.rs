//!
//! Contains `Chain` trait definition.
//!
use syn::PatIdent;

mod unit;

pub use unit::{TransformParsed, Unit, UnitResult};

///
/// `Chain` trait describes any chain with members of type `Member` and optional `PatIdent`.
///
pub trait Chain {
    type Member: Sized;
    ///
    /// Returns self members.
    ///
    fn get_members(&self) -> &[Self::Member];

    ///
    /// Returns optional `PatIdent` associated with chain.
    ///
    fn get_pat(&self) -> Option<&PatIdent>;
}

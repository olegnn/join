//!
//! Contains `Chain` trait definition.
//!

use std::cell::Ref;
use syn::parse::ParseStream;
use syn::Pat;

mod unit;

pub use unit::{Unit, UnitResult};

///
/// `Chain` trait describes any chain which can be constructed from `ParseStream`
/// with members of type `Member` and optional `Pat`.
///
pub trait Chain {
    type Member: Sized;

    ///
    /// Generates self from `ParseStream`.
    ///
    fn generate_from_stream(&mut self, input: ParseStream) -> syn::Result<()>;

    ///
    /// Returns vec of self members.
    ///
    fn get_members(&self) -> Ref<'_, Vec<Self::Member>>;

    ///
    /// Returns optional `Pat` associated with chain.
    ///
    fn get_pat(&self) -> Ref<'_, Option<Pat>>;
}

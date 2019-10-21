//!
//! `Chain` trait describes any chain which can be constructed from `ParseStream`
//! with members of type `Member` and optional `Pat`.
//! `Unit` defines one unit of expression parsing.
//!

use std::cell::Ref;
use syn::parse::ParseStream;
use syn::Pat;

mod unit;

pub use unit::{Unit, UnitResult};

pub trait Chain
where
    Self: Sized,
{
    type Member;

    fn new(
        input: ParseStream,
        other_pattern_check: Box<dyn Fn(ParseStream<'_>) -> bool>,
    ) -> syn::Result<Option<Self>>;

    fn get_members(&self) -> Ref<'_, Vec<Self::Member>>;

    fn get_pat(&self) -> Ref<'_, Option<Pat>>;
}

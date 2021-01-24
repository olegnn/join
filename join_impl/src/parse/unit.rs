//!
//! `Unit` module describes one unit of expression parsing which contains currently parsed `T`
//! and optional next `V`
//!
//!
use crate::common::MapOver;
use syn::parse::{Parse, ParseStream};

///
/// `Unit` defines one unit of expression parsing.
///
pub struct Unit<T, N> {
    ///
    /// Parsed value (for example `Expr` { Ok(1) }).
    ///
    pub parsed: T,
    ///
    /// Next `N` if provided.
    ///
    pub next: Option<N>,
}

///
/// `UnitResult` defines `Result` of one unit expression parsing.
///
pub type UnitResult<T, N> = syn::Result<Unit<T, N>>;

pub trait ParseUnit<N> {
    ///
    /// Parses input stream until next `N`
    ///
    fn parse_unit<T: Parse>(
        &self,
        input: ParseStream,
        allow_empty_parsed: bool,
    ) -> UnitResult<T, N>;
}

impl<V, R, N> MapOver<V, R, UnitResult<R, N>> for UnitResult<V, N> {
    fn map_over<F>(self, transform: F) -> UnitResult<R, N>
    where
        F: FnOnce(V) -> R,
    {
        self.map(|Unit { parsed, next }| Unit {
            parsed: transform(parsed),
            next,
        })
    }
}

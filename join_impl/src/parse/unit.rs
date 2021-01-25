//!
//! `Unit` module describes one unit of expression parsing which contains currently parsed `T`
//! and optional next `V`
//!
//!
use syn::parse::{Parse, ParseStream};

///
/// Defines one unit of expression parsing.
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
/// Defines `Result` of one unit expression parsing.
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

///
/// Allows to map `Self` over some parsed.
///
pub trait MapParsed<T, R> {
    type Output;

    fn map_parsed<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(T) -> R;
}

impl<T, R, N> MapParsed<T, R> for UnitResult<T, N> {
    type Output = UnitResult<R, N>;

    fn map_parsed<F>(self, f: F) -> Self::Output
    where
        F: FnOnce(T) -> R,
    {
        self.map(|Unit { parsed, next }| Unit {
            parsed: f(parsed),
            next,
        })
    }
}

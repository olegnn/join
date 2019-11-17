//!
//! `Unit` module describes one unit of expression parsing which contains current `Expr`
//! and optional next `ActionGroup`
//!
use super::super::group::ActionGroup;

///
/// `Unit` defines one unit of expression parsing.
///
pub struct Unit<T> {
    ///
    /// Parsed value (for example `Expr` { Ok(1) }).
    ///
    pub parsed: T,
    ///
    /// Next `ActionGroup`, if provided.
    ///
    pub next_group_type: Option<ActionGroup>,
}

pub type UnitResult<T> = syn::Result<Unit<T>>;

///
/// Trait which provides functionality to apply given `transform` function to change
/// result from `Self` (`<T>`) to `To` (`<R>`) where `transform`: `F: FnOnce(T) -> R`.
///
pub trait TransformParsed<T, R> {
    type To;
    ///
    /// Transforms `Self` to `To` using `transform` function.
    ///
    fn transform_parsed<F>(self, transform: F) -> Self::To
    where
        F: FnOnce(T) -> R;
}

impl<T, R> TransformParsed<T, R> for UnitResult<T> {
    type To = UnitResult<R>;

    fn transform_parsed<F>(self, transform: F) -> Self::To
    where
        F: FnOnce(T) -> R,
    {
        self.map(
            |Unit {
                 parsed,
                 next_group_type,
             }| Unit {
                parsed: transform(parsed),
                next_group_type,
            },
        )
    }
}

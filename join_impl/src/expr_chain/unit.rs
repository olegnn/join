//!
//! `Unit` module describes one unit of expression parsing which contains currently parsed `T`
//! and optional next `ActionGroup`
//!
use super::group::ActionGroup;

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

///
/// `UnitResult` defines `Result` of one unit expression parsing.
///
pub type UnitResult<T> = syn::Result<Unit<T>>;

///
/// Trait which provides functionality to apply given `transform` function to change
/// result from `Self` (`<T>`) to `O` (`<R>`) where `transform`: `F: FnOnce(T) -> R`.
///
pub trait TransformParsed<T, R, O> {
    ///
    /// Transforms `Self` to `O` using `transform` function.
    ///
    fn transform_parsed<F>(self, transform: F) -> O
    where
        F: FnOnce(T) -> R;
}

impl<T, R> TransformParsed<T, R, UnitResult<R>> for UnitResult<T> {
    fn transform_parsed<F>(self, transform: F) -> UnitResult<R>
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

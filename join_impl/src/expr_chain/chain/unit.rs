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
/// Trait which provides functionality to apply given `transform` function to `parsed` field in order to change
/// from `UnitResult<T>` to `UnitResult<R>` where `transform`: `F: FnOnce(T) -> R`.
/// 
pub trait TransformParsed<T> {
    fn transform_parsed<R, F>(self, transform: F) -> UnitResult<R>
    where
        F: FnOnce(T) -> R;
}

impl<T> TransformParsed<T> for UnitResult<T> {
    fn transform_parsed<R, F>(self, transform: F) -> UnitResult<R>
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

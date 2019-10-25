//!
//! Definition of `GroupDeterminer` and related macros.
//!

use proc_macro2::{TokenStream, TokenTree};
use syn::parse::ParseStream;

use super::ActionGroup;

///
/// `GroupDeterminer` is used to determine any `ActionGroup` or separator (for ex. `,`) in `ParseStream`
///
pub struct GroupDeterminer {
    group_type: Option<ActionGroup>,
    check_input_fn: Box<dyn Fn(ParseStream) -> bool>,
    check_parsed_fn: Option<Box<dyn Fn(TokenStream) -> bool>>,
    length: usize,
}

///
/// Creates instant and deferred `GroupDeterminer`'s for given `ActionGroup`.
/// Deferred are prefixed by `~`.
/// For example: `->` is instant, `~->` is deferred.
///
#[macro_export]
macro_rules! instant_and_deferred_determiners {
    ($($group_type: ident => $($tokens: expr),+; $length: expr),+) => {
        vec![
            $(
                $crate::group_determiner!(
                    $crate::expr_chain::group::ActionGroup::Instant($crate::expr_chain::group::CommandGroup::$group_type) => $($tokens),+; $length
                ),
                $crate::group_determiner!(
                    $crate::expr_chain::group::ActionGroup::Deferred($crate::expr_chain::group::CommandGroup::$group_type) => Token![~], $($tokens),+; $length + 1
                )
            ),*
        ]
    };
}

///
/// Creates function which checks if `ParseStream` next values are provided tokens. (Max = 3).
///
#[macro_export]
macro_rules! tokens_checker {
    ($token1: expr, $token2:expr, $token3:expr) => {
        Box::new(|input: ::syn::parse::ParseStream<'_>| {
            input.peek($token1) && input.peek2($token2) && input.peek3($token3)
        })
    };
    ($token1: expr, $token2:expr) => {
        Box::new(|input: ::syn::parse::ParseStream<'_>| input.peek($token1) && input.peek2($token2))
    };
    ($token: expr) => {
        Box::new(|input: ::syn::parse::ParseStream<'_>| input.peek($token))
    };
}

///
/// Creates `GroupDeterminer` with given (`ActionGroup` => tokens; length; Fn that checks `TokenStream` of parsed tokens`)
/// Example:
/// ```
/// use join_impl::expr_chain::group::{ActionGroup, CommandGroup};
/// use join_impl::group_determiner;
/// use syn::Token;
///
/// fn main() {
///     let then_determiner = group_determiner!(ActionGroup::Instant(CommandGroup::Then) => Token![->]; 2); // last param fn is optional
/// }
/// ```
///
#[macro_export]
macro_rules! group_determiner {
    ($group_type: expr => $($tokens: expr),+; $length: expr; $check_parsed_fn: expr) => {{
        let check_input_fn = $crate::tokens_checker!($($tokens),*);
        $crate::expr_chain::group::GroupDeterminer::new(
            $group_type,
            check_input_fn,
            Some($check_parsed_fn),
            $length
        )
    }};
    ($group_type: expr => $($tokens: expr),+; $length: expr) => {
        $crate::group_determiner!(
            $group_type => $($tokens),+; $length; Box::new($crate::expr_chain::utils::is_valid_expr)
        )
    };
}

impl GroupDeterminer {
    ///
    /// Constructs new `GroupDeterminer`.
    /// Example:
    /// ```
    /// extern crate join_impl;
    /// extern crate syn;
    ///
    /// use syn::Token;
    /// use join_impl::expr_chain::group::GroupDeterminer;
    ///
    /// fn main() {
    ///     let first_comma_determiner = GroupDeterminer::new(
    ///         None, // Because comma is not an action group
    ///         Box::new(|input| input.peek(Token![,])),
    ///         None,
    ///         1
    ///     );
    /// }
    /// ```
    ///
    pub fn new(
        group_type: impl Into<Option<ActionGroup>>,
        check_input_fn: Box<dyn Fn(ParseStream) -> bool>,
        check_parsed_fn: Option<Box<dyn Fn(TokenStream) -> bool>>,
        length: usize,
    ) -> Self {
        GroupDeterminer {
            group_type: group_type.into(),
            check_input_fn,
            check_parsed_fn,
            length,
        }
    }

    ///
    /// Returns type of group of `GroupDeterminer`.
    ///

    pub fn get_group_type(&self) -> Option<ActionGroup> {
        self.group_type
    }

    ///
    /// Checks if input next tokens are of self group type.
    ///  

    pub fn check_input(&self, input: ParseStream) -> bool {
        (self.check_input_fn)(input)
    }

    ///
    /// Checks already parsed tokens. In many cases it's used to check if
    /// parsed tokens are valid expression. in this case we can say for sure that
    /// we found separator.
    ///
    pub fn check_parsed(&self, input: TokenStream) -> bool {
        self.check_parsed_fn
            .as_ref()
            .map(|checker| checker(input))
            .unwrap_or(true)
    }

    ///
    /// Used to parse `length` tokens of type `TokenTree` from input `ParseStream`.
    ///
    pub fn erase_input<'a>(&self, input: ParseStream<'a>) -> syn::Result<ParseStream<'a>> {
        (0..self.length()).try_for_each(|_| input.parse::<TokenTree>().map(|_| ()))?;
        Ok(input)
    }

    ///
    /// Returns value of `length` field.
    ///
    pub fn length(&self) -> usize {
        self.length
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::utils::is_valid_expr;
    use super::super::*;
    use super::*;

    #[test]
    fn it_creates_comma_determiner() {
        let first_comma_determiner = GroupDeterminer::new(
            None, // Because comma is not an action group
            Box::new(|input| input.peek(::syn::Token![,])),
            None,
            1,
        );

        assert_eq!(first_comma_determiner.get_group_type(), None);
        assert!(first_comma_determiner.check_parsed(::quote::quote! { , }));
        assert_eq!(first_comma_determiner.length(), 1);
    }

    #[test]
    fn it_creates_then_determiner() {
        let then_determiner = GroupDeterminer::new(
            Some(ActionGroup::Instant(CommandGroup::Then)), // Because comma is not an action group
            Box::new(|input| input.peek(::syn::Token![=>])),
            Some(Box::new(is_valid_expr)),
            2,
        );

        assert_eq!(
            then_determiner.get_group_type(),
            Some(ActionGroup::Instant(CommandGroup::Then))
        );
        assert!(then_determiner.check_parsed(::quote::quote! { 23 }));
        assert_eq!(then_determiner.length(), 2);
    }
}

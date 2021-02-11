//!
//! Definition of `GroupDeterminer` and related macros.
//!

use crate::parse::utils::is_valid_stream;
use proc_macro2::{TokenStream, TokenTree};
use syn::parse::{Parse, ParseStream};

use super::Combinator;

pub type CheckStreamFn = fn(ParseStream) -> bool;

#[derive(Clone, Copy)]
union CheckStreamFnPointer {
    f: CheckStreamFn,
    raw: *const (),
}

unsafe impl std::marker::Send for CheckStreamFnPointer {}

unsafe impl std::marker::Sync for CheckStreamFnPointer {}

///
/// `GroupDeterminer` is used to determine any `Combinator` or separator (for ex. `,`) in `ParseStream`
///
#[derive(Clone)]
pub struct GroupDeterminer {
    combinator: Option<Combinator>,
    check_input_fn: CheckStreamFnPointer,
    validate_parsed: bool,
    length: u8,
}

///
/// Creates `GroupDeterminer` for given `Combinator`s with provided tokens.
///
#[macro_export]
macro_rules! define_group_determiners {
    ($($combinator: ident => $($token: expr),+ => $length: expr),+) => {{
        [
            $crate::define_determiner_with_no_group!(Token![,] => 0),
            $(
                $crate::define_group_determiner!(
                    $crate::chain::group::Combinator::$combinator => $($token),+ => $length
                )
            ),*,
            $crate::chain::group::GroupDeterminer::new_const(
                None,
                $crate::handler::Handler::peek_handler as *const (),
                true,
                0
            )
        ]
    }};
}

///
/// Creates `GroupDeterminer` with no `Combinator` and provided tokens.
///
#[macro_export]
macro_rules! define_determiner_with_no_group {
    ($($token: expr),+ => $length: expr) => {{
        let check_tokens = $crate::define_tokens_checker!($($token),+);
        $crate::chain::group::GroupDeterminer::new_const(
            None,
            check_tokens as *const (),
            true,
            $length
        )
    }}
}

///
/// Creates function which checks if `ParseStream` next values are provided tokens.
///
#[macro_export]
macro_rules! define_tokens_checker {
    ($token1: expr, $token2:expr, $token3:expr) => {{
        fn check_tokens(input: ::syn::parse::ParseStream<'_>) -> bool {
            input.peek($token1) && input.peek2($token2) && input.peek3($token3)
        }
        check_tokens
    }};
    ($token1: expr, $token2:expr) => {{
        fn check_tokens(input: ::syn::parse::ParseStream<'_>) -> bool {
            input.peek($token1) && input.peek2($token2)
        }
        check_tokens
    }};
    ($token: expr) => {{
        fn check_tokens(input: ::syn::parse::ParseStream<'_>) -> bool { input.peek($token) }
        check_tokens
    }};
    ($($token: expr),+) => {{
        fn check_tokens(input: ::syn::parse::ParseStream<'_>) -> bool {
            let input = input.fork();
            $(
                input.peek($token) && $crate::parse::utils::skip(&input)
            )&&+
        }
        check_tokens
    }};
}

///
/// Creates `GroupDeterminer` with given (`Combinator` => tokens => length => ?Check parsed tokens? (optional bool))
///
/// # Example:
/// ```
/// use join_impl::chain::group::Combinator;
/// use join_impl::define_group_determiner;
/// use syn::Token;
///
/// let then_determiner = define_group_determiner!(Combinator::Then => Token![->] => 2); // last param is optional, `true` by default
/// ```
///
#[macro_export]
macro_rules! define_group_determiner {
    ($combinator: expr => $($tokens: expr),+ => $length: expr => $validate_parsed: expr) => {{
        let check_tokens = $crate::define_tokens_checker!($($tokens),*);
        $crate::chain::group::GroupDeterminer::new_const(
            Some($combinator),
            check_tokens as *const (),
            $validate_parsed,
            $length
        )
    }};
    ($combinator: expr => $($tokens: expr),+ => $length: expr) => {
        $crate::define_group_determiner!(
            $combinator => $($tokens),+ => $length => true
        )
    };
}

impl GroupDeterminer {
    ///
    /// Constructs new `GroupDeterminer` using `const fn` (can be used to create const or static item).
    ///
    /// # Example:
    /// ```
    /// extern crate join_impl;
    /// extern crate syn;
    ///
    /// use syn::Token;
    /// use syn::parse::ParseStream;
    /// use join_impl::chain::group::GroupDeterminer;
    ///
    /// fn check_input(input: ParseStream) -> bool { input.peek(Token![,]) }
    ///
    /// let first_comma_determiner = GroupDeterminer::new_const(
    ///     None, // Because comma is not a command group
    ///     check_input as *const (),
    ///     false,
    ///     1
    /// );
    /// ```
    ///
    pub const fn new_const(
        combinator: Option<Combinator>,
        check_input_fn: *const (),
        validate_parsed: bool,
        length: u8,
    ) -> Self {
        Self {
            combinator,
            check_input_fn: CheckStreamFnPointer {
                raw: check_input_fn,
            },
            validate_parsed,
            length,
        }
    }

    ///
    /// Constructs new `GroupDeterminer`.
    ///
    /// # Example:
    /// ```
    /// extern crate join_impl;
    /// extern crate syn;
    ///
    /// use syn::Token;
    /// use syn::parse::ParseStream;
    /// use join_impl::chain::group::GroupDeterminer;
    ///
    /// fn check_input(input: ParseStream) -> bool { input.peek(Token![,]) }
    ///
    /// let first_comma_determiner = GroupDeterminer::new(
    ///     None, // Because comma is not a command group
    ///     check_input,
    ///     false,
    ///     1
    /// );
    /// ```
    ///
    pub fn new(
        combinator: impl Into<Option<Combinator>>,
        check_input_fn: CheckStreamFn,
        validate_parsed: bool,
        length: u8,
    ) -> Self {
        Self {
            combinator: combinator.into(),
            check_input_fn: CheckStreamFnPointer { f: check_input_fn },
            validate_parsed,
            length,
        }
    }

    ///
    /// Returns type of group of `GroupDeterminer`.
    ///
    pub fn combinator(&self) -> Option<Combinator> {
        self.combinator
    }

    ///
    /// Checks if input next tokens are of self group type.
    ///  
    pub fn check_input(&self, input: ParseStream<'_>) -> bool {
        (unsafe { self.check_input_fn.f })(input)
    }

    ///
    /// Checks already parsed tokens. In many cases it's used to check if
    /// parsed tokens are valid expression. in this case we can say for sure that
    /// we found separator.
    ///
    pub fn check_parsed<T: Parse>(&self, input: TokenStream) -> bool {
        !self.validate_parsed || is_valid_stream::<T>(input)
    }

    ///
    /// Used to parse `length` tokens of type `TokenTree` from input `ParseStream`.
    ///
    pub fn erase_input<'b>(&self, input: ParseStream<'b>) -> syn::Result<ParseStream<'b>> {
        for _ in 0..self.len() {
            input.parse::<TokenTree>()?;
        }
        Ok(input)
    }

    ///
    /// Returns value of `length` field.
    ///
    pub fn len(&self) -> u8 {
        self.length
    }

    ///
    /// Returns `true` if `length` is zero.
    ///
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::super::*;
    use super::*;

    #[test]
    fn it_creates_comma_determiner() {
        fn check_comma(input: ParseStream) -> bool {
            input.peek(::syn::Token![,])
        }

        let first_comma_determiner = GroupDeterminer::new_const(
            None, // Because comma is not a command group
            check_comma as *const (),
            false,
            1,
        );

        assert_eq!(first_comma_determiner.combinator(), None);
        assert!(first_comma_determiner.check_parsed::<::syn::Expr>(::quote::quote! { , }));
        assert_eq!(first_comma_determiner.len(), 1);
    }

    #[test]
    fn it_creates_then_determiner() {
        fn check_then(input: ParseStream) -> bool {
            input.peek(::syn::Token![=>])
        }

        let then_determiner = GroupDeterminer::new(Combinator::Then, check_then, true, 2);

        assert_eq!(then_determiner.combinator(), Some(Combinator::Then));
        assert!(then_determiner.check_parsed::<::syn::Expr>(::quote::quote! { 23 }));
        assert_eq!(then_determiner.len(), 2);
    }
}

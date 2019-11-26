//!
//! Definition of `GroupDeterminer` and related macros.
//!

use super::super::utils::is_valid_stream;
use proc_macro2::{TokenStream, TokenTree};
use syn::parse::{Parse, ParseStream};

use super::CommandGroup;

pub type DetermineGroupPredicate = fn(ParseStream) -> bool;

#[derive(Clone, Copy)]
union FnPointer {
    fun: DetermineGroupPredicate,
    raw: *const (),
}

unsafe impl std::marker::Send for FnPointer {}

unsafe impl std::marker::Sync for FnPointer {}

///
/// `GroupDeterminer` is used to determine any `ActionGroup` or separator (for ex. `,`) in `ParseStream`
///
#[derive(Clone)]
pub struct GroupDeterminer {
    group_type: Option<CommandGroup>,
    check_input_fn: FnPointer,
    validate_parsed: bool,
    length: usize,
}

///
/// Creates `GroupDeterminer` for given `CommandGroup`s with provided tokens.
///
#[macro_export]
macro_rules! define_instant_and_deferred_determiners {
    ($($group_type: ident => $($token: expr),+ => $length: expr),+) => {{
        [
            $crate::define_determiner_with_no_group!(Token![,] => 0),
            $(
                $crate::define_group_determiner!(
                    $crate::expr_chain::group::CommandGroup::$group_type => $($token),+ => $length
                )
            ),*,
            $crate::expr_chain::group::GroupDeterminer::new_const(
                None,
                $crate::handler::Handler::peek_handler as *const (),
                true,
                0
            )
        ]
    }};
}

///
/// Creates `GroupDeterminer` with no `CommandGroup` and provided tokens.
///
#[macro_export]
macro_rules! define_determiner_with_no_group {
    ($($token: expr),+ => $length: expr) => {{
        let check_tokens = $crate::define_tokens_checker!($($token),+);
        $crate::expr_chain::group::GroupDeterminer::new_const(
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
                input.peek($token) && $crate::expr_chain::utils::skip(&input)
            )&&+
        }
        check_tokens
    }};
}

///
/// Creates `GroupDeterminer` with given (`ActionGroup` => tokens; length; Check parsed tokens? bool)
/// Example:
/// ```
/// use join_impl::expr_chain::group::CommandGroup;
/// use join_impl::define_group_determiner;
/// use syn::Token;
///
/// fn main() {
///     let then_determiner = define_group_determiner!(CommandGroup::Then => Token![->] => 2); // last param is optional, true by default
/// }
/// ```
///
#[macro_export]
macro_rules! define_group_determiner {
    ($group_type: expr => $($tokens: expr),+ => $length: expr => $validate_parsed: expr) => {{
        let check_tokens = $crate::define_tokens_checker!($($tokens),*);
        $crate::expr_chain::group::GroupDeterminer::new_const(
            Some($group_type),
            check_tokens as *const (),
            $validate_parsed,
            $length
        )
    }};
    ($group_type: expr => $($tokens: expr),+ => $length: expr) => {
        $crate::define_group_determiner!(
            $group_type => $($tokens),+ => $length => true
        )
    };
}

impl GroupDeterminer {
    ///
    /// Constructs new `GroupDeterminer` using `const fn` (can be used to create const or static item).
    /// Example:
    /// ```
    /// extern crate join_impl;
    /// extern crate syn;
    ///
    /// use syn::Token;
    /// use syn::parse::ParseStream;
    /// use join_impl::expr_chain::group::GroupDeterminer;
    ///
    /// fn check_input(input: ParseStream) -> bool { input.peek(Token![,]) }
    ///
    /// fn main() {
    ///     let first_comma_determiner = GroupDeterminer::new_const(
    ///         None, // Because comma is not an action group
    ///         check_input as *const (),
    ///         false,
    ///         1
    ///     );
    /// }
    /// ```
    ///
    pub const fn new_const(
        group_type: Option<CommandGroup>,
        check_input_fn: *const (),
        validate_parsed: bool,
        length: usize,
    ) -> Self {
        GroupDeterminer {
            group_type,
            check_input_fn: FnPointer {
                raw: check_input_fn,
            },
            validate_parsed,
            length,
        }
    }

    ///
    /// Constructs new `GroupDeterminer`.
    /// Example:
    /// ```
    /// extern crate join_impl;
    /// extern crate syn;
    ///
    /// use syn::Token;
    /// use syn::parse::ParseStream;
    /// use join_impl::expr_chain::group::GroupDeterminer;
    ///
    /// fn check_input(input: ParseStream) -> bool { input.peek(Token![,]) }
    ///
    /// fn main() {
    ///     let first_comma_determiner = GroupDeterminer::new(
    ///         None, // Because comma is not an action group
    ///         check_input,
    ///         false,
    ///         1
    ///     );
    /// }
    /// ```
    ///
    pub fn new(
        group_type: impl Into<Option<CommandGroup>>,
        check_input_fn: DetermineGroupPredicate,
        validate_parsed: bool,
        length: usize,
    ) -> Self {
        GroupDeterminer {
            group_type: group_type.into(),
            check_input_fn: FnPointer {
                fun: check_input_fn,
            },
            validate_parsed,
            length,
        }
    }

    ///
    /// Returns type of group of `GroupDeterminer`.
    ///
    pub fn get_group_type(&self) -> Option<CommandGroup> {
        self.group_type
    }

    ///
    /// Checks if input next tokens are of self group type.
    ///  
    pub fn check_input(&self, input: ParseStream<'_>) -> bool {
        (unsafe { self.check_input_fn.fun })(input)
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

        assert_eq!(first_comma_determiner.get_group_type(), None);
        assert!(first_comma_determiner.check_parsed::<::syn::Expr>(::quote::quote! { , }));
        assert_eq!(first_comma_determiner.length(), 1);
    }

    #[test]
    fn it_creates_then_determiner() {
        fn check_then(input: ParseStream) -> bool {
            input.peek(::syn::Token![=>])
        }

        let then_determiner = GroupDeterminer::new(CommandGroup::Then, check_then, true, 2);

        assert_eq!(then_determiner.get_group_type(), Some(CommandGroup::Then));
        assert!(then_determiner.check_parsed::<::syn::Expr>(::quote::quote! { 23 }));
        assert_eq!(then_determiner.length(), 2);
    }
}

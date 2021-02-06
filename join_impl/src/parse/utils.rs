//!
//! Utils for `join!` macro parsing and code generation.
//!

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use std::fmt::Debug;
use syn::parse::{Parse, ParseStream};
use syn::{parse2, Expr};

use super::unit::{Unit, UnitResult};
use crate::chain::group::{ActionGroup, ApplicationType, Combinator, GroupDeterminer, MoveType};

///
/// Returns `true` if given stream is valid `Expr`.
///
pub fn is_valid_expr(input: TokenStream) -> bool {
    is_valid_stream::<Expr>(input)
}

///
/// Returns `true` if given stream can be parsed as `T`.
///
pub fn is_valid_stream<T: Parse>(input: TokenStream) -> bool {
    syn::parse2::<T>(input).is_ok()
}

///
/// Returns `true` if expr is {...}.
///
pub fn is_block_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::Block(_))
}

///
/// Parses input `ParseStream` until one of provided `GroupDeterminer`'s check will be valid or it reaches end.
///
pub fn parse_until<'a, T: Parse + Clone + Debug>(
    input: ParseStream<'_>,
    group_determiners: impl Iterator<Item = &'a GroupDeterminer> + Clone,
    deferred_determiner: &'a GroupDeterminer,
    wrapper_determiner: &'a GroupDeterminer,
    allow_empty_parsed: bool,
) -> UnitResult<T, ActionGroup> {
    let group_count = group_determiners.clone().count();
    let mut group_determiners = group_determiners.cycle();

    let mut tokens = TokenStream::new();
    let mut next = None;
    let mut deferred = false;
    let mut wrap = false;

    while !input.is_empty()
        && !{
            let group_determiners = &mut group_determiners;

            deferred = deferred_determiner.check_input(input);
            if deferred {
                deferred_determiner.erase_input(input)?;
            }

            let possible_group = group_determiners
                .take(group_count)
                .find(|group| group.check_input(input));
            possible_group
                .map(|group| {
                    tokens.is_empty() && allow_empty_parsed
                        || group.check_parsed::<T>(tokens.clone())
                })
                .unwrap_or(false)
                && {
                    next = possible_group;
                    true
                }
        }
    {
        let next: TokenTree = input.parse()?;
        next.to_tokens(&mut tokens);
    }

    //
    // Parses group determiner's tokens. (for ex. => -> |> etc.)
    //
    if let Some(group) = next {
        if let Some(group_type) = group.group_type() {
            let forked = input.fork();
            group.erase_input(&forked)?;

            wrap = wrapper_determiner.check_input(&forked);
            if wrap && group_type == Combinator::UNWRAP {
                return Err(input.error("Action can be either wrapped or unwrapped but not both"));
            } else if wrap && !group_type.can_be_wrapper() {
                return Err(input.error("This combinator can't be wrapper"));
            }
            if wrap {
                wrapper_determiner.erase_input(input)?;
            }
        }
        group.erase_input(input)?;
    }

    Ok(Unit {
        parsed: parse2(tokens)?,
        next: next.and_then(|group| {
            group.group_type().map(|group_type| {
                ActionGroup::new(
                    group_type,
                    if deferred {
                        ApplicationType::Deferred
                    } else {
                        ApplicationType::Instant
                    },
                    if wrap {
                        MoveType::Wrap
                    } else if group_type == Combinator::UNWRAP {
                        MoveType::Unwrap
                    } else {
                        MoveType::None
                    },
                )
            })
        }),
    })
}

///
/// Skips next item in `ParseStream`. Returns `true` in case of success.
///
pub fn skip(input: ParseStream<'_>) -> bool {
    input
        .step(|cursor| {
            if let Some((_lifetime, rest)) = cursor.lifetime() {
                Ok((true, rest))
            } else if let Some((_token, rest)) = cursor.token_tree() {
                Ok((true, rest))
            } else {
                Ok((false, *cursor))
            }
        })
        .unwrap()
}

//!
//! Utils for `join!` macro parsing and code generation.
//!

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{parse2, Expr};

use super::expr::{ApplyType, MoveType};
use super::unit::{Unit, UnitResult};
use super::{ActionGroup, CommandGroup, GroupDeterminer};

///
/// Returns true if given stream is valid `Expr`.
///
pub fn is_valid_expr(input: TokenStream) -> bool {
    is_valid_stream::<Expr>(input)
}

///
/// Returns true if given stream can be parsed as `T`.
///
pub fn is_valid_stream<T: Parse>(input: TokenStream) -> bool {
    syn::parse2::<T>(input).is_ok()
}

///
/// Returns true if expr is {...}.
///
pub fn is_block_expr(expr: &Expr) -> bool {
    match *expr {
        Expr::Block(_) => true,
        _ => false,
    }
}

///
/// Parses input `ParseStream` until one of provided `GroupDeterminer`'s check will be valid or it reaches end.
///
pub fn parse_until<'a, T: Parse>(
    input: ParseStream<'_>,
    group_determiners: impl Iterator<Item = &'a GroupDeterminer> + Clone,
    deferred_determiner: &'a GroupDeterminer,
    wrap_determiner: &'a GroupDeterminer,
    allow_empty_parsed: bool,
) -> UnitResult<T> {
    let group_count = group_determiners.clone().count();
    let mut group_determiners = group_determiners.cycle();

    let mut tokens = TokenStream::new();
    let mut next_group = None;
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
                    next_group = possible_group;
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
    if let Some(group) = next_group {
        if let Some(group_type) = group.get_group_type() {
            let forked = input.fork();

            group.erase_input(&forked)?;

            wrap = wrap_determiner.check_input(&forked);

            if wrap && group_type == CommandGroup::UNWRAP {
                return Err(input.error("Action can be either wraped or unwraped but not both"));
            } else if wrap && !group_type.can_be_wrapper() {
                return Err(input.error("This combinator can't be wrapper"));
            }

            if wrap {
                wrap_determiner.erase_input(input)?;
            }
        }
        group.erase_input(input)?;
    }

    Ok(Unit {
        parsed: parse2(tokens)?,
        next_group_type: next_group.and_then(|group| {
            group.get_group_type().map(|group_type| {
                ActionGroup::new(
                    group_type,
                    if deferred {
                        ApplyType::Deferred
                    } else {
                        ApplyType::Instant
                    },
                    if wrap {
                        MoveType::Wrap
                    } else if group_type == CommandGroup::UNWRAP {
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
/// Skips next item it `ParseStream`. Returns true in case of success.
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

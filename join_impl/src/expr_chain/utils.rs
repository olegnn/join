//!
//! Utils for `join!` macro parsing and code generation.
//!

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{parse2, Expr};

use super::chain::{Unit, UnitResult};
use super::GroupDeterminer;

///
/// Returns true if given is a valid `Expr`.
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
    input: ParseStream,
    group_determiners: impl Iterator<Item = &'a GroupDeterminer> + Clone,
    allow_empty_parsed: bool,
) -> UnitResult<T> {
    let mut tokens = TokenStream::new();
    let mut next_group = None;
    let (group_count, _) = group_determiners.size_hint();
    let mut group_determiners = group_determiners.cycle();

    while !input.is_empty()
        && !{
            let group_determiners = &mut group_determiners;
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
    let _ = next_group
        .as_ref()
        .and_then(|group| group.erase_input(input).ok());

    Ok(Unit {
        expr: parse2(tokens)?,
        next_group_type: next_group.and_then(GroupDeterminer::get_group_type),
    })
}

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

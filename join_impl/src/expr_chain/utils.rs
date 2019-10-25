//!
//! Utils for `join!` macro parsing and code generation.
//!

use proc_macro2::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::parse::ParseStream;
use syn::{parse2, Expr};

use super::chain::{Unit, UnitResult};
use super::GroupDeterminer;

///
/// Returns true if given stream can be parsed as `Expr`.
///
pub fn is_valid_expr(input: TokenStream) -> bool {
    syn::parse2::<Expr>(input).is_ok()
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
pub fn parse_until(input: ParseStream, group_determiners: &[GroupDeterminer]) -> UnitResult {
    let mut tokens = TokenStream::new();
    let mut next_group = None;

    while !input.is_empty() && {
        let possible_group = group_determiners
            .iter()
            .find(|group| group.check_input(&input));
        possible_group.is_none()
            || possible_group
                .map(|group| !group.check_parsed(tokens.clone()))
                .unwrap_or(false)
            || {
                next_group = possible_group;
                false
            }
    } {
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

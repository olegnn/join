//!
//! `ExprChain` definition and `ActionExprChain` implementation.
//!
//!

use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Expr::Let;
use syn::{Pat, PatIdent, Token};

use super::expr::{ActionExpr, InnerExpr};
use super::*;

///
/// Defines public  ExprChain` struct.
///
pub struct ExprChain<Member: Sized> {
    members: Vec<Member>,
    pat: Option<PatIdent>,
}

///
/// `ExprChain` of `ActionExpr`.
///
pub type ActionExprChain = ExprChain<ActionExpr>;

///
/// Generator of `ActionExprChain`.
///
pub struct ActionExprChainGenerator<'a> {
    #[cfg(not(feature = "static"))]
    group_determiners: &'a [GroupDeterminer],

    #[cfg(feature = "static")]
    group_determiners: ::std::sync::Arc<&'a [GroupDeterminer]>,
}

impl<'a> ActionExprChainGenerator<'a> {
    ///
    /// Creates new `ActionExprChainGenerator` with gived `GroupDeterminer`'s.
    ///
    #[cfg(not(feature = "static"))]
    pub fn new(group_determiners: &'a [GroupDeterminer]) -> Self {
        Self { group_determiners }
    }

    #[cfg(feature = "static")]
    pub fn new(group_determiners: ::std::sync::Arc<&'a [GroupDeterminer]>) -> Self {
        Self { group_determiners }
    }

    ///
    /// Parses input, fills chain with given expressions.
    ///
    pub fn parse_stream(&self, chain: &mut ActionExprChain, input: ParseStream) -> syn::Result<()> {
        let mut group_type = ActionGroup::Instant(CommandGroup::Initial);
        let mut member_index = 0;

        loop {
            let input = input;
            let (mut action_expr, next_group_type) = group_type.parse_stream(&self, input)?;

            if member_index == 0 {
                // Because first expr is `Initial`
                let exprs = action_expr
                    .extract_inner()
                    .expect("join: Failed to extract initial expr");
                if let Let(let_expr) = exprs
                    .first()
                    .expect("join: Failed to extract first expr of initial expr")
                {
                    if let Pat::Ident(pat) = &let_expr.pat {
                        chain.pat = Some(pat.clone());
                    } else {
                        return Err(input.error("Incorrect `let` pattern"));
                    }
                    action_expr = action_expr
                        .replace_inner(&mut vec![*let_expr.expr.clone()])
                        .expect("join: Failed to replace initial expr");
                }
                if action_expr
                    .extract_inner()
                    .expect("join: Failed to extract initial expr")
                    .first()
                    .expect("join: Failed to extract first expr of initial expr")
                    .into_token_stream()
                    .is_empty()
                {
                    return Err(input.error("Chain first member can't be empty"));
                }
            }

            chain.members.push(action_expr);

            if let Some(next_group_type) = next_group_type {
                group_type = next_group_type;
                member_index += 1;
            } else {
                break if chain.members.is_empty() {
                    Err(input.error("Chain can't be empty"))
                } else {
                    if chain
                        .members
                        .last()
                        .expect("join: Failed to extract last `ActionExpr` member")
                        .extract_inner()
                        .unwrap_or_else(Vec::new)
                        .last()
                        .map(|&expr| is_block_expr(expr))
                        .unwrap_or(false)
                    {
                        input.parse::<Option<Token![,]>>()?;
                    } else if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                    Ok(())
                };
            }
        }
    }

    ///
    /// Parses unit using self `GroupDeterminer`'s to determine unit end.
    ///
    pub fn parse_unit<T: Parse>(
        &self,
        input: ParseStream,
        allow_empty_parsed: bool,
    ) -> UnitResult<T> {
        parse_until(input, self.group_determiners.iter(), allow_empty_parsed)
    }
}

///
/// Implementation of `Chain` with `Member=ActionExpr`.
///
impl Chain for ActionExprChain
where
    Self: Sized,
{
    type Member = ActionExpr;

    fn get_members(&self) -> &[Self::Member] {
        &self.members
    }

    fn get_pat(&self) -> Option<&PatIdent> {
        self.pat.as_ref()
    }
}

///
/// Impl of Chain with `ActionExpr` member.
///
impl ActionExprChain
where
    Self: Sized,
{
    pub fn new<'a>(
        input: ParseStream,
        expr_chain_generator: &'a ActionExprChainGenerator,
    ) -> syn::Result<Option<Self>> {
        let mut expr_chain = ExprChain {
            members: Vec::new(),
            pat: None,
        };

        Ok(if input.is_empty() {
            None
        } else {
            expr_chain_generator.parse_stream(&mut expr_chain, input)?;
            Some(expr_chain)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::join::parse::DEFAULT_GROUP_DETERMINERS;
    use super::super::expr::{ErrActionExpr, ErrExpr, InitialExpr, ProcessActionExpr, ProcessExpr};
    use super::*;
    use ::quote::quote;
    use ::syn::parse::Parse;
    use ::syn::{parse2, parse_quote};

    impl Parse for ActionExprChain {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let gen = ActionExprChainGenerator::new(DEFAULT_GROUP_DETERMINERS);
            ActionExprChain::new(input, &gen)
                .transpose()
                .ok_or(input.error("Empty!"))?
        }
    }

    #[test]
    fn it_parses_chain_from_3_process_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 };
        assert_eq!(chain.get_members().len(), 3);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_parses_chain_from_3_process_expr_and_1_err_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 <| Ok(2) };
        assert_eq!(chain.get_members().len(), 4);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_parses_chain_from_initial_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) };
        assert_eq!(chain.get_members().len(), 1);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_attempts_to_parse_empty_chain() {
        assert!(parse2::<ActionExprChain>(quote! {}).is_err());
    }

    #[test]
    fn it_parses_chain_with_all_possible_command_expressions() {
        let chain: ActionExprChain = parse_quote! {
            Ok(2) => |_| Ok(3) |> |_| 4 <| Ok(5) => |v| Ok(v) <= |_| Ok(8) ?? |v| println!("{}", v) -> |v| v
        };

        let members = chain
            .get_members()
            .into_iter()
            .map(|v| v.clone())
            .collect::<Vec<_>>();

        assert_eq!(
            members[0],
            ActionExpr::Initial(InitialExpr(parse_quote! { Ok(2) }))
        );

        assert_eq!(
            members[1],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::AndThen(
                parse_quote! { |_| Ok(3) }
            )))
        );

        assert_eq!(
            members[2],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::Map(
                parse_quote! { |_| 4 }
            )))
        );

        assert_eq!(
            members[3],
            ActionExpr::Err(ErrActionExpr::Instant(ErrExpr::Or(parse_quote! { Ok(5) })))
        );

        assert_eq!(
            members[4],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::AndThen(
                parse_quote! { |v| Ok(v) }
            )))
        );

        assert_eq!(
            members[5],
            ActionExpr::Err(ErrActionExpr::Instant(ErrExpr::OrElse(
                parse_quote! { |_| Ok(8) }
            )))
        );

        assert_eq!(
            members[6],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::Inspect(
                parse_quote! { |v| println!("{}", v) }
            )))
        );

        assert_eq!(
            members[7],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::Then(
                parse_quote! { |v| v }
            )))
        );
    }

    #[test]
    fn it_parses_chain_with_all_possible_deferred_command_expressions() {
        let chain: ActionExprChain = parse_quote! {
            Ok(2) ~=> |_| Ok(3) ~|> |_| 4 ~<| Ok(5) ~=> |v| Ok(v) ~<= |_| Ok(8) ~?? |v| println!("{}", v) ~-> |v| v
        };

        let members = chain
            .get_members()
            .into_iter()
            .map(|v| v.clone())
            .collect::<Vec<_>>();

        assert_eq!(
            members[0],
            ActionExpr::Initial(InitialExpr(parse_quote! { Ok(2) }))
        );

        assert_eq!(
            members[1],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::AndThen(
                parse_quote! { |_| Ok(3) }
            )))
        );

        assert_eq!(
            members[2],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::Map(
                parse_quote! { |_| 4 }
            )))
        );

        assert_eq!(
            members[3],
            ActionExpr::Err(ErrActionExpr::Deferred(ErrExpr::Or(parse_quote! { Ok(5) })))
        );

        assert_eq!(
            members[4],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::AndThen(
                parse_quote! { |v| Ok(v) }
            )))
        );

        assert_eq!(
            members[5],
            ActionExpr::Err(ErrActionExpr::Deferred(ErrExpr::OrElse(
                parse_quote! { |_| Ok(8) }
            )))
        );

        assert_eq!(
            members[6],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::Inspect(
                parse_quote! { |v| println!("{}", v) }
            )))
        );

        assert_eq!(
            members[7],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::Then(
                parse_quote! { |v| v }
            )))
        );
    }
}

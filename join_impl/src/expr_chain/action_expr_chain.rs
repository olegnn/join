//!
//! `ExprChain` definition and `ActionExprChain` implementation.
//!
//!

use quote::ToTokens;
use std::cell::{Ref, RefCell};
use std::rc::Rc;
use syn::parse::ParseStream;
use syn::Expr::Let;
use syn::{Pat, Token};

use super::expr::ActionExpr;
use super::*;
use crate::instant_and_deferred_determiners;

///
/// Defines inner struct of `ExprChain`.
///
struct ExprChainInternal<Member, GroupDeterminer> {
    members: Vec<Member>,
    group_determiners: Vec<GroupDeterminer>,
    pat: Option<Pat>,
}

///
/// Defines public struct with inner `ExprChainInternal`.
///
pub struct ExprChain<Member: Sized, GroupDeterminer: Sized> {
    inner: Rc<RefCell<ExprChainInternal<Member, GroupDeterminer>>>,
}

///
/// ExprChain of `ActionExpr` with determiner `GroupDeterminer`.
///
pub type ActionExprChain = ExprChain<ActionExpr, GroupDeterminer>;

///
/// Implementation of `Chain` with `Member=ActionExpr`.
///
impl Chain for ActionExprChain
where
    Self: Sized,
{
    type Member = ActionExpr;

    fn get_members(&self) -> Ref<'_, Vec<Self::Member>> {
        Ref::map(RefCell::borrow(&self.inner), |inner| &inner.members)
    }

    fn get_pat(&self) -> Ref<'_, Option<Pat>> {
        Ref::map(RefCell::borrow(&self.inner), |inner| &inner.pat)
    }

    ///
    /// Parses input, fills self members with given expressions.
    ///
    fn generate_from_stream(&mut self, input: ParseStream) -> syn::Result<()> {
        let mut group_type = ActionGroup::Instant(CommandGroup::Initial);
        let mut is_block;
        let mut member_index = 0;

        while {
            let input = input;
            let Unit {
                mut expr,
                next_group_type,
            } = self.parse_unit(input)?;

            is_block = is_block_expr(&expr);

            if member_index == 0 {
                if let Let(let_expr) = expr {
                    self.inner.borrow_mut().pat = Some(let_expr.pat);
                    expr = *let_expr.expr;
                }
                if expr.clone().into_token_stream().is_empty() {
                    return Err(input.error("Chain first member can't be empty"));
                }
            }

            let member = group_type.map_to_action_expr(expr);
            self.inner.borrow_mut().members.push(member);

            member_index += 1;

            if let Some(next_group_type) = next_group_type {
                group_type = next_group_type;
                true
            } else {
                false
            }
        } {}

        let members = &RefCell::borrow(&self.inner).members;

        if members.is_empty() {
            Err(input.error("Chain can't be empty"))
        } else {
            if is_block {
                input.parse::<Option<Token![,]>>()?;
            } else if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
            Ok(())
        }
    }
}

///
/// Impl of Chain with `ActionExpr` member.
///
impl ActionExprChain
where
    Self: Sized,
{
    pub fn new(
        input: ParseStream,
        other_pattern_check: Box<dyn Fn(ParseStream<'_>) -> bool>,
    ) -> syn::Result<Option<Self>> {
        let mut group_determiners = instant_and_deferred_determiners! {
            Map => Token![|], Token![>]; 2,
            Then => Token![->]; 2,
            AndThen => Token![=>]; 2,
            Or => Token![<], Token![|]; 2,
            OrElse => Token![<=]; 2,
            Dot => Token![>], Token![.]; 2,
            MapErr => Token![!], Token![>]; 2,
            Inspect => Token![?], Token![>]; 2,
            Filter => Token![@], Token![>]; 2
        };

        group_determiners.extend(vec![
            GroupDeterminer::new(None, other_pattern_check, Some(Box::new(is_valid_expr)), 0),
            GroupDeterminer::new(
                None,
                Box::new(|input| input.peek(Token![,])),
                Some(Box::new(is_valid_expr)),
                0,
            ),
        ]);

        let inner = Rc::new(RefCell::new(ExprChainInternal {
            members: Vec::new(),
            group_determiners,
            pat: None,
        }));

        let mut expr_chain = ExprChain { inner };

        if input.is_empty() {
            Ok(None)
        } else {
            expr_chain.generate_from_stream(input)?;
            Ok(Some(expr_chain))
        }
    }

    ///
    /// Parses unit using self `GroupDeterminer`'s to determine unit end.
    ///
    fn parse_unit(&self, input: ParseStream) -> UnitResult {
        let expr_chain = RefCell::borrow(&self.inner);
        parse_until(input, &expr_chain.group_determiners[..])
    }
}

#[cfg(test)]
mod tests {
    use super::super::expr::{
        DefaultActionExpr, DefaultExpr, InitialExpr, ProcessActionExpr, ProcessExpr,
    };
    use super::*;
    use ::quote::quote;
    use ::syn::parse::Parse;

    impl Parse for ActionExprChain {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            ActionExprChain::new(input, Box::new(|_| false))
                .transpose()
                .ok_or(input.error("empty"))?
        }
    }

    #[test]
    fn it_parses_chain_from_3_process_expr() {
        let chain =
            ::syn::parse2::<ActionExprChain>(quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 })
                .unwrap();
        assert_eq!(chain.get_members().len(), 3);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_parses_chain_from_3_process_expr_and_1_default_expr() {
        let chain = ::syn::parse2::<ActionExprChain>(
            quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 <| Ok(2) },
        )
        .unwrap();
        assert_eq!(chain.get_members().len(), 4);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_parses_chain_from_initial_expr() {
        let chain = ::syn::parse2::<ActionExprChain>(quote! { Ok(2) }).unwrap();
        assert_eq!(chain.get_members().len(), 1);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_attempts_to_parse_empty_chain() {
        assert!(::syn::parse2::<ActionExprChain>(quote! {}).is_err());
    }

    #[test]
    fn it_parses_chain_with_all_possible_command_expressions() {
        let chain = ::syn::parse2::<ActionExprChain>(quote! {
            Ok(2) => |_| Ok(3) |> |_| 4 <| Ok(5) => |v| Ok(v) <= |_| Ok(8) ?> |v| println!("{}", v) -> |v| v
        })
        .unwrap();
        let members = chain.get_members();
        assert_eq!(
            members[0],
            ActionExpr::Initial(InitialExpr(syn::parse2(quote! { Ok(2) }).unwrap()))
        );

        assert_eq!(
            members[1],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::AndThen(
                syn::parse2(quote! { |_| Ok(3) }).unwrap()
            )))
        );

        assert_eq!(
            members[2],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::Map(
                syn::parse2(quote! { |_| 4 }).unwrap()
            )))
        );

        assert_eq!(
            members[3],
            ActionExpr::Default(DefaultActionExpr::Instant(DefaultExpr::Or(
                syn::parse2(quote! { Ok(5) }).unwrap()
            )))
        );

        assert_eq!(
            members[4],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::AndThen(
                syn::parse2(quote! { |v| Ok(v) }).unwrap()
            )))
        );

        assert_eq!(
            members[5],
            ActionExpr::Default(DefaultActionExpr::Instant(DefaultExpr::OrElse(
                syn::parse2(quote! { |_| Ok(8) }).unwrap()
            )))
        );

        assert_eq!(
            members[6],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::Inspect(
                syn::parse2(quote! { |v| println!("{}", v) }).unwrap()
            )))
        );

        assert_eq!(
            members[7],
            ActionExpr::Process(ProcessActionExpr::Instant(ProcessExpr::Then(
                syn::parse2(quote! { |v| v }).unwrap()
            )))
        );
    }

    #[test]
    fn it_parses_chain_with_all_possible_deferred_command_expressions() {
        let chain = ::syn::parse2::<ActionExprChain>(quote! {
            Ok(2) ~=> |_| Ok(3) ~|> |_| 4 ~<| Ok(5) ~=> |v| Ok(v) ~<= |_| Ok(8) ~?> |v| println!("{}", v) ~-> |v| v
        }).unwrap();

        let members = chain.get_members();

        assert_eq!(
            members[0],
            ActionExpr::Initial(InitialExpr(syn::parse2(quote! { Ok(2) }).unwrap()))
        );

        assert_eq!(
            members[1],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::AndThen(
                syn::parse2(quote! { |_| Ok(3) }).unwrap()
            )))
        );

        assert_eq!(
            members[2],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::Map(
                syn::parse2(quote! { |_| 4 }).unwrap()
            )))
        );

        assert_eq!(
            members[3],
            ActionExpr::Default(DefaultActionExpr::Deferred(DefaultExpr::Or(
                syn::parse2(quote! { Ok(5) }).unwrap()
            )))
        );

        assert_eq!(
            members[4],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::AndThen(
                syn::parse2(quote! { |v| Ok(v) }).unwrap()
            )))
        );

        assert_eq!(
            members[5],
            ActionExpr::Default(DefaultActionExpr::Deferred(DefaultExpr::OrElse(
                syn::parse2(quote! { |_| Ok(8) }).unwrap()
            )))
        );

        assert_eq!(
            members[6],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::Inspect(
                syn::parse2(quote! { |v| println!("{}", v) }).unwrap()
            )))
        );

        assert_eq!(
            members[7],
            ActionExpr::Process(ProcessActionExpr::Deferred(ProcessExpr::Then(
                syn::parse2(quote! { |v| v }).unwrap()
            )))
        );
    }
}

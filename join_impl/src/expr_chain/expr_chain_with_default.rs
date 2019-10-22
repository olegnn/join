//!
//! `ExprChain` definition and `ExprChainWithDefault` implementation.
//!
//!

use quote::ToTokens;
use std::cell::{Ref, RefCell};
use std::rc::Rc;
use syn::parse::ParseStream;
use syn::Expr::Let;
use syn::{Pat, Token};

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
/// Defines combination of possible `ProcessActionExpr` with possible `DefaultActionExpr`.
/// At least one element should be not `None`.
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessWithDefault(pub Option<ProcessActionExpr>, pub Option<DefaultActionExpr>);

///
/// ExprChain of `ProcessWithDefault` with determiner `GroupDeterminer`.
///
pub type ExprChainWithDefault = ExprChain<ProcessWithDefault, GroupDeterminer>;

///
/// Implementation of `Chain` with `Member=ProcessWithDefault`.
///
impl Chain for ExprChainWithDefault
where
    Self: Sized,
{
    type Member = ProcessWithDefault;

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
        let mut group_type = Some(ActionGroup::Instant(CommandGroup::Initial));
        let mut is_block;
        let mut member_index = 0;

        while {
            let input = input;
            let Unit {
                mut expr,
                next_group_type,
            } = self.parse_unit(input)?;

            if member_index == 0 {
                if let Let(let_expr) = expr {
                    self.inner.borrow_mut().pat = Some(let_expr.pat);
                    expr = *let_expr.expr;
                }
            }

            let (default_expr, next_group_type) = match next_group_type {
                Some(ActionGroup::Instant(CommandGroup::Or))
                | Some(ActionGroup::Deferred(CommandGroup::Or)) => {
                    let current_group_type = next_group_type.unwrap();
                    let Unit {
                        expr,
                        next_group_type,
                    } = self.parse_unit(input)?;
                    Some((
                        current_group_type.map_to_default_action_expr(expr),
                        next_group_type,
                    ))
                }
                Some(ActionGroup::Instant(CommandGroup::OrElse))
                | Some(ActionGroup::Deferred(CommandGroup::OrElse)) => {
                    let current_group_type = next_group_type.unwrap();
                    let Unit {
                        expr,
                        next_group_type,
                    } = self.parse_unit(input)?;
                    Some((
                        current_group_type.map_to_default_action_expr(expr),
                        next_group_type,
                    ))
                }
                _ => None,
            }
            .unwrap_or((None, next_group_type));

            let expr = group_type
                .and_then(|action_group_type| action_group_type.map_to_process_action_expr(expr));

            is_block = default_expr
                .as_ref()
                .map_or_else(
                    || expr.as_ref().map(|expr| expr.extract_expr().clone()),
                    |v| Some(v.extract_expr().clone()),
                )
                .map(|expr| is_block_expr(&expr))
                .unwrap_or(false);

            let member = ProcessWithDefault(expr, default_expr);

            if member_index == 0
                && match &member {
                    ProcessWithDefault(expr, _) => expr
                        .as_ref()
                        .unwrap()
                        .extract_expr()
                        .into_token_stream()
                        .is_empty(),
                }
            {
                return Err(input.error("Chain first member can't be empty"));
            }

            if member.0.is_some() || member.1.is_some() {
                self.inner.borrow_mut().members.push(member);
            }

            member_index += 1;

            group_type = next_group_type;

            group_type.is_some()
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

impl ExprChainWithDefault
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
    use super::super::expr::{DefaultExpr, ProcessExpr};
    use super::*;
    use ::quote::quote;
    use ::syn::parse::Parse;

    impl Parse for ExprChainWithDefault {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            ExprChainWithDefault::new(input, Box::new(|_| false))
                .transpose()
                .ok_or(input.error("empty"))?
        }
    }

    #[test]
    fn it_parses_chain_from_3_process_expr() {
        let chain =
            ::syn::parse2::<ExprChainWithDefault>(quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 })
                .unwrap();
        assert_eq!(chain.get_members().len(), 3);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_parses_chain_from_3_process_expr_and_1_default_expr() {
        let chain = ::syn::parse2::<ExprChainWithDefault>(
            quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 <| Ok(2) },
        )
        .unwrap();
        assert_eq!(chain.get_members().len(), 3);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_parses_chain_from_initial_expr() {
        let chain = ::syn::parse2::<ExprChainWithDefault>(quote! { Ok(2) }).unwrap();
        assert_eq!(chain.get_members().len(), 1);
        assert!(chain.get_pat().is_none());
    }

    #[test]
    fn it_attempts_to_parse_empty_chain() {
        assert!(::syn::parse2::<ExprChainWithDefault>(quote! {}).is_err());
    }

    #[test]
    fn it_parses_chain_with_all_possible_command_expressions() {
        let chain = ::syn::parse2::<ExprChainWithDefault>(quote! {
            Ok(2) => |_| Ok(3) |> |_| 4 <| Ok(5) => |v| Ok(v) <= |_| Ok(8) ?> |v| println!("{}", v) -> |v| v
        })
        .unwrap();
        let members = chain.get_members();
        assert_eq!(
            members[0],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::Initial(
                    syn::parse2(quote! { Ok(2) }).unwrap()
                ))),
                None
            )
        );
        assert_eq!(
            members[1],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::AndThen(
                    syn::parse2(quote! { |_| Ok(3) }).unwrap()
                ))),
                None
            )
        );

        assert_eq!(
            members[2],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::Map(
                    syn::parse2(quote! { |_| 4 }).unwrap()
                ))),
                Some(DefaultActionExpr::Instant(DefaultExpr::Or(
                    syn::parse2(quote! { Ok(5) }).unwrap()
                )))
            )
        );

        assert_eq!(
            members[3],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::AndThen(
                    syn::parse2(quote! { |v| Ok(v) }).unwrap()
                ))),
                Some(DefaultActionExpr::Instant(DefaultExpr::OrElse(
                    syn::parse2(quote! { |_| Ok(8) }).unwrap()
                )))
            )
        );

        assert_eq!(
            members[4],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::Inspect(
                    syn::parse2(quote! { |v| println!("{}", v) }).unwrap()
                ))),
                None
            )
        );

        assert_eq!(
            members[5],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::Then(
                    syn::parse2(quote! { |v| v }).unwrap()
                ))),
                None
            )
        );
    }

    #[test]
    fn it_parses_chain_with_all_possible_deferred_command_expressions() {
        let chain = ::syn::parse2::<ExprChainWithDefault>(quote! {
            Ok(2) ~=> |_| Ok(3) ~|> |_| 4 ~<| Ok(5) ~=> |v| Ok(v) ~<= |_| Ok(8) ~?> |v| println!("{}", v) ~-> |v| v
        }).unwrap();

        let members = chain.get_members();
        assert_eq!(
            members[0],
            ProcessWithDefault(
                Some(ProcessActionExpr::Instant(ProcessExpr::Initial(
                    syn::parse2(quote! { Ok(2) }).unwrap()
                ))),
                None
            )
        );
        assert_eq!(
            members[1],
            ProcessWithDefault(
                Some(ProcessActionExpr::Deferred(ProcessExpr::AndThen(
                    syn::parse2(quote! { |_| Ok(3) }).unwrap()
                ))),
                None
            )
        );

        assert_eq!(
            members[2],
            ProcessWithDefault(
                Some(ProcessActionExpr::Deferred(ProcessExpr::Map(
                    syn::parse2(quote! { |_| 4 }).unwrap()
                ))),
                Some(DefaultActionExpr::Deferred(DefaultExpr::Or(
                    syn::parse2(quote! { Ok(5) }).unwrap()
                )))
            )
        );

        assert_eq!(
            members[3],
            ProcessWithDefault(
                Some(ProcessActionExpr::Deferred(ProcessExpr::AndThen(
                    syn::parse2(quote! { |v| Ok(v) }).unwrap()
                ))),
                Some(DefaultActionExpr::Deferred(DefaultExpr::OrElse(
                    syn::parse2(quote! { |_| Ok(8) }).unwrap()
                )))
            )
        );

        assert_eq!(
            members[4],
            ProcessWithDefault(
                Some(ProcessActionExpr::Deferred(ProcessExpr::Inspect(
                    syn::parse2(quote! { |v| println!("{}", v) }).unwrap()
                ))),
                None
            )
        );

        assert_eq!(
            members[5],
            ProcessWithDefault(
                Some(ProcessActionExpr::Deferred(ProcessExpr::Then(
                    syn::parse2(quote! { |v| v }).unwrap()
                ))),
                None
            )
        );
    }
}

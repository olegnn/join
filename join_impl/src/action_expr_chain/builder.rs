//!
//! Builder of `Chain`<`ActionExpr`>.
//!

use quote::ToTokens;
use std::fmt::Debug;
use syn::parse::{Parse, ParseStream};
use syn::Expr::Let;
use syn::{Pat, Token};

use super::ActionExprChain;
use crate::chain::expr::InnerExpr;
use crate::chain::group::{ActionGroup, ApplicationType, Combinator, GroupDeterminer, MoveType};
use crate::chain::{Chain, ParseChain};
use crate::parse::unit::{ParseUnit, Unit, UnitResult};
use crate::parse::utils::{is_block_expr, parse_until};

pub struct ActionExprChainBuilder<'a> {
    #[cfg(not(feature = "static"))]
    group_determiners: &'a [GroupDeterminer],
    #[cfg(feature = "static")]
    group_determiners: ::std::sync::Arc<&'a [GroupDeterminer]>,

    deferred_determiner: &'a GroupDeterminer,
    wrapper_determiner: &'a GroupDeterminer,
}

impl<'a> ActionExprChainBuilder<'a> {
    ///
    /// Creates new `ActionExprChainBuilderOutput` with given `GroupDeterminer`'s.
    ///
    #[cfg(not(feature = "static"))]
    pub fn new(
        group_determiners: &'a [GroupDeterminer],
        deferred_determiner: &'a GroupDeterminer,
        wrapper_determiner: &'a GroupDeterminer,
    ) -> Self {
        Self {
            group_determiners,
            deferred_determiner,
            wrapper_determiner,
        }
    }

    ///
    /// Creates new `ActionExprChainBuilderOutput` with given `GroupDeterminer`'s.
    ///
    #[cfg(feature = "static")]
    pub fn new(
        group_determiners: ::std::sync::Arc<&'a [GroupDeterminer]>,
        deferred_determiner: &'a GroupDeterminer,
        wrapper_determiner: &'a GroupDeterminer,
    ) -> Self {
        Self {
            group_determiners,
            deferred_determiner,
            wrapper_determiner,
        }
    }
}

impl<'a> ParseChain<ActionExprChain> for ActionExprChainBuilder<'a> {
    ///
    /// Builds new `ActionExprChainBuilder` from input `ParseStream`.
    ///
    fn build_from_parse_stream(&self, input: ParseStream<'_>) -> syn::Result<ActionExprChain> {
        let mut chain = ActionExprChain::new(None, &[]);
        let mut action_group = ActionGroup::new(
            Combinator::Initial,
            ApplicationType::Instant,
            MoveType::None,
        );
        let mut member_idx = 0;
        let mut wrapper_count = 0isize;

        loop {
            let Unit {
                parsed: mut action_expr,
                next,
            } = action_group.parse_stream(self, input)?;

            if member_idx == 0 {
                // Because first expr is `Initial`
                let exprs = action_expr.inner_exprs().expect(
                    "join: Failed to extract initial expr. This's a bug, please report it.",
                );

                // If we have branch starting with `let` pattern,
                // check if it's correct and then, if it's, associate
                // it with given branch
                if let Let(let_expr) = exprs
                    .first()
                    .cloned()
                    .expect("join: Failed to extract first expr of initial expr. This's a bug, please report it.")
                {
                    if let Pat::Ident(pat) = &let_expr.pat {
                        chain.set_id(Some(pat.clone()));
                    } else {
                        return Err(input.error("Incorrect `let` pattern"));
                    }

                    action_expr = action_expr
                        .replace_inner_exprs(&[*let_expr.expr.clone()])
                        .expect("join: Failed to replace initial expr. This's a bug, please report it.");
                }

                if action_expr
                    .inner_exprs()
                    .expect("join: Failed to extract initial expr. This's a bug, please report it.")
                    .first()
                    .expect("join: Failed to extract first expr of initial expr. This's a bug, please report it.")
                    .into_token_stream()
                    .is_empty()
                {
                    return Err(input.error("Chain first expr can't be empty"));
                }
            }

            chain.append_member(action_expr);

            if let Some(next) = next {
                wrapper_count += match next.move_type {
                    MoveType::Wrap => 1,
                    MoveType::Unwrap => -1,
                    _ => 0,
                };
                if wrapper_count < 0 {
                    break Err(input.error("Unexpected `<<<`"));
                }

                action_group = next;
                member_idx += 1;
            } else {
                break if chain.is_empty() {
                    Err(input.error("Chain can't be empty"))
                } else {
                    if chain
                        .members()
                        .last()
                        .expect("join: Failed to extract last `ExprGroup<ActionExpr>` member. This's a bug, please report it.")
                        .inner_exprs()
                        .and_then(
                            |val|
                                val
                                    .last()
                        )
                        .map(is_block_expr)
                        .unwrap_or(false)
                    {
                        input.parse::<Option<Token![,]>>()?;
                    } else if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                    Ok(chain)
                };
            }
        }
    }
}

impl<'a> ParseUnit<ActionGroup> for ActionExprChainBuilder<'a> {
    ///
    /// Parses unit using self `GroupDeterminer`'s to determine unit end.
    ///
    fn parse_unit<T: Parse + Clone + Debug>(
        &self,
        input: ParseStream,
        allow_empty_parsed: bool,
    ) -> UnitResult<T, ActionGroup> {
        parse_until(
            input,
            self.group_determiners.iter(),
            self.deferred_determiner,
            self.wrapper_determiner,
            allow_empty_parsed,
        )
    }
}

#[cfg(test)]
mod tests {
    #[cfg(not(feature = "static"))]
    use super::super::super::join::parse::DEFAULT_GROUP_DETERMINERS;
    #[cfg(feature = "static")]
    use super::super::super::join::parse::DEFAULT_GROUP_DETERMINERS_STATIC as DEFAULT_GROUP_DETERMINERS;
    use super::super::super::join::parse::{DEFERRED_DETERMINER, WRAPPER_DETERMINER};
    use super::*;
    use crate::chain::expr::{ActionExpr, ErrExpr, InitialExpr, ProcessExpr};
    use crate::chain::group::{ApplicationType, Combinator, ExprGroup, MoveType};
    use ::quote::quote;
    use ::syn::parse::Parse;
    use ::syn::{parse2, parse_quote};
    use syn::parse::ParseStream;

    impl Parse for ActionExprChain {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let builder = ActionExprChainBuilder::new(
                DEFAULT_GROUP_DETERMINERS.clone(),
                DEFERRED_DETERMINER,
                WRAPPER_DETERMINER,
            );
            builder.build_from_parse_stream(input)
        }
    }

    #[test]
    fn it_parses_chain_from_3_process_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 };
        assert_eq!(chain.members().len(), 3);
        assert!(chain.id().is_none());
    }

    #[test]
    fn it_parses_chain_from_3_process_expr_and_1_err_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 <| Ok(2) };
        assert_eq!(chain.members().len(), 4);
        assert!(chain.id().is_none());
    }

    #[test]
    fn it_parses_chain_from_initial_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) };
        assert_eq!(chain.members().len(), 1);
        assert!(chain.id().is_none());
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

        let members = chain.members().to_vec();

        assert_eq!(
            members[0],
            ExprGroup::new(
                ActionExpr::Initial(InitialExpr::Single([parse_quote! { Ok(2) }])),
                ActionGroup::new(
                    Combinator::Initial,
                    ApplicationType::Instant,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[1],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::AndThen([parse_quote! { |_| Ok(3) }])),
                ActionGroup::new(
                    Combinator::AndThen,
                    ApplicationType::Instant,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[2],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Map([parse_quote! { |_| 4 }])),
                ActionGroup::new(Combinator::Map, ApplicationType::Instant, MoveType::None)
            )
        );

        assert_eq!(
            members[3],
            ExprGroup::new(
                ActionExpr::Err(ErrExpr::Or([parse_quote! { Ok(5) }])),
                ActionGroup::new(Combinator::Or, ApplicationType::Instant, MoveType::None)
            )
        );

        assert_eq!(
            members[4],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::AndThen([parse_quote! { |v| Ok(v) }])),
                ActionGroup::new(
                    Combinator::AndThen,
                    ApplicationType::Instant,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[5],
            ExprGroup::new(
                ActionExpr::Err(ErrExpr::OrElse([parse_quote! { |_| Ok(8) }])),
                ActionGroup::new(Combinator::OrElse, ApplicationType::Instant, MoveType::None)
            )
        );

        assert_eq!(
            members[6],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Inspect([
                    parse_quote! { |v| println!("{}", v) }
                ])),
                ActionGroup::new(
                    Combinator::Inspect,
                    ApplicationType::Instant,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[7],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Then([parse_quote! { |v| v }])),
                ActionGroup::new(Combinator::Then, ApplicationType::Instant, MoveType::None)
            )
        );
    }

    #[test]
    fn it_parses_chain_with_all_possible_deferred_command_expressions() {
        let chain: ActionExprChain = parse_quote! {
            Ok(2) ~=> |_| Ok(3) ~|> |_| 4 ~<| Ok(5) ~=> |v| Ok(v) ~<= |_| Ok(8) ~?? |v| println!("{}", v) ~-> |v| v
        };

        let members = chain.members().to_vec();

        assert_eq!(
            members[0],
            ExprGroup::new(
                ActionExpr::Initial(InitialExpr::Single([parse_quote! { Ok(2) }])),
                ActionGroup::new(
                    Combinator::Initial,
                    ApplicationType::Instant,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[1],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::AndThen([parse_quote! { |_| Ok(3) }])),
                ActionGroup::new(
                    Combinator::AndThen,
                    ApplicationType::Deferred,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[2],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Map([parse_quote! { |_| 4 }])),
                ActionGroup::new(Combinator::Map, ApplicationType::Deferred, MoveType::None)
            )
        );

        assert_eq!(
            members[3],
            ExprGroup::new(
                ActionExpr::Err(ErrExpr::Or([parse_quote! { Ok(5) }])),
                ActionGroup::new(Combinator::Or, ApplicationType::Deferred, MoveType::None)
            )
        );

        assert_eq!(
            members[4],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::AndThen([parse_quote! { |v| Ok(v) }])),
                ActionGroup::new(
                    Combinator::AndThen,
                    ApplicationType::Deferred,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[5],
            ExprGroup::new(
                ActionExpr::Err(ErrExpr::OrElse([parse_quote! { |_| Ok(8) }])),
                ActionGroup::new(
                    Combinator::OrElse,
                    ApplicationType::Deferred,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[6],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Inspect([
                    parse_quote! { |v| println!("{}", v) }
                ])),
                ActionGroup::new(
                    Combinator::Inspect,
                    ApplicationType::Deferred,
                    MoveType::None
                )
            )
        );

        assert_eq!(
            members[7],
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Then([parse_quote! { |v| v }])),
                ActionGroup::new(Combinator::Then, ApplicationType::Deferred, MoveType::None)
            )
        );
    }
}

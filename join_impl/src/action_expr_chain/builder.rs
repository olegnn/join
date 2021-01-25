use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Expr::Let;
use syn::{Pat, Token};

use super::ActionExprChain;
use crate::chain::expr::{ApplicationType, InnerExpr, MoveType};
use crate::chain::group::{ActionGroup, CommandGroup, GroupDeterminer};
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
        let mut group_type = ActionGroup::new(
            CommandGroup::Initial,
            ApplicationType::Instant,
            MoveType::None,
        );
        let mut member_index = 0;
        let mut wrapper_count = 0i16;

        loop {
            let Unit {
                parsed: mut action_expr,
                next,
            } = group_type.parse_stream(self, input)?;

            if member_index == 0 {
                // Because first expr is `Initial`
                let exprs = action_expr.get_inner().expect(
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
                        .replace_inner(&[*let_expr.expr.clone()])
                        .expect("join: Failed to replace initial expr. This's a bug, please report it.");
                }

                if action_expr
                    .get_inner()
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

                group_type = next;
                member_index += 1;
            } else {
                break if chain.is_empty() {
                    Err(input.error("Chain can't be empty"))
                } else {
                    if chain
                        .get_members()
                        .last()
                        .expect("join: Failed to extract last `ActionExpr` member. This's a bug, please report it.")
                        .get_inner()
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
    fn parse_unit<T: Parse>(
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
    use crate::chain::expr::{
        Action, ActionExpr, ApplicationType, ErrExpr, InitialExpr, MoveType, ProcessExpr,
    };
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
        assert_eq!(chain.get_members().len(), 3);
        assert!(chain.get_id().is_none());
    }

    #[test]
    fn it_parses_chain_from_3_process_expr_and_1_err_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) => |v| Ok(v + 1) |> |v| v + 2 <| Ok(2) };
        assert_eq!(chain.get_members().len(), 4);
        assert!(chain.get_id().is_none());
    }

    #[test]
    fn it_parses_chain_from_initial_expr() {
        let chain: ActionExprChain = parse_quote! { Ok(2) };
        assert_eq!(chain.get_members().len(), 1);
        assert!(chain.get_id().is_none());
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

        let members = chain.get_members().iter().cloned().collect::<Vec<_>>();

        assert_eq!(
            members[0],
            ActionExpr::Initial(Action::new(
                InitialExpr([parse_quote! { Ok(2) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[1],
            ActionExpr::Process(Action::new(
                ProcessExpr::AndThen([parse_quote! { |_| Ok(3) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[2],
            ActionExpr::Process(Action::new(
                ProcessExpr::Map([parse_quote! { |_| 4 }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[3],
            ActionExpr::Err(Action::new(
                ErrExpr::Or([parse_quote! { Ok(5) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[4],
            ActionExpr::Process(Action::new(
                ProcessExpr::AndThen([parse_quote! { |v| Ok(v) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[5],
            ActionExpr::Err(Action::new(
                ErrExpr::OrElse([parse_quote! { |_| Ok(8) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[6],
            ActionExpr::Process(Action::new(
                ProcessExpr::Inspect([parse_quote! { |v| println!("{}", v) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[7],
            ActionExpr::Process(Action::new(
                ProcessExpr::Then([parse_quote! { |v| v }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );
    }

    #[test]
    fn it_parses_chain_with_all_possible_deferred_command_expressions() {
        let chain: ActionExprChain = parse_quote! {
            Ok(2) ~=> |_| Ok(3) ~|> |_| 4 ~<| Ok(5) ~=> |v| Ok(v) ~<= |_| Ok(8) ~?? |v| println!("{}", v) ~-> |v| v
        };

        let members = chain.get_members().iter().cloned().collect::<Vec<_>>();

        assert_eq!(
            members[0],
            ActionExpr::Initial(Action::new(
                InitialExpr([parse_quote! { Ok(2) }]),
                ApplicationType::Instant,
                MoveType::None
            ))
        );

        assert_eq!(
            members[1],
            ActionExpr::Process(Action::new(
                ProcessExpr::AndThen([parse_quote! { |_| Ok(3) }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );

        assert_eq!(
            members[2],
            ActionExpr::Process(Action::new(
                ProcessExpr::Map([parse_quote! { |_| 4 }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );

        assert_eq!(
            members[3],
            ActionExpr::Err(Action::new(
                ErrExpr::Or([parse_quote! { Ok(5) }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );

        assert_eq!(
            members[4],
            ActionExpr::Process(Action::new(
                ProcessExpr::AndThen([parse_quote! { |v| Ok(v) }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );

        assert_eq!(
            members[5],
            ActionExpr::Err(Action::new(
                ErrExpr::OrElse([parse_quote! { |_| Ok(8) }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );

        assert_eq!(
            members[6],
            ActionExpr::Process(Action::new(
                ProcessExpr::Inspect([parse_quote! { |v| println!("{}", v) }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );

        assert_eq!(
            members[7],
            ActionExpr::Process(Action::new(
                ProcessExpr::Then([parse_quote! { |v| v }]),
                ApplicationType::Deferred,
                MoveType::None
            ))
        );
    }
}

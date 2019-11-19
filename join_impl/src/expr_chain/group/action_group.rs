//!
//! Definition of `ActionGroup`.
//!

use super::super::expr::{Action, ActionExpr, ApplyType, MoveType};
use super::super::ActionExprChainGenerator;
use super::super::{TransformParsed, Unit, UnitResult};
use super::command_group::CommandGroup;
use quote::quote;
use syn::parse::ParseStream;

///
/// `CommandGroup` with configuration. (`ApplyType` and `MoveType`).
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ActionGroup {
    pub group: CommandGroup,
    pub apply_type: ApplyType,
    pub move_type: MoveType,
}

impl ActionGroup {
    pub fn new(group: CommandGroup, apply_type: ApplyType, move_type: MoveType) -> Self {
        Self {
            group,
            apply_type,
            move_type,
        }
    }

    ///
    /// Parses `ParseStream` as `ActionExpr` using given `ActionExprChainGenerator`.
    ///
    pub fn parse_stream(
        self,
        action_expr_chain: &ActionExprChainGenerator,
        input: ParseStream<'_>,
    ) -> UnitResult<ActionExpr> {
        let Self {
            group,
            apply_type,
            move_type,
        } = self;

        if move_type == MoveType::Wrap {
            if group.is_process_expr() {
                group
                    .to_process_expr(quote! { |__v| __v })
                    .and_then(|value| {
                        value.ok_or_else(|| input.error("This combinator can't be wrapper"))
                    })
                    .and_then(|parsed| {
                        let Unit {
                            next_group_type, ..
                        } = group.parse_empty_expr(action_expr_chain, input).unwrap()?;
                        Ok(Unit {
                            parsed: ActionExpr::Process(Action::new(parsed, apply_type, move_type)),
                            next_group_type,
                        })
                    })
            } else if group.is_err_expr() {
                group
                    .to_err_expr(quote! { |__v| __v })
                    .and_then(|value| {
                        value.ok_or_else(|| input.error("This combinator can't be wrapper"))
                    })
                    .and_then(|parsed| {
                        let Unit {
                            next_group_type, ..
                        } = group.parse_empty_expr(action_expr_chain, input).unwrap()?;
                        Ok(Unit {
                            parsed: ActionExpr::Err(Action::new(parsed, apply_type, move_type)),
                            next_group_type,
                        })
                    })
            } else {
                unreachable!()
            }
        } else if group.is_process_expr() {
            group.parse_process_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (process). This is a bug, please report it.")
                .transform_parsed(|parsed| ActionExpr::Process(Action::new(parsed, apply_type, move_type)))
        } else if group.is_err_expr() {
            group.parse_err_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (err). This is a bug, please report it.")
                .transform_parsed(|parsed| ActionExpr::Err(Action::new(parsed, apply_type, move_type)))
        } else if group.is_initial_expr() {
            group.parse_initial_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (initial). This is a bug, please report it.")
                .transform_parsed(|parsed| ActionExpr::Initial(Action::new(parsed, apply_type, move_type)))
        } else {
            unreachable!()
        }
    }
}

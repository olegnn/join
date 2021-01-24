//!
//! Definition of `ActionGroup`.
//!

use super::command_group::CommandGroup;
use crate::chain::expr::{Action, ActionExpr, ApplicationType, MoveType};
use crate::common::MapOver;
use crate::parse::unit::ParseUnit;
use crate::parse::unit::{Unit, UnitResult};
use quote::quote;
use syn::parse::ParseStream;

///
/// `CommandGroup` with configuration. (`ApplicationType` and `MoveType`).
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ActionGroup {
    pub group: CommandGroup,
    pub application_type: ApplicationType,
    pub move_type: MoveType,
}

impl ActionGroup {
    ///
    /// Creates new `ActionGroup` with provided configuration.
    ///
    pub fn new(
        group: CommandGroup,
        application_type: ApplicationType,
        move_type: MoveType,
    ) -> Self {
        Self {
            group,
            application_type,
            move_type,
        }
    }

    ///
    /// Parses `ParseStream` as `ActionExpr` using given `ParseUnit`.
    ///
    pub fn parse_stream(
        self,
        action_expr_chain: &impl ParseUnit<ActionGroup>,
        input: ParseStream<'_>,
    ) -> UnitResult<ActionExpr, ActionGroup> {
        let Self {
            group,
            application_type,
            move_type,
        } = self;

        if move_type == MoveType::Wrap {
            let return_value = quote! { |__value| __value };
            if group.is_process_expr() {
                group
                    .to_process_expr(return_value)
                    .and_then(|value| {
                        value.ok_or_else(|| input.error("This combinator can't be wrapper!"))
                    })
                    .and_then(|parsed| {
                        let Unit { next, .. } =
                            group.parse_empty_expr(action_expr_chain, input).unwrap()?;

                        Ok(Unit {
                            parsed: ActionExpr::Process(Action::new(
                                parsed,
                                application_type,
                                move_type,
                            )),
                            next,
                        })
                    })
            } else if group.is_err_expr() {
                group
                    .to_err_expr(return_value)
                    .and_then(|value| {
                        value.ok_or_else(|| input.error("This combinator can't be wrapper!"))
                    })
                    .and_then(|parsed| {
                        let Unit { next, .. } =
                            group.parse_empty_expr(action_expr_chain, input).unwrap()?;

                        Ok(Unit {
                            parsed: ActionExpr::Err(Action::new(
                                parsed,
                                application_type,
                                move_type,
                            )),
                            next,
                        })
                    })
            } else {
                unreachable!(
                    "join: Initial group can't be wrapper. This's a bug, please report it."
                )
            }
        } else if group.is_process_expr() {
            group.parse_process_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (process). This is a bug, please report it.")
                .map_over(|parsed| ActionExpr::Process(Action::new(parsed, application_type, move_type)))
        } else if group.is_err_expr() {
            group.parse_err_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (err). This is a bug, please report it.")
                .map_over(|parsed| ActionExpr::Err(Action::new(parsed, application_type, move_type)))
        } else if group.is_initial_expr() {
            group.parse_initial_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (initial). This is a bug, please report it.")
                .map_over(|parsed| ActionExpr::Initial(Action::new(parsed, application_type, move_type)))
        } else {
            unreachable!()
        }
    }
}

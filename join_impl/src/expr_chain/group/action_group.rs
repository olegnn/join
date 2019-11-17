//!
//! Definition of `ActionGroup`.
//!

use super::super::expr::{ActionExpr, ErrActionExpr, ProcessActionExpr};
use super::super::ActionExprChainGenerator;
use super::super::Unit;
use super::command_group::CommandGroup;
use syn::parse::ParseStream;

///
/// `ActionGroup` represents two possible types of action: `Instant` and `Deferred`.
/// `Instant` and `Deferred` types could be any of type `CommandGroup`.
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ActionGroup {
    Instant(CommandGroup),
    Deferred(CommandGroup),
}

impl ActionGroup {
    ///
    /// Parses `ParseStream` as `ActionExpr` using given `ActionExprChainGenerator`.
    ///
    pub fn parse_stream(
        self,
        action_expr_chain: &ActionExprChainGenerator,
        input: ParseStream<'_>,
    ) -> syn::Result<(ActionExpr, Option<ActionGroup>)> {
        let command_group = match self {
            ActionGroup::Instant(command_group) => command_group,
            ActionGroup::Deferred(command_group) => command_group,
        };

        Ok(if command_group.is_process_expr() {
            let Unit { parsed, next_group_type } = command_group.parse_process_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (process). This is a bug, please report it.")?;
            let deferred_instant = match self {
                ActionGroup::Instant(_) => ProcessActionExpr::Instant,
                ActionGroup::Deferred(_) => ProcessActionExpr::Deferred,
            };
            (
                ActionExpr::Process(deferred_instant(parsed)),
                next_group_type,
            )
        } else if command_group.is_err_expr() {
            let Unit { parsed, next_group_type } = command_group.parse_err_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (err). This is a bug, please report it.")?;
            let deferred_instant = match self {
                ActionGroup::Instant(_) => ErrActionExpr::Instant,
                ActionGroup::Deferred(_) => ErrActionExpr::Deferred,
            };
            (ActionExpr::Err(deferred_instant(parsed)), next_group_type)
        } else if command_group.is_initial_expr() {
            let Unit { parsed, next_group_type } = command_group.parse_initial_expr(action_expr_chain, input).expect("join: Unexpected expression type in from_parse_stream (initial). This is a bug, please report it.")?;
            (ActionExpr::Initial(parsed), next_group_type)
        } else {
            unreachable!()
        })
    }
}

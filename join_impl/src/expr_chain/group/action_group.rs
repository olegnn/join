//!
//! Definition of `ActionGroup`.
//!

use syn::Expr;

use super::super::expr::{ActionExpr, DefaultActionExpr, InitialExpr, ProcessActionExpr};
use super::command_group::CommandGroup;

///
/// `ActionGroup` represents two possible types of action: `Instant` and `Deferred`.
/// `Instant` and `Deferred` type could be any of type `CommandGroup`.
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ActionGroup {
    Instant(CommandGroup),
    Deferred(CommandGroup),
}

impl ActionGroup {
    ///
    /// Maps given expr to `ActionExpr`. In any casee will return one of `ActionExpr` variants.
    ///
    ///
    pub fn map_to_action_expr(self, expr: Expr) -> ActionExpr {
        let command_group = match self {
            ActionGroup::Instant(command_group) => command_group,
            ActionGroup::Deferred(command_group) => command_group,
        };

        if command_group.is_default_expr() {
            ActionExpr::Default(
                self
                    .map_to_default_action_expr(expr)
                    .expect("join: Unexpected expression type in map_to_action_expr (default). This is a bug, please report it.")
            )
        } else if command_group.is_process_expr() {
            ActionExpr::Process(
                self
                    .map_to_process_action_expr(expr)
                    .expect("join: Unexpected expression type in map_to_action_expr (process). This is a bug, please report it.")
            )
        } else if command_group.is_initial_expr() {
            ActionExpr::Initial(
                self.map_to_initial_expr(expr)
                    .expect("join: Unexpected expression type in map_to_action_expr (initial). This is a bug, please report it.")
            )
        } else {
            unreachable!()
        }
    }

    ///
    /// Attempts to map expr to `ProcessActionExpr` with self type.
    /// Returns None if self isn't of `ProcessExpr` type
    ///
    pub fn map_to_process_action_expr(self, expr: Expr) -> Option<ProcessActionExpr> {
        match self {
            ActionGroup::Instant(group) => group
                .map_to_process_expr(expr)
                .map(ProcessActionExpr::Instant),
            ActionGroup::Deferred(group) => group
                .map_to_process_expr(expr)
                .map(ProcessActionExpr::Deferred),
        }
    }

    ///
    /// Attempts to map expr to `ProcessActionExpr` with self type.
    /// Returns None if self isn't of `ProcessExpr` type
    ///
    pub fn map_to_initial_expr(self, expr: Expr) -> Option<InitialExpr> {
        match self {
            ActionGroup::Instant(group) => group.map_to_initial_expr(expr),
            _ => unreachable!(),
        }
    }

    ///
    /// Attempts to map expr to `DefaultActionExpr` with self type.
    /// Returns None if self isn't of `DefaultExpr` type
    ///
    pub fn map_to_default_action_expr(self, expr: Expr) -> Option<DefaultActionExpr> {
        match self {
            ActionGroup::Instant(group) => group
                .map_to_default_expr(expr)
                .map(DefaultActionExpr::Instant),
            ActionGroup::Deferred(group) => group
                .map_to_default_expr(expr)
                .map(DefaultActionExpr::Deferred),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr_chain::expr::{DefaultExpr, ProcessExpr};

    #[test]
    fn it_tests_action_group_impl_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();
        let group = CommandGroup::Then;

        let action_group = ActionGroup::Instant(group.clone());

        assert_eq!(
            action_group
                .map_to_process_action_expr(expr.clone())
                .unwrap(),
            ProcessActionExpr::Instant(ProcessExpr::Then(expr.clone()))
        );

        assert!(action_group
            .map_to_default_action_expr(expr.clone())
            .is_none());

        let action_group = ActionGroup::Deferred(group.clone());

        assert_eq!(
            action_group
                .map_to_process_action_expr(expr.clone())
                .unwrap(),
            ProcessActionExpr::Deferred(ProcessExpr::Then(expr.clone()))
        );
        assert!(action_group
            .map_to_default_action_expr(expr.clone())
            .is_none());
    }

    #[test]
    fn it_tests_action_group_impl_default_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();
        let group = CommandGroup::Or;

        let action_group = ActionGroup::Instant(group.clone());

        assert_eq!(
            action_group
                .map_to_default_action_expr(expr.clone())
                .unwrap(),
            DefaultActionExpr::Instant(DefaultExpr::Or(expr.clone()))
        );
        assert_eq!(action_group.map_to_process_action_expr(expr.clone()), None);

        let action_group = ActionGroup::Deferred(group.clone());

        assert_eq!(
            action_group
                .map_to_default_action_expr(expr.clone())
                .unwrap(),
            DefaultActionExpr::Deferred(DefaultExpr::Or(expr.clone()))
        );
        assert_eq!(action_group.map_to_process_action_expr(expr.clone()), None);

        let group = CommandGroup::OrElse;

        let action_group = ActionGroup::Instant(group.clone());

        assert_eq!(
            action_group
                .map_to_default_action_expr(expr.clone())
                .unwrap(),
            DefaultActionExpr::Instant(DefaultExpr::OrElse(expr.clone()))
        );
        assert_eq!(action_group.map_to_process_action_expr(expr.clone()), None);

        let action_group = ActionGroup::Deferred(group.clone());

        assert_eq!(
            action_group
                .map_to_default_action_expr(expr.clone())
                .unwrap(),
            DefaultActionExpr::Deferred(DefaultExpr::OrElse(expr.clone()))
        );
        assert_eq!(action_group.map_to_process_action_expr(expr.clone()), None);
    }
}

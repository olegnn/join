//!
//! `ActionGroup` represents two possible types of action: `Instant` and `Deferred`.
//! Any type could be any of `CommandGroup`.
//!

use syn::Expr;

use super::super::expr::{DefaultActionExpr, ProcessActionExpr};
use super::command_group::CommandGroup;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ActionGroup {
    Instant(CommandGroup),
    Deferred(CommandGroup),
}

impl ActionGroup {
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

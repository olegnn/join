//!
//! Definition of `CommandGroup`.
//!
//!
use syn::Expr;

use super::super::expr::{DefaultExpr, InitialExpr, ProcessExpr};

///
/// `CommandGroup` is an enum of all possible `ProcessExpr` and `DefaultExpr` operations.
/// Used to express group which was found in input `ParseStream`
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CommandGroup {
    /// [ProcessExpr::Map]
    Map,
    /// [ProcessExpr::Dot]
    Dot,
    /// [ProcessExpr::Filter]
    Filter,
    /// [ProcessExpr::Inspect]
    Inspect,
    /// [ProcessExpr::Then]
    Then,
    /// [ProcessExpr::AndThen]
    AndThen,
    /// [DefaultExpr::Or]
    Or,
    /// [DefaultExpr::OrElse]
    OrElse,
    /// [DefaultExpr::MapErr]
    MapErr,
    /// [InitialExpr]
    Initial,
}

impl CommandGroup {
    ///
    /// Returns true if self command group is of `ProcessExpr` type.
    ///
    pub fn is_process_expr(&self) -> bool {
        !self.is_default_expr() && !self.is_initial_expr()
    }

    ///
    /// Returns true if self command group is of `InitialExpr` type.
    ///
    pub fn is_initial_expr(&self) -> bool {
        match self {
            CommandGroup::Initial => true,
            _ => false,
        }
    }

    ///
    /// Attempts to map expr to `InitialExpr` with self type.
    /// Returns None if self isn't of `InitialExpr` type
    ///
    pub fn map_to_initial_expr(&self, expr: Expr) -> Option<InitialExpr> {
        match self {
            CommandGroup::Initial => Some(InitialExpr(expr)),
            _ => None,
        }
    }

    ///
    /// Returns true if self command group is of `DefaultExpr` type.
    ///
    pub fn is_default_expr(&self) -> bool {
        match self {
            CommandGroup::Map => false,
            CommandGroup::AndThen => false,
            CommandGroup::Filter => false,
            CommandGroup::Dot => false,
            CommandGroup::Then => false,
            CommandGroup::Inspect => false,
            CommandGroup::Or => true,
            CommandGroup::OrElse => true,
            CommandGroup::MapErr => true,
            CommandGroup::Initial => false,
        }
    }

    ///
    /// Attempts to map expr to `ProcessExpr` with self type.
    /// Returns None if self isn't of `ProcessExpr` type
    ///
    pub fn map_to_process_expr(self, expr: Expr) -> Option<ProcessExpr> {
        match self {
            CommandGroup::Map => Some(ProcessExpr::Map(expr)),
            CommandGroup::AndThen => Some(ProcessExpr::AndThen(expr)),
            CommandGroup::Filter => Some(ProcessExpr::Filter(expr)),
            CommandGroup::Dot => Some(ProcessExpr::Dot(expr)),
            CommandGroup::Then => Some(ProcessExpr::Then(expr)),
            CommandGroup::Inspect => Some(ProcessExpr::Inspect(expr)),
            _ => None,
        }
    }

    ///
    /// Attempts to map expr to `DefaultExpr` with self type.
    /// Returns None if self isn't of `DefaultExpr` type
    ///
    pub fn map_to_default_expr(self, expr: Expr) -> Option<DefaultExpr> {
        match self {
            CommandGroup::Or => Some(DefaultExpr::Or(expr)),
            CommandGroup::OrElse => Some(DefaultExpr::OrElse(expr)),
            CommandGroup::MapErr => Some(DefaultExpr::MapErr(expr)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_tests_command_group_impl_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        let groups_vec: Vec<(CommandGroup, Box<dyn Fn(Expr) -> ProcessExpr>)> = vec![
            (CommandGroup::Map, Box::new(ProcessExpr::Map)),
            (CommandGroup::Dot, Box::new(ProcessExpr::Dot)),
            (CommandGroup::Filter, Box::new(ProcessExpr::Filter)),
            (CommandGroup::Inspect, Box::new(ProcessExpr::Inspect)),
            (CommandGroup::Then, Box::new(ProcessExpr::Then)),
            (CommandGroup::AndThen, Box::new(ProcessExpr::AndThen)),
        ];

        for (command_group, process_expr) in groups_vec.into_iter() {
            assert_eq!(
                command_group.map_to_process_expr(expr.clone()).unwrap(),
                process_expr(expr.clone())
            );
            assert_eq!(command_group.map_to_default_expr(expr.clone()), None);
        }
    }

    #[test]
    fn it_tests_command_group_impl_initial_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        assert_eq!(
            CommandGroup::Initial
                .map_to_initial_expr(expr.clone())
                .unwrap(),
            InitialExpr(expr.clone()),
        );
    }

    #[test]
    fn it_tests_command_group_impl_default_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        let groups_vec: Vec<(CommandGroup, Box<dyn Fn(Expr) -> DefaultExpr>)> = vec![
            (CommandGroup::Or, Box::new(DefaultExpr::Or)),
            (CommandGroup::OrElse, Box::new(DefaultExpr::OrElse)),
            (CommandGroup::MapErr, Box::new(DefaultExpr::MapErr)),
        ];

        for (command_group, process_expr) in groups_vec.into_iter() {
            assert_eq!(
                command_group.map_to_default_expr(expr.clone()).unwrap(),
                process_expr(expr.clone())
            );
            assert_eq!(command_group.map_to_process_expr(expr.clone()), None);
        }
    }
}

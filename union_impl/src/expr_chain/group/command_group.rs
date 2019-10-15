//!
//! `CommandGroup` is an enum of all possible `ProcessExpr` and `DefaultExpr` operations.
//! Used to express group which was found in input `ParseStream`
//!

use syn::Expr;

use super::super::expr::{DefaultExpr, ProcessExpr};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CommandGroup {
    /// [ProcessExpr::Map]
    Map,
    /// [ProcessExpr::Dot]
    Dot,
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
    /// [ProcessExpr::MapErr]
    MapErr,
    /// [ProcessExpr::Initial]
    Initial,
}

impl CommandGroup {
    ///
    /// Attempts to map expr to `ProcessExpr` with self type.
    /// Returns None if self isn't of `ProcessExpr` type
    ///
    pub fn map_to_process_expr(self, expr: Expr) -> Option<ProcessExpr> {
        match self {
            CommandGroup::Map => Some(ProcessExpr::Map(expr)),
            CommandGroup::AndThen => Some(ProcessExpr::AndThen(expr)),
            CommandGroup::Dot => Some(ProcessExpr::Dot(expr)),
            CommandGroup::Then => Some(ProcessExpr::Then(expr)),
            CommandGroup::MapErr => Some(ProcessExpr::MapErr(expr)),
            CommandGroup::Initial => Some(ProcessExpr::Initial(expr)),
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
            (CommandGroup::Inspect, Box::new(ProcessExpr::Inspect)),
            (CommandGroup::Then, Box::new(ProcessExpr::Then)),
            (CommandGroup::AndThen, Box::new(ProcessExpr::AndThen)),
            (CommandGroup::MapErr, Box::new(ProcessExpr::MapErr)),
            (CommandGroup::Initial, Box::new(ProcessExpr::Initial)),
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
    fn it_tests_command_group_impl_default_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        let groups_vec: Vec<(CommandGroup, Box<dyn Fn(Expr) -> DefaultExpr>)> = vec![
            (CommandGroup::Or, Box::new(DefaultExpr::Or)),
            (CommandGroup::OrElse, Box::new(DefaultExpr::OrElse)),
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

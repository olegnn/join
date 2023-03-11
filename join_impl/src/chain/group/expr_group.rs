//!
//! Definition of `ExprGroup<ActionExpr>` .
//!
use crate::chain::expr::{ActionExpr, InnerExpr};
use syn::Expr;

use super::*;

///
/// `Action` with `Expr` of type either `Process`, `Initial` or `Err`.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExprGroup<E: InnerExpr> {
    expr: E,
    action: ActionGroup,
}

impl ExprGroup<ActionExpr> {
    ///
    /// Creates new `ExprGroup<ActionExpr>` with given expr and config.
    ///
    pub fn new(expr: ActionExpr, action: ActionGroup) -> Self {
        Self { expr, action }
    }

    ///
    /// Returns inner expression.
    ///
    pub fn expr(&self) -> &ActionExpr {
        &self.expr
    }

    ///
    /// Returns `MoveType` of inner expr.
    ///
    pub fn move_type(&self) -> &MoveType {
        &self.action.move_type
    }

    ///
    /// Returns `ApplicationType` of inner expr.
    ///
    pub fn application_type(&self) -> &ApplicationType {
        &self.action.application_type
    }

    crate::parse_n_or_empty_unit_fn! {
        parse_empty_unit => [0, true],
        parse_single_unit => [1, false],
        parse_single_or_empty_unit => [1, true],
        parse_double_unit => [2, false],
        parse_double_or_empty_unit => [2, true],
        parse_triple_unit => [3, false],
        parse_triple_or_empty_unit => [3, true],
        parse_quatro_unit => [4, false],
        parse_quatro_or_empty_unit => [4, true]
    }
}

impl<E: InnerExpr> InnerExpr for ExprGroup<E> {
    fn replace_inner_exprs(mut self, exprs: &[Expr]) -> Option<Self> {
        self.expr = self.expr.replace_inner_exprs(exprs)?;
        Some(self)
    }

    fn inner_exprs(&self) -> Option<&[Expr]> {
        self.expr.inner_exprs()
    }

    fn is_replaceable(&self) -> bool {
        self.expr.is_replaceable()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chain::{expr::*, group::Combinator};
    use syn::{parse_quote, Expr};

    #[test]
    fn it_tests_inner_expr_trait_impl_for_action() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_expr: Expr = parse_quote! { |v| v + 2 };

        for action_expr in vec![
            ExprGroup::new(
                ActionExpr::Process(ProcessExpr::Then([expr.clone()])),
                ActionGroup::new(Combinator::Then, ApplicationType::Instant, MoveType::None),
            ),
            ExprGroup::new(
                ActionExpr::Err(ErrExpr::Or([expr.clone()])),
                ActionGroup::new(Combinator::Or, ApplicationType::Instant, MoveType::None),
            ),
            ExprGroup::new(
                ActionExpr::Initial(InitialExpr::Single([expr.clone()])),
                ActionGroup::new(
                    Combinator::Initial,
                    ApplicationType::Instant,
                    MoveType::None,
                ),
            ),
        ]
        .into_iter()
        {
            assert_eq!(action_expr.inner_exprs().clone(), Some(&[expr.clone()][..]));
            assert_eq!(
                action_expr
                    .replace_inner_exprs(&[replace_expr.clone()][..])
                    .unwrap()
                    .inner_exprs(),
                Some(&[replace_expr.clone()][..])
            )
        }
    }
}

//!
//! Definition of `ActionExpr`.
//!

use syn::Expr;

use super::*;

///
/// `Action` with `Expr` of type either `Process`, `Initial` or `Err`.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ActionExpr {
    Process(Action<ProcessExpr>),
    Initial(Action<InitialExpr>),
    Err(Action<ErrExpr>),
}

impl ActionExpr {
    ///
    /// Returns `MoveType` of inner expr.
    ///
    pub fn get_move_type(&self) -> &MoveType {
        match self {
            Self::Process(expr) => &expr.move_type,
            Self::Err(expr) => &expr.move_type,
            Self::Initial(expr) => &expr.move_type,
        }
    }

    ///
    /// Returns `ApplicationType` of inner expr.
    ///
    pub fn get_application_type(&self) -> &ApplicationType {
        match self {
            Self::Process(expr) => &expr.application_type,
            Self::Err(expr) => &expr.application_type,
            Self::Initial(expr) => &expr.application_type,
        }
    }
}

///
/// Describes `expr` with configuration.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Action<E: InnerExpr> {
    pub expr: E,
    pub application_type: ApplicationType,
    pub move_type: MoveType,
}

impl<E: InnerExpr> Action<E> {
    ///
    /// Creates new `Action` with given expr and config.
    ///
    pub fn new(expr: E, application_type: ApplicationType, move_type: MoveType) -> Self {
        Self {
            expr,
            application_type,
            move_type,
        }
    }
}

impl<E: InnerExpr> InnerExpr for Action<E> {
    fn replace_inner_exprs(mut self, exprs: &[Expr]) -> Option<Self> {
        self.expr = self.expr.replace_inner_exprs(exprs)?;
        Some(self)
    }

    fn get_inner_exprs(&self) -> Option<&[Expr]> {
        self.expr.get_inner_exprs()
    }

    fn is_replaceable(&self) -> bool {
        self.expr.is_replaceable()
    }
}

impl InnerExpr for ActionExpr {
    fn get_inner_exprs(&self) -> Option<&[Expr]> {
        match self {
            Self::Process(expr) => expr.get_inner_exprs(),
            Self::Err(expr) => expr.get_inner_exprs(),
            Self::Initial(expr) => expr.get_inner_exprs(),
        }
    }

    fn replace_inner_exprs(self, exprs: &[Expr]) -> Option<Self> {
        match self {
            Self::Process(inner) => inner.replace_inner_exprs(exprs).map(Self::Process),
            Self::Err(inner) => inner.replace_inner_exprs(exprs).map(Self::Err),
            Self::Initial(inner) => inner.replace_inner_exprs(exprs).map(Self::Initial),
        }
    }

    fn is_replaceable(&self) -> bool {
        match self {
            Self::Process(expr) => expr.is_replaceable(),
            Self::Err(expr) => expr.is_replaceable(),
            Self::Initial(expr) => expr.is_replaceable(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Expr};

    #[test]
    fn it_tests_inner_expr_trait_impl_for_action() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_expr: Expr = parse_quote! { |v| v + 2 };

        for action_expr in vec![
            ActionExpr::Process(Action::new(
                ProcessExpr::Then([expr.clone()]),
                ApplicationType::Instant,
                MoveType::None,
            )),
            ActionExpr::Err(Action::new(
                ErrExpr::Or([expr.clone()]),
                ApplicationType::Instant,
                MoveType::None,
            )),
            ActionExpr::Initial(Action::new(
                InitialExpr::Single([expr.clone()]),
                ApplicationType::Instant,
                MoveType::None,
            )),
        ]
        .into_iter()
        {
            assert_eq!(
                action_expr.get_inner_exprs().clone(),
                Some(&[expr.clone()][..])
            );
            assert_eq!(
                action_expr
                    .replace_inner_exprs(&[replace_expr.clone()][..])
                    .unwrap()
                    .get_inner_exprs(),
                Some(&[replace_expr.clone()][..])
            )
        }
    }
}

//!
//! Definition of `ActionExpr`, `ProcessActionExpr``.
//!

use syn::Expr;

use super::*;

///
/// `ActionExpr` is `Action` one of type `Process`, `Initial` or `Err`.
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
    /// Returns `ApplyType` of inner expr.
    ///
    pub fn get_apply_type(&self) -> &ApplyType {
        match self {
            Self::Process(expr) => &expr.apply_type,
            Self::Err(expr) => &expr.apply_type,
            Self::Initial(expr) => &expr.apply_type,
        }
    }
}

///
/// Defines `expr` with configuration (`ApplyType`, `MoveType`).
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Action<Expr: InnerExpr> {
    pub expr: Expr,
    pub apply_type: ApplyType,
    pub move_type: MoveType,
}

impl<Inner: InnerExpr> Action<Inner> {
    ///
    /// Create new `Action` with given expr and config.
    ///
    pub fn new(expr: Inner, apply_type: ApplyType, move_type: MoveType) -> Self {
        Self {
            expr,
            apply_type,
            move_type,
        }
    }
}

impl<Inner: InnerExpr> InnerExpr for Action<Inner> {
    fn replace_inner(&self, exprs: Vec<Expr>) -> Option<Action<Inner>> {
        self.expr.replace_inner(exprs).map(|expr| Self {
            expr,
            apply_type: self.apply_type.clone(),
            move_type: self.move_type.clone(),
        })
    }

    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        self.expr.extract_inner()
    }

    fn is_replaceable(&self) -> bool {
        self.expr.is_replaceable()
    }
}

impl InnerExpr for ActionExpr {
    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        match self {
            Self::Process(expr) => expr.extract_inner(),
            Self::Err(expr) => expr.extract_inner(),
            Self::Initial(expr) => expr.extract_inner(),
        }
    }

    fn replace_inner(&self, exprs: Vec<Expr>) -> Option<ActionExpr> {
        match self {
            Self::Process(inner) => inner.replace_inner(exprs).map(Self::Process),
            Self::Err(inner) => inner.replace_inner(exprs).map(Self::Err),
            Self::Initial(inner) => inner.replace_inner(exprs).map(Self::Initial),
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
                ProcessExpr::Then(expr.clone()),
                ApplyType::Instant,
                MoveType::None,
            )),
            ActionExpr::Err(Action::new(
                ErrExpr::Or(expr.clone()),
                ApplyType::Instant,
                MoveType::None,
            )),
            ActionExpr::Initial(Action::new(
                InitialExpr(expr.clone()),
                ApplyType::Instant,
                MoveType::None,
            )),
        ]
        .into_iter()
        {
            assert_eq!(action_expr.extract_inner().clone(), Some(vec![&expr]));
            assert_eq!(
                action_expr
                    .replace_inner(vec![replace_expr.clone()])
                    .unwrap()
                    .extract_inner(),
                Some(vec![&replace_expr])
            )
        }
    }
}

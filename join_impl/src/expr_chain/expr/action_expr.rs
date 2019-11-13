//!
//! Definition of `ActionExpr`, `ProcessActionExpr`, `ErrActionExpr`.
//!

use syn::Expr;

use super::{ErrExpr, InitialExpr, InnerExpr, ProcessExpr};

///
/// `InstantOrDeferredExpr` defines two types of action: `Instant` and `Deferred`
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum InstantOrDeferredExpr<Expr> {
    ///
    /// Action which will be applied to given value instantly.
    ///
    Instant(Expr),
    ///
    /// Action which will be applied after all chains have finished their actions on current step.
    ///
    Deferred(Expr),
}

impl<Inner: InnerExpr> InnerExpr for InstantOrDeferredExpr<Inner> {
    fn replace_inner(&self, exprs: &mut Vec<Expr>) -> Option<InstantOrDeferredExpr<Inner>> {
        match self {
            Self::Instant(inner) => inner.replace_inner(exprs).map(Self::Instant),
            Self::Deferred(inner) => inner.replace_inner(exprs).map(Self::Deferred),
        }
    }

    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        match self {
            Self::Instant(inner) => inner,
            Self::Deferred(inner) => inner,
        }
        .extract_inner()
    }
}

///
/// `ProcessActionExpr` is `InstantOrDeferredExpr` which actions are `ProcessExpr`.
///
pub type ProcessActionExpr = InstantOrDeferredExpr<ProcessExpr>;

///
/// `ErrActionExpr` is `InstantOrDeferredExpr` which actions are `ErrExpr`.
///
pub type ErrActionExpr = InstantOrDeferredExpr<ErrExpr>;

///
/// `ActionExpr` is one of `Process`(`ProcessActionExpr`) or `Err`(`ErrActionExpr`) expr,
/// each of which can be one of its subtypes.
///
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ActionExpr {
    ///
    /// Action of `ProcessActionExpr` type
    ///
    Process(ProcessActionExpr),
    ///
    /// Action of `ErrActionExpr` type
    ///
    Err(ErrActionExpr),
    ///
    /// Action of `InitialExpr` type
    ///
    Initial(InitialExpr),
}

impl InnerExpr for ActionExpr {
    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        match self {
            Self::Process(expr) => expr.extract_inner(),
            Self::Err(expr) => expr.extract_inner(),
            Self::Initial(expr) => expr.extract_inner(),
        }
    }

    fn replace_inner(&self, exprs: &mut Vec<Expr>) -> Option<ActionExpr> {
        match self {
            Self::Process(inner) => inner.replace_inner(exprs).map(Self::Process),
            Self::Err(inner) => inner.replace_inner(exprs).map(Self::Err),
            Self::Initial(inner) => inner.replace_inner(exprs).map(Self::Initial),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Expr};

    #[test]
    fn it_tests_extract_inner_trait_impl_for_process_action_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for process_action_expr in vec![
            ProcessActionExpr::Instant(ProcessExpr::Then(expr.clone())),
            ProcessActionExpr::Deferred(ProcessExpr::Then(expr.clone())),
        ]
        .into_iter()
        {
            assert_eq!(
                process_action_expr.extract_inner().clone(),
                Some(vec![&expr])
            );
        }
    }

    #[test]
    fn it_tests_extract_inner_trait_impl_for_err_action_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for err_action_expr in vec![
            ErrActionExpr::Instant(ErrExpr::Or(expr.clone())),
            ErrActionExpr::Deferred(ErrExpr::OrElse(expr.clone())),
        ]
        .into_iter()
        {
            assert_eq!(err_action_expr.extract_inner().clone(), Some(vec![&expr]));
        }
    }
}

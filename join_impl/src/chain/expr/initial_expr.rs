//!
//! `InitialExpr` definition.
//!

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::{ActionExpr, InnerExpr};

///
/// Used to define expression which is the start value in chain.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum InitialExpr {
    Single([Expr; 1]),
}

impl ToTokens for InitialExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let expr = self.inner_exprs().unwrap();
        let tokens = quote! { #( #expr )* };
        output.extend(tokens);
    }
}

impl InnerExpr for InitialExpr {
    fn inner_exprs(&self) -> Option<&[Expr]> {
        match self {
            Self::Single(expr) => Some(expr),
        }
    }

    fn replace_inner_exprs(self, exprs: &[Expr]) -> Option<Self> {
        exprs.last().cloned().map(|expr| Self::Single([expr]))
    }
}

impl From<InitialExpr> for ActionExpr {
    fn from(val: InitialExpr) -> Self {
        ActionExpr::Initial(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Expr};

    fn are_streams_equal(a: TokenStream, b: TokenStream) -> bool {
        format!("{:#?}", a) == format!("{:#?}", b)
    }

    #[test]
    fn it_tests_inner_expr_trait_impl_for_err_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        assert_eq!(
            InitialExpr::Single([expr.clone()]).inner_exprs().clone(),
            Some(&[expr][..])
        );
    }

    #[test]
    fn it_tests_inner_expr_trait_impl_replace_inner_for_initial_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_inner: Expr = parse_quote! { |v| 1 + v };

        assert_eq!(
            InitialExpr::Single([expr])
                .replace_inner_exprs(&[replace_inner.clone()][..])
                .unwrap()
                .inner_exprs()
                .clone(),
            Some(&[replace_inner][..])
        );
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_initial_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        assert!(are_streams_equal(
            InitialExpr::Single([expr.clone()]).into_token_stream(),
            expr.into_token_stream()
        ));
    }
}

//!
//! Definition of `ErrExpr`.
//!
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::InnerExpr;

///
/// Defines types of expression which will be evaluated in case of `Err` or `None`.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ErrExpr {
    ///
    /// .or(Expr)
    ///
    Or([Expr; 1]),
    ///
    /// .or_else(Expr)
    ///
    OrElse([Expr; 1]),
    ///
    /// .map_err(Expr)
    ///
    MapErr([Expr; 1]),
}

impl ToTokens for ErrExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            Self::Or([expr]) => {
                quote! { .or(#expr) }
            }
            Self::OrElse([expr]) => {
                quote! { .or_else(#expr) }
            }
            Self::MapErr([expr]) => {
                quote! { .map_err(#expr) }
            }
        };
        output.extend(tokens);
    }

    fn into_token_stream(self) -> TokenStream {
        let mut output = TokenStream::new();
        self.to_tokens(&mut output);
        output
    }
}

impl InnerExpr for ErrExpr {
    fn get_inner_exprs(&self) -> Option<&[Expr]> {
        Some(match self {
            Self::Or(expr) => expr,
            Self::OrElse(expr) => expr,
            Self::MapErr(expr) => expr,
        })
    }

    fn replace_inner_exprs(self, exprs: &[Expr]) -> Option<Self> {
        exprs.last().cloned().map(|expr| match self {
            Self::Or(_) => Self::Or([expr]),
            Self::OrElse(_) => Self::OrElse([expr]),
            Self::MapErr(_) => Self::MapErr([expr]),
        })
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

        for err_expr in vec![
            ErrExpr::Or([expr.clone()]),
            ErrExpr::OrElse([expr.clone()]),
            ErrExpr::MapErr([expr.clone()]),
        ]
        .into_iter()
        {
            assert_eq!(
                err_expr.get_inner_exprs().clone(),
                Some(&[expr.clone()][..])
            );
        }
    }

    #[test]
    fn it_tests_inner_expr_trait_impl_replace_inner_for_err_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_inner: Expr = parse_quote! { |v| 1 + v };

        for err_expr in vec![
            ErrExpr::Or([expr.clone()]),
            ErrExpr::OrElse([expr.clone()]),
            ErrExpr::MapErr([expr]),
        ]
        .into_iter()
        {
            assert_eq!(
                err_expr
                    .replace_inner_exprs(&[replace_inner.clone()][..])
                    .unwrap()
                    .get_inner_exprs()
                    .clone(),
                Some(&[replace_inner.clone()][..])
            );
        }
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_err_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for err_expr in vec![
            ErrExpr::Or([expr.clone()]),
            ErrExpr::OrElse([expr.clone()]),
            ErrExpr::MapErr([expr]),
        ]
        .into_iter()
        {
            let mut token_stream = TokenStream::new();
            err_expr.to_tokens(&mut token_stream);
            assert!(are_streams_equal(
                token_stream.clone(),
                err_expr.clone().into_token_stream()
            ));
            assert!(are_streams_equal(
                token_stream,
                match err_expr {
                    ErrExpr::Or([expr]) => {
                        quote! { .or(#expr) }
                    }
                    ErrExpr::OrElse([expr]) => {
                        quote! { .or_else(#expr) }
                    }
                    ErrExpr::MapErr([expr]) => {
                        quote! { .map_err(#expr) }
                    }
                }
            ))
        }
    }
}

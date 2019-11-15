//!
//! Definition of `ErrExpr`.
//!
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::InnerExpr;

///
/// `ErrExpr` used to define types of expression which will be evaluated in case of `Err` or `None`.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ErrExpr {
    ///
    /// .or(Expr)
    ///
    Or(Expr),
    ///
    /// .or_else(Expr)
    ///
    OrElse(Expr),
    ///
    /// .map_err(Expr)
    ///
    MapErr(Expr),
}

impl ToTokens for ErrExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            ErrExpr::Or(expr) => {
                quote! { .or(#expr) }
            }
            ErrExpr::OrElse(expr) => {
                quote! { .or_else(#expr) }
            }
            ErrExpr::MapErr(expr) => {
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
    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        Some(vec![match self {
            Self::Or(expr) => expr,
            Self::OrElse(expr) => expr,
            Self::MapErr(expr) => expr,
        }])
    }

    fn replace_inner(&self, exprs: &mut Vec<Expr>) -> Option<Self> {
        exprs.pop().and_then(|expr| {
            if exprs.is_empty() {
                Some(match self {
                    Self::Or(_) => Self::Or(expr),
                    Self::OrElse(_) => Self::OrElse(expr),
                    Self::MapErr(_) => Self::MapErr(expr),
                })
            } else {
                None
            }
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
    fn it_tests_extract_inner_trait_impl_for_err_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for err_expr in vec![
            ErrExpr::Or(expr.clone()),
            ErrExpr::OrElse(expr.clone()),
            ErrExpr::MapErr(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(err_expr.extract_inner().clone(), Some(vec![&expr]));
        }
    }

    #[test]
    fn it_tests_replace_inner_trait_impl_for_err_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_inner: Expr = parse_quote! { |v| 1 + v };

        for err_expr in vec![
            ErrExpr::Or(expr.clone()),
            ErrExpr::OrElse(expr.clone()),
            ErrExpr::MapErr(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(
                err_expr
                    .replace_inner(&mut vec![replace_inner.clone()])
                    .unwrap()
                    .extract_inner()
                    .clone(),
                Some(vec![&replace_inner])
            );
        }
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_err_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for err_expr in vec![
            ErrExpr::Or(expr.clone()),
            ErrExpr::OrElse(expr.clone()),
            ErrExpr::MapErr(expr.clone()),
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
                    ErrExpr::Or(expr) => {
                        quote! { .or(#expr) }
                    }
                    ErrExpr::OrElse(expr) => {
                        quote! { .or_else(#expr) }
                    }
                    ErrExpr::MapErr(expr) => {
                        quote! { .map_err(#expr) }
                    }
                }
            ))
        }
    }
}

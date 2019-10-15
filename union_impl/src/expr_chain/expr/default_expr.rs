//!
//! `DefaultExpr` used to define types of expression in default position.
//!

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::{ExtractExpr, ReplaceExpr};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DefaultExpr {
    ///
    /// .or(Expr)
    ///
    Or(Expr),
    ///
    /// .or_else(Expr)
    ///
    OrElse(Expr),
}

impl ToTokens for DefaultExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            DefaultExpr::Or(expr) => {
                quote! { .or(#expr) }
            }
            DefaultExpr::OrElse(expr) => {
                quote! { .or_else(#expr) }
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

impl ExtractExpr for DefaultExpr {
    type InnerExpr = Expr;

    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        match self {
            Self::Or(expr) => expr,
            Self::OrElse(expr) => expr,
        }
    }
}

impl ReplaceExpr for DefaultExpr {
    fn replace_expr(&self, expr: Expr) -> Self {
        match self {
            Self::Or(_) => Self::Or(expr),
            Self::OrElse(_) => Self::OrElse(expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn are_streams_equal(a: TokenStream, b: TokenStream) -> bool {
        format!("{:#?}", a) == format!("{:#?}", b)
    }

    #[test]
    fn it_tests_extract_expr_trait_impl_for_default_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        for default_expr in vec![
            DefaultExpr::Or(expr.clone()),
            DefaultExpr::OrElse(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(default_expr.extract_expr().clone(), expr);
            assert_eq!(
                default_expr.extract_inner_expr().clone(),
                match default_expr {
                    DefaultExpr::Or(expr) => expr,
                    DefaultExpr::OrElse(expr) => expr,
                }
            );
        }
    }

    #[test]
    fn it_tests_replace_expr_trait_impl_for_default_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();
        let replace_expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| 1 + v }).unwrap();

        for default_expr in vec![
            DefaultExpr::Or(expr.clone()),
            DefaultExpr::OrElse(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(
                default_expr
                    .replace_expr(replace_expr.clone())
                    .extract_expr()
                    .clone(),
                replace_expr
            );
        }
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_default_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        for default_expr in vec![
            DefaultExpr::Or(expr.clone()),
            DefaultExpr::OrElse(expr.clone()),
        ]
        .into_iter()
        {
            let mut token_stream = TokenStream::new();
            default_expr.to_tokens(&mut token_stream);
            assert!(are_streams_equal(
                token_stream.clone(),
                default_expr.clone().into_token_stream()
            ));
            assert!(are_streams_equal(
                token_stream,
                match default_expr {
                    DefaultExpr::Or(expr) => {
                        quote! { .or(#expr) }
                    }
                    DefaultExpr::OrElse(expr) => {
                        quote! { .or_else(#expr) }
                    }
                }
            ))
        }
    }
}

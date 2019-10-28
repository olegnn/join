//!
//! Contains `InitialExpr` definition.
//!

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::{ExtractExpr, ReplaceExpr};

///
/// `InitialExpr` used to define types of expression in initial position.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InitialExpr(pub Expr);

impl ToTokens for InitialExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let expr = self.extract_expr();
        let tokens = quote! { #expr };
        output.extend(tokens);
    }

    fn into_token_stream(self) -> TokenStream {
        let mut output = TokenStream::new();
        self.to_tokens(&mut output);
        output
    }
}

impl ExtractExpr for InitialExpr {
    type InnerExpr = Expr;

    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        &self.0
    }
}

impl ReplaceExpr for InitialExpr {
    fn replace_expr(&self, expr: Expr) -> Option<Self> {
        Some(InitialExpr(expr))
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

        assert_eq!(InitialExpr(expr.clone()).extract_inner_expr().clone(), expr);

        assert_eq!(InitialExpr(expr.clone()).extract_expr().clone(), expr);
    }

    #[test]
    fn it_tests_replace_expr_trait_impl_for_initial_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();
        let replace_expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| 1 + v }).unwrap();

        assert_eq!(
            InitialExpr(expr)
                .replace_expr(replace_expr.clone())
                .unwrap()
                .extract_expr()
                .clone(),
            replace_expr
        );
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_initial_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        assert!(are_streams_equal(
            InitialExpr(expr.clone()).into_token_stream(),
            expr.into_token_stream()
        ));
    }
}

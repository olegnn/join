//!
//! `ProcessExpr` used to define type of expressions in process position.
//!

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::{ExtractExpr, ReplaceExpr};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ProcessExpr {
    ///
    /// .map(Expr)
    ///
    Map(Expr),
    ///
    /// Expr()
    ///
    Then(Expr),
    ///
    /// .and_then(Expr)
    ///
    AndThen(Expr),
    ///
    /// |value| { Expr(value); value }
    ///
    Inspect(Expr),
    ///
    /// .map_err(Expr)
    ///
    MapErr(Expr),
    ///
    /// .Expr
    ///
    Dot(Expr),
    ///
    /// Expr
    ///
    Initial(Expr),
}

impl ToTokens for ProcessExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            ProcessExpr::AndThen(expr) => {
                quote! { .and_then(#expr) }
            }
            ProcessExpr::Map(expr) => {
                quote! { .map(#expr) }
            }
            ProcessExpr::MapErr(expr) => {
                quote! { .map_err(#expr) }
            }
            ProcessExpr::Dot(expr) => {
                quote! { .#expr }
            }
            ProcessExpr::Then(expr) => {
                quote! {{ let __handler = #expr; __handler }}
            }
            //
            // Not used for now because returning closure requires bound lifetimes
            //
            ProcessExpr::Inspect(_) => {
                //quote! { __inspect(#expr) }
                unimplemented!()
            }
            ProcessExpr::Initial(expr) => {
                quote! { #expr }
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

impl ExtractExpr for ProcessExpr {
    type InnerExpr = Expr;

    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        match self {
            Self::Map(expr) => expr,
            Self::Dot(expr) => expr,
            Self::MapErr(expr) => expr,
            Self::AndThen(expr) => expr,
            Self::Then(expr) => expr,
            Self::Initial(expr) => expr,
            Self::Inspect(expr) => expr,
        }
    }
}

impl ReplaceExpr for ProcessExpr {
    fn replace_expr(&self, expr: Expr) -> Self {
        match self {
            Self::Map(_) => Self::Map(expr),
            Self::Dot(_) => Self::Dot(expr),
            Self::MapErr(_) => Self::MapErr(expr),
            Self::AndThen(_) => Self::AndThen(expr),
            Self::Then(_) => Self::Then(expr),
            Self::Initial(_) => Self::Initial(expr),
            Self::Inspect(_) => Self::Inspect(expr),
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
    fn it_tests_extract_expr_trait_impl_for_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Dot(expr.clone()),
            ProcessExpr::MapErr(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Initial(expr.clone()),
            ProcessExpr::Inspect(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(process_expr.extract_expr().clone(), expr);
            assert_eq!(
                process_expr.extract_inner_expr().clone(),
                match process_expr {
                    ProcessExpr::Map(expr) => expr,
                    ProcessExpr::Dot(expr) => expr,
                    ProcessExpr::MapErr(expr) => expr,
                    ProcessExpr::AndThen(expr) => expr,
                    ProcessExpr::Then(expr) => expr,
                    ProcessExpr::Initial(expr) => expr,
                    ProcessExpr::Inspect(expr) => expr,
                }
            );
        }
    }

    #[test]
    fn it_tests_replace_expr_trait_impl_for_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();
        let replace_expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| 1 + v }).unwrap();

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Dot(expr.clone()),
            ProcessExpr::MapErr(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Initial(expr.clone()),
            ProcessExpr::Inspect(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(
                process_expr
                    .replace_expr(replace_expr.clone())
                    .extract_expr()
                    .clone(),
                replace_expr
            );
        }
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Dot(expr.clone()),
            ProcessExpr::MapErr(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Initial(expr.clone()),
        ]
        .into_iter()
        {
            let mut token_stream = TokenStream::new();
            process_expr.to_tokens(&mut token_stream);
            assert!(are_streams_equal(
                token_stream.clone(),
                process_expr.clone().into_token_stream()
            ));
            assert!(are_streams_equal(
                token_stream,
                match process_expr {
                    ProcessExpr::AndThen(expr) => {
                        quote! { .and_then(#expr) }
                    }
                    ProcessExpr::Map(expr) => {
                        quote! { .map(#expr) }
                    }
                    ProcessExpr::MapErr(expr) => {
                        quote! { .map_err(#expr) }
                    }
                    ProcessExpr::Dot(expr) => {
                        quote! { .#expr }
                    }
                    ProcessExpr::Then(expr) => {
                        quote! {{ let __handler = #expr; __handler }}
                    }
                    //
                    // Not used for now because returning closure requires bound lifetimes
                    //
                    ProcessExpr::Inspect(_) => {
                        //quote! { __inspect(#expr) }
                        unimplemented!()
                    }
                    ProcessExpr::Initial(expr) => {
                        quote! { #expr }
                    }
                }
            ));
        }

        assert!(::std::panic::catch_unwind(
            move || ProcessExpr::Inspect(expr.clone()).into_token_stream()
        )
        .is_err());
    }
}

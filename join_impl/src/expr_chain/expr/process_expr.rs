//!
//! Contains `ProcessExpr` definition.
//!

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Expr;

use super::{ExtractExpr, ReplaceExpr};

///
/// `ProcessExpr` used to define type of expressions in process position.
///
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
    /// .filter(Expr)
    ///
    Filter(Expr),
    ///
    /// Sync: |value| { Expr(&value); value }
    /// Async: |value| value.inspect(Expr)
    ///
    Inspect(Expr),
    ///
    /// .Expr
    ///
    Dot(Expr),
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
            ProcessExpr::Dot(expr) => {
                quote! { .#expr }
            }
            ProcessExpr::Filter(expr) => {
                quote! { .filter(#expr) }
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
            Self::Filter(expr) => expr,
            Self::AndThen(expr) => expr,
            Self::Then(expr) => expr,
            Self::Inspect(expr) => expr,
        }
    }
}

impl ReplaceExpr for ProcessExpr {
    fn replace_expr(&self, expr: Expr) -> Option<Self> {
        match self {
            Self::Map(_) => Some(Self::Map(expr)),
            Self::Dot(_) => None,
            Self::Filter(_) => Some(Self::Filter(expr)),
            Self::AndThen(_) => Some(Self::AndThen(expr)),
            Self::Then(_) => Some(Self::Then(expr)),
            Self::Inspect(_) => Some(Self::Inspect(expr)),
        }
    }

    fn is_replaceable(&self) -> bool {
        if let Self::Dot(_) = self {
            false
        } else {
            true
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
            ProcessExpr::Filter(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
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
                    ProcessExpr::Filter(expr) => expr,
                    ProcessExpr::AndThen(expr) => expr,
                    ProcessExpr::Then(expr) => expr,
                    ProcessExpr::Inspect(expr) => expr,
                }
            );
        }
    }

    #[test]
    fn it_tests_replace_expr_trait_impl_for_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();
        let replace_expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| 3 + v }).unwrap();

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Filter(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Inspect(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(
                process_expr
                    .replace_expr(replace_expr.clone())
                    .unwrap()
                    .extract_expr()
                    .clone(),
                replace_expr
            );
        }

        assert_eq!(
            ProcessExpr::Dot(expr.clone()).replace_expr(replace_expr),
            None
        );
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_process_expr() {
        let expr: ::syn::Expr = ::syn::parse2(::quote::quote! { |v| v + 1 }).unwrap();

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Filter(expr.clone()),
            ProcessExpr::Dot(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
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
                    ProcessExpr::Filter(expr) => {
                        quote! { .filter(#expr) }
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
                }
            ));
        }

        assert!(::std::panic::catch_unwind(
            move || ProcessExpr::Inspect(expr.clone()).into_token_stream()
        )
        .is_err());
    }
}

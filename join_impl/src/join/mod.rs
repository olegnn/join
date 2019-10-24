//!
//! Defines `Join` struct and `generate_join` function to generate output of the `join!` macro based on input and given config.
//!

pub mod config;
pub mod name_constructors;
pub mod parse;

pub use config::Config;

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::Path;

use super::expr_chain::expr::{
    DefaultActionExpr, ExtractExpr, ProcessActionExpr, ProcessExpr, ReplaceExpr,
};
use super::expr_chain::utils::is_block_expr;
use super::expr_chain::{Chain, ProcessWithDefault};
use super::handler::Handler;
use super::name_constructors::*;

///
/// Result of parsing `join!` macro input.
///
pub struct Join {
    pub futures_crate_path: Option<Path>,
    pub branches: Vec<Box<dyn Chain<Member = ProcessWithDefault>>>,
    pub handler: Option<Handler>,
}

fn expand_process_expr(
    previous_result: TokenStream,
    expr: &ProcessExpr,
    is_async: bool,
) -> TokenStream {
    match expr {
        ProcessExpr::Then(_) => {
            quote! { (#expr(#previous_result)) }
        }
        ProcessExpr::Inspect(expr) => {
            //
            // Define custom `into_token_stream` converter because `__inspect` function signature accepts two params.
            //
            let inspect_function_name = construct_inspect_function_name(is_async);
            quote! { #inspect_function_name(#expr, #previous_result) }
        }
        _ => {
            quote! { #previous_result#expr }
        }
    }
}

fn separate_block_expr<ExprType: ReplaceExpr + ExtractExpr + Clone>(
    inner_expr: &ExprType,
    step_number: impl Into<usize>,
    chain_index: impl Into<usize>,
) -> (TokenStream, ExprType) {
    let expr = inner_expr.extract_expr();
    if inner_expr.is_replaceable() && is_block_expr(expr) && step_number.into() > 0 {
        let wrapper_name = construct_result_wrapper_name(chain_index.into());
        if let Some(replaced) =
            inner_expr.replace_expr(syn::parse2(quote! { #wrapper_name }).unwrap())
        {
            return (quote! { let #wrapper_name = #expr; }, replaced);
        }
    }
    (quote! {}, inner_expr.clone())
}

///
/// Generates output of the `join!` macro based on parsed input and given config.
///
pub fn generate_join(
    Join {
        branches,
        handler,
        futures_crate_path,
    }: Join,
    Config { is_async, spawn }: Config,
) -> TokenStream {
    let futures_crate_path = if let Some(futures_crate_path) = futures_crate_path {
        if !is_async {
            panic!("futures_crate_path should be only provided for `async` `join!`")
        } else {
            futures_crate_path
        }
    } else {
        syn::parse2(quote! { ::futures }).unwrap()
    };

    let empty_stream = TokenStream::new();

    //
    // Total branch count.
    //
    let branch_count = branches.len();

    //
    // Spawn sync threads using `std::thread` module.
    //
    let sync_spawn = spawn && !is_async;

    let wrap_previous_result: Box<dyn Fn(TokenStream) -> TokenStream> = if is_async {
        //
        // In case of async `join` we should wrap given result into `Future`
        //
        Box::new(|value| quote! { { async move { #value } } })
    } else {
        //
        // Otherwise it will be enough to just use previous result.
        //
        Box::new(|value| quote! { #value })
    };

    let (
        //
        // Contains all chains depths. Used to calculate max length and determine if we reached chain's end and don't need to join branches anymore.
        //
        depths,
        //
        // `ProcessWithDefault` groups each of which represents chain of `Instant` actions but every next group is `Deferred` from previous.
        // [[map, or_else, map, and_then], [map, and_then]] =>
        // it will be interpreted as #expr.map().or_else.and_then() and after first step ends
        // #expr.map().and_then()
        //
        chains,
    ): (Vec<usize>, Vec<Vec<Vec<ProcessWithDefault>>>) = branches
        .iter()
        .map(|expr_chain| {
            expr_chain.get_members().iter().fold(
                (1usize, vec![Vec::new()]),
                |(depth, mut chain_acc), member| match &member.0 {
                    Some(ProcessActionExpr::Deferred(_)) => match member.1 {
                        Some(DefaultActionExpr::Instant(_)) | None => {
                            chain_acc.push(vec![member.clone()]);
                            (depth + 1, chain_acc)
                        }
                        Some(DefaultActionExpr::Deferred(_)) => {
                            chain_acc.push(vec![ProcessWithDefault(member.0.clone(), None)]);
                            chain_acc.push(vec![ProcessWithDefault(None, member.1.clone())]);
                            (depth + 2, chain_acc)
                        }
                    },
                    _ => match &member.1 {
                        Some(DefaultActionExpr::Instant(_)) | None => {
                            chain_acc.last_mut().unwrap().push(member.clone());
                            (depth, chain_acc)
                        }
                        Some(DefaultActionExpr::Deferred(_)) => {
                            chain_acc
                                .last_mut()
                                .unwrap()
                                .push(ProcessWithDefault(member.0.clone(), None));
                            chain_acc.push(vec![ProcessWithDefault(None, member.1.clone())]);
                            (depth + 1, chain_acc)
                        }
                    },
                },
            )
        })
        .unzip();

    //
    // Returns `Pat` name if exists, otherwise generates default chain result name.
    //
    let get_chain_result_name = |chain_index: usize| -> TokenStream {
        branches[chain_index]
            .get_pat()
            .as_ref()
            .map(|pat| quote! { #pat })
            .unwrap_or_else(|| {
                let name = construct_result_name(chain_index);
                name.into_token_stream()
            })
    };

    //
    // Calculates max chain depth.
    //
    let max_depth = *depths.iter().max().unwrap_or(&0) + (sync_spawn as usize);

    //
    // Contains all generated code to be executed step by step before final results.
    //
    let mut results_by_step = Vec::new();

    for step_number in 0..max_depth {
        if step_number > 0 {
            //
            // If we already have tuple of results, deconstruct them before use in order to every result could be captured by its chain closure correclty.
            //
            let previous_step_result_name = construct_step_result_name(step_number - 1);
            let results: Vec<_> = (0..branch_count)
                .map(|chain_index| {
                    let result_name = get_chain_result_name(chain_index);
                    quote! { #result_name }
                })
                .collect();

            results_by_step.push(if sync_spawn {
                let results_joiners = results.iter().enumerate().map(|(chain_index, result)| {
                    if depths[chain_index] >= step_number {
                        quote! { #result.join().unwrap() }
                    } else {
                        quote! { #result }
                    }
                });
                quote! {
                    let (#( #results ),*) = #previous_step_result_name;
                    let (#( #results ),*) = (#( #results_joiners ),*);
                }
            } else {
                quote! {
                    let (#( #results ),*) = #previous_step_result_name;
                }
            });
        }

        let (def_exprs, step_exprs): (Vec<_>, Vec<_>) = 
            chains
                .iter()
                .map(|chain| chain.get(step_number as usize))
                .enumerate()
                .map(|(chain_index, chain_step_actions)| match chain_step_actions {
                    Some(chain) => chain
                        .iter()
                        .fold(None, |acc, ProcessWithDefault(expr, default_expr)| {
                            let or_clause = 
                                default_expr
                                    .as_ref()
                                    .map(ExtractExpr::extract_inner_expr)
                                    .map(|expr| expr.into_token_stream())
                                    .unwrap_or_else(|| empty_stream.clone());
                            acc.map(
                                |(previous_def_expr, previous_result)| 
                                    match &expr {
                                        Some(ProcessActionExpr::Instant(process_expr)) => {
                                            let (def_expr, process_expr) = separate_block_expr(process_expr, step_number, chain_index);
                                            let process_expr = expand_process_expr(previous_result, &process_expr, is_async);
                                            (quote!{ #previous_def_expr #def_expr }, quote! { #process_expr#or_clause })
                                        }
                                        None => {
                                            let default_expr = default_expr.as_ref().unwrap().clone();
                                            let (def_expr, or_clause) = separate_block_expr(default_expr.extract_inner_expr(), step_number, chain_index);
                                            (quote!{ #previous_def_expr #def_expr }, quote! { #previous_result#or_clause })
                                        }
                                        _ => panic!("join: Unexpected expression type. This is a bug, please report it."),
                                    }
                            )
                            .or_else(|| 
                                Some(
                                    match expr {
                                        Some(ProcessActionExpr::Deferred(process_expr)) => {
                                            let previous_result_name = get_chain_result_name(chain_index);
                                            let previous_result = wrap_previous_result(quote! { #previous_result_name });
                                            let (def_expr, process_expr) = separate_block_expr(process_expr, step_number, chain_index);
                                            let process_expr = expand_process_expr(previous_result, &process_expr, is_async);
                                            (def_expr, quote! { #process_expr#or_clause })
                                        }
                                        Some(ProcessActionExpr::Instant(process_expr)) => {
                                            let (def_expr, process_expr) = separate_block_expr(process_expr, step_number, chain_index);
                                            (def_expr, quote! { #process_expr#or_clause })
                                        }
                                        None => {
                                            let previous_result_name = get_chain_result_name(chain_index);
                                            let previous_result = wrap_previous_result(quote! { #previous_result_name });
                                            let default_expr = default_expr.as_ref().unwrap().clone();
                                            let (def_expr, or_clause) = separate_block_expr(default_expr.extract_inner_expr(), step_number, chain_index);
                                            (def_expr, quote! { #previous_result#or_clause })
                                        }
                                    }
                                )
                            )
                        })
                        .map(|(def_expr, chain)|
                            (
                                def_expr,
                                if spawn {
                                    if is_async {
                                        let spawn_tokio_function_name = construct_spawn_tokio_function_name();
                                        quote! {
                                            { #spawn_tokio_function_name(Box::pin(#chain)) }
                                        }
                                    } else {
                                        let thread_builder_name = construct_thread_builder_name(chain_index);
                                        quote! {
                                            { #thread_builder_name.spawn(move || #chain ).unwrap() }
                                        }
                                    }
                                } else {
                                    if is_async {
                                        quote! { Box::pin(#chain) }
                                    } else {
                                        quote! { #chain }
                                    }
                                }
                            )
                        )
                        .unwrap_or_else(|| (empty_stream.clone(), empty_stream.clone())),
                    None => {
                        let previous_result_name = get_chain_result_name(chain_index);
                        (empty_stream.clone(), wrap_previous_result(quote! { #previous_result_name }))
                    }
                })
                .unzip();

        //
        // Name of variable which contains tuple of current step results.
        //
        let step_result_name = construct_step_result_name(step_number);

        results_by_step.push(if is_async {
            let await_results = if branch_count > 1 {
                quote! { #futures_crate_path::join!(#( #step_exprs ),*) }
            } else {
                quote! { (#( #step_exprs ),*.await) }
            };
            quote! {
                #(#def_exprs)*
                let #step_result_name = #await_results;
            }
        } else {
            //
            // In case of sync spawn generates thread builder for every chain.
            //
            let thread_builders = if spawn {
                (0..branch_count)
                    .map(|chain_index| {
                        let thread_name = construct_thread_name(chain_index).to_string();
                        let thread_builder_name = construct_thread_builder_name(chain_index);
                        quote! {
                            let #thread_builder_name = ::std::thread::Builder::new();
                            let #thread_builder_name = #thread_builder_name.name(
                                ::std::thread::current().name()
                                    .map(
                                        |current_thread_name|
                                            format!("{current_thread_name}_{new_thread_name}",
                                                current_thread_name=current_thread_name,
                                                new_thread_name=#thread_name
                                            )
                                    )
                                    .unwrap_or(#thread_name.to_owned())

                            );
                        }
                    })
                    .collect()
            } else {
                Vec::new()
            };
            quote! {
                #( #thread_builders );*
                #(#def_exprs)*
                let #step_result_name = (#( #step_exprs ),*);
            }
        });
    }

    let last_step_results = construct_step_result_name(max_depth - 1);

    //
    // Returns last step results at the end of expression.
    //
    let results = quote! { { #( #results_by_step )* #last_step_results } };

    //
    // Defines variable names to be used when destructuring results.
    //
    let result_vars: Vec<_> = (0..branch_count)
        .map(|index| {
            let result_name = construct_result_name(index);
            quote! { #result_name }
        })
        .collect();

    //
    // Will transpose tuple of results in result of tuple.
    //
    // (Result<A, Error>, Result<B, Error>, Result<C, Error>) => Result<(A, B, C), Error>
    //
    // ```
    // result0.and_then(|value0| result1.and_then(|value1| result2.map(|value2| (value0, value1, value2))))
    // ```
    //
    //
    let generate_results_unwrapper = || {
        (0..branch_count).fold(None, |acc, index| {
            let index = branch_count - index - 1;
            let value_var_name = construct_var_name(index);
            let result_var_name = construct_result_name(index);
            acc.and_then(|acc| Some(quote! { #result_var_name.and_then(|#value_var_name| #acc ) }))
                .or_else(|| {
                    //
                    // Generates final tuple of unwrapped results.
                    //
                    let tuple_values = (0..branch_count).map(|index| {
                        let value_var_name = construct_var_name(index);
                        quote! { #value_var_name }
                    });
                    Some(quote! { #result_var_name.map(|#value_var_name| (#( #tuple_values ),*) ) })
                })
        })
    };

    let results_wrapper = if is_async {
        quote! { async move { __results } }
    } else {
        quote! { __results }
    };

    //
    // Defines handler based on user input or returns result of tuple of values
    // [or single result in case of one branch].
    //
    let handle_results = handler.as_ref().map_or_else(
        || {
            let unwrap_results = generate_results_unwrapper();
            //
            // Transforms tuple of results in result of tuple
            //
            quote! {
                let (#( #result_vars ),*) = __results;
                let __results = #unwrap_results;
                __results
            }
        },
        |handler| match handler {
            Handler::Then(handler) => {
                //
                // Doesn't unwrap results because handler accepts results.
                //
                quote! {
                    let __handler = #handler;
                    let (#( #result_vars ),*) = __results;
                    __handler(#( #result_vars ),*)
                }
            }
            Handler::Map(handler) => {
                //
                // Unwraps results and pass them to handler if all of them are `Ok` (`Some`). Otherwise returns `Err` (`None`).
                //
                let unwrap_results = generate_results_unwrapper();

                if !is_async {
                    quote! {
                        let (#( #result_vars ),*) = __results;
                        let __results = #unwrap_results;
                        #results_wrapper.map(|__results| {
                            let __handler = #handler;
                            let (#( #result_vars ),*) = __results;
                            __handler(#( #result_vars ),*)
                        })
                    }
                } else {
                    quote! {
                        let (#( #result_vars ),*) = __results;
                        let __results = #unwrap_results;
                        #results_wrapper.map(|__results| {
                            __results.map(|__results| {
                                let __handler = #handler;
                                let (#( #result_vars ),*) = __results;
                                __handler(#( #result_vars ),*)
                            })
                        })
                    }
                }
            }
            Handler::AndThen(handler) => {
                //
                // Unwraps results and pass them to handler if all of them are `Ok` (`Some`). Otherwise returns `Err` (`None`).
                //
                let unwrap_results = generate_results_unwrapper();

                quote! {
                    let (#( #result_vars ),*) = __results;
                    let __results = #unwrap_results;
                    #results_wrapper.and_then(|__results| {
                        let __handler = #handler;
                        let (#( #result_vars ),*) = __results;
                        __handler(#( #result_vars ),*)
                    })
                }
            }
        },
    );

    let inspect_function_name = construct_inspect_function_name(is_async);
    let inspect_definition = if is_async {
        quote! {
            fn #inspect_function_name<T, I: #futures_crate_path::future::Future<Output = T>>(handler: impl Fn(&T) -> (), input: I) -> impl #futures_crate_path::future::Future<Output = T> {
                input.inspect(handler)
            }
        }
    } else {
        quote! {
            fn #inspect_function_name<I>(handler: impl Fn(&I) -> (), input: I) -> I {
                handler(&input);
                input
            }
        }
    };

    if is_async {
        let async_spawn_fn_definition = if spawn {
            let spawn_tokio_function_name = construct_spawn_tokio_function_name();
            quote! {
                async fn #spawn_tokio_function_name<T, F: #futures_crate_path::future::Future<Output = T>>(future: F) -> T
                where
                    F: Send + Sync + 'static,
                    T: Send + Sync + 'static,
                {
                    let (tx, rx) = #futures_crate_path::channel::oneshot::channel();

                    ::tokio::spawn(async move {
                        let value = future.await;
                        tx.send(value).unwrap_or_else(|_| panic!("Unexpected futures ::channel::oneshot::channel panic"));
                    });

                    rx.await.unwrap_or_else(|_| panic!("Unexpected futures ::channel::oneshot::channel panic"))
                }
            }
        } else {
            empty_stream.clone()
        };
        let await_handler = if handler.is_some() {
            quote! { .await }
        } else {
            empty_stream.clone()
        };
        quote! {
            Box::pin(
                async move {
                    use #futures_crate_path::{FutureExt, TryFutureExt, StreamExt, TryStreamExt};
                    #async_spawn_fn_definition
                    #inspect_definition
                    let __results = #results;
                    #handle_results#await_handler
                }
            )
        }
    } else {
        quote! {{
            #inspect_definition
            let __results = #results;
            #handle_results
        }}
    }
}

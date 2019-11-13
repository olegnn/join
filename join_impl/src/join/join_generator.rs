use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use std::error::Error;
use syn::{parse_quote, PatIdent, Path};

use super::super::expr_chain::expr::{
    ActionExpr, ErrActionExpr, InnerExpr, ProcessActionExpr, ProcessExpr,
};
use super::super::expr_chain::utils::is_block_expr;
use super::super::expr_chain::{ActionExprChain, Chain};
use super::super::handler::Handler;
use super::super::name_constructors::*;
use super::config::Config;

///
/// Defines generator for `join!` macro output.
///
pub struct JoinGenerator<'a> {
    ///
    /// Total branch count.
    ///
    branch_count: usize,
    ///
    /// Contains provided or auto-generated branch result names.
    ///
    branch_pats: Vec<Option<&'a PatIdent>>,
    ///
    /// `ActionExpr` groups each of which represents chain of `Instant` actions but every next group is `Deferred` from previous.
    /// [[map, or_else, map, and_then], [map, and_then]] =>
    /// it will be interpreted as #expr.map().or_else.and_then() and after first step ends
    /// #expr.map().and_then()
    ///
    chains: Vec<Vec<Vec<&'a ActionExpr>>>,
    ///
    /// Macro call params.
    ///
    config: Config,
    ///
    /// Contains all chains depths. Used to calculate max length and determine if we reached chain's end and don't need to join branches anymore.
    ///
    depths: Vec<usize>,
    ///
    /// Provided futures crate path.
    ///
    futures_crate_path: Option<&'a Path>,
    ///
    /// Final handler.
    ///
    handler: Option<&'a Handler>,
    ///
    /// Max step count of all branches.
    ///
    max_step_count: usize,
}

impl<'a> JoinGenerator<'a> {
    pub fn new(
        branches: &'a [ActionExprChain],
        handler: Option<&'a Handler>,
        futures_crate_path: Option<&'a Path>,
        config: Config,
    ) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        let Config { is_async, spawn } = config;

        let branch_count = branches.len();

        if !config.is_async && futures_crate_path.is_some() {
            Err("futures_crate_path should be only provided for `async` `join!`".into())
        } else if branch_count == 0 {
            Err("Join should have at least one branch".into())
        } else {
            let (depths_and_paths, chains): (Vec<_>, Vec<_>) = branches
                .iter()
                .map(|expr_chain| {
                    let (depth, steps) = expr_chain.get_members().iter().fold(
                        (1, vec![Vec::new()]),
                        |(depth, mut chain_acc), member| match member {
                            ActionExpr::Process(ProcessActionExpr::Deferred(_))
                            | ActionExpr::Err(ErrActionExpr::Deferred(_)) => {
                                chain_acc.push(vec![member]);
                                (depth + 1, chain_acc)
                            }
                            _ => {
                                chain_acc.last_mut().unwrap().push(member);
                                (depth, chain_acc)
                            }
                        },
                    );
                    ((depth, expr_chain.get_pat()), steps)
                })
                .unzip();

            let (depths, branch_pats): (Vec<usize>, Vec<Option<&PatIdent>>) =
                depths_and_paths.into_iter().unzip();

            Ok(JoinGenerator {
                futures_crate_path,
                config,
                //
                // Calculates max step count.
                //
                max_step_count: *depths.iter().max().unwrap() + ((spawn && !is_async) as usize),
                handler,
                branch_pats,
                depths,
                chains,
                branch_count,
            })
        }
    }

    ///
    /// Returns provided or autogenerated `Pat` for use in `let` bindings.
    ///
    fn get_branch_result_pat(&self, chain_index: impl Into<usize>) -> TokenStream {
        let chain_index = chain_index.into();
        self.branch_pats[chain_index]
            .map(ToTokens::into_token_stream)
            .unwrap_or_else(|| {
                let result_name = construct_result_name(chain_index);
                quote! { #result_name }
            })
    }

    ///
    /// Returns provided or autogenerated `Pat` for use in expressions.
    ///
    fn get_branch_result_name(&self, chain_index: impl Into<usize>) -> TokenStream {
        let chain_index = chain_index.into();
        self.branch_pats[chain_index]
            .map(|pat| pat.ident.clone().into_token_stream())
            .unwrap_or_else(|| construct_result_name(chain_index).into_token_stream())
    }

    ///
    /// Wraps previous result into future if `is_async`.
    ///
    fn wrap_previous_step_result(&self, value: TokenStream) -> TokenStream {
        if self.config.is_async {
            //
            // In case of async `join` we should wrap given result into `Future`
            //
            quote! { async move { #value } }
        } else {
            //
            // Otherwise it will be enough to just use previous result.
            //
            value
        }
    }

    ///
    /// Generates definition (exprs which are placed before step) and step (exprs which are places inside step chain) expressions.
    ///
    pub fn generate_def_and_step_exprs(
        &self,
        step_number: impl Into<usize>,
    ) -> (Vec<Option<TokenStream>>, Vec<TokenStream>) {
        let step_number = step_number.into();
        let Config { is_async, spawn } = self.config;

        self
            .chains
            .iter()
            .map(|chain| chain.get(step_number))
            .enumerate()
            .map(|(chain_index, chain_step_actions)| match chain_step_actions {
                Some(chain) => chain
                    .iter()
                    .enumerate()
                    .fold(None, |acc: Option<(Option<TokenStream>, TokenStream)>, (expr_index, action_expr)|
                        acc.map(|(previous_def_expr, previous_result)| 
                                match action_expr {
                                    ActionExpr::Process(ProcessActionExpr::Instant(process_expr)) => {
                                        let (def_expr, replaced_expr) = self.separate_block_expr(process_expr, chain_index, expr_index);
                                        let process_expr = self.expand_process_expr(
                                            previous_result, 
                                            replaced_expr.as_ref().unwrap_or(process_expr)
                                        );
                                        (
                                            previous_def_expr.map(
                                                |prev| quote! { #prev #def_expr }
                                            ).or(def_expr), 
                                            quote! { #process_expr }
                                        )
                                    }
                                    ActionExpr::Err(ErrActionExpr::Instant(err_expr)) => {
                                        let (def_expr, replaced_expr) = self.separate_block_expr(err_expr, chain_index, expr_index);
                                        let err_expr = replaced_expr.as_ref().unwrap_or(err_expr);
                                        (
                                            previous_def_expr.map(
                                                |prev| quote!{ #prev #def_expr }
                                            ).or(def_expr), 
                                            quote! { #previous_result#err_expr }
                                        )
                                    }
                                    _ => panic!("join: Unexpected expression found in chain_step_actions where acc = Some(..). This's a bug, please report it."),
                                })
                            .or_else(|| {
                                let previous_result_name = self.get_branch_result_name(chain_index);
                                let previous_result = self.wrap_previous_step_result(quote! { #previous_result_name });
                                Some(
                                    match action_expr {
                                        ActionExpr::Process(ProcessActionExpr::Deferred(process_expr))  => {
                                            let (def_expr, replaced_expr) = self.separate_block_expr(process_expr, chain_index, expr_index);
                                            let process_expr = self.expand_process_expr(
                                                previous_result, 
                                                replaced_expr.as_ref().unwrap_or(process_expr)
                                            );
                                            (def_expr, quote! { #process_expr })
                                        }
                                        ActionExpr::Err(ErrActionExpr::Deferred(err_expr)) => {
                                            let (def_expr, replaced_expr) = self.separate_block_expr(err_expr, chain_index, expr_index);
                                            let err_expr = replaced_expr.as_ref().unwrap_or(err_expr);
                                            (def_expr, quote! { #previous_result#err_expr })
                                        }
                                        ActionExpr::Initial(initial_expr) => {
                                            let (def_expr, replaced_initial) = self.separate_block_expr(initial_expr, chain_index, expr_index);
                                            let initial_expr = replaced_initial.as_ref().unwrap_or(initial_expr);
                                            (def_expr, quote! { #initial_expr })
                                        }
                                        _ => panic!("join: Unexpected expression found in chain_step_actions where acc = None. This's a bug, please report it."),
                                    }
                                )
                            })
                    )
                    .map(|(def_expr, chain)|
                        (
                            def_expr,
                            if spawn {
                                if is_async {
                                    let spawn_tokio_fn_name = construct_spawn_tokio_fn_name();
                                    quote! {
                                        { #spawn_tokio_fn_name(Box::pin(#chain)) }
                                    }
                                } else {
                                    let thread_builder_name = construct_thread_builder_name(chain_index);
                                    quote! {
                                        { #thread_builder_name.spawn(move || #chain ).unwrap() }
                                    }
                                }
                            } else if is_async {
                                quote! { Box::pin(#chain) }
                            } else {
                                quote! { #chain }
                            }
                        )
                    )
                    .expect("join: Unexpected error. This's a bug, please report it."),
                None => {
                    let previous_result_name = self.get_branch_result_name(chain_index);
                    (None, self.wrap_previous_step_result(quote! { #previous_result_name }))
                }
            })
            .unzip()
    }

    ///
    /// Generates thread builders if `join_spawn!` or `spawn!` macro call was used.
    ///
    pub fn generate_thread_builders(&self) -> Option<TokenStream> {
        let Config { is_async, spawn } = self.config;

        if is_async || !spawn {
            None
        } else {
            let thread_builders = (0..self.branch_count).map(|chain_index| {
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
            });
            Some(quote! { #( #thread_builders )* })
        }
    }

    ///
    /// Generates extractor for step results. Step results is a tuple, so in order to its every value be captured
    /// by its closure correctly, we need firstly extract all tuple values.
    ///
    pub fn generate_step_results_extractor(&self, step_number: impl Into<usize>) -> TokenStream {
        let step_number = step_number.into();
        let Config { is_async, spawn } = self.config;

        let current_step_result_name = construct_step_result_name(step_number);
        let results: Vec<_> = (0..self.branch_count)
            .map(|chain_index| {
                let result_name = self.get_branch_result_pat(chain_index);
                quote! { #result_name }
            })
            .collect();

        if spawn && !is_async {
            let results_joiners = (0..self.branch_count).map(|chain_index| {
                let result = self.get_branch_result_name(chain_index);
                if self.depths[chain_index] > step_number {
                    quote! { #result.join().unwrap() }
                } else {
                    quote! { #result }
                }
            });
            quote! {
                let (#( #results ),*) = #current_step_result_name;
                let (#( #results ),*) = (#( #results_joiners ),*);
            }
        } else {
            quote! {
                let (#( #results ),*) = #current_step_result_name;
            }
        }
    }

    ///
    /// Generates single step of all provided branches.
    ///
    pub fn generate_step(
        &self,
        def_exprs: &[Option<TokenStream>],
        step_exprs: &[TokenStream],
        step_number: impl Into<usize>,
    ) -> TokenStream {
        let step_number = step_number.into();
        let Config { is_async, .. } = self.config;

        //
        // Name of variable which contains tuple of current step results.
        //
        let current_step_result_name = construct_step_result_name(step_number);

        let current_step = if is_async {
            let futures_crate_path = self.futures_crate_path;
            let await_results = if self.branch_count > 1 {
                quote! { #futures_crate_path::join!(#( #step_exprs ),*) }
            } else {
                quote! { (#( #step_exprs ),*.await) }
            };
            quote! {
                #( #def_exprs )*
                let #current_step_result_name = #await_results;
            }
        } else {
            //
            // In case of sync spawn generates thread builder for every chain.
            //
            let thread_builders = self.generate_thread_builders();

            quote! {
                #thread_builders
                #( #def_exprs )*
                let #current_step_result_name = (#( #step_exprs ),*);
            }
        };

        if step_number < self.max_step_count - 1 {
            //
            // If we already have tuple of results, deconstruct them before use in order to every result could be captured by its chain closure correclty.
            //
            let step_results_extractor = self.generate_step_results_extractor(step_number);

            quote! {
                #current_step
                #step_results_extractor
            }
        } else {
            //
            // Returns last step results at the end of expression.
            //
            quote! {
                #current_step
                #current_step_result_name
            }
        }
    }

    ///
    /// Generates token stream which transposes tuple of results in result of tuple.
    ///
    /// (Result<A, Error>, Result<B, Error>, Result<C, Error>) => Result<(A, B, C), Error>
    ///
    /// ```
    /// fn main() {
    ///     let result0 = Ok::<_,u8>(0);
    ///     let result1 = Ok::<_,u8>(1);
    ///     let result2 = Ok::<_,u8>(2);
    ///     let final_result = result0.and_then(|value0| result1.and_then(|value1| result2.map(|value2| (value0, value1, value2))));
    /// }
    /// ```
    ///
    ///
    pub fn generate_results_unwrapper(&self) -> TokenStream {
        let branch_count = self.branch_count;
        let last_branch_index = branch_count - 1;

        (0..last_branch_index).fold(
            {
                //
                // Generates final tuple of unwrapped results.
                //
                let value_var_name = construct_var_name(last_branch_index);
                let result_var_name = construct_result_name(last_branch_index);
                let tuple_values = (0..=last_branch_index).map(|index| {
                    let value_var_name = construct_var_name(index);
                    quote! { #value_var_name }
                });
                quote! { #result_var_name.map(|#value_var_name| (#( #tuple_values ),*) ) }
            },
            |acc, index| {
                let index = last_branch_index - index - 1;
                let value_var_name = construct_var_name(index);
                let result_var_name = construct_result_name(index);
                quote! { #result_var_name.and_then(|#value_var_name|#acc) }
            },
        )
    }

    ///
    /// Generates token stream which contains handler definition (if exists),
    /// results unwrapper (if needed) and handler call with final results (if handler exists).
    ///
    pub fn generate_handler(&self) -> TokenStream {
        let Self {
            handler, config, ..
        } = self;

        let &Config { is_async, .. } = config;

        let await_handler = if is_async {
            Some(quote! { .await })
        } else {
            None
        };

        match handler {
            Some(Handler::Then(handler)) => {
                //
                // Doesn't unwrap results because handler accepts `Result`s.
                //
                let call_handler = self.extract_results_tuple(true);
                quote! {
                    let __handler = #handler;
                    #call_handler#await_handler
                }
            }
            Some(handler @ Handler::Map(_)) | Some(handler @ Handler::AndThen(_)) => {
                //
                // Unwraps results and pass them to handler if all of them are `Ok` (`Some`). Otherwise returns `Err` (`None`).
                //
                let unwrap_results = self.generate_results_unwrapper();

                let call_handler = self.extract_results_tuple(true);

                let handler_closure_body = if is_async
                    && match handler {
                        Handler::Map(_) => true,
                        _ => false,
                    } {
                    quote! {
                        __results.map(|__results| #call_handler)
                    }
                } else {
                    quote! {
                        #call_handler
                    }
                };

                let results_wrapper = if is_async {
                    quote! { async move { __results } }
                } else {
                    quote! { __results }
                };

                let (call_method, handler_definition) = match handler {
                    Handler::Map(handler) => (quote! { .map }, handler),
                    Handler::AndThen(handler) => (quote! { .and_then }, handler),
                    _ => unreachable!(),
                };

                let extracted_results = self.extract_results_tuple(false);

                quote! {
                    #extracted_results
                    let __results = #unwrap_results;
                    #results_wrapper#call_method(
                        |__results| {
                            let __handler = #handler_definition;
                            #handler_closure_body
                        }
                    )#await_handler
                }
            }
            None => {
                let unwrap_results = self.generate_results_unwrapper();
                let extracted_results = self.extract_results_tuple(false);
                //
                // Transforms tuple of results in result of tuple
                //
                quote! {
                    #extracted_results
                    let __results = #unwrap_results;
                    __results
                }
            }
        }
    }

    ///
    /// Expands process expr with given previous result.
    ///
    pub fn expand_process_expr(
        &self,
        previous_result: TokenStream,
        expr: &ProcessExpr,
    ) -> TokenStream {
        match expr {
            ProcessExpr::Then(_) => {
                quote! { (#expr(#previous_result)) }
            }
            ProcessExpr::Inspect(expr) => {
                let inspect_fn_name = construct_inspect_fn_name();
                if self.config.is_async {
                    quote! { #previous_result.inspect(#expr) }
                } else {
                    //
                    // Define custom `into_token_stream` converter because `__inspect` function signature accepts two params.
                    //
                    quote! { #inspect_fn_name(#expr, #previous_result) }
                }
            }
            _ => {
                quote! { #previous_result#expr }
            }
        }
    }

    ///
    /// Separates definition expr from step expr.
    ///
    pub fn separate_block_expr<ExprType: InnerExpr>(
        &self,
        inner_expr: &ExprType,
        chain_index: impl Into<usize>,
        expr_index: impl Into<usize>,
    ) -> (Option<TokenStream>, Option<ExprType>) {
        let chain_index = chain_index.into();
        let expr_index = expr_index.into();
        if inner_expr.is_replaceable() {
            inner_expr.extract_inner().and_then(|exprs| {
                let (def, mut replace_exprs) = exprs
                    .into_iter()
                    .enumerate()
                    .map(|(index, expr)| {
                        if is_block_expr(expr) {
                            let wrapper_name =
                                construct_result_wrapper_name(chain_index, expr_index, index);
                            (
                                Some((
                                    quote! { let #wrapper_name = #expr; },
                                    parse_quote! { #wrapper_name },
                                )),
                                None,
                            )
                        } else {
                            (None, Some(expr))
                        }
                    })
                    .fold(
                        (None, Vec::new()),
                        |(def_acc, mut replace_acc), (def_with_expr, expr)| {
                            if let Some((def, expr)) = def_with_expr {
                                replace_acc.push(expr);
                                (
                                    def_acc
                                        .map(|def_acc| quote! { #def_acc #def })
                                        .or(Some(def)),
                                    replace_acc,
                                )
                            } else {
                                replace_acc.push(expr.unwrap().clone());
                                (def_acc, replace_acc)
                            }
                        },
                    );

                def.map(|def| (def, inner_expr.replace_inner(&mut replace_exprs)))
            })
        } else {
            None
        }
        .map_or((None, None), |(def_expr, replaced_expr)| {
            (Some(def_expr), replaced_expr)
        })
    }

    ///
    /// Extracts all elements from tuple of values.
    ///
    pub fn extract_results_tuple(&self, handle: bool) -> TokenStream {
        //
        // Defines variable names to be used when destructuring results.
        //
        let result_vars: Vec<_> = (0..self.branch_count)
            .map(|index| {
                let result_name = construct_result_name(index);
                quote! { #result_name }
            })
            .collect();

        let extracted = quote! { let (#( #result_vars ),*) = __results; };

        if handle {
            quote! {{
                #extracted
                __handler(#( #result_vars ),*)
            }}
        } else {
            extracted
        }
    }
}

impl<'a> ToTokens for JoinGenerator<'a> {
    fn to_tokens(&self, output: &mut TokenStream) {
        let Config { is_async, spawn } = self.config;

        //
        // Contains all generated code to be executed step by step and returns final step results.
        //
        let step_by_step_results = (0..self.max_step_count)
            .map(|step_number| (step_number, self.generate_def_and_step_exprs(step_number)))
            .map(|(step_number, (def_exprs, step_exprs))| {
                self.generate_step(&def_exprs[..], &step_exprs, step_number)
            });

        //
        // Defines handler based on user input or returns result of tuple of values
        // [or single result in case of one branch].
        //
        let handle_results = self.generate_handler();

        let inspect_fn_definition = if !is_async {
            let inspect_fn_name = construct_inspect_fn_name();
            Some(quote! {
                fn #inspect_fn_name<I>(handler: impl Fn(&I) -> (), input: I) -> I {
                    handler(&input);
                    input
                }
            })
        } else {
            None
        };

        output.extend(
           if is_async {
                let futures_crate_path = self.futures_crate_path;
                let async_spawn_fn_definition = if spawn {
                    let spawn_tokio_fn_name = construct_spawn_tokio_fn_name();
                    Some(quote! {
                        async fn #spawn_tokio_fn_name<T, F>(future: F) -> T
                        where
                            F: #futures_crate_path::future::Future<Output = T> + Send + Sync + 'static,
                            T: Send + Sync + 'static,
                        {
                            let (tx, rx) = #futures_crate_path::channel::oneshot::channel();

                            ::tokio::spawn(async move {
                                let value = future.await;
                                tx.send(value).unwrap_or_else(|_| panic!("Unexpected futures ::channel::oneshot::channel panic"));
                            });

                            rx.await.unwrap_or_else(|_| panic!("Unexpected futures ::channel::oneshot::channel panic"))
                        }
                    })
                } else {
                    None
                };
                quote! {
                    Box::pin(
                        async move {
                            use #futures_crate_path::{FutureExt, TryFutureExt, StreamExt, TryStreamExt};
                            #async_spawn_fn_definition
                            #inspect_fn_definition                            
                            let __results = { #( #step_by_step_results )* };
                            #handle_results
                        }
                    )
                }
            } else {
                quote! {{
                    #inspect_fn_definition
                    let __results = { #( #step_by_step_results )* };
                    #handle_results
                }}
            }
       );
    }
}

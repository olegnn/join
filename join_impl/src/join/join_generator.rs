use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use std::error::Error;
use syn::{parse_quote, Ident, Index, PatIdent, Path};

use super::super::expr_chain::expr::{
    Action, ActionExpr, ApplyType, InnerExpr, MoveType, ProcessExpr,
};
use super::super::expr_chain::utils::is_block_expr;
use super::super::expr_chain::{ActionExprChain, Chain};
use super::super::handler::Handler;
use super::super::name_constructors::*;
use super::config::Config;

struct ActionExprPos<'a> {
    pub expr: &'a ActionExpr,
    pub chain_index: usize,
    pub expr_index: usize,
}

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
    /// it will be interpreted as `expr.map().or_else().map().and_then()`, and after first will be finished, `expr.map().and_then()`
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
    /// Final handler. (Only applicable for `try..` macros).
    ///
    handler: Option<&'a Handler>,
    ///
    /// Max step count of all branches.
    ///
    max_step_count: usize,
    transpose: bool,
}

struct StepAcc<'a> {
    def_stream: Option<TokenStream>,
    step_streams: Vec<(TokenStream, Option<ActionExprPos<'a>>)>,
}

impl<'a> JoinGenerator<'a> {
    ///
    /// Creates new `JoinGenerator` with given branches - `ActionExprChain`s, optional handler,
    /// optional `futures_crate_path` and `Config`.
    /// Will return Err if macro isn't `try` but handler provided, if `futures_crate_path` provided for non `async`
    /// macro, if branches count is equal to 0.
    ///
    pub fn new(
        branches: &'a [ActionExprChain],
        handler: Option<&'a Handler>,
        futures_crate_path: Option<&'a Path>,
        config: Config,
    ) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        let Config {
            is_async, is_try, ..
        } = config;

        let branch_count = branches.len();

        if !is_try
            && !match handler {
                Some(Handler::Then(_)) | None => true,
                _ => false,
            }
        {
            Err("`and_then` or `map` handler should be only provided for `try` `join!`".into())
        } else if is_try
            && match handler {
                Some(Handler::Then(_)) => true,
                _ => false,
            }
        {
            Err(
                "`then` handler should be only provided for `join!` but not for `try` `join!`"
                    .into(),
            )
        } else if !config.is_async && futures_crate_path.is_some() {
            Err("futures_crate_path should be only provided for `async` `join!`".into())
        } else if branch_count == 0 {
            Err("join should have at least one branch".into())
        } else {
            Ok({
                let (depths_and_paths, chains): (Vec<_>, Vec<_>) = branches
                    .iter()
                    .map(|expr_chain| {
                        let (depth, steps) = expr_chain.get_members().iter().fold(
                            (1, vec![Vec::new()]),
                            |(depth, mut chain_acc), member| match member {
                                ActionExpr::Process(Action { apply_type, .. })
                                | ActionExpr::Err(Action { apply_type, .. })
                                | ActionExpr::Initial(Action { apply_type, .. }) => {
                                    if apply_type == &ApplyType::Deferred {
                                        chain_acc.push(vec![member]);
                                        (depth + 1, chain_acc)
                                    } else {
                                        chain_acc.last_mut().unwrap().push(member);
                                        (depth, chain_acc)
                                    }
                                }
                            },
                        );
                        ((depth, expr_chain.get_pat()), steps)
                    })
                    .unzip();

                let (depths, branch_pats): (Vec<usize>, Vec<Option<&PatIdent>>) =
                    depths_and_paths.into_iter().unzip();

                JoinGenerator {
                    futures_crate_path,
                    config,
                    //
                    // Calculates max step count.
                    //
                    max_step_count: *depths.iter().max().unwrap(),
                    handler,
                    branch_pats,
                    depths,
                    chains,
                    branch_count,
                    transpose: is_try && !is_async,
                }
            })
        }
    }

    ///
    /// Returns provided or autogenerated `PatIdent` for use in `let` bindings.
    ///
    pub fn get_branch_result_pat(&self, chain_index: impl Into<usize>) -> TokenStream {
        let chain_index = chain_index.into();
        self.branch_pats[chain_index]
            .map(ToTokens::into_token_stream)
            .unwrap_or_else(|| construct_result_name(chain_index).into_token_stream())
    }

    ///
    /// Returns provided or autogenerated `Ident` for use in expressions.
    ///
    pub fn get_branch_result_name(&self, chain_index: impl Into<usize>) -> Ident {
        let chain_index = chain_index.into();
        self.branch_pats[chain_index]
            .map(|pat| pat.ident.clone())
            .unwrap_or_else(|| construct_result_name(chain_index))
    }

    ///
    /// Wraps given value into `{...}` if sync or into `async move {...}` if async
    ///
    pub fn wrap_into_block<T: ToTokens>(&self, value: &T) -> TokenStream {
        if self.config.is_async {
            //
            // In case of async `join` we should wrap given stream into `Future`.
            //
            quote! { async move { #value } }
        } else {
            //
            // Otherwise it will be enough to just wrap in {}.
            //
            quote! { { #value } }
        }
    }

    ///
    /// Generates definition (exprs which are placed before step) and step (exprs which are places inside step chain) streams for given step number.
    ///
    pub fn generate_def_and_step_streams_for_step(
        &self,
        step_number: impl Into<usize>,
    ) -> (Vec<Option<TokenStream>>, Vec<TokenStream>) {
        let step_number = step_number.into();
        let Config {
            is_async, is_spawn, ..
        } = self.config;

        self.chains
            .iter()
            .map(|chain| chain.get(step_number))
            .enumerate()
            .map(
                |(chain_index, chain_step_actions)| match chain_step_actions {
                    Some(chain) => chain
                        .iter()
                        .enumerate()
                        .fold(
                            None,
                            |acc: Option<StepAcc>,
                             (expr_index, &action_expr)| {
                                acc.map(|step_acc|
                                    self.process_step_action_expr(
                                        ActionExprPos { 
                                            expr: action_expr, 
                                            chain_index,
                                            expr_index 
                                        },
                                        step_acc
                                    )    
                                )
                                .or_else(|| {
                                    let previous_result_name =
                                        self.get_branch_result_name(chain_index);

                                    let wrapped_previous_result = self.wrap_into_block(&previous_result_name);

                                    let step_acc = StepAcc {
                                        def_stream: None, 
                                        step_streams: vec![(wrapped_previous_result, None)]
                                    };

                                    Some(
                                        self.process_step_action_expr(
                                            ActionExprPos { 
                                                expr: action_expr, 
                                                chain_index,
                                                expr_index 
                                            },
                                            step_acc
                                        )
                                    )
                                })
                            },
                        )
                        .map(|StepAcc { mut def_stream, mut step_streams }| loop {
                            if step_streams.len() > 1 {
                                let result = self.wrap_last_step_stream(StepAcc { def_stream, step_streams }, None);
                                def_stream = result.def_stream;
                                step_streams = result.step_streams;
                            } else {
                                break (def_stream, step_streams.pop().expect("join: Unexpected step streams length 0. This's a bug, please report it.").0);
                            }
                        })
                        .map(|(def_stream, chain)| {
                            (
                                def_stream,
                                if is_spawn {
                                    if is_async {
                                        let spawn_tokio_fn_name = construct_spawn_tokio_fn_name();
                                        quote! {
                                            { #spawn_tokio_fn_name(Box::pin(#chain)) }
                                        }
                                    } else {
                                        let thread_builder_name =
                                            construct_thread_builder_name(chain_index);
                                        quote! {
                                            { #thread_builder_name.spawn(move || #chain ).unwrap() }
                                        }
                                    }
                                } else if is_async {
                                    quote! { Box::pin(#chain) }
                                } else {
                                    chain
                                },
                            )
                        })
                        .expect("join: Unexpected None. This's a bug, please report it."),
                    None => {
                        let previous_result_name = self.get_branch_result_name(chain_index);
                        let previous_result = self.wrap_into_block(&previous_result_name);
                        
                        (
                            None,
                            previous_result,
                        )
                    }
                },
            )
            .unzip()
    }

    ///
    /// Generates thread builders if `join_spawn!` or `spawn!` macro call was used.
    ///
    pub fn generate_thread_builders(&self, step_number: impl Into<usize>) -> Option<TokenStream> {
        let step_number = step_number.into();
        let Config {
            is_async, is_spawn, ..
        } = self.config;

        if is_async || !is_spawn {
            None
        } else {
            let thread_builders = (0..self.branch_count).filter_map(|chain_index| {
                if self.depths[chain_index] > step_number { 
                    let thread_builder_name = construct_thread_builder_name(chain_index);
                    let construct_thread_builder_fn_name = construct_thread_builder_fn_name();
                    Some(quote! { let #thread_builder_name = #construct_thread_builder_fn_name(#chain_index); })
                } else {
                    None
                }
            });
            Some(quote! { #( #thread_builders )* })
        }
    }

    ///
    /// Generates single step code for all provided branches using definition and step streams.
    ///
    pub fn generate_step(
        &self,
        def_streams: &[Option<TokenStream>],
        step_streams: &[TokenStream],
        step_number: impl Into<usize>,
    ) -> TokenStream {
        let step_number = step_number.into();
        let Config {
            is_async, is_try, ..
        } = self.config;

        //
        // Name of variable which contains tuple of current step results.
        //
        let current_step_results_name = construct_step_results_name(step_number);

        if is_async {
            let futures_crate_path = self.futures_crate_path;
            let await_results = if self.branch_count > 1 {
                if is_try {
                    quote! { #futures_crate_path::try_join!(#( #step_streams ),*) }
                } else {
                    quote! { #futures_crate_path::join!(#( #step_streams ),*) }
                }
            } else {
                quote! { #( #step_streams )*.await }
            };

            quote! {
                #( #def_streams )*
                let #current_step_results_name = #await_results;
            }
        } else {
            //
            // In case of sync spawn generates thread builder for every chain.
            //
            let thread_builders = self.generate_thread_builders(step_number);

            quote! {
                #thread_builders
                #( #def_streams )*
                let #current_step_results_name = (#( #step_streams ),*);
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
    pub fn generate_results_transposer<'b, T: ToTokens + 'b>(
        &self,
        result_vars: &'b [T],
    ) -> TokenStream {
        let last_branch_index = self.branch_count - 1;

        (0..last_branch_index).fold(
            {
                //
                // Generates final tuple of unwrapped results.
                //
                let value_var_name = construct_var_name(last_branch_index);
                let result_var_name = &result_vars[last_branch_index];
                let tuple_values = (0..=last_branch_index).map(construct_var_name);

                quote! { #result_var_name.map(|#value_var_name| (#( #tuple_values ),*) ) }
            },
            |acc, index| {
                let index = last_branch_index - index - 1;
                let value_var_name = construct_var_name(index);
                let result_var_name = &result_vars[index];
                quote! { #result_var_name.and_then(|#value_var_name|#acc) }
            },
        )
    }

    ///
    /// Generates token stream which contains results unwrapper (if needed)
    /// and handler call with final results (if handler exists).
    ///
    pub fn generate_handle(&self, results_var: &Ident, handler_name: &Ident) -> TokenStream {
        let Self {
            handler, config, ..
        } = self;
        let &Config { is_async, .. } = config;

        let await_handler = if is_async {
            Some(quote! { .await })
        } else {
            None
        };

        //
        // Defines variable names to be used when destructuring results.
        //
        let result_vars: Vec<_> = (0..self.branch_count).map(construct_result_name).collect();

        match handler {
            Some(Handler::Then(_)) => {
                //
                // Doesn't unwrap results because handler accepts `Result`s (`Option`s).
                //
                let call_handler =
                    self.extract_results_tuple(results_var, &result_vars, handler_name);
                quote! {
                    #call_handler#await_handler
                }
            }
            Some(handler @ Handler::Map(_)) | Some(handler @ Handler::AndThen(_)) => {
                let call_handler =
                    self.extract_results_tuple(results_var, &result_vars, handler_name);

                let handler_closure_body = if is_async
                    && match handler {
                        Handler::Map(_) => true,
                        _ => false,
                    } {
                    quote! {
                        #results_var.map(|#results_var| #call_handler)
                    }
                } else {
                    call_handler
                };

                let call_method = match handler {
                    Handler::Map(_) => quote! { .map },
                    Handler::AndThen(_) => quote! { .and_then },
                    _ => unreachable!(),
                };

                let wrapped_results_var = self.wrap_into_block(results_var);

                quote! {
                    #wrapped_results_var#call_method(
                        |#results_var| {
                            #handler_closure_body
                        }
                    )#await_handler
                }
            }
            None => {
                quote! { #results_var }
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
                    // Define custom `into_token_stream` converter because `inspect` fn signature accepts two params.
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
    /// Separates definition stream from step expr.
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
                let (def, replace_exprs): (Option<_>, Vec<_>) = exprs
                    .into_iter()
                    .enumerate()
                    .map(|(index, expr)| {
                        if is_block_expr(expr) {
                            let wrapper_name =
                                construct_expr_wrapper_name(chain_index, expr_index, index);
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
                def.map(|def| (def, inner_expr.replace_inner(replace_exprs)))
            })
        } else {
            None
        }
        .map_or((None, None), |(def_stream, replaced_expr)| {
            (Some(def_stream), replaced_expr)
        })
    }

    ///
    /// Joins current step with next, returning `TokenStream` which contains all code.
    ///
    pub fn join_steps<T: ToTokens, T1: ToTokens>(
        &self,
        step_number: impl Into<usize>,
        step_stream: TokenStream,
        next_step_stream: impl Into<Option<TokenStream>>,
        result_pats: &[T],
        result_vars: &[T1],
    ) -> TokenStream {
        let step_number = step_number.into();
        let next_step_stream = next_step_stream.into();
        let Config {
            is_try,
            is_async,
            is_spawn,
        } = self.config;
        let current_step_results_name = construct_step_results_name(step_number);

        let processed_results = if is_spawn && !is_async {
            let result_joiners = if self.branch_count > 1 {
                (0..self.branch_count)
                    .map(|chain_index| {
                        let index = Index::from(chain_index);
                        let step_result = quote! { #current_step_results_name.#index };
                        if self.depths[chain_index] > step_number {
                            quote! { #step_result.join().unwrap() }
                        } else {
                            quote! { #step_result }
                        }
                    })
                    .collect()
            } else {
                vec![quote! { #current_step_results_name.join().unwrap() }]
            };

            quote! {
                let (#( #result_pats ),*) = (#( #result_joiners ),*);
            }
        } else {
            self.extract_results_tuple(&current_step_results_name, &result_pats, None)
        };

        if is_try && step_number < self.max_step_count - 1 {
            if self.transpose {
                let is_result_successful: Vec<_> = (0..self.branch_count)
                    .map(|index| {
                        let result_var = if is_spawn && !is_async {
                            (&result_vars[index]).into_token_stream()
                        } else if self.branch_count > 1 {
                            let index = Index::from(index);
                            quote! { #current_step_results_name.#index }
                        } else {
                            quote! { #current_step_results_name }
                        };
                        quote! { #result_var.as_ref().map(|_| true).unwrap_or(false) }
                    })
                    .collect();

                let results_vars_matcher =
                    result_vars.iter().enumerate().map(|(index, result_var)| {
                        quote! {
                            #index => #result_var.map(|_| unreachable!())
                        }
                    });

                let (fail_index_before_processed, fail_index_after_processed) = if is_spawn
                    && !is_async
                {
                    (
                        None,
                        quote! { [#(#is_result_successful),*].iter().position(|__v| !__v) },
                    )
                } else {
                    (
                        Some(
                            quote! { let __fail_index = [#(#is_result_successful),*].iter().position(|__v| !__v); },
                        ),
                        quote! { __fail_index },
                    )
                };

                quote! {
                    #step_stream
                    #fail_index_before_processed
                    #processed_results
                    if let Some(__fail_index) = #fail_index_after_processed {
                        match __fail_index {
                            #( #results_vars_matcher ),*,
                            _ => unreachable!()
                        }
                    } else {
                        #next_step_stream
                    }
                }
            } else {
                let current_step_results = if is_try && is_async {
                    let ok_result_vars = (0..self.branch_count).map(|index| {
                        let current_step_result = if self.branch_count > 1 {
                            let result_index = Index::from(index);
                            quote! { #current_step_results_name.#result_index }
                        } else {
                            quote! { #current_step_results_name }
                        };
                        quote! { Ok(#current_step_result) }
                    });

                    quote! {
                        let #current_step_results_name = (#(#ok_result_vars),*);
                        #processed_results
                    }
                } else {
                    processed_results
                };

                quote! {
                    #step_stream
                    match #current_step_results_name {
                        Ok(#current_step_results_name) => {
                            #current_step_results
                            #next_step_stream
                        },
                        Err(err) => Err(err)
                    }
                }
            }
        } else if self.transpose {
            let transposer = self.generate_results_transposer(&result_vars);

            quote! {
                #step_stream
                #processed_results
                #transposer
            }
        } else {
            let current_step_results = if is_spawn && !is_async {
                quote! { #processed_results; (#( #result_vars ),*) }
            } else {
                current_step_results_name.into_token_stream()
            };

            quote! {
                #step_stream
                #current_step_results
            }
        }
    }

    ///
    /// Generates `TokenStream` for all steps.
    ///
    pub fn generate_steps(&self) -> TokenStream {
        let result_vars: Vec<_> = (0..self.branch_count)
            .map(|chain_index| self.get_branch_result_name(chain_index))
            .collect();

        let result_pats: Vec<_> = (0..self.branch_count)
            .map(|chain_index| self.get_branch_result_pat(chain_index))
            .collect();

        (0..self.max_step_count)
            .map(|step_number| {
                (
                    step_number,
                    self.generate_def_and_step_streams_for_step(step_number),
                )
            })
            .map(|(step_number, (def_streams, step_streams))| {
                (
                    step_number,
                    self.generate_step(&def_streams, &step_streams, step_number),
                )
            })
            .rev()
            .fold(None, |next_step_stream, (step_number, step_stream)| {
                self.join_steps(
                    step_number,
                    step_stream,
                    next_step_stream,
                    &result_pats,
                    &result_vars,
                )
                .into()
            })
            .unwrap()
    }

    ///
    /// Extracts all elements from tuple of values.
    ///
    pub fn extract_results_tuple<'b, T: ToTokens + 'b>(
        &self,
        results_var: &Ident,
        result_vars: &'b [T],
        handler: impl Into<Option<&'b Ident>>,
    ) -> TokenStream {
        let handler = handler.into();

        let extracted = quote! { let (#( #result_vars ),*) = #results_var; };

        if let Some(handler) = handler {
            quote! {{
                #extracted
                #handler(#( #result_vars ),*)
            }}
        } else {
            extracted
        }
    }

    ///
    /// Pops top stream of step_streams and wraps it into previous expr, adds next expr (if `Some`) to the result chain.
    ///
    fn wrap_last_step_stream<'b, 'c>(
        &self,
        StepAcc {
            def_stream: previous_def_stream,
            mut step_streams,
        }: StepAcc<'c>,
        action_expr_with_indices: impl Into<Option<ActionExprPos<'b>>>,
    ) -> StepAcc<'c> {
        let (previous_step_stream, _) = step_streams.pop().expect(
            "join: Unexpected error on attempt to get last step stream. This's a bug, please report it.",
        );

        let (current_step_steam, action_expr_wrapper) = step_streams.pop().expect("join: Step expressions length is zero while it should be >1. This's a bug, please report it.");

        let action_expr_wrapper = action_expr_wrapper
            .expect("join: Expected wrapper, found `None`. This's a bug, please report it.");

        let internal_value_name = construct_internal_value_name();

        let replaced_expr = action_expr_wrapper
            .expr
            .replace_inner(vec![
                parse_quote! { |#internal_value_name| #previous_step_stream },
            ])
            .expect("join: Failed to replace expr in unwrap expr. This's a bug, please report it.");

        let replaced_action_expr_position = ActionExprPos {
            expr: &replaced_expr,
            expr_index: action_expr_wrapper.expr_index,
            chain_index: action_expr_wrapper.chain_index,
        };

        let (def_stream, step_stream) = self.generate_def_and_step_streams(
            previous_def_stream,
            current_step_steam,
            replaced_action_expr_position,
        );

        let (def_stream, step_stream) = self.generate_def_and_step_streams(
            def_stream,
            step_stream,
            action_expr_with_indices.into(),
        );

        step_streams.push((step_stream, None));

        StepAcc {
            def_stream,
            step_streams,
        }
    }

    ///
    /// Produces definition stream and step stream for given `ActionExpr` (if `Some`).
    ///
    fn generate_def_and_step_streams<'b>(
        &self,
        previous_def_stream: impl Into<Option<TokenStream>>,
        previous_step_stream: TokenStream,
        action_expr_with_indices: impl Into<Option<ActionExprPos<'b>>>,
    ) -> (Option<TokenStream>, TokenStream) {
        let previous_def_stream = previous_def_stream.into();

        if let Some(ActionExprPos {
            expr: action_expr,
            chain_index,
            expr_index,
        }) = action_expr_with_indices.into()
        {
            match action_expr {
                ActionExpr::Process(Action {
                    expr: process_expr, ..
                }) => {
                    let (def_stream, replaced_expr) =
                        self.separate_block_expr(process_expr, chain_index, expr_index);

                    let step_stream = self.expand_process_expr(
                        previous_step_stream,
                        replaced_expr.as_ref().unwrap_or(process_expr),
                    );

                    (
                        previous_def_stream
                            .map(|prev| quote! { #prev #def_stream })
                            .or(def_stream),
                        step_stream,
                    )
                }
                ActionExpr::Err(Action { expr: err_expr, .. }) => {
                    let (def_stream, replaced_expr) =
                        self.separate_block_expr(err_expr, chain_index, expr_index);

                    let err_expr = replaced_expr.as_ref().unwrap_or(err_expr);

                    (
                        previous_def_stream
                            .map(|prev| quote! { #prev #def_stream })
                            .or(def_stream),
                        quote! { #previous_step_stream#err_expr },
                    )
                }
                ActionExpr::Initial(Action {
                    expr: initial_expr, ..
                }) => {
                    let (def_stream, replaced_expr) =
                        self.separate_block_expr(initial_expr, chain_index, expr_index);

                    let initial_expr = replaced_expr.as_ref().unwrap_or(initial_expr);

                    (
                        previous_def_stream
                            .map(|prev| quote! { #prev #def_stream })
                            .or(def_stream),
                        quote! { #initial_expr },
                    )
                }
            }
        } else {
            (previous_def_stream, previous_step_stream)
        }
    }

    ///
    /// Generates code based on `MoveType` of expr. If `MoveType` is `Wrap`, it will generate nested exprs, if `Unwrap`,
    /// it will got one step up, otherwise continue current chain.
    ///
    fn process_step_action_expr<'b>(
        &self,
        action_expr_with_indices: impl Into<Option<ActionExprPos<'b>>>,
        step_acc: StepAcc<'b>,
    ) -> StepAcc<'b> {
        let ActionExprPos {
            expr: action_expr,
            chain_index,
            expr_index,
        } = action_expr_with_indices.into().expect("join: Unexpected `None` `ActionExprPos` in `process_step_action_expr`. This's a bug, please report it.");

        match action_expr.get_move_type() {
            MoveType::Unwrap => self.wrap_last_step_stream(
                step_acc,
                None, // Because now only `CommandGroup::UNWRAP` can have this `MoveType`
            ),
            MoveType::Wrap => {
                let StepAcc {
                    def_stream: previous_def_stream,
                    mut step_streams,
                } = step_acc;

                step_streams.last_mut().expect("join: Unexpected 0 length of step streams. This's a bug, please report it.").1 =
                    ActionExprPos {
                        expr: action_expr,
                        chain_index,
                        expr_index,
                    }.into();
                step_streams.push((construct_internal_value_name().into_token_stream(), None));

                StepAcc {
                    def_stream: previous_def_stream,
                    step_streams,
                }
            }
            MoveType::None => {
                let StepAcc {
                    def_stream: previous_def_stream,
                    mut step_streams,
                } = step_acc;

                let (previous_step_stream, _) =
                    step_streams.pop().expect("join: Unexpected `None` when pop step streams. This's a bug, please report it.");
                let (def_stream, step_stream) = self.generate_def_and_step_streams(
                    previous_def_stream,
                    previous_step_stream,
                    ActionExprPos {
                        expr: action_expr,
                        chain_index,
                        expr_index,
                    },
                );

                step_streams.push((step_stream, None));

                StepAcc {
                    def_stream,
                    step_streams,
                }
            }
        }
    }
}

impl<'a> ToTokens for JoinGenerator<'a> {
    fn to_tokens(&self, output: &mut TokenStream) {
        let Config {
            is_async, is_spawn, ..
        } = self.config;

        let results_var = construct_results_name();
        let handler_name = construct_handler_name();
        //
        // Contains all generated code to be executed step by step and returns final step results.
        //
        let steps_stream = self.generate_steps();

        //
        // Defines handler based on user input or returns result of tuple of values
        // [or single result in case of one branch].
        //
        let handle_results = self.generate_handle(&results_var, &handler_name);

        let handler_definition = if let Some(handler) = self.handler {
            let handler_expr = handler.extract_expr();

            Some(quote! { let #handler_name = #handler_expr; })
        } else {
            None
        };

        output.extend(
           if is_async {
                let futures_crate_path = self.futures_crate_path;
                let async_spawn_fn_definition = if is_spawn {
                    let spawn_tokio_fn_name = construct_spawn_tokio_fn_name();
                    Some(quote! {
                        async fn #spawn_tokio_fn_name<T, F>(__future: F) -> T
                        where
                            F: #futures_crate_path::future::Future<Output = T> + Send + Sync + 'static,
                            T: Send + Sync + 'static,
                        {
                            let (__tx, __rx) = #futures_crate_path::channel::oneshot::channel();

                            ::tokio::spawn(async move {
                                let __value = __future.await;
                                __tx.send(__value).map(|_| ()).unwrap_or_else(|_| ());
                            });

                            __rx.await.unwrap()
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
                            #handler_definition                           
                            let #results_var = { #steps_stream };
                            #handle_results
                        }
                    )
                }
            } else {
                let inspect_fn_definition = {
                    let inspect_fn_name = construct_inspect_fn_name();
                    let handler_name = construct_handler_name();
                    quote! {
                        fn #inspect_fn_name<I>(#handler_name: impl Fn(&I) -> (), __input: I) -> I {
                            #handler_name(&__input);
                            __input
                        }
                    }
                };
                let thread_builder_fn_definition = if is_spawn {
                    let construct_thread_builder_fn_name = construct_thread_builder_fn_name();
                    Some(
                       quote! {
                            fn #construct_thread_builder_fn_name(chain_index: usize) -> ::std::thread::Builder {
                                let thread_name = format!("join_{}", chain_index);
                                ::std::thread::Builder::new().name(
                                    ::std::thread::current().name()
                                        .map(
                                            |current_thread_name|
                                                format!("{current_thread_name}_{new_thread_name}",
                                                    current_thread_name=current_thread_name,
                                                    new_thread_name=thread_name
                                                )
                                        )
                                        .unwrap_or(thread_name)
        
                                )
                            }
                        }
                   )
                } else {
                    None
                };
                quote! {{
                    #inspect_fn_definition
                    #thread_builder_fn_definition
                    #handler_definition
                    let #results_var = { #steps_stream };
                    #handle_results
                }}
            }
       );
    }
}

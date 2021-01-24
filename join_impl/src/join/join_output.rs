//!
//! Definition and implementation of `join!` macro output generator.
//!

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::{parse_quote, Ident, Index, PatIdent, Path};

use super::config::Config;
use super::name_constructors::*;
use crate::action_expr_chain::ActionExprChain;
use crate::chain::expr::{Action, ActionExpr, ApplicationType, InnerExpr, MoveType, ProcessExpr};
use crate::chain::Chain;
use crate::handler::Handler;
use crate::parse::utils::is_block_expr;

struct ActionExprPos<'a> {
    pub expr: &'a ActionExpr,
    pub branch_index: u16,
    pub expr_index: u16,
}

impl<'a> ActionExprPos<'a> {
    fn new(
        expr: &'a ActionExpr,
        branch_index: impl Into<usize>,
        expr_index: impl Into<usize>,
    ) -> Self {
        Self {
            expr,
            branch_index: branch_index.into() as u16,
            expr_index: expr_index.into() as u16,
        }
    }
}

struct StepAcc<'a> {
    def_stream: Option<TokenStream>,
    step_streams: Vec<(TokenStream, Option<ActionExprPos<'a>>)>,
}

///
/// Generator of `join!` macro output.
///
pub struct JoinOutput<'a> {
    ///
    /// Total branch count.
    ///
    branch_count: u16,
    ///
    /// Provided result names for branches.
    ///
    branch_pats: Vec<Option<&'a PatIdent>>,
    ///
    /// `ActionExpr` groups each of which represents chain of `Instant` actions but every next group is `Deferred` from prev.
    /// [[map, or_else, map, and_then], [map, and_then]] =>
    /// it will be interpreted as `expr.map().or_else().map().and_then()`, and after first will be finished, `expr.map().and_then()`
    ///
    chains: Vec<Vec<Vec<&'a ActionExpr>>>,
    ///
    /// Macro call params.
    ///
    config: Config,
    ///
    /// Provided custom joiner function.
    ///
    custom_joiner: Option<&'a TokenStream>,
    ///
    /// Contains all branches depths. Used to calculate max length and determine if we reached branch's end.
    ///
    depths: Vec<u16>,
    ///
    /// Provided futures crate path.
    ///
    futures_crate_path: Option<&'a Path>,
    ///
    /// Optional final handler.
    ///
    handler: Option<&'a Handler>,
    ///
    /// Wrap branches into closure `move || {...}`
    ///
    lazy_branches: bool,
    ///
    /// Max step count of all branches.
    ///
    max_step_count: u16,
    ///
    /// Transform tuple of `Result`'s (`Option`s) into `Result`/`Option` of tuple.
    ///
    transpose: bool,
}

impl<'a> JoinOutput<'a> {
    ///
    /// Creates new `Join` with given branches - `ActionExprChain`s, optional handler,
    /// optional `futures_crate_path` and `Config`.
    /// Will return `Err` if macro isn't `try` but `map` or `and_then` handler provided or if `then` handler provided for `try` macro,
    /// if `futures_crate_path` provided for non `async` macro, if branches count is equal to 0.
    ///
    pub fn new(
        branches: &'a [ActionExprChain],
        handler: Option<&'a Handler>,
        futures_crate_path: Option<&'a Path>,
        custom_joiner: Option<&'a TokenStream>,
        custom_transpose_results: Option<bool>,
        lazy_branches: Option<bool>,
        config: Config,
    ) -> Result<Self, &'static str>
    where
        Self: Sized,
    {
        let Config {
            is_async,
            is_try,
            is_spawn,
        } = config;

        let branch_count = branches.len() as u16;

        if !is_try
            && (handler.map(Handler::is_map).unwrap_or(false)
                || handler.map(Handler::is_and_then).unwrap_or(false))
        {
            Err("`and_then` or `map` handler should be only provided for `try` `join!`")
        } else if is_try && handler.map(Handler::is_then).unwrap_or(false) {
            Err("`then` handler should be only provided for `join!` but not for `try` `join!`")
        } else if !config.is_async && futures_crate_path.is_some() {
            Err("futures_crate_path should be only provided for `async` `join!`")
        } else if branch_count == 0 {
            Err("join should have at least one branch")
        } else {
            Ok({
                let (depths_and_paths, chains): (Vec<_>, Vec<_>) = branches
                    .iter()
                    .map(|expr_chain| {
                        let (depth, steps) = expr_chain.get_members().iter().fold(
                            (1, vec![Vec::new()]),
                            |(depth, mut chain_acc), member| match member {
                                ActionExpr::Process(Action {
                                    application_type, ..
                                })
                                | ActionExpr::Err(Action {
                                    application_type, ..
                                })
                                | ActionExpr::Initial(Action {
                                    application_type, ..
                                }) => {
                                    if application_type == &ApplicationType::Deferred {
                                        chain_acc.push(vec![member]);
                                        (depth + 1, chain_acc)
                                    } else {
                                        chain_acc.last_mut().unwrap().push(member);
                                        (depth, chain_acc)
                                    }
                                }
                            },
                        );
                        ((depth, expr_chain.get_id()), steps)
                    })
                    .unzip();

                let (depths, branch_pats): (Vec<u16>, Vec<Option<&PatIdent>>) =
                    depths_and_paths.into_iter().unzip();

                Self {
                    futures_crate_path,
                    custom_joiner,
                    lazy_branches: lazy_branches.unwrap_or(is_spawn && !is_async),
                    config,
                    //
                    // Max step count is max depth of branches.
                    //
                    max_step_count: *depths.iter().max().unwrap(),
                    handler,
                    branch_pats,
                    depths,
                    chains,
                    branch_count,
                    //
                    // In case of `try` `async` `::futures::try_join!` macro will be used, so we don't need to
                    // transpose results.
                    //
                    transpose: custom_transpose_results.unwrap_or(is_try && !is_async),
                }
            })
        }
    }

    ///
    /// Generates `TokenStream` which contains all steps. `result_pats` will be used in `let` destructuring patterns and
    /// `result_vars` will be placed in actual step streams. They both are needed to join steps and make results of prev
    /// values in next.
    ///
    pub fn generate_steps<TPat: ToTokens + Clone, TVar: ToTokens + Clone>(
        &self,
        result_pats: &[TPat],
        result_vars: &[TVar],
    ) -> TokenStream {
        (0..self.max_step_count)
            .map(|step_number| {
                let step_results_name = construct_step_results_name(step_number);
                (
                    step_number,
                    self.generate_step(step_number, result_vars, &step_results_name),
                    step_results_name,
                )
            })
            .rev()
            .fold(
                None,
                |next_step_stream, (step_number, step_stream, step_results_name)| {
                    self.join_steps(
                        step_number,
                        step_stream,
                        next_step_stream,
                        result_pats,
                        result_vars,
                        &step_results_name,
                    )
                    .into()
                },
            )
            .unwrap()
    }

    ///
    /// Generates token stream which contains handler call with final results (if handler exists) or returns final results.
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
                let call_handler =
                    self.extract_results_tuple(results_var, &result_vars, handler_name, None);
                quote! {
                    #call_handler#await_handler
                }
            }
            Some(handler @ Handler::Map(_)) | Some(handler @ Handler::AndThen(_)) => {
                let call_handler =
                    self.extract_results_tuple(results_var, &result_vars, handler_name, None);

                let handler_closure_body = if is_async && handler.is_map() {
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
    /// Generates `TokenStream` for step with given index. Result vars are variables with prev step results.
    /// `result_vars` will be used as source values for step and `step_result_name` will contain tuple of step results.
    ///
    pub fn generate_step<TVar: ToTokens, TName: ToTokens>(
        &self,
        step_number: impl Into<usize>,
        result_vars: &[TVar],
        step_results_name: &TName,
    ) -> TokenStream {
        let step_number = step_number.into() as u16;
        let Config {
            is_async,
            is_spawn,
            is_try,
        } = self.config;

        let (def_streams, step_streams): (Vec<_>, Vec<_>) = self.chains
            .iter()
            .map(|chain| chain.get(step_number as usize))
            .enumerate()
            .filter_map(
                |(branch_index, chain_step_actions)|
                    chain_step_actions.and_then(
                        |chain_step_actions|
                        chain_step_actions
                        .iter()
                        .enumerate()
                        .fold(
                            None,
                            |acc: Option<StepAcc>,
                             (expr_index, &action_expr)| {
                                acc.map(|step_acc|
                                    self.process_step_action_expr(
                                        ActionExprPos::new(
                                            action_expr,
                                            branch_index,
                                            expr_index
                                        ),
                                        step_acc
                                    )
                                )
                                .or_else(|| {
                                    let prev_result_name = &result_vars[branch_index];

                                    let wrapped_prev_result = self.wrap_into_block(prev_result_name);

                                    let step_acc = StepAcc {
                                        def_stream: None,
                                        step_streams: vec![(wrapped_prev_result, None)]
                                    };

                                    Some(
                                        self.process_step_action_expr(
                                            ActionExprPos::new(
                                                action_expr,
                                                branch_index,
                                                expr_index
                                            ),
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
                                if self.get_active_step_branch_count(step_number) > 1 {
                                    let chain = if self.lazy_branches {
                                        quote! { move || #chain }
                                    } else {
                                        chain
                                    };
                                    if is_spawn {
                                        if is_async {
                                            let spawn_tokio_fn_name = construct_spawn_tokio_fn_name();
                                            quote! {
                                                { #spawn_tokio_fn_name(Box::pin(#chain)) }
                                            }
                                        } else {
                                            let thread_builder_name =
                                                construct_thread_builder_name(branch_index);
                                            quote! {
                                                { #thread_builder_name.spawn(#chain).unwrap() }
                                            }
                                        }
                                    } else {
                                        chain
                                    }
                                } else {
                                    chain
                                },
                            )
                        }),
                    )
            )
            .unzip();

        let joiner = if self.get_active_step_branch_count(step_number) > 1 {
            self.custom_joiner.map(Clone::clone).or_else(|| {
                if is_async {
                    let futures_crate_path = self.futures_crate_path;
                    if is_try {
                        Some(quote! { #futures_crate_path::try_join! })
                    } else {
                        Some(quote! { #futures_crate_path::join! })
                    }
                } else {
                    None
                }
            })
        } else {
            None
        };

        if is_async {
            let join_results = joiner
                .map(|joiner| quote! { #joiner(#( #step_streams ),*) })
                .unwrap_or_else(|| quote! { #( #step_streams )*.await });

            quote! {
                #( #def_streams )*
                let #step_results_name = #join_results;
            }
        } else {
            //
            // In case of sync spawn generates thread builder for every branch wich needs it and joins it.
            //
            let (thread_builders, spawn_joiners) = self
                .generate_thread_builders_and_spawn_joiners(step_number, step_results_name)
                .map(|(a, b)| (a.into(), b.into()))
                .unwrap_or((None, None));

            quote! {
                #thread_builders
                #( #def_streams )*
                let #step_results_name = #joiner(#( #step_streams ),*);
                #spawn_joiners
            }
        }
    }

    ///
    /// Joins step steam with next step stream (if `Some`), returning `TokenStream` which contains all code.
    /// `result_pats` will be used for destructuring `let` patterns while `result_vars` will be source values for next step.
    /// `result_vars` are also used in results transposer.
    ///
    pub fn join_steps<TPat: ToTokens + Clone, TVar: ToTokens + Clone, TName: ToTokens>(
        &self,
        step_number: impl Into<usize>,
        step_stream: TokenStream,
        next_step_stream: impl Into<Option<TokenStream>>,
        result_pats: &[TPat],
        result_vars: &[TVar],
        step_results_name: &TName,
    ) -> TokenStream {
        let step_number = step_number.into();
        let next_step_stream = next_step_stream.into();
        let &Self {
            transpose,
            max_step_count,
            branch_count,
            ..
        } = self;
        let Config {
            is_try, is_async, ..
        } = self.config;

        let extracted_results =
            self.extract_results_tuple(&step_results_name, &result_pats, None, step_number);
        let err_to_err = quote! { Err(err) => Err(err) };

        if is_try && (step_number as u16) < max_step_count - 1 {
            if transpose {
                let (is_result_successful, result_vars_matcher): (Vec<_>, Vec<_>) = result_vars
                    .iter()
                    .enumerate()
                    .filter_map(|(index, result_var)| {
                        if self.is_branch_active_in_step(step_number, index) {
                            (
                                quote! { #result_var.as_ref().map(|_| true).unwrap_or(false) },
                                quote! {
                                    #index => #result_var.map(|_| unreachable!())
                                },
                            )
                                .into()
                        } else {
                            None
                        }
                    })
                    .unzip();
                let value_name = construct_internal_value_name();

                quote! {
                    #step_stream
                    #extracted_results
                    if let Some(__fail_index) = [#( #is_result_successful ),*].iter().position(|#value_name| !#value_name) {
                        match __fail_index {
                            #( #result_vars_matcher ),*,
                            _ => unreachable!()
                        }
                    } else {
                        #next_step_stream
                    }
                }
            } else {
                let current_step_results = if is_async {
                    let mut index: u16 = 0;
                    let ok_result_vars = (0..branch_count).filter_map(|branch_index| {
                        if self.is_branch_active_in_step(step_number, branch_index) {
                            let result_var = self.generate_indexed_step_results_name(
                                step_results_name,
                                step_number,
                                index,
                            );
                            index += 1;

                            Some(quote! { Ok(#result_var) })
                        } else {
                            None
                        }
                    });

                    quote! {
                        let #step_results_name = (#(# ok_result_vars ),*);
                        #extracted_results
                    }
                } else {
                    extracted_results
                };

                quote! {
                    #step_stream
                    match #step_results_name {
                        Ok(#step_results_name) => {
                            #current_step_results
                            #next_step_stream
                        },
                        #err_to_err
                    }
                }
            }
        } else if transpose && is_try {
            let transposer = self.generate_results_transposer(&result_vars, None);

            quote! {
                #step_stream
                #extracted_results
                #transposer
            }
        } else if is_try {
            let final_stream = if self.branch_count > 1 {
                let results: Vec<_> = result_vars
                    .iter()
                    .enumerate()
                    .filter_map(|(index, result_var)| {
                        if !self.is_branch_active_in_step(step_number, index) {
                            Some(result_var.clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                if !results.is_empty() {
                    let transposer = self.generate_results_transposer(&results, &result_vars);

                    quote! {
                        match #step_results_name {
                            Ok(#step_results_name) => {
                                #extracted_results
                                #transposer
                            },
                            #err_to_err
                        }
                    }
                } else {
                    quote! {
                        match #step_results_name {
                            Ok(#step_results_name) => {
                                #extracted_results
                                Ok((#(# result_vars ),*))
                            },
                            #err_to_err
                        }
                    }
                }
            } else {
                let value_name = construct_internal_value_name();
                quote! {
                    match #step_results_name {
                        Ok(#value_name) => Ok((#value_name)),
                        #err_to_err
                    }
                }
            };

            quote! {
                #step_stream
                #final_stream
            }
        } else {
            let final_stream = next_step_stream.unwrap_or_else(|| quote! { (#( #result_vars ),*) });

            quote! {
                #step_stream
                #extracted_results
                #final_stream
            }
        }
    }

    ///
    /// Returns provided `PatIdent` or autogenerated branch name for use in `let` bindings.
    ///
    pub fn get_branch_result_pat(&self, branch_index: impl Into<usize>) -> TokenStream {
        let branch_index = branch_index.into();
        self.branch_pats[branch_index]
            .map(ToTokens::into_token_stream)
            .unwrap_or_else(|| construct_result_name(branch_index).into_token_stream())
    }

    ///
    /// Returns provided or autogenerated `Ident` for use in expressions.
    ///
    pub fn get_branch_result_name(&self, branch_index: impl Into<usize>) -> Ident {
        let branch_index = branch_index.into();
        self.branch_pats[branch_index]
            .map(|pat| pat.ident.clone())
            .unwrap_or_else(|| construct_result_name(branch_index))
    }

    ///
    /// Wraps given value into `{...}` if sync or into `async move {...}` if async.
    ///
    pub fn wrap_into_block<TVal: ToTokens>(&self, value: &TVal) -> TokenStream {
        if self.config.is_async {
            //
            // In case of async `join` we should wrap given value into `Future` with `move`.
            //
            quote! { async move { #value } }
        } else {
            //
            // Otherwise it will be enough to just wrap in `{...}`.
            //
            quote! { { #value } }
        }
    }

    ///
    /// Returns count of active branches for given step.
    ///
    pub fn get_active_step_branch_count(&self, step_number: impl Into<usize>) -> u16 {
        let step_number = step_number.into() as u16;
        self.depths
            .iter()
            .filter(|&&branch_depth| branch_depth > step_number)
            .count() as u16
    }

    ///
    /// Returns `true` if branch with given index is active in provided step.
    ///
    pub fn is_branch_active_in_step(
        &self,
        step_number: impl Into<usize>,
        branch_index: impl Into<usize>,
    ) -> bool {
        self.depths[branch_index.into()] > step_number.into() as u16
    }

    ///
    /// Generates indexed step result name if branch count for step is greater than 1, otherwise
    /// returns current step results name.
    ///
    fn generate_indexed_step_results_name<TName: ToTokens>(
        &self,
        step_results_name: &TName,
        step_number: impl Into<usize>,
        index: impl Into<usize>,
    ) -> TokenStream {
        let step_number = step_number.into() as u16;
        let index = index.into();

        if self.get_active_step_branch_count(step_number) > 1 {
            let index = Index::from(index);
            quote! { #step_results_name.#index }
        } else {
            step_results_name.into_token_stream()
        }
    }

    ///
    /// Generates thread builders and result joiners for step if `join_spawn!` or `spawn!` macro call was used.
    ///
    fn generate_thread_builders_and_spawn_joiners<TName: ToTokens>(
        &self,
        step_number: impl Into<usize>,
        step_results_name: &TName,
    ) -> Option<(TokenStream, TokenStream)> {
        let step_number = step_number.into() as u16;
        let Config {
            is_async, is_spawn, ..
        } = self.config;

        if is_async || !is_spawn || self.get_active_step_branch_count(step_number) < 2 {
            None
        } else {
            let thread_builders = (0..self.branch_count).filter_map(|branch_index| {
                if self.is_branch_active_in_step(step_number, branch_index) {
                    let thread_builder_name = construct_thread_builder_name(branch_index);
                    let construct_thread_builder_fn_name = construct_thread_builder_fn_name();
                    Some(quote! { let #thread_builder_name = #construct_thread_builder_fn_name(#branch_index); })
                } else {
                    None
                }
            });

            let mut index: u16 = 0;
            let result_joiners = (0..self.branch_count).filter_map(|branch_index| {
                if self.is_branch_active_in_step(step_number, branch_index) {
                    let step_result = self.generate_indexed_step_results_name(
                        step_results_name,
                        step_number,
                        index,
                    );
                    index += 1;
                    Some(quote! { #step_result.join().unwrap() })
                } else {
                    None
                }
            });

            Some((
                quote! { #( #thread_builders )* },
                quote! { let #step_results_name = (#( #result_joiners ),*); },
            ))
        }
    }

    ///
    /// Generates token stream which transposes tuple of `Result`s into `Result` of tuple.
    /// Optional `return_vars` may be specified to return something other than `result_vars`.
    ///
    /// (Result<A, Error>, Result<B, Error>, Result<C, Error>) => Result<(A, B, C), Error>
    ///
    /// # Example:
    ///
    /// ```
    /// let result0 = Ok::<_,()>(0);
    /// let result1 = Ok::<_,()>(1);
    /// let result2 = Ok::<_,()>(2);
    /// let final_result = result0.and_then(|value0| result1.and_then(|value1| result2.map(|value2| (value0, value1, value2))));
    /// assert_eq!(final_result, Ok((0,1,2)));
    /// ```
    ///
    ///
    fn generate_results_transposer<'b, T: ToTokens + 'b>(
        &self,
        result_vars: &'b [T],
        return_vars: impl Into<Option<&'b &'b [T]>>,
    ) -> TokenStream {
        let return_vars = return_vars.into();

        result_vars
            .iter()
            .rev()
            .fold(None, |acc, result_var_name| {
                acc.map_or_else(
                    || {
                        let return_value = return_vars.map_or_else(
                            || quote! { (#( #result_vars ),*) },
                            |return_vars| quote! { (#( #return_vars ),*) },
                        );
                        Some(quote! { #result_var_name.map(|#result_var_name| #return_value ) })
                    },
                    |acc| Some(quote! { #result_var_name.and_then(|#result_var_name| #acc) }),
                )
            })
            .unwrap()
    }

    ///
    /// Expands process expr with given prev result.
    ///
    fn expand_process_expr(&self, prev_result: TokenStream, expr: &ProcessExpr) -> TokenStream {
        match expr {
            ProcessExpr::Then(_) => {
                quote! { (#expr(#prev_result)) }
            }
            ProcessExpr::Inspect([expr]) => {
                let inspect_fn_name = construct_inspect_fn_name();
                if self.config.is_async {
                    quote! { #prev_result.inspect(#expr) }
                } else {
                    //
                    // Define custom `into_token_stream` converter because `inspect` fn signature accepts two params.
                    //
                    quote! { #inspect_fn_name(#expr, #prev_result) }
                }
            }
            _ => {
                quote! { #prev_result#expr }
            }
        }
    }

    ///
    /// Separates definition stream from step stream for given expression.
    ///
    fn separate_block_expr<ExprType: InnerExpr + Clone>(
        &self,
        inner_expr: &ExprType,
        branch_index: impl Into<usize>,
        expr_index: impl Into<usize>,
    ) -> (Option<TokenStream>, Option<ExprType>) {
        let branch_index = branch_index.into() as u16;
        let expr_index = expr_index.into() as u16;

        if inner_expr.is_replaceable() {
            inner_expr.extract_inner().and_then(|exprs| {
                let (def, replace_exprs): (Option<_>, Vec<_>) = exprs
                    .iter()
                    .enumerate()
                    .map(|(index, expr)| {
                        if is_block_expr(expr) {
                            let wrapper_name =
                                construct_expr_wrapper_name(branch_index, expr_index, index);
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
                def.map(|def| (def, inner_expr.clone().replace_inner(&replace_exprs)))
            })
        } else {
            None
        }
        .map_or((None, None), |(def_stream, replaced_expr)| {
            (Some(def_stream), replaced_expr)
        })
    }

    ///
    /// Extracts elements from tuple of values. Applies handler, if provided.
    ///
    fn extract_results_tuple<'b, TRes: ToTokens + 'b, TVar: ToTokens + 'b>(
        &self,
        results_var: &'b TRes,
        result_vars: &'b [TVar],
        handler: impl Into<Option<&'b Ident>>,
        step_number: impl Into<Option<usize>>,
    ) -> TokenStream {
        let handler = handler.into();
        let step_number = step_number.into();

        let extracted = step_number.map_or_else(
            || quote! { let (#( #result_vars ),*) = #results_var; },
            |step_number| {
                let mut index: u16 = 0;
                let result_vars = result_vars.iter().filter(|_| {
                    let result = self.is_branch_active_in_step(step_number, index);
                    index += 1;
                    result
                });
                quote! { let (#( #result_vars ),*) = #results_var; }
            },
        );

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
    /// Pops top stream of step_streams and wraps it into prev expr, adds next expr (if `Some`) to the result chain.
    ///
    fn wrap_last_step_stream<'b, 'c>(
        &self,
        StepAcc {
            def_stream: prev_def_stream,
            mut step_streams,
        }: StepAcc<'c>,
        action_expr_pos: impl Into<Option<ActionExprPos<'b>>>,
    ) -> StepAcc<'c> {
        let (prev_step_stream, _) = step_streams.pop().expect(
            "join: Unexpected error on attempt to get last step stream. This's a bug, please report it.",
        );

        let (current_step_steam, action_expr_wrapper) = step_streams.pop().expect("join: Step expressions length is zero while it should be >1. This's a bug, please report it.");

        let action_expr_wrapper = action_expr_wrapper
            .expect("join: Expected wrapper, found `None`. This's a bug, please report it.");

        let internal_value_name = construct_internal_value_name();

        let replaced_expr = action_expr_wrapper
            .expr
            .clone()
            .replace_inner(&[parse_quote! { |#internal_value_name| #prev_step_stream }])
            .expect("join: Failed to replace expr in unwrap expr. This's a bug, please report it.");

        let replaced_action_expr_position = ActionExprPos {
            expr: &replaced_expr,
            expr_index: action_expr_wrapper.expr_index,
            branch_index: action_expr_wrapper.branch_index,
        };

        let (def_stream, step_stream) = self.generate_def_and_step_streams(
            prev_def_stream,
            current_step_steam,
            replaced_action_expr_position,
        );

        let (def_stream, step_stream) =
            self.generate_def_and_step_streams(def_stream, step_stream, action_expr_pos.into());

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
        prev_def_stream: impl Into<Option<TokenStream>>,
        prev_step_stream: TokenStream,
        action_expr_pos: impl Into<Option<ActionExprPos<'b>>>,
    ) -> (Option<TokenStream>, TokenStream) {
        let prev_def_stream = prev_def_stream.into();

        if let Some(ActionExprPos {
            expr: action_expr,
            branch_index,
            expr_index,
        }) = action_expr_pos.into()
        {
            match action_expr {
                ActionExpr::Process(Action {
                    expr: process_expr, ..
                }) => {
                    let (def_stream, replaced_expr) =
                        self.separate_block_expr(process_expr, branch_index, expr_index);

                    let step_stream = self.expand_process_expr(
                        prev_step_stream,
                        replaced_expr.as_ref().unwrap_or(process_expr),
                    );

                    (
                        prev_def_stream
                            .map(|prev| quote! { #prev #def_stream })
                            .or(def_stream),
                        step_stream,
                    )
                }
                ActionExpr::Err(Action { expr: err_expr, .. }) => {
                    let (def_stream, replaced_expr) =
                        self.separate_block_expr(err_expr, branch_index, expr_index);

                    let err_expr = replaced_expr.as_ref().unwrap_or(err_expr);

                    (
                        prev_def_stream
                            .map(|prev| quote! { #prev #def_stream })
                            .or(def_stream),
                        quote! { #prev_step_stream#err_expr },
                    )
                }
                ActionExpr::Initial(Action {
                    expr: initial_expr, ..
                }) => {
                    let (def_stream, replaced_expr) =
                        self.separate_block_expr(initial_expr, branch_index, expr_index);

                    let initial_expr = replaced_expr.as_ref().unwrap_or(initial_expr);

                    (
                        prev_def_stream
                            .map(|prev| quote! { #prev #def_stream })
                            .or(def_stream),
                        quote! { #initial_expr },
                    )
                }
            }
        } else {
            (prev_def_stream, prev_step_stream)
        }
    }

    ///
    /// Generates code based on `MoveType` of expr. If `MoveType` is `Wrap`, it will generate nested exprs, if `Unwrap`,
    /// it will got one step up, otherwise continue current chain.
    ///
    fn process_step_action_expr<'b>(
        &self,
        action_expr_pos: impl Into<Option<ActionExprPos<'b>>>,
        step_acc: StepAcc<'b>,
    ) -> StepAcc<'b> {
        let ActionExprPos {
            expr: action_expr,
            branch_index,
            expr_index,
        } = action_expr_pos.into().expect("join: Unexpected `None` `ActionExprPos` in `process_step_action_expr`. This's a bug, please report it.");

        match action_expr.get_move_type() {
            MoveType::Unwrap => self.wrap_last_step_stream(
                step_acc,
                None, // Because now only `CommandGroup::UNWRAP` can have this `MoveType`
            ),
            MoveType::Wrap => {
                let StepAcc {
                    def_stream: prev_def_stream,
                    mut step_streams,
                } = step_acc;

                step_streams.last_mut().expect("join: Unexpected 0 length of step streams. This's a bug, please report it.").1 =
                    ActionExprPos::new(
                        action_expr,
                        branch_index,
                        expr_index,
                    ).into();
                step_streams.push((construct_internal_value_name().into_token_stream(), None));

                StepAcc {
                    def_stream: prev_def_stream,
                    step_streams,
                }
            }
            MoveType::None => {
                let StepAcc {
                    def_stream: prev_def_stream,
                    mut step_streams,
                } = step_acc;

                let (prev_step_stream, _) =
                    step_streams.pop().expect("join: Unexpected `None` when pop step streams. This's a bug, please report it.");
                let (def_stream, step_stream) = self.generate_def_and_step_streams(
                    prev_def_stream,
                    prev_step_stream,
                    ActionExprPos::new(action_expr, branch_index, expr_index),
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

impl<'a> ToTokens for JoinOutput<'a> {
    fn to_tokens(&self, output: &mut TokenStream) {
        let Config {
            is_async, is_spawn, ..
        } = self.config;

        let results_var = construct_results_name();
        let handler_name = construct_handler_name();
        let (result_pats, result_vars): (Vec<_>, Vec<_>) = (0..self.branch_count)
            .map(|branch_index| {
                (
                    self.get_branch_result_pat(branch_index),
                    self.get_branch_result_name(branch_index),
                )
            })
            .unzip();

        //
        // Contains all generated code to be executed step by step and returns final step results.
        //
        let steps_stream = self.generate_steps(&result_pats, &result_vars);

        //
        // Handle results based on user input or returns result of tuple of values
        // [or single result in case of one branch].
        //
        let handle_results = self.generate_handle(&results_var, &handler_name);

        let handler_definition = self
            .handler
            .map(Handler::extract_expr)
            .map(|handler_expr| quote! { let #handler_name = #handler_expr; });

        output.extend(
           if is_async {
                let futures_crate_path = self.futures_crate_path;
                let async_spawn_fn_definition = if is_spawn {
                    let spawn_tokio_fn_name = construct_spawn_tokio_fn_name();
                    let value_name = construct_internal_value_name();

                    Some(
                        quote! {
                            fn #spawn_tokio_fn_name<T, F>(__future: F) -> impl #futures_crate_path::future::Future<Output=T>
                            where
                                F: #futures_crate_path::future::Future<Output = T> + Send + 'static,
                                T: Send + 'static,
                            {
                                ::tokio::spawn(__future).map(|#value_name| #value_name.unwrap_or_else(|err| panic!("tokio JoinHandle failed: {:#?}", err)))
                            }
                        }
                    )
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
                    let value_name = construct_internal_value_name();

                    quote! {
                        fn #inspect_fn_name<I>(#handler_name: impl Fn(&I) -> (), #value_name: I) -> I {
                            #handler_name(&#value_name);
                            #value_name
                        }
                    }
                };
                let thread_builder_fn_definition = if is_spawn {
                    let construct_thread_builder_fn_name = construct_thread_builder_fn_name();

                    Some(
                       quote! {
                            fn #construct_thread_builder_fn_name(branch_index: u16) -> ::std::thread::Builder {
                                let thread_name = format!("join_{}", branch_index);
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

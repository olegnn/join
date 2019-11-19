//!
//! Definition of `CommandGroup`.
//!
//!
use proc_macro2::TokenStream;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse2;
use syn::Token;

use super::super::super::expr_chain::ActionExprChainGenerator;
use super::super::expr::{ErrExpr, InitialExpr, ProcessExpr};
use super::super::{TransformParsed, Unit, UnitResult};

///
/// `CommandGroup` is an enum of all possible `ProcessExpr`, `ErrExpr` and `InitialExpr` operations.
/// Used to express group which was found in input `ParseStream`
///
#[cfg(not(feature = "full"))]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CommandGroup {
    /// [ProcessExpr::Map]
    Map,
    /// [ProcessExpr::Dot]
    Dot,
    /// [ProcessExpr::Filter]
    Filter,
    /// [ProcessExpr::Inspect]
    Inspect,
    /// [ProcessExpr::Then]
    Then,
    /// [ProcessExpr::AndThen]
    AndThen,
    /// [ErrExpr::Or]
    Or,
    /// [ErrExpr::OrElse]
    OrElse,
    /// [ErrExpr::MapErr]
    MapErr,
    /// [InitialExpr]
    Initial,
    /// [ProcessExpr::Chain]
    Chain,
    /// [ProcessExpr::Flatten]
    Flatten,
    /// [ProcessExpr::Collect]
    Collect,
    /// [ProcessExpr::Enumerate]
    Enumerate,
    /// [ProcessExpr::Find]
    Find,
    /// [ProcessExpr::Fold]
    Fold,
    /// [ProcessExpr::TryFold]
    TryFold,
    /// [ProcessExpr::Unzip]
    Unzip,
    /// [ProcessExpr::Zip]
    Zip,
    /// [ProcessExpr::Partition]
    Partition,
    /// [ProcessExpr::FilterMap]
    FilterMap,
    /// [ProcessExpr::FindMap]
    FindMap,
    /// UNWRAP (Special `CommandGroup` used to define next group nested position [which will be #value.and_then(#previous_expr).#next_expr] )
    UNWRAP,
}

///
/// Contains all possible command groups.
///
#[cfg(feature = "full")]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CommandGroup {
    /// [ProcessExpr::Map]
    Map,
    /// [ProcessExpr::Dot]
    Dot,
    /// [ProcessExpr::Filter]
    Filter,
    /// [ProcessExpr::Inspect]
    Inspect,
    /// [ProcessExpr::Then]
    Then,
    /// [ProcessExpr::AndThen]
    AndThen,
    /// [ErrExpr::Or]
    Or,
    /// [ErrExpr::OrElse]
    OrElse,
    /// [ErrExpr::MapErr]
    MapErr,
    /// [InitialExpr]
    Initial,
    /// [ProcessExpr::All]
    All,
    /// [ProcessExpr::Any]
    Any,
    /// [ProcessExpr::ByRef]
    ByRef,
    /// [ProcessExpr::Chain]
    Chain,
    /// [ProcessExpr::Cloned]
    Cloned,
    /// [ProcessExpr::Cmp]
    Cmp,
    /// [ProcessExpr::Collect]
    Collect,
    /// [ProcessExpr::Copied]
    Copied,
    /// [ProcessExpr::Count]
    Count,
    /// [ProcessExpr::Cycle]
    Cycle,
    /// [ProcessExpr::Enumerate]
    Enumerate,
    /// [ProcessExpr::Eq]
    Eq,
    /// [ProcessExpr::FilterMap]
    FilterMap,
    /// [ProcessExpr::Find]
    Find,
    /// [ProcessExpr::FindMap]
    FindMap,
    /// [ProcessExpr::FlatMap]
    FlatMap,
    /// [ProcessExpr::Flatten]
    Flatten,
    /// [ProcessExpr::Fold]
    Fold,
    /// [ProcessExpr::ForEach]
    ForEach,
    /// [ProcessExpr::Fuse]
    Fuse,
    /// [ProcessExpr::Ge]
    Ge,
    /// [ProcessExpr::Gt]
    Gt,
    /// [ProcessExpr::IsSorted]
    IsSorted,
    /// [ProcessExpr::IsSortedBy]
    IsSortedBy,
    /// [ProcessExpr::IsSortedByKey]
    IsSortedByKey,
    /// [ProcessExpr::Last]
    Last,
    /// [ProcessExpr::Le]
    Le,
    /// [ProcessExpr::Lt]
    Lt,
    /// [ProcessExpr::Max]
    Max,
    /// [ProcessExpr::MaxBy]
    MaxBy,
    /// [ProcessExpr::MaxByKey]
    MaxByKey,
    /// [ProcessExpr::Min]
    Min,
    /// [ProcessExpr::MinBy]
    MinBy,
    /// [ProcessExpr::MinByKey]
    MinByKey,
    /// [ProcessExpr::Ne]
    Ne,
    /// [ProcessExpr::Nth]
    Nth,
    /// [ProcessExpr::PartialCmp]
    PartialCmp,
    /// [ProcessExpr::IsPartitioned]
    IsPartitioned,
    /// [ProcessExpr::Partition]
    Partition,
    /// [ProcessExpr::PartitionInPlace]
    PartitionInPlace,
    /// [ProcessExpr::Peekable]
    Peekable,
    /// [ProcessExpr::Position]
    Position,
    /// [ProcessExpr::Product]
    Product,
    /// [ProcessExpr::Rev]
    Rev,
    /// [ProcessExpr::Rposition]
    Rposition,
    /// [ProcessExpr::Scan]
    Scan,
    /// [ProcessExpr::SizeHint]
    SizeHint,
    /// [ProcessExpr::Skip]
    Skip,
    /// [ProcessExpr::SkipWhile]
    SkipWhile,
    /// [ProcessExpr::StepBy]
    StepBy,
    /// [ProcessExpr::Sum]
    Sum,
    /// [ProcessExpr::Take]
    Take,
    /// [ProcessExpr::TakeWhile]
    TakeWhile,
    /// [ProcessExpr::TryFold]
    TryFold,
    /// [ProcessExpr::TryForEach]
    TryForEach,
    /// UNWRAP (Special `CommandGroup` used to define next group nested position [which will be #value.and_then(#previous_expr).#next_expr] )
    UNWRAP,
    /// [ProcessExpr::Unzip]
    Unzip,
    /// [ProcessExpr::Zip]
    Zip,
}

impl ::std::fmt::Display for CommandGroup {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Empty();

impl Parse for Empty {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            Ok(Empty())
        } else {
            Err(input.error("Unexpected tokens"))
        }
    }
}

fn to_none<T, R>(_: T) -> Option<R> {
    None
}

fn to_some<T>(v: T) -> Option<T> {
    Some(v)
}

fn parse_empty_unit(
    action_expr_chain_gen: &ActionExprChainGenerator,
    input: ParseStream<'_>,
) -> UnitResult<Empty> {
    action_expr_chain_gen.parse_unit::<Empty>(input, true)
}

fn parse_single_unit<ParseUnit: Parse, ResultExpr>(
    to_expr: fn(ParseUnit) -> ResultExpr,
    action_expr_chain_gen: &ActionExprChainGenerator,
    input: ParseStream<'_>,
) -> UnitResult<ResultExpr> {
    action_expr_chain_gen
        .parse_unit(input, false)
        .transform_parsed(to_expr)
}

fn parse_single_or_empty_unit<ParseUnit: Parse, ResultExpr>(
    to_expr: fn(Option<ParseUnit>) -> ResultExpr,
    action_expr_chain_gen: &ActionExprChainGenerator,
    input: ParseStream<'_>,
) -> UnitResult<ResultExpr> {
    action_expr_chain_gen
        .parse_unit::<Empty>(&input.fork(), true)
        .and_then(|_| action_expr_chain_gen.parse_unit::<Empty>(&input, true))
        .transform_parsed(to_none)
        .or_else(|_| {
            action_expr_chain_gen
                .parse_unit::<ParseUnit>(input, false)
                .transform_parsed(to_some)
        })
        .transform_parsed(to_expr)
}

fn parse_n_or_empty_unit<ParseUnit: Parse, ResultExpr>(
    to_expr: fn(Option<Vec<ParseUnit>>) -> ResultExpr,
    action_expr_chain_gen: &ActionExprChainGenerator,
    input: ParseStream<'_>,
    unit_count: usize,
) -> UnitResult<ResultExpr> {
    action_expr_chain_gen
        .parse_unit::<Empty>(&input.fork(), true)
        .and_then(|_| action_expr_chain_gen.parse_unit::<Empty>(&input, true))
        .transform_parsed(to_none)
        .or_else(|_| {
            (0..unit_count)
                .map(|index| {
                    action_expr_chain_gen
                        .parse_unit::<ParseUnit>(input, false)
                        .and_then(|unit| {
                            if index + 1 < unit_count {
                                input.parse::<Token![,]>().and_then(|_| {
                                    if unit.next_group_type.is_none() {
                                        Ok(unit)
                                    } else {
                                        Err(input.error("Unexpected action group."))
                                    }
                                })
                            } else {
                                Ok(unit)
                            }
                        })
                })
                .try_fold(
                    Unit {
                        parsed: Some(Vec::with_capacity(unit_count)),
                        next_group_type: None,
                    },
                    |mut acc, unit| {
                        unit.map(|unit| {
                            acc.parsed.as_mut().unwrap().push(unit.parsed);
                            acc.next_group_type = unit.next_group_type;
                            acc
                        })
                    },
                )
        })
        .transform_parsed(to_expr)
}

fn parse_double_unit<ParseUnit: Parse, ParseUnit2: Parse, ResultExpr>(
    to_expr: fn((ParseUnit, ParseUnit2)) -> ResultExpr,
    action_expr_chain_gen: &ActionExprChainGenerator,
    input: ParseStream<'_>,
) -> UnitResult<ResultExpr> {
    action_expr_chain_gen.parse_unit(input, false).and_then(
        |Unit {
             parsed,
             next_group_type,
         }| {
            if next_group_type.is_some() {
                Err(input.error("Expected 2 expressions, found group identifier!"))
            } else {
                input.parse::<syn::Token![,]>()?;
                action_expr_chain_gen
                    .parse_unit(input, false)
                    .transform_parsed(|parsed2| to_expr((parsed, parsed2)))
            }
        },
    )
}

macro_rules! from_empty_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_empty_unit($action_expr_chain_gen, $input).transform_parsed(|_| $create_expr))
    };
}

macro_rules! from_single_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_single_unit(
            $create_expr,
            $action_expr_chain_gen,
            $input,
        ))
    };
}

macro_rules! from_single_or_empty_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_single_or_empty_unit(
            $create_expr,
            $action_expr_chain_gen,
            $input,
        ))
    };
}

macro_rules! from_four_or_empty_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_n_or_empty_unit(
            $create_expr,
            $action_expr_chain_gen,
            $input,
            4,
        ))
    };
}

macro_rules! from_double_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_double_unit(
            $create_expr,
            $action_expr_chain_gen,
            $input,
        ))
    };
}

///
/// Defines group type predicates and parser functions.
///
impl CommandGroup {
    ///
    /// Returns true if self command group is of `ProcessExpr` type.
    ///
    pub fn is_process_expr(self) -> bool {
        !self.is_err_expr() && !self.is_initial_expr()
    }

    ///
    /// Returns true if self command group is of `ErrExpr` type.
    ///
    pub fn is_err_expr(self) -> bool {
        match self {
            CommandGroup::Or => true,
            CommandGroup::OrElse => true,
            CommandGroup::MapErr => true,
            _ => false,
        }
    }

    ///
    /// Returns true if self command group is of `InitialExpr` type.
    ///
    pub fn is_initial_expr(self) -> bool {
        match self {
            CommandGroup::Initial => true,
            _ => false,
        }
    }

    pub fn can_be_wrapper(self) -> bool {
        match self {
            CommandGroup::Map => true,
            CommandGroup::AndThen => true,
            CommandGroup::Filter => true,
            CommandGroup::Flatten => true,
            CommandGroup::Dot => true,
            CommandGroup::Then => true,
            CommandGroup::Inspect => true,
            CommandGroup::Chain => true,
            CommandGroup::FilterMap => true,
            CommandGroup::Find => true,
            CommandGroup::FindMap => true,
            CommandGroup::Partition => true,
            CommandGroup::Zip => true,
            CommandGroup::Or => true,
            CommandGroup::OrElse => true,
            CommandGroup::MapErr => true,
            _ => false,
        }
    }

    pub fn to_process_expr(self, tokens: TokenStream) -> syn::Result<Option<ProcessExpr>> {
        Ok(Some(match self {
            CommandGroup::Map => ProcessExpr::Map(parse2(tokens)?),
            CommandGroup::AndThen => ProcessExpr::AndThen(parse2(tokens)?),
            CommandGroup::Filter => ProcessExpr::Filter(parse2(tokens)?),
            CommandGroup::Flatten => ProcessExpr::Flatten,
            CommandGroup::Dot => ProcessExpr::Dot(parse2(tokens)?),
            CommandGroup::Then => ProcessExpr::Then(parse2(tokens)?),
            CommandGroup::Inspect => ProcessExpr::Inspect(parse2(tokens)?),
            CommandGroup::Chain => ProcessExpr::Chain(parse2(tokens)?),
            CommandGroup::FilterMap => ProcessExpr::FilterMap(parse2(tokens)?),
            CommandGroup::Find => ProcessExpr::Find(parse2(tokens)?),
            CommandGroup::FindMap => ProcessExpr::FindMap(parse2(tokens)?),
            CommandGroup::Partition => ProcessExpr::Partition(parse2(tokens)?),
            CommandGroup::Zip => ProcessExpr::Zip(parse2(tokens)?),
            _ => {
                return Ok(None);
            }
        }))
    }

    ///
    /// Attempts to parse given `TokenStream` as `ErrExpr`.
    ///
    pub fn to_err_expr(self, tokens: TokenStream) -> syn::Result<Option<ErrExpr>> {
        Ok(Some(match self {
            CommandGroup::Or => ErrExpr::Or(parse2(tokens)?),
            CommandGroup::OrElse => ErrExpr::OrElse(parse2(tokens)?),
            CommandGroup::MapErr => ErrExpr::MapErr(parse2(tokens)?),
            _ => {
                return Ok(None);
            }
        }))
    }

    ///
    /// Attempts to parse given `TokenStream` as `InitialExpr`.
    ///
    pub fn to_initial_expr(self, tokens: TokenStream) -> syn::Result<Option<InitialExpr>> {
        Ok(Some(match self {
            CommandGroup::Initial => InitialExpr(parse2(tokens)?),
            _ => {
                return Ok(None);
            }
        }))
    }

    ///
    /// Attempts to parse given `ParseStream` as `ProcessExpr`.
    ///
    #[cfg(not(feature = "full"))]
    pub fn parse_process_expr(
        self,
        action_expr_chain_gen: &ActionExprChainGenerator,
        input: ParseStream,
    ) -> Option<UnitResult<ProcessExpr>> {
        match self {
            CommandGroup::Map => from_single_unit!(ProcessExpr::Map, action_expr_chain_gen, input),
            CommandGroup::AndThen => {
                from_single_unit!(ProcessExpr::AndThen, action_expr_chain_gen, input)
            }
            CommandGroup::Filter => {
                from_single_unit!(ProcessExpr::Filter, action_expr_chain_gen, input)
            }
            CommandGroup::Flatten => {
                from_empty_unit!(ProcessExpr::Flatten, action_expr_chain_gen, input)
            }
            CommandGroup::Dot => from_single_unit!(ProcessExpr::Dot, action_expr_chain_gen, input),
            CommandGroup::Then => {
                from_single_unit!(ProcessExpr::Then, action_expr_chain_gen, input)
            }
            CommandGroup::Inspect => {
                from_single_unit!(ProcessExpr::Inspect, action_expr_chain_gen, input)
            }
            CommandGroup::Chain => {
                from_single_unit!(ProcessExpr::Chain, action_expr_chain_gen, input)
            }
            CommandGroup::Collect => {
                from_single_or_empty_unit!(ProcessExpr::Collect, action_expr_chain_gen, input)
            }
            CommandGroup::Enumerate => {
                from_empty_unit!(ProcessExpr::Enumerate, action_expr_chain_gen, input)
            }
            CommandGroup::FilterMap => {
                from_single_unit!(ProcessExpr::FilterMap, action_expr_chain_gen, input)
            }
            CommandGroup::Find => {
                from_single_unit!(ProcessExpr::Find, action_expr_chain_gen, input)
            }
            CommandGroup::Fold => {
                from_double_unit!(ProcessExpr::Fold, action_expr_chain_gen, input)
            }
            CommandGroup::FindMap => {
                from_single_unit!(ProcessExpr::FindMap, action_expr_chain_gen, input)
            }
            CommandGroup::Partition => {
                from_single_unit!(ProcessExpr::Partition, action_expr_chain_gen, input)
            }
            CommandGroup::TryFold => {
                from_double_unit!(ProcessExpr::TryFold, action_expr_chain_gen, input)
            }
            CommandGroup::Unzip => {
                from_four_or_empty_unit!(ProcessExpr::Unzip, action_expr_chain_gen, input)
            }
            CommandGroup::Zip => from_single_unit!(ProcessExpr::Zip, action_expr_chain_gen, input),
            CommandGroup::UNWRAP => {
                from_empty_unit!(ProcessExpr::UNWRAP, action_expr_chain_gen, input)
            }
            _ => None,
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `ProcessExpr`.
    ///
    #[cfg(feature = "full")]
    pub fn parse_process_expr(
        self,
        action_expr_chain_gen: &ActionExprChainGenerator,
        input: ParseStream,
    ) -> Option<UnitResult<ProcessExpr>> {
        match self {
            CommandGroup::Map => from_single_unit!(ProcessExpr::Map, action_expr_chain_gen, input),
            CommandGroup::AndThen => {
                from_single_unit!(ProcessExpr::AndThen, action_expr_chain_gen, input)
            }
            CommandGroup::Filter => {
                from_single_unit!(ProcessExpr::Filter, action_expr_chain_gen, input)
            }
            CommandGroup::Dot => from_single_unit!(ProcessExpr::Dot, action_expr_chain_gen, input),
            CommandGroup::Then => {
                from_single_unit!(ProcessExpr::Then, action_expr_chain_gen, input)
            }
            CommandGroup::Inspect => {
                from_single_unit!(ProcessExpr::Inspect, action_expr_chain_gen, input)
            }
            CommandGroup::All => from_single_unit!(ProcessExpr::All, action_expr_chain_gen, input),
            CommandGroup::Any => from_single_unit!(ProcessExpr::Any, action_expr_chain_gen, input),
            CommandGroup::ByRef => {
                from_empty_unit!(ProcessExpr::ByRef, action_expr_chain_gen, input)
            }
            CommandGroup::Chain => {
                from_single_unit!(ProcessExpr::Chain, action_expr_chain_gen, input)
            }
            CommandGroup::Cloned => {
                from_empty_unit!(ProcessExpr::Cloned, action_expr_chain_gen, input)
            }
            CommandGroup::Cmp => from_single_unit!(ProcessExpr::Cmp, action_expr_chain_gen, input),
            CommandGroup::Collect => {
                from_single_or_empty_unit!(ProcessExpr::Collect, action_expr_chain_gen, input)
            }
            CommandGroup::Copied => {
                from_empty_unit!(ProcessExpr::Copied, action_expr_chain_gen, input)
            }
            CommandGroup::Count => {
                from_empty_unit!(ProcessExpr::Count, action_expr_chain_gen, input)
            }
            CommandGroup::Cycle => {
                from_empty_unit!(ProcessExpr::Cycle, action_expr_chain_gen, input)
            }
            CommandGroup::Enumerate => {
                from_empty_unit!(ProcessExpr::Enumerate, action_expr_chain_gen, input)
            }
            CommandGroup::Eq => from_single_unit!(ProcessExpr::Eq, action_expr_chain_gen, input),
            CommandGroup::FilterMap => {
                from_single_unit!(ProcessExpr::FilterMap, action_expr_chain_gen, input)
            }
            CommandGroup::Find => {
                from_single_unit!(ProcessExpr::Find, action_expr_chain_gen, input)
            }
            CommandGroup::FindMap => {
                from_single_unit!(ProcessExpr::FindMap, action_expr_chain_gen, input)
            }
            CommandGroup::FlatMap => {
                from_single_unit!(ProcessExpr::FlatMap, action_expr_chain_gen, input)
            }
            CommandGroup::Flatten => {
                from_empty_unit!(ProcessExpr::Flatten, action_expr_chain_gen, input)
            }
            CommandGroup::Fold => {
                from_double_unit!(ProcessExpr::Fold, action_expr_chain_gen, input)
            }
            CommandGroup::ForEach => {
                from_single_unit!(ProcessExpr::ForEach, action_expr_chain_gen, input)
            }
            CommandGroup::Fuse => from_empty_unit!(ProcessExpr::Fuse, action_expr_chain_gen, input),
            CommandGroup::Ge => from_single_unit!(ProcessExpr::Ge, action_expr_chain_gen, input),
            CommandGroup::Gt => from_single_unit!(ProcessExpr::Gt, action_expr_chain_gen, input),
            CommandGroup::IsSorted => {
                from_empty_unit!(ProcessExpr::IsSorted, action_expr_chain_gen, input)
            }
            CommandGroup::IsSortedBy => {
                from_single_unit!(ProcessExpr::IsSortedBy, action_expr_chain_gen, input)
            }
            CommandGroup::IsSortedByKey => {
                from_single_unit!(ProcessExpr::IsSortedByKey, action_expr_chain_gen, input)
            }
            CommandGroup::IsPartitioned => {
                from_empty_unit!(ProcessExpr::IsPartitioned, action_expr_chain_gen, input)
            }
            CommandGroup::Last => from_empty_unit!(ProcessExpr::Last, action_expr_chain_gen, input),
            CommandGroup::Le => from_single_unit!(ProcessExpr::Le, action_expr_chain_gen, input),
            CommandGroup::Lt => from_single_unit!(ProcessExpr::Lt, action_expr_chain_gen, input),
            CommandGroup::Max => from_empty_unit!(ProcessExpr::Max, action_expr_chain_gen, input),
            CommandGroup::MaxBy => {
                from_single_unit!(ProcessExpr::MaxBy, action_expr_chain_gen, input)
            }
            CommandGroup::MaxByKey => {
                from_single_unit!(ProcessExpr::MaxByKey, action_expr_chain_gen, input)
            }
            CommandGroup::Min => from_empty_unit!(ProcessExpr::Min, action_expr_chain_gen, input),
            CommandGroup::MinBy => {
                from_single_unit!(ProcessExpr::MinBy, action_expr_chain_gen, input)
            }
            CommandGroup::MinByKey => {
                from_single_unit!(ProcessExpr::MinByKey, action_expr_chain_gen, input)
            }
            CommandGroup::Ne => from_single_unit!(ProcessExpr::Ne, action_expr_chain_gen, input),
            CommandGroup::Nth => from_single_unit!(ProcessExpr::Nth, action_expr_chain_gen, input),
            CommandGroup::PartialCmp => {
                from_single_unit!(ProcessExpr::PartialCmp, action_expr_chain_gen, input)
            }
            CommandGroup::Partition => {
                from_single_unit!(ProcessExpr::Partition, action_expr_chain_gen, input)
            }
            CommandGroup::PartitionInPlace => {
                from_single_unit!(ProcessExpr::PartitionInPlace, action_expr_chain_gen, input)
            }
            CommandGroup::Peekable => {
                from_empty_unit!(ProcessExpr::Peekable, action_expr_chain_gen, input)
            }
            CommandGroup::Position => {
                from_single_unit!(ProcessExpr::Position, action_expr_chain_gen, input)
            }
            CommandGroup::Product => {
                from_empty_unit!(ProcessExpr::Product, action_expr_chain_gen, input)
            }
            CommandGroup::Rev => from_empty_unit!(ProcessExpr::Rev, action_expr_chain_gen, input),
            CommandGroup::Rposition => {
                from_single_unit!(ProcessExpr::Rposition, action_expr_chain_gen, input)
            }
            CommandGroup::Scan => {
                from_single_unit!(ProcessExpr::Scan, action_expr_chain_gen, input)
            }
            CommandGroup::SizeHint => {
                from_empty_unit!(ProcessExpr::SizeHint, action_expr_chain_gen, input)
            }
            CommandGroup::Skip => {
                from_single_unit!(ProcessExpr::Skip, action_expr_chain_gen, input)
            }
            CommandGroup::SkipWhile => {
                from_single_unit!(ProcessExpr::SkipWhile, action_expr_chain_gen, input)
            }
            CommandGroup::StepBy => {
                from_single_unit!(ProcessExpr::StepBy, action_expr_chain_gen, input)
            }
            CommandGroup::Sum => from_empty_unit!(ProcessExpr::Sum, action_expr_chain_gen, input),
            CommandGroup::Take => {
                from_single_unit!(ProcessExpr::Take, action_expr_chain_gen, input)
            }
            CommandGroup::TakeWhile => {
                from_single_unit!(ProcessExpr::TakeWhile, action_expr_chain_gen, input)
            }
            CommandGroup::TryFold => {
                from_double_unit!(ProcessExpr::TryFold, action_expr_chain_gen, input)
            }
            CommandGroup::TryForEach => {
                from_single_unit!(ProcessExpr::TryForEach, action_expr_chain_gen, input)
            }
            CommandGroup::Unzip => {
                from_four_or_empty_unit!(ProcessExpr::Unzip, action_expr_chain_gen, input)
            }
            CommandGroup::Zip => from_single_unit!(ProcessExpr::Zip, action_expr_chain_gen, input),
            _ => None,
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `ErrExpr`.
    ///
    pub fn parse_err_expr(
        self,
        action_expr_chain_gen: &ActionExprChainGenerator,
        input: ParseStream,
    ) -> Option<UnitResult<ErrExpr>> {
        match self {
            CommandGroup::Or => from_single_unit!(ErrExpr::Or, action_expr_chain_gen, input),
            CommandGroup::OrElse => {
                from_single_unit!(ErrExpr::OrElse, action_expr_chain_gen, input)
            }
            CommandGroup::MapErr => {
                from_single_unit!(ErrExpr::MapErr, action_expr_chain_gen, input)
            }
            _ => None,
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `InitialExpr`.
    ///
    pub fn parse_initial_expr(
        self,
        action_expr_chain_gen: &ActionExprChainGenerator,
        input: ParseStream,
    ) -> Option<UnitResult<InitialExpr>> {
        match self {
            CommandGroup::Initial => from_single_unit!(InitialExpr, action_expr_chain_gen, input),
            _ => None,
        }
    }

    pub fn parse_empty_expr(
        self,
        action_expr_chain_gen: &ActionExprChainGenerator,
        input: ParseStream,
    ) -> Option<UnitResult<Empty>> {
        Some(parse_empty_unit(action_expr_chain_gen, input))
    }
}

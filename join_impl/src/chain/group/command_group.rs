//!
//! Definition of `CommandGroup` with parsers for all possible groups.
//!
//!
use proc_macro2::TokenStream;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::{parse2, Token};

use crate::chain::expr::{ErrExpr, InitialExpr, ProcessExpr};
use crate::chain::group::ActionGroup;
use crate::parse::unit::{MapParsed, ParseUnit, Unit, UnitResult};

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
    /// UNWRAP (Special `CommandGroup` used to define that next group position is by one level up - which will be `#value.and_then(#previous_expr).#next_expr` )
    UNWRAP,
}

///
/// All possible command groups.
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
    /// UNWRAP (Special `CommandGroup` used to define that next group position is by one level up - which will be `#value.and_then(#previous_expr).#next_expr` )
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

///
/// Struct which parses empty `ParseStream` as `Ok`, non-empty as `Err`.
///
pub struct Empty;

impl Parse for Empty {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            Ok(Self)
        } else {
            Err(input.error("Unexpected tokens"))
        }
    }
}

fn to_none<T, R>(_: T) -> Option<R> {
    None
}

fn parse_empty_unit(
    action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
    input: ParseStream<'_>,
) -> UnitResult<Empty, ActionGroup> {
    action_expr_chain_gen.parse_unit(input, true)
}

fn parse_single_unit<P: Parse, ResultExpr>(
    to_expr: fn(P) -> ResultExpr,
    action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
    input: ParseStream<'_>,
) -> UnitResult<ResultExpr, ActionGroup> {
    action_expr_chain_gen
        .parse_unit::<P>(input, false)
        .map_parsed(to_expr)
}

fn parse_single_or_empty_unit<P: Parse, ResultExpr>(
    to_expr: fn(Option<P>) -> ResultExpr,
    action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
    input: ParseStream<'_>,
) -> UnitResult<ResultExpr, ActionGroup> {
    action_expr_chain_gen
        .parse_unit::<Empty>(&input.fork(), true)
        .and_then(|_| action_expr_chain_gen.parse_unit::<Empty>(&input, true))
        .map_parsed(to_none)
        .or_else(|_| {
            action_expr_chain_gen
                .parse_unit::<P>(input, false)
                .map_parsed(Some)
        })
        .map_parsed(to_expr)
}

fn parse_double_unit<P: Parse, P2: Parse, ResultExpr>(
    to_expr: fn((P, P2)) -> ResultExpr,
    action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
    input: ParseStream<'_>,
) -> UnitResult<ResultExpr, ActionGroup> {
    action_expr_chain_gen
        .parse_unit(input, false)
        .and_then(|Unit { parsed, next }| {
            if next.is_some() {
                Err(input.error("Expected 2 expressions, found group identifier!"))
            } else {
                input.parse::<syn::Token![,]>()?;
                action_expr_chain_gen
                    .parse_unit(input, false)
                    .map_parsed(|parsed2| to_expr((parsed, parsed2)))
            }
        })
}

macro_rules! from_empty_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_empty_unit($action_expr_chain_gen, $input).map_parsed(|_| $create_expr))
    };
}

macro_rules! from_single_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_single_unit(
            |expr| $create_expr([expr]),
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

macro_rules! iter_to_tuple {
    (1, $value: expr) => {{ let value = $value; value.next().expect("join: Unexpected None when constructing tuple from Iterator. This's a bug, please report it") }};
    (2, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (3, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (4, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (5, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (6, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (7, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (8, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
    (9, $value: expr) => {{ let mut value = $value; (iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value), iter_to_tuple!(1, &mut value)) }};
}

macro_rules! define_args {
    (1, $type: ty) => {
        ($type)
    };
    (2, $type: ty) => {
        ($type, $type)
    };
    (3, $type: ty) => {
        ($type, $type, $type)
    };
    (4, $type: ty) => {
        ($type, $type, $type, $type)
    };
    (5, $type: ty) => {
        ($type, $type, $type, $type, $type)
    };
    (6, $type: ty) => {
        ($type, $type, $type, $type, $type, $type)
    };
    (7, $type: ty) => {
        ($type, $type, $type, $type, $type, $type, $type)
    };
    (8, $type: ty) => {
        ($type, $type, $type, $type, $type, $type, $type, $type)
    };
    (9, $type: ty) => {
        (
            $type, $type, $type, $type, $type, $type, $type, $type, $type,
        )
    };
}

macro_rules! from_n_or_empty_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr, $unit_count: tt) => {{
        fn parse_n_or_empty_unit<P: Parse, ResultExpr>(
            to_expr: fn(Option<define_args!($unit_count, P)>) -> ResultExpr,
            action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
            input: ParseStream<'_>
        ) -> UnitResult<ResultExpr, ActionGroup> {
            let unit_count = $unit_count;
            action_expr_chain_gen
                .parse_unit::<Empty>(&input.fork(), true)
                .and_then(|_| action_expr_chain_gen.parse_unit::<Empty>(&input, true))
                .map_parsed(to_none)
                .or_else(|_| {
                    (0..unit_count)
                        .map(|index| {
                            action_expr_chain_gen
                                .parse_unit::<P>(input, false)
                                .and_then(|unit| {
                                    if index + 1 < unit_count {
                                        input.parse::<Token![,]>().and_then(|_| {
                                            if unit.next.is_none() {
                                                Ok(unit)
                                            } else {
                                                Err(input.error(&format!(
                                                    "Expected {} units, found group identifier!",
                                                    unit_count
                                                )))
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
                                next: None,
                            },
                            |mut acc, unit| {
                                unit.map(|unit| {
                                    acc.parsed.as_mut().unwrap().push(unit.parsed);
                                    acc.next = unit.next;
                                    acc
                                })
                            },
                        )
                })
                .map_parsed(|parsed| parsed.map(|parsed_vec| iter_to_tuple!($unit_count, parsed_vec.into_iter())))
                .map_parsed(to_expr)
        }

        Some(parse_n_or_empty_unit(
            $create_expr,
            $action_expr_chain_gen,
            $input
        ))
    }};
}

macro_rules! from_double_unit {
    ($create_expr: path, $action_expr_chain_gen: expr, $input: expr) => {
        Some(parse_double_unit(
            |(expr1, expr2)| $create_expr([expr1, expr2]),
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
    /// Returns `true` if self command group is of `ProcessExpr` type.
    ///
    pub fn is_process_expr(self) -> bool {
        !self.is_err_expr() && !self.is_initial_expr()
    }

    ///
    /// Returns `true` if self command group is of `ErrExpr` type.
    ///
    pub fn is_err_expr(self) -> bool {
        matches!(self, Self::Or | Self::OrElse | Self::MapErr)
    }

    ///
    /// Returns `true` if self command group is of `InitialExpr` type.
    ///
    pub fn is_initial_expr(self) -> bool {
        matches!(self, Self::Initial)
    }

    ///
    /// Returns `true` if command group can be a wrapper.
    ///
    pub fn can_be_wrapper(self) -> bool {
        matches!(
            self,
            Self::Map
                | Self::AndThen
                | Self::Filter
                | Self::Inspect
                | Self::FilterMap
                | Self::Find
                | Self::FindMap
                | Self::Partition
                | Self::OrElse
                | Self::MapErr
        )
    }

    ///
    /// Attempts to parse given `TokenStream` as `ProcessExpr`.
    ///
    // TODO: implement for full list of process expr.
    pub fn to_process_expr(self, tokens: TokenStream) -> syn::Result<Option<ProcessExpr>> {
        Ok(Some(match self {
            Self::Map => ProcessExpr::Map([parse2(tokens)?]),
            Self::AndThen => ProcessExpr::AndThen([parse2(tokens)?]),
            Self::Filter => ProcessExpr::Filter([parse2(tokens)?]),
            Self::Then => ProcessExpr::Then([parse2(tokens)?]),
            Self::Inspect => ProcessExpr::Inspect([parse2(tokens)?]),
            Self::Chain => ProcessExpr::Chain([parse2(tokens)?]),
            Self::FilterMap => ProcessExpr::FilterMap([parse2(tokens)?]),
            Self::Find => ProcessExpr::Find([parse2(tokens)?]),
            Self::FindMap => ProcessExpr::FindMap([parse2(tokens)?]),
            Self::Partition => ProcessExpr::Partition([parse2(tokens)?]),
            Self::Zip => ProcessExpr::Zip([parse2(tokens)?]),
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
            Self::Or => ErrExpr::Or([parse2(tokens)?]),
            Self::OrElse => ErrExpr::OrElse([parse2(tokens)?]),
            Self::MapErr => ErrExpr::MapErr([parse2(tokens)?]),
            _ => {
                return Ok(None);
            }
        }))
    }

    ///
    /// Attempts to parse given `TokenStream` as `InitialExpr`.
    ///
    pub fn to_initial_expr(self, tokens: TokenStream) -> syn::Result<Option<InitialExpr>> {
        Ok(match self {
            Self::Initial => Some(InitialExpr::Single([parse2(tokens)?])),
            _ => None,
        })
    }

    ///
    /// Attempts to parse given `ParseStream` as `ProcessExpr`.
    ///
    #[cfg(not(feature = "full"))]
    pub fn parse_process_expr(
        self,
        action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> Option<UnitResult<ProcessExpr, ActionGroup>> {
        match self {
            Self::Map => from_single_unit!(ProcessExpr::Map, action_expr_chain_gen, input),
            Self::AndThen => from_single_unit!(ProcessExpr::AndThen, action_expr_chain_gen, input),
            Self::Filter => from_single_unit!(ProcessExpr::Filter, action_expr_chain_gen, input),
            Self::Flatten => from_empty_unit!(ProcessExpr::Flatten, action_expr_chain_gen, input),
            Self::Dot => from_single_unit!(ProcessExpr::Dot, action_expr_chain_gen, input),
            Self::Then => from_single_unit!(ProcessExpr::Then, action_expr_chain_gen, input),
            Self::Inspect => from_single_unit!(ProcessExpr::Inspect, action_expr_chain_gen, input),
            Self::Chain => from_single_unit!(ProcessExpr::Chain, action_expr_chain_gen, input),
            Self::Collect => {
                from_single_or_empty_unit!(ProcessExpr::Collect, action_expr_chain_gen, input)
            }
            Self::Enumerate => {
                from_empty_unit!(ProcessExpr::Enumerate, action_expr_chain_gen, input)
            }
            Self::FilterMap => {
                from_single_unit!(ProcessExpr::FilterMap, action_expr_chain_gen, input)
            }
            Self::Find => from_single_unit!(ProcessExpr::Find, action_expr_chain_gen, input),
            Self::Fold => from_double_unit!(ProcessExpr::Fold, action_expr_chain_gen, input),
            Self::FindMap => from_single_unit!(ProcessExpr::FindMap, action_expr_chain_gen, input),
            Self::Partition => {
                from_single_unit!(ProcessExpr::Partition, action_expr_chain_gen, input)
            }
            Self::TryFold => from_double_unit!(ProcessExpr::TryFold, action_expr_chain_gen, input),
            Self::Unzip => {
                from_n_or_empty_unit!(ProcessExpr::Unzip, action_expr_chain_gen, input, 4)
            }
            Self::Zip => from_single_unit!(ProcessExpr::Zip, action_expr_chain_gen, input),
            Self::UNWRAP => from_empty_unit!(ProcessExpr::UNWRAP, action_expr_chain_gen, input),
            _ => None,
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `ProcessExpr`.
    ///
    #[cfg(feature = "full")]
    pub fn parse_process_expr(
        self,
        action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> Option<UnitResult<ProcessExpr, ActionGroup>> {
        match self {
            Self::Map => from_single_unit!(ProcessExpr::Map, action_expr_chain_gen, input),
            Self::AndThen => from_single_unit!(ProcessExpr::AndThen, action_expr_chain_gen, input),
            Self::Filter => from_single_unit!(ProcessExpr::Filter, action_expr_chain_gen, input),
            Self::Dot => from_single_unit!(ProcessExpr::Dot, action_expr_chain_gen, input),
            Self::Then => from_single_unit!(ProcessExpr::Then, action_expr_chain_gen, input),
            Self::Inspect => from_single_unit!(ProcessExpr::Inspect, action_expr_chain_gen, input),
            Self::All => from_single_unit!(ProcessExpr::All, action_expr_chain_gen, input),
            Self::Any => from_single_unit!(ProcessExpr::Any, action_expr_chain_gen, input),
            Self::ByRef => from_empty_unit!(ProcessExpr::ByRef, action_expr_chain_gen, input),
            Self::Chain => from_single_unit!(ProcessExpr::Chain, action_expr_chain_gen, input),
            Self::Cloned => from_empty_unit!(ProcessExpr::Cloned, action_expr_chain_gen, input),
            Self::Cmp => from_single_unit!(ProcessExpr::Cmp, action_expr_chain_gen, input),
            Self::Collect => {
                from_single_or_empty_unit!(ProcessExpr::Collect, action_expr_chain_gen, input)
            }
            Self::Copied => from_empty_unit!(ProcessExpr::Copied, action_expr_chain_gen, input),
            Self::Count => from_empty_unit!(ProcessExpr::Count, action_expr_chain_gen, input),
            Self::Cycle => from_empty_unit!(ProcessExpr::Cycle, action_expr_chain_gen, input),
            Self::Enumerate => {
                from_empty_unit!(ProcessExpr::Enumerate, action_expr_chain_gen, input)
            }
            Self::Eq => from_single_unit!(ProcessExpr::Eq, action_expr_chain_gen, input),
            Self::FilterMap => {
                from_single_unit!(ProcessExpr::FilterMap, action_expr_chain_gen, input)
            }
            Self::Find => from_single_unit!(ProcessExpr::Find, action_expr_chain_gen, input),
            Self::FindMap => from_single_unit!(ProcessExpr::FindMap, action_expr_chain_gen, input),
            Self::FlatMap => from_single_unit!(ProcessExpr::FlatMap, action_expr_chain_gen, input),
            Self::Flatten => from_empty_unit!(ProcessExpr::Flatten, action_expr_chain_gen, input),
            Self::Fold => from_double_unit!(ProcessExpr::Fold, action_expr_chain_gen, input),
            Self::ForEach => from_single_unit!(ProcessExpr::ForEach, action_expr_chain_gen, input),
            Self::Fuse => from_empty_unit!(ProcessExpr::Fuse, action_expr_chain_gen, input),
            Self::Ge => from_single_unit!(ProcessExpr::Ge, action_expr_chain_gen, input),
            Self::Gt => from_single_unit!(ProcessExpr::Gt, action_expr_chain_gen, input),
            Self::IsSorted => from_empty_unit!(ProcessExpr::IsSorted, action_expr_chain_gen, input),
            Self::IsSortedBy => {
                from_single_unit!(ProcessExpr::IsSortedBy, action_expr_chain_gen, input)
            }
            Self::IsSortedByKey => {
                from_single_unit!(ProcessExpr::IsSortedByKey, action_expr_chain_gen, input)
            }
            Self::IsPartitioned => {
                from_empty_unit!(ProcessExpr::IsPartitioned, action_expr_chain_gen, input)
            }
            Self::Last => from_empty_unit!(ProcessExpr::Last, action_expr_chain_gen, input),
            Self::Le => from_single_unit!(ProcessExpr::Le, action_expr_chain_gen, input),
            Self::Lt => from_single_unit!(ProcessExpr::Lt, action_expr_chain_gen, input),
            Self::Max => from_empty_unit!(ProcessExpr::Max, action_expr_chain_gen, input),
            Self::MaxBy => from_single_unit!(ProcessExpr::MaxBy, action_expr_chain_gen, input),
            Self::MaxByKey => {
                from_single_unit!(ProcessExpr::MaxByKey, action_expr_chain_gen, input)
            }
            Self::Min => from_empty_unit!(ProcessExpr::Min, action_expr_chain_gen, input),
            Self::MinBy => from_single_unit!(ProcessExpr::MinBy, action_expr_chain_gen, input),
            Self::MinByKey => {
                from_single_unit!(ProcessExpr::MinByKey, action_expr_chain_gen, input)
            }
            Self::Ne => from_single_unit!(ProcessExpr::Ne, action_expr_chain_gen, input),
            Self::Nth => from_single_unit!(ProcessExpr::Nth, action_expr_chain_gen, input),
            Self::PartialCmp => {
                from_single_unit!(ProcessExpr::PartialCmp, action_expr_chain_gen, input)
            }
            Self::Partition => {
                from_single_unit!(ProcessExpr::Partition, action_expr_chain_gen, input)
            }
            Self::PartitionInPlace => {
                from_single_unit!(ProcessExpr::PartitionInPlace, action_expr_chain_gen, input)
            }
            Self::Peekable => from_empty_unit!(ProcessExpr::Peekable, action_expr_chain_gen, input),
            Self::Position => {
                from_single_unit!(ProcessExpr::Position, action_expr_chain_gen, input)
            }
            Self::Product => from_empty_unit!(ProcessExpr::Product, action_expr_chain_gen, input),
            Self::Rev => from_empty_unit!(ProcessExpr::Rev, action_expr_chain_gen, input),
            Self::Rposition => {
                from_single_unit!(ProcessExpr::Rposition, action_expr_chain_gen, input)
            }
            Self::Scan => from_single_unit!(ProcessExpr::Scan, action_expr_chain_gen, input),
            Self::SizeHint => from_empty_unit!(ProcessExpr::SizeHint, action_expr_chain_gen, input),
            Self::Skip => from_single_unit!(ProcessExpr::Skip, action_expr_chain_gen, input),
            Self::SkipWhile => {
                from_single_unit!(ProcessExpr::SkipWhile, action_expr_chain_gen, input)
            }
            Self::StepBy => from_single_unit!(ProcessExpr::StepBy, action_expr_chain_gen, input),
            Self::Sum => from_empty_unit!(ProcessExpr::Sum, action_expr_chain_gen, input),
            Self::Take => from_single_unit!(ProcessExpr::Take, action_expr_chain_gen, input),
            Self::TakeWhile => {
                from_single_unit!(ProcessExpr::TakeWhile, action_expr_chain_gen, input)
            }
            Self::TryFold => from_double_unit!(ProcessExpr::TryFold, action_expr_chain_gen, input),
            Self::TryForEach => {
                from_single_unit!(ProcessExpr::TryForEach, action_expr_chain_gen, input)
            }
            Self::Unzip => {
                from_n_or_empty_unit!(ProcessExpr::Unzip, action_expr_chain_gen, input, 4)
            }
            Self::Zip => from_single_unit!(ProcessExpr::Zip, action_expr_chain_gen, input),
            Self::UNWRAP => from_empty_unit!(ProcessExpr::UNWRAP, action_expr_chain_gen, input),
            _ => None,
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `ErrExpr`.
    ///
    pub fn parse_err_expr(
        self,
        action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> Option<UnitResult<ErrExpr, ActionGroup>> {
        match self {
            Self::Or => from_single_unit!(ErrExpr::Or, action_expr_chain_gen, input),
            Self::OrElse => from_single_unit!(ErrExpr::OrElse, action_expr_chain_gen, input),
            Self::MapErr => from_single_unit!(ErrExpr::MapErr, action_expr_chain_gen, input),
            _ => None,
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `InitialExpr`.
    ///
    pub fn parse_initial_expr(
        self,
        action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> Option<UnitResult<InitialExpr, ActionGroup>> {
        match self {
            Self::Initial => from_single_unit!(InitialExpr::Single, action_expr_chain_gen, input),
            _ => None,
        }
    }

    ///
    /// Attempts to parse any `ActionGroup` type.
    ///
    pub fn parse_empty_expr(
        self,
        action_expr_chain_gen: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> Option<UnitResult<Empty, ActionGroup>> {
        Some(parse_empty_unit(action_expr_chain_gen, input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_tests_can_be_wrapper() {
        assert!(CommandGroup::Map.can_be_wrapper());
        assert!(CommandGroup::AndThen.can_be_wrapper());
        assert!(CommandGroup::Filter.can_be_wrapper());
        assert!(CommandGroup::Inspect.can_be_wrapper());
        assert!(CommandGroup::FilterMap.can_be_wrapper());
        assert!(CommandGroup::Find.can_be_wrapper());
        assert!(CommandGroup::FindMap.can_be_wrapper());
        assert!(CommandGroup::Partition.can_be_wrapper());
        assert!(CommandGroup::OrElse.can_be_wrapper());
        assert!(CommandGroup::MapErr.can_be_wrapper());

        assert!(!CommandGroup::Flatten.can_be_wrapper());
        assert!(!CommandGroup::Dot.can_be_wrapper());
        assert!(!CommandGroup::Then.can_be_wrapper());
        assert!(!CommandGroup::Chain.can_be_wrapper());
        assert!(!CommandGroup::Collect.can_be_wrapper());
        assert!(!CommandGroup::Enumerate.can_be_wrapper());
        assert!(!CommandGroup::Fold.can_be_wrapper());
        assert!(!CommandGroup::TryFold.can_be_wrapper());
        assert!(!CommandGroup::Unzip.can_be_wrapper());
        assert!(!CommandGroup::Zip.can_be_wrapper());
        assert!(!CommandGroup::UNWRAP.can_be_wrapper());
    }

    // TODO: more tests
}

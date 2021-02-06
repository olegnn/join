//!
//! Definition of `ActionGroup` with parser's mappings.
//!

use super::{ApplicationType, Combinator, ExprGroup, MoveType};
use crate::chain::expr::{ActionExpr, ErrExpr, InitialExpr, ProcessExpr};
use crate::parse::empty::Empty;
use crate::parse::unit::{ParseUnit, Unit, UnitResult};
use syn::parse::ParseStream;
use syn::parse_quote;

///
/// `Combinator` with configuration.
///
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ActionGroup {
    pub combinator: Combinator,
    pub application_type: ApplicationType,
    pub move_type: MoveType,
}

impl ActionGroup {
    ///
    /// Creates new `ActionGroup` with provided configuration.
    ///
    pub fn new(
        combinator: Combinator,
        application_type: ApplicationType,
        move_type: MoveType,
    ) -> Self {
        Self {
            combinator,
            application_type,
            move_type,
        }
    }

    ///
    /// Parses `ParseStream` as `ActionExpr` using given `ParseUnit`.
    ///
    pub fn parse_stream(
        &self,
        unit_parser: &impl ParseUnit<ActionGroup>,
        input: ParseStream<'_>,
    ) -> UnitResult<ExprGroup<ActionExpr>, ActionGroup> {
        let &Self {
            combinator,
            move_type,
            ..
        } = self;

        if move_type == MoveType::Wrap {
            self.to_wrapper_action_expr()
                .ok_or_else(|| {
                    input.error(format!("combinator {:?} can't be a wrapper", combinator))
                })
                .and_then(|val| {
                    Ok(Unit {
                        parsed: val,
                        next: unit_parser.parse_unit::<Empty>(input, true)?.next,
                    })
                })
        } else {
            self.parse_action_expr(unit_parser, input)
        }
    }

    ///
    /// Attempts to build a wrapper expression from `Self` using provided `ActionGroup`.
    ///
    fn to_wrapper_action_expr(&self) -> Option<ExprGroup<ActionExpr>> {
        let return_val = parse_quote! { |__v| __v };

        Some(ExprGroup::new(
            match self.combinator {
                Combinator::Map => ActionExpr::Process(ProcessExpr::Map([return_val])),
                Combinator::AndThen => ActionExpr::Process(ProcessExpr::AndThen([return_val])),
                Combinator::Filter => ActionExpr::Process(ProcessExpr::Filter([return_val])),
                Combinator::Inspect => ActionExpr::Process(ProcessExpr::Inspect([return_val])),
                Combinator::FilterMap => ActionExpr::Process(ProcessExpr::FilterMap([return_val])),
                Combinator::Find => ActionExpr::Process(ProcessExpr::Find([return_val])),
                Combinator::FindMap => ActionExpr::Process(ProcessExpr::FindMap([return_val])),
                Combinator::Partition => ActionExpr::Process(ProcessExpr::Partition([return_val])),
                Combinator::OrElse => ActionExpr::Err(ErrExpr::OrElse([return_val])),
                Combinator::MapErr => ActionExpr::Err(ErrExpr::MapErr([return_val])),
                _ => return None,
            },
            *self,
        ))
    }

    ///
    /// Attempts to parse given `ParseStream` as `ProcessExpr`.
    ///
    #[cfg(not(feature = "full"))]
    fn parse_action_expr(
        &self,
        unit_parser: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> UnitResult<ExprGroup<ActionExpr>, ActionGroup> {
        match self.combinator {
            Combinator::Map => {
                ExprGroup::parse_single_unit(ProcessExpr::Map, unit_parser, self, input)
            }
            Combinator::AndThen => {
                ExprGroup::parse_single_unit(ProcessExpr::AndThen, unit_parser, self, input)
            }
            Combinator::Filter => {
                ExprGroup::parse_single_unit(ProcessExpr::Filter, unit_parser, self, input)
            }
            Combinator::Flatten => {
                ExprGroup::parse_empty_unit(ProcessExpr::Flatten, unit_parser, self, input)
            }
            Combinator::Dot => {
                ExprGroup::parse_single_unit(ProcessExpr::Dot, unit_parser, self, input)
            }
            Combinator::Then => {
                ExprGroup::parse_single_unit(ProcessExpr::Then, unit_parser, self, input)
            }
            Combinator::Inspect => {
                ExprGroup::parse_single_unit(ProcessExpr::Inspect, unit_parser, self, input)
            }
            Combinator::Chain => {
                ExprGroup::parse_single_unit(ProcessExpr::Chain, unit_parser, self, input)
            }
            Combinator::Collect => ExprGroup::parse_single_or_empty_unit(
                ProcessExpr::Collect,
                unit_parser,
                self,
                input,
            ),
            Combinator::Enumerate => {
                ExprGroup::parse_empty_unit(ProcessExpr::Enumerate, unit_parser, self, input)
            }
            Combinator::FilterMap => {
                ExprGroup::parse_single_unit(ProcessExpr::FilterMap, unit_parser, self, input)
            }
            Combinator::Find => {
                ExprGroup::parse_single_unit(ProcessExpr::Find, unit_parser, self, input)
            }
            Combinator::Fold => {
                ExprGroup::parse_double_unit(ProcessExpr::Fold, unit_parser, self, input)
            }
            Combinator::FindMap => {
                ExprGroup::parse_single_unit(ProcessExpr::FindMap, unit_parser, self, input)
            }
            Combinator::Partition => {
                ExprGroup::parse_single_unit(ProcessExpr::Partition, unit_parser, self, input)
            }
            Combinator::TryFold => {
                ExprGroup::parse_double_unit(ProcessExpr::TryFold, unit_parser, self, input)
            }
            Combinator::Unzip => {
                ExprGroup::parse_quatro_or_empty_unit(ProcessExpr::Unzip, unit_parser, self, input)
            }
            Combinator::Zip => {
                ExprGroup::parse_single_unit(ProcessExpr::Zip, unit_parser, self, input)
            }
            Combinator::UNWRAP => {
                ExprGroup::parse_empty_unit(ProcessExpr::UNWRAP, unit_parser, self, input)
            }
            Combinator::Or => ExprGroup::parse_single_unit(ErrExpr::Or, unit_parser, self, input),
            Combinator::OrElse => {
                ExprGroup::parse_single_unit(ErrExpr::OrElse, unit_parser, self, input)
            }
            Combinator::MapErr => {
                ExprGroup::parse_single_unit(ErrExpr::MapErr, unit_parser, self, input)
            }
            Combinator::Initial => {
                ExprGroup::parse_single_unit(InitialExpr::Single, unit_parser, self, input)
            }
        }
    }

    ///
    /// Attempts to parse given `ParseStream` as `ProcessExpr`.
    ///
    #[cfg(feature = "full")]
    fn parse_action_expr(
        &self,
        unit_parser: &impl ParseUnit<ActionGroup>,
        input: ParseStream,
    ) -> UnitResult<ExprGroup<ActionExpr>, ActionGroup> {
        match self.combinator {
            Combinator::Map => {
                ExprGroup::parse_single_unit(ProcessExpr::Map, unit_parser, self, input)
            }
            Combinator::AndThen => {
                ExprGroup::parse_single_unit(ProcessExpr::AndThen, unit_parser, self, input)
            }
            Combinator::Filter => {
                ExprGroup::parse_single_unit(ProcessExpr::Filter, unit_parser, self, input)
            }
            Combinator::Dot => {
                ExprGroup::parse_single_unit(ProcessExpr::Dot, unit_parser, self, input)
            }
            Combinator::Then => {
                ExprGroup::parse_single_unit(ProcessExpr::Then, unit_parser, self, input)
            }
            Combinator::Inspect => {
                ExprGroup::parse_single_unit(ProcessExpr::Inspect, unit_parser, self, input)
            }
            Combinator::All => {
                ExprGroup::parse_single_unit(ProcessExpr::All, unit_parser, self, input)
            }
            Combinator::Any => {
                ExprGroup::parse_single_unit(ProcessExpr::Any, unit_parser, self, input)
            }
            Combinator::ByRef => {
                ExprGroup::parse_empty_unit(ProcessExpr::ByRef, unit_parser, self, input)
            }
            Combinator::Chain => {
                ExprGroup::parse_single_unit(ProcessExpr::Chain, unit_parser, self, input)
            }
            Combinator::Cloned => {
                ExprGroup::parse_empty_unit(ProcessExpr::Cloned, unit_parser, self, input)
            }
            Combinator::Cmp => {
                ExprGroup::parse_single_unit(ProcessExpr::Cmp, unit_parser, self, input)
            }
            Combinator::Collect => ExprGroup::parse_single_or_empty_unit(
                ProcessExpr::Collect,
                unit_parser,
                self,
                input,
            ),
            Combinator::Copied => {
                ExprGroup::parse_empty_unit(ProcessExpr::Copied, unit_parser, self, input)
            }
            Combinator::Count => {
                ExprGroup::parse_empty_unit(ProcessExpr::Count, unit_parser, self, input)
            }
            Combinator::Cycle => {
                ExprGroup::parse_empty_unit(ProcessExpr::Cycle, unit_parser, self, input)
            }
            Combinator::Enumerate => {
                ExprGroup::parse_empty_unit(ProcessExpr::Enumerate, unit_parser, self, input)
            }
            Combinator::Eq => {
                ExprGroup::parse_single_unit(ProcessExpr::Eq, unit_parser, self, input)
            }
            Combinator::FilterMap => {
                ExprGroup::parse_single_unit(ProcessExpr::FilterMap, unit_parser, self, input)
            }
            Combinator::Find => {
                ExprGroup::parse_single_unit(ProcessExpr::Find, unit_parser, self, input)
            }
            Combinator::FindMap => {
                ExprGroup::parse_single_unit(ProcessExpr::FindMap, unit_parser, self, input)
            }
            Combinator::FlatMap => {
                ExprGroup::parse_single_unit(ProcessExpr::FlatMap, unit_parser, self, input)
            }
            Combinator::Flatten => {
                ExprGroup::parse_empty_unit(ProcessExpr::Flatten, unit_parser, self, input)
            }
            Combinator::Fold => {
                ExprGroup::parse_double_unit(ProcessExpr::Fold, unit_parser, self, input)
            }
            Combinator::ForEach => {
                ExprGroup::parse_single_unit(ProcessExpr::ForEach, unit_parser, self, input)
            }
            Combinator::Fuse => {
                ExprGroup::parse_empty_unit(ProcessExpr::Fuse, unit_parser, self, input)
            }
            Combinator::Ge => {
                ExprGroup::parse_single_unit(ProcessExpr::Ge, unit_parser, self, input)
            }
            Combinator::Gt => {
                ExprGroup::parse_single_unit(ProcessExpr::Gt, unit_parser, self, input)
            }
            Combinator::IsSorted => {
                ExprGroup::parse_empty_unit(ProcessExpr::IsSorted, unit_parser, self, input)
            }
            Combinator::IsSortedBy => {
                ExprGroup::parse_single_unit(ProcessExpr::IsSortedBy, unit_parser, self, input)
            }
            Combinator::IsSortedByKey => {
                ExprGroup::parse_single_unit(ProcessExpr::IsSortedByKey, unit_parser, self, input)
            }
            Combinator::IsPartitioned => {
                ExprGroup::parse_empty_unit(ProcessExpr::IsPartitioned, unit_parser, self, input)
            }
            Combinator::Last => {
                ExprGroup::parse_empty_unit(ProcessExpr::Last, unit_parser, self, input)
            }
            Combinator::Le => {
                ExprGroup::parse_single_unit(ProcessExpr::Le, unit_parser, self, input)
            }
            Combinator::Lt => {
                ExprGroup::parse_single_unit(ProcessExpr::Lt, unit_parser, self, input)
            }
            Combinator::Max => {
                ExprGroup::parse_empty_unit(ProcessExpr::Max, unit_parser, self, input)
            }
            Combinator::MaxBy => {
                ExprGroup::parse_single_unit(ProcessExpr::MaxBy, unit_parser, self, input)
            }
            Combinator::MaxByKey => {
                ExprGroup::parse_single_unit(ProcessExpr::MaxByKey, unit_parser, self, input)
            }
            Combinator::Min => {
                ExprGroup::parse_empty_unit(ProcessExpr::Min, unit_parser, self, input)
            }
            Combinator::MinBy => {
                ExprGroup::parse_single_unit(ProcessExpr::MinBy, unit_parser, self, input)
            }
            Combinator::MinByKey => {
                ExprGroup::parse_single_unit(ProcessExpr::MinByKey, unit_parser, self, input)
            }
            Combinator::Ne => {
                ExprGroup::parse_single_unit(ProcessExpr::Ne, unit_parser, self, input)
            }
            Combinator::Nth => {
                ExprGroup::parse_single_unit(ProcessExpr::Nth, unit_parser, self, input)
            }
            Combinator::PartialCmp => {
                ExprGroup::parse_single_unit(ProcessExpr::PartialCmp, unit_parser, self, input)
            }
            Combinator::Partition => {
                ExprGroup::parse_single_unit(ProcessExpr::Partition, unit_parser, self, input)
            }
            Combinator::PartitionInPlace => ExprGroup::parse_single_unit(
                ProcessExpr::PartitionInPlace,
                unit_parser,
                self,
                input,
            ),
            Combinator::Peekable => {
                ExprGroup::parse_empty_unit(ProcessExpr::Peekable, unit_parser, self, input)
            }
            Combinator::Position => {
                ExprGroup::parse_single_unit(ProcessExpr::Position, unit_parser, self, input)
            }
            Combinator::Product => {
                ExprGroup::parse_empty_unit(ProcessExpr::Product, unit_parser, self, input)
            }
            Combinator::Rev => {
                ExprGroup::parse_empty_unit(ProcessExpr::Rev, unit_parser, self, input)
            }
            Combinator::Rposition => {
                ExprGroup::parse_single_unit(ProcessExpr::Rposition, unit_parser, self, input)
            }
            Combinator::Scan => {
                ExprGroup::parse_single_unit(ProcessExpr::Scan, unit_parser, self, input)
            }
            Combinator::SizeHint => {
                ExprGroup::parse_empty_unit(ProcessExpr::SizeHint, unit_parser, self, input)
            }
            Combinator::Skip => {
                ExprGroup::parse_single_unit(ProcessExpr::Skip, unit_parser, self, input)
            }
            Combinator::SkipWhile => {
                ExprGroup::parse_single_unit(ProcessExpr::SkipWhile, unit_parser, self, input)
            }
            Combinator::StepBy => {
                ExprGroup::parse_single_unit(ProcessExpr::StepBy, unit_parser, self, input)
            }
            Combinator::Sum => {
                ExprGroup::parse_empty_unit(ProcessExpr::Sum, unit_parser, self, input)
            }
            Combinator::Take => {
                ExprGroup::parse_single_unit(ProcessExpr::Take, unit_parser, self, input)
            }
            Combinator::TakeWhile => {
                ExprGroup::parse_single_unit(ProcessExpr::TakeWhile, unit_parser, self, input)
            }
            Combinator::TryFold => {
                ExprGroup::parse_double_unit(ProcessExpr::TryFold, unit_parser, self, input)
            }
            Combinator::TryForEach => {
                ExprGroup::parse_single_unit(ProcessExpr::TryForEach, unit_parser, self, input)
            }
            Combinator::Unzip => {
                ExprGroup::parse_quatro_or_empty_unit(ProcessExpr::Unzip, unit_parser, self, input)
            }
            Combinator::Zip => {
                ExprGroup::parse_single_unit(ProcessExpr::Zip, unit_parser, self, input)
            }
            Combinator::UNWRAP => {
                ExprGroup::parse_empty_unit(ProcessExpr::UNWRAP, unit_parser, self, input)
            }
            Combinator::Or => ExprGroup::parse_single_unit(ErrExpr::Or, unit_parser, self, input),
            Combinator::OrElse => {
                ExprGroup::parse_single_unit(ErrExpr::OrElse, unit_parser, self, input)
            }
            Combinator::MapErr => {
                ExprGroup::parse_single_unit(ErrExpr::MapErr, unit_parser, self, input)
            }
            Combinator::Initial => {
                ExprGroup::parse_single_unit(InitialExpr::Single, unit_parser, self, input)
            }
        }
    }
}

//!
//! All possible `Combinator`s.
//!

///
/// `Combinator` is an enum of all possible `ProcessExpr`, `ErrExpr` and `InitialExpr` operations.
/// Used to express group which was found in input `ParseStream`.
///
#[cfg(not(feature = "full"))]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Combinator {
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
    /// UNWRAP (Special `Combinator` used to define that next group position is by one level up - which will be `#value.and_then(#previous_expr).#next_expr` )
    UNWRAP,
}

///
/// All possible command groups.
///
#[cfg(feature = "full")]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Combinator {
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
    /// UNWRAP (Special `Combinator` used to define that next group position is by one level up - which will be `#value.and_then(#previous_expr).#next_expr` )
    UNWRAP,
    /// [ProcessExpr::Unzip]
    Unzip,
    /// [ProcessExpr::Zip]
    Zip,
}

impl ::std::fmt::Display for Combinator {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

///
/// Defines group type predicates and parser functions.
///
impl Combinator {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_tests_can_be_wrapper() {
        assert!(Combinator::Map.can_be_wrapper());
        assert!(Combinator::AndThen.can_be_wrapper());
        assert!(Combinator::Filter.can_be_wrapper());
        assert!(Combinator::Inspect.can_be_wrapper());
        assert!(Combinator::FilterMap.can_be_wrapper());
        assert!(Combinator::Find.can_be_wrapper());
        assert!(Combinator::FindMap.can_be_wrapper());
        assert!(Combinator::Partition.can_be_wrapper());
        assert!(Combinator::OrElse.can_be_wrapper());
        assert!(Combinator::MapErr.can_be_wrapper());

        assert!(!Combinator::Flatten.can_be_wrapper());
        assert!(!Combinator::Dot.can_be_wrapper());
        assert!(!Combinator::Then.can_be_wrapper());
        assert!(!Combinator::Chain.can_be_wrapper());
        assert!(!Combinator::Collect.can_be_wrapper());
        assert!(!Combinator::Enumerate.can_be_wrapper());
        assert!(!Combinator::Fold.can_be_wrapper());
        assert!(!Combinator::TryFold.can_be_wrapper());
        assert!(!Combinator::Unzip.can_be_wrapper());
        assert!(!Combinator::Zip.can_be_wrapper());
        assert!(!Combinator::UNWRAP.can_be_wrapper());
    }

    // TODO: more tests
}

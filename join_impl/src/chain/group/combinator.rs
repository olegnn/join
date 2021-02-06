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
    /// [crate::chain::expr::ProcessExpr::Map]
    Map,
    /// [crate::chain::expr::ProcessExpr::Dot]
    Dot,
    /// [crate::chain::expr::ProcessExpr::Filter]
    Filter,
    /// [crate::chain::expr::ProcessExpr::Inspect]
    Inspect,
    /// [crate::chain::expr::ProcessExpr::Then]
    Then,
    /// [crate::chain::expr::ProcessExpr::AndThen]
    AndThen,
    /// [crate::chain::expr::ErrExpr::Or]
    Or,
    /// [crate::chain::expr::ErrExpr::OrElse]
    OrElse,
    /// [crate::chain::expr::ErrExpr::MapErr]
    MapErr,
    /// [crate::chain::expr::InitialExpr]
    Initial,
    /// [crate::chain::expr::ProcessExpr::Chain]
    Chain,
    /// [crate::chain::expr::ProcessExpr::Flatten]
    Flatten,
    /// [crate::chain::expr::ProcessExpr::Collect]
    Collect,
    /// [crate::chain::expr::ProcessExpr::Enumerate]
    Enumerate,
    /// [crate::chain::expr::ProcessExpr::Find]
    Find,
    /// [crate::chain::expr::ProcessExpr::Fold]
    Fold,
    /// [crate::chain::expr::ProcessExpr::TryFold]
    TryFold,
    /// [crate::chain::expr::ProcessExpr::Unzip]
    Unzip,
    /// [crate::chain::expr::ProcessExpr::Zip]
    Zip,
    /// [crate::chain::expr::ProcessExpr::Partition]
    Partition,
    /// [crate::chain::expr::ProcessExpr::FilterMap]
    FilterMap,
    /// [crate::chain::expr::ProcessExpr::FindMap]
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
    /// [crate::chain::expr::ProcessExpr::Map]
    Map,
    /// [crate::chain::expr::ProcessExpr::Dot]
    Dot,
    /// [crate::chain::expr::ProcessExpr::Filter]
    Filter,
    /// [crate::chain::expr::ProcessExpr::Inspect]
    Inspect,
    /// [crate::chain::expr::ProcessExpr::Then]
    Then,
    /// [crate::chain::expr::ProcessExpr::AndThen]
    AndThen,
    /// [crate::chain::expr::ErrExpr::Or]
    Or,
    /// [crate::chain::expr::ErrExpr::OrElse]
    OrElse,
    /// [crate::chain::expr::ErrExpr::MapErr]
    MapErr,
    /// [crate::chain::expr::InitialExpr]
    Initial,
    /// [crate::chain::expr::ProcessExpr::All]
    All,
    /// [crate::chain::expr::ProcessExpr::Any]
    Any,
    /// [crate::chain::expr::ProcessExpr::ByRef]
    ByRef,
    /// [crate::chain::expr::ProcessExpr::Chain]
    Chain,
    /// [crate::chain::expr::ProcessExpr::Cloned]
    Cloned,
    /// [crate::chain::expr::ProcessExpr::Cmp]
    Cmp,
    /// [crate::chain::expr::ProcessExpr::Collect]
    Collect,
    /// [crate::chain::expr::ProcessExpr::Copied]
    Copied,
    /// [crate::chain::expr::ProcessExpr::Count]
    Count,
    /// [crate::chain::expr::ProcessExpr::Cycle]
    Cycle,
    /// [crate::chain::expr::ProcessExpr::Enumerate]
    Enumerate,
    /// [crate::chain::expr::ProcessExpr::Eq]
    Eq,
    /// [crate::chain::expr::ProcessExpr::FilterMap]
    FilterMap,
    /// [crate::chain::expr::ProcessExpr::Find]
    Find,
    /// [crate::chain::expr::ProcessExpr::FindMap]
    FindMap,
    /// [crate::chain::expr::ProcessExpr::FlatMap]
    FlatMap,
    /// [crate::chain::expr::ProcessExpr::Flatten]
    Flatten,
    /// [crate::chain::expr::ProcessExpr::Fold]
    Fold,
    /// [crate::chain::expr::ProcessExpr::ForEach]
    ForEach,
    /// [crate::chain::expr::ProcessExpr::Fuse]
    Fuse,
    /// [crate::chain::expr::ProcessExpr::Ge]
    Ge,
    /// [crate::chain::expr::ProcessExpr::Gt]
    Gt,
    /// [crate::chain::expr::ProcessExpr::IsSorted]
    IsSorted,
    /// [crate::chain::expr::ProcessExpr::IsSortedBy]
    IsSortedBy,
    /// [crate::chain::expr::ProcessExpr::IsSortedByKey]
    IsSortedByKey,
    /// [crate::chain::expr::ProcessExpr::Last]
    Last,
    /// [crate::chain::expr::ProcessExpr::Le]
    Le,
    /// [crate::chain::expr::ProcessExpr::Lt]
    Lt,
    /// [crate::chain::expr::ProcessExpr::Max]
    Max,
    /// [crate::chain::expr::ProcessExpr::MaxBy]
    MaxBy,
    /// [crate::chain::expr::ProcessExpr::MaxByKey]
    MaxByKey,
    /// [crate::chain::expr::ProcessExpr::Min]
    Min,
    /// [crate::chain::expr::ProcessExpr::MinBy]
    MinBy,
    /// [crate::chain::expr::ProcessExpr::MinByKey]
    MinByKey,
    /// [crate::chain::expr::ProcessExpr::Ne]
    Ne,
    /// [crate::chain::expr::ProcessExpr::Nth]
    Nth,
    /// [crate::chain::expr::ProcessExpr::PartialCmp]
    PartialCmp,
    /// [crate::chain::expr::ProcessExpr::IsPartitioned]
    IsPartitioned,
    /// [crate::chain::expr::ProcessExpr::Partition]
    Partition,
    /// [crate::chain::expr::ProcessExpr::PartitionInPlace]
    PartitionInPlace,
    /// [crate::chain::expr::ProcessExpr::Peekable]
    Peekable,
    /// [crate::chain::expr::ProcessExpr::Position]
    Position,
    /// [crate::chain::expr::ProcessExpr::Product]
    Product,
    /// [crate::chain::expr::ProcessExpr::Rev]
    Rev,
    /// [crate::chain::expr::ProcessExpr::Rposition]
    Rposition,
    /// [crate::chain::expr::ProcessExpr::Scan]
    Scan,
    /// [crate::chain::expr::ProcessExpr::SizeHint]
    SizeHint,
    /// [crate::chain::expr::ProcessExpr::Skip]
    Skip,
    /// [crate::chain::expr::ProcessExpr::SkipWhile]
    SkipWhile,
    /// [crate::chain::expr::ProcessExpr::StepBy]
    StepBy,
    /// [crate::chain::expr::ProcessExpr::Sum]
    Sum,
    /// [crate::chain::expr::ProcessExpr::Take]
    Take,
    /// [crate::chain::expr::ProcessExpr::TakeWhile]
    TakeWhile,
    /// [crate::chain::expr::ProcessExpr::TryFold]
    TryFold,
    /// [crate::chain::expr::ProcessExpr::TryForEach]
    TryForEach,
    /// UNWRAP (Special `Combinator` used to define that next group position is by one level up - which will be `#value.and_then(#previous_expr).#next_expr` )
    UNWRAP,
    /// [crate::chain::expr::ProcessExpr::Unzip]
    Unzip,
    /// [crate::chain::expr::ProcessExpr::Zip]
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

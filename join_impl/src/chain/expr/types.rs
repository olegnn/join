//!
//! Action types definitions.
//!

///
/// `ActionType` defines action of 3 types: `Initial` (`InitialExpr`), `Process` (`ProcessExpr`), `Err` (`ErrExpr`).
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ActionType {
    Initial,
    Process,
    Err,
}

///
/// `ApplicationType` defines two types of action: `Instant` and `Deferred`. `Instant` means that action will be applied
/// instantly, deferred means that it will wait for all actions in current step to be finished.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ApplicationType {
    Instant,
    Deferred,
}

///
/// `MoveType` defines nested combinator types.
///
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Wrap,
    Unwrap,
    None,
}

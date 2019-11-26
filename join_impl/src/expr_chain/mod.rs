//!
//! Contains `Chain` trait and implementation, expressions, groups and utils to work with all of them.
//!

pub mod action_expr_chain;
pub mod chain;
pub mod expr;
pub mod group;
pub mod unit;
pub mod utils;

pub use action_expr_chain::{ActionExprChain, ActionExprChainGenerator};
pub use chain::Chain;
pub use expr::ActionExpr;
use group::{ActionGroup, CommandGroup, GroupDeterminer};
pub use unit::{TransformParsed, Unit, UnitResult};
use utils::{is_block_expr, parse_until};

//!
//! Contains `Chain` trait and implementation, expressions, groups and utils to work with all of them.
//!

pub mod chain;
pub mod expr;
pub mod expr_chain_with_default;
pub mod group;
pub mod utils;

pub use chain::Chain;
use chain::{Unit, UnitResult};
use expr::{DefaultActionExpr, ExtractExpr, ProcessActionExpr};
pub use expr_chain_with_default::{ExprChainWithDefault, ProcessWithDefault};
use group::{ActionGroup, CommandGroup, GroupDeterminer};
use utils::{is_block_expr, is_valid_expr, parse_until};

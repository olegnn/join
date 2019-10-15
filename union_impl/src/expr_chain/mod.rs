//!
//! Contains `Chain` trait and implementation, expressions, groups and utils to work with all of them.
//!

pub mod chain;
pub mod expr;
pub mod expr_chain_with_default;
pub mod group;
pub mod utils;

pub use chain::Chain;
pub use expr_chain_with_default::{ExprChainWithDefault, ProcessWithDefault};
use expr::{ProcessActionExpr, DefaultActionExpr, ExtractExpr};
use chain::{Unit, UnitResult};
use group::{ActionGroup, CommandGroup, GroupDeterminer};
use utils::{is_valid_expr, parse_until, is_block_expr};
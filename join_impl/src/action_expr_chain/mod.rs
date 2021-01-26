//!
//! Chain implemented for `ActionExpr`
//!
pub mod builder;

use crate::chain::expr::ActionExpr;
use crate::chain::Chain;
pub use builder::ActionExprChainBuilder;
use syn::PatIdent;

///
/// Chain with members of type `ActionExpr`. and optional `PatIdent` identifier.
///
pub struct ActionExprChain {
    ident: Option<PatIdent>,
    members: Vec<ActionExpr>,
}

///
/// Implementation of `Chain` with `ActionExpr` members.
///
impl Chain for ActionExprChain
where
    Self: Sized,
{
    type Member = ActionExpr;
    type Identifier = PatIdent;

    fn new(ident: impl Into<Option<PatIdent>>, members: &[ActionExpr]) -> Self {
        Self {
            ident: ident.into(),
            members: members.to_vec(),
        }
    }

    fn append_member(&mut self, val: ActionExpr) -> usize {
        self.members.push(val);
        self.members.len()
    }

    fn set_id(&mut self, val: impl Into<Option<PatIdent>>) -> &mut Self {
        self.ident = val.into();
        self
    }

    fn get_members(&self) -> &[Self::Member] {
        &self.members
    }

    fn get_id(&self) -> Option<&Self::Identifier> {
        self.ident.as_ref()
    }

    fn remove_member(&mut self, idx: usize) -> Option<Self::Member> {
        if idx < self.len() {
            Some(self.members.remove(idx))
        } else {
            None
        }
    }

    fn len(&self) -> usize {
        self.members.len()
    }
}

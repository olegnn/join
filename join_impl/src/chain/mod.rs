pub mod expr;
pub mod group;
pub mod parse_chain;
pub use parse_chain::ParseChain;

///
/// Any chain with members of type `Member` and optional `Identifier`.
///
pub trait Chain
where
    Self: Sized,
{
    type Member: Sized;
    type Identifier: Sized;

    ///
    /// Constructs new Chain.
    ///
    fn new(id: impl Into<Option<Self::Identifier>>, members: &[Self::Member]) -> Self;

    ///
    /// Adds member to chain.
    ///
    fn append_member(&mut self, member: Self::Member) -> usize;

    ///
    /// Removes member from chain.
    ///
    fn remove_member(&mut self, idx: usize) -> Option<Self::Member>;

    ///
    /// Returns self members.
    ///
    fn get_members(&self) -> &[Self::Member];

    ///
    /// Returns chain length.
    ///
    fn len(&self) -> usize;

    ///
    /// Checks if chain is empty.
    ///
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    ///
    /// Sets chain identifier.
    ///
    fn set_id(&mut self, id: impl Into<Option<Self::Identifier>>) -> &mut Self;

    ///
    /// Returns optional `Identifier` associated with chain.
    ///
    fn get_id(&self) -> Option<&Self::Identifier>;
}

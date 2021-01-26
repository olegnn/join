use super::Chain;
use syn::parse::ParseStream;

///
/// Build chain from `ParseStream`
///
pub trait ParseChain<T: Chain> {
    ///
    ///  Builds `T` from input `ParseStream`.
    ///
    fn build_from_parse_stream(&self, input: ParseStream<'_>) -> syn::Result<T>;
}

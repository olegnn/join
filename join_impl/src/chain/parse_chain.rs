use syn::parse::ParseStream;
use super::Chain;

///
/// Build chain from `ParseStream`
/// 
pub trait ParseChain<T: Chain> {
    ///
    ///  Builds `Self` from input `ParseStream`.
    ///
    fn build_from_parse_stream(&self, input: ParseStream<'_>) -> syn::Result<T>;
}

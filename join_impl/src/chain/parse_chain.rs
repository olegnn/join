use syn::parse::ParseStream;

pub trait ParseChain<T> {
    ///
    ///  `Self` from input `ParseStream`.
    ///
    fn build_from_parse_stream(&self, input: ParseStream<'_>) -> syn::Result<T>;
}

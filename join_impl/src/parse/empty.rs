//!
//! Empty parsing unit with all necessary traits implemented.
//!

use syn::parse::{Parse, ParseStream};

///
/// Struct which parses empty `ParseStream` as `Ok`, non-empty as `Err`.
///
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Empty;

impl Parse for Empty {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            Ok(Self)
        } else {
            Err(input.error("Unexpected tokens"))
        }
    }
}

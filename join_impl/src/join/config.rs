//!
//! Configuration of the `join!` macro.
//!

///
/// Defines configuration of the `join!` macro.
///
pub struct Config {
    pub is_async: bool,
    pub is_try: bool,
    pub spawn: bool,
}

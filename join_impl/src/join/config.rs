//!
//! Configuration of the `join!` macro.
//!

///
/// Defines configuration of the `join!` macro.
///
pub struct Config {
    ///
    /// Are branches sync or futures.
    ///
    pub is_async: bool,
    ///
    /// Transpose final values into one `Result`/`Option`.
    ///
    pub is_try: bool,
    ///
    /// Spawn `std::thread` for sync or `tokio::spawn` for futures.
    ///
    pub spawn: bool,
}

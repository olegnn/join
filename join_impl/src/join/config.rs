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
    /// Transpose final values into one `Result`/`Option` and check results at the end of step.
    ///
    pub is_try: bool,
    ///
    /// Spawn branches using `std::thread::spawn` for sync or `tokio::spawn` for futures.
    ///
    pub is_spawn: bool,
}

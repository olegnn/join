///
/// Provides functionality to map `Self` to another type
///
pub trait Map<T>: Sized {
    ///
    /// Maps value to other type using supplied function.
    ///
    fn map<F>(self, f: F) -> T
    where
        F: FnOnce(Self) -> T;
}

///
/// Provides functionality to map `Self` over some type to another type
///
pub trait MapOver<V, R, T> {
    ///
    /// Maps value over some type to other type using supplied function.
    ///
    fn map_over<F>(self, f: F) -> T
    where
        F: FnOnce(V) -> R;
}

pub trait Presence: Copy + 'static {
    type Wrap<T>;

    fn map<T, U>(wrap: Self::Wrap<T>, f: impl FnOnce(T) -> U) -> Self::Wrap<U>;

    fn into_option<T>(wrap: Self::Wrap<T>) -> Option<T>;
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Required;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Optional;

impl Presence for Required {
    type Wrap<T> = T;

    #[inline]
    fn map<T, U>(wrap: T, f: impl FnOnce(T) -> U) -> U {
        f(wrap)
    }

    #[inline]
    fn into_option<T>(wrap: T) -> Option<T> {
        Some(wrap)
    }
}

impl Presence for Optional {
    type Wrap<T> = Option<T>;

    #[inline]
    fn map<T, U>(wrap: Option<T>, f: impl FnOnce(T) -> U) -> Option<U> {
        wrap.map(f)
    }

    #[inline]
    fn into_option<T>(wrap: Option<T>) -> Option<T> {
        wrap
    }
}

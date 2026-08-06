/// Trait for attempting to get a reference to a specific variant in an enum
///
/// This trait enables safe downcasting from an enum to a specific variant's inner type.
pub trait TryAsRef<T> {
    /// Attempts to return a reference to the inner type if the enum variant matches
    ///
    /// # Returns
    /// - `Some(&T)` if the enum variant contains the requested type
    /// - `None` if the enum variant doesn't match
    fn try_as_ref(&self) -> Option<&T>;
}

/// Trait for attempting to get a mutable reference to a specific variant in an enum
///
/// This trait enables safe mutable downcasting from an enum to a specific variant's inner type.
pub trait TryAsMut<T> {
    /// Attempts to return a mutable reference to the inner type if the enum variant matches
    ///
    /// # Returns
    /// - `Some(&mut T)` if the enum variant contains the requested type
    /// - `None` if the enum variant doesn't match
    fn try_as_mut(&mut self) -> Option<&mut T>;
}

/// Implements both `TryAsRef` and `TryAsMut` for an enum type
///
/// This macro generates implementations for trying to access specific variant types
/// through references or mutable references.
///
/// # Example
/// ```
/// enum MyEnum {
///     Variant1(String),
///     Variant2(usize),
/// }
///
/// impl_try_as!(MyEnum, Variant1(String), Variant2(usize));
/// ```
#[macro_export]
macro_rules! impl_try_as {
    ($enum_type:ident, $($variant:ident($variant_type:ty)),*) => {
        $(
            impl $crate::convert::TryAsRef<$variant_type> for $enum_type {
                fn try_as_ref(&self) -> Option<&$variant_type> {
                    match self {
                        $enum_type::$variant(val) => Some(val),
                        _ => None,
                    }
                }
            }

            impl $crate::convert::TryAsMut<$variant_type> for $enum_type {
                fn try_as_mut(&mut self) -> Option<&mut $variant_type> {
                    match self {
                        $enum_type::$variant(val) => Some(val),
                        _ => None,
                    }
                }
            }
        )*
    };
}

/// Implements `TryAsRef` for an enum type
///
/// This macro generates immutable reference access implementations for trying
/// to access specific variant types.
#[macro_export]
macro_rules! impl_try_as_ref {
    ($enum_type:ident, $($variant:ident($variant_type:ty)),*) => {
        $(
            impl $crate::convert::TryAsRef<$variant_type> for $enum_type {
                fn try_as_ref(&self) -> Option<&$variant_type> {
                    match self {
                        $enum_type::$variant(val) => Some(val),
                        _ => None,
                    }
                }
            }
        )*
    };
}

/// Implements `TryAsMut` for an enum type
///
/// This macro generates mutable reference access implementations for trying
/// to access specific variant types.
#[macro_export]
macro_rules! impl_try_as_mut {
    ($enum_type:ident, $($variant:ident($variant_type:ty)),*) => {
        $(
            impl $crate::convert::TryAsMut<$variant_type> for $enum_type {
                fn try_as_mut(&mut self) -> Option<&mut $variant_type> {
                    match self {
                        $enum_type::$variant(val) => Some(val),
                        _ => None,
                    }
                }
            }
        )*
    };
}

/// Implements `TryInto` for converting from an enum to its variant's inner type
///
/// This macro generates implementations to consume an enum and extract a specific
/// variant's inner value.
#[macro_export]
macro_rules! impl_try_into {
    ($enum_type:ident, $($variant:ident($variant_type:ty)),*) => {
        $(
            impl ::core::convert::TryInto<$variant_type> for $enum_type {
                type Error = ();

                fn try_into(self) -> Result<$variant_type, Self::Error> {
                    match self {
                        $enum_type::$variant(val) => Ok(val),
                        _ => Err(()),
                    }
                }
            }
        )*
    };
}

/// Implements `TryFrom` for converting from an enum to its variant's inner type
///
/// This macro generates implementations to consume an enum and extract a specific
/// variant's inner value.
#[macro_export]
macro_rules! impl_try_from {
    ($enum_type:ident, $($variant:ident($variant_type:ty)),*) => {
        $(
            impl ::core::convert::TryFrom<$enum_type> for $variant_type {
                type Error = ();

                fn try_from(value: $enum_type) -> Result<Self, Self::Error> {
                    match value {
                        $enum_type::$variant(val) => Ok(val),
                        _ => Err(()),
                    }
                }
            }
        )*
    };
}

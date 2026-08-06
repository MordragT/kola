use std::fmt;

pub trait DisplayWith<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, t: &T) -> fmt::Result;
}

pub trait SerializeWith<T> {
    fn serialize<S>(&self, serializer: S, t: &T) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer;
}

#[macro_export]
macro_rules! as_variant {
    ($enum: expr, $($variant: path), *) => {
        match $enum {
            $( $variant(inner) )|* => ::core::option::Option::Some(inner),
            _ => ::core::option::Option::None
        }
    }
}

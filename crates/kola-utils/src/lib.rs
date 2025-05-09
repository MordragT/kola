pub use convert::{TryAsMut, TryAsRef};
pub use errors::Errors;
pub use interner::{Interner, PathInterner, PathKey, StrInterner, StrKey};

mod convert;
mod errors;
mod interner;

#[macro_export]
macro_rules! as_variant {
    ($enum: expr, $($variant: path), *) => {
        match $enum {
            $( $variant(inner) )|* => ::core::option::Option::Some(inner),
            _ => ::core::option::Option::None
        }
    }
}

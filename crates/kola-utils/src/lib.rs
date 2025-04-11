pub use convert::{TryAsMut, TryAsRef};
pub use errors::Errors;

mod convert;
mod errors;

#[macro_export]
macro_rules! as_variant {
    ($enum: expr, $($variant: path), *) => {
        match $enum {
            $( $variant(inner) )|* => ::core::option::Option::Some(inner),
            _ => ::core::option::Option::None
        }
    }
}

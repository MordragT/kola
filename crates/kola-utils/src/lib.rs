#![feature(const_cmp)]
#![feature(const_trait_impl)]

pub mod convert;
pub mod dependency;
pub mod display;
pub mod errors;
pub mod interner;
pub mod interner_ext;
pub mod io;
pub mod scope;
pub mod serde;
pub mod visit;

#[macro_export]
macro_rules! as_variant {
    ($enum: expr, $($variant: path), *) => {
        match $enum {
            $( $variant(inner) )|* => ::core::option::Option::Some(inner),
            _ => ::core::option::Option::None
        }
    }
}

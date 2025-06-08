#![feature(variant_count)]
#![feature(negative_impls)]

pub mod bimap;
pub mod convert;
pub mod dependency;
pub mod errors;
pub mod id;
pub mod interner;
pub mod io;
pub mod tracker;
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

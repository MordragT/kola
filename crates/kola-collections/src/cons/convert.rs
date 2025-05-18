macro_rules! tuple_from_ctx {
    ($($types:ident),*) => {
        impl<$($types),*> From<$crate::Cons!($($types,)*)> for ($($types,)*) {
            #[allow(non_snake_case, clippy::unused_unit)]
            fn from(value: $crate::Cons!($($types,)*)) -> Self {
                let $crate::cons!($($types,)*) = value;
                ($($types,)*)
            }
        }
    };
}

// Conversion from heterogenous list to tuple is implemented for tuples of size 12 and less

tuple_from_ctx!(A, B, C, D, E, F, G, H, I, J, K, L);
tuple_from_ctx!(A, B, C, D, E, F, G, H, I, J, K);
tuple_from_ctx!(A, B, C, D, E, F, G, H, I, J);
tuple_from_ctx!(A, B, C, D, E, F, G, H, I);
tuple_from_ctx!(A, B, C, D, E, F, G, H);
tuple_from_ctx!(A, B, C, D, E, F, G);
tuple_from_ctx!(A, B, C, D, E, F);
tuple_from_ctx!(A, B, C, D, E);
tuple_from_ctx!(A, B, C, D);
tuple_from_ctx!(A, B, C);
tuple_from_ctx!(A, B);
tuple_from_ctx!(A);
tuple_from_ctx!();

macro_rules! ctx_from_tuple {
    ($($types:ident),*) => {
        impl<$($types),*> From<($($types,)*)> for $crate::Cons!($($types,)*) {
            #[allow(non_snake_case)]
            fn from(value: ($($types,)*)) -> Self {
                let ($($types,)*) = value;
                $crate::cons!($($types,)*)
            }
        }
    };
}

// Conversion from tuple to heterogenous list is implemented for tuples of size 12 and less

ctx_from_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
ctx_from_tuple!(A, B, C, D, E, F, G, H, I, J, K);
ctx_from_tuple!(A, B, C, D, E, F, G, H, I, J);
ctx_from_tuple!(A, B, C, D, E, F, G, H, I);
ctx_from_tuple!(A, B, C, D, E, F, G, H);
ctx_from_tuple!(A, B, C, D, E, F, G);
ctx_from_tuple!(A, B, C, D, E, F);
ctx_from_tuple!(A, B, C, D, E);
ctx_from_tuple!(A, B, C, D);
ctx_from_tuple!(A, B, C);
ctx_from_tuple!(A, B);
ctx_from_tuple!(A);
ctx_from_tuple!();

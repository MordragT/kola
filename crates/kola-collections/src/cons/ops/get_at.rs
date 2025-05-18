use crate::cons::Cons;

// A trait for getting a component at a specific position
pub trait GetAt<T, const N: usize> {
    fn get_at(&self) -> &T;
    fn get_at_mut(&mut self) -> &mut T;
}

// Implement for Cons at position 0 (head)
impl<Head, Tail> GetAt<Head, 0> for Cons<Head, Tail> {
    fn get_at(&self) -> &Head {
        &self.0
    }

    fn get_at_mut(&mut self) -> &mut Head {
        &mut self.0
    }
}

// Macro to generate GetAt implementations for positions
#[macro_export]
macro_rules! generate_get_at_impls {
    ($($n:expr),*) => {
        $(
            impl<Head, Tail, T> GetAt<T, $n> for Cons<Head, Tail>
            where
                Tail: GetAt<T, {$n - 1}>,
            {
                fn get_at(&self) -> &T {
                    self.1.get_at()
                }

                fn get_at_mut(&mut self) -> &mut T {
                    self.1.get_at_mut()
                }
            }
        )*
    };
}

// Generate implementations for positions 1-15
generate_get_at_impls!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

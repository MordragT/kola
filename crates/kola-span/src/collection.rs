use std::fmt::Debug;

pub trait Collection<I, T>: Debug {
    type Output: Debug;

    fn new_with(input: &mut I) -> Self;

    fn push_with(&mut self, item: T, input: &mut I);

    fn finish_with(self, input: &mut I) -> Self::Output;
}

impl<I, T, C> Collection<I, T> for C
where
    C: Default + Extend<T> + Debug,
{
    type Output = C;

    fn new_with(_input: &mut I) -> Self {
        C::default()
    }

    fn push_with(&mut self, item: T, _input: &mut I) {
        self.extend(std::iter::once(item));
    }

    fn finish_with(self, _input: &mut I) -> Self::Output {
        self
    }
}

use std::array;

use crate::{
    Report,
    input::Input,
    parser::{Failure, Parser},
};

#[derive(Clone, Copy)]
pub struct Many<const N: usize, P> {
    pub(super) parser: P,
}

impl<I, O, P, const N: usize> Parser<I, [O; N]> for Many<N, P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<[O; N], Failure> {
        array::try_from_fn(|_| self.parser.parse(input, report))
    }
}

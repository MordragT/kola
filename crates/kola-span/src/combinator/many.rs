use std::array;

use crate::{
    Report,
    input::Input,
    parser::{ParseResult, Parser},
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
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<[O; N], I::Token> {
        let checkpoint = input.checkpoint();
        array::try_from_fn(|_| self.parser.parse(input, report)).inspect_err(|e| {
            if e.is_miss() {
                input.reset(checkpoint);
            }
        })
        // TODO: probably only the first error is okay to be miss
    }
}

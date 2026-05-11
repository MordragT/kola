use std::fmt::Debug;

use crate::{
    Loc, Report,
    input::Input,
    parser::{Failure, Parser},
};

#[derive(Clone, Copy)]
pub struct Spanned<P> {
    pub(super) parser: P,
}

impl<I, O, P> Parser<I, (O, Loc)> for Spanned<P>
where
    I: Input,
    O: Debug,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(O, Loc), Failure> {
        let start = input.loc();
        let result = self.parser.parse(input, report)?;
        let loc = start.union(input.prev_loc());
        Ok((result, loc))
    }
}

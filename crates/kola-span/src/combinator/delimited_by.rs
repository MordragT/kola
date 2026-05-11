use std::{fmt::Debug, marker::PhantomData};

use crate::{
    Report,
    input::Input,
    parser::{Failure, Parser},
};

pub struct DelimitedBy<P, P1, P2, O1, O2> {
    pub(super) _marker: PhantomData<(O1, O2)>,
    pub(super) parser: P,
    pub(super) open: P1,
    pub(super) close: P2,
}

impl<P, P1, P2, O1, O2> Clone for DelimitedBy<P, P1, P2, O1, O2>
where
    P: Clone,
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            open: self.open.clone(),
            close: self.close.clone(),
        }
    }
}

impl<P, P1, P2, O1, O2> Copy for DelimitedBy<P, P1, P2, O1, O2>
where
    P: Copy,
    P1: Copy,
    P2: Copy,
{
}

impl<I, O, O1, O2, P, P1, P2> Parser<I, O> for DelimitedBy<P, P1, P2, O1, O2>
where
    I: Input,
    O: Debug,
    O1: Debug,
    O2: Debug,
    P: Parser<I, O>,
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        self.open.parse(input, report)?;
        let o = self.parser.parse(input, report)?;
        self.close.parse(input, report)?;
        Ok(o)
    }
}

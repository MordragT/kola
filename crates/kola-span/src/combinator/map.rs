use std::marker::PhantomData;

use crate::{
    Loc, Report,
    input::Input,
    parser::{Failure, Parser},
};

pub struct Map<P, F, O> {
    pub(super) _marker: PhantomData<O>,
    pub(super) parser: P,
    pub(super) f: F,
}

impl<P, F, O> Clone for Map<P, F, O>
where
    P: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            f: self.f.clone(),
        }
    }
}

impl<P, F, O> Copy for Map<P, F, O>
where
    P: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, F> Parser<I, O1> for Map<P, F, O>
where
    I: Input,
    P: Parser<I, O>,
    F: Fn(O) -> O1,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O1, Failure> {
        self.parser.parse(input, report).map(|ok| (self.f)(ok))
    }
}

pub struct MapWith<P, F, O> {
    pub(super) _marker: PhantomData<O>,
    pub(super) parser: P,
    pub(super) f: F,
}

impl<P, F, O> Clone for MapWith<P, F, O>
where
    P: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            f: self.f.clone(),
        }
    }
}

impl<P, F, O> Copy for MapWith<P, F, O>
where
    P: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, F> Parser<I, O1> for MapWith<P, F, O>
where
    I: Input,
    P: Parser<I, O>,
    F: Fn(O, Loc, &mut I) -> O1,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O1, Failure> {
        let start = input.loc();
        let result = self.parser.parse(input, report)?;
        let loc = start.union(input.prev_loc());
        Ok((self.f)(result, loc, input))
    }
}

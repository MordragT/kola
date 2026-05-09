use std::marker::PhantomData;

use crate::{
    Failure, Report,
    input::Input,
    parser::{ParseResult, Parser},
};

#[derive(Clone, Copy)]
pub struct Then<P1, P2> {
    pub(super) first: P1,
    pub(super) second: P2,
}

impl<I, O, O1, P1, P2> Parser<I, (O, O1)> for Then<P1, P2>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O1>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<(O, O1), I::Token> {
        let checkpoint = input.checkpoint();
        let o = self.first.parse(input, report)?;
        let o1 = match self.second.parse(input, report) {
            Ok(o1) => o1,
            Err(Failure::Raise(e)) => return Err(Failure::Raise(e)),
            Err(Failure::Miss(miss)) => {
                input.reset(checkpoint);
                return Err(Failure::Miss(miss));
            }
        };
        Ok((o, o1))
    }
}

pub struct IgnoreThen<P1, P2, O> {
    pub(super) _marker: PhantomData<O>,
    pub(super) first: P1,
    pub(super) second: P2,
}

impl<P1, P2, O> Clone for IgnoreThen<P1, P2, O>
where
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            first: self.first.clone(),
            second: self.second.clone(),
        }
    }
}

impl<P1, P2, O> Copy for IgnoreThen<P1, P2, O>
where
    P1: Copy,
    P2: Copy,
{
}

impl<I, O, O1, P1, P2> Parser<I, O1> for IgnoreThen<P1, P2, O>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O1>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O1, I::Token> {
        let checkpoint = input.checkpoint();
        self.first.parse(input, report)?;
        match self.second.parse(input, report) {
            Ok(o1) => Ok(o1),
            Err(Failure::Raise(e)) => Err(Failure::Raise(e)),
            Err(Failure::Miss(miss)) => {
                input.reset(checkpoint);
                Err(Failure::Miss(miss))
            }
        }
    }
}

pub struct ThenIgnore<P1, P2, O1> {
    pub(super) _marker: PhantomData<O1>,
    pub(super) first: P1,
    pub(super) second: P2,
}

impl<P1, P2, O1> Clone for ThenIgnore<P1, P2, O1>
where
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            first: self.first.clone(),
            second: self.second.clone(),
        }
    }
}

impl<P1, P2, O1> Copy for ThenIgnore<P1, P2, O1>
where
    P1: Copy,
    P2: Copy,
{
}

impl<I, O, O1, P1, P2> Parser<I, O> for ThenIgnore<P1, P2, O1>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O1>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        let checkpoint = input.checkpoint();
        let o = self.first.parse(input, report)?;
        if let Err(Failure::Miss(miss)) = self.second.parse(input, report) {
            input.reset(checkpoint);
            return Err(Failure::Miss(miss));
        }
        Ok(o)
    }
}

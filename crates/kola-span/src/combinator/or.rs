use std::marker::PhantomData;

use crate::{
    Failure, Loc, Report,
    input::Input,
    parser::{ParseResult, Parser},
    skip::Skip,
};

pub struct Or<P1, P2, O> {
    pub(super) _marker: PhantomData<O>,
    pub(super) first: P1,
    pub(super) second: P2,
}

impl<P1, P2, O> Clone for Or<P1, P2, O>
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

impl<P1, P2, O> Copy for Or<P1, P2, O>
where
    P1: Copy,
    P2: Copy,
{
}

impl<I, O, P1, P2> Parser<I, O> for Or<P1, P2, O>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        let checkpoint = input.checkpoint();
        match self.first.parse(input, report) {
            Ok(o) => Ok(o),
            Err(Failure::Raise(e)) => Err(Failure::Raise(e)),
            Err(Failure::Miss(_)) => {
                input.reset(checkpoint);
                self.second.parse(input, report)
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct OrNot<P> {
    pub(super) parser: P,
}

impl<I, O, P> Parser<I, Option<O>> for OrNot<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<Option<O>, I::Token> {
        let checkpoint = input.checkpoint();
        match self.parser.parse(input, report) {
            Ok(o) => Ok(Some(o)),
            Err(Failure::Raise(e)) => Err(Failure::Raise(e)),
            Err(Failure::Miss(_)) => {
                input.reset(checkpoint);
                Ok(None)
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct OrElse<P, S, F> {
    pub(super) parser: P,
    pub(super) skipper: S,
    pub(super) fallback: F,
}

impl<I, O, F, P, S> Parser<I, O> for OrElse<P, S, F>
where
    I: Input,
    P: Parser<I, O>,
    S: Skip<I>,
    F: Fn(Loc, &mut I) -> O,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        match self.parser.parse(input, report) {
            Ok(o) => Ok(o),
            Err(Failure::Raise(e)) => Err(Failure::Raise(e)),
            Err(Failure::Miss(_)) => {
                let loc = self.skipper.skip(input);
                let output = (self.fallback)(loc, input);
                Ok(output)
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct OrReport<P, S, F> {
    pub(super) parser: P,
    pub(super) skipper: S,
    pub(super) fallback: F,
}

impl<I, O, F, P, S> Parser<I, O> for OrReport<P, S, F>
where
    I: Input,
    P: Parser<I, O>,
    S: Skip<I>,
    F: Fn(Loc, &mut I) -> O,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        match self.parser.parse(input, report) {
            Ok(o) => Ok(o),
            Err(Failure::Miss(miss)) => Err(Failure::Miss(miss)),
            Err(Failure::Raise(e)) => {
                report.add_diagnostic(e);
                let loc = self.skipper.skip(input);
                let output = (self.fallback)(loc, input);
                Ok(output)
            }
        }
    }
}

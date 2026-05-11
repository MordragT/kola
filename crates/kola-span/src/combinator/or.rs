use std::{fmt::Debug, marker::PhantomData};

use crate::{
    Loc, Report,
    input::Input,
    parser::{Failure, Parser},
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
    O: Debug,
    P1: Parser<I, O>,
    P2: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let input_cp = input.checkpoint();
        let report_cp = report.checkpoint();

        let e1 = match self.first.parse(input, report) {
            Ok(o) => return Ok(o),
            Err(Failure::Throw(cp)) => return Err(Failure::Throw(cp)),
            Err(Failure::Abort(e1)) => e1,
        };

        input.reset(input_cp);

        let e2 = match self.second.parse(input, report) {
            Ok(o) => {
                report.reset(report_cp);
                return Ok(o);
            }
            Err(Failure::Throw(cp)) => return Err(Failure::Throw(cp)),
            Err(Failure::Abort(e2)) => e2,
        };

        Err(e1.with_trace_element(e2.loc, e2.to_string()).into())
    }
}

#[derive(Clone, Copy)]
pub struct OrNot<P> {
    pub(super) parser: P,
}

impl<I, O, P> Parser<I, Option<O>> for OrNot<P>
where
    I: Input,
    O: Debug,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<Option<O>, Failure> {
        let input_cp = input.checkpoint();
        let report_cp = report.checkpoint();

        match self.parser.parse(input, report) {
            Ok(o) => Ok(Some(o)),
            Err(Failure::Throw(cp)) => Err(Failure::Throw(cp)),
            Err(Failure::Abort(_)) => {
                input.reset(input_cp);
                report.reset(report_cp);
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
    O: Debug,
    P: Parser<I, O>,
    S: Skip<I>,
    F: Fn(Loc, &mut I) -> O,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let report_cp = report.checkpoint();

        match self.parser.parse(input, report) {
            Ok(o) => Ok(o),
            Err(Failure::Throw(cp)) => Err(Failure::Throw(cp)),
            Err(Failure::Abort(_)) => {
                report.reset(report_cp);
                let loc = self.skipper.skip(input);
                let output = (self.fallback)(loc, input);
                Ok(output)
            }
        }
    }
}

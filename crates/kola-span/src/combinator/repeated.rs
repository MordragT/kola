use std::{fmt::Debug, marker::PhantomData};

use crate::{
    Report,
    input::Input,
    parser::{Failure, IterParser, Parser},
};

pub struct Repeated<P, O> {
    pub(super) _marker: PhantomData<O>,
    pub(super) parser: P,
    pub(super) min: usize,
    pub(super) max: Option<usize>,
}

impl<P, O> Clone for Repeated<P, O>
where
    P: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            min: self.min,
            max: self.max,
        }
    }
}

impl<P, O> Copy for Repeated<P, O> where P: Copy {}

impl<P, O> Repeated<P, O> {
    pub const fn at_least(mut self, min: usize) -> Self {
        self.min = min;
        self
    }

    pub const fn at_most(mut self, max: usize) -> Self {
        self.max = Some(max);
        self
    }

    pub const fn exactly(mut self, n: usize) -> Self {
        self.min = n;
        self.max = Some(n);
        self
    }
}

impl<I, O, P> IterParser<I, O> for Repeated<P, O>
where
    I: Input,
    O: Debug,
    P: Parser<I, O>,
{
    type State = usize;

    fn drive(
        &self,
        state: &mut usize,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Failure> {
        if self.max.is_some_and(|max| *state >= max) {
            return Ok(None);
        }

        let input_cp = input.checkpoint();
        let report_cp = report.checkpoint();

        let e = match self.parser.parse(input, report) {
            Ok(o) => {
                *state += 1;
                return Ok(Some(o));
            }
            Err(Failure::Throw(e)) => return Err(Failure::Throw(e)),
            Err(Failure::Abort(e)) => e,
        };

        input.reset(input_cp);

        if *state < self.min {
            Err(e
                .with_help(format!("expected at least {} items", self.min))
                .into())
        } else {
            report.reset(report_cp);
            Ok(None)
        }
    }
}

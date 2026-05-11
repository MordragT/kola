use std::fmt::Debug;

use crate::{
    Loc, Report,
    input::Input,
    parser::{Failure, IterParser, Parser},
    skip::Skip,
};

#[derive(Clone, Copy)]
pub struct Catch<P, S, F> {
    pub(super) parser: P,
    pub(super) skipper: S,
    pub(super) fallback: F,
}

impl<I, O, F, P, S> Parser<I, O> for Catch<P, S, F>
where
    I: Input,
    O: Debug,
    P: Parser<I, O>,
    S: Skip<I>,
    F: Fn(Loc, Report, &mut I) -> O,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let report_cp = match self.parser.parse(input, report) {
            Ok(o) => return Ok(o),
            Err(Failure::Abort(diag)) => return Err(Failure::Abort(diag)),
            Err(Failure::Throw(cp)) => cp,
        };

        let catched = report.split_reset(report_cp);
        let loc = self.skipper.skip(input);
        let output = (self.fallback)(loc, catched, input);
        Ok(output)
    }
}

impl<I, O, F, IP, S> IterParser<I, O> for Catch<IP, S, F>
where
    I: Input,
    O: Debug,
    IP: IterParser<I, O>,
    S: Skip<I>,
    F: Fn(Loc, Report, &mut I) -> O,
{
    type State = IP::State;

    #[inline]
    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Failure> {
        let report_cp = match self.parser.drive(state, input, report) {
            Ok(Some(o)) => return Ok(Some(o)),
            Ok(None) => return Ok(None),
            Err(Failure::Abort(diag)) => return Err(Failure::Abort(diag)),
            Err(Failure::Throw(cp)) => cp,
        };

        let catched = report.split_reset(report_cp);
        let loc = self.skipper.skip(input);
        let output = (self.fallback)(loc, catched, input);
        Ok(Some(output))
    }
}

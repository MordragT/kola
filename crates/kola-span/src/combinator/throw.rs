use crate::{
    Loc, Report,
    input::Input,
    parser::{Failure, Parser},
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
    P: Parser<I, O>,
    S: Skip<I>,
    F: Fn(Loc, Report, &mut I) -> O,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let report_cp = match self.parser.parse(input, report) {
            Ok(o) => return Ok(o),
            Err(Failure::Abort(diag)) => return Err(Failure::Abort(diag)),
            Err(Failure::Emit(cp)) => cp,
        };

        let catched = report.split_reset(report_cp);
        let loc = self.skipper.skip(input);
        let output = (self.fallback)(loc, catched, input);
        Ok(output)
    }
}

#[derive(Clone, Copy)]
pub struct Throw<P> {
    pub(super) parser: P,
}

impl<I, O, P> Parser<I, O> for Throw<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let e = match self.parser.parse(input, report) {
            Ok(o) => return Ok(o),
            Err(e) => e,
        };

        match e {
            Failure::Abort(diag) => {
                let report_cp = report.checkpoint();
                report.add_diagnostic(diag);

                Err(Failure::Emit(report_cp))
            }
            Failure::Emit(cp) => Err(Failure::Emit(cp)),
        }
    }
}

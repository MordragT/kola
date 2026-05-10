use crate::{
    Report,
    input::Input,
    parser::{Failure, Parser},
};

#[derive(Clone, Copy)]
pub struct WithHelp<P> {
    pub(super) parser: P,
    pub(super) help: &'static str,
}

impl<I, O, P> Parser<I, O> for WithHelp<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        match self.parser.parse(input, report) {
            Ok(ok) => Ok(ok),
            Err(Failure::Abort(diag)) => Err(Failure::Abort(diag.with_help(self.help))),
            Err(Failure::Emit(_)) => todo!(), // flatten report and add with_note then add back to report ?
        }
    }
}

#[derive(Clone, Copy)]
pub struct WithNote<P> {
    pub(super) parser: P,
    pub(super) note: &'static str,
}

impl<I, O, P> Parser<I, O> for WithNote<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        match self.parser.parse(input, report) {
            Ok(ok) => Ok(ok),
            Err(Failure::Abort(diag)) => Err(Failure::Abort(diag.with_note(self.note))),
            Err(Failure::Emit(_)) => todo!(), // flatten report and add with_note then add back to report ?
        }
    }
}

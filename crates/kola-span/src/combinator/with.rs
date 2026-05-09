use crate::{
    Failure, Report,
    input::Input,
    parser::{ParseResult, Parser},
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
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        match self.parser.parse(input, report) {
            Ok(ok) => Ok(ok),
            Err(Failure::Miss(miss)) => Err(Failure::Miss(miss)),
            Err(Failure::Raise(e)) => Err(Failure::Raise(e.with_help(self.help))),
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
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        match self.parser.parse(input, report) {
            Ok(ok) => Ok(ok),
            Err(Failure::Miss(miss)) => Err(Failure::Miss(miss)),
            Err(Failure::Raise(e)) => Err(Failure::Raise(e.with_note(self.note))),
        }
    }
}

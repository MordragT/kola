use crate::{Diagnostic, Report, input::Input};

pub trait Parser<I: Input, O>: Sized {
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Vec<Diagnostic>>;
}

impl<I, O, F> Parser<I, O> for F
where
    I: Input,
    F: Fn(&mut I, &mut Report) -> Result<O, Vec<Diagnostic>>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Vec<Diagnostic>> {
        self(input, report)
    }
}

pub trait FixpointParser<I: Input, O> {
    type Parser: Parser<I, O>;
    const PARSER: Self::Parser;
}

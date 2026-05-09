use crate::{Diagnostic, Failure, Report, input::Input};

pub type ParseResult<O, T> = Result<O, Failure<T>>;

pub trait Parser<I: Input, O>: Sized {
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token>;
}

impl<I, O, P> Parser<I, O> for &P
where
    I: Input,
    P: Parser<I, O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        (**self).parse(input, report)
    }
}

pub trait IterParser<I: Input, O>: Sized {
    type State: Default;

    /// Drive one step of the iteration.
    /// Returns:
    ///   Ok(Some(item)) - parsed an item, continue
    ///   Ok(None)       - done, stop iteration
    ///   Err(diag)      - fatal error
    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic>;
}

impl<I, O, P> IterParser<I, O> for &P
where
    I: Input,
    P: IterParser<I, O>,
{
    type State = P::State;

    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        (**self).drive(state, input, report)
    }
}

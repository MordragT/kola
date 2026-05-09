use crate::{
    Report,
    input::Input,
    parser::{ParseResult, Parser},
};

#[derive(Clone, Copy)]
pub struct Rewind<P> {
    pub(super) parser: P,
}

impl<I: Input, O, P: Parser<I, O>> Parser<I, O> for Rewind<P> {
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        let checkpoint = input.checkpoint();
        let result = self.parser.parse(input, report);
        // Always reset the input, whether it succeeded or failed!
        input.reset(checkpoint);
        result
    }
}

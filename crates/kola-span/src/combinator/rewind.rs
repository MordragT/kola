use crate::{
    Report,
    input::Input,
    parser::{Failure, Parser},
};

#[derive(Clone, Copy)]
pub struct Rewind<P> {
    pub(super) parser: P,
}

impl<I: Input, O, P: Parser<I, O>> Parser<I, O> for Rewind<P> {
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let checkpoint = input.checkpoint();
        let result = self.parser.parse(input, report);
        // Always reset the input, whether it succeeded or failed!
        input.reset(checkpoint);
        result
    }
}

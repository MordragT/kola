use std::fmt::Debug;

use crate::{
    Report,
    input::Input,
    parser::{Failure, Parser},
};

#[derive(Clone, Copy)]
pub struct Throw<P> {
    pub(super) parser: P,
    pub(super) reason: &'static str,
    pub(super) note: Option<&'static str>,
}

impl<P> Throw<P> {
    pub const fn with_note(mut self, note: &'static str) -> Self {
        self.note = Some(note);
        self
    }
}

impl<I, O, P> Parser<I, O> for Throw<P>
where
    I: Input,
    O: Debug,
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
                report.add_diagnostic(
                    diag.with_help(self.reason)
                        .with_notes(self.note.map(|n| n.to_owned())),
                );

                Err(Failure::Throw(report_cp))
            }
            Failure::Throw(cp) => Err(Failure::Throw(cp)),
        }
    }
}

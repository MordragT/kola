use std::{fmt::Debug, marker::PhantomData};

use crate::{
    Report,
    input::Input,
    parser::{Failure, IterParser, Parser},
};

pub struct SeparatedBy<P, S, OS, O> {
    pub(super) _marker: PhantomData<(OS, O)>,
    pub(super) parser: P,
    pub(super) min: usize,
    pub(super) max: Option<usize>,
    pub(super) sep: S,
    pub(super) allow_leading: bool,
    pub(super) allow_trailing: bool,
}

impl<P, S, OS, O> Clone for SeparatedBy<P, S, OS, O>
where
    P: Clone,
    S: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            min: self.min,
            max: self.max,
            sep: self.sep.clone(),
            allow_leading: self.allow_leading,
            allow_trailing: self.allow_trailing,
        }
    }
}

impl<P, S, OS, O> Copy for SeparatedBy<P, S, OS, O>
where
    P: Copy,
    S: Copy,
{
}

impl<P, S, OS, O> SeparatedBy<P, S, OS, O> {
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

    pub const fn allow_leading(mut self) -> Self {
        self.allow_leading = true;
        self
    }

    pub const fn allow_trailing(mut self) -> Self {
        self.allow_trailing = true;
        self
    }
}

pub struct SeparatedByState {
    count: usize,
    first: bool,
}

impl Default for SeparatedByState {
    fn default() -> Self {
        Self {
            count: 0,
            first: true,
        }
    }
}

impl<I, O, OS, P, S> IterParser<I, O> for SeparatedBy<P, S, OS, O>
where
    I: Input,
    O: Debug,
    OS: Debug,
    P: Parser<I, O>,
    S: Parser<I, OS>,
{
    type State = SeparatedByState;

    fn drive(
        &self,
        state: &mut SeparatedByState,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Failure> {
        if self.max.is_some_and(|max| state.count >= max) {
            return Ok(None);
        }

        let input_cp = input.checkpoint();
        let report_cp = report.checkpoint();

        // Leading separator: speculative, fully rolled back on failure
        if state.first {
            if self.allow_leading && self.sep.parse(input, report).is_err() {
                input.reset(input_cp);
                report.reset(report_cp);
            }

            state.first = false;
        } else {
            // Non-first: require separator
            match self.sep.parse(input, report) {
                Ok(_) => {} // committed past the separator
                Err(Failure::Throw(cp)) => return Err(Failure::Throw(cp)),
                Err(Failure::Abort(e)) => {
                    input.reset(input_cp);

                    return if state.count < self.min {
                        Err(e
                            .with_help(format!("Expected at least {} items", self.min))
                            .into())
                    } else {
                        report.reset(report_cp);
                        Ok(None)
                    };
                }
            }
        }

        let input_cp = input.checkpoint();
        let report_cp = report.checkpoint();

        // Sep consumed (or first item) — now try the item
        match self.parser.parse(input, report) {
            Ok(o) => {
                state.count += 1;
                Ok(Some(o))
            }
            Err(Failure::Throw(cp)) => return Err(Failure::Throw(cp)),
            Err(Failure::Abort(e)) => {
                if self.allow_trailing {
                    // Trailing sep — roll back sep+item attempt
                    input.reset(input_cp);

                    if state.count < self.min {
                        Err(e
                            .with_help(format!("Expected at least {} items", self.min))
                            .into())
                    } else {
                        report.reset(report_cp);
                        Ok(None)
                    }
                } else {
                    // Sep consumed but no item = fatal
                    Err(e.with_help(format!("todo")).into())
                }
            }
        }
    }
}

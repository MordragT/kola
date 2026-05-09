use std::marker::PhantomData;

use crate::{
    Diagnostic, Failure, Report,
    input::Input,
    parser::{IterParser, Parser},
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
    P: Parser<I, O>,
    S: Parser<I, OS>,
{
    type State = SeparatedByState;

    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        if self.max.is_some_and(|max| state.count >= max) {
            return Ok(None);
        }

        let checkpoint = input.checkpoint();

        // Leading separator: speculative, fully rolled back on failure
        if state.first {
            if self.allow_leading && self.sep.parse(input, report).is_err() {
                input.reset(checkpoint);
            }

            state.first = false;
        } else {
            // Non-first: require separator
            match self.sep.parse(input, report) {
                Ok(_) => {} // committed past the separator
                Err(Failure::Raise(e)) => return Err(e),
                Err(Failure::Miss(miss)) => {
                    input.reset(checkpoint);

                    return if state.count < self.min {
                        Err(miss
                            .throw()
                            .with_help(format!("Expected at least {} items", self.min)))
                    } else {
                        Ok(None)
                    };
                }
            }
        }

        let checkpoint = input.checkpoint();

        // Sep consumed (or first item) — now try the item
        match self.parser.parse(input, report) {
            Ok(o) => {
                state.count += 1;
                Ok(Some(o))
            }
            Err(Failure::Raise(e)) => Err(e),
            Err(Failure::Miss(miss)) => {
                if self.allow_trailing {
                    // Trailing sep — roll back sep+item attempt
                    input.reset(checkpoint);

                    if state.count < self.min {
                        Err(miss
                            .throw()
                            .with_help(format!("Expected at least {} items", self.min)))
                    } else {
                        Ok(None)
                    }
                } else {
                    // Sep consumed but no item = fatal
                    Err(miss.throw())
                }
            }
        }
    }
}

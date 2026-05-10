use std::marker::PhantomData;

use crate::{
    Diagnostic, Report,
    input::Input,
    parser::{Failure, IterParser, Parser},
};

pub struct SplitHead<IP, O, C> {
    pub(super) _marker: PhantomData<(O, C)>,
    pub(super) iter: IP,
}

impl<IP, O, C> Clone for SplitHead<IP, O, C>
where
    IP: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            iter: self.iter.clone(),
        }
    }
}

impl<IP, O, C> Copy for SplitHead<IP, O, C> where IP: Copy {}

impl<I, IP, O, C> Parser<I, (O, C)> for SplitHead<IP, O, C>
where
    I: Input,
    IP: IterParser<I, O>,
    C: Default + Extend<O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(O, C), Failure> {
        let mut state = IP::State::default();

        let checkpoint = input.checkpoint();
        let loc = input.loc();

        // Parse head item
        let head = match self
            .iter
            .drive(&mut state, input, report)
            .map_err(Failure::Abort)?
        {
            Some(o) => o,
            None => {
                input.reset(checkpoint);
                return Err(Diagnostic::error(loc, "Expected at least one item").into());
            }
        };

        // Parse tail items
        let mut tail = C::default();
        loop {
            match self
                .iter
                .drive(&mut state, input, report)
                .map_err(Failure::Abort)?
            {
                Some(o) => tail.extend(std::iter::once(o)),
                None => break,
            }
        }

        Ok((head, tail))
    }
}

pub struct SplitTail<IP, O, C> {
    pub(super) _marker: PhantomData<(O, C)>,
    pub(super) iter: IP,
}

impl<IP, O, C> Clone for SplitTail<IP, O, C>
where
    IP: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            iter: self.iter.clone(),
        }
    }
}

impl<IP, O, C> Copy for SplitTail<IP, O, C> where IP: Copy {}

impl<I, IP, O, C> Parser<I, (C, O)> for SplitTail<IP, O, C>
where
    I: Input,
    IP: IterParser<I, O>,
    C: Default + Extend<O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(C, O), Failure> {
        let mut state = IP::State::default();
        let mut head = C::default();

        // 1. Parse the first item. If None, we fail immediately.
        let mut prev = match self
            .iter
            .drive(&mut state, input, report)
            .map_err(Failure::Abort)?
        {
            Some(o) => o,
            None => return Err(Diagnostic::error(input.loc(), "Expected at least one item").into()),
        };

        // 2. Loop lazily
        loop {
            match self
                .iter
                .drive(&mut state, input, report)
                .map_err(Failure::Abort)?
            {
                Some(next) => {
                    // Because 'next' exists, 'prev' is NOT the tail.
                    // Push 'prev' into the collection and rotate.
                    head.extend(std::iter::once(prev));
                    prev = next;
                }
                None => {
                    // 'prev' is the final item! We break.
                    break;
                }
            }
        }

        Ok((head, prev))
    }
}

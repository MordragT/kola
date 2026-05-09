use std::marker::PhantomData;

use crate::{
    Failure, Report,
    input::Input,
    parser::{IterParser, ParseResult, Parser},
};

use super::{SplitHead, SplitTail};

pub struct Collect<IP, O, C> {
    pub(super) _marker: PhantomData<(O, C)>,
    pub(super) iter: IP,
}

impl<IP, O, C> Collect<IP, O, C> {
    pub fn split_head(self) -> SplitHead<IP, O, C> {
        SplitHead {
            _marker: PhantomData,
            iter: self.iter,
        }
    }

    pub fn split_tail(self) -> SplitTail<IP, O, C> {
        SplitTail {
            _marker: PhantomData,
            iter: self.iter,
        }
    }
}

impl<IP, O, C> Clone for Collect<IP, O, C>
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

impl<IP, O, C> Copy for Collect<IP, O, C> where IP: Copy {}

impl<I, O, C, IP> Parser<I, C> for Collect<IP, O, C>
where
    I: Input,
    IP: IterParser<I, O>,
    C: Default + Extend<O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<C, I::Token> {
        let mut state = IP::State::default();
        let mut items = C::default();

        loop {
            match self
                .iter
                .drive(&mut state, input, report)
                .map_err(Failure::Raise)?
            {
                Some(o) => items.extend(std::iter::once(o)),
                None => break,
            }
        }

        Ok(items)
    }
}

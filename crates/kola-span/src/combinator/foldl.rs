use std::marker::PhantomData;

use crate::{
    Failure, Loc, Report,
    input::Input,
    parser::{IterParser, ParseResult, Parser},
};

pub struct Foldl<P, IP, F, O1> {
    pub(super) _marker: PhantomData<O1>,
    pub(super) init: P,
    pub(super) iter: IP,
    pub(super) f: F,
}

impl<P, IP, F, O1> Clone for Foldl<P, IP, F, O1>
where
    P: Clone,
    IP: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            init: self.init.clone(),
            iter: self.iter.clone(),
            f: self.f.clone(),
        }
    }
}

impl<P, IP, F, O1> Copy for Foldl<P, IP, F, O1>
where
    P: Copy,
    IP: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, IP, F> Parser<I, O> for Foldl<P, IP, F, O1>
where
    I: Input,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        let mut acc = self.init.parse(input, report)?;
        let mut state = IP::State::default();

        loop {
            match self
                .iter
                .drive(&mut state, input, report)
                .map_err(Failure::Raise)?
            {
                Some(item) => acc = (self.f)(acc, item),
                None => break,
            }
        }

        Ok(acc)
    }
}

pub struct FoldlWith<P, IP, F, O1> {
    pub(super) _marker: PhantomData<O1>,
    pub(super) init: P,
    pub(super) iter: IP,
    pub(super) f: F,
}

impl<P, IP, F, O1> Clone for FoldlWith<P, IP, F, O1>
where
    P: Clone,
    IP: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            init: self.init.clone(),
            iter: self.iter.clone(),
            f: self.f.clone(),
        }
    }
}

impl<P, IP, F, O1> Copy for FoldlWith<P, IP, F, O1>
where
    P: Copy,
    IP: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, IP, F> Parser<I, O> for FoldlWith<P, IP, F, O1>
where
    I: Input,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1, Loc, &mut I) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<O, I::Token> {
        let start = input.loc();
        let mut acc = self.init.parse(input, report)?;
        let mut state = IP::State::default();

        loop {
            match self
                .iter
                .drive(&mut state, input, report)
                .map_err(Failure::Raise)?
            {
                Some(item) => {
                    let loc = start.union(input.prev_loc());
                    acc = (self.f)(acc, item, loc, input);
                }
                None => break,
            }
        }

        Ok(acc)
    }
}

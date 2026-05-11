use std::{fmt::Debug, marker::PhantomData};

use crate::{
    Loc, Report,
    input::Input,
    parser::{Failure, IterParser, Parser},
};

pub struct Foldr<P, IP, F, O1> {
    pub(super) _marker: PhantomData<O1>,
    pub(super) init: P,
    pub(super) iter: IP,
    pub(super) f: F,
}

impl<P, IP, F, O1> Clone for Foldr<P, IP, F, O1>
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

impl<P, IP, F, O1> Copy for Foldr<P, IP, F, O1>
where
    P: Copy,
    IP: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, IP, F> Parser<I, O> for Foldr<P, IP, F, O1>
where
    I: Input,
    O: Debug,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let mut state = IP::State::default();
        let mut items = Vec::new();

        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(item) => items.push(item),
                None => break,
            }
        }

        let init = self.init.parse(input, report)?;
        Ok(items.into_iter().rfold(init, &self.f))
    }
}

// FoldrWith: same but closure gets &mut I
pub struct FoldrWith<P, IP, F, O1> {
    pub(super) _marker: PhantomData<O1>,
    pub(super) init: P,
    pub(super) iter: IP,
    pub(super) f: F,
}

impl<P, IP, F, O1> Clone for FoldrWith<P, IP, F, O1>
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

impl<P, IP, F, O1> Copy for FoldrWith<P, IP, F, O1>
where
    P: Copy,
    IP: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, IP, F> Parser<I, O> for FoldrWith<P, IP, F, O1>
where
    I: Input,
    O: Debug,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O1, O, Loc, &mut I) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Failure> {
        let start = input.loc();
        let mut state = IP::State::default();
        let mut items = Vec::new();

        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(item) => items.push(item),
                None => break,
            }
        }

        let init = self.init.parse(input, report)?;
        let loc = start.union(input.prev_loc());
        Ok(items
            .into_iter()
            .rfold(init, |acc, item| (self.f)(item, acc, loc, input)))
    }
}

use std::marker::PhantomData;

use crate::{
    Report,
    input::Input,
    parser::{ParseResult, Parser},
};

pub struct To<O, P, T> {
    pub(super) _marker: PhantomData<O>,
    pub(super) parser: P,
    pub(super) value: T,
}

impl<O, P, T> Clone for To<O, P, T>
where
    P: Clone,
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            value: self.value.clone(),
        }
    }
}

impl<O, P, T> Copy for To<O, P, T>
where
    P: Copy,
    T: Copy,
{
}

impl<I, O, P, T> Parser<I, T> for To<O, P, T>
where
    I: Input,
    P: Parser<I, O>,
    T: Clone,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> ParseResult<T, I::Token> {
        self.parser.parse(input, report).map(|_| self.value.clone())
    }
}

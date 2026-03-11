use std::{array, marker::PhantomData};

use crate::{Diagnostic, Loc, input::Input, parser::Parser};

pub const trait Combinator<I: Input, O>: Sized + Parser<I, O> {
    fn map<F, O1>(self, f: F) -> Map<Self, F, O>
    where
        F: FnMut(O) -> O1,
    {
        Map {
            _marker: PhantomData,
            parser: self,
            f,
        }
    }

    fn map_with<F, O1>(self, f: F) -> MapWith<Self, F, O>
    where
        F: FnMut(O, &mut I) -> O1,
    {
        MapWith {
            _marker: PhantomData,
            parser: self,
            f,
        }
    }

    fn to<T: Clone>(self, value: T) -> To<O, Self, T> {
        To {
            _marker: PhantomData,
            parser: self,
            value,
        }
    }

    fn then<P, O1>(self, other: P) -> Then<Self, P>
    where
        P: Parser<I, O1>,
    {
        Then {
            first: self,
            second: other,
        }
    }

    fn ignore_then<P, O1>(self, other: P) -> IgnoreThen<Self, P, O>
    where
        P: Parser<I, O1>,
    {
        IgnoreThen {
            _marker: PhantomData,
            first: self,
            second: other,
        }
    }

    fn then_ignore<P, O1>(self, other: P) -> ThenIgnore<Self, P, O1>
    where
        P: Parser<I, O1>,
    {
        ThenIgnore {
            _marker: PhantomData,
            first: self,
            second: other,
        }
    }

    fn or<P, O1>(self, other: P) -> Or<Self, P, O1>
    where
        P: Parser<I, O1>,
    {
        Or {
            _marker: PhantomData,
            first: self,
            second: other,
        }
    }

    fn or_not(self) -> OrNot<Self> {
        OrNot { parser: self }
    }

    fn repeat<const N: usize>(self) -> Repeat<N, Self> {
        Repeat { parser: self }
    }

    fn repeated(self) -> Repeated<Self, O> {
        Repeated {
            _marker: PhantomData,
            parser: self,
            min: 0,
            max: None,
        }
    }

    fn foldl<P, F, C>(self, other: P, f: F) -> Foldl<Self, P, F, C>
    where
        P: Parser<I, C>,
        C: IntoIterator,
        F: Fn(O, C::Item) -> O,
    {
        Foldl {
            _marker: PhantomData,
            left: self,
            right: other,
            f,
        }
    }

    fn foldl_with<P, F, C>(self, other: P, f: F) -> FoldlWith<Self, P, F, C>
    where
        P: Parser<I, C>,
        C: IntoIterator,
        F: Fn(O, C::Item, &mut I) -> O,
    {
        FoldlWith {
            _marker: PhantomData,
            left: self,
            right: other,
            f,
        }
    }

    fn foldr<P1, F, O1>(self, other: P1, f: F) -> Foldr<Self, P1, F, O>
    where
        P1: Parser<I, O1>,
        O: IntoIterator,
        O::IntoIter: DoubleEndedIterator,
        F: Fn(O::Item, O1) -> O1,
    {
        Foldr {
            _marker: PhantomData,
            left: self,
            right: other,
            f,
        }
    }

    fn foldr_with<P1, F, O1>(self, other: P1, f: F) -> FoldrWith<Self, P1, F, O>
    where
        P1: Parser<I, O1>,
        O: IntoIterator,
        O::IntoIter: DoubleEndedIterator,
        F: Fn(O::Item, O1, &mut I) -> O1,
    {
        FoldrWith {
            _marker: PhantomData,
            left: self,
            right: other,
            f,
        }
    }

    fn separated_by<S, OS>(self, sep: S) -> SeparatedBy<Self, S, OS, O>
    where
        S: Parser<I, OS>,
    {
        SeparatedBy {
            _marker: PhantomData,
            parser: self,
            sep,
            allow_leading: false,
            allow_trailing: false,
            min: 0,
            max: None,
        }
    }

    fn delimited_by<P1, P2, O1, O2>(self, open: P1, close: P2) -> DelimitedBy<Self, P1, P2, O1, O2>
    where
        P1: Parser<I, O1>,
        P2: Parser<I, O2>,
    {
        DelimitedBy {
            _marker: PhantomData,
            parser: self,
            open,
            close,
        }
    }

    fn spanned(self) -> Spanned<Self> {
        Spanned { parser: self }
    }

    // fn context(self, message: &'static str) -> Context<Self> {
    //     Context {
    //         parser: self,
    //         message,
    //     }
    // }
}

impl<I, O, P> const Combinator<I, O> for P
where
    I: Input,
    P: Parser<I, O>,
{
}

pub struct Map<P, F, O> {
    _marker: PhantomData<O>,
    parser: P,
    f: F,
}

impl<I, O, O1, P, F> Parser<I, O1> for Map<P, F, O>
where
    I: Input,
    P: Parser<I, O>,
    F: Fn(O) -> O1,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<O1, Vec<Diagnostic>> {
        self.parser.parse(input).map(|ok| (self.f)(ok))
    }
}

pub struct MapWith<P, F, O> {
    _marker: PhantomData<O>,
    parser: P,
    f: F,
}

impl<I, O, O1, P, F> Parser<I, O1> for MapWith<P, F, O>
where
    I: Input,
    P: Parser<I, O>,
    F: Fn(O, &mut I) -> O1,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<O1, Vec<Diagnostic>> {
        self.parser.parse(input).map(|ok| (self.f)(ok, input))
    }
}

pub struct To<O, P, T> {
    _marker: PhantomData<O>,
    parser: P,
    value: T,
}

impl<I, O, P, T> Parser<I, T> for To<O, P, T>
where
    I: Input,
    P: Parser<I, O>,
    T: Clone,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<T, Vec<Diagnostic>> {
        self.parser.parse(input).map(|_| self.value.clone())
    }
}

pub struct Then<P1, P2> {
    first: P1,
    second: P2,
}

impl<I, O, O1, P1, P2> Parser<I, (O, O1)> for Then<P1, P2>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O1>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<(O, O1), Vec<Diagnostic>> {
        let o = self.first.parse(input)?;
        let o1 = self.second.parse(input)?;
        Ok((o, o1))
    }
}

pub struct IgnoreThen<P1, P2, O> {
    _marker: PhantomData<O>,
    first: P1,
    second: P2,
}

impl<I, O, O1, P1, P2> Parser<I, O1> for IgnoreThen<P1, P2, O>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O1>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<O1, Vec<Diagnostic>> {
        self.first.parse(input)?;
        self.second.parse(input)
    }
}

pub struct ThenIgnore<P1, P2, O1> {
    _marker: PhantomData<O1>,
    first: P1,
    second: P2,
}

impl<I, O, O1, P1, P2> Parser<I, O> for ThenIgnore<P1, P2, O1>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O1>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        let o = self.first.parse(input)?;
        self.second.parse(input)?;
        Ok(o)
    }
}

pub struct Or<P1, P2, O> {
    _marker: PhantomData<O>,
    first: P1,
    second: P2,
}

impl<I, O, P1, P2> Parser<I, O> for Or<P1, P2, O>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        let checkpoint = input.checkpoint();
        match self.first.parse(input) {
            Ok(o) => Ok(o),
            Err(_) => {
                input.reset(checkpoint);
                self.second.parse(input)
            }
        }
    }
}

pub struct OrNot<P> {
    parser: P,
}

impl<I, O, P> Parser<I, Option<O>> for OrNot<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<Option<O>, Vec<Diagnostic>> {
        let checkpoint = input.checkpoint();
        match self.parser.parse(input) {
            Ok(o) => Ok(Some(o)),
            Err(_) => {
                input.reset(checkpoint);
                Ok(None)
            }
        }
    }
}

pub struct Repeat<const N: usize, P> {
    parser: P,
}

impl<I, O, P, const N: usize> Parser<I, [O; N]> for Repeat<N, P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<[O; N], Vec<Diagnostic>> {
        let checkpoint = input.checkpoint();
        array::try_from_fn(|_| self.parser.parse(input)).map_err(|e| {
            input.reset(checkpoint);
            e
        })
    }
}

pub struct Repeated<P, O> {
    _marker: PhantomData<O>,
    parser: P,
    min: usize,
    max: Option<usize>,
}

impl<P, O> Repeated<P, O> {
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
}

impl<I, O, C, P> Parser<I, C> for Repeated<P, O>
where
    I: Input,
    C: FromIterator<O>,
    P: Parser<I, O>,
{
    fn parse(&self, input: &mut I) -> Result<C, Vec<Diagnostic>> {
        let mut count = 0usize;

        let items: C = std::iter::from_fn(|| {
            if self.max.is_some_and(|max| count >= max) {
                return None;
            }
            let checkpoint = input.checkpoint();
            match self.parser.parse(input) {
                Ok(item) => {
                    count += 1;
                    Some(item)
                }
                Err(_) => {
                    input.reset(checkpoint);
                    None
                }
            }
        })
        .collect();

        if count >= self.min {
            Ok(items)
        } else {
            Err(vec![Diagnostic::error(
                input.loc(),
                format!("expected at least {} items", self.min),
            )])
        }
    }
}

pub struct Foldl<P1, P2, F, C> {
    _marker: PhantomData<C>,
    left: P1,
    right: P2,
    f: F,
}

impl<I, O, C, P1, P2, F> Parser<I, O> for Foldl<P1, P2, F, C>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, C>,
    C: IntoIterator,
    F: Fn(O, C::Item) -> O,
{
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        let init = self.left.parse(input)?;
        let items = self.right.parse(input)?;
        Ok(items.into_iter().fold(init, &self.f))
    }
}

pub struct FoldlWith<P1, P2, F, C> {
    _marker: PhantomData<C>,
    left: P1,
    right: P2,
    f: F,
}

impl<I, O, C, P1, P2, F> Parser<I, O> for FoldlWith<P1, P2, F, C>
where
    I: Input,
    P1: Parser<I, O>,
    P2: Parser<I, C>,
    C: IntoIterator,
    F: Fn(O, C::Item, &mut I) -> O,
{
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        let init = self.left.parse(input)?;
        let items = self.right.parse(input)?;
        Ok(items
            .into_iter()
            .fold(init, |acc, item| (self.f)(acc, item, input)))
    }
}

pub struct Foldr<P1, P2, F, C> {
    _marker: PhantomData<C>,
    left: P1,
    right: P2,
    f: F,
}

impl<I, O, C, P1, P2, F> Parser<I, O> for Foldr<P1, P2, F, C>
where
    I: Input,
    P1: Parser<I, C>,
    P2: Parser<I, O>,
    C: IntoIterator,
    C::IntoIter: DoubleEndedIterator,
    F: Fn(O, C::Item) -> O,
{
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        let items = self.left.parse(input)?;
        let init = self.right.parse(input)?;
        Ok(items.into_iter().rfold(init, &self.f))
    }
}

pub struct FoldrWith<P1, P2, F, C> {
    _marker: PhantomData<C>,
    left: P1,
    right: P2,
    f: F,
}

impl<I, O, C, P1, P2, F> Parser<I, O> for FoldrWith<P1, P2, F, C>
where
    I: Input,
    P1: Parser<I, C>,
    P2: Parser<I, O>,
    C: IntoIterator,
    C::IntoIter: DoubleEndedIterator,
    F: Fn(O, C::Item, &mut I) -> O,
{
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        let items = self.left.parse(input)?;
        let init = self.right.parse(input)?;
        Ok(items
            .into_iter()
            .rfold(init, |acc, item| (self.f)(acc, item, input)))
    }
}

pub struct SeparatedBy<P, S, OS, O> {
    _marker: PhantomData<(OS, O)>,
    parser: P,
    sep: S,
    allow_leading: bool,
    allow_trailing: bool,
    min: usize,
    max: Option<usize>,
}

impl<P, S, OS, O> SeparatedBy<P, S, OS, O> {
    pub const fn allow_leading(mut self) -> Self {
        self.allow_leading = true;
        self
    }

    pub const fn allow_trailing(mut self) -> Self {
        self.allow_trailing = true;
        self
    }

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
}

impl<I, O, OS, C, P, S> Parser<I, C> for SeparatedBy<P, S, OS, O>
where
    I: Input,
    P: Parser<I, O>,
    S: Parser<I, OS>,
    C: Default + Extend<O>,
{
    fn parse(&self, input: &mut I) -> Result<C, Vec<Diagnostic>> {
        let mut items = C::default();

        // Handle optional leading separator
        let mut leading_sep_present = false;
        if self.allow_leading {
            let checkpoint = input.checkpoint();
            if self.sep.parse(input).is_ok() {
                leading_sep_present = true;
            } else {
                input.reset(checkpoint);
            }
        }

        // Try the first item — zero items is valid when min == 0.
        let first_checkpoint = input.checkpoint();
        let mut count = match self.parser.parse(input) {
            Ok(o) => {
                items.extend(std::iter::once(o));
                1
            }
            Err(e) => {
                input.reset(first_checkpoint);
                // If we consumed a leading separator but can't parse an item, that's an error
                if leading_sep_present {
                    return Err(e);
                }
                if self.min > 0 {
                    return Err(e);
                }
                return Ok(items);
            }
        };

        // Repeatedly try: sep then item.
        loop {
            // Check max limit before trying separator
            if let Some(max) = self.max {
                if count >= max {
                    break;
                }
            }

            let sep_checkpoint = input.checkpoint();

            match self.sep.parse(input) {
                Err(_) => {
                    // No separator found — we're done cleanly.
                    input.reset(sep_checkpoint);
                    break;
                }
                Ok(_) => {
                    match self.parser.parse(input) {
                        Ok(o) => {
                            items.extend(std::iter::once(o));
                            count += 1;
                        }
                        Err(_) if self.allow_trailing => {
                            // Trailing separator is allowed — consume it and stop.
                            break;
                        }
                        Err(_) => {
                            // No trailing allowed — put the separator back and stop.
                            input.reset(sep_checkpoint);
                            break;
                        }
                    }
                }
            }
        }

        if count < self.min {
            return Err(vec![Diagnostic::error(
                input.loc(),
                format!("expected at least {} items but found {}", self.min, count),
            )]);
        }

        Ok(items)
    }
}

pub struct DelimitedBy<P, P1, P2, O1, O2> {
    _marker: PhantomData<(O1, O2)>,
    parser: P,
    open: P1,
    close: P2,
}

impl<I, O, O1, O2, P, P1, P2> Parser<I, O> for DelimitedBy<P, P1, P2, O1, O2>
where
    I: Input,
    P: Parser<I, O>,
    P1: Parser<I, O1>,
    P2: Parser<I, O2>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
        self.open.parse(input)?;
        let result = self.parser.parse(input);
        self.close.parse(input)?;
        result
    }
}

pub struct Spanned<P> {
    parser: P,
}

impl<I, O, P> Parser<I, (O, Loc)> for Spanned<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I) -> Result<(O, Loc), Vec<Diagnostic>> {
        let start = input.loc();
        self.parser.parse(input).map(|o| (o, start))
    }
}

// pub struct Context<P> {
//     parser: P,
//     message: &'static str,
// }

// impl<I, O, P> Parser<I, O> for Context<P>
// where
//     I: Input,
//     P: Parser<I, O>,
// {
//     #[inline]
//     fn parse(&self, input: &mut I) -> Result<O, Vec<Diagnostic>> {
//         self.parser.parse(input).map_err(|mut e| {
//             // TODO this isn't really right I want context information probably in all errors
//             // so maybe use diagnostics.notes ?? but then maybe I can add multiple helpers here
//             // to help create better Diagnostics
//             e.insert(0, Diagnostic::error(input.loc(), self.message.to_owned()));
//             e
//         })
//     }
// }

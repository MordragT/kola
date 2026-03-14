use std::{array, marker::PhantomData};

use crate::{
    Diagnostic, Loc, Report,
    input::Input,
    parser::{IterParser, Parser},
};

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

    fn foldl<IP, F, O1>(self, other: IP, f: F) -> Foldl<Self, IP, F, O1>
    where
        IP: IterParser<I, O1>,
        F: Fn(O, O1) -> O,
    {
        Foldl {
            _marker: PhantomData,
            init: self,
            iter: other,
            f,
        }
    }

    fn foldl_with<IP, F, O1>(self, other: IP, f: F) -> FoldlWith<Self, IP, F, O1>
    where
        IP: IterParser<I, O1>,
        F: Fn(O, O1, &mut I) -> O,
    {
        FoldlWith {
            _marker: PhantomData,
            init: self,
            iter: other,
            f,
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

    fn recover<R>(self, recovery: R) -> Recover<Self, R>
    where
        R: Parser<I, O>,
    {
        Recover {
            parser: self,
            recovery,
        }
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O1, Diagnostic> {
        self.parser.parse(input, report).map(|ok| (self.f)(ok))
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O1, Diagnostic> {
        self.parser
            .parse(input, report)
            .map(|ok| (self.f)(ok, input))
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<T, Diagnostic> {
        self.parser.parse(input, report).map(|_| self.value.clone())
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(O, O1), Diagnostic> {
        let o = self.first.parse(input, report)?;
        let o1 = self.second.parse(input, report)?;
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O1, Diagnostic> {
        self.first.parse(input, report)?;
        self.second.parse(input, report)
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let o = self.first.parse(input, report)?;
        self.second.parse(input, report)?;
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let checkpoint = input.checkpoint();
        match self.first.parse(input, report) {
            Ok(o) => Ok(o),
            Err(_) => {
                input.reset(checkpoint);
                self.second.parse(input, report)
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<Option<O>, Diagnostic> {
        let checkpoint = input.checkpoint();
        match self.parser.parse(input, report) {
            Ok(o) => Ok(Some(o)),
            Err(_) => {
                input.reset(checkpoint);
                Ok(None)
            }
        }
    }
}

// TODO rename to Many and create as a primitive ?
pub struct Repeat<const N: usize, P> {
    parser: P,
}

impl<I, O, P, const N: usize> Parser<I, [O; N]> for Repeat<N, P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<[O; N], Diagnostic> {
        let checkpoint = input.checkpoint();
        array::try_from_fn(|_| self.parser.parse(input, report)).map_err(|e| {
            input.reset(checkpoint);
            e
        })
    }
}

pub struct Foldl<P, IP, F, O1> {
    _marker: PhantomData<O1>,
    init: P,
    iter: IP,
    f: F,
}

impl<I, O, O1, P, IP, F> Parser<I, O> for Foldl<P, IP, F, O1>
where
    I: Input,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let mut acc = self.init.parse(input, report)?;
        let mut state = IP::State::default();

        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(item) => acc = (self.f)(acc, item),
                None => break,
            }
        }

        Ok(acc)
    }
}

pub struct FoldlWith<P, IP, F, O1> {
    _marker: PhantomData<O1>,
    init: P,
    iter: IP,
    f: F,
}

impl<I, O, O1, P, IP, F> Parser<I, O> for FoldlWith<P, IP, F, O1>
where
    I: Input,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1, &mut I) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let mut acc = self.init.parse(input, report)?;
        let mut state = IP::State::default();

        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(item) => acc = (self.f)(acc, item, input),
                None => break,
            }
        }

        Ok(acc)
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        self.open.parse(input, report)?;
        let result = self.parser.parse(input, report);
        self.close.parse(input, report)?;
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(O, Loc), Diagnostic> {
        let start = input.loc();
        self.parser.parse(input, report).map(|o| (o, start))
    }
}

pub struct Recover<P, R> {
    parser: P,
    recovery: R,
}

impl<I, O, P, R> Parser<I, O> for Recover<P, R>
where
    I: Input,
    P: Parser<I, O>,
    R: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let checkpoint = input.checkpoint();
        match self.parser.parse(input, report) {
            Ok(o) => Ok(o),
            Err(e) => {
                input.reset(checkpoint);
                report.add_diagnostic(e);
                self.recovery.parse(input, report)
            }
        }
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
//     fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
//         self.parser.parse(input, report).map_err(|mut e| {
//             // TODO this isn't really right I want context information probably in all errors
//             // so maybe use diagnostics.notes ?? but then maybe I can add multiple helpers here
//             // to help create better Diagnostics
//             e.insert(0, Diagnostic::error(input.loc(), self.message.to_owned()));
//             e
//         })
//     }
// }

pub const trait IterCombinator<I: Input, O>: IterParser<I, O> {
    fn collect<C>(self) -> Collect<Self, O, C> {
        Collect {
            _marker: PhantomData,
            iter: self,
        }
    }

    fn separated_by<S, OS>(self, sep: S) -> SeparatedBy<Self, S, OS, O>
    where
        S: Parser<I, OS>,
    {
        SeparatedBy {
            _marker: PhantomData,
            iter: self,
            sep,
            allow_leading: false,
            allow_trailing: false,
        }
    }

    fn foldr<P1, F, O1>(self, other: P1, f: F) -> Foldr<P1, Self, F, O>
    where
        P1: Parser<I, O1>,
        F: Fn(O, O1) -> O1,
    {
        Foldr {
            _marker: PhantomData,
            iter: self,
            init: other,
            f,
        }
    }

    fn foldr_with<P1, F, O1>(self, other: P1, f: F) -> FoldrWith<P1, Self, F, O>
    where
        P1: Parser<I, O1>,
        F: Fn(O, O1, &mut I) -> O1,
    {
        FoldrWith {
            _marker: PhantomData,
            iter: self,
            init: other,
            f,
        }
    }

    // count, collect
}

impl<I, O, IP> const IterCombinator<I, O> for IP
where
    I: Input,
    IP: IterParser<I, O>,
{
}

pub struct Collect<IP, O, C> {
    _marker: PhantomData<(O, C)>,
    iter: IP,
}

impl<I, O, C, IP> Parser<I, C> for Collect<IP, O, C>
where
    I: Input,
    IP: IterParser<I, O>,
    C: Default + Extend<O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<C, Diagnostic> {
        let mut state = IP::State::default();
        let mut items = C::default();

        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(o) => items.extend(std::iter::once(o)),
                None => break,
            }
        }

        Ok(items)
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

impl<I, O, P> IterParser<I, O> for Repeated<P, O>
where
    I: Input,
    P: Parser<I, O>,
{
    type State = usize;

    fn drive(
        &self,
        state: &mut usize,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        if self.max.is_some_and(|max| *state >= max) {
            return Ok(None);
        }
        let checkpoint = input.checkpoint();
        match self.parser.parse(input, report) {
            Ok(o) => {
                *state += 1;
                Ok(Some(o))
            }
            Err(e) => {
                input.reset(checkpoint);
                if *state < self.min { Err(e) } else { Ok(None) }
            }
        }
    }
}

pub struct SeparatedBy<IC, S, OS, O> {
    _marker: PhantomData<(OS, O)>,
    iter: IC,
    sep: S,
    allow_leading: bool,
    allow_trailing: bool,
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
}
pub struct SeparatedByState<S> {
    inner: S,
    first: bool,
}

impl<S: Default> Default for SeparatedByState<S> {
    fn default() -> Self {
        Self {
            inner: S::default(),
            first: true,
        }
    }
}

impl<I, O, OS, IP, S> IterParser<I, O> for SeparatedBy<IP, S, OS, O>
where
    I: Input,
    IP: IterParser<I, O>,
    S: Parser<I, OS>,
{
    type State = SeparatedByState<IP::State>;

    fn drive(
        &self,
        state: &mut Self::State,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        // Handle separator (skip on first item)
        if !state.first {
            let checkpoint = input.checkpoint();
            match self.sep.parse(input, report) {
                Ok(_) => {}
                Err(_) => {
                    input.reset(checkpoint);
                    return Ok(None); // No separator = done, cleanly
                }
            }
        } else {
            // Handle optional leading separator
            if self.allow_leading {
                let checkpoint = input.checkpoint();
                if self.sep.parse(input, report).is_err() {
                    input.reset(checkpoint);
                }
            }
        }

        state.first = false;

        // Try next item
        let checkpoint = input.checkpoint();
        match self.iter.drive(&mut state.inner, input, report)? {
            Some(o) => Ok(Some(o)),
            None => {
                // No item after separator
                if self.allow_trailing {
                    Ok(None)
                } else {
                    input.reset(checkpoint);
                    Ok(None)
                }
            }
        }
    }
}

pub struct Foldr<P, IP, F, O1> {
    _marker: PhantomData<O1>,
    init: P,
    iter: IP,
    f: F,
}

impl<I, O, O1, P, IP, F> Parser<I, O> for Foldr<P, IP, F, O1>
where
    I: Input,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
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
    _marker: PhantomData<O1>,
    init: P,
    iter: IP,
    f: F,
}

impl<I, O, O1, P, IP, F> Parser<I, O> for FoldrWith<P, IP, F, O1>
where
    I: Input,
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O, O1, &mut I) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let mut state = IP::State::default();
        let mut items = Vec::new();

        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(item) => items.push(item),
                None => break,
            }
        }

        let init = self.init.parse(input, report)?;
        Ok(items
            .into_iter()
            .rfold(init, |acc, item| (self.f)(acc, item, input)))
    }
}

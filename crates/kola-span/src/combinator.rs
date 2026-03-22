use std::{array, marker::PhantomData};

use crate::{
    Diagnostic, Loc, Report,
    input::Input,
    parser::{IterParser, Parser},
    pratt::{Pratt, pratt},
};

pub const trait Combinator<I: Input, O>: Parser<I, O> + Copy {
    fn map<F, O1>(self, f: F) -> Map<Self, F, O>
    where
        F: Fn(O) -> O1,
    {
        Map {
            _marker: PhantomData,
            parser: self,
            f,
        }
    }

    fn map_with<F, O1>(self, f: F) -> MapWith<Self, F, O>
    where
        F: Fn(O, Loc, &mut I) -> O1,
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
        F: Fn(O, O1, Loc, &mut I) -> O,
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

    fn pratt<Ops>(self, ops: Ops) -> Pratt<Self, Ops, O> {
        pratt(self, ops)
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

    fn rewind(self) -> Rewind<Self> {
        Rewind { parser: self }
    }

    fn with_help(self, help: &'static str) -> WithHelp<Self> {
        WithHelp { parser: self, help }
    }

    fn with_note(self, note: &'static str) -> WithNote<Self> {
        WithNote { parser: self, note }
    }

    /// Skip tokens until `predicate` returns true (predicate matches the upcoming token).
    /// The matching token is NOT consumed. Returns the `Loc` spanning the skipped region.
    fn skip_until<F>(self, predicate: F) -> SkipUntil<F>
    where
        F: Fn(&I::Token) -> bool,
    {
        SkipUntil { predicate }
    }
}

impl<I, O, P> const Combinator<I, O> for P
where
    I: Input,
    P: Parser<I, O> + Copy,
{
}

pub struct Map<P, F, O> {
    _marker: PhantomData<O>,
    parser: P,
    f: F,
}

impl<P, F, O> Clone for Map<P, F, O>
where
    P: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            f: self.f.clone(),
        }
    }
}

impl<P, F, O> Copy for Map<P, F, O>
where
    P: Copy,
    F: Copy,
{
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

impl<P, F, O> Clone for MapWith<P, F, O>
where
    P: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            f: self.f.clone(),
        }
    }
}

impl<P, F, O> Copy for MapWith<P, F, O>
where
    P: Copy,
    F: Copy,
{
}

impl<I, O, O1, P, F> Parser<I, O1> for MapWith<P, F, O>
where
    I: Input,
    P: Parser<I, O>,
    F: Fn(O, Loc, &mut I) -> O1,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O1, Diagnostic> {
        let start = input.loc();
        let result = self.parser.parse(input, report)?;
        let loc = start.union(input.prev_loc());
        Ok((self.f)(result, loc, input))
    }
}

pub struct To<O, P, T> {
    _marker: PhantomData<O>,
    parser: P,
    value: T,
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<T, Diagnostic> {
        self.parser.parse(input, report).map(|_| self.value.clone())
    }
}

#[derive(Clone, Copy)]
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

impl<P1, P2, O> Clone for IgnoreThen<P1, P2, O>
where
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            first: self.first.clone(),
            second: self.second.clone(),
        }
    }
}

impl<P1, P2, O> Copy for IgnoreThen<P1, P2, O>
where
    P1: Copy,
    P2: Copy,
{
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

impl<P1, P2, O1> Clone for ThenIgnore<P1, P2, O1>
where
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            first: self.first.clone(),
            second: self.second.clone(),
        }
    }
}

impl<P1, P2, O1> Copy for ThenIgnore<P1, P2, O1>
where
    P1: Copy,
    P2: Copy,
{
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

impl<P1, P2, O> Clone for Or<P1, P2, O>
where
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            first: self.first.clone(),
            second: self.second.clone(),
        }
    }
}

impl<P1, P2, O> Copy for Or<P1, P2, O>
where
    P1: Copy,
    P2: Copy,
{
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

#[derive(Clone, Copy)]
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
#[derive(Clone, Copy)]
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let start = input.loc();
        let mut acc = self.init.parse(input, report)?;
        let mut state = IP::State::default();

        loop {
            match self.iter.drive(&mut state, input, report)? {
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

pub struct DelimitedBy<P, P1, P2, O1, O2> {
    _marker: PhantomData<(O1, O2)>,
    parser: P,
    open: P1,
    close: P2,
}

impl<P, P1, P2, O1, O2> Clone for DelimitedBy<P, P1, P2, O1, O2>
where
    P: Clone,
    P1: Clone,
    P2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            open: self.open.clone(),
            close: self.close.clone(),
        }
    }
}

impl<P, P1, P2, O1, O2> Copy for DelimitedBy<P, P1, P2, O1, O2>
where
    P: Copy,
    P1: Copy,
    P2: Copy,
{
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

#[derive(Clone, Copy)]
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
        let result = self.parser.parse(input, report)?;
        let loc = start.union(input.prev_loc());
        Ok((result, loc))
    }
}

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
pub struct Rewind<P> {
    parser: P,
}

impl<I: Input, O, P: Parser<I, O>> Parser<I, O> for Rewind<P> {
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        let checkpoint = input.checkpoint();
        let result = self.parser.parse(input, report);
        // Always reset the input, whether it succeeded or failed!
        input.reset(checkpoint);
        result
    }
}

#[derive(Clone, Copy)]
pub struct WithHelp<P> {
    parser: P,
    help: &'static str,
}

impl<I, O, P> Parser<I, O> for WithHelp<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        self.parser
            .parse(input, report)
            .map_err(|e| e.with_help(self.help))
    }
}

#[derive(Clone, Copy)]
pub struct WithNote<P> {
    parser: P,
    note: &'static str,
}

impl<I, O, P> Parser<I, O> for WithNote<P>
where
    I: Input,
    P: Parser<I, O>,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        self.parser
            .parse(input, report)
            .map_err(|e| e.with_note(self.note))
    }
}

#[derive(Clone, Copy)]
pub struct SkipUntil<F> {
    predicate: F,
}

impl<I, F> Parser<I, Loc> for SkipUntil<F>
where
    I: Input,
    F: Fn(&I::Token) -> bool,
{
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<Loc, Diagnostic> {
        let start = input.loc();
        loop {
            match input.peek() {
                Some(tok) => {
                    if (self.predicate)(&tok) {
                        // Do not consume the matching token; stop here.
                        break;
                    } else {
                        input.advance();
                    }
                }
                None => break,
            }
        }
        let loc = start.union(input.prev_loc());
        Ok(loc)
    }
}

#[derive(Clone, Copy)]
pub struct SkipDelimiters<T, const N: usize> {
    /// Index into `pairs` designating the primary target pair.
    /// Must be < N for the target to be valid. If out of range, parsing will
    /// gracefully recover by scanning until EOF (no special target).
    target_index: usize,
    pairs: [(T, T); N],
}

/// Convenience constructor: pass a target index (must be < N) and an array of pairs to track.
pub const fn skip_delimiters<T, const N: usize>(
    target_index: usize,
    pairs: [(T, T); N],
) -> SkipDelimiters<T, N>
where
    T: Copy + PartialEq,
{
    // Assert at construction time that the target index is valid.
    // In const contexts this becomes a compile-time error if misused.
    assert!(target_index < N);
    SkipDelimiters {
        target_index,
        pairs,
    }
}

impl<I, T, const N: usize> Parser<I, Loc> for SkipDelimiters<T, N>
where
    I: Input<Token = T>,
    T: Copy + PartialEq,
{
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<Loc, Diagnostic> {
        let start = input.loc();
        let mut stack: Vec<usize> = Vec::new();

        'outer: while let Some(tok) = input.advance() {
            // If we see the target close at base depth, we've already consumed it; stop.
            if tok == self.pairs[self.target_index].1 && stack.is_empty() {
                break;
            }

            for (idx, (open, close)) in self.pairs.iter().enumerate() {
                if tok == *open {
                    stack.push(idx);
                    break;
                } else if tok == *close {
                    if stack.last().copied() == Some(idx) {
                        stack.pop();
                        // If we just popped the final opener and it belonged to the target pair,
                        // then we've consumed the matching close for the target and should stop.
                        if idx == self.target_index && stack.is_empty() {
                            break 'outer;
                        }
                    }
                    break;
                }
            }
        }

        let loc = start.union(input.prev_loc());
        Ok(loc)
    }
}

pub const trait IterCombinator<I: Input, O>: IterParser<I, O> + Copy {
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
        F: Fn(O, O1, Loc, &mut I) -> O1,
    {
        FoldrWith {
            _marker: PhantomData,
            iter: self,
            init: other,
            f,
        }
    }
    // count
}

impl<I, O, IP> const IterCombinator<I, O> for IP
where
    I: Input,
    IP: IterParser<I, O> + Copy,
{
}

pub struct Collect<IP, O, C> {
    _marker: PhantomData<(O, C)>,
    iter: IP,
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

pub struct SplitHead<IP, O, C> {
    _marker: PhantomData<(O, C)>,
    iter: IP,
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(O, C), Diagnostic> {
        let mut state = IP::State::default();

        let checkpoint = input.checkpoint();
        let loc = input.loc();

        // Parse head item
        let head = match self.iter.drive(&mut state, input, report)? {
            Some(o) => o,
            None => {
                input.reset(checkpoint);
                return Err(Diagnostic::error(loc, "expected at least one item"));
            }
        };

        // Parse tail items
        let mut tail = C::default();
        loop {
            match self.iter.drive(&mut state, input, report)? {
                Some(o) => tail.extend(std::iter::once(o)),
                None => break,
            }
        }

        Ok((head, tail))
    }
}

pub struct SplitTail<IP, O, C> {
    _marker: PhantomData<(O, C)>,
    iter: IP,
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
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<(C, O), Diagnostic> {
        let mut state = IP::State::default();
        let mut head = C::default();

        // 1. Parse the first item. If None, we fail immediately.
        let mut prev = match self.iter.drive(&mut state, input, report)? {
            Some(o) => o,
            None => return Err(Diagnostic::error(input.loc(), "expected at least one item")),
        };

        // 2. Loop lazily
        loop {
            match self.iter.drive(&mut state, input, report)? {
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

pub struct Repeated<P, O> {
    _marker: PhantomData<O>,
    parser: P,
    min: usize,
    max: Option<usize>,
}

impl<P, O> Clone for Repeated<P, O>
where
    P: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            parser: self.parser.clone(),
            min: self.min,
            max: self.max,
        }
    }
}

impl<P, O> Copy for Repeated<P, O> where P: Copy {}

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

impl<IC, S, OS, O> Clone for SeparatedBy<IC, S, OS, O>
where
    IC: Clone,
    S: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            iter: self.iter.clone(),
            sep: self.sep.clone(),
            allow_leading: self.allow_leading,
            allow_trailing: self.allow_trailing,
        }
    }
}

impl<IC, S, OS, O> Copy for SeparatedBy<IC, S, OS, O>
where
    IC: Copy,
    S: Copy,
{
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
        let checkpoint = input.checkpoint();

        // Handle separator (skip on first item)
        if !state.first {
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
                if self.sep.parse(input, report).is_err() {
                    input.reset(checkpoint);
                }
            }
        }

        state.first = false;

        // Try next item
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
    P: Parser<I, O>,
    IP: IterParser<I, O1>,
    F: Fn(O1, O, Loc, &mut I) -> O,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
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

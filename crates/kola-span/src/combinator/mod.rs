use std::{fmt::Debug, marker::PhantomData};

use crate::{
    Loc, Report,
    input::Input,
    parser::{IterParser, Parser},
    pratt::{Pratt, pratt},
    skip::Skip,
};

mod catch;
mod collect;
mod delimited_by;
mod foldl;
mod foldr;
mod many;
mod map;
mod or;
mod repeated;
mod rewind;
mod separated_by;
mod spanned;
mod split;
mod then;
mod throw;
mod to;

pub use catch::Catch;
pub use collect::Collect;
pub use delimited_by::DelimitedBy;
pub use foldl::{Foldl, FoldlWith};
pub use foldr::{Foldr, FoldrWith};
pub use many::Many;
pub use map::{Map, MapWith};
pub use or::{Or, OrElse, OrNot};
pub use repeated::Repeated;
pub use rewind::Rewind;
pub use separated_by::SeparatedBy;
pub use spanned::Spanned;
pub use split::{SplitHead, SplitTail};
pub use then::{IgnoreThen, Then, ThenIgnore};
pub use throw::Throw;
pub use to::To;

pub const trait Combinator<I: Input, O: Debug>: Parser<I, O> + Copy {
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
        O1: Debug,
        P: Parser<I, O1>,
    {
        Then {
            first: self,
            second: other,
        }
    }

    fn ignore_then<P, O1>(self, other: P) -> IgnoreThen<Self, P, O>
    where
        O1: Debug,
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
        O1: Debug,
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
        O1: Debug,
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

    fn or_else<S, F>(self, skipper: S, fallback: F) -> OrElse<Self, S, F>
    where
        S: Skip<I>,
        F: Fn(Loc, &mut I) -> O,
    {
        OrElse {
            parser: self,
            skipper,
            fallback,
        }
    }

    fn many<const N: usize>(self) -> Many<N, Self> {
        Many { parser: self }
    }

    fn repeated(self) -> Repeated<Self, O> {
        Repeated {
            _marker: PhantomData,
            parser: self,
            min: 0,
            max: None,
        }
    }

    fn separated_by<S, OS>(self, sep: S) -> SeparatedBy<Self, S, OS, O>
    where
        OS: Debug,
        S: Parser<I, OS>,
    {
        SeparatedBy {
            _marker: PhantomData,
            parser: self,
            min: 0,
            max: None,
            sep,
            allow_leading: false,
            allow_trailing: false,
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
        O1: Debug,
        O2: Debug,
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

    fn rewind(self) -> Rewind<Self> {
        Rewind { parser: self }
    }

    fn throw(self, reason: &'static str) -> Throw<Self> {
        Throw {
            parser: self,
            reason,
            note: None,
        }
    }

    fn catch<S, F>(self, skipper: S, fallback: F) -> Catch<Self, S, F>
    where
        S: Skip<I>,
        F: Fn(Loc, Report, &mut I) -> O,
    {
        Catch {
            parser: self,
            skipper,
            fallback,
        }
    }
}

impl<I, O, P> const Combinator<I, O> for P
where
    O: Debug,
    I: Input,
    P: Parser<I, O> + Copy,
{
}

pub const trait IterCombinator<I: Input, O>: IterParser<I, O> + Copy {
    fn catch<S, F>(self, skipper: S, fallback: F) -> Catch<Self, S, F>
    where
        S: Skip<I>,
        F: Fn(Loc, Report, &mut I) -> O,
    {
        Catch {
            parser: self,
            skipper,
            fallback,
        }
    }

    fn collect<C>(self) -> Collect<Self, O, C> {
        Collect {
            _marker: PhantomData,
            iter: self,
        }
    }

    fn foldr<P1, F, O1>(self, other: P1, f: F) -> Foldr<P1, Self, F, O>
    where
        O1: Debug,
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
        O1: Debug,
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

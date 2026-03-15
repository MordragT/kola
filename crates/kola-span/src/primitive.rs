use crate::{Diagnostic, Loc, Report, combinator::Combinator, input::Input, parser::Parser};

pub const trait Lazy<I: Input, O> {
    type Combinator: Combinator<I, O>;
    const COMBINATOR: Self::Combinator;
}

pub struct OpaqueFn<I, O> {
    f: fn(&mut I, &mut Report) -> Result<O, Diagnostic>,
}

impl<I, O> Clone for OpaqueFn<I, O> {
    fn clone(&self) -> Self {
        Self { f: self.f }
    }
}

impl<I, O> Copy for OpaqueFn<I, O> {}

pub const fn lazy<I, O, R>() -> OpaqueFn<I, O>
where
    I: Input,
    R: Lazy<I, O>,
{
    #[inline(never)]
    fn trampoline<I, O, R>(input: &mut I, report: &mut Report) -> Result<O, Diagnostic>
    where
        I: Input,
        R: Lazy<I, O>,
    {
        R::COMBINATOR.parse(input, report)
    }

    OpaqueFn {
        f: trampoline::<I, O, R>,
    }
}

impl<I, O> Parser<I, O> for OpaqueFn<I, O>
where
    I: Input,
{
    #[inline]
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        (self.f)(input, report)
    }
}

#[derive(Clone, Copy)]
pub struct Any {}

impl<I: Input> Parser<I, Loc> for Any {
    #[inline]
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<Loc, Diagnostic> {
        match input.peek() {
            Some(_) => {
                let loc = input.loc();
                input.advance();
                Ok(loc)
            }
            None => Err(Diagnostic::error(input.loc(), "unexpected end of input")),
        }
    }
}

pub const fn any() -> Any {
    Any {}
}

pub struct Just<I: Input> {
    expected: I::Token,
}

impl<I: Input> Clone for Just<I> {
    fn clone(&self) -> Self {
        Self {
            expected: self.expected.clone(),
        }
    }
}

impl<I> Copy for Just<I>
where
    I: Input,
    I::Token: Copy,
{
}

impl<I: Input> Parser<I, Loc> for Just<I> {
    #[inline]
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<Loc, Diagnostic> {
        let loc = input.loc();
        match input.peek() {
            Some(t) if t == self.expected => {
                input.advance();
                Ok(loc)
            }
            Some(_) => Err(Diagnostic::error(
                loc,
                format!("unexpected token, expected {:?}", self.expected),
            )),
            None => Err(Diagnostic::error(loc, "unexpected end of input")),
        }
    }
}

pub const fn just<I: Input>(expected: I::Token) -> Just<I> {
    Just { expected }
}

#[derive(Clone, Copy)]
pub struct Choice<T> {
    parsers: T,
}

#[derive(Clone, Copy)]
pub struct Group<T> {
    parser: T,
}

macro_rules! impl_choice_tuple {
    () => {};
    ($head:ident $(, $tail:ident)* $(,)?) => {
        impl_choice_tuple!($($tail),*);
        impl_choice_tuple!(~ $head $(, $tail)*);
    };
    (~ $Head:ident $(, $X:ident)* $(,)?) => {
        impl<I, O, $Head, $($X),*> Parser<I, O> for Choice<($Head, $($X,)*)>
        where
            I: Input,
            $Head: Parser<I, O>,
            $($X: Parser<I, O>,)*
        {
            #[allow(non_snake_case)]
            fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
                let (ref $Head, $(ref $X,)*) = self.parsers;
                let checkpoint = input.checkpoint();
                let mut error = $crate::Diagnostic::error(input.loc(), "Error in choice combinator");
                match $Head.parse(input, report) {
                    Ok(o) => return Ok(o),
                    Err(e) => { input.reset(checkpoint); error.add_trace_element(e.message, e.loc); }
                }
                $(
                    match $X.parse(input, report) {
                        Ok(o) => return Ok(o),
                        Err(e) => { input.reset(checkpoint);  error.add_trace_element(e.message, e.loc); }
                    }
                )*
                Err(error)
            }
        }
    };
}

impl_choice_tuple!(
    P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16
);

macro_rules! impl_group_tuple {
    () => {};
    (($head_p:ident, $head_o:ident) $(, ($tail_p:ident, $tail_o:ident))* $(,)?) => {
        impl_group_tuple!($(($tail_p, $tail_o)),*);
        impl_group_tuple!(~ ($head_p, $head_o) $(, ($tail_p, $tail_o))*);
    };
    (~ ($Head:ident, $HO:ident) $(, ($X:ident, $XO:ident))* $(,)?) => {
        impl<I, $Head, $HO, $($X, $XO),*> Parser<I, ($HO, $($XO,)*)> for Group<($Head, $($X,)*)>
        where
            I: Input,
            $Head: Parser<I, $HO>,
            $($X: Parser<I, $XO>,)*
        {
            #[allow(non_snake_case)]
            fn parse(&self, input: &mut I, report: &mut Report) -> Result<($HO, $($XO,)*), Diagnostic> {
                let (ref $Head, $(ref $X,)*) = self.parser;
                let checkpoint = input.checkpoint();
                let $HO = match $Head.parse(input, report) {
                    Ok(o) => o,
                    Err(e) => { input.reset(checkpoint); return Err(e); }
                };
                $(
                    let $XO = match $X.parse(input, report) {
                        Ok(o) => o,
                        Err(e) => { input.reset(checkpoint); return Err(e); }
                    };
                )*
                Ok(($HO, $($XO,)*))
            }
        }
    };
}

impl_group_tuple!(
    (P1, O1),
    (P2, O2),
    (P3, O3),
    (P4, O4),
    (P5, O5),
    (P6, O6),
    (P7, O7),
    (P8, O8),
    (P9, O9),
    (P10, O10),
    (P11, O11),
    (P12, O12),
    (P13, O13),
    (P14, O14),
    (P15, O15),
    (P16, O16)
);

pub const fn choice<T>(parsers: T) -> Choice<T> {
    Choice { parsers }
}

pub const fn group<T>(parser: T) -> Group<T> {
    Group { parser }
}

#[derive(Clone, Copy)]
pub struct Select<F>(pub F);

impl<I, O, F> Parser<I, O> for Select<F>
where
    I: Input,
    F: Fn(I::Token) -> Option<O>,
{
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<O, Diagnostic> {
        let loc = input.loc();
        match input.peek() {
            Some(token) => match (self.0)(token) {
                Some(output) => {
                    input.advance();
                    Ok(output)
                }
                None => Err(Diagnostic::error(loc, "unexpected token")),
            },
            None => Err(Diagnostic::error(loc, "unexpected end of input")),
        }
    }
}

#[macro_export]
macro_rules! select {
    ($($pat:pat $(if $guard:expr)? => $expr:expr),+ $(,)?) => {
        Select(|token| match token {
            $($pat $(if $guard)? => Some($expr),)+
            _ => None,
        })
    };
}

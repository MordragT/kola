use crate::{Diagnostic, Loc, Report, input::Input, parser::Parser};

pub struct Any {}

impl<I: Input> Parser<I, Loc> for Any {
    #[inline]
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<Loc, Vec<Diagnostic>> {
        match input.peek() {
            Some(_) => {
                let loc = input.loc();
                input.advance();
                Ok(loc)
            }
            None => Err(vec![Diagnostic::error(
                input.loc(),
                "unexpected end of input",
            )]),
        }
    }
}

pub const fn any() -> Any {
    Any {}
}

pub struct Just<I: Input> {
    expected: I::Token,
}

impl<I: Input> Parser<I, Loc> for Just<I> {
    #[inline]
    fn parse(&self, input: &mut I, _report: &mut Report) -> Result<Loc, Vec<Diagnostic>> {
        let loc = input.loc();
        match input.peek() {
            Some(t) if t == self.expected => {
                input.advance();
                Ok(loc)
            }
            Some(_) => Err(vec![Diagnostic::error(
                loc,
                format!("unexpected token, expected {:?}", self.expected),
            )]),
            None => Err(vec![Diagnostic::error(loc, "unexpected end of input")]),
        }
    }
}

pub const fn just<I: Input>(expected: I::Token) -> Just<I> {
    Just { expected }
}

pub struct Choice<T> {
    parsers: T,
}

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
            fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Vec<Diagnostic>> {
                let (ref $Head, $(ref $X,)*) = self.parsers;
                let checkpoint = input.checkpoint();
                let mut errors = Vec::new();
                match $Head.parse(input, report) {
                    Ok(o) => return Ok(o),
                    Err(mut e) => { input.reset(checkpoint); errors.append(&mut e); }
                }
                $(
                    match $X.parse(input, report) {
                        Ok(o) => return Ok(o),
                        Err(mut e) => { input.reset(checkpoint); errors.append(&mut e); }
                    }
                )*
                Err(errors)
            }
        }
    };
}

impl_choice_tuple!(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12);

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
            fn parse(&self, input: &mut I, report: &mut Report) -> Result<($HO, $($XO,)*), Vec<Diagnostic>> {
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
);

pub const fn choice<T>(parsers: T) -> Choice<T> {
    Choice { parsers }
}

pub const fn group<T>(parser: T) -> Group<T> {
    Group { parser }
}

use std::marker::PhantomData;

use crate::{Diagnostic, Loc, Report, input::Input, parser::Parser};

/// Result of attempting to match an operator in the Pratt parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpMatch<O> {
    /// The operator matched and a new left-hand side was produced, along with its location.
    Matched(O, Loc),
    /// The operator did not match (either precedence was too low or parser failed),
    /// returning the original left-hand side unchanged.
    Unmatched(O, Loc),
}

/// Helper trait to expose `parse_bp` generically to the operator implementations.
pub trait PrattParser<I: Input, O>: Parser<I, O> {
    fn parse_bp(
        &self,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
    ) -> Result<(O, Loc), Diagnostic>;
}

/// Trait representing a collection of operators (prefix, infix, postfix)
/// checked sequentially during the Pratt loop at runtime.
pub trait PrattOps<I: Input, O> {
    /// Attempt to parse a prefix operator and return the parsed value.
    fn parse_prefix<P: PrattParser<I, O>>(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<(O, Loc)>, Diagnostic>;

    /// Attempt to parse an infix or postfix operator.
    fn parse_infix<P: PrattParser<I, O>>(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
        lhs_loc: Loc,
    ) -> Result<OpMatch<O>, Diagnostic>;
}

/// A Pratt (Top-Down Operator Precedence) parser combinator.
///
/// Combines operators in a `const` context while executing a fast, flat parsing loop at runtime.
#[derive(Debug, Clone, Copy)]
pub struct Pratt<Atom, Ops, O> {
    _marker: PhantomData<O>,
    atom: Atom,
    ops: Ops,
}

/// Create a new Pratt parser with the given atom parser and operators.
pub const fn pratt<Atom, Ops, O>(atom: Atom, ops: Ops) -> Pratt<Atom, Ops, O> {
    Pratt {
        _marker: PhantomData,
        atom,
        ops,
    }
}

impl<I, O, Atom, Ops> PrattParser<I, O> for Pratt<Atom, Ops, O>
where
    I: Input,
    Atom: Parser<I, O>,
    Ops: PrattOps<I, O>,
{
    fn parse_bp(
        &self,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
    ) -> Result<(O, Loc), Diagnostic> {
        // 1. Parse prefix or atom
        let (mut lhs, mut lhs_loc) =
            if let Some(prefix_expr) = self.ops.parse_prefix(self, input, report)? {
                prefix_expr
            } else {
                let start_loc = input.loc();
                let atom = self.atom.parse(input, report)?;
                let end_loc = input.prev_loc();
                (atom, start_loc.union(end_loc))
            };

        // 2. Loop for infix / postfix operators
        loop {
            match self
                .ops
                .parse_infix(self, input, report, min_bp, lhs, lhs_loc)?
            {
                OpMatch::Matched(new_lhs, new_loc) => {
                    lhs = new_lhs;
                    lhs_loc = new_loc;
                }
                OpMatch::Unmatched(original_lhs, original_loc) => {
                    return Ok((original_lhs, original_loc));
                }
            }
        }
    }
}

impl<I, O, Atom, Ops> Parser<I, O> for Pratt<Atom, Ops, O>
where
    I: Input,
    Atom: Parser<I, O>,
    Ops: PrattOps<I, O>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        self.parse_bp(input, report, 0).map(|(o, _)| o)
    }
}

/// The empty set of operators (base case for the cons list).
#[derive(Debug, Clone, Copy)]
pub struct PrattNil;

impl PrattNil {
    pub const fn prefix<OpParser, F, Op>(
        self,
        op_parser: OpParser,
        r_bp: u8,
        f: F,
    ) -> PrattCons<Self, PrefixOp<OpParser, F, Op>> {
        PrattCons {
            tail: self,
            head: PrefixOp::new(op_parser, r_bp, f),
        }
    }

    pub const fn infix<OpParser, F, Op>(
        self,
        op_parser: OpParser,
        l_bp: u8,
        r_bp: u8,
        f: F,
    ) -> PrattCons<Self, InfixOp<OpParser, F, Op>> {
        PrattCons {
            tail: self,
            head: InfixOp::new(op_parser, l_bp, r_bp, f),
        }
    }

    pub const fn postfix<OpParser, F, Op>(
        self,
        op_parser: OpParser,
        l_bp: u8,
        f: F,
    ) -> PrattCons<Self, PostfixOp<OpParser, F, Op>> {
        PrattCons {
            tail: self,
            head: PostfixOp::new(op_parser, l_bp, f),
        }
    }
}

impl<I: Input, O> PrattOps<I, O> for PrattNil {
    fn parse_prefix<P: PrattParser<I, O>>(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
    ) -> Result<Option<(O, Loc)>, Diagnostic> {
        Ok(None)
    }

    fn parse_infix<P: PrattParser<I, O>>(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
        _min_bp: u8,
        lhs: O,
        lhs_loc: Loc,
    ) -> Result<OpMatch<O>, Diagnostic> {
        Ok(OpMatch::Unmatched(lhs, lhs_loc))
    }
}

/// A reverse cons list (snoc list) of operators, so we can build it easily.
#[derive(Debug, Clone, Copy)]
pub struct PrattCons<Tail, Head> {
    pub tail: Tail,
    pub head: Head,
}

impl<Tail, Head> PrattCons<Tail, Head> {
    pub const fn prefix<OpParser, F, Op>(
        self,
        op_parser: OpParser,
        r_bp: u8,
        f: F,
    ) -> PrattCons<Self, PrefixOp<OpParser, F, Op>> {
        PrattCons {
            tail: self,
            head: PrefixOp::new(op_parser, r_bp, f),
        }
    }

    pub const fn infix<OpParser, F, Op>(
        self,
        op_parser: OpParser,
        l_bp: u8,
        r_bp: u8,
        f: F,
    ) -> PrattCons<Self, InfixOp<OpParser, F, Op>> {
        PrattCons {
            tail: self,
            head: InfixOp::new(op_parser, l_bp, r_bp, f),
        }
    }

    pub const fn postfix<OpParser, F, Op>(
        self,
        op_parser: OpParser,
        l_bp: u8,
        f: F,
    ) -> PrattCons<Self, PostfixOp<OpParser, F, Op>> {
        PrattCons {
            tail: self,
            head: PostfixOp::new(op_parser, l_bp, f),
        }
    }
}

impl<I, O, Tail, Head> PrattOps<I, O> for PrattCons<Tail, Head>
where
    I: Input,
    Tail: PrattOps<I, O>,
    Head: PrattOps<I, O>,
{
    fn parse_prefix<P: PrattParser<I, O>>(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<(O, Loc)>, Diagnostic> {
        if let Some(res) = self.head.parse_prefix(pratt, input, report)? {
            return Ok(Some(res));
        }
        self.tail.parse_prefix(pratt, input, report)
    }

    fn parse_infix<P: PrattParser<I, O>>(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
        lhs_loc: Loc,
    ) -> Result<OpMatch<O>, Diagnostic> {
        match self
            .head
            .parse_infix(pratt, input, report, min_bp, lhs, lhs_loc)?
        {
            OpMatch::Matched(new_lhs, new_loc) => Ok(OpMatch::Matched(new_lhs, new_loc)),
            OpMatch::Unmatched(original_lhs, original_loc) => {
                self.tail
                    .parse_infix(pratt, input, report, min_bp, original_lhs, original_loc)
            }
        }
    }
}

/// A parser for a prefix operator in a Pratt parser.
#[derive(Debug, Clone, Copy)]
pub struct PrefixOp<OpParser, F, Op> {
    pub _marker: PhantomData<Op>,
    pub op_parser: OpParser,
    pub r_bp: u8,
    pub f: F,
}

impl<OpParser, F, Op> PrefixOp<OpParser, F, Op> {
    pub const fn new(op_parser: OpParser, r_bp: u8, f: F) -> Self {
        Self {
            _marker: PhantomData,
            op_parser,
            r_bp,
            f,
        }
    }
}

impl<I, O, Op, OpParser, F> PrattOps<I, O> for PrefixOp<OpParser, F, Op>
where
    I: Input,
    OpParser: Parser<I, Op>,
    F: Fn(Op, O, Loc, &mut I) -> O,
{
    fn parse_prefix<P: PrattParser<I, O>>(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<(O, Loc)>, Diagnostic> {
        let checkpoint = input.checkpoint();
        let start_loc = input.loc();
        match self.op_parser.parse(input, report) {
            Ok(op) => {
                let (rhs, rhs_loc) = pratt.parse_bp(input, report, self.r_bp)?;
                let full_loc = start_loc.union(rhs_loc);
                Ok(Some(((self.f)(op, rhs, full_loc, input), full_loc)))
            }
            Err(_) => {
                input.reset(checkpoint);
                Ok(None)
            }
        }
    }

    fn parse_infix<P: PrattParser<I, O>>(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
        _min_bp: u8,
        lhs: O,
        lhs_loc: Loc,
    ) -> Result<OpMatch<O>, Diagnostic> {
        Ok(OpMatch::Unmatched(lhs, lhs_loc))
    }
}

/// A parser for an infix operator in a Pratt parser.
#[derive(Debug, Clone, Copy)]
pub struct InfixOp<OpParser, F, Op> {
    pub _marker: PhantomData<Op>,
    pub op_parser: OpParser,
    pub l_bp: u8,
    pub r_bp: u8,
    pub f: F,
}

impl<OpParser, F, Op> InfixOp<OpParser, F, Op> {
    pub const fn new(op_parser: OpParser, l_bp: u8, r_bp: u8, f: F) -> Self {
        Self {
            _marker: PhantomData,
            op_parser,
            l_bp,
            r_bp,
            f,
        }
    }
}

impl<I, O, Op, OpParser, F> PrattOps<I, O> for InfixOp<OpParser, F, Op>
where
    I: Input,
    OpParser: Parser<I, Op>,
    F: Fn(O, Op, O, Loc, &mut I) -> O,
{
    fn parse_prefix<P: PrattParser<I, O>>(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
    ) -> Result<Option<(O, Loc)>, Diagnostic> {
        Ok(None)
    }

    fn parse_infix<P: PrattParser<I, O>>(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
        lhs_loc: Loc,
    ) -> Result<OpMatch<O>, Diagnostic> {
        if self.l_bp < min_bp {
            return Ok(OpMatch::Unmatched(lhs, lhs_loc));
        }

        let checkpoint = input.checkpoint();
        match self.op_parser.parse(input, report) {
            Ok(op) => {
                let (rhs, rhs_loc) = pratt.parse_bp(input, report, self.r_bp)?;
                let full_loc = lhs_loc.union(rhs_loc);
                Ok(OpMatch::Matched(
                    (self.f)(lhs, op, rhs, full_loc, input),
                    full_loc,
                ))
            }
            Err(_) => {
                input.reset(checkpoint);
                Ok(OpMatch::Unmatched(lhs, lhs_loc))
            }
        }
    }
}

/// A parser for a postfix operator in a Pratt parser.
#[derive(Debug, Clone, Copy)]
pub struct PostfixOp<OpParser, F, Op> {
    pub _marker: PhantomData<Op>,
    pub op_parser: OpParser,
    pub l_bp: u8,
    pub f: F,
}

impl<OpParser, F, Op> PostfixOp<OpParser, F, Op> {
    pub const fn new(op_parser: OpParser, l_bp: u8, f: F) -> Self {
        Self {
            _marker: PhantomData,
            op_parser,
            l_bp,
            f,
        }
    }
}

impl<I, O, Op, OpParser, F> PrattOps<I, O> for PostfixOp<OpParser, F, Op>
where
    I: Input,
    OpParser: Parser<I, Op>,
    F: Fn(O, Op, Loc, &mut I) -> O,
{
    fn parse_prefix<P: PrattParser<I, O>>(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
    ) -> Result<Option<(O, Loc)>, Diagnostic> {
        Ok(None)
    }

    fn parse_infix<P: PrattParser<I, O>>(
        &self,
        _pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
        lhs_loc: Loc,
    ) -> Result<OpMatch<O>, Diagnostic> {
        if self.l_bp < min_bp {
            return Ok(OpMatch::Unmatched(lhs, lhs_loc));
        }

        let checkpoint = input.checkpoint();
        match self.op_parser.parse(input, report) {
            Ok(op) => {
                let end_loc = input.prev_loc();
                let full_loc = lhs_loc.union(end_loc);
                Ok(OpMatch::Matched(
                    (self.f)(lhs, op, full_loc, input),
                    full_loc,
                ))
            }
            Err(_) => {
                input.reset(checkpoint);
                Ok(OpMatch::Unmatched(lhs, lhs_loc))
            }
        }
    }
}

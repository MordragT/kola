use std::marker::PhantomData;

use crate::{Diagnostic, Report, input::Input, parser::Parser};

/// Result of attempting to match an operator in the Pratt parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpMatch<O> {
    /// The operator matched and a new left-hand side was produced.
    Matched(O),
    /// The operator did not match (either precedence was too low or parser failed),
    /// returning the original left-hand side unchanged.
    Unmatched(O),
}

/// Helper trait to expose `parse_bp` generically to the operator implementations.
pub trait PrattParser<I: Input, O>: Parser<I, O> {
    fn parse_bp(&self, input: &mut I, report: &mut Report, min_bp: u8) -> Result<O, Diagnostic>;
}

/// Trait representing a collection of operators (prefix, infix, postfix)
/// checked sequentially during the Pratt loop at runtime.
pub trait PrattOps<I: Input, O, P> {
    /// Attempt to parse a prefix operator and return the parsed value.
    fn parse_prefix(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic>;

    /// Attempt to parse an infix or postfix operator.
    fn parse_infix(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
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
    Ops: PrattOps<I, O, Self>,
{
    fn parse_bp(&self, input: &mut I, report: &mut Report, min_bp: u8) -> Result<O, Diagnostic> {
        // 1. Parse prefix or atom
        let mut lhs = if let Some(prefix_expr) = self.ops.parse_prefix(self, input, report)? {
            prefix_expr
        } else {
            self.atom.parse(input, report)?
        };

        // 2. Loop for infix / postfix operators
        loop {
            match self.ops.parse_infix(self, input, report, min_bp, lhs)? {
                OpMatch::Matched(new_lhs) => {
                    lhs = new_lhs;
                }
                OpMatch::Unmatched(original_lhs) => {
                    return Ok(original_lhs);
                }
            }
        }
    }
}

impl<I, O, Atom, Ops> Parser<I, O> for Pratt<Atom, Ops, O>
where
    I: Input,
    Atom: Parser<I, O>,
    Ops: PrattOps<I, O, Self>,
{
    fn parse(&self, input: &mut I, report: &mut Report) -> Result<O, Diagnostic> {
        self.parse_bp(input, report, 0)
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

impl<I: Input, O, P> PrattOps<I, O, P> for PrattNil {
    fn parse_prefix(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        Ok(None)
    }

    fn parse_infix(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
        _min_bp: u8,
        lhs: O,
    ) -> Result<OpMatch<O>, Diagnostic> {
        Ok(OpMatch::Unmatched(lhs))
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

impl<I, O, P, Tail, Head> PrattOps<I, O, P> for PrattCons<Tail, Head>
where
    I: Input,
    Tail: PrattOps<I, O, P>,
    Head: PrattOps<I, O, P>,
{
    fn parse_prefix(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        if let Some(res) = self.head.parse_prefix(pratt, input, report)? {
            return Ok(Some(res));
        }
        self.tail.parse_prefix(pratt, input, report)
    }

    fn parse_infix(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
    ) -> Result<OpMatch<O>, Diagnostic> {
        match self.head.parse_infix(pratt, input, report, min_bp, lhs)? {
            OpMatch::Matched(new_lhs) => Ok(OpMatch::Matched(new_lhs)),
            OpMatch::Unmatched(original_lhs) => {
                self.tail
                    .parse_infix(pratt, input, report, min_bp, original_lhs)
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

impl<I, O, P, Op, OpParser, F> PrattOps<I, O, P> for PrefixOp<OpParser, F, Op>
where
    I: Input,
    OpParser: Parser<I, Op>,
    P: PrattParser<I, O>,
    F: Fn(Op, O) -> O,
{
    fn parse_prefix(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        let checkpoint = input.checkpoint();
        match self.op_parser.parse(input, report) {
            Ok(op) => {
                let rhs = pratt.parse_bp(input, report, self.r_bp)?;
                Ok(Some((self.f)(op, rhs)))
            }
            Err(_) => {
                input.reset(checkpoint);
                Ok(None)
            }
        }
    }

    fn parse_infix(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
        _min_bp: u8,
        lhs: O,
    ) -> Result<OpMatch<O>, Diagnostic> {
        Ok(OpMatch::Unmatched(lhs))
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

impl<I, O, P, Op, OpParser, F> PrattOps<I, O, P> for InfixOp<OpParser, F, Op>
where
    I: Input,
    OpParser: Parser<I, Op>,
    P: PrattParser<I, O>,
    F: Fn(O, Op, O) -> O,
{
    fn parse_prefix(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        Ok(None)
    }

    fn parse_infix(
        &self,
        pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
    ) -> Result<OpMatch<O>, Diagnostic> {
        if self.l_bp < min_bp {
            return Ok(OpMatch::Unmatched(lhs));
        }

        let checkpoint = input.checkpoint();
        match self.op_parser.parse(input, report) {
            Ok(op) => {
                let rhs = pratt.parse_bp(input, report, self.r_bp)?;
                Ok(OpMatch::Matched((self.f)(lhs, op, rhs)))
            }
            Err(_) => {
                input.reset(checkpoint);
                Ok(OpMatch::Unmatched(lhs))
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

impl<I, O, P, Op, OpParser, F> PrattOps<I, O, P> for PostfixOp<OpParser, F, Op>
where
    I: Input,
    OpParser: Parser<I, Op>,
    P: PrattParser<I, O>,
    F: Fn(O, Op) -> O,
{
    fn parse_prefix(
        &self,
        _pratt: &P,
        _input: &mut I,
        _report: &mut Report,
    ) -> Result<Option<O>, Diagnostic> {
        Ok(None)
    }

    fn parse_infix(
        &self,
        _pratt: &P,
        input: &mut I,
        report: &mut Report,
        min_bp: u8,
        lhs: O,
    ) -> Result<OpMatch<O>, Diagnostic> {
        if self.l_bp < min_bp {
            return Ok(OpMatch::Unmatched(lhs));
        }

        let checkpoint = input.checkpoint();
        match self.op_parser.parse(input, report) {
            Ok(op) => Ok(OpMatch::Matched((self.f)(lhs, op))),
            Err(_) => {
                input.reset(checkpoint);
                Ok(OpMatch::Unmatched(lhs))
            }
        }
    }
}

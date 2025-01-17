use std::fmt;

use bumpalo::{collections::Vec, Bump};
use owo_colors::OwoColorize;

use super::notation::{Notation, NotationRepr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrintOptions {
    pub indentation: &'static str,
    /// Maximum line width that we'll try to stay within
    pub width: u32,
}

impl Default for PrintOptions {
    fn default() -> Self {
        Self {
            indentation: "  ",
            width: 100,
        }
    }
}

impl PrintOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_indentation(mut self, indentation: &'static str) -> Self {
        self.indentation = indentation;
        self
    }

    pub fn with_width(mut self, width: u32) -> Self {
        self.width = width;
        self
    }
}

#[derive(Clone, Copy)]
struct Chunk<'a> {
    notation: &'a Notation<'a>,
    indent: u32,
    flat: bool,
}

impl<'a> Chunk<'a> {
    fn new(notation: &'a Notation<'a>) -> Self {
        Self {
            notation,
            indent: 0,
            flat: false,
        }
    }

    fn with_notation(mut self, n: &'a Notation<'a>) -> Self {
        self.notation = n;
        self
    }

    fn flatten(mut self) -> Self {
        self.flat = true;
        self
    }

    fn indent(mut self) -> Self {
        self.indent += 1;
        self
    }
    fn dedent(mut self) -> Self {
        self.indent -= 1;
        self
    }
}

pub struct Printer<'a> {
    options: PrintOptions,
    /// Current column position
    pos: u32,
    /// A stack of chunks to print. The _top_ of the stack is the
    /// _end_ of the vector, which represents the _earliest_ part
    /// of the document to print.
    chunks: Vec<'a, Chunk<'a>>,
}

impl<'a> Printer<'a> {
    pub fn new(notation: &'a Notation<'a>, options: PrintOptions, arena: &'a Bump) -> Self {
        let chunk = Chunk::new(notation);
        let mut chunks = Vec::new_in(arena);
        chunks.push(chunk);

        Self {
            options,
            pos: 0,
            chunks,
        }
    }

    pub fn print(&mut self, output: &mut impl fmt::Write, arena: &'a Bump) -> fmt::Result {
        use NotationRepr::*;

        while let Some(chunk) = self.chunks.pop() {
            match chunk.notation.inner.as_ref() {
                Empty => (),
                Linebreak => {
                    output.write_char('\n')?;
                    for _ in 0..chunk.indent {
                        output.write_str(&self.options.indentation)?;
                    }
                    self.pos = chunk.indent;
                }
                Text(t) => {
                    output.write_fmt(format_args!("{}", t.inner.style(t.style)))?;
                    self.pos += t.width;
                }
                Flat(n) => self.chunks.push(chunk.with_notation(n).flatten()),
                Indent(n) => self.chunks.push(chunk.with_notation(n).indent()),
                Dedent(n) => self.chunks.push(chunk.with_notation(n).dedent()),
                // reverse so that items are in correct order
                Join(ns) => self
                    .chunks
                    .extend(ns.into_iter().rev().map(|n| chunk.with_notation(n))),
                Group(a, b) => {
                    // Push b first so it is on top of the stack
                    self.chunks.push(chunk.with_notation(b));
                    self.chunks.push(chunk.with_notation(a));
                }
                Choice(a, b) => {
                    if chunk.flat || self.fits(chunk.with_notation(a), arena) {
                        self.chunks.push(chunk.with_notation(a));
                    } else {
                        self.chunks.push(chunk.with_notation(b))
                    }
                }
            }
        }

        Ok(())
    }

    fn fits(&self, chunk: Chunk<'a>, arena: &'a Bump) -> bool {
        use NotationRepr::*;

        let mut stack = bumpalo::vec![in arena; chunk];
        let mut spilled = &self.chunks as &[_];

        let mut rem = match self.options.width.checked_sub(self.pos) {
            Some(r) => r,
            None => return false,
        };

        while let Some(chunk) = stack.pop().or_else(|| {
            spilled.split_last().map(|(chunk, chunks)| {
                spilled = chunks;
                *chunk
            })
        }) {
            match chunk.notation.inner.as_ref() {
                Empty => (),
                Linebreak => return true,
                Text(t) => match rem.checked_sub(t.width) {
                    Some(r) => rem = r,
                    None => return false,
                },
                Flat(n) => stack.push(chunk.with_notation(n).flatten()),
                Indent(n) => stack.push(chunk.with_notation(n).indent()),
                Dedent(n) => stack.push(chunk.with_notation(n).dedent()),
                Join(ns) => stack.extend(ns.into_iter().map(|n| chunk.with_notation(n))),
                Group(a, b) => {
                    stack.push(chunk.with_notation(a));
                    stack.push(chunk.with_notation(b));
                }
                Choice(a, b) => {
                    if chunk.flat {
                        stack.push(chunk.with_notation(a))
                    } else {
                        stack.push(chunk.with_notation(b))
                    }
                }
            }
        }

        true
    }
}

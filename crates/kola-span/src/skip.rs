use std::marker::PhantomData;

use crate::{Loc, input::Input};

pub trait Skip<I: Input>: Sized {
    fn skip(&self, input: &mut I) -> Loc;
}

pub struct SkipUntil<I, F> {
    _marker: PhantomData<I>,
    predicate: F,
}

impl<I, F> Clone for SkipUntil<I, F>
where
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            predicate: self.predicate.clone(),
        }
    }
}

impl<I, F> Copy for SkipUntil<I, F> where F: Copy {}

/// Skip tokens until `predicate` returns true (predicate matches the upcoming token).
/// The matching token is NOT consumed. Returns the `Loc` spanning the skipped region.
pub const fn skip_until<I, F>(predicate: F) -> SkipUntil<I, F>
where
    I: Input,
    F: Fn(&I::Token) -> bool,
{
    SkipUntil {
        _marker: PhantomData,
        predicate,
    }
}

impl<I, F> Skip<I> for SkipUntil<I, F>
where
    I: Input,
    F: Fn(&I::Token) -> bool,
{
    fn skip(&self, input: &mut I) -> Loc {
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
        start.union(input.prev_loc())
    }
}

pub struct SkipDelimiters<I, const N: usize>
where
    I: Input,
{
    /// Index into `pairs` designating the primary target pair.
    /// Must be < N for the target to be valid. If out of range, parsing will
    /// gracefully recover by scanning until EOF (no special target).
    target_index: usize,
    pairs: [(I::Token, I::Token); N],
}

impl<I, const N: usize> Clone for SkipDelimiters<I, N>
where
    I: Input,
    I::Token: Clone,
{
    fn clone(&self) -> Self {
        Self {
            target_index: self.target_index,
            pairs: self.pairs.clone(),
        }
    }
}

impl<I, const N: usize> Copy for SkipDelimiters<I, N>
where
    I: Input,
    I::Token: Copy,
{
}

/// Convenience constructor: pass a target index (must be < N) and an array of pairs to track.
pub const fn skip_delimiters<I, const N: usize>(
    target_index: usize,
    pairs: [(I::Token, I::Token); N],
) -> SkipDelimiters<I, N>
where
    I: Input,
    I::Token: Copy + PartialEq,
{
    // Assert at construction time that the target index is valid.
    // In const contexts this becomes a compile-time error if misused.
    assert!(target_index < N);
    SkipDelimiters {
        target_index,
        pairs,
    }
}

impl<I, const N: usize> Skip<I> for SkipDelimiters<I, N>
where
    I: Input,
    I::Token: Copy + PartialEq,
{
    fn skip(&self, input: &mut I) -> Loc {
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

        start.union(input.prev_loc())
    }
}

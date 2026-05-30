use std::borrow::Cow;

use kola_ir::{
    id::Id,
    instr::{Func, HandlerClause},
    ir::{Ir, IrView},
};
use kola_utils::interner::StrKey;

use crate::{arenas::RangeIdx, heap::Heap};

/// The return clause handles the normal result when no effects are performed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnClause {
    /// Built-in identity function (x -> x)
    Identity,
    /// User-defined function from IR
    Function(Func),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapOpClauses(pub RangeIdx<(StrKey, Func)>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawOpClauses<'a>(pub Cow<'a, [(StrKey, Func)]>);

impl<'a> RawOpClauses<'a> {
    pub fn from_clauses(head: Id<HandlerClause>, ir: &Ir) -> Self {
        let mut clauses = Vec::new();

        // Walk the linked list of clauses
        for HandlerClause {
            op, param, body, ..
        } in ir.iter_clauses(Some(head))
        {
            // Create a function for this handler clause
            let handler_func = Func {
                param, // The operation argument parameter
                body,  // The handler body
            };

            // Add the operation clause to the handler
            clauses.push((op, handler_func));
        }

        Self(Cow::Owned(clauses))
    }

    pub fn find(&self, op: StrKey) -> Option<Func> {
        self.0
            .iter()
            .find(|(name, _)| *name == op)
            .map(|(_, handler)| *handler)
    }
}

/// Contains the behavior of the handler, which is a set of clauses.
///
/// ```ignore
/// handle
///  some_computation()
/// with
///   return x -> x + 1       // This is the return clause
///   operation1 x k -> ...   // This is an operation clause
///   operation2 x k -> ...   // Another operation clause
/// ```s
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapHandler {
    pub return_clause: ReturnClause,
    pub op_clauses: Option<HeapOpClauses>,
}

impl HeapHandler {
    #[inline]
    pub fn identity() -> Self {
        Self {
            return_clause: ReturnClause::Identity,
            op_clauses: None,
        }
    }

    #[inline]
    pub fn get(self, heap: &Heap) -> RawHandler<'_> {
        if let Some(op_clauses) = self.op_clauses {
            let clauses = heap.get_op_clauses(op_clauses);
            RawHandler {
                return_clause: self.return_clause,
                op_clauses: Some(clauses),
            }
        } else {
            RawHandler {
                return_clause: self.return_clause,
                op_clauses: None,
            }
        }
    }
}

/// Contains the behavior of the handler, which is a set of clauses.
///
/// ```ignore
/// handle
///  some_computation()
/// with
///   return x -> x + 1       // This is the return clause
///   operation1 x k -> ...   // This is an operation clause
///   operation2 x k -> ...   // Another operation clause
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct RawHandler<'a> {
    pub return_clause: ReturnClause,
    pub op_clauses: Option<RawOpClauses<'a>>,
}

impl<'a> RawHandler<'a> {
    #[inline]
    pub fn identity() -> Self {
        Self {
            return_clause: ReturnClause::Identity,
            op_clauses: None,
        }
    }

    #[inline]
    pub fn function(return_clause: Func) -> Self {
        Self {
            return_clause: ReturnClause::Function(return_clause),
            op_clauses: None,
        }
    }

    /// Create a handler from IR handler clauses
    #[inline]
    pub fn from_clauses(head: Id<HandlerClause>, ir: &Ir) -> Self {
        Self {
            // Currently handler clauses cannot create a user defined return clause
            return_clause: ReturnClause::Identity,
            op_clauses: Some(RawOpClauses::from_clauses(head, ir)),
        }
    }

    #[inline]
    pub fn alloc(&self, heap: &mut Heap) -> HeapHandler {
        if let Some(op_clauses) = &self.op_clauses {
            let clauses = heap.alloc_op_clauses(op_clauses);
            HeapHandler {
                return_clause: self.return_clause,
                op_clauses: Some(clauses),
            }
        } else {
            HeapHandler {
                return_clause: self.return_clause,
                op_clauses: None,
            }
        }
    }

    #[inline]
    pub fn find_operation(&self, op: StrKey) -> Option<Func> {
        self.op_clauses
            .as_ref()
            .and_then(|clauses| clauses.find(op))
    }
}

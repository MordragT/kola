use kola_ir::{
    id::Id,
    instr::{Func, HandlerClause},
    ir::{Ir, IrView},
};
use kola_utils::interner::StrKey;

/// The return clause handles the normal result when no effects are performed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnClause {
    /// Built-in identity function (x -> x)
    Identity,
    /// User-defined function from IR
    Function(Func),
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
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Handler {
    pub return_clause: ReturnClause,
    pub op_clauses: Option<Id<HandlerClause>>,
}

impl Handler {
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
    pub fn from_clauses(op_clauses: Id<HandlerClause>) -> Self {
        Self {
            // Currently handler clauses cannot create a user defined return clause
            return_clause: ReturnClause::Identity,
            op_clauses: Some(op_clauses),
        }
    }

    #[inline]
    pub fn find(&self, operation: StrKey, ir: &Ir) -> Option<Func> {
        let Some(head) = self.op_clauses else {
            return None; // No operation clauses
        };

        // Walk the linked list of clauses
        for HandlerClause {
            op, param, body, ..
        } in ir.iter_clauses(Some(head))
        {
            if op == operation {
                return Some(Func { param, body });
            }
        }

        None
    }
}

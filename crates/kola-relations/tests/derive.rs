use kola_relations::*;
use kola_relations_macro::{RelationStorage, TryProject};
use std::collections::HashMap;

// --- Mock AST types for tests ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MockLiteral {
    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MockCall {
    pub func: Id<MockLiteral>,
    pub arg: Id<MockLiteral>,
}

#[derive(Debug, Clone, PartialEq, Eq, TryProject)]
pub enum MockExpr {
    Lit(Id<MockLiteral>),
    Call(Id<MockCall>),
}

// --- Mock Storage and DB contexts ---

#[derive(RelationStorage)]
pub struct MockStorage {
    #[relation]
    pub literals: Vec<MockLiteral>,
    #[relation]
    pub calls: Vec<MockCall>,
    #[relation]
    pub exprs: Vec<MockExpr>,
}

impl MockStorage {
    pub fn new() -> Self {
        Self {
            literals: Vec::new(),
            calls: Vec::new(),
            exprs: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Unit Tests ---

    #[test]
    fn test_basic_scan_and_filter() {
        let mut store = MockStorage::new();
        // 100% clean, annotation-free, auto-inferred allocation!
        let lit1 = store.alloc(MockLiteral { value: 42 });
        let _lit2 = store.alloc(MockLiteral { value: 100 });

        // query::<T>() uses the beautiful single type parameter syntax!
        // No closure annotations are needed thanks to the direct method where clause!
        let results: Vec<_> = query::<MockLiteral>()
            .filter(|lit| lit.value > 50)
            .execute(&store)
            .collect();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].1.value, 100);
        assert_ne!(results[0].0, lit1);
    }

    #[test]
    fn test_joins() {
        let mut store = MockStorage::new();
        // 100% clean, annotation-free, auto-inferred allocation!
        let lit1 = store.alloc(MockLiteral { value: 10 });
        let lit2 = store.alloc(MockLiteral { value: 20 });

        // Zero boiler-plate HashMap as a side-table out of the box!
        let mut types = HashMap::new();
        types.insert(lit1, "Int".to_string());

        let db = (&store, &types);

        // 1. Inner Join (.with)
        let with_results: Vec<_> = query::<MockLiteral>()
            .with::<String>()
            .execute(&db)
            .collect();

        assert_eq!(with_results.len(), 1);
        assert_eq!(with_results[0].0, lit1);
        assert_eq!(with_results[0].1.1, "Int");

        // 2. Left Join (.left_join)
        let left_results: Vec<_> = query::<MockLiteral>()
            .left_join::<String>()
            .execute(&db)
            .collect();

        assert_eq!(left_results.len(), 2);
        assert_eq!(left_results[0].0, lit1);
        assert_eq!(left_results[0].1.1, Some(&"Int".to_string()));
        assert_eq!(left_results[1].0, lit2);
        assert_eq!(left_results[1].1.1, None);

        // 3. Anti-Join (.without)
        let without_results: Vec<_> = query::<MockLiteral>()
            .without::<String>()
            .execute(&db)
            .collect();

        assert_eq!(without_results.len(), 1);
        assert_eq!(without_results[0].0, lit2);
    }

    #[test]
    fn test_cast_and_projections_via_macros() {
        let mut store = MockStorage::new();
        // 100% clean, annotation-free, auto-inferred allocation!
        let lit1 = store.alloc(MockLiteral { value: 5 });
        let lit2 = store.alloc(MockLiteral { value: 7 });
        let call = store.alloc(MockCall {
            func: lit1,
            arg: lit2,
        });
        let _expr = store.alloc(MockExpr::Call(call));

        // Let's query MockExpr, cast it to MockCall, project its arg, and verify value
        let results: Vec<_> = query::<MockExpr>()
            .cast::<MockCall>()
            .project(|call| call.arg)
            .execute(&store)
            .collect();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, lit2);
        assert_eq!(results[0].1.value, 7);
    }

    #[test]
    fn test_mutating_updates() {
        let mut store = MockStorage::new();
        // 100% clean, annotation-free, auto-inferred allocation!
        let _lit1 = store.alloc(MockLiteral { value: 10 });
        let _lit2 = store.alloc(MockLiteral { value: 20 });

        // Update all values greater than 15 by adding 5
        query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .update(&mut store, |lit| {
                lit.value += 5;
            });

        assert_eq!(store.literals[0].value, 10);
        assert_eq!(store.literals[1].value, 25);
    }

    #[test]
    fn test_mutating_updates_direct() {
        let mut store = MockStorage::new();
        // 100% clean, annotation-free, auto-inferred allocation!
        let _lit1 = store.alloc(MockLiteral { value: 10 });
        let _lit2 = store.alloc(MockLiteral { value: 20 });

        // Mutates contiguous memory in a single pass with NO allocations and NO repeated index lookups
        query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .update_direct(&mut store, |lit| {
                lit.value += 10;
            });

        assert_eq!(store.literals[0].value, 10);
        assert_eq!(store.literals[1].value, 30);
    }

    #[test]
    fn test_overlay_context() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 1 });
        let lit2 = store.alloc(MockLiteral { value: 2 });

        let mut base_types = HashMap::new();
        base_types.insert(lit1, "Int".to_string());
        base_types.insert(lit2, "Float".to_string());

        let mut local_types = HashMap::new();
        local_types.insert(lit2, "Overridden".to_string());

        // Speculative overlay is now natively represented as a flat 3-tuple!
        // local_types takes priority and shadowed base_types perfectly.
        let db = (&store, &local_types, &base_types);

        let results: Vec<_> = query::<MockLiteral>()
            .with::<String>()
            .execute(&db)
            .collect();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].0, lit1);
        assert_eq!(results[0].1.1, "Int");
        assert_eq!(results[1].0, lit2);
        assert_eq!(results[1].1.1, "Overridden"); // shadowed successfully!
    }
}

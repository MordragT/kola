use kola_relations::*;
use kola_relations_macro::{RelationStorage, TryCast};
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

#[derive(Debug, Clone, PartialEq, Eq, TryCast)]
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
    pub slices: SliceStorage,
}

impl MockStorage {
    pub fn new() -> Self {
        Self {
            literals: Vec::new(),
            calls: Vec::new(),
            exprs: Vec::new(),
            slices: SliceStorage::new(),
        }
    }
}

impl SliceRelation<MockLiteral> for MockStorage {
    fn get_slice(&self, id: SliceId<MockLiteral>) -> &[Id<MockLiteral>] {
        self.slices.get(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_scan_and_filter() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 42 });
        let _lit2 = store.alloc(MockLiteral { value: 100 });

        let results: Vec<_> = query::<MockLiteral>()
            .filter(|lit| lit.value > 50)
            .iter_rows(&store, RelationIds::<MockLiteral>::ids(&store))
            .flatten()
            .collect();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].1.value, 100);
        assert_ne!(results[0].0, lit1);
    }

    #[test]
    fn test_joins() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 10 });
        let lit2 = store.alloc(MockLiteral { value: 20 });

        let mut types = HashMap::new();
        types.insert(lit1, "Int".to_string());

        let db = (&store, &types);

        let with_results: Vec<_> = query::<MockLiteral>()
            .with::<String>()
            .iter_rows(&db, RelationIds::<MockLiteral>::ids(&db))
            .flatten()
            .collect();

        assert_eq!(with_results.len(), 1);
        assert_eq!(with_results[0].0, lit1);
        assert_eq!(with_results[0].1.1, "Int");

        let left_results: Vec<_> = query::<MockLiteral>()
            .left_join::<String>()
            .iter_rows(&db, RelationIds::<MockLiteral>::ids(&db))
            .collect();

        assert_eq!(left_results.len(), 2);
        assert_eq!(left_results[0].0, lit1);
        assert_eq!(left_results[0].1.1, Some(&"Int".to_string()));
        assert_eq!(left_results[1].0, lit2);
        assert_eq!(left_results[1].1.1, None);

        let without_results: Vec<_> = query::<MockLiteral>()
            .without::<String>()
            .iter_rows(&db, RelationIds::<MockLiteral>::ids(&db))
            .flatten()
            .collect();

        assert_eq!(without_results.len(), 1);
        assert_eq!(without_results[0].0, lit2);
    }

    #[test]
    fn test_cast_and_projections_via_macros() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 5 });
        let lit2 = store.alloc(MockLiteral { value: 7 });
        let call = store.alloc(MockCall {
            func: lit1,
            arg: lit2,
        });
        let _expr = store.alloc(MockExpr::Call(call));

        let results: Vec<_> = query::<MockExpr>()
            .cast::<MockCall>()
            .project(|call| call.arg)
            .iter_rows(&store, RelationIds::<MockExpr>::ids(&store))
            .flatten()
            .collect();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, lit2);
        assert_eq!(results[0].1.value, 7);
    }

    #[test]
    fn test_mutating_updates() {
        let mut store = MockStorage::new();
        let _lit1 = store.alloc(MockLiteral { value: 10 });
        let _lit2 = store.alloc(MockLiteral { value: 20 });

        query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .update_all(&mut store, |lit| {
                lit.value += 5;
            });

        assert_eq!(store.literals[0].value, 10);
        assert_eq!(store.literals[1].value, 25);
    }

    #[test]
    fn test_filter_after_join() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 10 });
        let lit2 = store.alloc(MockLiteral { value: 20 });

        let mut types = HashMap::new();
        types.insert(lit1, "Int".to_string());
        types.insert(lit2, "Float".to_string());

        let db = (&store, &types);
        let results: Vec<_> = query::<MockLiteral>()
            .with::<String>()
            .filter(|(_, ty)| *ty == "Int")
            .iter_rows(&db, RelationIds::<MockLiteral>::ids(&db))
            .flatten()
            .collect();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, lit1);
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

        let db = (&store, &local_types, &base_types);

        let results: Vec<_> = query::<MockLiteral>()
            .with::<String>()
            .iter_rows(&db, RelationIds::<MockLiteral>::ids(&db))
            .flatten()
            .collect();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].0, lit1);
        assert_eq!(results[0].1.1, "Int");
        assert_eq!(results[1].0, lit2);
        assert_eq!(results[1].1.1, "Overridden");
    }

    #[test]
    fn test_iterators() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 10 });
        let lit2 = store.alloc(MockLiteral { value: 20 });

        let first = query::<MockLiteral>()
            .iter(&store, RelationIds::<MockLiteral>::ids(&store))
            .next();
        assert_eq!(first.unwrap().value, 10);

        let count = query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .iter(&store, RelationIds::<MockLiteral>::ids(&store))
            .flatten()
            .count();
        assert_eq!(count, 1);

        let one = query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .get(&store, lit2)
            .unwrap();
        assert_eq!(one.value, 20);

        let collected: Vec<&MockLiteral> =
            query::<MockLiteral>().iter(&store, [lit1, lit2]).collect();
        assert_eq!(collected.len(), 2);
        assert_ne!(lit1, lit2);
    }

    #[test]
    fn test_scratch_update() {
        let mut store = MockStorage::new();
        store.alloc(MockLiteral { value: 10 });
        store.alloc(MockLiteral { value: 20 });

        let mut ids = Vec::new();
        query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .select_ids(&store, RelationIds::<MockLiteral>::ids(&store), &mut ids);

        assert_eq!(ids.len(), 1);
        update_ids(&mut store, &ids, |lit| lit.value += 5);
        assert_eq!(store.literals[1].value, 25);
    }

    #[test]
    fn test_split_update() {
        let mut read = MockStorage::new();
        let lit = read.alloc(MockLiteral { value: 5 });
        let call = read.alloc(MockCall {
            func: lit,
            arg: lit,
        });
        read.alloc(MockExpr::Call(call));

        let mut write = MockStorage::new();
        write.alloc(MockLiteral { value: 0 });

        let updated = update_split(
            query::<MockExpr>()
                .cast::<MockCall>()
                .project(|call| call.arg),
            &read,
            &mut write,
            RelationIds::<MockExpr>::ids(&read),
            |lit| lit.value += 1,
        );

        assert_eq!(updated, 1);
        assert_eq!(write.literals[0].value, 1);
    }

    #[test]
    fn test_cast_is_prism() {
        let mut store = MockStorage::new();
        let lit = store.alloc(MockLiteral { value: 5 });
        let call = store.alloc(MockCall {
            func: lit,
            arg: lit,
        });
        store.alloc(MockExpr::Call(call));

        assert_eq!(
            query::<MockExpr>()
                .cast::<MockLiteral>()
                .iter(&store, RelationIds::<MockExpr>::ids(&store))
                .flatten()
                .count(),
            0
        );
    }

    #[test]
    #[should_panic(expected = "expected a")]
    fn test_expect_panics() {
        let mut store = MockStorage::new();
        let lit = store.alloc(MockLiteral { value: 5 });
        let call = store.alloc(MockCall {
            func: lit,
            arg: lit,
        });
        store.alloc(MockExpr::Call(call));

        query::<MockExpr>()
            .cast::<MockLiteral>()
            .expect()
            .iter(&store, RelationIds::<MockExpr>::ids(&store))
            .next();
    }

    #[test]
    fn test_slice_update_with_scratch() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 10 });
        let lit2 = store.alloc(MockLiteral { value: 20 });
        let slice = store.slices.alloc([lit1, lit2]);

        let mut scratch = Vec::new();
        let updated = query::<MockLiteral>()
            .filter(|lit| lit.value > 15)
            .update_with_scratch(&mut store, slice, &mut scratch, |lit| lit.value += 1);

        assert_eq!(updated, 1);
        assert_eq!(store.literals[1].value, 21);
    }

    #[test]
    fn test_inspect_get_id_and_slice_seeds() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 10 });
        let lit2 = store.alloc(MockLiteral { value: 20 });
        let slice = store.slices.alloc([lit1, lit2]);

        let seen = std::cell::Cell::new(0);
        let ids: Vec<_> = query::<MockLiteral>()
            .inspect(|_| seen.set(seen.get() + 1))
            .iter_from_slice(&store, slice)
            .map(|lit| lit.value)
            .collect();

        assert_eq!(ids, vec![10, 20]);
        assert_eq!(seen.get(), 2);

        let ids_from_slice: Vec<_> = query::<MockLiteral>()
            .iter_ids_from_slice(&store, slice)
            .collect();
        assert_eq!(ids_from_slice, vec![lit1, lit2]);

        let rows_from_slice: Vec<_> = query::<MockLiteral>()
            .iter_rows_from_slice(&store, slice)
            .collect();
        assert_eq!(rows_from_slice.len(), 2);
        assert_eq!(rows_from_slice[1].0, lit2);

        let ids: Vec<_> = query::<MockLiteral>()
            .iter_ids(&store, [lit1, lit2])
            .collect();
        assert_eq!(ids, vec![lit1, lit2]);

        assert_eq!(lit1.query().get_id(&store), lit1);
        assert_eq!(lit1.query().get(&store).value, 10);
        assert_eq!(
            lit1.query().filter(|lit| lit.value > 15).get_id(&store),
            None
        );

        let (left, right) = lit1
            .query()
            .branch(&store, |id, _| (id.query(), id.query()));
        assert_eq!(left.value, 10);
        assert_eq!(right.value, 10);

        let absent = lit1
            .query()
            .filter(|lit| lit.value > 15)
            .branch(&store, |id, _| id.query());
        assert!(absent.is_none());

        let branch_rows =
            query::<MockLiteral>().branch_slice(&store, slice, |id, _| (id.query(), id.query()));
        assert_eq!(branch_rows.len(), 2);
        assert_eq!(branch_rows[0].0.value, 10);
        assert_eq!(branch_rows[1].0.value, 20);
    }

    #[test]
    fn test_presence_return_types() {
        let mut store = MockStorage::new();
        let lit = store.alloc(MockLiteral { value: 10 });

        let total: &MockLiteral = query::<MockLiteral>().get(&store, lit);
        assert_eq!(total.value, 10);

        let optional: Option<&MockLiteral> =
            query::<MockLiteral>().filter(|_| true).get(&store, lit);
        assert_eq!(optional.unwrap().value, 10);

        let slice = store.slices.alloc([lit]);
        let id = store.id_at(slice, 0).unwrap();
        let from_slice: &MockLiteral = query::<MockLiteral>().get(&store, id);
        assert_eq!(from_slice.value, 10);

        let many: Vec<&MockLiteral> = query::<MockLiteral>()
            .iter(&store, store.get_slice(slice).iter().copied())
            .collect();
        assert_eq!(many.len(), 1);
    }
}

use kola_relations::*;
use kola_relations_macro::{RelationProjections, TryProject};
use std::collections::HashMap;

// --- Mock AST types for tests ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MockLiteral {
    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, RelationProjections)]
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

pub struct MockStorage {
    pub literals: Vec<MockLiteral>,
    pub calls: Vec<MockCall>,
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

// RelationIds impls
impl RelationIds<MockLiteral> for MockStorage {
    type Ids<'a> = IdIter<MockLiteral>;
    fn ids(&self) -> Self::Ids<'_> {
        IdIter::new(0, self.literals.len() as u32)
    }
}

impl RelationIds<MockCall> for MockStorage {
    type Ids<'a> = IdIter<MockCall>;
    fn ids(&self) -> Self::Ids<'_> {
        IdIter::new(0, self.calls.len() as u32)
    }
}

impl RelationIds<MockExpr> for MockStorage {
    type Ids<'a> = IdIter<MockExpr>;
    fn ids(&self) -> Self::Ids<'_> {
        IdIter::new(0, self.exprs.len() as u32)
    }
}

// PartialRelation & TotalRelation for MockLiteral
impl PartialRelation<MockLiteral, MockLiteral> for MockStorage {
    fn get_opt(&self, id: Id<MockLiteral>) -> Option<&MockLiteral> {
        self.literals.get(id.as_usize())
    }
}
impl TotalRelation<MockLiteral, MockLiteral> for MockStorage {
    fn get(&self, id: Id<MockLiteral>) -> &MockLiteral {
        &self.literals[id.as_usize()]
    }
    type Rows<'a> = std::iter::Zip<IdIter<MockLiteral>, std::slice::Iter<'a, MockLiteral>>;
    fn rows(&self) -> Self::Rows<'_> {
        self.ids().zip(self.literals.iter())
    }
}

// PartialRelation & TotalRelation for MockCall
impl PartialRelation<MockCall, MockCall> for MockStorage {
    fn get_opt(&self, id: Id<MockCall>) -> Option<&MockCall> {
        self.calls.get(id.as_usize())
    }
}
impl TotalRelation<MockCall, MockCall> for MockStorage {
    fn get(&self, id: Id<MockCall>) -> &MockCall {
        &self.calls[id.as_usize()]
    }
    type Rows<'a> = std::iter::Zip<IdIter<MockCall>, std::slice::Iter<'a, MockCall>>;
    fn rows(&self) -> Self::Rows<'_> {
        self.ids().zip(self.calls.iter())
    }
}

// PartialRelation & TotalRelation for MockExpr
impl PartialRelation<MockExpr, MockExpr> for MockStorage {
    fn get_opt(&self, id: Id<MockExpr>) -> Option<&MockExpr> {
        self.exprs.get(id.as_usize())
    }
}
impl TotalRelation<MockExpr, MockExpr> for MockStorage {
    fn get(&self, id: Id<MockExpr>) -> &MockExpr {
        &self.exprs[id.as_usize()]
    }
    type Rows<'a> = std::iter::Zip<IdIter<MockExpr>, std::slice::Iter<'a, MockExpr>>;
    fn rows(&self) -> Self::Rows<'_> {
        self.ids().zip(self.exprs.iter())
    }
}

// TotalRelationMut for MockLiteral (for updates)
impl TotalRelationMut<MockLiteral, MockLiteral> for MockStorage {
    fn get_mut(&mut self, id: Id<MockLiteral>) -> &mut MockLiteral {
        &mut self.literals[id.as_usize()]
    }
    type RowsMut<'a> = std::iter::Zip<IdIter<MockLiteral>, std::slice::IterMut<'a, MockLiteral>>;
    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
        self.ids().zip(self.literals.iter_mut())
    }
}

// Allocate impls
impl Allocate<MockLiteral> for MockStorage {
    type Value = MockLiteral;
    fn alloc(&mut self, value: Self::Value) -> Id<MockLiteral> {
        let id = Id::new(self.literals.len() as u32);
        self.literals.push(value);
        id
    }
}
impl Allocate<MockCall> for MockStorage {
    type Value = MockCall;
    fn alloc(&mut self, value: Self::Value) -> Id<MockCall> {
        let id = Id::new(self.calls.len() as u32);
        self.calls.push(value);
        id
    }
}
impl Allocate<MockExpr> for MockStorage {
    type Value = MockExpr;
    fn alloc(&mut self, value: Self::Value) -> Id<MockExpr> {
        let id = Id::new(self.exprs.len() as u32);
        self.exprs.push(value);
        id
    }
}

// --- Side Table (Mock Type Map) ---

pub struct MockTypeMap(HashMap<Id<MockLiteral>, String>);

impl RelationIds<MockLiteral> for MockTypeMap {
    type Ids<'a> = IdIter<MockLiteral>;
    fn ids(&self) -> Self::Ids<'_> {
        IdIter::new(0, 0) // not scanning keys through side table
    }
}

impl PartialRelation<MockLiteral, String> for MockTypeMap {
    fn get_opt(&self, id: Id<MockLiteral>) -> Option<&String> {
        self.0.get(&id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Unit Tests ---

    #[test]
    fn test_basic_scan_and_filter() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 42 });
        let _lit2 = store.alloc(MockLiteral { value: 100 });

        let results: Vec<_> = query::<MockLiteral, MockLiteral>()
            .filter(|(_, lit)| lit.value > 50)
            .execute(&store)
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

        let mut types = MockTypeMap(HashMap::new());
        types.0.insert(lit1, "Int".to_string());

        let db = (&store, &types);

        // 1. Inner Join (.with)
        let with_results: Vec<_> = query::<MockLiteral, MockLiteral>()
            .with::<String>()
            .execute(&db)
            .collect();

        assert_eq!(with_results.len(), 1);
        assert_eq!(with_results[0].0, lit1);
        assert_eq!(with_results[0].1.1, "Int");

        // 2. Left Join (.left_join)
        let left_results: Vec<_> = query::<MockLiteral, MockLiteral>()
            .left_join::<String>()
            .execute(&db)
            .collect();

        assert_eq!(left_results.len(), 2);
        assert_eq!(left_results[0].0, lit1);
        assert_eq!(left_results[0].1.1, Some(&"Int".to_string()));
        assert_eq!(left_results[1].0, lit2);
        assert_eq!(left_results[1].1.1, None);

        // 3. Anti-Join (.without)
        let without_results: Vec<_> = query::<MockLiteral, MockLiteral>()
            .without::<String>()
            .execute(&db)
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
        let expr = store.alloc(MockExpr::Call(call));

        // Let's query MockExpr, cast it to MockCall, project its arg, and verify value
        let results: Vec<_> = query::<MockExpr, MockExpr>()
            .cast::<MockCall>()
            .project_arg() // auto-derived by RelationProjections!
            .execute(&store)
            .collect();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, lit2);
        assert_eq!(results[0].1.value, 7);
    }

    #[test]
    fn test_mutating_updates() {
        let mut store = MockStorage::new();
        store.alloc(MockLiteral { value: 10 });
        store.alloc(MockLiteral { value: 20 });

        // Update all values greater than 15 by adding 5
        query::<MockLiteral, MockLiteral>()
            .filter(|(_, lit)| lit.value > 15)
            .update(&mut store, |lit| {
                lit.value += 5;
            });

        assert_eq!(store.literals[0].value, 10);
        assert_eq!(store.literals[1].value, 25);
    }

    #[test]
    fn test_overlay_context() {
        let mut store = MockStorage::new();
        let lit1 = store.alloc(MockLiteral { value: 1 });
        let lit2 = store.alloc(MockLiteral { value: 2 });

        let mut base_types = MockTypeMap(HashMap::new());
        base_types.0.insert(lit1, "Int".to_string());
        base_types.0.insert(lit2, "Float".to_string());

        let mut local_types = MockTypeMap(HashMap::new());
        local_types.0.insert(lit2, "Overridden".to_string());

        let db = Overlay::new((&store, &base_types), &local_types);

        let results: Vec<_> = query::<MockLiteral, MockLiteral>()
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

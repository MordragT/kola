# Architecture Specification: Pattern-Driven Structural Refinements and Row-Polymorphic Contracts

## 1. System Overview

This specification details the design for a total, strongly-typed configuration language based on **Hindley-Milner type inference**, **algebraic effects**, and **row polymorphism**. 

To avoid the overhead, non-deterministic performance, and complex C/C++ dependencies of heavy external SMT solvers (e.g., Z3), the language implements static contracts and path-sensitive narrowing by unifying **Pattern Types / CoverSets** with **Extensible Row Polymorphism**.

### Core Guarantees
* **Totality & Purity**: Expressions evaluated during type checking and contract validation are pure and guaranteed to terminate.
* **Deterministic Execution**: The AST is represented as contiguous flat arena buffers (`Vec<T>`, `Id<T>`), allowing ultra-fast structural normalization, hashing, and type traversal.
* **Zero SMT Reliance**: Soundness is maintained using structural set-algebraic inclusion (`CoverSet::is_superset`) for structural patterns, supplemented by nominal structural hashes for opaque/arbitrary runtime predicates.

---

## 2. Core Architecture & Flat AST Representation

The AST is organized around flat, arena-allocated vectors with typed 32-bit indices (`Id<T>`).

```rust
pub struct Id<T> {
    pub index: u32,
    _marker: PhantomData<T>,
}

pub struct AST {
    pub exprs: Vec<Expr>,
    pub cases: Vec<CaseExpr>,
    pub types: Vec<TypeExpr>,
    // ...
}
```

### Advantages for the Contract Engine
1. **Contiguous Traversal**: Bottom-up normalization and structural comparisons run linearly over dense vectors with minimal cache missing.
2. **Deterministic Canonical Indexing**: Free variables in guards or contract paths are converted to relative de Bruijn/canonical indices, allowing structurally identical pattern constraints across different lexical scopes to hash identically.

---

## 3. Structural Narrowing via CoverSet & Pattern Types

Instead of maintaining a separate contract DSL or constraint solver, **contracts are modeled directly as Pattern Types**. The language reuses its internal pattern-matching exhaustiveness machinery (`CoverSet`) to track static refinements.

### The Refinement Mechanics
When an `if let` pattern or path check occurs:
$$\text{if let } P = x \text{ then } E_1 \text{ else } E_2$$

1. **Extraction**: The pattern $P$ is converted into a `CoverSet` via `ActualSet::actual_set`.
2. **Local Environment Extension**: Within $E_1$, $x$ is re-bound in the local typing context ($\Gamma$) to carry the extracted `CoverSet` as a row refinement on its type.
3. **Subtyping via Set Inclusion**: A function expecting a refined parameter checks subtyping using structural set operations (`is_superset`), rather than calling an external solver engine.

---

## 4. Unifying CoverSet with Extensible Rows

Refinement constraints accumulate in extensible row variables attached to base types:

$$\tau ::= \text{Int} \ [\ C \ \mid \ r \ ] \ \mid \ \{ l : \tau \ \mid \ r \ \} \ \mid \ \dots$$

Where $C$ represents a set of accumulated `CoverSet` items and $r$ is a row variable allowing ad-hoc capability extension and unification.

### Unified CoverSet Topology

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CoverSet {
    /// Matches nothing at all
    Empty,
    /// Matches all possible values (wildcard patterns)
    Universal,
    /// Finite set of atomic scalar values (booleans, unit)
    Atom(AtomSet),
    /// Record types with labeled fields and sub-coverages
    Record(RowSet),
    /// Variant types with tagged constructors
    Variant(RowSet),
    /// List types with exact or bounded length constraints
    List(ListSet),
    /// Arbitrary or non-decomposable predicate hashes
    Opaque(ExprHash),
}
```

### Dual-Layer Solver Pipeline

| Check Type | Mechanism | Precision / Strategy |
| :--- | :--- | :--- |
| **Structural Shapes** (Records, Enums, Ranges, List Lengths) | `CoverSet::is_superset` | **Set Algebra**: Evaluates containment mathematically (e.g., $1..50 \subseteq 1..100$). |
| **Arbitrary Pure Predicates** (e.g., `is_valid_ip(x)`) | `CoverSet::Opaque(ExprHash)` | **Syntactic Equality**: Structural AST canonical normalization + Deterministic Hashing. Treated as uninterpreted nominal brand tokens. |

---

## 5. Local Path Projection & Record Row Refinement

Path projections allow nested struct fields to be narrowed locally without invalidating or black-boxing the parent record.

### Example

```lts
type Person = { name : Str, age : Int }

let peter = { name = "peter", age = 26 } in
if let { age = 20.. } = peter then
    // 'peter' is re-bound locally in this branch
    do_something peter
```

### Execution Flow in Type Checker

1. **AST Match**: `if let { age = 20.. } = peter` is parsed.
2. **CoverSet Synthesis**: The pattern `{ age = 20.. }` produces:
   $$\text{CoverSet::Record}(\text{RowSet::Extension}\{ \text{head}: \text{age} \mapsto \text{AtLeast}(20), \text{tail}: \text{Universal} \})$$
3. **Row Re-binding**: `peter` is updated in the lexical scope's type environment:
   $$\text{peter} : \{ \text{name} : \text{Str}, \text{age} : \text{Int} \ [\ \text{CoverSet::List}(\text{AtLeast}(20)) \ \mid \ c \ ] \ \mid \ r \}$$
4. **Row Polymorphic Unification**: Downstream calls accepting records with constrained `age` fields unify seamlessly via standard row extension rules.
5. **Runtime Erasure**: Refinement tags are erased during code generation, leaving zero execution runtime overhead.

---

## 6. Diagnostic Integration

Because `CoverSet::Opaque` and structural refinements store stable canonical `ExprHash`es or span IDs referencing nodes in the flat arena `Vec`, compiler diagnostics remain pristine:

* Internal type unification failures map the missing `CoverSet` hash back to its source `Span` in the arena.
* Error reporting formats user-facing messages directly from original source text without exposing raw internal hashes to the developer.
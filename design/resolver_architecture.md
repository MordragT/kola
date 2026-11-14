# Resolver Architecture (v1 module system)

**Status:** implemented through Phase C (composition); Phase D (orders, `Db`,
and member resolution) is next.
**Depends on:** `design/module_system_v1_spec.md` (normative semantics);
the AST/grammar increment has landed.

This document describes both the current resolver and the agreed target shape.
Where the implementation is still transitional, it says so explicitly.

## Current implementation status

**Done**

- Functors, `kola-subst` in the resolver, `Sym::level`/`LEVEL`, and the old
  `DefMap` substitution machinery are gone.
- One `Tree` per module; `SourceTrees` plus `module_of` translate
  `(SourceId, ModuleKey)` to `ModuleSym` once.
- `Loc`/`Key<T>` no longer implement `Default`; tree-fragment constructors take
  explicit locations and use `alloc_located`. Span-less allocation exists only
  for metadata types with a meaningful `Default` (such as `()`).
- Phase B is in place: public definitions and abstract obligations are recorded
  separately.
- Discovery is split into:
  - `discover/mod.rs` — orchestration, `Inputs`, `ModuleState`;
  - `discover/collect.rs` — top-level declaration scan;
  - `discover/bodies.rs` — type schemes, module types, expressions.
- Discovery owns its per-module output (`ModuleState`) and moves it between
  `Collector` and `BodyVisitor`; only genuinely global accumulators are borrowed.
- `env::Module` became `ModuleBuilder`, a transient discovery/build row.
- Unqualified lookup collapse:
  - unqualified names resolve from local scopes, the module's pre-collected
    `NameMap`, or builtins;
  - anything else is an immediate discovery diagnostic;
  - `ValueLookup`/`TypeLookup`/`TypeAnnotLookup`/`ModuleTypeLookup`/
    `ModuleTypeAnnotLookup` are deleted;
  - `Lookups` contains only module-path queries;
  - value/type/module-type lookups are now only topological-order builders
    (`order_values`, `order_types`, `order_module_types`).
- `elaborate/` is deleted. Supported module forms are imports, inline bodies,
  qualified module paths, and `with`/`over`/`using`. Path aliases
  (`module x = a::b`) and `super` are deferred.
- Composition is implemented in `compose.rs`:
  - discovery allocates a `ModuleSym` and a `ComposeJob` for every merge
    expression, including nested ones;
  - compose resolves operand paths, evaluates nested merges, and fills the
    composed module's `ModuleMembers`;
  - `with` conflicts on distinct origins, `over` prefers the left, and `using`
    supplies outstanding members from the right;
  - composed modules get populated `NameMap`s so path lookup can traverse them.
- Typed surfaces:
  - `Definition<S, B>`, `Obligation<D>`;
  - `Definitions<K, S, B>` and `Obligations<K, D>` (sorted tables);
  - `ModuleMembers` split into `ModuleDefinitions` and `ModuleObligations`;
  - no phantom `D = ()` for namespaces without obligations.
- Relational metadata:
  - `MaybeVec` (dense slots with occupied-only iteration) for partially
    populated metadata;
  - `#[derive(RelationStorage)]` supports `MaybeVec`;
  - `PartialRelationSet` for insertion by id;
  - `#[derive(Merge)]` for merging relation tables;
  - `Substitutable` impls for `SideMap`/`SideVec`/`MaybeVec` in
    `kola-relations`, and `#[derive(Substitutable)]` in `kola-subst-macro`.

**Next**

- Add `resolve_members` against final `ModuleMembers`; remove `ModuleLookup`
  and consumer dependence on `NameMap` (§3.7).
- Redesign `Db`/`ModuleView`, carry `(sym, id)` in orders, remove `DefMap`
  (§3.8, Phase D).
- Print from declaration sites and surface listings (§6).
- Decide whether path aliases and `super` return.
- Replace the remaining `SideMap` metadata with `MaybeVec` where density
  warrants it, and remove the remaining unused imports / test warnings.

## 1. Purpose and scope

`kola-resolver` turns parsed source into a symbol graph consumed by the typer,
lowerer, and language server. It is the only phase that knows about lexical
scopes, module identity, and module composition.

It is **not** a type checker. It resolves names, decides which public definition
wins, computes outstanding abstract members, and (in the target design)
elaborates `using` by materializing definitions. Type compatibility belongs to
the typer.

## 2. What the crate must do

1. **Assign identity.** One `ModuleSym` per module tree (`ModuleKey`), one
   `ValueSym`/`TypeSym`/`ModuleTypeSym` per declaration that needs identity.
2. **Resolve lexical references** inside expressions: lets, lambda params,
   pattern binds, handler params.
3. **Resolve module references**: imports, inline bodies, paths, and (target)
   the three composition operators.
4. **Compose module members**: per namespace and public key, pick the winning
   definition (`with` = conflict, `over` = left wins, `using` = supply) and
   compute outstanding abstract keys.
5. **Preserve obligations.** An abstract `x : T` remains an obligation even after
   a definition satisfies it, so the typer can check the supplied definition.
6. **Report** duplicate declarations, unresolved names, non-exported access,
   cycles, and outstanding members at the entry point.
7. **Emit the orders** the typer needs: per-module value/type/module-type
   topological order and a module order.

## 3. Architecture

### 3.1 Stages

```text
SourceTrees (one Tree per ModuleKey, from the parser)
        │
        ▼
   [A] DISCOVER  ── per module tree
        │            collect: top-level linear scan -> symbols, names,
        │                     nodes, ModuleMembers, graph nodes, entry points
        │            bodies:  type schemes, module types, expressions
        │                     (all top-level names already known)
        ▼
   [B] COMPOSE (implemented)
        │            evaluate merge expressions in dependency order:
        │            resolve operand paths / bodies / imports / nested merges
        │            with / over / using -> ModuleMembers
        │            populate composed module names
        │            report conflicts, cycles, non-exported access
        ▼
   [C] PATH LOOKUP (transitional)
        │            resolve qualified module paths (ModuleLookup)
        │            to ModuleSym + ResolvedModule
        ▼
   [D] RESOLVE (planned)
        │            resolve qualified members against final ModuleMembers
        │            then per-module orders + module order
        ▼
       Db
```

Two structural facts drive the design:

- **Intra-module forward references do not exist.** One tree is one module, and
  the whole top-level name set is collected before any body is visited. `x`
  referring to a sibling `y` declared later resolves immediately.
- **Composition is its own stage.** `elaborate/` no longer exists. Module
  composition is a topologically ordered surface operation, not a fixed-point
  job worklist.

### 3.2 Supported module forms and why `elaborate` was removed

The removed `elaborate/` did two things:

1. it ran a fixed-point queue of `ElabJob`s to resolve path module aliases;
2. it inserted `super` into each module's names.

Both are gone. What remains is the subset that does not need deferred path jobs:

- `module m = { ... }` — inline body, its own tree/`ModuleSym`;
- `module m = import file` — alias of the imported file's root `ModuleSym`;
- `m::x` — qualified reference via module-path resolution and consumer member
  lookup;
- `module m : Sig` — an abstract module obligation (no symbol/body yet);
- `module c = a with b`, `a over b`, `a using b` — composed modules with their
  own `ModuleSym` and `ModuleMembers`; operands may be paths, bodies, imports, or
  nested compositions.

Unsupported for now, with explicit diagnostics:

- `module x = a::b` — path alias (including `super::...` aliases).

The fixed-point job machinery was only required by functors and by aliases whose
targets might not exist yet. With functors gone, composition is a DAG over
module symbols. Discovery records a `ComposeJob` per merge expression; compose
resolves nested jobs recursively and reports cycles. Path aliases remain a
separate deferred feature.

### 3.3 Why `kola-subst` disappears from the resolver

The old resolver had 31 `Substitutable<Substitution>` impls across `def.rs`,
`name.rs`, `env.rs`, `lookup/`, and `elaborate/`. They existed to rewrite symbols
inside cloned functor bodies at application time.

With functors removed, each module tree is discovered and resolved exactly once
under its own `ModuleSym`; there is nothing to substitute. `kola-subst` remains a
dependency of `kola-typer` and `kola-types`. Collection-level substitution now
belongs where the collections live:

- `kola-relations` implements `Substitutable` for `SideMap`/`SideVec`/`MaybeVec`;
- `kola-subst-macro` provides `#[derive(Substitutable)]` for metadata structs.

### 3.4 Discovery: top-level collect, then bodies

**One `Tree` is one module.** Every `Id<T>` in tree *K* belongs to module *K*.
`Id<T>` is a dense per-kind index into that tree's per-kind vec; it carries no
module component. This is why discovery is not a cross-tree traversal.

Top-level declarations are reachable directly from their per-kind vecs:

```rust
for (i, (bind, _)) in tree.value_bind.iter().enumerate() { /* ... */ }
for (i, (bind, _)) in tree.type_bind.iter().enumerate() { /* ... */ }
for (i, (bind, _)) in tree.module_bind.iter().enumerate() { /* ... */ }
for (i, (bind, _)) in tree.module_type_bind.iter().enumerate() { /* ... */ }
```

The two discovery stages live in `discover/`:

```text
discover/mod.rs      orchestration, Inputs, ModuleState
discover/collect.rs  Collector: top-level linear scan
discover/bodies.rs   BodyVisitor: type schemes, module types, expressions
```

`Inputs` is the immutable context: `sym`, `source`, `key`, `loc`, `tree`, `files`,
`module_of`, `interner`. `ModuleState` is the owned per-module output:

```rust
struct ModuleState {
    module: ModuleBuilder,
    value_graph: ValueGraph,
    type_graph: TypeGraph,
    module_type_graph: ModuleTypeGraph,
    report: Report,
}
```

- `Collector` owns `ModuleState` and borrows only the global declarations it
  needs (`DefMap`, entry points, module graph). It returns `ModuleState`.
- `BodyVisitor` owns the `ModuleState`, borrows `Lookups`, and owns lexical
  `Bindings`/`Scopes`; it returns the state after visiting bodies.
- `discover` inserts the returned graphs/module into the global maps and returns
  the report.

The declaration half of the old visitor is gone. `collect` registers all
top-level names and symbols; `bodies` visits only type schemes, module types,
and expressions, resolving same-module names from the already-complete
`NameMap`.

`ModuleBuilder` is a transient build row:

```rust
pub struct ModuleBuilder {
    pub loc: Loc,
    pub key: ModuleKey,
    pub names: NameMap,       // discovery-local resolution
    pub nodes: NodeMap,       // local binder metadata
    pub members: ModuleMembers,
}
```

Private names stay in `NameMap`; the surface is public-only.

### 3.5 Data model

```rust
pub type ModuleSym     = Sym<ModuleNamespace>;
pub type ModuleTypeSym = Sym<ModuleTypeNamespace>;
pub type TypeSym       = Sym<TypeNamespace>;
pub type ValueSym      = Sym<ValueNamespace>;

pub struct Sym<T: ?Sized> { id: NonZeroU32, _t: PhantomData<T> }
```

#### Typed definitions and obligations

```rust
pub struct Definition<S, B> {
    pub sym: S,
    /// Source identity for conflict checks. Equal to `sym` for a source
    /// definition; a `using` forwarder keeps the provider's origin.
    pub origin: S,
    /// Where the defining syntax lives, for printing and diagnostics.
    pub site: (ModuleSym, Id<B>),
}

pub struct Obligation<D> {
    pub loc: Loc,
    pub declared: D,
}
```

There is no `Key` enum, no `AnySym`/`AnyId` in the surface, no `vis` field, and
no `DefinitionBody`/`Forward` variant. Visibility is public-only; private support
stays in `NameMap`. `using` copies a provider `Definition` rather than
synthesizing a forwarder body, and `origin` supplies the conflict identity.

#### Definitions and obligations

The public surface is split by role, and each role has a symmetric sorted table
per namespace. Only values and modules can be abstract, so only those two
namespaces have an obligation table:

```rust
pub struct ModuleMembers {
    pub definitions: ModuleDefinitions,
    pub obligations: ModuleObligations,
}

pub struct ModuleDefinitions {
    pub values: Definitions<ValueName, ValueSym, ValueBind>,
    pub types: Definitions<TypeName, TypeSym, TypeBind>,
    pub modules: Definitions<ModuleName, ModuleSym, ModuleBind>,
    pub module_types: Definitions<ModuleTypeName, ModuleTypeSym, ModuleTypeBind>,
}

pub struct ModuleObligations {
    pub values: Obligations<ValueName, Id<TypeScheme>>,
    pub modules: Obligations<ModuleName, Id<ModuleType>>,
}

pub struct Definitions<K, S, B> {
    pub entries: Vec<(K, Definition<S, B>)>,
}

pub struct Obligations<K, D> {
    pub entries: Vec<(K, Obligation<D>)>,
}
```

- `Definitions` is a sorted, deduplicated table of public definitions.
- `Obligations` is the same shape for abstract declarations.
- `Outstanding` is per namespace: `obligations \ definitions`;
  `ModuleMembers::outstanding_values()` and `outstanding_modules()` expose it.
- Compose merges `definitions` per namespace and unions `obligations`.
- `using` copies definitions but keeps obligations for the typer’s check.
- Entry-point completeness checks both outstanding value and module keys.
- There is no combined definitions/obligations table and no phantom `D` for
  namespaces that cannot be abstract.

#### Sorted vectors

Each namespace’s tables are sorted by the typed name key (interned `StrKey` id
order). Key counts are in the tens, so a sorted `Vec` with binary search is
chosen for contiguous iteration and for the composition zip; `BTreeMap` would
also be acceptable. The surface has no display contract; display sorting is the
printer’s job.

#### Metadata

`NodeMap`/`TypedNodes` are `#[derive(RelationStorage)]` structs. Metadata columns
use `MaybeVec` (dense slots that may still be empty) because discovery and typing
set entries out of order; iteration visits occupied slots only. `SideVec` remains
for fully dense cases; `SideMap` for genuinely sparse ones.

### 3.6 Composition

Composition operates on typed `ModuleMembers`, one namespace at a time:

```rust
fn compose_with(lhs: &ModuleMembers, rhs: &ModuleMembers, loc: Loc)
    -> Result<ModuleMembers, Diagnostic>;
fn compose_over(lhs: &ModuleMembers, rhs: &ModuleMembers) -> ModuleMembers;
fn compose_using(lhs: &ModuleMembers, rhs: &ModuleMembers) -> ModuleMembers;
```

Discovery allocates one `ModuleSym` and one `ComposeJob` per merge expression.
Compose resolves each job recursively (so nested merges and composed operand
paths work), fills the module's `ModuleMembers`, and populates the composed
module's `NameMap` for later path lookup.

| operation | definitions | obligations |
| --- | --- | --- |
| `with` | union; error on one key with two distinct origins | union |
| `over` | left wins on equal key; left abstract does not mask right definition | union |
| `using` | `Outstanding(a) ∩ definitions(b)`; copy the provider definitions | retained |

- `using` produces ordinary definitions; downstream code never special-cases it.
- `origin` makes `(a using b) with (a using b)` idempotent.
- Composition is cold: once per module expression, on tens of keys.
- The runtime’s `RecordArena` merge is the algorithmic model (sorted two-pointer
  zip), but its shadowed-duplicate spine is not adopted: a surface needs one
  definite answer per key for `Outstanding`, lookups, diagnostics, and printing.
- A lazy structural `Merge(l, r) | Single(m)` representation is not adopted:
  obligations compose fine, but conflicts would be deferred and enumeration
  would materialize anyway.
- Copy-on-write trees and splicing operands into one tree are rejected:
  operands must survive, `Id<T>` is tree-local, and splicing is a full reference
  classification plus AST remap.
- A composed module is a **view over `ModuleMembers`**, not an owner of syntax.

### 3.7 Module paths and member resolution

Qualified references have two steps. They are currently split across stages, and
that split is transitional.

**Step 1: module path** (`M::x` → `M`).

- Discovery records `ModuleLookup { path, id: Id<ModulePath>, source, loc }` for
  qualified references in bodies.
- Compose resolves module-expression operand paths itself and populates composed
  module names first.
- `lookup_modules` then walks body paths through module names, checks module
  visibility, adds a module dependency, and writes `ResolvedModule` on the
  `ModulePath` node.
- `ModuleLookup` remains as the deferred body-reference pass. It is not an
  obligation; it is a reference. Path aliases (`module x = a::b`) are still
  unsupported.

**Step 2: member** (`x` in the target module).

- Today the typer reads `modules[target].names.get_value(x)` and the lowerer
  emits a module record access by label. This works only because source modules
  have a `NameMap`; composed modules will not.
- Target: a resolver `resolve_members` pass, after compose:
  - walk `QualifiedExpr` / `QualifiedType` / `QualifiedModuleType`;
  - read the resolved `ModuleSym` from the path;
  - look up the member in
    `target.members.definitions.values/types/module_types`;
  - write `ResolvedValue::Reference` / `ResolvedType::Reference` /
    `ResolvedModuleType`;
  - add the value/type graph dependency;
  - report “not exported” when the key is absent from the public surface.
- Then typer/lowerer consume resolved symbols uniformly, `NameMap` becomes
  discovery-only, and `ModuleLookup`/`Lookups` can be removed.

Visibility caveat:

- surfaces are public-only, which is correct for cross-module access;
- a module may legally reference its own private submodule by path, so path step
  1 may need `NameMap` for same-module private module names;
- member step 2 is public-surface based; private members are not accessible
  through qualified paths.

### 3.8 Module abstractions and `Db`

Current:

- `ModuleBuilder` — transient per-module build row owned by discovery;
- `ModuleView` — convenience bundle of `module`, `tree_map`, `members`, `defs`,
  and orders;
- `Db` — holds `tree_map`, `files`, `modules`, `defs`, graphs, orders,
  entry points.

Target:

- `ModuleView` should disappear. Consumers take `&Db` + `ModuleSym` and query
  per-module columns.
- `Db` should become the relation-storage context: per-module `nodes`, `members`,
  orders, and identity/tree mapping as columns.
- `ModuleBuilder` is not a long-lived abstraction; after discovery, its data is
  decomposed into `Db` columns and its `NameMap` can be dropped once path/member
  resolution no longer needs it.
- `ModuleSym` ids come from a shared generator across namespaces, so they are
  sparse globally. A dense `SideVec<ModuleSym, _>` would be mostly holes; use a
  sparse `SideMap` or give namespaces their own dense id spaces before choosing
  `SideVec`.

### 3.9 Entry point

After composition, the root module must have no outstanding values or modules.
This is a resolver-stage structural error, reported before type checking. It is
not a type-compatibility check.

## 4. Using `kola-relations` for read-only traversal

### 4.1 Containers and derives

- `MaybeVec<T, M>` — dense slots with `Option` internals; supports out-of-order
  `set`/`insert`; `ids()`/`rows()` visit occupied slots only; `TotalRelation::get`
  expects an occupied slot.
- `SideVec<T, M>` — fully dense vector.
- `SideMap<T, M>` — sparse map.
- `#[derive(RelationStorage)]` generates relation impls for `SideVec`, `MaybeVec`,
  `SideMap`, and `Vec`.
- `PartialRelationSet` provides insert-by-id for `MaybeVec`/`SideMap`/maps.
- `#[derive(Merge)]` generates row-wise `merge` for relation columns.
- `kola-relations` implements `Substitutable` for its containers; the
  `Substitutable` derive lives in `kola-subst-macro`.

Metadata joins (`Query::with::<M>()`, `with_required`, `left_join`) remain the
right tool for structural inspection and resolver tests.

### 4.2 Queries read, the visitor writes

Discovery’s expression half must stay a hand-written `Visitor`:

- it mutates lexical scope (`LinearScope::enter`/`exit`);
- it creates symbols, so it is a generator, not a read;
- failures are located diagnostics, while plans are assertion-oriented.

The top-level half is a plain per-kind scan. Queries are for inspection and
tests, not declaration collection or scope mutation.

## 5. Tree ownership

`SourceTrees { root: ModuleKey, trees: IndexMap<ModuleKey, Tree<Loc>> }` is the
parser output and the resolver input.

- **One `ModuleSym` per `ModuleKey`**, not per source file.
- **`Id<T>` never crosses a tree boundary.** Only `ModuleKey`/`ModuleSym` crosses.
  `ModuleRef(ModuleKey)` is the bridge node in the parent tree.
- **`Loc` carries `SourceId`**, and spans live in `Tree<Loc>`; there is no
  parallel `LocMap`/`LocVec`.
- **`Loc` and `Key<T>` do not implement `Default`.** Tree-fragment constructors
  take an explicit `meta: M` and use `alloc_located`; span-less allocation is
  available only for metadata types with a meaningful `Default` (e.g. `()`).
- A symbol’s source location is `Definition.site = (ModuleSym, Id<B>)` →
  `tree.get_span(id)`.

### 5.1 One translation, then one key space

```rust
module_of: HashMap<(SourceId, ModuleKey), ModuleSym>
```

Translate once in `resolve`; after that, composition, orders, `Db`, and consumers
see only `ModuleSym`. `files: SourceId -> ModuleSym` remains the import target
map.

## 6. Printing

### 6.1 Current constraints

- `TreePrinter` holds one `tree: &Tree<M>`; spans and node relations are
  resolved within that tree.
- `Id<T>` is local to one tree, so a printer cannot walk across trees without a
  composite address.
- Nested inline bodies currently print as `moduleN` because `ModuleRef` prints
  its key; this is not a regression.
- Composed modules have no tree; their members live in operand trees.

### 6.2 Target

- Render from a declaration site, not always a tree root:
  `TreePrinter::render_subtree(id)` alongside `render(root_id())`.
- A named module — composed or not — is its `ModuleBind` in the enclosing
  module’s tree.
- Composed modules get a **surface listing**, not syntax: iterate
  `members.definitions` per namespace and print key + origin/site.
- Nested inline body expansion remains a separate composite-address feature.

`ModuleView` should expose a declaration site and the tree its syntax lives in;
there is no invented `ModuleSource`.

## 7. Phases

### Phase A — port to per-module trees (done)

- Replaced `TreeMap`/`LocMap`/`LocVec` with `SourceTrees`/`Tree<Loc>`.
- Replaced `kola_tree::id::Id` with the relations `Id`.
- Replaced `GetOpt`/`Get` with `PartialRelation`/`TotalRelation`, and the
  side-table macro with `#[derive(RelationStorage)]`.
- Deleted functors, functor substitution, `kola-subst` from the resolver, and
  `Sym::level`.
- Fixed printer arity and driver/LS plumbing.
- Removed `Loc`/`Key` `Default` and made tree-fragment locations explicit.

Verification: workspace builds and existing examples resolve.

### Phase B — three states instead of two (done)

- `ModuleMembers` records defined public members and abstract obligations.
- Value/module binds with a body are defined; typed binds without a body are
  obligations.
- The typed surface shape is installed:
  `ModuleMembers { definitions, obligations }`, with `Definitions` and
  `Obligations` tables per namespace.

### Phase C — composition (done)

Composition is implemented in `compose.rs`:

```text
import(file)        -> files[file]
body(ModuleRef)     -> module_of[(source_id, key)]
path(a::b)          -> resolve through module names, visibility-checked
merge(lhs, op, rhs) -> compose_{with,over,using}
```

- Discovery allocates a `ModuleSym`/`ModuleMembers` and a `ComposeJob` for each
  merge expression, including nested merges.
- Compose resolves operands recursively, so a composed module can be an operand
  and paths can traverse composed modules.
- `with` reports conflicting definitions at the composition site; `over` keeps
  the left definition; `using` supplies outstanding members from the right and
  leaves the provider's other fields behind.
- `using` copies the provider's `Definition`, preserving `origin` for
  idempotence.
- Composed module names are populated from their definitions so subsequent path
  lookup can traverse them.
- Path aliases and `super` remain unsupported.

### Phase D — orders, `Db`, `DefMap` removal

- Orders become `Vec<(Sym, Id<Bind>)>` so consumers iterate `(id, tree)`.
- `Db` carries `SourceTrees`, `module_of`, per-module `nodes`/`members`, graphs
  and orders, entry points.
- Delete `DefMap`.
- `ModuleView` disappears; consumers query `Db` + `ModuleSym`.
- Dense `MaybeVec`/`SideMap` columns as appropriate; module syms are sparse.

### Phase E — cleanup and consumers

- Printing from declaration sites and surface listings.
- `resolve_members` output consumed by typer/lowerer; `NameMap` discovery-only.
- LS source ownership (`Source = ariadne::Source<&'static str>` currently forces
  leaking document text), shared interner/source manager.
- Remove remaining dead imports and test warnings.

## 8. Consumer contract

Current typer/lowerer use:

```text
db.all_modules()      db.modules        db.entry_points
view.sym              view.module       view.defs
view.members          view.tree_map      view.value_order / type_order
view.value_node(sym)  view.type_node(sym)
view.module.nodes
```

Mapping to the target:

| today | target |
| --- | --- |
| `view.tree_map` | `view.tree()` / declaration site |
| `view.defs[sym]` | `Definition.site` → `tree.get_span(id)` |
| `view.value_node(sym)` | orders carry `(sym, id)` |
| `view.type_node(sym)` | same |
| `view.members` | typed `ModuleMembers` |
| `view.module.nodes` | survives: local binder symbols |
| qualified member lookup via `names` | `resolve_members` writes `Resolved*` |
| `ModuleView` | `Db` + `ModuleSym` queries |

Local value symbols remain: the lowerer uses them as IR identity.

## 9. Complexity

Let `N` = AST nodes, `M` = modules, `k` = keys in one namespace, `V`/`E` =
order-graph nodes/edges, `d` = lexical scope depth.

| phase | cost | note |
| --- | --- | --- |
| top-level collect, per tree | `O(N)` | per-kind vecs |
| surface insert | `O(k log k)` per namespace | sorted vec |
| body visit | `O(N · d)` | linear-scope lookups dominate |
| module path lookup | `O(path length · log k)` | module namespace |
| member lookup | `O(log k)` | typed surface |
| composition | `O(Σ(k_a + k_b))` | sorted zip per namespace |
| orders | `O(V + E)` per module | topological sort |

The hotspot is still `d`: `LinearScope` wraps `StackMap`, whose lookups are
linear. That is pre-existing in the typer and resolver; `ShadowMap` exists in
`kola-collections` as a separate optimization.

Notes:

- `ModuleSym` ids are sparse globally; use `SideMap`/`MaybeVec` rather than a
  dense `SideVec<ModuleSym, _>`.
- `SourceId` is a global interner key; `files` stays a map.
- The old per-lookup module-rank array is gone with unqualified lookups.

## 10. Migration checklist

Done:

- [x] Drop `kola-subst` from `kola-resolver`; delete functor substitution.
- [x] Delete functors and `Sym::level`/`LEVEL`/`enter`/`exit`/`branch`.
- [x] Per-module trees; `SourceTrees`/`module_of`.
- [x] Remove `Loc`/`Key` `Default`; explicit locations in constructors.
- [x] Phase B member states and obligations.
- [x] Split discovery into `collect` and `bodies`; top-level scan before bodies.
- [x] Delete unqualified lookups; immediate unresolved-name diagnostics.
- [x] Delete `elaborate/`; imports, inline bodies, qualified paths, and
      composition.
- [x] Typed `Definition`/`Obligation`/`Definitions`/`Obligations`/`ModuleMembers`.
- [x] Split `ModuleMembers` into `definitions` and `obligations` tables.
- [x] Compose `with`/`over`/`using`; allocate merge module symbols.

Next:

- [ ] Decide path aliases/`super` return.
- [ ] `resolve_members` against `ModuleMembers`; delete `ModuleLookup`/`Lookups`.
- [ ] Orders carry `(sym, id)`; delete `DefMap`.
- [ ] `Db` relation storage; remove `ModuleView`; `NameMap` discovery-only.
- [ ] Printing from declaration sites / surface listings.
- [ ] LS source ownership and shared interner.
- [ ] Remove dead imports and test warnings.

## 11. Decisions and open points

**Decided**

- One `ModuleSym` per `ModuleKey`; one translation then a single key space.
- Composition is a topological surface operation, not a job worklist.
- Types/logic: public surfaces only; private names stay in `NameMap`.
- Typed per-namespace tables; no `Key` enum or `AnySym`/`AnyId` in surfaces.
- Top-level `definitions`/`obligations` split; only values/modules have
  obligations.
- `using` copies definitions and relies on `origin`; no forwarder body.
- Composed modules are views over `ModuleMembers`, not trees.
- Cross-module member resolution belongs in the resolver against final surfaces,
  not in typer/lowerer `NameMap` lookups.
- `MaybeVec` for partially populated metadata; `RelationStorage`/`Merge`/
  `PartialRelationSet` derives; `Substitutable` in `kola-relations`.

**Open**

1. Where module-type conformance lives: resolver structural shape vs typer
   value/type equality.
2. Path aliases/`super` return in compose or later; `super` needs an
   acyclicity/parent rule.
3. Inferred vs declared obligations.
4. Incremental resolution and LS invalidation.
5. Partial results after each error class.
6. Composed module lowering/runtime records.
7. Lowerer-minted local symbols (could collapse local `NodeMap` tables).
8. `ModuleSym` density / `Db` column representation.
9. Scope lookup cost (`LinearScope` → `ShadowMap`), separate optimization.
10. Source ownership for the language server (currently leaked `&'static str`).

## 12. Summary

- **Discover** is two-stage: a top-level linear scan builds symbols, names,
  nodes, `ModuleMembers`, graphs, and entry points; a body visitor then resolves
  locals and same-module names with no forward-reference queue.
- **Path lookup** is the remaining transitional pass; composed modules and
  member resolution will replace it.
- **Compose** is a set of per-namespace sorted-zip operations over typed
  surfaces, run once in topological order.
- **Resolve** (target) resolves qualified members against final
  `ModuleMembers`; typer/lowerer consume resolved symbols, not `NameMap`.
- **Identity and output** no longer need `DefMap`: `Definition.site` carries the
  source site, and orders will carry `(sym, id)`.
- **Relational metadata** is `MaybeVec`-backed and derive-driven; queries read,
  the visitor writes.
- **Printing** renders from declaration sites; composed modules get a surface
  listing, not syntax.

# Resolver Architecture (v1 module system)

**Status:** design proposal, supersedes the previous `resolver_architecture.md`.
**Depends on:** `design/module_system_v1_spec.md` (normative semantics);
the AST/grammar increment has already landed.

## 1. Purpose and scope

`kola-resolver` turns parsed source into a symbol graph that the typer, lowerer,
and language server consume. It is the only phase that knows about lexical
scopes, module identity, and composition.

It is **not** a type checker. Per spec §9.1 it resolves names, decides which
public definition wins, computes outstanding abstract members, and elaborates
`using` forwarders. Type compatibility is decided afterwards.

This document replaces the previous one, which was written against the discarded
`require`/`provide`/`//` design. The differences that matter:

| Old document assumed | v1 actually specifies |
| --- | --- |
| `require` / `provide` keywords | a typed bind with no body is abstract |
| requirement rows as the abstraction | three member states: absent / abstract / defined |
| open modules lower to lambdas over requirement records | modules are symbolic; composition is resolved, not applied |
| `//` composition | `with` (symmetric), `over` (left-preferential), `using` (supply) |
| functors with module parameters | no functors at all (spec §2.1 non-goal) |
| `export` marks visibility | public by default, `private` opts out |

## 2. What the crate must do

From the spec, in dependency order:

1. **Assign identity.** One `ModuleSym` per module tree (`ModuleKey`), one
   `ValueSym`/`TypeSym`/`ModuleTypeSym` per declaration that needs identity.
2. **Resolve lexical references** inside expressions: let-bindings, lambda
   params, pattern binds, handler params.
3. **Resolve module references**: `import`, inline bodies, paths, and the three
   composition operators.
4. **Compose module surfaces**: per public key, pick the winning definition
   (`with` = conflict, `over` = left wins, `using` = forwarder) and compute the
   surviving set of abstract keys.
5. **Preserve obligations.** An abstract `x : T` stays in the obligation set even
   after a definition satisfies it, so the typer can check the supplied
   definition against it.
6. **Report** duplicate declarations, unresolved names, non-exported access,
   cycles, and outstanding members at the entry point.
7. **Emit the orders** the typer needs: per-module value/type topological order,
   and a module order.

## 3. Architecture

### 3.1 Stages

```text
SourceTrees (one Tree per ModuleKey, from the parser)
        │
        ▼
   [A] DISCOVER  ── per module tree
        │            · linear scan: symbols + surface for every top-level bind
        │            · visit expressions: lexical scopes, local symbols
        │            · same-module references resolve here (no queue)
        │            · queue only *cross-module* references
        ▼
   [B] COMPOSE   ── evaluate the module-expression DAG, topological order
        │            · import / path / body / with / over / using
        │            · build each module's surface
        │            · report conflicts, cycles, non-exported access
        ▼
   [C] RESOLVE   ── cross-module references against final surfaces
        │            · then per-module topological orders + module order
        ▼
      Db
```

Two structural changes from the old design:

**Composition is its own stage**, not a side effect of elaboration jobs. The old
crate fused "resolve a module alias" into a fixed-point worklist that also did
functor substitution.

**Intra-module forward references do not exist.** Because one tree is one module,
every top-level name in a module is collectible by a linear scan *before* any
expression is visited. So `x` referring to a sibling `y` declared later resolves
immediately — there is nothing to queue. Only `other::x` needs a second pass,
because it depends on another module's composed surface.

### 3.2 Why not keep the fixed-point worklist

The old `elaborate/` ran a `VecDeque` of jobs, re-queueing blocked ones until no
progress was made, then reporting a cycle. That was required because a functor
instance had to be *synthesised* (substitution over an entire `Module` plus its
lookups and jobs) and its argument modules might not exist yet.

None of that survives v1:

- no functors, so no instance synthesis and no substitution;
- `with`/`over`/`using` operands are references to already-declared modules, not
  newly constructed ones;
- the dependency relation is a DAG over module symbols, which
  `DependencyGraph::topological_sort` already computes.

So `Compose` runs **once, in topological order**, and a cycle is whatever
`topological_sort` reports. This removes the stagnation check, the
`UnresolvedBinds` set, and the `StepResult::{Done, Blocked, Error}` state
machine.

### 3.3 Why `kola-subst` disappears from the resolver

The old resolver had 31 `Substitutable<Substitution>` impls across `def.rs`,
`name.rs`, `env.rs`, `lookup/` and `elaborate/`. All of them exist to rewrite
symbols inside a cloned functor body at application time (`Functor::apply`,
`env.rs:76-97`).

With functors removed there is nothing to substitute: each module tree is
elaborated exactly once, under its own `ModuleSym`. `kola-subst` stays a
dependency of `kola-typer` and `kola-types`, where it belongs, and drops out of
`kola-resolver` entirely. That deletes roughly a third of the crate.

### 3.4 Discovery is mostly linear scan, not traversal

**One `Tree` is one module**, so every `Id<T>` in tree *K* belongs to module *K*.
`Id<T>` is a dense per-kind index into that tree's per-kind vec
(`Id::new(vec.len())` on alloc, `vec[id.as_usize()]` on read — `tree.rs:257-258`,
`:208`). There is no shared id space to key across modules, and no per-module
`NodeMap` is needed to hold "which module does this belong to".

Two facts make top-level collection trivial:

- `ValueBind`, `TypeBind`, `ModuleBind`, `ModuleTypeBind` are referenced **only**
  through `Bind::Value(Id<ValueBind>)` and friends (`node/module.rs:42-48`).
  Nothing else points at them, so each is directly reachable from its own vec.
- One tree contains exactly one `ModuleBody` (the root), so `root_id()` is
  unambiguous — the driver's `TODO: tree.root_id() is wrong` comments
  (`driver:157,221`) dissolve by construction once trees are per module.

So collecting declarations is a linear scan:

```rust
for (i, (bind, loc)) in tree.value_bind.iter().enumerate() { /* ... */ }
```

Order within the scan is per-kind allocation order, i.e. source order for that
kind. Anything that needs true interleaving across kinds (printing, "declared
here" ordering) walks the body slice `tree.get(tree.root_id()).0`, which is
exactly source order. Both are cheap; keep both.

**The scan happens before expression visiting.** That is what removes
intra-module forward references: the module's whole public name set is known
before any body is traversed, so `y` used in `x`'s body resolves whether `y` is
declared above or below.

Discovery therefore shrinks to: the linear scan, plus a visitor for
**expressions** — where `LinearScope::enter`/`exit`, local binder symbols, and
pattern binds live. Note the lowerer consumes those local symbols as IR identity
(§3.8), so they are not removable; only their dead `level` metadata is.

### 3.5 Data model

```rust
pub type ModuleSym     = Sym<ModuleNamespace>;
pub type ModuleTypeSym = Sym<ModuleTypeNamespace>;
pub type TypeSym       = Sym<TypeNamespace>;
pub type ValueSym      = Sym<ValueNamespace>;

/// `level` is deleted; a symbol is just an id. See §3.8.
pub struct Sym<T: ?Sized> { id: NonZeroU32, _t: PhantomData<T> }

/// One public key = (namespace, source name).
pub enum Key {
    Value(ValueName),
    Type(TypeName),
    Module(ModuleName),
    ModuleType(ModuleTypeName),
}
```

```rust
/// A module's resolved surface. One of these per ModuleSym.
/// Both vecs are kept sorted by Key.
pub struct ModuleSurface {
    pub defined:     Vec<(Key, Definition)>,
    pub obligations: Vec<(Key, Obligation)>,
}

pub struct Definition {
    pub sym: AnySym,
    /// Source identity. NOT redundant with `sym`: a `using` forwarder has a
    /// fresh `sym` but keeps the provider's origin, which is what makes
    /// `(a using b) with (a using b)` idempotent rather than a conflict.
    pub origin: AnySym,
    pub vis: Vis,
    /// Where the defining syntax lives, for printing and diagnostics.
    pub site: (ModuleSym, AnyId),
    pub body: DefinitionBody,
}

pub enum DefinitionBody {
    Source(Id<ValueBind>),                 // ordinary definition in `site`'s tree
    Forward { from: ModuleSym, key: Key }, // `B::k`
}

pub struct Obligation {
    pub loc: Loc,
    pub declared: ObligationKind, // Value(Id<TypeScheme>) | Type(Kind) | Module(Id<ModuleType>)
}
```

`Outstanding(surface) = obligations \ defined` by key — a direct transcription of
spec §4.1.

**No `privates` field.** Private definitions never move between modules: `using`
anchors to the provider, so `B`'s private support stays in `B` and the result
only records a dependency on `B`. Privates are per-tree state, not per-surface.

#### Why a flat sorted `Vec`, not `BTreeMap` / `HashMap`

At this scale the containers are within noise of each other:

| representation | build | lookup |
| --- | --- | --- |
| `HashMap` | `O(k)` | `O(1)`, one pointer chase |
| `BTreeMap` | `O(k log k)` | `O(log k)` |
| sorted `Vec` | `O(k log k)` | `O(log k)`, binary search |
| merge tree, uncached | free | `O(D · k)` |

Key counts are in the tens, and a linear scan over a contiguous sorted vec is
~32 comparisons in one or two cache lines. So this is **not decided on
performance**. It is decided on shape: composition (§3.6) is already a sorted
two-pointer zip, and both enumeration (printer, `--show-members`) and that zip
want contiguous iteration. A sorted `Vec` is the same asymptotics as `BTreeMap`
with fewer allocations and no per-node pointer chasing.

`BTreeMap` would be an equally acceptable choice. What matters is that there is
**one** representation: normalized, deduplicated, at most one definition and one
obligation per key.

#### What "sorted by `Key`" means

`Key`'s `Ord` compares interned `StrKey` **ids**, not names
(`kola-interner/src/lib.rs:56`). So canonical order is global interning order —
deterministic, and identical on both sides of a zip because they share one
interner, so the merge and binary search are both correct.

It is *not* alphabetical, and it is only approximately source order (a name
interned earlier elsewhere gets a lower id). That is fine for the surface, which
has no display contract. **Sorting for display is the printer's job**: one
`O(k log k)` over the slice at print time. Do not make the surface's canonical
order alphabetical — that would push the interner into `compose`.

### 3.6 Composition as a sorted zip

Model composition on `RecordArena::merge` / `merge_left`
(`kola-runtime/src/record.rs:446`, `:522`): a sorted two-pointer zip over keys.
The algorithm transfers; the container does not.

```rust
fn compose_with(a: &ModuleSurface, b: &ModuleSurface) -> Result<ModuleSurface>;
fn compose_over(a: &ModuleSurface, b: &ModuleSurface) -> Result<ModuleSurface>;
fn compose_using(a: &ModuleSurface, b: &ModuleSurface) -> Result<ModuleSurface>;
```

| runtime | modules |
| --- | --- |
| `merge_left` (left wins) | `over` |
| `merge` (conflict on equal key) | `with` |
| O(1) tail sharing | not adopted (see below) |

- `compose_with`: zip, error where both sides define a key with **distinct
  origins**, else union; union `obligations`.
- `compose_over`: zip, left wins on equal key; union `obligations`. `a`'s
  *abstract* declaration does not mask `b`'s definition (spec §5.2), so this
  merges over `defined` only and recomputes outstanding.
- `compose_using`: `S = Outstanding(a) ∩ dom(b.defined)`; for each `k in S`
  synthesize a **fresh** forwarder anchored to `b` (spec §7.2), then
  `compose_with(a, forwarders)`.

`using` needs no special-casing downstream: it produces an ordinary surface whose
forwarders are ordinary definitions with fresh origins.

#### Why not the arena

The runtime's `RecordArena` is the right shape for the runtime and the wrong one
here:

1. **Composition is cold.** Once per module expression at compile time, on tens
   of keys. Records merge in the interpreter loop, millions of times.
2. **It keeps shadowed duplicates on the spine.** Traced `merge_left` on
   `left = [a:1, c:3]`, `right = [a:2, b:2]` yields `[a:1, a:2, b:2, c:3]` —
   duplicates retained, first occurrence wins; `len`'s own doc comment says
   "including shadowed entries". That is excellent for a runtime with lazy
   precedence, and wrong for a surface: `Outstanding`, "what is `c::port`?", the
   printer and the conflict diagnostic all need one definite answer per key. A
   shadowed spine pushes first-occurrence semantics into every consumer, where a
   mistake is a silent wrong answer.
3. **`with` must diagnose, not prefer.** Two distinct origins at one key is an
   error, so `with` needs the dedup pass regardless. If `with` normalized while
   `over` kept a spine, there would be two representations of a surface.

What *does* transfer is the arena's failure discipline: `merge` truncates
speculative nodes and returns `None` on mismatch. With a flat `Vec` that comes
free by building into a local and returning `Ok` only at the end.

#### Why not `Merge(Left, Right) | Single(Module)` evaluated on the fly

A structural module — an enum of composites resolved lazily — is a legitimate
design, and **obligations are not what rules it out**: obligations compose by the
same union rule as definitions, so any mechanism that resolves `defined` resolves
`obligations` identically. The costs are elsewhere:

- **Conflicts get deferred.** `with` must report two distinct origins at one key.
  Unmaterialized, that only surfaces when someone looks up the key, so
  `module c = a with b` succeeds and `c::port` fails three files later. Getting
  the error back at the definition site needs a validation walk — the same
  `O(total keys)` the materialization costs.
- **Enumeration materializes anyway.** `all_modules()`, the printer and
  `--show-members` need "all members of `c`", which a merge tree can only answer
  by full traversal and dedup — i.e. building the map that was being avoided.
- **Random access repeats work.** The typer asks `surface[key]` once per qualified
  reference, and the entry point asks `Outstanding(root)`. Uncached, each is a
  walk.

So it trades build cost for query cost *and* defers diagnostics. The decisive
argument is not the constant factor but that the derived surface and the merge
tree could disagree; materializing once means one source of truth.

If deferred evaluation is ever wanted, the cheap variant is memoizing the
materialization behind the enum — but that *is* the flat surface with extra
indirection.

#### Why not copy-on-write trees

`Tree` is already CoW-ready — every per-kind vec is `Arc<Vec<(T, M)>>` and every
write goes through `Arc::make_mut` (`tree.rs:139,170,235,246,258`).

But composition never *writes*: `with`/`over`/`using` only select among existing
nodes. A composed module that cloned `a`'s tree would make zero `make_mut` calls
— it would be a pure alias, which is the "it does not own a tree" situation
again. CoW is relevant only to splicing, and that fails for reasons CoW cannot
fix (below). Cheap copying would make the wrong representation look free.

#### Why not splice operands into one tree

1. **Operands must survive.** I4 says `base::x` resolves against the
   independently resolved `base`; §6.3 says every named source module
   type-checks independently. `module c = a with b` cannot consume `a`'s tree.
2. **Ids are tree-local by design.** Copying `a`'s nodes into a fresh tree means
   remapping every internal `Id`, and remapping requires knowing whether each
   reference is `Self` (late-bound — must *stay* symbolic against the final
   receiver), `Local` (point at the copied private) or `External` (anchor to the
   other module). That is precisely the reference categorization the surface
   model needs anyway, so splicing is that analysis *plus* a deep AST rewrite, on
   every `with`/`over`.
3. **`Id<T>` has no module component.** Putting `a`'s `Id` inside `c`'s tree
   would break the one invariant the parser maintains.

A composed module is a **view over surfaces**, not an owner of a tree.

### 3.7 Entry point (spec §8.5)

After composition: `Outstanding(root)` must be empty. This is a resolver-stage
error, reported before the typer runs, because it is a structural fact, not a
typing one.

### 3.8 Deleting `DefMap`, `DefinitionSite`, and `Sym::level`

#### `DefMap` is removable

Every consumer of `DefMap` either wants a `Loc` for a diagnostic or wants the
node for a symbol, and both have better homes now.

The four typer sites use **only the location** — verified, they never touch the
node id:

```text
typer.rs:380   &self.defs[module_sym]   → uses !.1     ("In this module")
typer.rs:393   self.defs[binding.sym]   → uses  .1     ("Declared here")
typer.rs:989   &self.defs[module_sym]   → uses !.1
typer.rs:1002  self.defs[binding.sym]   → uses  .1
```

Replacements:

| use | replacement |
| --- | --- |
| value/type "declared here" | `Definition.site` → `tree.get_span(id)` |
| module "in this module" | `ModuleView::declaration_site()` → the `ModuleBind`'s span |
| `value_node(sym)` / `type_node(sym)` | **orders carry the node id**: `Vec<(ValueSym, Id<ValueBind>)>` |
| module node | `declaration_site()`, which exists for composed modules too |

Because the orders carry `(sym, id)` pairs (or just `Id`s, deriving the sym from
the tree's dense `SideVec`), the typer and lowerer iterate `(id, tree)` directly
and never map sym → node. That also covers private symbols, which are absent from
the surface but present in their own module's order.

So neither `DefMap`'s five `Defs<N, T>` tables nor the previously proposed
`DefinitionSite` needs to exist. `Definition.origin` stays; `Definition.sym` stays.

#### `Sym::level` is dead

`Sym` carries `{ id, level }`, where `level` comes from a global `AtomicU32` with
`enter`/`exit`/`branch` helpers. Verified: **zero readers of `.level()`**
anywhere in the workspace. The only callers of `enter`/`exit` are
`discover.rs:665-667`, whose sole effect is to make ids differ.

Delete `level`, `LEVEL`, `enter`, `exit`, `branch`. `Sym` collapses to a
`NonZeroU32` plus `PhantomData`, `Copy`, `Eq`, `Hash`, and the per-namespace
`Display` prefixes.

#### `NodeMap` shrinks; it does not disappear

An earlier note in this design claimed the resolver's local value symbols
(`bind_pats`, `lambda_exprs`, `handler_clauses`, `let_exprs`, `list_el_pats`,
`record_field_pats`, `qualified_exprs`) had no readers. That is wrong. **Both**
the typer and the lowerer consume them, and the lowerer does so as IR identity:

```rust
// kola-typer
typer.rs  uses ModuleView::module.nodes for local_env fallback and metadata

// kola-lowerer
module.rs:192    let (id, tree, _) = view.value_node(value_sym);
module.rs:194    Symbol::new(view.module.nodes.get_unchecked(id).id())
module.rs:209    Normalizer::new(..., &view.module.nodes, ...)   // locals
normalizer.rs:187  ResolvedValue::Reference(sym) => ir::Atom::Symbol(...)
normalizer.rs:299  symbol_of(id)         // lambda param
normalizer.rs:362  symbol_of(clause_id)  // handler clause
normalizer.rs:915  symbol_of(id)         // bind pat
normalizer.rs:1024 symbol_of(el)         // list spread
normalizer.rs:1213 symbol_of(field_id)   // record field pat
```

The IR is symbol-based, so binders need stable ids that match their uses.

**What actually goes** is the *top-level* half of `NodeMap` —
`value_binds`, `type_binds`, `module_binds`, `module_type_binds` — because those
are derivable from `Definition.sym` in the surface and from orders carrying
`(sym, id)`. The seven local tables stay, and `ModuleView` keeps a `nodes` field
alongside `surface`. So the correct statement is: `NodeMap` shrinks to local
binders; it does not disappear.

Those remaining tables *could* go if the lowerer minted its own scope-stack
symbols (it already mints intermediates via `next_symbol`), collapsing seven
tables. That is lowerer work with its own risk and should be a separate step, not
part of the resolver rewrite. **Open point 7.**

## 4. Using `kola-relations` for read-only traversal

### 4.1 Where queries genuinely help

**Classifying a module expression** (spec §4.3's three reference categories):

```rust
expr.query()
    .cast::<node::ModuleMergeExpr>()
    .then(|merge| {
        (
            merge.project(|m| m.op),
            merge.project(|m| m.lhs),
            merge.project(|m| m.rhs),
        )
    })
```

**Validating structural assumptions** in resolver tests — same style as the
parser tests, without hand-written visitor boilerplate.

**Metadata joins** are the strongest fit. `Query::with::<M>()`,
`with_required::<M>()` and `left_join::<M>()` join a node to its side-table
metadata, which is exactly "walk every bind, get its resolved symbol":

```rust
binds
    .query()
    .with_required::<SymbolOf>()
    .each(|(bind, sym)| { /* ... */ });
```

Metadata should be **dense `SideVec` per tree, not `SideMap`**: ids are dense
within a kind in a per-module tree, so `SideVec<ValueBind, ValueSym>` is an
exact-fit array with no hashing and no sparse-map overhead. `NodeMap` should be a
`#[derive(RelationStorage)]` struct of such fields, not the deleted
`define_side_table!` output, and not one map keyed across all modules.

### 4.2 Where queries must not be used

Discovery's **expression** half must stay a hand-written `Visitor`:

- it mutates lexical scope (`LinearScope::enter`/`exit`), which plans cannot do;
- it creates symbols, so it is a generator, not a read;
- failures are diagnostics with locations, not assertion panics. Plans are
  `#[track_caller]`-and-panic; a resolver must recover and continue.

Rule of thumb: **queries read, the visitor writes.** Top-level declaration
collection is neither — it is a plain `for` loop over a per-kind vec (§3.4).

### 4.3 The tuple-context caveat

`(A, B)` tuples implement `PartialRelation` by delegating to the *remaining*
fields (`relation.rs:524-560`), while `TotalRelation` and `Allocate` delegate to
the *first* field. That overlay convention is useful — `(&tree, &node_map)` gives
AST structure plus metadata joins — but it has two consequences:

- `TotalRelation` lookups resolve against the first element only, so the thing
  you query *through* must be first;
- a metadata join (`with::<M>()`) must find `M` in the second or later slot.

Once the context exceeds two members, use a named struct with
`#[derive(RelationStorage)]` rather than a bare tuple, so the layering is explicit
and the code does not depend on positional convention.

## 5. Tree ownership

`SourceTrees { root: ModuleKey, trees: IndexMap<ModuleKey, Tree<Loc>> }` is the
parser output and the right input shape for the resolver too.

- **One `ModuleSym` per `ModuleKey`**, not per source file: a file can hold
  several inline modules, and v1 composes module *surfaces*, so identity follows
  the module.
- **`Id<T>` never crosses a tree boundary.** Only `ModuleKey`/`ModuleSym` crosses.
  `ModuleRef(ModuleKey)` is the bridge node in the parent tree.
- **`Loc` already carries `SourceId`**, and spans live in `Tree<Loc>`. There is no
  parallel `LocMap`/`LocVec`. A symbol's location is
  `Definition.site = (ModuleSym, AnyId)` → `tree.get_span(id)`.

### 5.1 One translation, then one key space

`ModuleKey` and `ModuleSym` are two number spaces (per-source vs program-wide).
Translate **once**, and never thread both:

```rust
module_of: HashMap<(SourceId, ModuleKey), ModuleSym>
```

After discovery, everything is a `ModuleSym`. Nothing downstream — composition,
orders, `Db`, consumers — sees a `ModuleKey`.

## 6. Printing

### 6.1 Current state

Per-module trees already exist, so the tree *is* per module. What does not exist
is per-module **rendering**. The facts:

- `TreePrinter<'a, M>` holds one `tree: &'a Tree<M>` (`print.rs:52-58`), and the
  blanket `Notate` impls require `Tree<M>: TotalRelation<T,T> + TotalSpan<T,M>`
  (`print.rs:83-109`) — one tree per render.
- All five call sites pass one tree: `driver:100,159,223`, `typer/check.rs:208`,
  `resolver/resolve.rs:285`.
- The driver is stale twice over: `TreePrinter::new` takes four arguments and it
  passes three, and `view.tree_map[&source_id]` now yields `SourceTrees`, not
  `Tree`. Both are compile errors once the resolver builds, so neither is silent.

### 6.2 Two gaps

**Nested inline bodies print as `moduleN`.** `ModuleExpr::Body(Id<ModuleRef>)`
prints the raw key (`node/module.rs:231-234`), and the parser splits nested bodies
into their own trees, so a parent renders `module m = module3`. This is today's
behaviour, not a regression — but it is why "one tree per module" *feels* like it
broke printing.

**Composed modules have no tree.** `module c = a with b` produces a `ModuleSym`
in `module_order`, so `db.all_modules()` will try to print it and no tree is
"c's".

### 6.3 The fundamental constraint

`Id<T>` is a per-kind dense index *within one tree*, not a global address. No
`Notate` impl can resolve an `Id` without knowing its tree, so a printer that
walks across trees is impossible as currently typed — it needs a composite
address `(ModuleKey, AnyId)`. That, not the absence of a tree, is the real reason
composed modules cannot be walked.

### 6.4 Resolution

**Render from a node, not from a tree root.** `TreePrinter` gains
`render_subtree(id)` alongside `render(root_id())`, and callers pass a
declaration site instead of assuming the root:

- a file root is `ModuleBody`;
- a named module — composed or not — is its `ModuleBind`, which lives in the
  *enclosing* module's tree, so `c` renders as `module c = a with b` with no
  fabricated tree. `Definition.site` already gives the `(ModuleSym, AnyId)` pair;
- nested inline bodies still need the composite-address mode to be expanded
  inline; v1 keeps the current `moduleN` output, which is at least unambiguous.

**Composed surfaces get a listing, not syntax.** For `--show-members` (and for
composed modules generally) print `ModuleSurface` as a table — key, visibility,
origin module. That is a projection over the sorted vec, not a tree walk, and it
is honest that a composed module has no single source span.

Consequently `ModuleView` exposes `declaration_site() -> (ModuleSym, AnyId)` and
`tree()` only for the tree its syntax lives in — for a composed module, the
enclosing tree. There is no invented `ModuleSource` abstraction.

## 7. Phases in detail

### Phase A — Port to the new tree (mechanical, no semantic change)

1. Replace `TreeMap`/`LocMap`/`LocVec` with `SourceTrees`/`Tree<Loc>`.
2. Replace `kola_tree::id::Id` with `kola_relations::Id`.
3. Replace `query::GetOpt`/`Get` with `PartialRelation`/`TotalRelation`, or
   `SideVec`/`SideMap`.
4. Replace `define_side_table!(NodeMap {..})` with a `#[derive(RelationStorage)]`
   struct of dense `SideVec` fields.
5. Fix `TreePrinter::new` arity, `kola_syntax::loc` imports, and the driver's
   `tree_map[&source_id]` uses.
6. Delete all functor code (`FunctorSym`, `FunctorDef`, `FunctorMap`, `Functor`,
   `ElabFunctorAppl`, `NameMap::functors`, `DefMap::functors`, `Db::functor`,
   `ModuleView::functor_node`, `NameCollision::FunctorBind`,
   `ResolutionDecorator`'s functor arm).
7. Delete the functor's substitution machinery with it — `kola-subst` drops out of
   `Cargo.toml`.
8. Delete `Sym::level`/`LEVEL`/`enter`/`exit`/`branch`.

Verification: workspace compiles, existing behaviour preserved.

### Phase B — Three states instead of two

Introduce `ModuleSurface` (§3.5). `ValueBind.value` and `ModuleBind.value` are
already `Option`, so discovery stops assuming a body:

- a bind with a body → `defined`;
- a typed bind with no body → `obligations`, and **not** `defined`;
- a bind with neither → already a parser-level recovered error, so unreachable.

### Phase C — Composition

Replace `elaborate/` with `compose/`, resolving module expressions in topological
order:

```text
import(file)        -> module_of[(source_id, root_key)]
body(ModuleRef)     -> module_of[(source_id, key)]
path(a::b)          -> walk surfaces, visibility-checked
merge(lhs, op, rhs) -> compose_{with,over,using}
```

Errors are structural: duplicate definitions under `with`, non-exported access,
cyclic module dependencies, outstanding members at the entry point.

### Phase D — Orders, output, and the `DefMap` removal

- collect all top-level names per module **before** visiting expressions, so
  intra-module references resolve without a queue;
- per-module `ValueGraph`/`TypeGraph`/`ModuleTypeGraph`;
- orders become `Vec<(Sym, Id<Bind>)>`, carrying the node id;
- module order from the composition DAG;
- `Db` carries `SourceTrees`, `module_of`, `surfaces`, the graphs and orders, and
  `entry_points`. No `DefMap`, no `DefinitionSite`, no `loc_map`.

`ModuleView` becomes `sym`, `is_root`, `declaration_site()`, `tree()`,
`&ModuleSurface`, and the three orders.

## 8. Consumer contract

The typer uses exactly this today:

```text
db.all_modules()      db.modules        db.entry_points
view.sym              view.module       view.defs
view.tree_map         view.value_order  view.type_order
view.value_node(sym)  view.type_node(sym)
```

Mapping:

| today | new |
| --- | --- |
| `view.tree_map` | `view.tree()` — fixes the inline-module bug |
| `view.defs[sym].1` | `surface` lookup → `Definition.site` → `tree.get_span(id)` |
| `view.value_node(sym)` | order already yields `(sym, id)`, so no lookup |
| `view.type_node(sym)` | same |
| `view.module.nodes` | **stays** — local binder symbols, used by typer and lowerer |

`Typer::new(.., spans, ..)` keeps its shape; spans now come from the tree.

The lowerer is a second consumer with the same shape (`module.rs:191-209`,
`normalizer.rs:187-1213`), so both `ModuleView::value_order` and
`ModuleView::module.nodes` must survive the port. Only the module-level `defs`
lookup is removed.

## 9. Complexity

Let `N` = AST nodes, `M` = modules, `k` = keys in one module namespace, `V`/`E`
= order-graph nodes/edges, `d` = lexical scope depth, `L` = composition links.

| phase | cost | note |
| --- | --- | --- |
| linear scan, per tree | `O(N)` | per-kind vecs, no hashing |
| surface insert | `O(k log k)` | sorted `Vec` (or `O(k)` into a `HashMap`) |
| expression visit | **`O(N · d)`** | ⚠ see below |
| all composition | `O(Σ(k_a + k_b))` | sorted zip, Σ over the DAG |
| `using` | `+O(|S|)` | forwarders |
| orders | `O(V + E)` per module | topological sort |
| **total** | **`O(N·d + Σk log k + V + E)`** | `d` dominates |

**The hotspot is `d`, and it is pre-existing.** `LinearScope` wraps `StackMap`,
which is a `Vec<(K, V)>` with **linear** lookups
(`stack_map.rs:22-24`, `:30-33`). So a scope lookup is `O(d)` and a deeply nested
body is `O(N·d)` — quadratic in the worst case. The typer's
`LocalTypeEnv = LinearScope<StrKey, MonoType>` (`types/env.rs:23`) has the same
shape, so this design neither introduces nor fixes it. `kola-collections` already
has `ShadowMap` for `O(1)`-amortized shadowing lookups if it turns out to matter;
it should be a separate change, not part of the resolver rewrite.

Two smaller notes:

- **The global `GENERATOR` is shared across namespaces** (`symbol.rs:30`), so ids
  are program-unique. That makes `Vec<Option<_>>` indexed by id a valid
  alternative to any `HashMap<AnySym, _>`. It also means the old
  `vec![max_sym; max_sym + 1]` rank array in `resolve.rs` is sized by total
  symbol count, not module count — replace with `HashMap<ModuleSym, usize>` or
  drop it once lookups order by `module_order` directly.
- **`SourceId = PathKey`** (`span/source.rs:9`), a global interner key, so it is
  dense but global. `files: SourceId → ModuleSym` stays a map; a `Vec` would be
  sized by every interned path in the program.

## 10. Migration checklist

- [ ] Drop `kola-subst` from `kola-resolver`; delete the 31 `Substitutable` impls.
- [ ] Delete `Functor*` symbols/defs/env/jobs/lookups/decorator arm.
- [ ] Delete `Sym::level`, `LEVEL`, `enter`, `exit`, `branch`.
- [ ] `Db` holds `SourceTrees` + `module_of`; delete `DefMap`, `DefinitionSite`,
      `LocMap`/`LocVec`.
- [ ] Dense `SideVec` metadata via `#[derive(RelationStorage)]`; `NodeMap` shrinks
      to the seven local-binder tables (top-level binds move to surface + orders).
- [ ] Collect top-level names before visiting expressions; queue only cross-module refs.
- [ ] `ModuleSurface` = sorted `defined` + `obligations`, no `privates`.
- [ ] `compose/` replacing `elaborate/`; three sorted-zip operators.
- [ ] Entry-point completeness check.
- [ ] Orders carry `(sym, id)`.
- [ ] `TreePrinter::render_subtree`; callers pass declaration sites.
- [ ] Re-point the four `typer.rs` `defs` sites; update lowerer call sites.
- [ ] Fix `kola-ls` (`LexInput`, `tokenize` arity) — independent of this work but
      blocking a clean workspace build.

## 11. Decisions and open points

**Decided:**

- One `ModuleSym` per `ModuleKey`, not per source file — one translation
  (`module_of`), then a single key space.
- Composition is a separate topological pass, not a job worklist.
- `kola-subst` leaves the resolver; `Sym::level` and its atomics are deleted.
- Top-level declaration collection is a linear scan over per-kind vecs, run
  **before** expression visiting, which removes intra-module forward references.
  The visitor is retained only for expressions and lexical scopes.
- Local value symbols stay: the lowerer uses them as IR identity (§3.8).
- Metadata is dense `SideVec` per tree.
- `DefMap` and `DefinitionSite` are both deleted; orders carry node ids and
  `Definition.site` carries the location. `Definition.origin` stays because
  forwarders need an identity distinct from their fresh symbol.
- Surfaces are flat sorted `Vec`s, normalized and deduplicated, composed by
  sorted zip — modelled on the runtime's record merge but deliberately not
  sharing its arena or its shadowed-duplicate spine.
- A structural `Merge(l, r) | Single(m)` module is not adopted: obligations are
  not the obstacle, but deferring conflicts and repeated enumeration are.
- No copy-on-write or splicing for composition: a composed module is a view over
  surfaces.
- Printing renders from a declaration site; composed modules get a surface
  listing, not syntax.
- `using` elaborates to ordinary forwarder definitions, so downstream code never
  special-cases it.

**Open (deliberately deferred, per spec §12):**

1. **Where module-type conformance lives.** The resolver can check key sets and
   kinds; value type equality is the typer's. Suggested split: resolver verifies
   structural shape and namespace, typer verifies value/type compatibility.
2. **`super::` in v1.** Specified but with no configuration use case. If kept it
   needs an acyclicity rule; if dropped, one fewer edge kind.
3. **Inferred vs declared obligations.** Discovery records only *declared*
   abstract members. Whether an inferred-use requirement should also create an
   obligation is a parser/spec question — currently a bodyless untyped bind is a
   recovered parse error.
4. **Incremental resolution.** The global `AtomicU32` generator and the
   whole-program `Db` are not obviously incremental. The language server will
   need invalidation boundaries; this design does not provide them.
5. **Partial results on error.** The resolver returns a partially populated `Db`
   today. The new model should state which parts remain usable after each error
   class, so the LS never reports nonsense from a half-composed surface.
6. **Inline expansion of nested module bodies.** Requires the composite
   `(ModuleKey, AnyId)` printer mode; deferred, current output is `moduleN`.
7. **Lowerer-minted local symbols.** If the lowerer owns its scope stack, the
   seven local `NodeMap` tables disappear. Separate step, with its own risk.
8. **Scope lookup cost.** `LinearScope`→`StackMap` is linear per lookup, making
   deeply nested bodies `O(N·d)`. `ShadowMap` exists; not bundled here.

## 12. Summary

The old resolver's three-stage shape (discover → elaborate → lookup) is sound;
what changes is the middle stage's content and everything downstream of the
symbol model.

- **Discover** is mostly a linear scan: one tree per module means per-kind vecs
  answer every top-level question directly, and running the scan before visiting
  bodies removes intra-module forward references entirely. The visitor survives
  only for expressions.
- **Compose** is three pure sorted-zip operators over flat surfaces, run once in
  topological order. Functor substitution and the blocked-job worklist disappear,
  and `NodeMap` shrinks to local binders.
- **Resolve** shrinks to cross-module references, then topological ordering.
- **Identity lookup** needs no map: orders carry node ids and surfaces carry
  definition sites, so `DefMap` and `DefinitionSite` both go.
- **Queries** earn their place in structural inspection, resolver tests, and
  dense metadata joins — not in scope mutation.
- **Printing** renders from a declaration site, so composed modules need no tree
  of their own.

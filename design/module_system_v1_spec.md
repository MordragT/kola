---
title: "Module System — v1 Design Specification"
subtitle: "A compile-time, virtual-by-default module calculus for a total configuration language"
date: "10 November 2026"
---

> **Status:** Proposed v1 specification, intended to be implementable. The core semantic choices below are treated as decisions for v1; narrowly scoped syntax and future extensions are identified explicitly. This document describes a design, not an implementation that has already been validated.

# 1. Executive summary

The module system has **three public member states**: absent, abstract (declared but not defined), and defined. Public definitions are always **virtual**: composition may replace them and their ordinary public dependencies are late-bound against the resulting module. There are **no special default or frozen provisions**.

Private definitions are **lexically bound and non-virtual**, but are **not frozen**: references from a private definition to a public member still resolve against the final composed module. A private name is represented by an internal identity, so equal private spellings from different source modules do not conflict.

Three operations are supported:

- `with` merges compatible modules symmetrically. It rejects two different public definitions of the same name, but matches abstract declarations with definitions.
- `over` merges modules with **left-hand precedence** on competing public definitions. It also matches abstract declarations with definitions.
- `using` selectively satisfies **currently outstanding** abstract members of the left module by constructing forwarding definitions that select corresponding members from a **complete** right-hand provider. It can be derived from `with` and qualified selection.

Modules are compile-time constructs, not runtime objects. A module with outstanding abstract members cannot be selected from externally or materialized as a value. Abstract members may nevertheless be referenced symbolically *inside* an open module under their declared types/signatures. Resolution is name- and identity-aware but not type-aware; a later type checker validates open modules and final compositions.

# 2. Goals, invariants, and scope

## 2.1 Goals

1. Keep the surface language small and compatible with its existing typed declaration syntax.
2. Allow symmetric composition, predictable left-biased overriding, and selective injection of dependencies.
3. Preserve late binding through arbitrary composition, including through private helpers.
4. Make missing inputs explicit; a definition cannot silently depend on an absent member.
5. Separate name resolution from type checking; preserve the obligations needed by the latter.
6. Require no runtime module-object machinery; resolve module expressions statically.
7. Avoid priorities, fallback slots, and frozen public members.

**Non-goal — first-order restriction.** Module types, requirement sets, and composition are strictly first-order in v1. Higher-order functors and higher-order module unification are forbidden rather than merely unimplemented; this constraint is what keeps §12.3.4 from expanding the core model.

## 2.2 Normative invariants

**I1 — Public means virtual.** Every public provision may be selected as the winner of a later `over`. No public `final`/`frozen` modifier exists in v1.

**I2 — Private means lexical, not frozen.** A private binding cannot be replaced by name across module boundaries; its references to public members remain late-bound.

**I3 — Abstraction is a commitment.** An abstract declaration persists as an interface/type obligation after a definition is supplied. The outstanding *status* can disappear; its declared constraint does not.

**I4 — Qualified external selection is anchored.** `base::x` refers to `x` in the independently resolved `base`, not to the eventual receiver of an `over` operation.

**I5 — No implicit absent references.** A source-module definition may refer to a public name only if that source module declares or defines that name, or the name is explicitly qualified or lexically inherited.

**I6 — No implicit type-directed resolution.** Operators match names within namespaces and preserve type constraints. They do not inspect type equality to choose definitions.

**I7 — Compile-time modules.** Modules are not value-level records; only projections of complete modules are available to the term/type language.

## 2.3 Four namespaces

A public key is a pair `(namespace, source_name)`. V1 retains distinct namespaces for **values**, **types**, **submodules**, and **module types**. The same spelling in different namespaces is not a name collision. All public matching below is performed by key, not spelling alone.

Private *type* declarations are excluded from v1. Private value bindings are specified fully; private helper submodules may use the same lexical-identity principle once their surface syntax is finalized. The details of higher-order functors and module-type elaboration are outside this document; they must respect these namespace and composition rules.

# 3. Surface language

## 3.1 Core forms

The following examples use commas in module literals, as in the existing language. `Type`, `Kind`, and `Signature` stand for existing type/signature syntax; the signature grammar is not changed by this proposal.

```kl
module server = {
    port : Num,                       # abstract public value
    host = "localhost",                # virtual public provision
    private separator = ":",           # lexical private definition
    address = host ++ separator ++ show port,
}

module defaults = { port = 5000 }
module configured = server using defaults
module deployed = { host = "service.internal" } over configured
```

Public declarations are public by default, so `export` is removed. A typed declaration without a body denotes an abstract member; a typed declaration with a body denotes a provision whose type annotation belongs to that definition.

| Form | Interpretation |
|---|---|
| `x : T` | Abstract public value with a persistent obligation `x : T` |
| `x : T = e` | Public value provision with declared type `T` |
| `x = e` | Public value provision with an inferred type |
| `private x = e` | Private lexical value definition |
| `type T = τ` | Public virtual type provision/alias |
| `type T : K` | Abstract public type of kind `K` (proposed explicit form) |
| `module m = M` | Public submodule provision |
| `module m : S` | Abstract public submodule with signature `S` (proposed form) |

The exact punctuation for abstract types, abstract submodules, and module types can be polished alongside the existing parser; their *semantic* status as abstract names is fixed here. When the spelling is settled, put the namespace marker in the same prefix position used elsewhere (`module type S`), i.e. `type T : Kind` and `module m : S`, rather than encoding the namespace as a type expression. No keyword such as `require` or `default` is needed for ordinary abstract/concrete values.

## 3.2 Scope and duplicate declarations

Within one module literal and one namespace, a public and private declaration cannot share a name. Two private declarations of the same name in that scope are likewise invalid. Across **different** modules, identical private spellings are legal, including when one module has a public member with the same spelling.

An unqualified reference inside a source module binds to its own private declaration if one exists; otherwise it denotes a public member declared or defined in that source module. A reference to an undeclared public member is an error, even if a future `with` or `over` might coincidentally introduce it. Explicit abstract declarations are how such dependencies are declared.

## 3.3 Operator direction and precedence

`A over B` means **A wins over B** for overlapping public definitions. `A with B` is symmetric. `A using B` forwards from B into outstanding public requirements of A. Parenthesize mixed operators in v1; no cross-operator precedence should be inferred from the examples.

# 4. Static representation

## 4.1 Module structure

A resolved symbolic module is represented by

\[
  M = (Q, D, H),
\]

where:

- `Q : Key → finite set of Obligation` stores **all source abstract declarations** and their required types, kinds, or signatures, identified by declaration identity. The set is retained even after satisfaction.
- `D : Key ⇀ Definition` stores the currently **winning public definitions**. Each definition has a stable origin identity, an expression/body, an optional attached concrete annotation, and provenance.
- `H : PrivateId ⇀ Definition` stores private definitions keyed by **lexical identity**, not source spelling. Its references to public members are symbolic.

A public name is **outstanding** precisely when it has an obligation but has no current definition:

\[
  \mathrm{Outstanding}(M)
    = \{k \mid Q_M(k) \neq \varnothing \ \land\ k \notin \mathrm{dom}(D_M)\}.
\]

A module is **complete** iff `Outstanding(M) = ∅`. A complete module remains virtual and overridable.

The extra distinction between `Q` and `D` is important. If an abstract `port : Num` is satisfied by `port = "hello"`, the resolver records a provided `port` but retains the `Num` obligation so the type checker can reject it.

## 4.2 Definition identity and alpha-equivalence

Definitions have stable identities within a named module. Reusing the *same* named module reuses its public definition identities; independently written equal bodies have distinct identities. Private identities are also stable and unique. An implementation may allocate unique symbol IDs; no textual name mangling is necessary.

Modules differing only by consistent renaming of internal private IDs are semantically equivalent. Algebraic equalities below are understood up to such alpha-equivalence and the permitted reordering of independent declarations.

## 4.3 Three reference categories

After lexical resolution, expression references are categorized as:

- `Self(k)`: reference to a public member of the **final receiver** of the definition. It is late-bound.
- `Local(α)`: reference to the private definition with identity `α`. It is lexically bound; that definition may itself contain `Self` references.
- `External(N, k)`: reference to member `k` of the independently named/resolved module `N`. Its module environment is anchored to `N`.

`super::x` is a fourth syntactic form of a **lexically anchored parent-module reference**, not a synonym for any previous override layer. Its target must be well-founded in the existing module dependency order.

# 5. Core operator semantics

All operator rules are applied **independently per public key**. Identity and public-name matching occur before type checking. Malformed types, incompatible signatures, and non-terminating dependency graphs are rejected in later validation, not silently resolved by these operators.

## 5.1 `with`: symmetric compatible union

For `C = A with B`:

\[
Q_C(k)=Q_A(k)\cup Q_B(k).
\]

Definitions combine with the following partial merge operation `⊔`:

| `D_A(k)` | `D_B(k)` | `D_C(k)` |
|---|---|---|
| absent | absent | absent |
| definition `d` | absent | `d` |
| absent | definition `d` | `d` |
| definition `d` | same identity `d` | `d` |
| definition `d₁` | distinct identity `d₂` | **name conflict** |

An abstract declaration is represented in `Q` rather than `D`; therefore abstract+abstract remains outstanding, while abstract+definition is satisfied automatically and retains its abstract obligation in `Q`.

The candidate private store is the identity-disjoint union `H_A ⊎ H_B`. Only private definitions reachable from winning public definitions need to be retained in a normalized composed module; the same semantics can be implemented by retaining all candidates with provenance and pruning later.

### Example

```kl
module a = {
    port : Num,
    host = "localhost:" ++ show port,
}
module b = { port = 5000 }
module c = a with b   # complete; c::host = "localhost:5000"
```

Two independent `port = 5000` definitions still conflict under `with`; structural or value equality is not used to excuse duplicates.

## 5.2 `over`: left-preferential union

For `C = A over B`:

\[
Q_C(k)=Q_A(k)\cup Q_B(k),
\]

\[
D_C(k)=\begin{cases}
D_A(k),&k\in\mathrm{dom}(D_A),\\
D_B(k),&\text{otherwise.}
\end{cases}
\]

Thus a **definition** on the left replaces a definition on the right; an **abstract declaration** on the left does not mask a concrete definition on the right. Both sides can introduce new public keys. The result's outstanding set is recomputed from `Q_C` and `D_C`.

Private definitions retain lexical identity, subject to reachability from the surviving public definitions. `over` neither accesses nor replaces private definitions by their source spelling.

```kl
module base = {
    version = "1.0",
    source = fetch_source version,
}
module updated = { version = "2.0" } over base
# updated::source uses version "2.0".
```

The same rule permits incomplete composition:

```kl
module base = {
    compiler : Compiler,
    flags = ["-O2"],
    command = compiler::compile flags,
}
module debug = { flags = ["-O0", "-g"] } over base
# debug remains incomplete until compiler is supplied.
```

### Abstract obligations survive replacement

```kl
module required = { port : Num }
module wrong = { port = "bad" }
module combined = wrong over required
# Name resolution succeeds; type checking rejects the Num obligation.
```

In contrast, a concrete annotation on a **discarded** definition does not constrain the winning replacement (v1 loose rule). The original named module is still checked independently.

## 5.3 Laws and their conditions

Let `∅` be the empty module. On symbolic, well-formed operands, ignoring private-ID renamings:

\[
 A\ \mathrm{with}\ B = B\ \mathrm{with}\ A,
\]
\[
 (A\ \mathrm{with}\ B)\ \mathrm{with}\ C = A\ \mathrm{with}\ (B\ \mathrm{with}\ C)
\]

whenever all involved `with` merges are defined, and

\[
 A\ \mathrm{with}\ A = A.
\]

For `over`, the left-preferential definition union is associative and idempotent:

\[
 (A\ \mathrm{over}\ B)\ \mathrm{over}\ C = A\ \mathrm{over}\ (B\ \mathrm{over}\ C),
\]
\[
 A\ \mathrm{over}\ A = A.
\]

Both have `∅` as a two-sided identity; `over` is **not** commutative. There is no general distributivity claim between `with` and `over`.

These are **symbolic composition laws**. A named intermediate module can still be rejected for an independent type error or cycle. To avoid order-dependent false rejections, normalize an entire composition expression before checking its final effective definitions; separately check every named source module at its own declaration boundary.

# 6. Private binding semantics

## 6.1 Lexical identity, virtual public dependencies

```kl
module a = {
    suffix : String,
    private prefix = "A-",
    label = prefix ++ suffix,
}
module b = {
    private prefix = "B-",
    suffix = "hello",
    description = prefix ++ suffix,
}
module c = a with b
# c::label       = "A-hello"
# c::description = "B-hello"
```

The two lexical `prefix` declarations have different private IDs and never conflict. Both occurrences of `suffix` are public symbolic references and resolve to `c.suffix`. Replacing `suffix` later with `over` changes both strings, but not the private prefixes.

## 6.2 Public/private collision across modules

```kl
module a = {
    private port = 5000,
    address = show port,
}
module b = { port = 8080 }
module c = a with b
# c::port = 8080; c::address = "5000"
```

This is legal. Within one source module, the same-name private/public declaration in the same namespace would be rejected.

## 6.3 Reachability and independent checking

When `over` discards a public definition, private definitions used **only** by the discarded definition need not appear in the composed module. However:

- Every named source module must type-check independently, including its otherwise dead private definitions.
- Abstract declarations remain in `Q`, even if the code that originally referenced them has been overridden. An explicit requirement is an interface commitment, not merely an inferred dataflow dependency.
- Reachability is transitive through `Local` edges; `Self` references are resolved through the winning public definition map, not rewritten into private bindings.
- Pruning must not affect definition identities, diagnostics for source modules, or the observable result of composition.

This avoids letting an overridden implementation leave an irrelevant private type error in its replacement while still rejecting invalid source modules.

# 7. `using`: selective forwarding as a derived operator

## 7.1 Intent

`A using B` examines the **currently outstanding** public keys of A, finds corresponding provisions of B, and introduces forwarding definitions for those keys only. It does **not** import B's public dependency closure or private implementation details.

For v1, B must be a **complete** module in the ordinary selection sense. If B lacks a name required by A, the operation may be partial: the unmatched requirement remains outstanding in the result. Wrong-kind or wrong-type matches are validated by the later type checker.

## 7.2 Definition

Let

\[
S = \mathrm{Outstanding}(A)\cap\mathrm{dom}(D_B).
\]

Define `Forward(S,B)` to be a synthetic module with one fresh public definition per key `k ∈ S`, whose body selects B's member in the appropriate namespace:

\[
 D_{\mathrm{Forward}(S,B)}(k) = \mathrm{External}(B,k).
\]

Then

\[
\boxed{A\ \mathrm{using}\ B\ \equiv\ A\ \mathrm{with}\ \mathrm{Forward}(S,B)}.
\]

`Outstanding` and `Forward` are **compiler-internal elaboration functions**, not user-accessible reflection APIs. Forwarders are public and virtual just like other provisions. Synthetic definitions have stable fresh identities attached to their elaborated expression.

## 7.3 Example and binding boundary

```kl
module app = {
    flags : List String,
    command = compile flags,
}
module defaults = {
    optimization = "O2",
    flags = ["-" ++ optimization],
}
module configured = app using defaults
```

The operation behaves like:

```kl
module configured = app with {
    flags = defaults::flags,
}
```

`configured` exposes `flags` and `command`, not `optimization`. The original provider remains independent:

```kl
module changed = { flags = ["-O0"] } over configured
# changed::command sees ["-O0"].

module changed_provider = { optimization = "O0" } over defaults
module configured_again = app using changed_provider
# configured_again::flags = ["-O0"].
```

A new `optimization` introduced on top of `configured` would **not** change `defaults::flags`: the forwarder is intentionally anchored to the complete provider. To customize provider dependencies, compose the provider **before** using it.

This is a semantic distinction between the operators, not a violation of late binding:

- `with` and `over` inherit ordinary definitions into a **shared receiver**.
- `using` generates definitions that select from an **independent, anchored provider**.

## 7.4 Partial and repeated linking

```kl
module app = { port : Num, host : String, url = host ++ show port }
module config = { port = 8080, host = "localhost", extra = true }
module half = app with { port = 9000 }
module final = half using config
# Only host is forwarded; port remains 9000 and extra is not exported.
```

A second `using` sees the **residual** outstanding set, not the original syntax of the first module. A provider with no matching names produces no forwarded definitions and leaves outstanding requirements unchanged; implementations may warn about this likely mistake.

## 7.5 Other namespaces

Forwarding is namespace-preserving. A required value forwards a value expression; an abstract type forwards an anchored type alias; an abstract submodule forwards a module selection; and module types follow their established signature rules. The resolver does not use type compatibility to decide which names match. For module-valued requirements, the selected provider submodule must itself be complete in v1.

# 8. Selection, lexical parents, and module boundaries

## 8.1 External selection of a complete module

For a named module M, external selection `M::x` is legal only if M has **no outstanding requirements** at the selected module boundary. This is deliberately a module-wide restriction, even if `x` itself does not depend on an outstanding requirement.

```kl
module incomplete = { port : Num, flags = ["-O2"] }
# incomplete::flags  -- rejected: incomplete is open.

module ready = incomplete with { port = 5000 }
# ready::flags       -- permitted.
```

Completeness does not freeze `ready.flags` or any other public member.

## 8.2 Internal use of abstract members

An open module **may** refer to its declared abstract members symbolically:

```kl
module lib = {
    backend : BackendSignature,
    load = fn path => backend::read path,
}
```

This is not value-level selection of an external incomplete module. `backend::read` is an internal projection justified by the declared signature. It is checked under that assumption, and is evaluated only after a complete backend is supplied. Without this distinction, module-valued abstract requirements would be unusable.

## 8.3 Named original module vs inherited definition

```kl
module package = {
    version = "1.0",
    flags = ["-O" ++ version],
    source = fetch_source version,
}
module customized = {
    version = "2.0",
    flags = package::flags ++ ["-g"],
} over package
```

`package::flags` is anchored and uses version `"1.0"`; the surviving inherited `source` definition is late-bound to `customized.version` and uses `"2.0"`. Consequently, the customized flags contain the old optimization plus `"-g"`.

The compiler must represent anchored qualified references differently from ordinary `Self` references. Ordinary `flags = flags ++ ["-g"]` is **not** an escape to the old implementation; it is a self-cycle and must be rejected.

## 8.4 `super` and acyclicity

`super::x` denotes the **lexical parent** of an inline module, not the module below an override. In v1 it follows the conservative external-selection rule: the lexical parent must be complete for selected members to be evaluated, and module dependencies must form a directed acyclic graph. A parent may not depend on a child in a way that creates a cycle. A later relaxation could permit qualified selection from a dependency-closed subset of an open parent, but that is not part of v1.

## 8.5 Entry point and file-level modules

A single `.kl` file is an implicit module literal: its top-level binds form the root `ModuleBody` of one module tree. It is **open** precisely when it declares abstract members. No marker at the file level is required, and openness is not visible at the import site.

The root module of the program is subject to an additional completeness rule:

\[
\mathrm{Outstanding}(\text{root}) = \varnothing.
\]

Outstanding abstract members at the entry point are an error. Non-root modules may remain open. This is the only place where an open module is rejected outright rather than merely being unusable.

The rule is stated for the entry point rather than for every module because §8.1's module-wide restriction already prevents an open module from being consumed. Combined with file-level modules, that restriction means a file declaring one abstract member is unselectable in its entirety, including for members that do not depend on that abstraction. If this proves too strict, the dependency-closure relaxation of §12.3.3 applies to selection only; the entry-point rule above is unaffected.

# 9. Type checking and validation

## 9.1 Two distinct phases

**Module resolution** performs namespace-aware symbol lookup, binds lexical private identities, computes the public definition winners, calculates outstanding keys, elaborates `using` forwarders, and preserves abstract obligations. It is **not** responsible for deciding type compatibility.

**Type checking** receives the resolved symbolic module(s). It validates source annotations, all persistent abstract obligations, kind/signature compatibility, references and bodies in the effective environment, and the types of resulting exposed members.

A resolved name with a chosen definition may have **multiple abstract obligations**, and all must be checked. Inconsistent abstract obligations are a type error even if the module is still open.

## 9.2 Open-module checking

An open module can be checked under assumptions for its abstract members. For example:

```kl
module x = {
    port : Num,
    doubled = port * 2,
}
```

The type checker assumes `port : Num` when checking `doubled`; it does not need a concrete value for `port`. A later composition supplying a value checks that definition against the preserved obligation.

## 9.3 Concrete annotations are not inherited contracts

V1 takes the **looser** rule decided in the design discussion:

```kl
module a = { value : Num = 42 }
module b = { value = "hello" }
module c = b over a
# The overridden Num annotation from a does not constrain c.value.
```

The named `a` is independently checked as `Num`. In `c`, its concrete definition and attached annotation are discarded, so `c.value` may be `String`. Any **surviving** definition that expects `Num`, or any independent abstract `value : Num` obligation, will still cause type checking to fail.

This is intentionally less strict than persistent public interface contracts. An explicit future `seal`/signature ascription operation could introduce such contracts without changing today's `over` semantics.

## 9.4 Public type provisions

Public type definitions are virtual. `over` may replace a public type alias, but the **final effective module** must be checked again because surviving value definitions may become ill-typed under the replacement. The resolver carries names, bodies, and annotations; the type checker resolves their actual type-level meaning.

Private type definitions are **out of scope in v1**. All types are structural/non-nominal in the current language; private type aliases can be reconsidered as a convenience feature if they prove useful.

## 9.5 Cycles and totality

Construct a dependency graph after resolving an effective module:

- `Self(x)` edges target the winning public definition for `x`.
- `Local(α)` edges target the specific private definition `α`.
- `External(N,x)` edges target the independently checked module N.
- Abstract members provide assumptions but are not executable definitions.

Reject strongly connected components that would introduce forbidden recursion; module-level resolution remains static and compatible with the language's totality requirement. Type-level cycles are checked under the language's existing type-alias rules. Every named source module is also independently checked, so an invalid source is not retroactively repaired merely by being overridden elsewhere.

# 10. Resolution algorithm (implementation outline)

1. **Parse and classify.** Assign `(namespace, name)` keys; recognize abstract declarations, public definitions, and private definitions. Reject same-scope duplicate names.
2. **Bind source-local references.** Resolve private names to unique lexical IDs. Resolve ordinary public names to `Self(key)` only if declared or defined in the source module; encode qualified module references as anchored targets. Preserve `super` targets.
3. **Build symbolic modules.** Initialize `Q`, `D`, and `H` for literals and previously named modules.
4. **Evaluate module expressions symbolically.** `with` uses compatible union; `over` chooses left public definitions. Never force public expression evaluation during composition.
5. **Elaborate `using`.** Confirm that the provider is complete; compute `Outstanding(left)`; construct fresh anchored forwarding definitions for the matching provider keys; merge via `with`.
6. **Normalize private reachability.** Retain the lexical private definitions needed by surviving public bodies, without changing their identities or their internal `Self` edges. Preserve `Q` regardless of reachability.
7. **Check structural well-formedness.** Report unresolved public names as outstanding, ensure qualified selection rules hold, and check the static dependency graph for cycles at the appropriate module boundaries.
8. **Type-check.** Check every named source module under abstract assumptions and every effective composed module under its final winning definitions. Verify all surviving concrete annotations and **all** abstract obligations.
9. **Expose completed projections.** Permit external `M::x` only for complete, validated modules. Compile away module constructs; emit only the selected type/value-level definitions.

Resolution can be implemented with immutable definition DAGs and shared symbol IDs; there is no requirement to copy or textual-rename every inherited body. The same origin definition may be instantiated under multiple receiver environments, so evaluation/type-check caches must include the receiver environment, not just the definition ID.

# 11. Semantic regression tests

These are specification-level tests; the snippets are schematic and should be adapted to the final parser and type grammar.

| Test | Input shape | Expected |
|---|---|---|
| Abstract+definition via `with` | `{x : Num} with {x = 2}` | Complete, `x = 2` |
| Abstract+definition via `over` | `{x : Num} over {x = 2}` | Complete, `x = 2` |
| Conflict under `with` | `{x = 1} with {x = 1}` (distinct origins) | Name conflict |
| Same-origin diamond | `a with a` | Valid, one definition identity |
| Left precedence | `{x = 2} over {x = 1}` | `x = 2` |
| Left abstract, right defined | `{x : Num} over {x = 2}` | `x = 2` |
| Right abstract, left defined | `{x = 2} over {x : Num}` | `x = 2` |
| Unmatched abstraction | `{x : Num} with {y = 1}` | Open, `x` outstanding |
| Requirement/type mismatch | `{x : Num} with {x = "bad"}` | Resolve, then type error |
| Two inconsistent obligations | `{x : Num} with {x : String}` | Type error |
| Private spelling collision | A.private `p`, B.private `p` | Valid, independent lexical IDs |
| Public/private across modules | A.private `p`, B.public `p` | Valid, no capture |
| Public/private in one literal | `private p`; `p = ...` | Declaration error |
| Private helper late-bound | private helper uses public `x`; override `x` | Helper sees replacement `x` |
| Original via qualification | patch uses `base::x` | Base's original composed value |
| Accidental self reference | `{x = x ++ suffix} over base` | Cycle error if `x` resolves to itself |
| `using` only outstanding | `A` already defines `x` | Provider does not replace `x` |
| `using` ignores extras | Provider offers `x,y`; A needs `x` | `y` not exported by result |
| `using` provider dependency | Forward `x = B::x` | Provider dependencies remain anchored to B |
| `using` incomplete provider | B has outstanding member | Error in v1 |
| External open selection | `Open::already_defined` | Error while Open remains incomplete |
| Internal open assumption | `x : Num; y = x * 2` | Type-check under abstract `x` |
| Concrete annotation replaced | B's `x : Num = 1`, A's `x = "hi"`; `A over B` | Allowed if no surviving Num obligation/use |
| Abstract commitment retained | Abstract `x` used only by overwritten body | `x` still outstanding |
| Public type override | Replace alias used by surviving member | Recheck; reject on mismatch |
| Private dead definition | Replaced public no longer references private | May prune in result; source still checked |
| Member cycle introduced by merge | `a` depends on `b`; `b` depends on `a` | Cycle error |
| Open entry point | Root module declares `x : Num` | Error: outstanding at entry point |
| File-level open module | File with one abstract member, imported | Module is open; §8.1 selection restriction applies |

# 12. Decisions, limits, and future work

## 12.1 V1 decisions

- No `require`, `slot`, `default`, `frozen`, `final`, or priority modifier in the core surface language.
- Public-by-default definitions are virtual. Private value definitions are lexical and non-virtual but retain late-bound public references.
- `with` is a partial commutative merge; `over` is left-biased and both satisfy matching abstract declarations.
- An abstract declaration contributes a persistent type/signature obligation; a replaced concrete annotation does not.
- `using` is defined as selective qualified forwarding from a complete provider, derived through `with`.
- External module selection requires completeness; internal abstract references are checked symbolically against signatures.
- Type compatibility is decided after symbolic resolution, not during definition selection.
- Private type aliases are omitted from v1.

## 12.2 Explicitly deferred or parser-level details

- Exact surface spelling for abstract type/module/module-type declarations, without changing their abstract-member semantics.
- Higher-order functors; using incomplete module-valued providers; propagating nested module requirements into enclosing scopes.
- Interface ascription, sealing, public frozen definitions, optional default priorities, or making concrete annotations persistent override contracts. A future `seal`/signature-ascription operation should be **destructive** (hide non-signature members, shed requirements, optionally abstract type components), not merely a conformance check; non-destructive conformance does not solve the requirement-restriction problem noted in §12.3.1.
- Renaming, hiding, restricting, and explicit public interface projection operators.
- Partial selection from an open named module when the selected member's dependency closure is independently complete.
- Optional private type abbreviations, and richer private nested-module syntax.
- Diagnostic policy for `using` when a provider supplies none of the requested keys (warning vs no-op).

## 12.3 Potential pressure points to revisit after implementation

1. **Requirement persistence.** Explicit abstract declarations remain commitments even if the definition that uses them is overridden. This is algebraically clean but may motivate a later interface-restriction operator.
2. **Anchoring in `using`.** The forwarded member remains virtual, yet its own provider dependencies do not rebind to the receiver. This is deliberate and should be taught clearly.
3. **Coarse completeness.** Requiring an entire module to be complete for qualified external selection is simple, but can be stricter than necessary.
4. **Open submodule signatures.** A future higher-order module system must define how nested requirements and virtual public definitions interact without weakening the v1 model.

# 13. Compact reference

| Construct | Purpose | Winner / binding behavior | Can leave module open? |
|---|---|---|---|
| `x : T` | Declare abstract public member | No implementation; persistent obligation | Yes |
| `x = e` | Provide virtual public member | Ordinary `Self` dependencies late-bound | No new requirement |
| `private x = e` | Provide lexical local helper | Private identity stable; public dependencies late-bound | No new requirement |
| `A with B` | Symmetric compatible merge | Distinct overlapping definitions conflict | Yes |
| `A over B` | Prefer A's public definitions | A wins; abstract declarations may be satisfied by either side | Yes |
| `A using B` | Supply A's outstanding names from B | Forwarded `B::x` is anchored; forwarded x itself remains virtual | Yes, if some requirements remain |
| `M::x` | Access completed named module | Anchored to M's own resolved environment | M must be complete |
| `super::x` | Access lexical parent | Anchored to parent; subject to acyclic dependency rules | Parent selection must be valid |

**Design principle:** *Public is virtual; private is lexical; abstract is missing; completeness permits selection; and qualification captures an independent module environment.*

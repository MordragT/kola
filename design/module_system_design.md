# Architecture Design Document: Compile-Time Module System

## 1. Overview & Core Philosophy

This document specifies the design for a strict, compile-time module system integrated into a Hindley-Milner typed configuration and package management language.

### Core Principles

* **Strict Static Evaluation**: Modules operate exclusively during compile-time elaboration. There is no runtime laziness or dynamic module loading.

* **First-Order Restriction**: Module types and requirements are strictly first-order to avoid higher-order functor unification complexity.

* **Separation from Records**: While modules share structural aesthetics with records, they are distinct AST entities (`ModuleExpr` vs `RecordExpr`).

* **Low Boilerplate Ergonomics**: Public-by-default export semantics, explicit `private` annotations for hidden members, and value-by-default requirement declarations.

## 2. Syntax & Scope Mechanics

### 2.1 Public-by-Default Semantics

All declarations within a module are exported by default. Hide bindings using the `private` keyword.

```
module option_utils = {
    type Option = forall a . < Some : a, None >,

    // Public by default
    is_some : forall a . Option a -> Bool = fn opt =>
        case opt
        | < Some : _ > => true
        | < None > => false,

    // Hidden internal helper
    private internal_check = fn opt => ...
}

```

### 2.2 File-Level Modules

A single `.kl` file acts as a implicit static module block. If a file contains any `require` statements, it functions as an open module (template/functor).

## 3. Requirement & Provision Mechanics (`require` / `provide`)

Modules declare unfulfilled dependencies using `require` statements. Requirements distinguish between **modules**, **types**, and **values**.

### 3.1 Syntax Definitions

| Kind | Requirement Syntax | Provision Syntax | 
| ----- | ----- | ----- | 
| **Value** (Default) | `require <name> : <Type>` | `provide <name> = <Expr>` | 
| **Type** | `require type <Name>` | `provide type <Name> = <TypeExpr>` | 
| **Module** | `require module <name> : <ModuleType>` | `provide module <name> = <ModuleExpr>` | 

### 3.2 Open Module Example

```
module base_service = {
    // Requirements
    require module logger : LoggerModule,
    require type Connection,
    require port : Num,                  // Value requirement (default kind)
    require host : Str,                  // Value requirement (default kind)

    // Derived export
    endpoint = concat host (show port),
    
    run = fn () => logger::log (concat "Listening on " endpoint)
}

```

## 4. Module Composition & Overriding (`//`)

Module scope extension, overriding, and requirement-closing use the non-commutative module composition operator (`//`).

### 4.1 Operator Semantics (`//` vs `&`)

* `&`: Reserved for **runtime structural record merging**.

* `//`: Reserved for **compile-time module extension and override propagation**.

`a // b` indicates that `b` shadows or satisfies requirements in `a`. It is left-biased with respect to shadowing.

### 4.2 Closing an Open Module

```
module prod_service = base_service // {
    provide module logger = json_logger,
    provide type Connection = TcpSocket,
    
    // Fulfilling value requirements
    provide port = 8080,
    provide host = "0.0.0.0",

    // Value update/overwrite (triggers deep re-evaluation)
    version = "1.2.0",

    // Adding a new module member
    environment = "production",
}

```

## 5. Structural Module Type Checking (`: Sig`)

Module signatures (`moduletype`) define baseline structural contracts. Subtyping/conformance checking via `: Sig` is **non-destructive validation** (it ensures required fields exist with valid types without stripping extra fields).

```
module type ServiceContract = {
    type Connection,
    export endpoint : Str,
    export run : Unit -> Unit,
}

// Conformance check on an open module template
module typed_service : ServiceContract = base_service

```

## 6. Lowering & Compilation Strategy

### 6.1 Function-Based Elaboration Pipeline

Open modules lower to compile-time functions over a row of requirement bindings:

```
[Open Module AST]  --->  compile-time lambda: (RequirementsRecord) => ConcreteModuleRecord

```

Instantiating an open module via `base // { provide ... }` corresponds to static application of arguments to the requirement lambda.

### 6.2 Prohibiting Open Module Projections

Field accesses on open modules (e.g., `open_mod::field`) are strictly prohibited during Phase 1. Fields can only be projected from fully closed modules.

## 7. Dependency Resolution & Cycle Detection

1. **Acyclic DAG Guarantee**: The compile-time elaboration engine builds a Directed Acyclic Graph (DAG) of module imports and requirement dependencies.

2. **Topological Ordering**: Modules are elaborated in reverse topological order. Cyclic dependencies trigger a compile-time error.

3. **Unfulfilled Requirement Errors**: If elaboration completes with unfulfilled `require` holes, a diagnostic error highlights missing provisions.

## 8. Future Work

* **Open Module Projections**: Extended static signature inference to permit reading fields from open modules prior to instantiation.

* **Module Path Aliasing**: Enhanced `use` / `import` syntax for granular selective imports across complex nested module hierarchies.
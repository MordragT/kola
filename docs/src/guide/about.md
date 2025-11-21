# About Kola

**Kola** is a statically and structurally typed configuration language, designed for safety, expressiveness, and predictability. Built on the simply-typed lambda calculus, Kola guarantees termination and offers advanced features for modern configuration needs.

## Key Features

- **Statically & Structurally Typed:** Robust static and structural typing with full type inference.
- **Guaranteed Termination:** All programs are guaranteed to terminate — no infinite loops, no surprises.
- **Algebraic Effects & Functors:** Model complex behaviors and modularize configuration logic.
- **Expressive & Composable:** Templating, parameterization, and advanced abstraction patterns are first-class citizens.
- **Strict Evaluation:** Predictable results and easier reasoning about configurations.
- **Precise Effect Control:** Fine-grained control over effects, with a system designed for safe extensibility (plugins coming soon).

## Use Cases

- **Package Management:**
  Define, compose, and manage package builds and dependencies in a safe, declarative, and reproducible way. Kola’s effect system allows you to model build steps and side effects precisely, while static typing ensures correctness.

- **System Configuration:**
  Express complex system configurations, templates, and policies with strong guarantees. Kola’s composability and type inference make it easy to create reusable modules for configuring servers, services, and infrastructure.

- **DevOps & Automation:**
  Automate deployment pipelines, CI/CD workflows, and infrastructure provisioning with confidence. Kola’s termination guarantee and effect system provide predictable, auditable automation scripts that are easy to reason about and maintain.

## Example

TODO: Add example that shows the most important features of Kola.

```kola
{
    # Helper
    add_one = fn x => x + 1,
    multiply = fn x => fn y => x * y,

    # Conditional
    max = fn a => fn b =>
        if a > b then a else b,

    # More complex expressions
    compute = fn n =>
        let doubled = n * 2 in
        let incremented = (add_one doubled) in
        let multiplier = (multiply 3) in
        (multiplier incremented),

    # Nested if
    categorize = fn score =>
        if score >= 90 then "A"
        else if score >= 80 then "B"
        else if score >= 70 then "C"
        else "F",

    export module formula = {
        # Complex arithmetic
        export formula = fn x => fn y =>
            let sum = x + y in
            let diff = x - y in
            let product = sum * diff in
            product / 2,
    },

    # Entry point
    main =
        let test_num = 15 in                                    # 15
        let result1 = (compute test_num) in                     # 93
        let result2 = ((max result1) 100) in                    # 100
        let grade = (categorize 85) in                          # "B"
        let final_calc = ((formula::formula result2) 50) in     # 3750
        final_calc + 1000,                                      # 4750
}
```

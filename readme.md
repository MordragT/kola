# Kola - Konfiguration Language

## Overview

- Row Polymorphism
- Let Polymorphism
- Strict Evaluation
- Static Type-System with Inference

### Abstract complex configurations

- Using Row Polymorphism many problems can
    be generically solved

### Visible Side-Effects of "Platform"

- Platforms are plugins given to the Compiler,
    which define Functionality with Side-Effects

- Invoking these will give a special effect propagated
- TODO only allow in application code ??
- TODO does separation of library and application code make sense ?

- Useful for interaction with 'effectful' configurations like getting Secrets or generating files for build systems


### Separation of Concerns

- For e.g. Test's one might want to imiate effectful operations (Mocking)
- Algebraic Effects with their custom handlers
  can promote code reuse

### Configuration Drift

- One source of truth to not have to change every component

### Static Checks for Partial Functions

https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)

- generating configuration should always terminate
- but allowing recursion makes it possible for programmers to implement new generic functionalities (especially in library code)

TODO: Ist das machbar ?

Simply Typed Lambda Calculus: Not Turing Complete

Let-Polymorphism: Intuitively I don't think introduces Turing Completness but I have to check

Row-Polymorphism: Same as Let-Polymorphism

Algebraic Effects: Does introduce Turing Completness:

```
effect next(n) : Num -> Num

let fib(n) = next(n -1) + next(n - 2) in

# TODO assuming that the defined handler does not propagate effects
handle fib(5) with {
  next(n) => if n <= 1 resume(n) else resume(fib(n))
}
```

But we can statically check if effects occur therefore we can identify more easily where potential problems may occur

### Verify Types

- Static Typesystem to identify errors early on

### Runtime Validation using generated Assertions on Records

- Syntactic sugar for better error messages

```
{ a @is_above(6) }
```

## ToDo's

### Parser & Lexer

- [x] LiteralExpr (`true`)
- [x] IdentExpr (`a`)
- [x] ListExpr (`[a, 2, c]`)
- [x] RecordExpr (`{ a = 10, b = b }`)
  - [ ] Optional Types (`{ a : Num = b }`)
- [x] RecordSelectExpr (`a.b.c`)
- [x] RecordExtendExpr (`{ r | +b = 20 }`)
  - [ ] Optional Types (`{ r : Person | +age : Num = 25 }`)
- [x] RecordRestrictExpr (`{ r | -c }`)
  - [ ] Optional Types (`{ r : Person | -age }`)
- [x] RecordUpdateExpr (`{ r | a = 5 }`)
  - [ ] Optional Types
- [x] UnaryExpr (`!true`, `-a`)
- [x] BinaryExpr (`a + b`)
  - [ ] ListOp (undecided Syntax `a :: []`, `[a] ++ [b]`)
- [x] LetExpr (`let x = 10 in ...`)
  - [ ] Patterns (`let { a } = { a = 10 } in ...`)
  - [ ] Optional Types
  - [ ] Syntactic Sugar in Top Level?? (`x = 10` == `let x = 10 in`)
- [x] IfExpr (`if true then ... else ...`)
- [x] CaseExpr (`case x of { a: { b } } => ..., _ => ...`)
- [x] FuncExpr (`\a => \b => a + b`)
  - [ ] Optional Types
- [x] CallExpr (`(f (g a))`)
  - [ ] Pipes (`a |> g |> f`)
  - [ ] Syntactic Sugar (`(f a b) with f -> 'a -> 'b`)
- [ ] TypeAlias (`Person : { name : Str, age : Num }`)
  - [ ] Open Records (`Aged : { * | age : Num }`)
  - [ ] Parametric Polymorphism (`Node a : { inner : a, span : Span }`)
- [ ] TypeAliasOp?? (`Member : { Person | +id : Num | -age }`)
- [ ] EffectDecl (`io ~ { read_file : Str -> Str }`, `print ~ Str -> {}`)
  - [ ] ComposeEffects?? (`buf_io ~ { io | +buf_read : Str -> Str }`)
  - [ ] Polymorphic Effects?? (`map a b 'e : (a -> b ~ 'e) -> (List a) -> (List b) ~ 'e`)
- [ ] FuncEffectSignature
  - (`let read_something : Num -> Num ~ { read_a : io.read_file, read_b : io.read_file } = ... in ...`)
- [ ] Invoke Effects (just like function ?? or with do ??)
- [ ] Effect Handler
  - (`do read_something(10) with read_a f => ..., read_b f => ..., return r => ...`)
  - (`handle read_something with ...`)
  - [ ] Optional Mark Effects as absent if handled (`a -> b ~ { io | +e : .. } becomes a -> b ~ { io }`)

### Type Inference

- [x] LiteralExpr (`true`)
- [x] IdentExpr (`a`)
- [ ] ListExpr (`[a, 2, c]`)
- [ ] RecordExpr (`{ a = 10, b = b }`)
- [ ] RecordSelectExpr (`a.b.c`)
- [ ] RecordExtendExpr (`{ r | +b = 20 }`)
- [ ] RecordRestrictExpr (`{ r | -c }`)
- [ ] RecordUpdateExpr (`{ r | a = 5 }`)
- [x] UnaryExpr (`!true`, `-a`)
- [x] BinaryExpr (`a + b`)
- [x] LetExpr (`let x = 10 in ...`)
- [x] IfExpr (`if true then ... else ...`)
- [ ] CaseExpr (`case x of { a: { b } } => ..., _ => ...`)
- [x] FuncExpr (`\a => \b => a + b`)
- [x] CallExpr (`(f (g a))`)


## Roadmap

### Create DevOps Example to better investigate challenges with configurations and evaluate existing solutions

- [ ] todos webservice with Database, Backend, Frontend
- [ ] define functional tests: Unit, Integration, (system)
- [ ] implement devops pipeline

### Implement ir Lowering and Interpreter

- A-Normal Form terms
- CEK-style abstract machine
CEK machine operates on configurations which are triples of the
form hC | E | Ki.
• The control C is the expression currently being evaluated.
• The environment E binds the free variables.
• The continuation K instructs the machine what to do once it is
done evaluating the current term in the C component (Liberating Effects...)
https://en.wikipedia.org/wiki/CEK_Machine


### Implement Row Polymorphism and Optional Type Definitions

### Implement Merge Operations for Records

### Other

- [ ] Algebraic Effects
- [ ] Recursion
- [ ] Module System
- [ ] Variants (Sum Types)
- [ ] Intrinsic Effects
  - Divergence?? (partial function may not terminate)
  - Total
  - Exception?
  - Console?

## References

- [Hindley Milner](https://www.lesswrong.com/posts/vTS8K4NBSi9iyCrPo/a-reckless-introduction-to-hindley-milner-type-inference)




```
package (input_a, input_b) {

  {
    name,
    a: input_a.xyz
    inputs [input_a, input_b]

  }
}


{
  
}
```
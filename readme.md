https://web.cecs.pdx.edu/~mpj/pubs/96-3.pdf
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf
https://blog.devgenius.io/the-landscape-of-declarative-configuration-55bb85d6997b

## Features

- Statically typed with Type inference
- Structural Typing with extensible Records
- Type annotations for "first-class" modules

## Roadmap

- [] Parser and Lexer
- [] Polymorphic Extensible Records (Row Polymorphism) (Scoped Labels Daan Leijen and "A Polymorphic Type System for Extensible Records and Variants" Gaster and Jones)
  - [] Selection: Selects the value of a label from a record
  - [] Restriction: Removes a label from a record
  - [] Extension: Extend a record with a label
  - Open Records
  - Closed Records
  - Allow Duplicate Labels ({ +x = 20 | { x = 10 }})
- [] Type Inference and Type Checking: Hindley Milner
- [] Delimited Continuations
- [] Algebraic Effects ("Koka: Programming with Row-polymorphic Effect Types" Daan Leijen)
  - [] Capture effects using Row Polymorphism
    - Effects of a function often determined by effects of functions passed to it (e.g. map function)
    - By using Row Polymorphism we can have "open" and "closed" effects we allow polymorphic effects
    - Because of duplicate labels in row polymorphism this gives precise types for effect elimination (e.g. catching exceptions)
      - A exception handler can throw an exception on its own:
        - catch : ∀αμ. (() → 〈exn | μ〉 α, exception → μ α) → μ α
        - Here we assume that catch takes two functions, the action and the
            exception handler that takes as an argument the thrown exception.
            Here, the exn effect of the action is discarded in the final effect
            μ since all exceptions are handled by the handler. But of course,
            the handler can itself throw an exception and have an exn effect
            itself. In that case μ will unify with a type of the form 〈exn|μ′〉
            giving action the effect 〈exn|exn|μ′〉 where exn occurs duplicated,
            which gives us exactly the right behavior.
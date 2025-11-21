<div align=center>

# Kola - Konfiguration Language

 [![NixOS][nixos-badge]][nixos-url]
 [![Docs][docs.rs-badge]][docs.rs-url]
 [![Website][website-badge]][website-url]
 ![License][license]

[website-badge]: https://img.shields.io/website?url=https%3A%2F%2Fmordragt.github.io%2Fkola&style=for-the-badge
[website-url]: https://mordragt.github.io/kola
[docs.rs-badge]: https://img.shields.io/badge/docs.rs-kola-4d76ae?style=for-the-badge
[docs.rs-url]: https://mordragt.github.io/kola/kola
[nixos-badge]: https://img.shields.io/badge/Flakes-Nix-informational.svg?logo=nixos&style=for-the-badge
[nixos-url]: https://nixos.org
[license]: https://img.shields.io/github/license/mordragt/kola?style=for-the-badge

Structurally and statically typed configuration language.

</div>

## About

Kola is a statically and structurally typed configuration language based on the simply-typed lambda calculus (STLC). It preserves the termination guarantees of STLC, ensuring that all programs are guaranteed to terminate and are not Turing complete.

Kola extends this foundation with row polymorphism, full type inference, algebraic effects, and functors. This combination enables expressive configuration patterns such as templating and parameterization, while providing strong static checks and composability.

The effect system is designed to offer principled and precise control over side effects, making it possible to implement features like plugins (planned) that handle effects in a modular way. Kola aims to provide a robust foundation for configuration tasks, balancing safety, expressiveness, and predictability.

## Installation

Kola is currently still in development and thus is only available via Cargo or Nix.
You can use Cargo to install Kola by cloning this repository and running `cargo install --path .`.

## Usage

- `kola parse <path>`: checks the provided file and returns the generated ast.
- `kola analyze <path>`: type checks and recursively discovers imports and returns a dependency-resolved ast.
- `kola compile <path>`: type checks and compiles the provided file, returning the resulting intermediate representation.
- `kola run <path>`: type checks, compiles, and runs the provided file, returning the resulting value.

## References

- [Hindley Milner](https://www.lesswrong.com/posts/vTS8K4NBSi9iyCrPo/a-reckless-introduction-to-hindley-milner-type-inference)

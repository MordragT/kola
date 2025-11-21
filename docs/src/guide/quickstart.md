# Quickstart

## Installation

### Prerequisites

- Rust Nightly with Cargo
- Terminal for accessing Kola via its command line interface (CLI)
- Text Editor
  - Zed is recommended, along with the [official extension](https://example.com) <!-- TODO --> (coming soon)

Kola can be used as a standalone tool or integrated as a dependency in an existing Rust project. For standalone installation, run:

::: code-group

```sh [cargo]
$ cargo install --git https://github.com/MordragT/kola
```

:::

## 🚧 Embedding

Embedding is currently a work in progress, as the exact details of how the plugin system will work are still being designed. However, you can already integrate subcrates of Kola into your Rust projects.

### Prerequisites

- Rust Nightly with Cargo

To integrate Kola into your existing Rust project, either run the command below or manually add it to your `Cargo.toml` file:

::: code-group

```sh [cargo]
$ cargo add --git https://github.com/MordragT/kola
```

:::

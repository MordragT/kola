# CLI

Kola comes with a command-line interface that provides essential tools for working with Kola configuration files. The general syntax for Kola's CLI is:

```
kola <COMMAND>
```


## Available Commands

Kola provides the following commands:

- **parse**: Checks a file and returns the generated abstract syntax tree (AST).
- **analyze**: Type checks a file, resolves imports, and returns a dependency-resolved AST.
- **compile**: Type checks and compiles a file, returning the resulting intermediate representation.
- **run**: Type checks, compiles, and runs a file, returning the resulting value.
- **help**: Displays help information for commands.

## Parse Command

The parse command checks your Kola file and returns the generated AST:

```
kola parse <PATH>
```

Where `<PATH>` is the path to the Kola file you want to parse.

## Analyze Command

The analyze command type checks your Kola file, recursively discovers imports, and returns a dependency-resolved AST:

```
kola analyze <PATH>
```

Where `<PATH>` is the path to the Kola file you want to analyze.

## Compile Command

The compile command type checks and compiles your Kola file, returning the resulting intermediate representation:

```
kola compile <PATH>
```

Where `<PATH>` is the path to the Kola file you want to compile.

## Run Command

The run command type checks, compiles, and runs your Kola file, returning the resulting value:

```
kola run <PATH>
```

Where `<PATH>` is the path to the Kola file you want to execute.

## Global Options

All commands support these options:

- `-h, --help`: Display help information
- `-V, --version`: Display version information (available at the top level)

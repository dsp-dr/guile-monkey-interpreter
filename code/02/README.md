# Chapter 2: Parsing

This chapter adds the parser to build Abstract Syntax Trees.

## Components

- **AST**: Node definitions for the syntax tree
- **Parser**: Pratt parser implementation

## Running

```bash
guile -L src/monkey --no-auto-compile -l main.scm
```

This starts a REPL that parses input and displays the AST.

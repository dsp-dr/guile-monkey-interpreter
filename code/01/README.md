# Chapter 1: Lexing

This chapter implements the lexer for the Monkey programming language.

## Components

- **Token**: Token type definitions
- **Lexer**: Converts source code into tokens

## Running

```bash
guile -L src/monkey --no-auto-compile -l main.scm
```

This starts a REPL that tokenizes input and displays the tokens.

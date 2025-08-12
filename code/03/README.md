# Chapter 3: Evaluation

This chapter implements the evaluator for executing Monkey programs.

## Components

- **Object**: Runtime value representations
- **Evaluator**: Tree-walking interpreter
- **Environment**: Variable bindings

## Running

```bash
guile -L src/monkey --no-auto-compile -l main.scm
```

This starts a fully functional Monkey REPL.

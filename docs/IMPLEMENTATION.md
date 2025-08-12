# Guile Monkey Interpreter - Implementation Guide

## Overview

This guide explains how the Monkey interpreter is implemented in Guile Scheme, following the structure of "Writing An Interpreter in Go" while adapting to Scheme idioms.

## Architecture

The interpreter follows a traditional architecture:

```
Source Code → Lexer → Tokens → Parser → AST → Evaluator → Result
```

### Key Components

1. **Token** - Token type definitions and constants
2. **Lexer** - Converts source text to tokens
3. **AST** - Abstract Syntax Tree node definitions
4. **Parser** - Pratt parser building the AST
5. **Object** - Runtime value representations
6. **Evaluator** - Tree-walking interpreter
7. **REPL** - Interactive Read-Eval-Print-Loop

## Scheme Adaptations

### Records vs Structs

Go uses structs; we use SRFI-9 records:

```go
// Go
type Token struct {
    Type    TokenType
    Literal string
}
```

```scheme
;; Scheme
(define-record-type <token>
  (make-token type literal)
  token?
  (type token-type)
  (literal token-literal))
```

### Pattern Matching

Where Go uses switch statements, we use `match` from `(ice-9 match)`:

```go
// Go
switch ch {
case '=':
    return Token{Type: ASSIGN, Literal: "="}
case '+':
    return Token{Type: PLUS, Literal: "+"}
}
```

```scheme
;; Scheme
(match ch
  (#\= (make-token ASSIGN "="))
  (#\+ (make-token PLUS "+")))
```

### Mutation and State

Go freely uses mutation; in Scheme we're more careful:

```go
// Go - Direct mutation
l.position = l.readPosition
l.readPosition += 1
```

```scheme
;; Scheme - Explicit setters
(set-lexer-position! lexer (lexer-read-position lexer))
(set-lexer-read-position! lexer (+ 1 (lexer-read-position lexer)))
```

### Error Handling

Go uses multiple return values; we use exceptions and conditions:

```go
// Go
val, ok := env.Get(name)
if !ok {
    return nil, fmt.Errorf("undefined: %s", name)
}
```

```scheme
;; Scheme
(or (env-get env name)
    (make-error-object (format #f "undefined: ~a" name)))
```

## Module System

Each component is a Guile module with explicit exports:

```scheme
(define-module (monkey lexer lexer)
  #:use-module (monkey token token)
  #:use-module (srfi srfi-9)
  #:export (make-lexer
            next-token))
```

### Module Naming Convention

- `(monkey component name)` - Main module for component
- `(monkey component test)` - Test module
- Files: `src/monkey/component/name.scm`

## Implementation Details

### Chapter 1: Lexer

The lexer maintains position in the input string and produces tokens:

```scheme
(define-record-type <lexer>
  (make-lexer* input position read-position ch)
  lexer?
  (input lexer-input)
  (position lexer-position set-lexer-position!)
  (read-position lexer-read-position set-lexer-read-position!)
  (ch lexer-ch set-lexer-ch!))
```

Key functions:
- `make-lexer` - Constructor
- `next-token` - Main tokenization function
- `read-char!` - Advance position
- `skip-whitespace!` - Handle whitespace
- `read-identifier`, `read-number`, `read-string` - Multi-char tokens

### Chapter 2: Parser

The parser uses Pratt parsing (operator precedence) to build an AST:

```scheme
(define-record-type <parser>
  (make-parser* lexer cur-token peek-token errors 
                prefix-parse-fns infix-parse-fns)
  parser?
  ...)
```

Key concepts:
- **Prefix parse functions** - Handle prefix operators and literals
- **Infix parse functions** - Handle binary operators
- **Precedence levels** - Determine parsing order

### Chapter 3: Evaluator

The evaluator walks the AST and produces values:

```scheme
(define (eval node env)
  (cond
   ((program? node) (eval-program node env))
   ((let-statement? node) (eval-let-statement node env))
   ((identifier? node) (eval-identifier node env))
   ...))
```

Environment for variable bindings:

```scheme
(define-record-type <environment>
  (make-environment* store outer)
  environment?
  (store environment-store)
  (outer environment-outer))
```

### Chapter 4: Extensions

Arrays, hashes, and built-in functions:

```scheme
;; Array operations
(define builtin-len
  (make-builtin-function
    (lambda (args)
      (match args
        ((obj)
         (cond
          ((string-object? obj) 
           (make-integer-object (string-length (string-object-value obj))))
          ((array-object? obj)
           (make-integer-object (length (array-object-elements obj))))
          (else (make-error-object "len: unsupported type"))))
        (_ (make-error-object "len: wrong number of arguments"))))))
```

## Testing Strategy

Tests use SRFI-64 and follow a table-driven approach:

```scheme
(test-group "arithmetic expressions"
  (for-each
    (lambda (test-case)
      (match test-case
        ((input expected)
         (test-equal input expected (eval-input input)))))
    '(("5 + 5" 10)
      ("5 - 5" 0)
      ("5 * 5" 25)
      ("5 / 5" 1))))
```

## Performance Considerations

1. **Tail recursion** - Use named let for loops
2. **Hash tables** - For environments and built-ins
3. **Minimal copying** - Share structure where possible
4. **Lazy evaluation** - Where appropriate

## Extending the Interpreter

To add a new feature:

1. **Update tokens** - Add new token types if needed
2. **Update lexer** - Recognize new syntax
3. **Define AST nodes** - New record types
4. **Update parser** - Parse new constructs
5. **Implement evaluation** - Add to evaluator
6. **Add tests** - Comprehensive test coverage

### Example: Adding `for` loops

1. Add `FOR` token
2. Lexer recognizes "for" keyword
3. Define `<for-statement>` AST node
4. Parser handles `for (init; cond; update) { body }`
5. Evaluator implements for loop semantics
6. Tests verify functionality

## Best Practices

1. **Immutability by default** - Only mutate when necessary
2. **Clear module boundaries** - Well-defined exports
3. **Comprehensive tests** - Test-driven development
4. **Documentation** - Document public APIs
5. **Error messages** - Helpful and specific
6. **REPL-friendly** - Easy to experiment

## Debugging Tips

1. Use `(debug-enable 'trace)` for function tracing
2. Add print statements in evaluator
3. Build intermediate REPLs (lexer-only, parser-only)
4. Use `pk` (peek) for quick debugging
5. Write small test cases

## Common Pitfalls

1. **Circular dependencies** - Careful module organization
2. **Mutation bugs** - Prefer functional style
3. **Tail recursion** - Avoid stack overflow
4. **Symbol comparison** - Use `eq?` not `equal?`
5. **Macro hygiene** - Be careful with syntax transformers

## Resources

- [Guile Manual](https://www.gnu.org/software/guile/manual/)
- [SRFI Documents](https://srfi.schemers.org/)
- [Original Go Implementation](https://github.com/ThorstenBall/monkey)
- [Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
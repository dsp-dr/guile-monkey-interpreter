# Monkey Language Examples

This directory contains example programs written in the Monkey programming language.

## Basic Examples

- `fibonacci.monkey` - Recursive Fibonacci sequence calculator
- `closure.monkey` - Demonstrates closures and higher-order functions
- `higher-order.monkey` - Examples of functions as first-class values

## MAL (Make-a-Lisp) Implementation

A series of examples implementing a simple Lisp interpreter in Monkey, following the [MAL tutorial](https://github.com/kanaka/mal):

- `mal-step0.monkey` - **Step 0: REPL** - Basic Read-Eval-Print-Loop that echoes input
- `mal-step1.monkey` - **Step 1: Read/Print** - S-expression tokenizer and parser with pretty printing
- `mal-step2.monkey` - **Step 2: Eval** - Arithmetic expression evaluation (+, -, *, /)

### Running MAL Examples

```bash
# Run via REPL (interactive)
./monkey
>> let src = "mal-step2.monkey"
>> # Then paste the code

# Or evaluate expressions directly
echo 'puts("(+ 2 3) = 5")' | ./monkey
```

### MAL Features Demonstrated

1. **Tokenization** - Breaking input into tokens (parens, numbers, symbols, strings)
2. **Parsing** - Building an AST from tokens using recursive descent
3. **AST Representation** - Using Monkey's hash maps to represent Lisp nodes
4. **Evaluation** - Walking the AST and computing results
5. **Pretty Printing** - Converting AST back to S-expression notation

### Example MAL Session (from step2)

```
user> (+ 1 2)
3

user> (+ 5 (* 2 3))
11

user> (- (+ 5 (* 2 3)) 3)
8
```

## Notes

- The Monkey interpreter must be run from the project root directory
- MAL examples show Monkey's capability to implement other language interpreters
- These are educational examples following the MAL tutorial structure
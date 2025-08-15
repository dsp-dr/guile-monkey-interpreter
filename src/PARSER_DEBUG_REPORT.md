# Parser Debugging Report

## Issue Summary
The original `parser.scm` file experienced severe parenthesis nesting issues that prevented the module from loading. The core problem was in the `parse-hash-literal` function where incorrect parenthesis counts caused subsequent function definitions to be interpreted as expressions within the function body, leading to "definition in expression context" errors.

## Root Cause Analysis

### Primary Issue
- **Location**: `parse-hash-literal` function (lines 428-471)
- **Problem**: Excessive closing parentheses at line 471 (10-12 parens instead of the correct number)
- **Effect**: This prematurely closed the function, causing `parse-expression-list` at line 473 to be interpreted as inside an expression context rather than as a top-level definition

### Specific Error
```
Syntax error:
./monkey/parser/parser.scm:473:0: definition in expression context, where definitions are not allowed, 
in form (define (parse-expression-list parser end-token) ...)
```

## Tools Developed

### 1. Parenthesis Balance Checker (`balance-parens.scm`)
- Counts total open/close parentheses
- Shows unclosed parenthesis positions
- Finds function boundaries

### 2. Depth Analyzer (`fix-parser-structure.scm`)
- Line-by-line depth tracking
- Visual representation of nesting levels
- Identifies depth anomalies

### 3. Function Boundary Finder (`fix-parser-final.scm`)
- Tracks function start/end points
- Shows depth changes per line
- Identifies where functions actually end vs. where they should end

### 4. Trace Tools (`experiments/101-for-loop-debugging/trace-tools.scm`)
- AST dumper
- Token stream viewer
- Evaluation tracer
- Test case runner

### 5. LSP/Tree-IL Experiment (`experiments/102-lsp-tree-il-tooling/`)
- Emacs LSP setup for structural editing
- Tree-IL visualization concepts
- Paredit integration for better parenthesis management

## Debugging Attempts

### Attempt 1: Manual Parenthesis Counting
- **Method**: Manually counted parentheses at key locations
- **Result**: Inconsistent due to complex nesting
- **Learning**: Need automated tools

### Attempt 2: Binary Search for Error Location
- **Method**: Added/removed parentheses at different locations
- **Result**: Created new syntax errors
- **Learning**: Problem was structural, not just count

### Attempt 3: Function-by-Function Analysis
- **Method**: Used depth tracking to analyze each function
- **Result**: Found depth drop from 15 to 5 at line 471
- **Learning**: Identified exact location of excess parentheses

### Attempt 4: Incremental Fixes
- **Method**: Adjusted parentheses one at a time
- **Result**: Created cascading errors
- **Learning**: File structure was too corrupted for incremental fixes

## Solution: Modular Rebuild

### Architecture
```
parser-new.scm (main module)
├── parser-base.scm (core infrastructure)
├── parser-statements.scm (statement parsing)
├── parser-literals.scm (literal parsing)
└── parser-expressions.scm (expression parsing)
```

### Benefits
1. **Isolation**: Each component can be tested independently
2. **Clarity**: Smaller files are easier to debug
3. **Maintainability**: Changes don't affect entire parser
4. **Testing**: Can unit test each module

## Interactive REPL Testing

### Setting up Debug Session
```scheme
;; Start Guile with debugging
$ guile --debug

;; Load the new parser
scheme@(guile-user)> (add-to-load-path ".")
scheme@(guile-user)> (use-modules (monkey parser parser-new))

;; Test basic parsing
scheme@(guile-user)> (use-modules (monkey lexer lexer))
scheme@(guile-user)> (define l (make-lexer "let x = 5;"))
scheme@(guile-user)> (define p (make-parser l))
scheme@(guile-user)> (define prog (parse-program p))
scheme@(guile-user)> (parser-errors p)

;; Debug commands available:
;; ,backtrace (,bt) - Show call stack
;; ,frame N - Select stack frame
;; ,locals - Show local variables
;; ,up / ,down - Navigate stack
;; ,break procedure - Set breakpoint
;; ,trace procedure - Trace calls
```

### Testing For Loops
```scheme
;; Test for loop parsing
scheme@(guile-user)> (define l (make-lexer "for (let i = 0; i < 10; i = i + 1) { x = x + i; }"))
scheme@(guile-user)> (define p (make-parser l))
scheme@(guile-user)> (define prog (parse-program p))
scheme@(guile-user)> (if (null? (parser-errors p))
                         (display "Parse successful!\n")
                         (for-each (lambda (e) (format #t "Error: ~a~%" e))
                                   (parser-errors p)))
```

## Lessons Learned

1. **Modular Design**: Complex parsers should be split into modules from the start
2. **Tool Investment**: Time spent building debugging tools pays off
3. **Incremental Testing**: Test each component before integration
4. **Version Control**: Keep working versions before major changes
5. **Documentation**: Document parenthesis nesting strategy

## Next Steps

1. Complete testing of modular parser
2. Integrate with existing codebase
3. Run full test suite
4. Document module interfaces
5. Consider Tree-sitter or similar for future projects

## Recommendations

### Immediate
- Use the modular parser going forward
- Add unit tests for each module
- Document the module architecture

### Long-term
- Investigate Tree-sitter for Scheme parsing
- Set up LSP server for better IDE support
- Consider s-expression based DSL instead of C-like syntax
- Use paredit or similar for all Scheme editing
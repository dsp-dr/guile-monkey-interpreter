# Project Structure Guide

This document explains the organization of the Monkey Interpreter project.

## Directory Layout

```
.
├── src/                     # SOURCE CODE ONLY
│   ├── monkey/             # Core interpreter implementation
│   │   ├── token/          # Token type definitions
│   │   ├── lexer/          # Tokenization/lexical analysis
│   │   ├── ast/            # Abstract syntax tree nodes
│   │   ├── parser/         # Modular Pratt parser implementation
│   │   ├── object/         # Runtime object system
│   │   ├── evaluator/      # Tree-walking evaluator
│   │   └── repl/           # Read-Eval-Print-Loop
│   └── experiments/        # Language feature experiments
│       ├── 001-for-loops/
│       ├── 002-break-continue/
│       └── 103-mermaid-visualizer/
│
├── tests/                   # ALL TEST FILES
│   ├── integration/        # Integration and feature tests
│   │   ├── test-all-features.scm
│   │   ├── test-chapter4.scm
│   │   ├── test-for-loops.scm
│   │   ├── test-lambda-shorthand.scm
│   │   ├── test-new-parser.scm
│   │   └── test-quick-wins.scm
│   ├── lexer-test.scm      # Unit tests for lexer
│   └── parser-test.scm     # Unit tests for parser
│
├── code/                    # BOOK CHAPTER IMPLEMENTATIONS
│   ├── 01/                 # Chapter 1: Lexing
│   ├── 02/                 # Chapter 2: Parsing
│   ├── 03/                 # Chapter 3: Evaluation
│   └── 04/                 # Chapter 4: Extending
│
├── tutorials/               # LEARNING MATERIALS
│   ├── 01-getting-started/ # Installation, first program
│   ├── 02-language-features/ # Language guide
│   ├── 03-extending-monkey/ # Adding features
│   ├── 04-debugging/       # Debugging techniques
│   └── 05-advanced-topics/ # Architecture deep dives
│
├── docs/                    # TECHNICAL DOCUMENTATION
│   ├── reports/            # Status and analysis reports
│   │   ├── FEATURES_IMPLEMENTED.md
│   │   ├── PARSER_DEBUG_REPORT.md
│   │   ├── STATUS_REPORT.md
│   │   └── test-results.md
│   ├── BOOK_BREAKDOWN.md
│   ├── DEMO_GUIDE.md
│   └── IMPLEMENTATION.md
│
├── scripts/                 # UTILITY SCRIPTS
│   ├── debug/              # Debugging tools
│   │   ├── balance-parens.scm
│   │   ├── fix-parser-final.scm
│   │   └── fix-parser-structure.scm
│   ├── gdb-guile.sh        # GDB debugging wrapper
│   ├── tmux-guile.sh       # tmux session manager
│   ├── test.sh             # Test runner
│   └── monkey              # Monkey REPL launcher
│
├── examples/               # EXAMPLE MONKEY PROGRAMS
│   ├── fibonacci.monkey
│   ├── closure.monkey
│   ├── higher-order.monkey
│   └── showcase.monkey
│
├── demo/                   # DEMO MATERIALS
│   ├── monkey-demo.cast   # asciinema recording
│   ├── monkey-demo.gif    # Demo GIF
│   └── run-demo.scm       # Demo runner script
│
├── resources/              # EXTERNAL RESOURCES
│   ├── book-chunks/       # Book excerpts
│   └── *.pdf              # Reference materials
│
└── logs/                   # GENERATED LOGS (gitignored)
    └── *.log              # Test and debug output
```

## Key Principles

### 1. Source Code Isolation
- `src/` contains ONLY source code
- No tests, logs, or documentation in src/
- Clean separation of implementation from everything else

### 2. Test Organization
- All tests in `tests/` directory
- Unit tests at root of tests/
- Integration tests in `tests/integration/`

### 3. Documentation Hierarchy
- `tutorials/` - User-facing learning materials
- `docs/` - Technical documentation and reports
- `README.md` files in each directory for local guidance

### 4. Script Organization
- `scripts/` - All executable utilities
- `scripts/debug/` - Debugging-specific tools
- Clear, descriptive script names

### 5. Clean Project Root
Only essential files at root:
- `README.md` - Project overview
- `LICENSE` - Legal information
- `Makefile` - Build automation
- `DOCUMENTATION_STATUS.md` - Doc tracking
- `.gitignore` - Version control config

## File Movement Guide

When adding new files:

| File Type | Location |
|-----------|----------|
| Scheme source code | `src/monkey/` |
| Experiments | `src/experiments/XXX-name/` |
| Unit tests | `tests/` |
| Integration tests | `tests/integration/` |
| Monkey examples | `examples/` |
| Shell scripts | `scripts/` |
| Debug tools | `scripts/debug/` |
| Tutorials | `tutorials/XX-topic/` |
| Technical docs | `docs/` |
| Status reports | `docs/reports/` |
| Log files | `logs/` |
| External resources | `resources/` |

## Running from New Locations

### Tests
```bash
# Old: guile -L src src/test-quick-wins.scm
# New:
guile -L src tests/integration/test-quick-wins.scm
```

### Debug Tools
```bash
# Old: src/balance-parens.scm file.scm
# New:
scripts/debug/balance-parens.scm file.scm
```

### Visualization
```bash
# Paths updated but functionality unchanged
src/experiments/103-mermaid-visualizer/generate-ast-diagram.scm "let x = 5;"
```

## Benefits of This Structure

1. **Clarity**: Clear purpose for each directory
2. **Maintainability**: Easy to find and organize files
3. **Scalability**: Structure supports growth
4. **Professional**: Industry-standard organization
5. **Clean**: Minimal clutter in any directory

## Migration Notes

If you have scripts or workflows that reference old paths:

1. Test files moved from `src/` to `tests/integration/`
2. Debug tools moved from `src/` to `scripts/debug/`
3. Reports moved to `docs/reports/`
4. The core source code in `src/monkey/` remains unchanged

Update your scripts accordingly or use the Makefile targets which have been updated to use the new paths.
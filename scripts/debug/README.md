# Debug Scripts

This directory contains debugging tools developed during parser debugging sessions.

## Tools

### Parenthesis Balance Checker
- `balance-parens.scm` - Checks for mismatched parentheses in Scheme files
- Usage: `./balance-parens.scm <filename>`

### Parser Fix Tools
- `fix-parser-structure.scm` - Analyzes parser structure and depth
- `fix-parser-final.scm` - Final parser fixing tool with function boundary detection

## Usage Examples

### Check Parenthesis Balance
```bash
./scripts/debug/balance-parens.scm src/monkey/parser/parser.scm
```

### Analyze Parser Structure
```bash
./scripts/debug/fix-parser-structure.scm
```

## Background

These tools were created during the debugging of complex parenthesis mismatches in the parser module. They demonstrate practical approaches to:
- Static analysis of Scheme code
- Parenthesis tracking with position information
- Function boundary detection
- Structural validation

See the [Debugging Workshop](../../tutorials/04-debugging/01-debugging-workshop.md) for detailed context on how these tools were developed and used.
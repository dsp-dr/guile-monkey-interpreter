# Monkey Language Extension Experiments

This directory contains proof-of-concept implementations and analysis for potential language extensions to the Monkey interpreter.

## Quick Summary

| Feature | Effort | Impact | Priority | Hours |
|---------|--------|--------|----------|-------|
| Array operations (map, filter, reduce) | LOW | Very High | VERY HIGH | 6 |
| Anonymous function shorthand `\|x\| x + 1` | LOW | High | HIGH | 6 |
| String interpolation `${name}` | MEDIUM | High | HIGH | 9 |
| For loops | MEDIUM | Medium | MEDIUM | 8 |
| Break/Continue | MEDIUM | High | HIGH | 8 |
| Exception handling | HIGH | High | MEDIUM | 16 |
| Pattern matching | HIGH | High | LOW | 25 |
| Module system | HIGH | Very High | HIGH | 25+ |

## Experiments

### Language Features
- `01-for-loops.monkey` - C-style for loops
- `02-break-continue.monkey` - Loop control flow
- `03-string-interpolation.monkey` - Template strings
- `04-pattern-matching.monkey` - Scheme-like match expressions
- `05-exception-handling.monkey` - Try/catch/finally blocks
- `06-tail-call-optimization.monkey` - Recursive optimization
- `07-anonymous-shorthand.monkey` - Lambda syntax sugar

### Built-in Extensions
- `08-array-operations.monkey` - Functional array methods
- `09-module-system.monkey` - Import/export system

## Implementation Roadmap

### 🎯 Phase 1: Quick Wins (1 week)
Essential features that provide immediate value:
- ✅ Array operations (map, filter, reduce)
- ✅ String manipulation functions
- ✅ Anonymous function shorthand
- ✅ String interpolation

### 🚀 Phase 2: Core Enhancements (2 weeks)
Improving the core language:
- ⬜ Break/continue statements
- ⬜ For loops
- ⬜ File I/O operations
- ⬜ Math functions
- ⬜ Code formatter

### 🔧 Phase 3: Advanced Features (1 month)
Major architectural improvements:
- ⬜ Module system
- ⬜ Exception handling
- ⬜ JSON support
- ⬜ Tail call optimization
- ⬜ Debugger

### 🌟 Phase 4: Ecosystem (2+ months)
Building a complete development ecosystem:
- ⬜ Pattern matching
- ⬜ HTTP client
- ⬜ Language Server Protocol
- ⬜ Package manager

## Running Experiments

Each experiment file can be run with the current interpreter to see what's possible with existing features:

```bash
./monkey experiments/01-for-loops.monkey
```

The experiments demonstrate:
1. Current workarounds for missing features
2. Proposed syntax for new features
3. Implementation approaches
4. Level of effort estimates

## Key Insights

### Leveraging Guile
Many features can be implemented efficiently by wrapping Guile's functionality:
- **File I/O** - Use Guile's port system
- **JSON** - Use guile-json module
- **HTTP** - Use Guile's web client
- **Math** - Direct bindings to Scheme functions
- **TCO** - Leverage Scheme's optimization

### Parser Considerations
The current parser uses `with-return` for early exits. Complex features like pattern matching may require significant parser refactoring.

### Priority Rationale

**Very High Priority** - Features that dramatically improve usability:
- Array operations (essential for data manipulation)
- String functions (basic text processing)

**High Priority** - Features that improve developer experience:
- Anonymous function shorthand (cleaner code)
- String interpolation (better readability)
- Module system (code organization)

**Medium Priority** - Nice-to-have improvements:
- For loops (familiar syntax)
- Exception handling (error management)
- Debugger (development tool)

**Low Priority** - Advanced features:
- Pattern matching (powerful but complex)
- Compilation targets (performance optimization)
- Package manager (ecosystem growth)

## Next Steps

1. **Implement Phase 1** - Start with array operations and string functions
2. **Gather feedback** - Test with real use cases
3. **Refine priorities** - Adjust based on user needs
4. **Create standard library** - Package common utilities

## Contributing

To add a new experiment:
1. Create a `.monkey` file demonstrating the feature
2. Include current workarounds and proposed syntax
3. Document implementation approach
4. Estimate level of effort
5. Update this README with summary

## See Also

- [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) - Detailed analysis and estimates
- [../examples/](../examples/) - Current language examples
- [../docs/](../docs/) - Language documentation
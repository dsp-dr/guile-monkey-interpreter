# Monkey Interpreter Tutorials

Welcome to the Monkey Interpreter tutorials! These guides will help you understand, use, and extend the Monkey programming language interpreter written in Guile Scheme.

## Tutorial Structure

### 📚 [01 - Getting Started](01-getting-started/)
- [Installation Guide](01-getting-started/01-installation.md)
- [Your First Monkey Program](01-getting-started/02-first-program.md)
- [Using the REPL](01-getting-started/03-using-repl.md)
- [Running Scripts](01-getting-started/04-running-scripts.md)

### 🐒 [02 - Language Features](02-language-features/)
- [Variables and Data Types](02-language-features/01-variables-types.md)
- [Functions and Closures](02-language-features/02-functions.md)
- [Control Flow](02-language-features/03-control-flow.md)
- [Built-in Functions](02-language-features/04-builtins.md)
- [Arrays and Hashes](02-language-features/05-collections.md)

### 🔧 [03 - Extending Monkey](03-extending-monkey/)
- [Adding New Built-in Functions](03-extending-monkey/01-new-builtins.md)
- [Implementing Language Features](03-extending-monkey/02-language-features.md)
- [FFI Extensions](03-extending-monkey/03-ffi-extensions.md)
- [Module System Design](03-extending-monkey/04-module-system.md)

### 🐛 [04 - Debugging](04-debugging/)
- [Debugging Workshop](04-debugging/01-debugging-workshop.md) - Comprehensive debugging guide
- [Using the REPL Debugger](04-debugging/02-repl-debugging.md)
- [GDB Integration](04-debugging/03-gdb-debugging.md)
- [Interactive tmux Sessions](04-debugging/04-tmux-sessions.md)
- [Visualization Tools](04-debugging/05-visualization.md)

### 🚀 [05 - Advanced Topics](05-advanced-topics/)
- [Interpreter Architecture](05-advanced-topics/01-architecture.md)
- [Parser Implementation](05-advanced-topics/02-parser-details.md)
- [Evaluator Internals](05-advanced-topics/03-evaluator.md)
- [Performance Optimization](05-advanced-topics/04-performance.md)
- [Testing Strategies](05-advanced-topics/05-testing.md)

## Quick Start Guide

If you're new to the Monkey interpreter, follow this path:

1. **Install Guile and the interpreter** → [Installation Guide](01-getting-started/01-installation.md)
2. **Learn basic Monkey syntax** → [Your First Program](01-getting-started/02-first-program.md)
3. **Explore language features** → [Language Features](02-language-features/)
4. **Try debugging tools** → [Debugging Workshop](04-debugging/01-debugging-workshop.md)

## Interactive Learning

### Try These Examples

```monkey
// Variables and arithmetic
let x = 5;
let y = 10;
puts(x + y);  // 15

// Functions
let add = fn(a, b) { a + b };
puts(add(3, 4));  // 7

// Arrays
let arr = [1, 2, 3];
puts(len(arr));  // 3
puts(first(arr));  // 1

// Higher-order functions
let twice = fn(f, x) { f(f(x)) };
let addTwo = fn(x) { x + 2 };
puts(twice(addTwo, 5));  // 9
```

### Run the REPL

```bash
# Start the interpreter
make repl

# Or directly with Guile
guile -L src src/monkey/main.scm
```

## Tutorial Format

Each tutorial follows a consistent structure:

- **Overview**: What you'll learn
- **Prerequisites**: Required knowledge
- **Content**: Step-by-step instructions
- **Examples**: Working code samples
- **Exercises**: Practice problems
- **Summary**: Key takeaways

## Contributing to Tutorials

We welcome contributions to improve and expand these tutorials! 

### Guidelines

1. Keep examples simple and focused
2. Test all code samples
3. Provide clear explanations
4. Include common pitfalls
5. Add exercises for practice

### Submitting Tutorials

1. Fork the repository
2. Create your tutorial in the appropriate section
3. Follow the existing format
4. Test all examples
5. Submit a pull request

## Resources

### Book References
- [Writing An Interpreter in Go](https://interpreterbook.com/) - Original book
- [Chapter Implementation](../code/) - Progressive implementation

### Documentation
- [Implementation Guide](../docs/IMPLEMENTATION.md)
- [API Reference](../docs/README.md)
- [Examples](../examples/)

### Tools
- [tmux Debugging Session](../scripts/tmux-guile.sh)
- [GDB Integration](../scripts/gdb-guile.sh)
- [Mermaid Visualizers](../src/experiments/103-mermaid-visualizer/)

## Support

If you have questions or need help:

1. Check the [FAQ](05-advanced-topics/faq.md)
2. Review [Common Issues](04-debugging/common-issues.md)
3. Open an [Issue](https://github.com/dsp-dr/guile-monkey-interpreter/issues)

## License

These tutorials are part of the Monkey Interpreter project and are released under the MIT License.
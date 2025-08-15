# Contributing to Guile Monkey Interpreter

Thank you for your interest in contributing! This educational project welcomes contributions that enhance learning and understanding of interpreter implementation.

## Getting Started

1. **Read the book**: Familiarity with ["Writing An Interpreter in Go"](https://interpreterbook.com/) is helpful
2. **Explore the tutorials**: Start with [`tutorials/01-getting-started/`](tutorials/01-getting-started/)
3. **Run the tests**: `make test` to ensure everything works
4. **Try the REPL**: `make repl` to experiment with Monkey

## How to Contribute

### Reporting Issues

- **Bug reports**: Include Monkey code that reproduces the issue
- **Feature requests**: Explain the educational value
- **Documentation**: Point out unclear or missing explanations

### Submitting Changes

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-addition`
3. Make your changes following the guidelines below
4. Run tests: `make test`
5. Commit with conventional commits: `git commit -m "feat: add new built-in function"`
6. Push: `git push origin feature/amazing-addition`
7. Open a Pull Request

## Development Guidelines

### Code Organization

Place files in the appropriate directory:

| Type | Location | Example |
|------|----------|---------|
| Core interpreter code | `src/monkey/` | Parser, evaluator |
| New language features | `experiments/00X-name/` | `experiments/011-type-system/` |
| Debugging tools | `experiments/10X-name/` | `experiments/104-profiler/` |
| Test files | `tests/` or `tests/integration/` | `tests/integration/test-feature.scm` |
| Monkey examples | `examples/` | `examples/recursion.monkey` |
| Tutorials | `tutorials/XX-topic/` | `tutorials/02-language-features/` |

### Coding Standards

#### Scheme Style
```scheme
;; Good: Clear, simple, educational
(define (eval-infix-expression operator left right)
  (cond
   ((and (integer? left) (integer? right))
    (eval-integer-infix-expression operator left right))
   ((string=? operator "==")
    (make-boolean (equal? left right)))
   (else 
    (make-error (format #f "unknown operator: ~a ~a ~a" 
                        (object-type left) operator (object-type right))))))

;; Avoid: Overly clever or compact code that's hard to understand
```

#### Commit Messages
Follow conventional commits:
- `feat:` New feature
- `fix:` Bug fix
- `docs:` Documentation only
- `test:` Adding tests
- `refactor:` Code change that neither fixes a bug nor adds a feature
- `chore:` Changes to build process or auxiliary tools

Example:
```
feat: implement do-while loops

- Add do-while parsing in parser
- Implement evaluation logic
- Add tests for do-while statements
- Update documentation

Closes #42
```

### Testing

- Every new feature needs tests
- Tests should be educational - show how the feature works
- Use descriptive test names that explain what's being tested

```scheme
(test-equal "for loop with break statement exits early"
  5  ; Expected: loop breaks when i == 5
  (eval-string "
    let result = 0;
    for (let i = 0; i < 10; i = i + 1) {
      if (i == 5) { break; }
      let result = i;
    }
    result;"))
```

### Documentation

- Code should be self-explanatory (prefer clear code over comments)
- Add README files to new experiments explaining the feature
- Update tutorials if adding significant features
- Include example Monkey programs demonstrating new features

## What We're Looking For

### ✅ Great Contributions

- **Educational features**: Things that help people learn
- **Clear implementations**: Code that's easy to understand
- **Helpful documentation**: Tutorials, examples, explanations
- **Test coverage**: Tests that demonstrate functionality
- **Book alignment**: Features that complement the book's teachings

### ❌ Not a Good Fit

- **Production optimizations**: This is for learning, not performance
- **Complex abstractions**: Keep it simple and educational
- **Breaking changes**: Should maintain compatibility with the book
- **Non-educational features**: Features without learning value

## Experiment Proposals

For new language features, create an experiment:

1. Create `experiments/0XX-feature-name/README.md`:
```markdown
# Experiment: Feature Name

## Motivation
Why this feature is educational...

## Design
How it works...

## Implementation Plan
- [ ] Parser changes
- [ ] Evaluator changes
- [ ] Tests

## Example
```monkey
// Show the feature in action
```
```

2. Implement in the experiment directory first
3. If successful, propose integration into main interpreter

## Questions?

- Check existing issues and discussions
- Read the [tutorials](tutorials/)
- Explore the [examples](examples/)
- Open an issue for clarification

## Recognition

Contributors are recognized in commit history with Co-Authored-By tags and in pull request descriptions.

## License

By contributing, you agree that your contributions will be licensed under the same MIT License that covers this project.

---

Remember: This is an educational project. The goal is to learn and help others learn about interpreters. Keep contributions clear, educational, and fun!
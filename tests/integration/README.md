# Integration Tests

This directory contains integration and feature tests for the Monkey interpreter.

## Test Files

- `test-all-features.scm` - Comprehensive test of all implemented features
- `test-chapter4.scm` - Chapter 4 extended built-ins tests
- `test-for-loops.scm` - For loop implementation tests
- `test-lambda-shorthand.scm` - Lambda shorthand syntax tests
- `test-new-parser.scm` - Modular parser validation tests
- `test-quick-wins.scm` - Quick win extensions tests

## Running Tests

### Individual Tests
```bash
guile -L src tests/integration/test-quick-wins.scm
```

### All Integration Tests
```bash
for test in tests/integration/test-*.scm; do
  echo "Running $test..."
  guile -L src "$test"
done
```

### Using Make
```bash
make test        # Run all tests
make test-quick  # Run quick wins tests
```

## Test Structure

Each test file follows the SRFI-64 testing framework:

```scheme
(use-modules (srfi srfi-64))

(test-begin "test-name")

(test-equal "description"
  expected-value
  actual-value)

(test-end "test-name")
```
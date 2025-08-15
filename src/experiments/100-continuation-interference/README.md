# Experiment 100: Continuation Interference in Nested with-return Macros

## Problem Statement

When `parse-let-statement` (which uses `with-return` macro) is called from within `parse-for-expression`'s `with-return` context, the continuations interfere, causing "unbound variable: return" errors.

## Root Cause Analysis

The `with-return` macro uses `call/cc` (call-with-current-continuation) to create an escape continuation:

```scheme
(define-syntax with-return
  (syntax-rules ()
    ((with-return body ...)
     (call/cc (lambda (return)
                body ...)))))
```

When nested, each `with-return` creates its own `return` binding in its lexical scope. The issue arises when:

1. `parse-for-expression` calls `with-return`, binding `return` in its scope
2. Inside, it calls `parse-let-statement` which has its own `with-return`
3. The inner `with-return` shadows the outer `return`
4. When `parse-let-statement` returns normally (not via `return`), control returns to `parse-for-expression`
5. But the lexical binding of `return` from the outer `with-return` may not be properly restored in all execution paths

## Test Cases

### Test 1: Simple Nested with-return
```scheme
(define (outer)
  (with-return
    (format #t "Outer start~%")
    (let ((result (inner)))
      (format #t "After inner: ~a~%" result)
      (return 'outer-return))))

(define (inner)
  (with-return
    (format #t "Inner start~%")
    (return 'inner-return)))
```

### Test 2: Nested with-return in let* binding
```scheme
(define (problematic)
  (with-return
    (let* ((x 1)
           (y (with-return (return 2)))
           (z 3))
      (return (+ x y z)))))
```

### Test 3: The Actual Problem Pattern
```scheme
(define (parse-for-like)
  (with-return
    (let ((init (parse-let-like)))
      (unless init
        (return #f))  ; This fails!
      'success)))

(define (parse-let-like)
  (with-return
    (if (some-condition)
        (return #f)
        'let-result)))
```

## Solutions Explored

### Solution 1: Remove with-return from inner functions
Remove `with-return` from functions that are called within other `with-return` contexts.

### Solution 2: Use exceptions instead of continuations
Replace `with-return` with exception handling for error propagation.

### Solution 3: Pass continuation explicitly
Instead of relying on lexical binding, pass the continuation as a parameter.

### Solution 4: Use a single with-return at top level
Only use `with-return` at the top-level parsing functions, not in helper functions.
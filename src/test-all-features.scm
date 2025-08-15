#!/usr/bin/env guile
!#
;;; Test all implemented features: array operations, lambda shorthand, string interpolation

(add-to-load-path ".")
(use-modules (monkey object object)
             (monkey object environment)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey evaluator evaluator)
             (srfi srfi-64))

(test-begin "all-features")

(define (test-eval input)
  "Helper to evaluate Monkey code and return result"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (eval-program program env)))

;;; ============================================================================
;;; Array Operations Tests
;;; ============================================================================

(test-group "array-operations"
  (test-equal "map doubles elements"
    "[2, 4, 6]"
    (object->string (test-eval "map([1, 2, 3], fn(x) { x * 2 })")))
  
  (test-equal "filter keeps even numbers"
    "[2, 4]"
    (object->string (test-eval "filter([1, 2, 3, 4], fn(x) { x - (x / 2) * 2 == 0 })")))
  
  (test-equal "reduce sums array"
    "10"
    (object->string (test-eval "reduce([1, 2, 3, 4], fn(a, b) { a + b }, 0)")))
  
  (test-equal "reduce with multiplication"
    "24"
    (object->string (test-eval "reduce([1, 2, 3, 4], fn(a, b) { a * b }, 1)")))
  
  (test-equal "sort orders array"
    "[1, 1, 2, 3, 4, 5]"
    (object->string (test-eval "sort([3, 1, 4, 1, 5, 2])")))
  
  (test-equal "chaining operations"
    "[4, 6]"
    (object->string (test-eval "let nums = [1, 2, 3]; map(filter(nums, fn(x) { x > 1 }), fn(x) { x * 2 })"))))

;;; ============================================================================
;;; Lambda Shorthand Tests
;;; ============================================================================

(test-group "lambda-shorthand"
  (test-equal "single parameter"
    "5"
    (object->string (test-eval "let add_one = |x| x + 1; add_one(4)")))
  
  (test-equal "two parameters"
    "7"
    (object->string (test-eval "let add = |x, y| x + y; add(3, 4)")))
  
  (test-equal "no parameters"
    "42"
    (object->string (test-eval "let get_42 = || 42; get_42()")))
  
  (test-equal "with map"
    "[2, 4, 6]"
    (object->string (test-eval "map([1, 2, 3], |x| x * 2)")))
  
  (test-equal "with filter"
    "[2, 3, 4]"
    (object->string (test-eval "filter([1, 2, 3, 4], |x| x > 1)")))
  
  (test-equal "with reduce"
    "10"
    (object->string (test-eval "reduce([1, 2, 3, 4], |a, b| a + b, 0)")))
  
  (test-equal "nested lambda"
    "9"
    (object->string (test-eval "let twice = |f| |x| f(f(x)); let add3 = |x| x + 3; twice(add3)(3)")))
  
  (test-equal "comparison lambda"
    "[3, 4, 5]"
    (object->string (test-eval "filter([1, 2, 3, 4, 5], |x| x > 2)"))))

;;; ============================================================================
;;; String Interpolation Tests
;;; ============================================================================

(test-group "string-interpolation"
  (test-equal "single value"
    "Hello, World!"
    (object->string (test-eval "format(\"Hello, {0}!\", \"World\")")))
  
  (test-equal "multiple values"
    "The sum of 3 and 4 is 7"
    (object->string (test-eval "format(\"The sum of {0} and {1} is {2}\", 3, 4, 7)")))
  
  (test-equal "with variables"
    "User: Alice, Age: 30"
    (object->string (test-eval "let name = \"Alice\"; let age = 30; format(\"User: {0}, Age: {1}\", name, age)")))
  
  (test-equal "with expressions"
    "Result: 12"
    (object->string (test-eval "format(\"Result: {0}\", 3 * 4)")))
  
  (test-equal "with array"
    "Array: [1, 2, 3]"
    (object->string (test-eval "format(\"Array: {0}\", [1, 2, 3])")))
  
  (test-equal "empty format"
    "No interpolation"
    (object->string (test-eval "format(\"No interpolation\")"))))

;;; ============================================================================
;;; Integration Tests - Combining Features
;;; ============================================================================

(test-group "integration"
  (test-equal "lambda with array ops"
    "[4, 16]"
    (object->string (test-eval "map(filter([1, 2, 3, 4], |x| x - (x / 2) * 2 == 0), |x| x * x)")))
  
  (test-equal "format with computed values"
    "Sum: 10, Product: 24"
    (object->string (test-eval "let nums = [1, 2, 3, 4]; format(\"Sum: {0}, Product: {1}\", reduce(nums, |a, b| a + b, 0), reduce(nums, |a, b| a * b, 1))")))
  
  (test-equal "complex chain"
    "Results: [2, 4, 6]"
    (object->string (test-eval "let transform = |arr| map(arr, |x| x * 2); format(\"Results: {0}\", transform([1, 2, 3]))"))))

(test-end "all-features")

;; Display results
(define runner (test-runner-current))
(format #t "\n")
(format #t "All Features Test Results\n")
(format #t "==========================\n")
(format #t "Passed: ~a\n" (test-runner-pass-count runner))
(format #t "Failed: ~a\n" (test-runner-fail-count runner))
(format #t "Total:  ~a\n" 
        (+ (test-runner-pass-count runner)
           (test-runner-fail-count runner)))
(format #t "==========================\n")

(when (> (test-runner-fail-count runner) 0)
  (format #t "\nNote: Some tests failed. Check the log for details.\n"))

(exit (if (zero? (test-runner-fail-count runner)) 0 1))
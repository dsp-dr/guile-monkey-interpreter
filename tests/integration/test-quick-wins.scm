#!/usr/bin/env guile
!#
;;; Test Quick Win Extensions

(add-to-load-path (dirname (current-filename)))
(use-modules (monkey object object)
             (monkey object environment)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey evaluator evaluator)
             (srfi srfi-64))

(test-begin "quick-wins")

(define (test-eval input)
  "Helper to evaluate Monkey code and return result"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (eval-program program env)))

;;; Array Operations Tests

(test-group "array-operations"
  ;; Test map
  (test-equal "map doubles elements"
    "[2, 4, 6]"
    (object->string (test-eval "map([1, 2, 3], fn(x) { x * 2 })")))
  
  ;; Test filter
  (test-equal "filter keeps even numbers"
    "[2, 4]"
    (object->string (test-eval "filter([1, 2, 3, 4], fn(x) { x - (x / 2) * 2 == 0 })")))
  
  ;; Test reduce
  (test-equal "reduce sums array"
    "10"
    (object->string (test-eval "reduce([1, 2, 3, 4], fn(a, b) { a + b }, 0)")))
  
  ;; Test sort
  (test-equal "sort orders array"
    "[1, 1, 2, 3, 4, 5]"
    (object->string (test-eval "sort([3, 1, 4, 1, 5, 2])"))))

;;; String Functions Tests

(test-group "string-functions"
  ;; Test trim
  (test-equal "trim removes whitespace"
    "hello"
    (object->string (test-eval "trim(\"  hello  \")")))
  
  ;; Test replace
  (test-equal "replace substitutes string"
    "hello monkey"
    (object->string (test-eval "replace(\"hello world\", \"world\", \"monkey\")")))
  
  ;; Test substring
  (test-equal "substring extracts portion"
    "hello"
    (object->string (test-eval "substring(\"hello world\", 0, 5)"))))

;;; Math Functions Tests

(test-group "math-functions"
  ;; Test abs
  (test-equal "abs of negative"
    "42"
    (object->string (test-eval "abs(-42)")))
  
  (test-equal "abs of positive"
    "42"
    (object->string (test-eval "abs(42)")))
  
  ;; Test min
  (test-equal "min finds smallest"
    "1"
    (object->string (test-eval "min(5, 2, 8, 1, 9)")))
  
  ;; Test max
  (test-equal "max finds largest"
    "9"
    (object->string (test-eval "max(5, 2, 8, 1, 9)"))))

;;; Integration test
(test-group "integration"
  (test-equal "complex expression with new functions"
    "20"
    (object->string 
      (test-eval 
        "let nums = [1, 2, 3, 4, 5];
         let doubled = map(nums, fn(x) { x * 2 });
         reduce(doubled, fn(a, b) { a + b }, 0)"))))

(test-end "quick-wins")

;; Display results
(define runner (test-runner-current))
(format #t "\n")
(format #t "Quick Win Extensions Test Results\n")
(format #t "==================================\n")
(format #t "Passed: ~a\n" (test-runner-pass-count runner))
(format #t "Failed: ~a\n" (test-runner-fail-count runner))
(format #t "Total:  ~a\n" 
        (+ (test-runner-pass-count runner)
           (test-runner-fail-count runner)))
(format #t "==================================\n")

(exit (if (zero? (test-runner-fail-count runner)) 0 1))
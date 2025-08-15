#!/usr/bin/env guile
!#
;;; Test for loops and break/continue

(add-to-load-path ".")
(use-modules (monkey object object)
             (monkey object environment)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey evaluator evaluator)
             (srfi srfi-64))

(test-begin "for-loops")

(define (test-eval input)
  "Helper to evaluate Monkey code and return result"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (if (null? (parser-errors parser))
        (eval-program program env)
        (begin
          (format #t "Parser errors: ~a~%" (parser-errors parser))
          (make-error-object "Parse error")))))

;;; Basic for loop tests
(test-group "basic-for-loops"
  (test-equal "simple counting loop"
    "10"
    (object->string 
      (test-eval "let sum = 0; for (let i = 0; i < 5; i = i + 1) { let sum = sum + i; } sum")))
  
  (test-equal "for loop with array"
    "[0, 1, 2, 3, 4]"
    (object->string
      (test-eval "let arr = []; for (let i = 0; i < 5; i = i + 1) { let arr = push(arr, i); } arr")))
  
  (test-equal "nested for loops"
    "9"
    (object->string
      (test-eval "let count = 0; for (let i = 0; i < 3; i = i + 1) { for (let j = 0; j < 3; j = j + 1) { let count = count + 1; } } count")))
  
  (test-equal "for loop without init"
    "5"
    (object->string
      (test-eval "let i = 0; let sum = 0; for (; i < 5; i = i + 1) { let sum = sum + 1; } sum")))
  
  (test-equal "for loop without update"
    "3"
    (object->string
      (test-eval "let count = 0; for (let i = 0; i < 3;) { let count = count + 1; let i = i + 1; } count"))))

;;; Break statement tests
(test-group "break-statement"
  (test-equal "break in for loop"
    "3"
    (object->string
      (test-eval "let sum = 0; for (let i = 0; i < 10; i = i + 1) { if (i == 3) { break; } let sum = sum + 1; } sum")))
  
  (test-equal "break in while loop"
    "5"
    (object->string
      (test-eval "let i = 0; while (i < 10) { if (i == 5) { break; } let i = i + 1; } i")))
  
  (test-equal "break in nested loops"
    "6"
    (object->string
      (test-eval "let count = 0; for (let i = 0; i < 3; i = i + 1) { for (let j = 0; j < 10; j = j + 1) { if (j == 2) { break; } let count = count + 1; } } count"))))

;;; Continue statement tests
(test-group "continue-statement"
  (test-equal "continue in for loop"
    "12"
    (object->string
      (test-eval "let sum = 0; for (let i = 0; i < 5; i = i + 1) { if (i == 2) { continue; } let sum = sum + i; } sum")))
  
  (test-equal "continue in while loop"
    "20"
    (object->string
      (test-eval "let sum = 0; let i = 0; while (i < 6) { let i = i + 1; if (i == 3) { continue; } let sum = sum + i; } sum")))
  
  (test-equal "continue with update"
    "[0, 1, 3, 4]"
    (object->string
      (test-eval "let arr = []; for (let i = 0; i < 5; i = i + 1) { if (i == 2) { continue; } let arr = push(arr, i); } arr"))))

;;; Complex examples
(test-group "complex-examples"
  (test-equal "prime numbers"
    "[2, 3, 5, 7]"
    (object->string
      (test-eval "
        let primes = [];
        for (let n = 2; n < 10; n = n + 1) {
          let is_prime = true;
          for (let i = 2; i < n; i = i + 1) {
            if (n - (n / i) * i == 0) {
              let is_prime = false;
              break;
            }
          }
          if (is_prime) {
            let primes = push(primes, n);
          }
        }
        primes")))
  
  (test-equal "fibonacci with for loop"
    "55"
    (object->string
      (test-eval "
        let a = 0;
        let b = 1;
        let fib = 0;
        for (let i = 0; i < 10; i = i + 1) {
          let fib = a + b;
          let a = b;
          let b = fib;
        }
        fib"))))

(test-end "for-loops")

;; Display results
(define runner (test-runner-current))
(format #t "\n")
(format #t "For Loops Test Results\n")
(format #t "======================\n")
(format #t "Passed: ~a\n" (test-runner-pass-count runner))
(format #t "Failed: ~a\n" (test-runner-fail-count runner))
(format #t "Total:  ~a\n" 
        (+ (test-runner-pass-count runner)
           (test-runner-fail-count runner)))
(format #t "======================\n")

(exit (if (zero? (test-runner-fail-count runner)) 0 1))
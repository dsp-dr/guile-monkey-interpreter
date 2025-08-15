#!/usr/bin/env guile
!#
;;; Test lambda shorthand syntax

(add-to-load-path ".")
(use-modules (monkey object object)
             (monkey object environment)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey evaluator evaluator)
             (srfi srfi-64))

(test-begin "lambda-shorthand")

(define (test-eval input)
  "Helper to evaluate Monkey code and return result"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (eval-program program env)))

;;; Test basic lambda shorthand
(test-group "basic-lambda"
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
  
  (test-equal "nested lambda"
    "9"
    (object->string (test-eval "let twice = |f| |x| f(f(x)); let add3 = |x| x + 3; twice(add3)(3)"))))

(test-end "lambda-shorthand")

;; Display results
(define runner (test-runner-current))
(format #t "\n")
(format #t "Lambda Shorthand Test Results\n")
(format #t "==============================\n")
(format #t "Passed: ~a\n" (test-runner-pass-count runner))
(format #t "Failed: ~a\n" (test-runner-fail-count runner))
(format #t "Total:  ~a\n" 
        (+ (test-runner-pass-count runner)
           (test-runner-fail-count runner)))
(format #t "==============================\n")

(exit (if (zero? (test-runner-fail-count runner)) 0 1))
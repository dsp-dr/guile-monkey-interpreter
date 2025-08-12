#!/usr/bin/env guile
!#
;;; Chapter 03 - Evaluator Test Runner
;;; Run all evaluator tests

(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (srfi srfi-64)
             (monkey token token)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey ast ast)
             (monkey object object)
             (monkey object environment)
             (monkey evaluator evaluator)
             (ice-9 format))

(test-runner-factory
 (lambda ()
   (let ((runner (test-runner-simple)))
     (test-runner-on-test-end! runner
       (lambda (runner)
         (let ((name (test-runner-test-name runner))
               (result (test-result-kind runner)))
           (case result
             ((pass) (format #t "✓ ~a~%" name))
             ((fail) (format #t "✗ ~a~%" name))
             ((xpass) (format #t "⚠ ~a (unexpected pass)~%" name))
             ((xfail) (format #t "⚠ ~a (expected fail)~%" name))
             ((skip) (format #t "○ ~a (skipped)~%" name))))))
     runner)))

(test-begin "Chapter 03 - Evaluator Tests")

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(define (test-eval input)
  "Helper to evaluate input and return result"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (eval program env)))

(define (test-integer-object obj expected)
  "Test if object is integer with expected value"
  (test-assert "is integer object" (integer-object? obj))
  (when (integer-object? obj)
    (test-equal "integer value" expected (integer-object-value obj))))

(define (test-boolean-object obj expected)
  "Test if object is boolean with expected value"
  (test-assert "is boolean object" (boolean-object? obj))
  (when (boolean-object? obj)
    (test-equal "boolean value" expected (boolean-object-value obj))))

(define (test-null-object obj)
  "Test if object is null"
  (test-assert "is null object" (null-object? obj)))

;;; ============================================================================
;;; Integer Literal Tests
;;; ============================================================================

(test-group "Integer literals"
  (let ((tests '(("5" 5)
                 ("10" 10)
                 ("-5" -5)
                 ("-10" -10)
                 ("5 + 5 + 5 + 5 - 10" 10)
                 ("2 * 2 * 2 * 2 * 2" 32)
                 ("-50 + 100 + -50" 0)
                 ("5 * 2 + 10" 20)
                 ("5 + 2 * 10" 25)
                 ("20 + 2 * -10" 0)
                 ("50 / 2 * 2 + 10" 60)
                 ("2 * (5 + 10)" 30)
                 ("3 * 3 * 3 + 10" 37)
                 ("3 * (3 * 3) + 10" 37)
                 ("(5 + 10 * 2 + 15 / 3) * 2 + -10" 50))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input) 
                       expected
                       (if (integer-object? result)
                           (integer-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; Boolean Expression Tests
;;; ============================================================================

(test-group "Boolean expressions"
  (let ((tests '(("true" #t)
                 ("false" #f)
                 ("1 < 2" #t)
                 ("1 > 2" #f)
                 ("1 < 1" #f)
                 ("1 > 1" #f)
                 ("1 == 1" #t)
                 ("1 != 1" #f)
                 ("1 == 2" #f)
                 ("1 != 2" #t)
                 ("true == true" #t)
                 ("false == false" #t)
                 ("true == false" #f)
                 ("true != false" #t)
                 ("false != true" #t)
                 ("(1 < 2) == true" #t)
                 ("(1 < 2) == false" #f)
                 ("(1 > 2) == true" #f)
                 ("(1 > 2) == false" #t))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input)
                       expected
                       (if (boolean-object? result)
                           (boolean-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; Bang Operator Tests
;;; ============================================================================

(test-group "Bang operator"
  (let ((tests '(("!true" #f)
                 ("!false" #t)
                 ("!5" #f)
                 ("!!true" #t)
                 ("!!false" #f)
                 ("!!5" #t))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input)
                       expected
                       (if (boolean-object? result)
                           (boolean-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; If Expression Tests
;;; ============================================================================

(test-group "If expressions"
  (let ((tests '(("if (true) { 10 }" 10)
                 ("if (false) { 10 }" null)
                 ("if (1) { 10 }" 10)
                 ("if (1 < 2) { 10 }" 10)
                 ("if (1 > 2) { 10 }" null)
                 ("if (1 > 2) { 10 } else { 20 }" 20)
                 ("if (1 < 2) { 10 } else { 20 }" 10))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (cond
            ((eq? expected 'null)
             (test-assert (format #f "eval: ~a is null" input)
                          (null-object? result)))
            (else
             (test-equal (format #f "eval: ~a" input)
                         expected
                         (if (integer-object? result)
                             (integer-object-value result)
                             #f)))))))
     tests)))

;;; ============================================================================
;;; Return Statement Tests
;;; ============================================================================

(test-group "Return statements"
  (let ((tests '(("return 10;" 10)
                 ("return 10; 9;" 10)
                 ("return 2 * 5; 9;" 10)
                 ("9; return 2 * 5; 9;" 10)
                 ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }" 10))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input)
                       expected
                       (if (integer-object? result)
                           (integer-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; Let Statement Tests
;;; ============================================================================

(test-group "Let statements"
  (let ((tests '(("let a = 5; a;" 5)
                 ("let a = 5 * 5; a;" 25)
                 ("let a = 5; let b = a; b;" 5)
                 ("let a = 5; let b = a; let c = a + b + 5; c;" 15))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input)
                       expected
                       (if (integer-object? result)
                           (integer-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; Function Tests
;;; ============================================================================

(test-group "Function objects"
  (let ((input "fn(x) { x + 2; };"))
    (let ((result (test-eval input)))
      (test-assert "is function object" (function-object? result))
      (when (function-object? result)
        (test-equal "parameter count" 1 
                    (length (function-object-parameters result)))
        (test-assert "has body" 
                     (block-statement? (function-object-body result)))
        (test-assert "has environment"
                     (environment? (function-object-env result)))))))

(test-group "Function application"
  (let ((tests '(("let identity = fn(x) { x; }; identity(5);" 5)
                 ("let identity = fn(x) { return x; }; identity(5);" 5)
                 ("let double = fn(x) { x * 2; }; double(5);" 10)
                 ("let add = fn(x, y) { x + y; }; add(5, 5);" 10)
                 ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));" 20)
                 ("fn(x) { x; }(5)" 5))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input)
                       expected
                       (if (integer-object? result)
                           (integer-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; Closure Tests
;;; ============================================================================

(test-group "Closures"
  (let ((input "
let newAdder = fn(x) {
  fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);"))
    (let ((result (test-eval input)))
      (test-equal "closure result" 4
                  (if (integer-object? result)
                      (integer-object-value result)
                      #f)))))

;;; ============================================================================
;;; String Tests
;;; ============================================================================

(test-group "String literals"
  (let ((input "\"Hello World!\""))
    (let ((result (test-eval input)))
      (test-assert "is string object" (string-object? result))
      (when (string-object? result)
        (test-equal "string value" "Hello World!"
                    (string-object-value result))))))

(test-group "String concatenation"
  (let ((input "\"Hello\" + \" \" + \"World!\""))
    (let ((result (test-eval input)))
      (test-assert "is string object" (string-object? result))
      (when (string-object? result)
        (test-equal "concatenated string" "Hello World!"
                    (string-object-value result))))))

;;; ============================================================================
;;; Built-in Function Tests
;;; ============================================================================

(test-group "Built-in len function"
  (let ((tests '(("len(\"\")" 0)
                 ("len(\"four\")" 4)
                 ("len(\"hello world\")" 11)
                 ("len([1, 2, 3])" 3)
                 ("len([])" 0))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (test-equal (format #f "eval: ~a" input)
                       expected
                       (if (integer-object? result)
                           (integer-object-value result)
                           #f)))))
     tests)))

;;; ============================================================================
;;; Array Tests
;;; ============================================================================

(test-group "Array literals"
  (let ((input "[1, 2 * 2, 3 + 3]"))
    (let ((result (test-eval input)))
      (test-assert "is array object" (array-object? result))
      (when (array-object? result)
        (let ((elements (array-object-elements result)))
          (test-equal "array length" 3 (length elements))
          (test-equal "first element" 1 
                      (integer-object-value (car elements)))
          (test-equal "second element" 4
                      (integer-object-value (cadr elements)))
          (test-equal "third element" 6
                      (integer-object-value (caddr elements))))))))

(test-group "Array indexing"
  (let ((tests '(("[1, 2, 3][0]" 1)
                 ("[1, 2, 3][1]" 2)
                 ("[1, 2, 3][2]" 3)
                 ("let i = 0; [1][i];" 1)
                 ("[1, 2, 3][1 + 1];" 3)
                 ("let myArray = [1, 2, 3]; myArray[2];" 3)
                 ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];" 6)
                 ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]" 2)
                 ("[1, 2, 3][3]" null)
                 ("[1, 2, 3][-1]" null))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (cond
            ((eq? expected 'null)
             (test-assert (format #f "eval: ~a is null" input)
                          (null-object? result)))
            (else
             (test-equal (format #f "eval: ~a" input)
                         expected
                         (if (integer-object? result)
                             (integer-object-value result)
                             #f)))))))
     tests)))

;;; ============================================================================
;;; Hash Tests
;;; ============================================================================

(test-group "Hash literals"
  (let ((input "{\"foo\": 5, \"bar\": 10}"))
    (let ((result (test-eval input)))
      (test-assert "is hash object" (hash-object? result))
      (when (hash-object? result)
        (test-equal "hash size" 2 
                    (length (hash-object-pairs result)))))))

(test-group "Hash indexing"
  (let ((tests '(("{\"foo\": 5}[\"foo\"]" 5)
                 ("{\"foo\": 5}[\"bar\"]" null)
                 ("let key = \"foo\"; {\"foo\": 5}[key]" 5)
                 ("{}[\"foo\"]" null)
                 ("{5: 5}[5]" 5)
                 ("{true: 5}[true]" 5)
                 ("{false: 5}[false]" 5))))
    
    (for-each
     (lambda (test)
       (let ((input (car test))
             (expected (cadr test)))
         (let ((result (test-eval input)))
           (cond
            ((eq? expected 'null)
             (test-assert (format #f "eval: ~a is null" input)
                          (null-object? result)))
            (else
             (test-equal (format #f "eval: ~a" input)
                         expected
                         (if (integer-object? result)
                             (integer-object-value result)
                             #f)))))))
     tests)))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(test-group "Error handling"
  (let ((tests '(("5 + true;"
                  "5 + true; 5;"
                  "true + false;"
                  "5; true + false; 5"
                  "if (10 > 1) { true + false; }"
                  "foobar"
                  "\"Hello\" - \"World\""))))
    
    (for-each
     (lambda (input)
       (let ((result (test-eval input)))
         (test-assert (format #f "eval: ~a produces error" input)
                      (error-object? result))))
     tests)))

;;; ============================================================================
;;; While Loop Tests
;;; ============================================================================

(test-group "While loops"
  (let ((input "let x = 0; while (x < 5) { let x = x + 1; }; x"))
    (let ((result (test-eval input)))
      (test-equal "while loop counter" 5
                  (if (integer-object? result)
                      (integer-object-value result)
                      #f)))))

;;; ============================================================================
;;; Recursive Function Tests
;;; ============================================================================

(test-group "Recursive functions"
  (let ((input "
let factorial = fn(n) {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
};
factorial(5);"))
    (let ((result (test-eval input)))
      (test-equal "factorial(5)" 120
                  (if (integer-object? result)
                      (integer-object-value result)
                      #f))))
  
  (let ((input "
let fibonacci = fn(n) {
  if (n < 2) {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
};
fibonacci(10);"))
    (let ((result (test-eval input)))
      (test-equal "fibonacci(10)" 55
                  (if (integer-object? result)
                      (integer-object-value result)
                      #f)))))

(test-end "Chapter 03 - Evaluator Tests")

;; Print test summary
(let ((runner (test-runner-current)))
  (format #t "\n")
  (format #t "========================================\n")
  (format #t "Chapter 03 - Evaluator Test Results\n")
  (format #t "========================================\n")
  (format #t "Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "Total:  ~a\n" (+ (test-runner-pass-count runner)
                              (test-runner-fail-count runner)))
  (format #t "========================================\n")
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))
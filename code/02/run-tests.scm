#!/usr/bin/env guile
!#
;;; Chapter 02 - Parser Test Runner
;;; Run all parser tests

(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (srfi srfi-64)
             (monkey token token)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey ast ast)
             (ice-9 format)
             (ice-9 match))

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

(test-begin "Chapter 02 - Parser Tests")

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(define (parse-input input)
  "Parse input string and return program"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser)))
    (when (not (null? (parser-errors parser)))
      (for-each (lambda (err)
                  (format #t "Parser error: ~a~%" err))
                (parser-errors parser)))
    program))

(define (test-integer-literal expr expected-value)
  "Test if expression is integer literal with expected value"
  (and (integer-literal? expr)
       (= (integer-literal-value expr) expected-value)))

(define (test-identifier expr expected-value)
  "Test if expression is identifier with expected value"
  (and (identifier? expr)
       (string=? (identifier-value expr) expected-value)))

(define (test-boolean-literal expr expected-value)
  "Test if expression is boolean with expected value"
  (and (boolean? expr)
       (eq? (boolean-value expr) expected-value)))

(define (test-string-literal expr expected-value)
  "Test if expression is string literal with expected value"
  (and (string-literal? expr)
       (string=? (string-literal-value expr) expected-value)))

;;; ============================================================================
;;; Let Statement Tests
;;; ============================================================================

(test-group "Let statements"
  (let* ((input "let x = 5;
                 let y = true;
                 let foobar = y;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "Three statements" 3 (length stmts))
    
    (test-assert "First is let statement" (let-statement? (car stmts)))
    (test-equal "First identifier" "x" 
                (identifier-value (let-statement-name (car stmts))))
    (test-assert "First value is 5"
                 (test-integer-literal (let-statement-value (car stmts)) 5))
    
    (test-assert "Second is let statement" (let-statement? (cadr stmts)))
    (test-equal "Second identifier" "y"
                (identifier-value (let-statement-name (cadr stmts))))
    (test-assert "Second value is true"
                 (test-boolean-literal (let-statement-value (cadr stmts)) #t))
    
    (test-assert "Third is let statement" (let-statement? (caddr stmts)))
    (test-equal "Third identifier" "foobar"
                (identifier-value (let-statement-name (caddr stmts))))
    (test-assert "Third value is y"
                 (test-identifier (let-statement-value (caddr stmts)) "y"))))

;;; ============================================================================
;;; Return Statement Tests
;;; ============================================================================

(test-group "Return statements"
  (let* ((input "return 5;
                 return true;
                 return foobar;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "Three statements" 3 (length stmts))
    
    (test-assert "First is return" (return-statement? (car stmts)))
    (test-assert "First value is 5"
                 (test-integer-literal (return-statement-value (car stmts)) 5))
    
    (test-assert "Second is return" (return-statement? (cadr stmts)))
    (test-assert "Second value is true"
                 (test-boolean-literal (return-statement-value (cadr stmts)) #t))
    
    (test-assert "Third is return" (return-statement? (caddr stmts)))
    (test-assert "Third value is foobar"
                 (test-identifier (return-statement-value (caddr stmts)) "foobar"))))

;;; ============================================================================
;;; Expression Statement Tests
;;; ============================================================================

(test-group "Expression statements"
  (let* ((input "x;
                 5;
                 true;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "Three statements" 3 (length stmts))
    
    (test-assert "First is expression statement" 
                 (expression-statement? (car stmts)))
    (test-assert "First is identifier x"
                 (test-identifier (expression-statement-expression (car stmts)) "x"))
    
    (test-assert "Second is expression statement"
                 (expression-statement? (cadr stmts)))
    (test-assert "Second is integer 5"
                 (test-integer-literal (expression-statement-expression (cadr stmts)) 5))
    
    (test-assert "Third is expression statement"
                 (expression-statement? (caddr stmts)))
    (test-assert "Third is boolean true"
                 (test-boolean-literal (expression-statement-expression (caddr stmts)) #t))))

;;; ============================================================================
;;; Prefix Expression Tests
;;; ============================================================================

(test-group "Prefix expressions"
  (let ((tests '(("!5" "!" 5)
                 ("-15" "-" 15)
                 ("!true" "!" #t)
                 ("!false" "!" #f))))
    
    (for-each
     (match-lambda
       ((input op value)
        (let* ((program (parse-input input))
               (stmt (car (program-statements program)))
               (expr (expression-statement-expression stmt)))
          
          (test-assert (format #f "~a is prefix expression" input)
                       (prefix-expression? expr))
          (test-equal (format #f "~a operator" input) op
                      (prefix-expression-operator expr))
          (cond
           ((number? value)
            (test-assert (format #f "~a right value" input)
                         (test-integer-literal (prefix-expression-right expr) value)))
           ((boolean? value)
            (test-assert (format #f "~a right value" input)
                         (test-boolean-literal (prefix-expression-right expr) value)))))))
     tests)))

;;; ============================================================================
;;; Infix Expression Tests
;;; ============================================================================

(test-group "Infix expressions"
  (let ((tests '(("5 + 5" 5 "+" 5)
                 ("5 - 5" 5 "-" 5)
                 ("5 * 5" 5 "*" 5)
                 ("5 / 5" 5 "/" 5)
                 ("5 > 5" 5 ">" 5)
                 ("5 < 5" 5 "<" 5)
                 ("5 == 5" 5 "==" 5)
                 ("5 != 5" 5 "!=" 5))))
    
    (for-each
     (match-lambda
       ((input left op right)
        (let* ((program (parse-input input))
               (stmt (car (program-statements program)))
               (expr (expression-statement-expression stmt)))
          
          (test-assert (format #f "~a is infix expression" input)
                       (infix-expression? expr))
          (test-assert (format #f "~a left value" input)
                       (test-integer-literal (infix-expression-left expr) left))
          (test-equal (format #f "~a operator" input) op
                      (infix-expression-operator expr))
          (test-assert (format #f "~a right value" input)
                       (test-integer-literal (infix-expression-right expr) right)))))
     tests)))

;;; ============================================================================
;;; Operator Precedence Tests
;;; ============================================================================

(test-group "Operator precedence"
  (let ((tests '(("-a * b" "((-a) * b)")
                 ("!-a" "(!(-a))")
                 ("a + b + c" "((a + b) + c)")
                 ("a + b - c" "((a + b) - c)")
                 ("a * b * c" "((a * b) * c)")
                 ("a * b / c" "((a * b) / c)")
                 ("a + b / c" "(a + (b / c))")
                 ("a + b * c + d / e - f" "(((a + (b * c)) + (d / e)) - f)")
                 ("5 > 4 == 3 < 4" "((5 > 4) == (3 < 4))")
                 ("5 < 4 != 3 > 4" "((5 < 4) != (3 > 4))")
                 ("3 + 4 * 5 == 3 * 1 + 4 * 5" "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
                 ("1 + (2 + 3) + 4" "((1 + (2 + 3)) + 4)")
                 ("(5 + 5) * 2" "((5 + 5) * 2)")
                 ("2 / (5 + 5)" "(2 / (5 + 5))")
                 ("-(5 + 5)" "(-(5 + 5))")
                 ("!(true == true)" "(!(true == true))"))))
    
    (for-each
     (match-lambda
       ((input expected)
        (let ((program (parse-input input)))
          (test-equal (format #f "Precedence: ~a" input)
                      expected
                      (program->string program)))))
     tests)))

;;; ============================================================================
;;; If Expression Tests
;;; ============================================================================

(test-group "If expressions"
  (let* ((input "if (x < y) { x }")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is if expression" (if-expression? expr))
    (test-assert "Condition is infix" 
                 (infix-expression? (if-expression-condition expr)))
    (test-assert "Consequence is block"
                 (block-statement? (if-expression-consequence expr)))
    (test-equal "One statement in consequence" 1
                (length (block-statement-statements 
                        (if-expression-consequence expr))))
    (test-assert "No alternative"
                 (not (if-expression-alternative expr)))))

(test-group "If-else expressions"
  (let* ((input "if (x < y) { x } else { y }")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is if expression" (if-expression? expr))
    (test-assert "Has alternative"
                 (block-statement? (if-expression-alternative expr)))
    (test-equal "One statement in alternative" 1
                (length (block-statement-statements
                        (if-expression-alternative expr))))))

;;; ============================================================================
;;; Function Literal Tests
;;; ============================================================================

(test-group "Function literals"
  (let* ((input "fn(x, y) { x + y; }")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is function literal" (function-literal? expr))
    (test-equal "Two parameters" 2 
                (length (function-literal-parameters expr)))
    (test-assert "First param is x"
                 (test-identifier (car (function-literal-parameters expr)) "x"))
    (test-assert "Second param is y"
                 (test-identifier (cadr (function-literal-parameters expr)) "y"))
    (test-assert "Body is block"
                 (block-statement? (function-literal-body expr)))
    (test-equal "One statement in body" 1
                (length (block-statement-statements
                        (function-literal-body expr))))))

(test-group "Function parameter variations"
  (let ((tests '(("fn() {};" 0 ())
                 ("fn(x) {};" 1 ("x"))
                 ("fn(x, y, z) {};" 3 ("x" "y" "z")))))
    
    (for-each
     (match-lambda
       ((input param-count param-names)
        (let* ((program (parse-input input))
               (stmt (car (program-statements program)))
               (fn (expression-statement-expression stmt)))
          
          (test-equal (format #f "Parameter count for ~a" input)
                      param-count
                      (length (function-literal-parameters fn)))
          
          (for-each
           (lambda (expected param)
             (test-assert (format #f "Parameter ~a" expected)
                          (test-identifier param expected)))
           param-names
           (function-literal-parameters fn)))))
     tests)))

;;; ============================================================================
;;; Call Expression Tests
;;; ============================================================================

(test-group "Call expressions"
  (let* ((input "add(1, 2 * 3, 4 + 5);")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is call expression" (call-expression? expr))
    (test-assert "Function is identifier"
                 (test-identifier (call-expression-function expr) "add"))
    (test-equal "Three arguments" 3
                (length (call-expression-arguments expr)))
    (test-assert "First arg is 1"
                 (test-integer-literal (car (call-expression-arguments expr)) 1))
    (test-assert "Second arg is infix"
                 (infix-expression? (cadr (call-expression-arguments expr))))
    (test-assert "Third arg is infix"
                 (infix-expression? (caddr (call-expression-arguments expr))))))

;;; ============================================================================
;;; String Literal Tests
;;; ============================================================================

(test-group "String literals"
  (let* ((input "\"hello world\";")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is string literal" (string-literal? expr))
    (test-equal "String value" "hello world"
                (string-literal-value expr))))

;;; ============================================================================
;;; Array Literal Tests
;;; ============================================================================

(test-group "Array literals"
  (let* ((input "[1, 2 * 2, 3 + 3]")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is array literal" (array-literal? expr))
    (test-equal "Three elements" 3
                (length (array-literal-elements expr)))
    (test-assert "First element is 1"
                 (test-integer-literal (car (array-literal-elements expr)) 1))
    (test-assert "Second element is infix"
                 (infix-expression? (cadr (array-literal-elements expr))))
    (test-assert "Third element is infix"
                 (infix-expression? (caddr (array-literal-elements expr))))))

(test-group "Empty array"
  (let* ((input "[]")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is array literal" (array-literal? expr))
    (test-equal "Zero elements" 0
                (length (array-literal-elements expr)))))

;;; ============================================================================
;;; Index Expression Tests
;;; ============================================================================

(test-group "Index expressions"
  (let* ((input "myArray[1 + 1]")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is index expression" (index-expression? expr))
    (test-assert "Left is identifier"
                 (test-identifier (index-expression-left expr) "myArray"))
    (test-assert "Index is infix"
                 (infix-expression? (index-expression-index expr)))))

;;; ============================================================================
;;; Hash Literal Tests
;;; ============================================================================

(test-group "Hash literals"
  (let* ((input "{\"one\": 1, \"two\": 2, \"three\": 3}")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is hash literal" (hash-literal? expr))
    (test-equal "Three pairs" 3
                (length (hash-literal-pairs expr)))
    
    (let ((first-pair (car (hash-literal-pairs expr))))
      (test-assert "First key is string"
                   (test-string-literal (car first-pair) "one"))
      (test-assert "First value is 1"
                   (test-integer-literal (cdr first-pair) 1)))))

(test-group "Empty hash"
  (let* ((input "{}")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is hash literal" (hash-literal? expr))
    (test-equal "Zero pairs" 0
                (length (hash-literal-pairs expr)))))

;;; ============================================================================
;;; While Expression Tests
;;; ============================================================================

(test-group "While expressions"
  (let* ((input "while (x < 10) { let x = x + 1; }")
         (program (parse-input input))
         (stmt (car (program-statements program)))
         (expr (expression-statement-expression stmt)))
    
    (test-assert "Is while expression" (while-expression? expr))
    (test-assert "Condition is infix"
                 (infix-expression? (while-expression-condition expr)))
    (test-assert "Body is block"
                 (block-statement? (while-expression-body expr)))
    (test-equal "One statement in body" 1
                (length (block-statement-statements
                        (while-expression-body expr))))))

;;; ============================================================================
;;; Complex Program Tests
;;; ============================================================================

(test-group "Complex programs"
  (let* ((input "
let fibonacci = fn(n) {
  if (n < 2) {
    return n;
  }
  return fibonacci(n - 1) + fibonacci(n - 2);
};

let result = fibonacci(10);
")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "Two statements" 2 (length stmts))
    
    (test-assert "First is let statement" (let-statement? (car stmts)))
    (test-equal "First identifier" "fibonacci"
                (identifier-value (let-statement-name (car stmts))))
    (test-assert "First value is function"
                 (function-literal? (let-statement-value (car stmts))))
    
    (test-assert "Second is let statement" (let-statement? (cadr stmts)))
    (test-equal "Second identifier" "result"
                (identifier-value (let-statement-name (cadr stmts))))
    (test-assert "Second value is call"
                 (call-expression? (let-statement-value (cadr stmts))))))

(test-end "Chapter 02 - Parser Tests")

;; Print test summary
(let ((runner (test-runner-current)))
  (format #t "\n")
  (format #t "========================================\n")
  (format #t "Chapter 02 - Parser Test Results\n")
  (format #t "========================================\n")
  (format #t "Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "Total:  ~a\n" (+ (test-runner-pass-count runner)
                              (test-runner-fail-count runner)))
  (format #t "========================================\n")
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))
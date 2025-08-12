;;; Parser Test Suite
;;; Tests for the Pratt parser implementation

(use-modules (srfi srfi-64)
             (monkey token token)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey ast ast)
             (ice-9 match))

(test-begin "parser")

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(define (parse-input input)
  "Parse input string and return program"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer)))
    (parse-program parser)))

(define (check-parser-errors parser)
  "Check for parser errors and fail test if any"
  (let ((errors (parser-errors parser)))
    (unless (null? errors)
      (for-each (lambda (err)
                  (format #t "Parser error: ~a~%" err))
                errors)
      (test-assert "No parser errors" #f))))

(define (test-integer-literal expr value)
  "Test if expression is integer literal with given value"
  (test-assert "is integer literal" (integer-literal? expr))
  (when (integer-literal? expr)
    (test-equal "integer value" value (integer-literal-value expr))))

(define (test-identifier expr value)
  "Test if expression is identifier with given value"
  (test-assert "is identifier" (identifier? expr))
  (when (identifier? expr)
    (test-equal "identifier value" value (identifier-value expr))))

(define (test-boolean expr value)
  "Test if expression is boolean with given value"
  (test-assert "is boolean" (boolean? expr))
  (when (boolean? expr)
    (test-equal "boolean value" value (boolean-value expr))))

(define (test-literal-expression expr expected)
  "Test literal expression value"
  (cond
   ((number? expected) (test-integer-literal expr expected))
   ((string? expected) (test-identifier expr expected))
   ((boolean? expected) (test-boolean expr expected))
   (else (test-assert "Unknown literal type" #f))))

(define (test-infix-expression expr left op right)
  "Test infix expression structure"
  (test-assert "is infix expression" (infix-expression? expr))
  (when (infix-expression? expr)
    (test-literal-expression (infix-expression-left expr) left)
    (test-equal "operator" op (infix-expression-operator expr))
    (test-literal-expression (infix-expression-right expr) right)))

;;; ============================================================================
;;; Let Statement Tests
;;; ============================================================================

(test-group "let statements"
  (let* ((input "let x = 5;
                 let y = 10;
                 let foobar = 838383;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 3 (length stmts))
    
    (let ((tests '(("x" . 5)
                   ("y" . 10)
                   ("foobar" . 838383))))
      (for-each
       (match-lambda*
         (((name . value) stmt)
          (test-assert "is let statement" (let-statement? stmt))
          (when (let-statement? stmt)
            (test-equal "identifier name" name 
                        (identifier-value (let-statement-name stmt)))
            (test-integer-literal (let-statement-value stmt) value))))
       tests stmts))))

;;; ============================================================================
;;; Return Statement Tests
;;; ============================================================================

(test-group "return statements"
  (let* ((input "return 5;
                 return 10;
                 return 993322;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 3 (length stmts))
    
    (for-each
     (lambda (stmt)
       (test-assert "is return statement" (return-statement? stmt)))
     stmts)))

;;; ============================================================================
;;; Identifier Expression Tests
;;; ============================================================================

(test-group "identifier expression"
  (let* ((input "foobar;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-identifier expr "foobar"))))))

;;; ============================================================================
;;; Integer Literal Tests
;;; ============================================================================

(test-group "integer literal expression"
  (let* ((input "5;")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-integer-literal expr 5))))))

;;; ============================================================================
;;; Prefix Expression Tests
;;; ============================================================================

(test-group "prefix expressions"
  (let ((tests '(("!5" "!" 5)
                 ("-15" "-" 15)
                 ("!true" "!" #t)
                 ("!false" "!" #f))))
    
    (for-each
     (match-lambda
       ((input op value)
        (let* ((program (parse-input input))
               (stmts (program-statements program)))
          
          (test-equal "statement count" 1 (length stmts))
          
          (let ((stmt (car stmts)))
            (test-assert "is expression statement" (expression-statement? stmt))
            (when (expression-statement? stmt)
              (let ((expr (expression-statement-expression stmt)))
                (test-assert "is prefix expression" (prefix-expression? expr))
                (when (prefix-expression? expr)
                  (test-equal "operator" op (prefix-expression-operator expr))
                  (test-literal-expression (prefix-expression-right expr) value))))))))
     tests)))

;;; ============================================================================
;;; Infix Expression Tests
;;; ============================================================================

(test-group "infix expressions"
  (let ((tests '(("5 + 5" 5 "+" 5)
                 ("5 - 5" 5 "-" 5)
                 ("5 * 5" 5 "*" 5)
                 ("5 / 5" 5 "/" 5)
                 ("5 > 5" 5 ">" 5)
                 ("5 < 5" 5 "<" 5)
                 ("5 == 5" 5 "==" 5)
                 ("5 != 5" 5 "!=" 5)
                 ("true == true" #t "==" #t)
                 ("true != false" #t "!=" #f)
                 ("false == false" #f "==" #f))))
    
    (for-each
     (match-lambda
       ((input left-val op right-val)
        (let* ((program (parse-input input))
               (stmts (program-statements program)))
          
          (test-equal "statement count" 1 (length stmts))
          
          (let ((stmt (car stmts)))
            (test-assert "is expression statement" (expression-statement? stmt))
            (when (expression-statement? stmt)
              (let ((expr (expression-statement-expression stmt)))
                (test-infix-expression expr left-val op right-val)))))))
     tests)))

;;; ============================================================================
;;; Operator Precedence Tests
;;; ============================================================================

(test-group "operator precedence"
  (let ((tests '(("-a * b" "((-a) * b)")
                 ("!-a" "(!(-a))")
                 ("a + b + c" "((a + b) + c)")
                 ("a + b - c" "((a + b) - c)")
                 ("a * b * c" "((a * b) * c)")
                 ("a * b / c" "((a * b) / c)")
                 ("a + b / c" "(a + (b / c))")
                 ("a + b * c + d / e - f" "(((a + (b * c)) + (d / e)) - f)")
                 ("3 + 4; -5 * 5" "(3 + 4)((-5) * 5)")
                 ("5 > 4 == 3 < 4" "((5 > 4) == (3 < 4))")
                 ("5 < 4 != 3 > 4" "((5 < 4) != (3 > 4))")
                 ("3 + 4 * 5 == 3 * 1 + 4 * 5" "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
                 ("true" "true")
                 ("false" "false")
                 ("3 > 5 == false" "((3 > 5) == false)")
                 ("3 < 5 == true" "((3 < 5) == true)")
                 ("1 + (2 + 3) + 4" "((1 + (2 + 3)) + 4)")
                 ("(5 + 5) * 2" "((5 + 5) * 2)")
                 ("2 / (5 + 5)" "(2 / (5 + 5))")
                 ("-(5 + 5)" "(-(5 + 5))")
                 ("!(true == true)" "(!(true == true))"))))
    
    (for-each
     (match-lambda
       ((input expected)
        (let ((program (parse-input input)))
          (test-equal (format #f "precedence: ~a" input) 
                      expected 
                      (program->string program)))))
     tests)))

;;; ============================================================================
;;; Boolean Expression Tests
;;; ============================================================================

(test-group "boolean expressions"
  (let ((tests '(("true" #t)
                 ("false" #f))))
    
    (for-each
     (match-lambda
       ((input value)
        (let* ((program (parse-input input))
               (stmts (program-statements program)))
          
          (test-equal "statement count" 1 (length stmts))
          
          (let ((stmt (car stmts)))
            (test-assert "is expression statement" (expression-statement? stmt))
            (when (expression-statement? stmt)
              (let ((expr (expression-statement-expression stmt)))
                (test-boolean expr value)))))))
     tests)))

;;; ============================================================================
;;; If Expression Tests
;;; ============================================================================

(test-group "if expressions"
  (let* ((input "if (x < y) { x }")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is if expression" (if-expression? expr))
          (when (if-expression? expr)
            (test-infix-expression (if-expression-condition expr) "x" "<" "y")
            (test-equal "consequence statements" 1 
                        (length (block-statement-statements 
                                (if-expression-consequence expr))))
            (test-assert "no alternative" 
                         (not (if-expression-alternative expr)))))))))

(test-group "if-else expressions"
  (let* ((input "if (x < y) { x } else { y }")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is if expression" (if-expression? expr))
          (when (if-expression? expr)
            (test-infix-expression (if-expression-condition expr) "x" "<" "y")
            (test-equal "consequence statements" 1 
                        (length (block-statement-statements 
                                (if-expression-consequence expr))))
            (test-assert "has alternative" (if-expression-alternative expr))
            (test-equal "alternative statements" 1
                        (length (block-statement-statements 
                                (if-expression-alternative expr))))))))))

;;; ============================================================================
;;; Function Literal Tests
;;; ============================================================================

(test-group "function literals"
  (let* ((input "fn(x, y) { x + y; }")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is function literal" (function-literal? expr))
          (when (function-literal? expr)
            (test-equal "parameter count" 2 
                        (length (function-literal-parameters expr)))
            (test-identifier (car (function-literal-parameters expr)) "x")
            (test-identifier (cadr (function-literal-parameters expr)) "y")
            (test-equal "body statements" 1
                        (length (block-statement-statements 
                                (function-literal-body expr))))))))))

(test-group "function parameter parsing"
  (let ((tests '(("fn() {};" ())
                 ("fn(x) {};" ("x"))
                 ("fn(x, y, z) {};" ("x" "y" "z")))))
    
    (for-each
     (match-lambda
       ((input expected-params)
        (let* ((program (parse-input input))
               (stmt (car (program-statements program)))
               (fn (expression-statement-expression stmt)))
          
          (test-equal "parameter count" 
                      (length expected-params)
                      (length (function-literal-parameters fn)))
          
          (for-each
           (lambda (expected param)
             (test-identifier param expected))
           expected-params
           (function-literal-parameters fn)))))
     tests)))

;;; ============================================================================
;;; Call Expression Tests
;;; ============================================================================

(test-group "call expressions"
  (let* ((input "add(1, 2 * 3, 4 + 5);")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is call expression" (call-expression? expr))
          (when (call-expression? expr)
            (test-identifier (call-expression-function expr) "add")
            (test-equal "argument count" 3 
                        (length (call-expression-arguments expr)))
            (test-literal-expression (car (call-expression-arguments expr)) 1)
            (test-infix-expression (cadr (call-expression-arguments expr)) 2 "*" 3)
            (test-infix-expression (caddr (call-expression-arguments expr)) 4 "+" 5)))))))

;;; ============================================================================
;;; String Literal Tests
;;; ============================================================================

(test-group "string literals"
  (let* ((input "\"hello world\";")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is string literal" (string-literal? expr))
          (when (string-literal? expr)
            (test-equal "string value" "hello world" 
                        (string-literal-value expr))))))))

;;; ============================================================================
;;; Array Literal Tests
;;; ============================================================================

(test-group "array literals"
  (let* ((input "[1, 2 * 2, 3 + 3]")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is array literal" (array-literal? expr))
          (when (array-literal? expr)
            (test-equal "element count" 3 
                        (length (array-literal-elements expr)))
            (test-integer-literal (car (array-literal-elements expr)) 1)
            (test-infix-expression (cadr (array-literal-elements expr)) 2 "*" 2)
            (test-infix-expression (caddr (array-literal-elements expr)) 3 "+" 3)))))))

;;; ============================================================================
;;; Index Expression Tests
;;; ============================================================================

(test-group "index expressions"
  (let* ((input "myArray[1 + 1]")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is index expression" (index-expression? expr))
          (when (index-expression? expr)
            (test-identifier (index-expression-left expr) "myArray")
            (test-infix-expression (index-expression-index expr) 1 "+" 1)))))))

;;; ============================================================================
;;; Hash Literal Tests
;;; ============================================================================

(test-group "hash literals"
  (let* ((input "{\"one\": 1, \"two\": 2, \"three\": 3}")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is hash literal" (hash-literal? expr))
          (when (hash-literal? expr)
            (test-equal "pair count" 3 
                        (length (hash-literal-pairs expr)))))))))

(test-group "empty hash literal"
  (let* ((input "{}")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is hash literal" (hash-literal? expr))
          (when (hash-literal? expr)
            (test-equal "pair count" 0 
                        (length (hash-literal-pairs expr)))))))))

;;; ============================================================================
;;; While Expression Tests
;;; ============================================================================

(test-group "while expressions"
  (let* ((input "while (x < 10) { let x = x + 1; }")
         (program (parse-input input))
         (stmts (program-statements program)))
    
    (test-equal "statement count" 1 (length stmts))
    
    (let ((stmt (car stmts)))
      (test-assert "is expression statement" (expression-statement? stmt))
      (when (expression-statement? stmt)
        (let ((expr (expression-statement-expression stmt)))
          (test-assert "is while expression" (while-expression? expr))
          (when (while-expression? expr)
            (test-infix-expression (while-expression-condition expr) "x" "<" 10)
            (test-equal "body statements" 1
                        (length (block-statement-statements 
                                (while-expression-body expr))))))))))

(test-end "parser")

;; Print summary
(let ((runner (test-runner-current)))
  (format #t "\nParser Test Summary:\n")
  (format #t "  Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "  Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "  Total:  ~a\n" (+ (test-runner-pass-count runner)
                                  (test-runner-fail-count runner))))
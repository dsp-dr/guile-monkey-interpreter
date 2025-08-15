#!/usr/bin/env guile
!#
;;; Test the new modular parser

(add-to-load-path ".")
(use-modules (monkey lexer lexer)
             (monkey ast ast)
             (ice-9 format))

;; Import parser components carefully
(use-modules ((monkey parser parser-new) #:select (make-parser parse-program))
             ((monkey parser parser-base) #:select (parser-errors parser?)))

(define (ast-node-type node)
  "Get the type of an AST node"
  (cond
   ((let-statement? node) 'let-statement)
   ((return-statement? node) 'return-statement)
   ((expression-statement? node) 'expression-statement)
   ((break-statement? node) 'break-statement)
   ((continue-statement? node) 'continue-statement)
   ((block-statement? node) 'block-statement)
   ((identifier? node) 'identifier)
   ((integer-literal? node) 'integer-literal)
   ((string-literal? node) 'string-literal)
   ((boolean? node) 'boolean)
   ((prefix-expression? node) 'prefix-expression)
   ((infix-expression? node) 'infix-expression)
   ((if-expression? node) 'if-expression)
   ((while-expression? node) 'while-expression)
   ((for-expression? node) 'for-expression)
   ((function-literal? node) 'function-literal)
   ((call-expression? node) 'call-expression)
   ((array-literal? node) 'array-literal)
   ((hash-literal? node) 'hash-literal)
   ((index-expression? node) 'index-expression)
   (else 'unknown)))

(define (test-parse input expected-type)
  "Test parsing an input string"
  (format #t "~%Testing: ~a~%" input)
  (let* ((l (make-lexer input))
         (p (make-parser l))
         (prog (parse-program p)))
    (cond
     ((not (null? (parser-errors p)))
      (format #t "  ✗ Parse errors:~%")
      (for-each (lambda (err) (format #t "    - ~a~%" err))
                (parser-errors p))
      #f)
     ((null? (program-statements prog))
      (format #t "  ✗ No statements parsed~%")
      #f)
     (else
      (let ((stmt (car (program-statements prog))))
        (format #t "  ✓ Parsed as: ~a~%" (ast-node-type stmt))
        (if (expression-statement? stmt)
            (let ((expr (expression-statement-expression stmt)))
              (format #t "    Expression type: ~a~%" (ast-node-type expr))
              (eq? (ast-node-type expr) expected-type))
            #t))))))

(define (run-tests)
  "Run parser tests"
  (format #t "=== Testing New Modular Parser ===~%")
  
  ;; Basic tests
  (test-parse "let x = 5;" 'let-statement)
  (test-parse "return 10;" 'return-statement)
  (test-parse "5 + 10;" 'infix-expression)
  (test-parse "!true;" 'prefix-expression)
  (test-parse "if (x < y) { x } else { y }" 'if-expression)
  (test-parse "fn(x, y) { x + y }" 'function-literal)
  (test-parse "[1, 2, 3];" 'array-literal)
  (test-parse "{\"a\": 1, \"b\": 2};" 'hash-literal)
  
  ;; New features
  (test-parse "while (x < 10) { let x = x + 1; }" 'while-expression)
  (test-parse "for (let i = 0; i < 10; i = i + 1) { x; }" 'for-expression)
  (test-parse "break;" 'break-statement)
  (test-parse "continue;" 'continue-statement)
  (test-parse "|x| x + 1;" 'function-literal)
  
  (format #t "~%=== Testing For Loop in Detail ===~%")
  (let* ((input "for (let i = 0; i < 10; i = i + 1) { sum = sum + i; }")
         (l (make-lexer input))
         (p (make-parser l))
         (prog (parse-program p)))
    (if (not (null? (parser-errors p)))
        (begin
          (format #t "Parse errors:~%")
          (for-each (lambda (err) (format #t "  - ~a~%" err))
                    (parser-errors p)))
        (begin
          (format #t "✓ For loop parsed successfully!~%")
          (let ((stmt (car (program-statements prog))))
            (when (expression-statement? stmt)
              (let ((expr (expression-statement-expression stmt)))
                (when (for-expression? expr)
                  (format #t "  Init: ~a~%" 
                          (if (for-expression-init expr)
                              (ast-node-type (for-expression-init expr))
                              "none"))
                  (format #t "  Condition: ~a~%"
                          (if (for-expression-condition expr)
                              (ast-node-type (for-expression-condition expr))
                              "none"))
                  (format #t "  Update: ~a~%"
                          (if (for-expression-update expr)
                              (ast-node-type (for-expression-update expr))
                              "none"))
                  (format #t "  Body: ~a statements~%"
                          (length (block-statement-statements 
                                  (for-expression-body expr))))))))))))

(run-tests)
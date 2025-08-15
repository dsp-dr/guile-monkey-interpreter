#!/usr/bin/env guile
!#
;;; Apply refactoring to remove with-return from parser functions

(use-modules (ice-9 format)
             (ice-9 textual-ports)
             (ice-9 regex))

;;; Manual refactoring patterns for common cases

(define (refactor-parse-expression-statement)
  "Refactor parse-expression-statement"
  '(define (parse-expression-statement parser)
     "Parse expression statement"
     (let ((stmt-token (parser-cur-token parser)))
       (let ((expr (parse-expression parser LOWEST)))
         (when (peek-token-is? parser SEMICOLON)
           (next-token! parser))
         (if expr
             (make-expression-statement stmt-token expr)
             #f)))))

(define (refactor-parse-block-statement)
  "Refactor parse-block-statement"
  '(define (parse-block-statement parser)
     "Parse block statement: { statements }"
     (let ((block-token (parser-cur-token parser))
           (statements '()))
       (next-token! parser)
       (let loop ((stmts '()))
         (cond
          ((cur-token-is? parser RBRACE)
           (make-block-statement block-token (reverse stmts)))
          ((cur-token-is? parser EOF)
           #f)
          (else
           (let ((stmt (parse-statement parser)))
             (next-token! parser)
             (loop (cons stmt stmts)))))))))

(define (refactor-parse-if-expression)
  "Refactor parse-if-expression"
  '(define (parse-if-expression parser)
     "Parse if expression"
     (let ((expr-token (parser-cur-token parser)))
       (if (not (expect-peek! parser LPAREN))
           #f
           (begin
             (next-token! parser)
             (let ((condition (parse-expression parser LOWEST)))
               (if (not (expect-peek! parser RPAREN))
                   #f
                   (if (not (expect-peek! parser LBRACE))
                       #f
                       (let ((consequence (parse-block-statement parser)))
                         (if (peek-token-is? parser ELSE)
                             (begin
                               (next-token! parser)
                               (if (not (expect-peek! parser LBRACE))
                                   #f
                                   (let ((alternative (parse-block-statement parser)))
                                     (make-if-expression expr-token condition consequence alternative))))
                             (make-if-expression expr-token condition consequence #f)))))))))))

(define (refactor-parse-while-expression)
  "Refactor parse-while-expression"
  '(define (parse-while-expression parser)
     "Parse while expression"
     (let ((expr-token (parser-cur-token parser)))
       (if (not (expect-peek! parser LPAREN))
           #f
           (begin
             (next-token! parser)
             (let ((condition (parse-expression parser LOWEST)))
               (if (not (expect-peek! parser RPAREN))
                   #f
                   (if (not (expect-peek! parser LBRACE))
                       #f
                       (let ((body (parse-block-statement parser)))
                         (make-while-expression expr-token condition body))))))))))

(define (refactor-parse-function-literal)
  "Refactor parse-function-literal"
  '(define (parse-function-literal parser)
     "Parse function literal"
     (let ((fn-token (parser-cur-token parser)))
       (if (not (expect-peek! parser LPAREN))
           #f
           (let ((parameters (parse-function-parameters parser)))
             (if (not (expect-peek! parser LBRACE))
                 #f
                 (let ((body (parse-block-statement parser)))
                   (make-function-literal fn-token parameters body))))))))

(define (refactor-parse-prefix-expression)
  "Refactor parse-prefix-expression"
  '(define (parse-prefix-expression parser)
     "Parse prefix expression"
     (let ((expr-token (parser-cur-token parser))
           (operator (token-literal (parser-cur-token parser))))
       (next-token! parser)
       (let ((right (parse-expression parser PREFIX)))
         (make-prefix-expression expr-token operator right)))))

(define (refactor-parse-infix-expression)
  "Refactor parse-infix-expression"
  '(define (parse-infix-expression parser left)
     "Parse infix expression"
     (let ((expr-token (parser-cur-token parser))
           (operator (token-literal (parser-cur-token parser)))
           (precedence (cur-precedence parser)))
       (next-token! parser)
       (let ((right (parse-expression parser precedence)))
         (make-infix-expression expr-token operator left right)))))

(define (refactor-parse-grouped-expression)
  "Refactor parse-grouped-expression"
  '(define (parse-grouped-expression parser)
     "Parse grouped expression"
     (next-token! parser)
     (let ((expr (parse-expression parser LOWEST)))
       (if (expect-peek! parser RPAREN)
           expr
           #f))))

(define (refactor-parse-call-expression)
  "Refactor parse-call-expression"
  '(define (parse-call-expression parser fn)
     "Parse call expression"
     (let ((expr-token (parser-cur-token parser))
           (arguments (parse-expression-list parser RPAREN)))
       (make-call-expression expr-token fn arguments))))

(define (refactor-parse-lambda-shorthand)
  "Refactor parse-lambda-shorthand"
  '(define (parse-lambda-shorthand parser)
     "Parse lambda shorthand: |x| x + 1"
     (let ((pipe-token (parser-cur-token parser)))
       (next-token! parser)
       (if (not (cur-token-is? parser IDENT))
           (begin
             (parser-error! parser (format #f "expected identifier after |, got ~a"
                                          (token-type (parser-cur-token parser))))
             #f)
           (let ((param (make-identifier (parser-cur-token parser)
                                        (token-literal (parser-cur-token parser)))))
             (if (not (expect-peek! parser PIPE))
                 (begin
                   (parser-error! parser (format #f "expected |, got ~a"
                                               (token-type (parser-peek-token parser))))
                   #f)
                 (begin
                   (next-token! parser)
                   (let ((body-expr (parse-expression parser LOWEST)))
                     (let ((body (make-block-statement
                                 pipe-token
                                 (list (make-return-statement pipe-token body-expr)))))
                       (make-function-literal pipe-token (list param) body))))))))))

(define (refactor-parse-expression-list)
  "Refactor parse-expression-list"
  '(define (parse-expression-list parser end-token)
     "Parse list of expressions until end-token"
     (let loop ((expressions '()))
       (cond
        ((peek-token-is? parser end-token)
         (next-token! parser)
         (reverse expressions))
        ((null? expressions)
         (next-token! parser)
         (if (cur-token-is? parser end-token)
             '()
             (let ((expr (parse-expression parser LOWEST)))
               (if (peek-token-is? parser COMMA)
                   (begin
                     (next-token! parser)
                     (loop (cons expr expressions)))
                   (if (expect-peek! parser end-token)
                       (reverse (cons expr expressions))
                       #f)))))
        (else
         (next-token! parser)
         (let ((expr (parse-expression parser LOWEST)))
           (if (peek-token-is? parser COMMA)
               (begin
                 (next-token! parser)
                 (loop (cons expr expressions)))
               (if (expect-peek! parser end-token)
                   (reverse (cons expr expressions))
                   #f))))))))

;;; Generate the refactored parser.scm
(define (generate-refactored-parser)
  "Generate complete refactored parser"
  (format #t ";;; Refactored parser functions~%~%")
  
  (for-each (lambda (func)
              (format #t ";;; ~a~%" (cadr (cadr func)))
              (pretty-print func)
              (newline))
            (list (refactor-parse-expression-statement)
                  (refactor-parse-block-statement)
                  (refactor-parse-if-expression)
                  (refactor-parse-while-expression)
                  (refactor-parse-function-literal)
                  (refactor-parse-prefix-expression)
                  (refactor-parse-infix-expression)
                  (refactor-parse-grouped-expression)
                  (refactor-parse-call-expression)
                  (refactor-parse-lambda-shorthand)
                  (refactor-parse-expression-list))))

;;; Pretty printer
(define (pretty-print expr)
  (define (pp expr indent)
    (cond
     ((null? expr) (display "()"))
     ((not (pair? expr)) (display expr))
     ((eq? (car expr) 'define)
      (display "(define ")
      (pp (cadr expr) (+ indent 8))
      (newline)
      (display (make-string (+ indent 2) #\space))
      (for-each (lambda (e)
                  (pp e (+ indent 2))
                  (unless (eq? e (last (cddr expr)))
                    (newline)
                    (display (make-string (+ indent 2) #\space))))
                (cddr expr))
      (display ")"))
     ((eq? (car expr) 'let)
      (display "(let ")
      (pp (cadr expr) (+ indent 5))
      (for-each (lambda (e)
                  (newline)
                  (display (make-string (+ indent 5) #\space))
                  (pp e (+ indent 5)))
                (cddr expr))
      (display ")"))
     ((eq? (car expr) 'if)
      (display "(if ")
      (pp (cadr expr) (+ indent 4))
      (newline)
      (display (make-string (+ indent 4) #\space))
      (pp (caddr expr) (+ indent 4))
      (when (not (null? (cdddr expr)))
        (newline)
        (display (make-string (+ indent 4) #\space))
        (pp (cadddr expr) (+ indent 4)))
      (display ")"))
     (else
      (display "(")
      (pp (car expr) indent)
      (for-each (lambda (e)
                  (display " ")
                  (pp e indent))
                (cdr expr))
      (display ")"))))
  (pp expr 0))

;;; Run the refactoring
(generate-refactored-parser)
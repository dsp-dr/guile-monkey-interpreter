#!/usr/bin/env guile
!#
;;; Refactoring tool to remove with-return from parser functions
;;; This tool analyzes and transforms Scheme code to eliminate continuation issues

(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 textual-ports)
             (srfi srfi-1))

;;; Pattern matching for with-return forms
(define (with-return-form? expr)
  "Check if expression is a with-return form"
  (and (pair? expr)
       (eq? (car expr) 'with-return)))

(define (return-call? expr)
  "Check if expression is a return call"
  (and (pair? expr)
       (eq? (car expr) 'return)))

;;; Transform with-return forms
(define (transform-with-return expr)
  "Transform a with-return form to remove the continuation"
  (match expr
    (('with-return body ...)
     ;; Remove the with-return wrapper and transform the body
     (transform-body body))
    (else expr)))

(define (transform-body body)
  "Transform the body of a with-return, replacing return calls"
  (if (null? body)
      '()
      (let ((transformed (map transform-expr body)))
        ;; If body is a single expression, return it directly
        (if (= (length transformed) 1)
            (car transformed)
            ;; Otherwise wrap in begin
            `(begin ,@transformed)))))

(define (transform-expr expr)
  "Transform a single expression"
  (match expr
    ;; Transform (return value) to just value
    (('return value)
     (transform-expr value))
    
    ;; Transform (unless condition (return #f)) to (if (not condition) #f ...)
    (('unless condition ('return #f))
     `(if (not ,condition) #f))
    
    ;; Transform (unless condition (return value)) to (if (not condition) value ...)
    (('unless condition ('return value))
     `(if (not ,condition) ,(transform-expr value)))
    
    ;; Transform (when condition (return value)) to (if condition value ...)
    (('when condition ('return value))
     `(if ,condition ,(transform-expr value)))
    
    ;; Transform nested with-return
    (('with-return body ...)
     (transform-with-return expr))
    
    ;; Transform let forms
    (('let bindings body ...)
     `(let ,(map transform-binding bindings)
        ,@(map transform-expr body)))
    
    ;; Transform let* forms
    (('let* bindings body ...)
     `(let* ,(map transform-binding bindings)
        ,@(map transform-expr body)))
    
    ;; Transform if forms
    (('if condition then else)
     `(if ,(transform-expr condition)
          ,(transform-expr then)
          ,(transform-expr else)))
    
    ;; Transform if forms without else
    (('if condition then)
     `(if ,(transform-expr condition)
          ,(transform-expr then)))
    
    ;; Transform begin forms
    (('begin exprs ...)
     `(begin ,@(map transform-expr exprs)))
    
    ;; Transform cond forms
    (('cond clauses ...)
     `(cond ,@(map transform-cond-clause clauses)))
    
    ;; Leave other expressions unchanged
    ((? list? lst)
     (map transform-expr lst))
    
    (else expr)))

(define (transform-binding binding)
  "Transform a let binding"
  (match binding
    ((var value)
     `(,var ,(transform-expr value)))
    (else binding)))

(define (transform-cond-clause clause)
  "Transform a cond clause"
  (match clause
    ((condition exprs ...)
     `(,(transform-expr condition) ,@(map transform-expr exprs)))
    (else clause)))

;;; Function transformation
(define (transform-function func)
  "Transform a complete function definition"
  (match func
    (('define (name args ...) body ...)
     (let ((transformed-body (map transform-expr body)))
       `(define (,name ,@args)
          ,@(if (any with-return-form? body)
                ;; If body contained with-return, we've transformed it
                transformed-body
                ;; Otherwise leave as is
                body))))
    (else func)))

;;; Analyze function for with-return usage
(define (analyze-function func)
  "Analyze a function and report with-return usage"
  (match func
    (('define (name args ...) body ...)
     (let ((has-with-return? (any with-return-form? body))
           (return-calls (count-return-calls body)))
       (list 'function name
             'has-with-return has-with-return?
             'return-calls return-calls)))
    (else #f)))

(define (count-return-calls expr)
  "Count return calls in an expression"
  (cond
   ((return-call? expr) 1)
   ((list? expr)
    (apply + (map count-return-calls expr)))
   (else 0)))

;;; Extract function from file
(define (extract-function file-path function-name)
  "Extract a specific function from a file"
  (let ((content (call-with-input-file file-path get-string-all)))
    ;; This is simplified - real implementation would need proper parsing
    (format #t "Extracting ~a from ~a~%" function-name file-path)
    #f))

;;; Test transformation
(define (test-transformation)
  "Test the transformation on sample code"
  (let ((sample '(define (parse-expression-statement parser)
                   (with-return
                     (let ((stmt-token (parser-cur-token parser)))
                       (let ((expr (parse-expression parser LOWEST)))
                         (when (peek-token-is? parser SEMICOLON)
                           (next-token! parser))
                         (unless expr
                           (return #f))
                         (make-expression-statement stmt-token expr)))))))
    
    (format #t "Original:~%")
    (pretty-print sample)
    (format #t "~%Transformed:~%")
    (pretty-print (transform-function sample))
    (format #t "~%Analysis:~%")
    (pretty-print (analyze-function sample))))

;;; Generate Scheme IR
(define (generate-ir expr)
  "Generate intermediate representation for expression"
  (format #t "~%=== Intermediate Representation ===~%")
  (format #t "Expression type: ~a~%" (if (pair? expr) (car expr) 'atom))
  (when (pair? expr)
    (format #t "Sub-expressions: ~a~%" (length (cdr expr))))
  
  ;; Show continuation points
  (when (with-return-form? expr)
    (format #t "CONTINUATION POINT: with-return creates new continuation~%"))
  
  ;; Show return points
  (let ((returns (find-return-points expr)))
    (when (not (null? returns))
      (format #t "RETURN POINTS: ~a~%" returns))))

(define (find-return-points expr)
  "Find all return points in expression"
  (cond
   ((return-call? expr) (list expr))
   ((list? expr)
    (apply append (map find-return-points expr)))
   (else '())))

;;; Debugging helpers
(define (trace-continuation expr)
  "Trace continuation flow through expression"
  (format #t "~%=== Continuation Trace ===~%")
  (trace-cont-helper expr 0 '()))

(define (trace-cont-helper expr depth cont-stack)
  "Helper for continuation tracing"
  (let ((indent (make-string (* 2 depth) #\space)))
    (cond
     ((with-return-form? expr)
      (format #t "~a[ENTER with-return, depth=~a]~%" indent depth)
      (trace-cont-helper (cdr expr) (+ depth 1) (cons 'with-return cont-stack))
      (format #t "~a[EXIT with-return, depth=~a]~%" indent depth))
     
     ((return-call? expr)
      (format #t "~a[RETURN from ~a]~%" indent (if (null? cont-stack) 'NONE (car cont-stack))))
     
     ((list? expr)
      (for-each (lambda (e) (trace-cont-helper e depth cont-stack)) expr))
     
     (else #t))))

;;; Main refactoring function
(define (refactor-parser-function name code)
  "Refactor a parser function to remove with-return"
  (format #t "~%=== Refactoring ~a ===~%" name)
  
  ;; Analyze before
  (format #t "Before:~%")
  (let ((analysis (analyze-function code)))
    (pretty-print analysis))
  
  ;; Generate IR
  (generate-ir code)
  
  ;; Trace continuations
  (trace-continuation code)
  
  ;; Transform
  (let ((transformed (transform-function code)))
    (format #t "~%After transformation:~%")
    (pretty-print transformed)
    
    ;; Verify no returns remain
    (let ((remaining-returns (count-return-calls transformed)))
      (if (= remaining-returns 0)
          (format #t "✓ Successfully removed all return calls~%")
          (format #t "⚠ Warning: ~a return calls remain~%" remaining-returns)))
    
    transformed))

;;; Run tests
(format #t "=== Refactoring Tool Test ===~%")
(test-transformation)

;;; Example usage
(format #t "~%~%=== Example Refactoring ===~%")
(define example-func
  '(define (parse-if-expression parser)
     (with-return
       (let ((expr-token (parser-cur-token parser)))
         (unless (expect-peek! parser LPAREN)
           (return #f))
         (next-token! parser)
         (let ((condition (parse-expression parser LOWEST)))
           (unless (expect-peek! parser RPAREN)
             (return #f))
           (unless (expect-peek! parser LBRACE)
             (return #f))
           (let ((consequence (parse-block-statement parser)))
             (if (peek-token-is? parser ELSE)
                 (begin
                   (next-token! parser)
                   (unless (expect-peek! parser LBRACE)
                     (return #f))
                   (let ((alternative (parse-block-statement parser)))
                     (make-if-expression expr-token condition consequence alternative)))
                 (make-if-expression expr-token condition consequence #f))))))))

(refactor-parser-function 'parse-if-expression example-func)
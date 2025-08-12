;;; AST Validator Implementation
;;; Validates S-expression ASTs against the specification

(define-module (monkey ir ast-validator)
  #:use-module (monkey ir ast-spec)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (validate-ast-file
            validate-ast-string
            validate-and-report
            ast->sexp
            sexp->ast))

;;; ============================================================================
;;; File and String Validators
;;; ============================================================================

(define (validate-ast-file filename)
  "Validate an AST from a file containing S-expressions"
  (call-with-input-file filename
    (lambda (port)
      (let ((sexp (read port)))
        (validate-ast sexp)))))

(define (validate-ast-string str)
  "Validate an AST from a string containing S-expressions"
  (call-with-input-string str
    (lambda (port)
      (let ((sexp (read port)))
        (validate-ast sexp)))))

(define (validate-and-report sexp)
  "Validate an AST and print a detailed report"
  (let-values (((valid? errors) (validate-ast sexp)))
    (if valid?
        (begin
          (display "✓ AST is valid!\n")
          (display "\nAST Summary:\n")
          (display-ast-summary sexp)
          #t)
        (begin
          (display "✗ AST validation failed!\n\n")
          (display "Errors found:\n")
          (for-each (lambda (error)
                     (format #t "  • ~a~%" error))
                   errors)
          #f))))

;;; ============================================================================
;;; AST Summary Display
;;; ============================================================================

(define (display-ast-summary ast)
  "Display a summary of the AST structure"
  (define (count-nodes ast)
    (let ((counts (make-hash-table)))
      (define (traverse node)
        (when (list? node)
          (let ((type (car node)))
            (hash-set! counts type 
                      (+ 1 (or (hash-ref counts type) 0)))
            (when (and (not (null? (cdr node)))
                      (list? (cadr node)))
              (for-each (lambda (attr)
                         (when (pair? attr)
                           (let ((value (cdr attr)))
                             (cond
                              ((list? value)
                               (for-each traverse value))
                              ((and (list? value) 
                                   (not (null? value))
                                   (symbol? (car value)))
                               (traverse value))))))
                       (cadr node))))))
      (traverse ast)
      counts))
  
  (let ((counts (count-nodes ast)))
    (display "Node counts:\n")
    (hash-for-each (lambda (key value)
                    (format #t "  ~a: ~a~%" key value))
                  counts)))

;;; ============================================================================
;;; AST Conversion Functions
;;; ============================================================================

(define (ast->sexp ast-node)
  "Convert internal AST representation to S-expression format"
  ;; This would convert from the internal Monkey AST records
  ;; to the canonical S-expression format
  (match ast-node
    ;; Add pattern matching for each AST node type
    ((? program? node)
     `(program ((statements . ,(map ast->sexp (program-statements node))))))
    
    ((? let-statement? node)
     `(let-statement 
       ((name . ,(ast->sexp (let-statement-name node)))
        (value . ,(ast->sexp (let-statement-value node))))))
    
    ((? identifier? node)
     `(identifier ((value . ,(identifier-value node)))))
    
    ((? integer-literal? node)
     `(integer-literal ((value . ,(integer-literal-value node)))))
    
    ((? infix-expression? node)
     `(infix-expression
       ((operator . ,(infix-expression-operator node))
        (left . ,(ast->sexp (infix-expression-left node)))
        (right . ,(ast->sexp (infix-expression-right node))))))
    
    ;; Default case - return as-is if already in S-expression format
    (_ ast-node)))

(define (sexp->ast sexp)
  "Convert S-expression format to internal AST representation"
  (let-values (((valid? errors) (validate-ast sexp)))
    (if (not valid?)
        (error "Cannot convert invalid S-expression to AST:" errors)
        (match sexp
          ;; Convert each S-expression node type to internal representation
          (('program attrs)
           (let ((statements (assq-ref attrs 'statements)))
             (make-program (map sexp->ast statements))))
          
          (('let-statement attrs)
           (let ((name (assq-ref attrs 'name))
                 (value (assq-ref attrs 'value)))
             (make-let-statement (sexp->ast name) (sexp->ast value))))
          
          (('identifier attrs)
           (make-identifier (assq-ref attrs 'value)))
          
          (('integer-literal attrs)
           (make-integer-literal (assq-ref attrs 'value)))
          
          ;; Add more conversions as needed
          (_ sexp)))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (assq-ref alist key)
  "Safe association list lookup"
  (let ((pair (assq key alist)))
    (if pair (cdr pair) #f)))

;;; ============================================================================
;;; Pretty Printing
;;; ============================================================================

(define (pretty-print-ast sexp)
  "Pretty print an AST S-expression"
  (pretty-print sexp #:width 80 #:max-expr-width 60))
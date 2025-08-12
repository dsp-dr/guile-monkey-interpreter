;;; AST S-expression Specification for Monkey Language
;;; This file defines the canonical S-expression format for Monkey AST nodes
;;; and provides validation functions

(define-module (monkey ir ast-spec)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (ast-spec
            validate-ast
            ast-node?
            get-node-spec
            serialize-ast
            deserialize-ast))

;;; ============================================================================
;;; AST Node Specifications
;;; ============================================================================

;; Each specification is a list of:
;; (node-type required-fields optional-fields child-nodes)

(define ast-spec
  '(;; Program - Root node
    (program
     ()                         ; required fields
     ()                         ; optional fields  
     ((statements . list)))     ; child nodes

    ;; Statements
    (let-statement
     ((name . identifier))      ; required: variable name
     ()                         ; optional
     ((value . expression)))    ; child: value expression

    (return-statement
     ()
     ()
     ((value . expression)))

    (expression-statement
     ()
     ()
     ((expression . expression)))

    (block-statement
     ()
     ()
     ((statements . list)))

    (while-statement
     ()
     ()
     ((condition . expression)
      (body . block-statement)))

    ;; Expressions
    (identifier
     ((value . string))         ; the identifier name
     ()
     ())

    (integer-literal
     ((value . integer))        ; the integer value
     ()
     ())

    (boolean-literal
     ((value . boolean))        ; #t or #f
     ()
     ())

    (string-literal
     ((value . string))         ; the string value
     ()
     ())

    (prefix-expression
     ((operator . string))      ; "!" or "-"
     ()
     ((right . expression)))

    (infix-expression
     ((operator . string))      ; "+", "-", "*", "/", etc.
     ()
     ((left . expression)
      (right . expression)))

    (if-expression
     ()
     ()
     ((condition . expression)
      (consequence . block-statement)
      (alternative . block-statement)))  ; optional

    (function-literal
     ()
     ((name . string))          ; optional function name
     ((parameters . list)       ; list of identifiers
      (body . block-statement)))

    (call-expression
     ()
     ()
     ((function . expression)   ; function to call
      (arguments . list)))      ; list of expressions

    (array-literal
     ()
     ()
     ((elements . list)))       ; list of expressions

    (index-expression
     ()
     ()
     ((left . expression)       ; array or hash
      (index . expression)))    ; index value

    (hash-literal
     ()
     ()
     ((pairs . list)))))        ; list of (key . value) pairs

;;; ============================================================================
;;; Validation Schema Definition
;;; ============================================================================

(define-record-type <ast-node-spec>
  (make-ast-node-spec type required optional children)
  ast-node-spec?
  (type ast-node-spec-type)
  (required ast-node-spec-required)
  (optional ast-node-spec-optional)
  (children ast-node-spec-children))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (get-node-spec type)
  "Get the specification for a given node type"
  (assq type ast-spec))

(define (validate-type value expected-type)
  "Validate that a value matches the expected type"
  (match expected-type
    ('string (string? value))
    ('integer (integer? value))
    ('boolean (boolean? value))
    ('identifier (and (list? value) 
                     (eq? (car value) 'identifier)))
    ('expression (ast-node? value))
    ('block-statement (and (list? value)
                          (eq? (car value) 'block-statement)))
    ('list (list? value))
    (_ #t)))

;;; ============================================================================
;;; Main Validation Function
;;; ============================================================================

(define (ast-node? sexp)
  "Check if an S-expression is a valid AST node"
  (and (list? sexp)
       (not (null? sexp))
       (symbol? (car sexp))
       (get-node-spec (car sexp))))

(define (validate-ast sexp)
  "Validate an S-expression against the AST specification.
   Returns (values #t '()) on success, or (values #f errors) on failure."
  (define errors '())
  
  (define (add-error! msg)
    (set! errors (cons msg errors)))
  
  (define (validate-node node path)
    (if (not (list? node))
        (add-error! (format #f "~a: Expected list, got ~a" 
                           path (class-of node)))
        (let* ((node-type (car node))
               (spec (get-node-spec node-type)))
          (if (not spec)
              (add-error! (format #f "~a: Unknown node type '~a'" 
                                 path node-type))
              (let ((spec-required (cadr spec))
                    (spec-optional (caddr spec))
                    (spec-children (cadddr spec))
                    (node-attrs (if (null? (cdr node)) '() (cadr node))))
                
                ;; Validate required fields
                (for-each 
                 (lambda (req)
                   (let* ((field-name (car req))
                          (field-type (cdr req))
                          (field-value (assq field-name node-attrs)))
                     (cond
                      ((not field-value)
                       (add-error! (format #f "~a: Missing required field '~a'"
                                          path field-name)))
                      ((not (validate-type (cdr field-value) field-type))
                       (add-error! (format #f "~a.~a: Type mismatch, expected ~a"
                                          path field-name field-type))))))
                 spec-required)
                
                ;; Validate optional fields
                (for-each
                 (lambda (opt)
                   (let* ((field-name (car opt))
                          (field-type (cdr opt))
                          (field-value (assq field-name node-attrs)))
                     (when (and field-value
                               (not (validate-type (cdr field-value) field-type)))
                       (add-error! (format #f "~a.~a: Type mismatch, expected ~a"
                                          path field-name field-type)))))
                 spec-optional)
                
                ;; Validate children
                (for-each
                 (lambda (child-spec)
                   (let* ((child-name (car child-spec))
                          (child-type (cdr child-spec))
                          (child-value (assq child-name node-attrs)))
                     (when child-value
                       (let ((child-data (cdr child-value)))
                         (cond
                          ((eq? child-type 'list)
                           (let loop ((children child-data) (idx 0))
                             (unless (null? children)
                               (validate-node (car children)
                                            (format #f "~a.~a[~a]" 
                                                   path child-name idx))
                               (loop (cdr children) (+ idx 1)))))
                          ((eq? child-type 'expression)
                           (validate-node child-data
                                        (format #f "~a.~a" path child-name)))
                          ((eq? child-type 'block-statement)
                           (validate-node child-data
                                        (format #f "~a.~a" path child-name))))))))
                 spec-children))))))
  
  (validate-node sexp "root")
  (if (null? errors)
      (values #t '())
      (values #f (reverse errors))))

;;; ============================================================================
;;; Serialization Functions
;;; ============================================================================

(define (serialize-ast ast-node)
  "Convert an AST node to canonical S-expression format"
  ;; This is a placeholder - in practice, this would convert from
  ;; the internal AST representation to S-expressions
  (match ast-node
    ((? ast-node?) ast-node)
    (_ (error "Invalid AST node for serialization"))))

(define (deserialize-ast sexp)
  "Convert an S-expression to internal AST representation"
  (let-values (((valid? errors) (validate-ast sexp)))
    (if valid?
        sexp  ; For now, just return the validated S-expression
        (error "Invalid S-expression for AST:" errors))))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================

;; Example valid AST:
;; (program
;;   ((statements . ((let-statement
;;                    ((name . (identifier ((value . "x"))))
;;                     (value . (integer-literal ((value . 5))))))
;;                   (expression-statement
;;                    ((expression . (infix-expression
;;                                   ((operator . "+")
;;                                    (left . (identifier ((value . "x"))))
;;                                    (right . (integer-literal ((value . 10)))))))))))))
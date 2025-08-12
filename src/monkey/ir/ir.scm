;;; Intermediate Representation Module
;;; Main entry point for AST S-expression handling

(define-module (monkey ir ir)
  #:use-module (monkey ir ast-spec)
  #:use-module (monkey ir ast-validator)
  #:use-module (monkey ast ast)
  #:use-module (monkey parser parser)
  #:use-module (monkey lexer lexer)
  #:use-module (ice-9 pretty-print)
  #:export (monkey->sexp
            sexp->monkey
            validate-sexp
            parse-to-sexp
            load-sexp
            save-sexp
            pretty-print-sexp))

;;; ============================================================================
;;; High-level API
;;; ============================================================================

(define (monkey->sexp monkey-code)
  "Parse Monkey code and convert to S-expression AST"
  (let* ((lexer (make-lexer monkey-code))
         (parser (make-parser lexer))
         (program (parse-program parser)))
    (if (null? (parser-errors parser))
        (ast->sexp-internal program)
        (error "Parse errors:" (parser-errors parser)))))

(define (sexp->monkey sexp)
  "Convert S-expression AST back to Monkey code"
  (let-values (((valid? errors) (validate-ast sexp)))
    (if valid?
        (sexp->monkey-code sexp)
        (error "Invalid S-expression:" errors))))

(define (validate-sexp sexp)
  "Validate an S-expression AST and return detailed results"
  (validate-and-report sexp))

(define (parse-to-sexp filename)
  "Parse a Monkey source file to S-expression AST"
  (call-with-input-file filename
    (lambda (port)
      (let ((content (get-string-all port)))
        (monkey->sexp content)))))

(define (load-sexp filename)
  "Load an S-expression AST from a file"
  (call-with-input-file filename read))

(define (save-sexp sexp filename)
  "Save an S-expression AST to a file"
  (call-with-output-file filename
    (lambda (port)
      (pretty-print sexp port #:width 80))))

(define (pretty-print-sexp sexp)
  "Pretty print an S-expression AST to stdout"
  (pretty-print sexp #:width 80 #:max-expr-width 60))

;;; ============================================================================
;;; Internal Conversion Functions
;;; ============================================================================

(define (ast->sexp-internal node)
  "Convert internal AST to S-expression format"
  (cond
   ;; Program
   ((program? node)
    `(program
      ((statements . ,(map ast->sexp-internal (program-statements node))))))
   
   ;; Statements
   ((let-statement? node)
    `(let-statement
      ((name . ,(ast->sexp-internal (let-statement-name node)))
       (value . ,(ast->sexp-internal (let-statement-value node))))))
   
   ((return-statement? node)
    `(return-statement
      ((value . ,(ast->sexp-internal (return-statement-value node))))))
   
   ((expression-statement? node)
    `(expression-statement
      ((expression . ,(ast->sexp-internal 
                      (expression-statement-expression node))))))
   
   ((block-statement? node)
    `(block-statement
      ((statements . ,(map ast->sexp-internal 
                          (block-statement-statements node))))))
   
   ;; Expressions
   ((identifier? node)
    `(identifier ((value . ,(identifier-value node)))))
   
   ((integer-literal? node)
    `(integer-literal ((value . ,(integer-literal-value node)))))
   
   ((boolean? node)
    `(boolean-literal ((value . ,(boolean-value node)))))
   
   ((string-literal? node)
    `(string-literal ((value . ,(string-literal-value node)))))
   
   ((prefix-expression? node)
    `(prefix-expression
      ((operator . ,(prefix-expression-operator node))
       (right . ,(ast->sexp-internal (prefix-expression-right node))))))
   
   ((infix-expression? node)
    `(infix-expression
      ((operator . ,(infix-expression-operator node))
       (left . ,(ast->sexp-internal (infix-expression-left node)))
       (right . ,(ast->sexp-internal (infix-expression-right node))))))
   
   ((if-expression? node)
    `(if-expression
      ((condition . ,(ast->sexp-internal (if-expression-condition node)))
       (consequence . ,(ast->sexp-internal (if-expression-consequence node)))
       ,@(if (if-expression-alternative node)
             `((alternative . ,(ast->sexp-internal 
                               (if-expression-alternative node))))
             '()))))
   
   ((function-literal? node)
    `(function-literal
      ((parameters . ,(map ast->sexp-internal 
                          (function-literal-parameters node)))
       (body . ,(ast->sexp-internal (function-literal-body node))))))
   
   ((call-expression? node)
    `(call-expression
      ((function . ,(ast->sexp-internal (call-expression-function node)))
       (arguments . ,(map ast->sexp-internal 
                         (call-expression-arguments node))))))
   
   ((array-literal? node)
    `(array-literal
      ((elements . ,(map ast->sexp-internal 
                        (array-literal-elements node))))))
   
   ((index-expression? node)
    `(index-expression
      ((left . ,(ast->sexp-internal (index-expression-left node)))
       (index . ,(ast->sexp-internal (index-expression-index node))))))
   
   ((hash-literal? node)
    `(hash-literal
      ((pairs . ,(map (lambda (pair)
                       `((key . ,(ast->sexp-internal (car pair)))
                         (value . ,(ast->sexp-internal (cdr pair)))))
                     (hash-literal-pairs node))))))
   
   ;; Default
   (else
    (error "Unknown AST node type:" node))))

(define (sexp->monkey-code sexp)
  "Convert S-expression AST to Monkey source code"
  (match sexp
    (('program attrs)
     (let ((statements (assq 'statements attrs)))
       (string-join 
        (map sexp->monkey-code (cdr statements))
        "\n")))
    
    (('let-statement attrs)
     (format #f "let ~a = ~a;"
             (sexp->monkey-code (cdr (assq 'name attrs)))
             (sexp->monkey-code (cdr (assq 'value attrs)))))
    
    (('return-statement attrs)
     (format #f "return ~a;"
             (sexp->monkey-code (cdr (assq 'value attrs)))))
    
    (('expression-statement attrs)
     (sexp->monkey-code (cdr (assq 'expression attrs))))
    
    (('identifier attrs)
     (cdr (assq 'value attrs)))
    
    (('integer-literal attrs)
     (number->string (cdr (assq 'value attrs))))
    
    (('boolean-literal attrs)
     (if (cdr (assq 'value attrs)) "true" "false"))
    
    (('string-literal attrs)
     (format #f "\"~a\"" (cdr (assq 'value attrs))))
    
    (('infix-expression attrs)
     (format #f "(~a ~a ~a)"
             (sexp->monkey-code (cdr (assq 'left attrs)))
             (cdr (assq 'operator attrs))
             (sexp->monkey-code (cdr (assq 'right attrs)))))
    
    ;; Add more cases as needed
    (_ (format #f "/* TODO: ~a */" (car sexp)))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (get-string-all port)
  "Read entire file as string"
  (let loop ((chars '()))
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (list->string (reverse chars))
          (loop (cons ch chars))))))
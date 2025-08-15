#!/usr/bin/env guile
!#
;;; Generate Mermaid diagram for AST structure

(add-to-load-path "../..")
(use-modules (monkey lexer lexer)
             (monkey ast ast)
             (ice-9 format))

;; Import parser components
(use-modules ((monkey parser parser-new) #:select (make-parser parse-program))
             ((monkey parser parser-base) #:select (parser-errors)))

(define *node-counter* 0)

(define (next-node-id)
  "Generate unique node ID"
  (set! *node-counter* (+ *node-counter* 1))
  (format #f "node~a" *node-counter*))

(define (ast->mermaid node parent-id)
  "Convert AST node to Mermaid syntax"
  (let ((node-id (next-node-id)))
    (cond
     ;; Program
     ((program? node)
      (format #t "    ~a[\"Program\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (for-each 
        (lambda (stmt) (ast->mermaid stmt node-id))
        (program-statements node)))
     
     ;; Statements
     ((let-statement? node)
      (format #t "    ~a[\"Let: ~a\"]~%" 
              node-id (identifier-value (let-statement-name node)))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (ast->mermaid (let-statement-value node) node-id))
     
     ((return-statement? node)
      (format #t "    ~a[\"Return\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (when (return-statement-value node)
        (ast->mermaid (return-statement-value node) node-id)))
     
     ((expression-statement? node)
      (ast->mermaid (expression-statement-expression node) parent-id))
     
     ((block-statement? node)
      (format #t "    ~a[\"Block\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (for-each
        (lambda (stmt) (ast->mermaid stmt node-id))
        (block-statement-statements node)))
     
     ((break-statement? node)
      (format #t "    ~a[\"Break\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))
     
     ((continue-statement? node)
      (format #t "    ~a[\"Continue\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))
     
     ;; Expressions
     ((identifier? node)
      (format #t "    ~a[\"ID: ~a\"]~%" 
              node-id (identifier-value node))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))
     
     ((integer-literal? node)
      (format #t "    ~a[\"Int: ~a\"]~%" 
              node-id (integer-literal-value node))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))
     
     ((string-literal? node)
      (format #t "    ~a[\"String: ~s\"]~%" 
              node-id (string-literal-value node))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))
     
     ((boolean? node)
      (format #t "    ~a[\"Bool: ~a\"]~%" 
              node-id (if (boolean-value node) "true" "false"))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))
     
     ((prefix-expression? node)
      (format #t "    ~a[\"Prefix: ~a\"]~%" 
              node-id (prefix-expression-operator node))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (ast->mermaid (prefix-expression-right node) node-id))
     
     ((infix-expression? node)
      (format #t "    ~a[\"Infix: ~a\"]~%" 
              node-id (infix-expression-operator node))
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (ast->mermaid (infix-expression-left node) node-id)
      (ast->mermaid (infix-expression-right node) node-id))
     
     ((if-expression? node)
      (format #t "    ~a[\"If\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (let ((cond-id (next-node-id))
            (then-id (next-node-id)))
        (format #t "    ~a[\"Condition\"]~%" cond-id)
        (format #t "    ~a --> ~a~%" node-id cond-id)
        (ast->mermaid (if-expression-condition node) cond-id)
        
        (format #t "    ~a[\"Then\"]~%" then-id)
        (format #t "    ~a --> ~a~%" node-id then-id)
        (ast->mermaid (if-expression-consequence node) then-id)
        
        (when (if-expression-alternative node)
          (let ((else-id (next-node-id)))
            (format #t "    ~a[\"Else\"]~%" else-id)
            (format #t "    ~a --> ~a~%" node-id else-id)
            (ast->mermaid (if-expression-alternative node) else-id)))))
     
     ((while-expression? node)
      (format #t "    ~a[\"While\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (let ((cond-id (next-node-id))
            (body-id (next-node-id)))
        (format #t "    ~a[\"Condition\"]~%" cond-id)
        (format #t "    ~a --> ~a~%" node-id cond-id)
        (ast->mermaid (while-expression-condition node) cond-id)
        
        (format #t "    ~a[\"Body\"]~%" body-id)
        (format #t "    ~a --> ~a~%" node-id body-id)
        (ast->mermaid (while-expression-body node) body-id)))
     
     ((for-expression? node)
      (format #t "    ~a[\"For\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (when (for-expression-init node)
        (let ((init-id (next-node-id)))
          (format #t "    ~a[\"Init\"]~%" init-id)
          (format #t "    ~a --> ~a~%" node-id init-id)
          (ast->mermaid (for-expression-init node) init-id)))
      (when (for-expression-condition node)
        (let ((cond-id (next-node-id)))
          (format #t "    ~a[\"Condition\"]~%" cond-id)
          (format #t "    ~a --> ~a~%" node-id cond-id)
          (ast->mermaid (for-expression-condition node) cond-id)))
      (when (for-expression-update node)
        (let ((update-id (next-node-id)))
          (format #t "    ~a[\"Update\"]~%" update-id)
          (format #t "    ~a --> ~a~%" node-id update-id)
          (ast->mermaid (for-expression-update node) update-id)))
      (let ((body-id (next-node-id)))
        (format #t "    ~a[\"Body\"]~%" body-id)
        (format #t "    ~a --> ~a~%" node-id body-id)
        (ast->mermaid (for-expression-body node) body-id)))
     
     ((function-literal? node)
      (format #t "    ~a[\"Function\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (let ((params-id (next-node-id))
            (body-id (next-node-id)))
        (format #t "    ~a[\"Params: ~a\"]~%" 
                params-id 
                (string-join 
                  (map identifier-value (function-literal-parameters node))
                  ", "))
        (format #t "    ~a --> ~a~%" node-id params-id)
        
        (format #t "    ~a[\"Body\"]~%" body-id)
        (format #t "    ~a --> ~a~%" node-id body-id)
        (ast->mermaid (function-literal-body node) body-id)))
     
     ((call-expression? node)
      (format #t "    ~a[\"Call\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (let ((func-id (next-node-id))
            (args-id (next-node-id)))
        (format #t "    ~a[\"Function\"]~%" func-id)
        (format #t "    ~a --> ~a~%" node-id func-id)
        (ast->mermaid (call-expression-function node) func-id)
        
        (format #t "    ~a[\"Arguments\"]~%" args-id)
        (format #t "    ~a --> ~a~%" node-id args-id)
        (for-each
          (lambda (arg) (ast->mermaid arg args-id))
          (call-expression-arguments node))))
     
     ((array-literal? node)
      (format #t "    ~a[\"Array\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (for-each
        (lambda (elem) (ast->mermaid elem node-id))
        (array-literal-elements node)))
     
     ((hash-literal? node)
      (format #t "    ~a[\"Hash\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (for-each
        (lambda (pair)
          (let ((pair-id (next-node-id)))
            (format #t "    ~a[\"Pair\"]~%" pair-id)
            (format #t "    ~a --> ~a~%" node-id pair-id)
            (ast->mermaid (car pair) pair-id)
            (ast->mermaid (cdr pair) pair-id)))
        (hash-literal-pairs node)))
     
     ((index-expression? node)
      (format #t "    ~a[\"Index\"]~%" node-id)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id))
      (ast->mermaid (index-expression-left node) node-id)
      (ast->mermaid (index-expression-index node) node-id))
     
     (else
      (format #t "    ~a[\"Unknown: ~a\"]~%" node-id node)
      (when parent-id
        (format #t "    ~a --> ~a~%" parent-id node-id)))))

(define (generate-ast-diagram input)
  "Generate Mermaid diagram for parsed input"
  (let* ((l (make-lexer input))
         (p (make-parser l))
         (prog (parse-program p)))
    (format #t "graph TD~%")
    (format #t "    %% AST for: ~a~%" input)
    (format #t "~%")
    
    (if (null? (parser-errors p))
        (begin
          (ast->mermaid prog #f)
          (format #t "~%")
          (format #t "    %% Styling~%")
          (format #t "    classDef statement fill:#f9f,stroke:#333,stroke-width:2px~%")
          (format #t "    classDef expression fill:#bbf,stroke:#333,stroke-width:2px~%")
          (format #t "    classDef literal fill:#bfb,stroke:#333,stroke-width:2px~%"))
        (begin
          (format #t "    Error[\"Parse Errors:\"]~%")
          (for-each
            (lambda (err)
              (let ((err-id (next-node-id)))
                (format #t "    ~a[\"~a\"]~%" err-id err)
                (format #t "    Error --> ~a~%" err-id)))
            (parser-errors p))))))

;; Main
(define (main args)
  (if (null? (cdr args))
      (begin
        (format (current-error-port) "Usage: ~a \"monkey code\"~%" (car args))
        (exit 1))
      (generate-ast-diagram (cadr args))))

(main (command-line))
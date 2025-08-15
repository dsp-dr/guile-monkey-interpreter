#!/usr/bin/env guile
!#
;;; Generate Mermaid diagram showing module dependencies

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 ftw)
             (srfi srfi-1))

(define (extract-dependencies filename)
  "Extract module dependencies from a Scheme file"
  (catch #t
    (lambda ()
      (with-input-from-file filename
        (lambda ()
          (let loop ((deps '()))
            (let ((expr (read)))
              (if (eof-object? expr)
                  deps
                  (match expr
                    (('define-module name . rest)
                     (let ((uses (extract-uses rest)))
                       (cons (cons name uses) deps)))
                    (_ (loop deps))))))))
    (lambda args '())))

(define (extract-uses module-def)
  "Extract #:use-module declarations from module definition"
  (let loop ((def module-def) (uses '()))
    (match def
      (() uses)
      ((#:use-module module . rest)
       (loop rest (cons module uses)))
      ((_ . rest)
       (loop rest uses)))))

(define (module-name->id name)
  "Convert module name to Mermaid node ID"
  (string-join (map symbol->string name) "_"))

(define (module-name->label name)
  "Convert module name to readable label"
  (string-join (map symbol->string name) "::"))

(define (generate-mermaid-graph deps)
  "Generate Mermaid graph syntax from dependencies"
  (format #t "graph TD~%")
  (format #t "    %% Module dependency graph for Monkey interpreter~%~%")
  
  ;; Define nodes with labels
  (for-each
    (lambda (dep)
      (let* ((module (car dep))
             (id (module-name->id module))
             (label (module-name->label module)))
        (format #t "    ~a[\"~a\"]~%" id label)))
    deps)
  
  (format #t "~%")
  
  ;; Define edges
  (for-each
    (lambda (dep)
      (let ((from-id (module-name->id (car dep)))
            (uses (cdr dep)))
        (for-each
          (lambda (used-module)
            (when (assoc used-module deps) ; Only show internal deps
              (let ((to-id (module-name->id used-module)))
                (format #t "    ~a --> ~a~%" from-id to-id))))
          uses)))
    deps)
  
  ;; Add styling
  (format #t "~%")
  (format #t "    %% Styling~%")
  (format #t "    classDef parser fill:#f9f,stroke:#333,stroke-width:2px~%")
  (format #t "    classDef evaluator fill:#bbf,stroke:#333,stroke-width:2px~%")
  (format #t "    classDef ast fill:#bfb,stroke:#333,stroke-width:2px~%")
  
  ;; Apply styles based on module names
  (for-each
    (lambda (dep)
      (let* ((module (car dep))
             (id (module-name->id module))
             (name (symbol->string (last module))))
        (cond
         ((string-contains name "parser")
          (format #t "    class ~a parser~%" id))
         ((string-contains name "evaluator")
          (format #t "    class ~a evaluator~%" id))
         ((string-contains name "ast")
          (format #t "    class ~a ast~%" id)))))
    deps))

(define (analyze-directory dir)
  "Analyze all Scheme files in a directory"
  (let ((deps '()))
    (ftw dir
      (lambda (filename statinfo flag)
        (when (and (eq? flag 'regular)
                   (string-suffix? ".scm" filename)
                   (string-contains filename "monkey"))
          (let ((file-deps (extract-dependencies filename)))
            (set! deps (append deps file-deps))))
        #t))
    deps))

;; Main
(define (main)
  (let ((deps (analyze-directory ".")))
    (generate-mermaid-graph deps)))

(main)
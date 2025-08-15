#!/usr/bin/env guile
!#

(add-to-load-path "src")
(use-modules (monkey lexer lexer)
             (monkey parser parser)
             (monkey evaluator evaluator)
             (monkey object object)
             (monkey object environment)
             (ice-9 rdelim))

(define (run-file filename)
  "Run a Monkey program from a file"
  (let* ((input (call-with-input-file filename read-string))
         (lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (if (null? (parser-errors parser))
        (eval-program program env)
        (begin
          (format #t "Parser errors:~%")
          (for-each (lambda (err) (format #t "  ~a~%" err))
                   (parser-errors parser))))))

(run-file "examples/features-showcase.monkey")
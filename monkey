#!/usr/bin/env guile
!#
;;; Monkey Language Interpreter
;;; Main entry point

(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (ice-9 getopt-long)
             (monkey main))

(define (main args)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                       (version (single-char #\v) (value #f))
                       (eval (single-char #\e) (value #t))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (eval-expr (option-ref options 'eval #f))
         (files (option-ref options '() '())))
    
    (cond
     (help-wanted (show-help))
     (version-wanted (show-version))
     (eval-expr (monkey-eval eval-expr))
     ((null? files) (start-monkey-repl))
     (else (run-file (car files))))))

(main (command-line))

;;; Chapter 03 - Complete Monkey REPL with Evaluator
;;; Interactive REPL that evaluates Monkey code

(define-module (monkey main)
  #:use-module (monkey token token)
  #:use-module (monkey lexer lexer)
  #:use-module (monkey parser parser)
  #:use-module (monkey ast ast)
  #:use-module (monkey object object)
  #:use-module (monkey object environment)
  #:use-module (monkey evaluator evaluator)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:export (start-repl
            start-monkey-repl
            run-file
            monkey-eval
            show-help
            show-version
            main))

(define PROMPT ">> ")
(define MONKEY-FACE "            __,__
   .--.  .-\"     \"-.  .--.
  / .. \\/  .-. .-.  \\/ .. \\
 | |  '|  /   Y   \\  |'  | |
 | \\   \\  \\ 0 | 0 /  /   / |
  \\ '- ,\\.-\"``\"``\"-./, -' /
   ''-' /_   ^ ^   _\\ '-''
       |  \\._   _./  |
       \\   \\ '~' /   /
        '._ '-=-' _.'
           '-----'")

(define (start-repl)
  "Start the Monkey REPL"
  (display MONKEY-FACE)
  (newline)
  (display "Welcome to the Monkey Programming Language!\n")
  (display "Complete Interpreter with Chapters 1-4 Implemented\n")
  (display "Type Monkey code to evaluate (Ctrl-D to exit)\n\n")
  
  ;; Enable readline if available
  (catch #t
    (lambda () (activate-readline))
    (lambda args #t))
  
  ;; Create global environment
  (let ((env (make-environment)))
    (repl-loop env)))

(define (repl-loop env)
  "Main REPL loop"
  (display PROMPT)
  (force-output)
  (let ((line (read-line)))
    (cond
     ((eof-object? line)
      (newline)
      (display "Goodbye!\n"))
     ((string-null? line)
      (repl-loop env))
     (else
      (catch #t
        (lambda ()
          (let ((result (eval-input line env)))
            (unless (null-object? result)
              (display (object->string result))
              (newline))))
        (lambda (key . args)
          (format #t "Error: ~a~%" args)))
      (repl-loop env)))))

(define (eval-input input env)
  "Parse and evaluate input string"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (errors (parser-errors parser)))
    
    (if (not (null? errors))
        (begin
          (display "Parser errors:\n")
          (for-each (lambda (err)
                      (format #t "  ~a~%" err))
                    errors)
          *null*)
        (eval program env))))

;; Alias for compatibility
(define start-monkey-repl start-repl)

(define (run-file filename)
  "Execute a Monkey source file"
  (if (file-exists? filename)
      (let ((env (make-environment)))
        (call-with-input-file filename
          (lambda (port)
            (let ((content (get-string-all port)))
              (let ((result (eval-input content env)))
                (unless (null-object? result)
                  (display (object->string result))
                  (newline)))))))
      (begin
        (format #t "Error: File not found: ~a~%" filename)
        (exit 1))))

(define (monkey-eval expr)
  "Evaluate a Monkey expression from command line"
  (let ((env (make-environment)))
    (let ((result (eval-input expr env)))
      (unless (null-object? result)
        (display (object->string result))
        (newline)))))

(define (show-help)
  "Display help message"
  (display "Monkey Language Interpreter\n")
  (display "Usage: monkey [OPTIONS] [FILE]\n\n")
  (display "Options:\n")
  (display "  -h, --help      Show this help message\n")
  (display "  -v, --version   Show version information\n")
  (display "  -e, --eval EXPR Evaluate expression\n")
  (display "\nWithout arguments, starts the REPL\n"))

(define (show-version)
  "Display version information"
  (display "Monkey Language Interpreter v1.0\n")
  (display "Based on 'Writing An Interpreter In Go'\n")
  (display "Implemented in GNU Guile Scheme\n"))

(define (main args)
  "Main entry point"
  (if (and (> (length args) 1)
           (file-exists? (cadr args)))
      ;; Execute file if provided
      (let ((env (make-environment)))
        (call-with-input-file (cadr args)
          (lambda (port)
            (let ((content (get-string-all port)))
              (let ((result (eval-input content env)))
                (unless (null-object? result)
                  (display (object->string result))
                  (newline)))))))
      ;; Otherwise start REPL
      (start-repl)))

;; Helper to read entire file (compatibility)
(define (get-string-all port)
  "Read entire contents of a port as a string"
  (let loop ((chars '()))
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (list->string (reverse chars))
          (loop (cons ch chars))))))

;; If run as a script
(when (batch-mode?)
  (main (command-line)))

;;; ============================================================================
;;; Example usage in REPL:
;;; >> let x = 5;
;;; >> let y = 10;
;;; >> x + y
;;; 15
;;; >> let add = fn(a, b) { a + b };
;;; >> add(x, y)
;;; 15
;;; >> if (x > y) { "x is greater" } else { "y is greater" }
;;; y is greater
;;; >> let fib = fn(n) { if (n < 2) { n } else { fib(n-1) + fib(n-2) } };
;;; >> fib(10)
;;; 55
;;; ============================================================================
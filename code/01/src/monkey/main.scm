;;; Chapter 01 - Lexer REPL
;;; This goes in code/01/src/monkey/main.scm

(define-module (monkey main)
  #:use-module (monkey token token)
  #:use-module (monkey lexer lexer)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 format)
  #:export (start-repl
            main))

(define PROMPT ">> ")

(define (start-repl)
  "Start the lexer REPL"
  (display "Welcome to the Monkey Programming Language!\n")
  (display "Chapter 01: Lexer REPL\n")
  (display "Type Monkey code to see tokens (Ctrl-D to exit)\n\n")
  
  ;; Enable readline if available
  (catch #t
    (lambda () (activate-readline))
    (lambda args #t))
  
  (let loop ()
    (display PROMPT)
    (force-output)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        (newline)
        (display "Goodbye!\n"))
       ((string-null? line)
        (loop))
       (else
        (catch #t
          (lambda ()
            (display-tokens line))
          (lambda (key . args)
            (format #t "Error: ~a~%" args)))
        (loop))))))

(define (display-tokens input)
  "Tokenize input and display results"
  (let ((lexer (make-lexer input)))
    (format #t "\nTokens for: ~s\n" input)
    (format #t "~50,'-a~%" "")
    (let loop ()
      (let ((tok (next-token lexer)))
        (format #t "~15a ~s~%" 
                (token-type tok) 
                (token-literal tok))
        (unless (eq? (token-type tok) EOF)
          (loop))))
    (format #t "~50,'-a~%\n" "")))

(define (main args)
  "Main entry point"
  (start-repl))

;; If run as a script
(when (batch-mode?)
  (main (command-line)))

;;; ============================================================================
;;; Example usage in REPL:
;;; >> let x = 5;
;;; 
;;; Tokens for: "let x = 5;"
;;; --------------------------------------------------
;;; LET             "let"
;;; IDENT           "x"
;;; =               "="
;;; INT             "5"
;;; ;               ";"
;;; EOF             ""
;;; --------------------------------------------------
;;; ============================================================================
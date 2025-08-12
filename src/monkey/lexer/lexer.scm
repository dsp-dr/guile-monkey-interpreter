;;; Chapter 01 - Complete Lexer Implementation
;;; This goes in code/01/src/monkey/lexer/lexer.scm

(define-module (monkey lexer lexer)
  #:use-module (monkey token token)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-lexer
            next-token
            lexer?
            lexer-input
            lexer-position
            lexer-read-position
            lexer-ch))

;; Lexer record type
(define-record-type <lexer>
  (make-lexer* input position read-position ch)
  lexer?
  (input lexer-input)
  (position lexer-position set-lexer-position!)
  (read-position lexer-read-position set-lexer-read-position!)
  (ch lexer-ch set-lexer-ch!))

(define (make-lexer input)
  "Create a new lexer for the given input string"
  (let ((l (make-lexer* input 0 0 #f)))
    (read-char! l)
    l))

(define (read-char! lexer)
  "Read the next character and advance position"
  (if (>= (lexer-read-position lexer) (string-length (lexer-input lexer)))
      (set-lexer-ch! lexer #\nul)
      (set-lexer-ch! lexer 
                     (string-ref (lexer-input lexer) 
                                (lexer-read-position lexer))))
  (set-lexer-position! lexer (lexer-read-position lexer))
  (set-lexer-read-position! lexer (+ 1 (lexer-read-position lexer))))

(define (peek-char lexer)
  "Peek at the next character without advancing"
  (if (>= (lexer-read-position lexer) (string-length (lexer-input lexer)))
      #\nul
      (string-ref (lexer-input lexer) (lexer-read-position lexer))))

(define (skip-whitespace! lexer)
  "Skip whitespace characters"
  (while (member (lexer-ch lexer) '(#\space #\tab #\newline #\return))
    (read-char! lexer)))

(define (read-identifier lexer)
  "Read an identifier or keyword"
  (let ((position (lexer-position lexer)))
    (while (or (char-alphabetic? (lexer-ch lexer))
               (char-numeric? (lexer-ch lexer))
               (char=? (lexer-ch lexer) #\_))
      (read-char! lexer))
    (substring (lexer-input lexer) position (lexer-position lexer))))

(define (read-number lexer)
  "Read a number"
  (let ((position (lexer-position lexer)))
    (while (char-numeric? (lexer-ch lexer))
      (read-char! lexer))
    (substring (lexer-input lexer) position (lexer-position lexer))))

(define (read-string lexer)
  "Read a string literal"
  (let ((position (+ 1 (lexer-position lexer))))
    (read-char! lexer) ; skip opening quote
    (while (and (not (char=? (lexer-ch lexer) #\"))
                (not (char=? (lexer-ch lexer) #\nul)))
      (read-char! lexer))
    (let ((value (substring (lexer-input lexer) 
                           position 
                           (lexer-position lexer))))
      (read-char! lexer) ; skip closing quote
      value)))

(define (next-token lexer)
  "Get the next token from input"
  (skip-whitespace! lexer)
  
  (let ((ch (lexer-ch lexer)))
    (match ch
      ;; Single character tokens
      (#\= (let ((next-ch (peek-char lexer)))
             (if (char=? next-ch #\=)
                 (begin
                   (read-char! lexer)
                   (read-char! lexer)
                   (make-token EQ "=="))
                 (begin
                   (read-char! lexer)
                   (make-token ASSIGN "=")))))
      
      (#\+ (read-char! lexer) (make-token PLUS "+"))
      (#\- (read-char! lexer) (make-token MINUS "-"))
      (#\! (let ((next-ch (peek-char lexer)))
             (if (char=? next-ch #\=)
                 (begin
                   (read-char! lexer)
                   (read-char! lexer)
                   (make-token NOT-EQ "!="))
                 (begin
                   (read-char! lexer)
                   (make-token BANG "!")))))
      
      (#\/ (read-char! lexer) (make-token SLASH "/"))
      (#\* (read-char! lexer) (make-token ASTERISK "*"))
      (#\< (read-char! lexer) (make-token LT "<"))
      (#\> (read-char! lexer) (make-token GT ">"))
      
      (#\; (read-char! lexer) (make-token SEMICOLON ";"))
      (#\: (read-char! lexer) (make-token COLON ":"))
      (#\, (read-char! lexer) (make-token COMMA ","))
      
      (#\( (read-char! lexer) (make-token LPAREN "("))
      (#\) (read-char! lexer) (make-token RPAREN ")"))
      (#\{ (read-char! lexer) (make-token LBRACE "{"))
      (#\} (read-char! lexer) (make-token RBRACE "}"))
      (#\[ (read-char! lexer) (make-token LBRACKET "["))
      (#\] (read-char! lexer) (make-token RBRACKET "]"))
      
      (#\" (make-token STRING (read-string lexer)))
      
      (#\nul (make-token EOF ""))
      
      ;; Multi-character tokens
      (_ (cond
          ((char-alphabetic? ch)
           (let ((literal (read-identifier lexer)))
             (make-token (lookup-ident literal) literal)))
          
          ((char-numeric? ch)
           (make-token INT (read-number lexer)))
          
          (else
           (let ((tok (make-token ILLEGAL (string ch))))
             (read-char! lexer)
             tok)))))))

;; Helper macro for while loops (since Guile doesn't have built-in while)
(define-syntax while
  (syntax-rules ()
    ((_ condition body ...)
     (let loop ()
       (when condition
         body ...
         (loop))))))

;;; ============================================================================
;;; Test helpers for REPL
;;; ============================================================================

(define (tokenize input)
  "Tokenize entire input string and return list of tokens"
  (let ((lexer (make-lexer input)))
    (let loop ((tokens '()))
      (let ((tok (next-token lexer)))
        (if (eq? (token-type tok) EOF)
            (reverse (cons tok tokens))
            (loop (cons tok tokens)))))))

(define (print-tokens tokens)
  "Pretty print a list of tokens"
  (for-each (lambda (tok)
              (format #t "Type: ~15a Literal: ~s~%"
                      (token-type tok)
                      (token-literal tok)))
            tokens))
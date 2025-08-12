;;; token.scm - Token definitions for Monkey language

(define-module (monkey token token)
  #:use-module (srfi srfi-9)
  #:export (;; Token type
            make-token
            token?
            token-type
            token-literal
            
            ;; Token type constants
            ILLEGAL EOF
            IDENT INT STRING
            
            ;; Operators
            ASSIGN PLUS MINUS BANG ASTERISK SLASH
            LT GT
            EQ NOT-EQ
            
            ;; Delimiters
            COMMA SEMICOLON COLON
            LPAREN RPAREN
            LBRACE RBRACE
            LBRACKET RBRACKET
            
            ;; Keywords
            FUNCTION LET TRUE FALSE
            IF ELSE RETURN WHILE
            
            ;; Utility
            lookup-ident))

;; Token record type
(define-record-type <token>
  (make-token type literal)
  token?
  (type token-type)
  (literal token-literal))

;; Token type constants
(define ILLEGAL 'ILLEGAL)
(define EOF 'EOF)

;; Identifiers + Literals
(define IDENT 'IDENT)
(define INT 'INT)
(define STRING 'STRING)

;; Operators
(define ASSIGN '=)
(define PLUS '+)
(define MINUS '-)
(define BANG '!)
(define ASTERISK '*)
(define SLASH '/)
(define LT '<)
(define GT '>)
(define EQ '==)
(define NOT-EQ '!=)

;; Delimiters
(define COMMA ',)
(define SEMICOLON ';)
(define COLON ':)
(define LPAREN 'LPAREN)
(define RPAREN 'RPAREN)
(define LBRACE 'LBRACE)
(define RBRACE 'RBRACE)
(define LBRACKET 'LBRACKET)
(define RBRACKET 'RBRACKET)

;; Keywords
(define FUNCTION 'FUNCTION)
(define LET 'LET)
(define TRUE 'TRUE)
(define FALSE 'FALSE)
(define IF 'IF)
(define ELSE 'ELSE)
(define RETURN 'RETURN)
(define WHILE 'WHILE)

;; Keywords table
(define keywords
  `(("fn" . ,FUNCTION)
    ("let" . ,LET)
    ("true" . ,TRUE)
    ("false" . ,FALSE)
    ("if" . ,IF)
    ("else" . ,ELSE)
    ("return" . ,RETURN)
    ("while" . ,WHILE)))

(define (lookup-ident ident)
  "Look up identifier - return keyword token type or IDENT"
  (or (assoc-ref keywords ident) IDENT))

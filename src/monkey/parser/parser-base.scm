;;; parser-base.scm - Core parser infrastructure
;;; Contains parser type definition and basic operations

(define-module (monkey parser parser-base)
  #:use-module (monkey token token)
  #:use-module (monkey lexer lexer)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 format)
  #:export (
    ;; Parser type
    make-parser*
    parser?
    parser-lexer
    parser-cur-token
    parser-peek-token
    parser-errors
    parser-prefix-parse-fns
    parser-infix-parse-fns
    set-parser-cur-token!
    set-parser-peek-token!
    set-parser-errors!
    
    ;; Helper functions
    next-token!
    cur-token-is?
    peek-token-is?
    expect-peek!
    peek-precedence
    cur-precedence
    peek-error!
    no-prefix-parse-fn-error!
    parser-error!
    register-prefix
    register-infix
    
    ;; Precedence levels
    LOWEST
    EQUALS
    LESSGREATER
    SUM
    PRODUCT
    PREFIX
    CALL
    INDEX
    precedences))

;;; ============================================================================
;;; Precedence Levels
;;; ============================================================================

(define LOWEST 1)
(define EQUALS 2)      ; ==
(define LESSGREATER 3) ; > or <
(define SUM 4)         ; +
(define PRODUCT 5)     ; *
(define PREFIX 6)      ; -X or !X
(define CALL 7)        ; myFunction(X)
(define INDEX 8)       ; array[index]

(define precedences
  `((,EQ . ,EQUALS)
    (,NOT-EQ . ,EQUALS)
    (,LT . ,LESSGREATER)
    (,GT . ,LESSGREATER)
    (,PLUS . ,SUM)
    (,MINUS . ,SUM)
    (,SLASH . ,PRODUCT)
    (,ASTERISK . ,PRODUCT)
    (,LPAREN . ,CALL)
    (,LBRACKET . ,INDEX)))

;;; ============================================================================
;;; Parser Type Definition
;;; ============================================================================

(define-record-type <parser>
  (make-parser* lexer cur-token peek-token errors 
                prefix-parse-fns infix-parse-fns)
  parser?
  (lexer parser-lexer)
  (cur-token parser-cur-token set-parser-cur-token!)
  (peek-token parser-peek-token set-parser-peek-token!)
  (errors parser-errors set-parser-errors!)
  (prefix-parse-fns parser-prefix-parse-fns)
  (infix-parse-fns parser-infix-parse-fns))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (next-token! parser)
  "Advance to the next token"
  (set-parser-cur-token! parser (parser-peek-token parser))
  (set-parser-peek-token! parser (next-token (parser-lexer parser))))

(define (cur-token-is? parser type)
  "Check if current token is of given type"
  (and (parser-cur-token parser)
       (eq? (token-type (parser-cur-token parser)) type)))

(define (peek-token-is? parser type)
  "Check if peek token is of given type"
  (and (parser-peek-token parser)
       (eq? (token-type (parser-peek-token parser)) type)))

(define (expect-peek! parser type)
  "Check peek token and advance if it matches"
  (if (peek-token-is? parser type)
      (begin (next-token! parser) #t)
      (begin (peek-error! parser type) #f)))

(define (peek-precedence parser)
  "Get precedence of peek token"
  (or (assq-ref precedences 
               (token-type (parser-peek-token parser)))
      LOWEST))

(define (cur-precedence parser)
  "Get precedence of current token"
  (or (assq-ref precedences 
               (token-type (parser-cur-token parser)))
      LOWEST))

(define (peek-error! parser type)
  "Add a peek error to the parser"
  (let ((msg (format #f "expected next token to be ~a, got ~a instead"
                     type
                     (token-type (parser-peek-token parser)))))
    (set-parser-errors! parser 
                        (cons msg (parser-errors parser)))))

(define (no-prefix-parse-fn-error! parser type)
  "Add error for missing prefix parse function"
  (let ((msg (format #f "no prefix parse function for ~a found" type)))
    (set-parser-errors! parser
                        (cons msg (parser-errors parser)))))

(define (parser-error! parser msg)
  "Add a generic error to the parser"
  (set-parser-errors! parser
                      (cons msg (parser-errors parser))))

(define (register-prefix parser type fn)
  "Register a prefix parse function"
  (hash-table-set! (parser-prefix-parse-fns parser) type fn))

(define (register-infix parser type fn)
  "Register an infix parse function"
  (hash-table-set! (parser-infix-parse-fns parser) type fn))
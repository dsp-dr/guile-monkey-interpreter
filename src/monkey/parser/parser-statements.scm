;;; parser-statements.scm - Statement parsing functions
;;; Handles let, return, expression statements, and blocks

(define-module (monkey parser parser-statements)
  #:use-module (monkey token token)
  #:use-module (monkey ast ast)
  #:use-module (monkey parser parser-base)
  #:export (parse-statement
            parse-let-statement
            parse-return-statement
            parse-expression-statement
            parse-block-statement
            parse-break-statement
            parse-continue-statement
            set-parse-expression!))

;; Forward declaration - will be provided by parser-expressions
(define parse-expression #f)

(define (set-parse-expression! fn)
  "Set the parse-expression function (to avoid circular dependency)"
  (set! parse-expression fn))

(define (parse-statement parser)
  "Parse a statement"
  (cond
   ((cur-token-is? parser LET) (parse-let-statement parser))
   ((cur-token-is? parser RETURN) (parse-return-statement parser))
   ((cur-token-is? parser BREAK) (parse-break-statement parser))
   ((cur-token-is? parser CONTINUE) (parse-continue-statement parser))
   (else (parse-expression-statement parser))))

(define (parse-let-statement parser)
  "Parse let statement: let <identifier> = <expression>"
  (let ((stmt-token (parser-cur-token parser)))
    (if (not (expect-peek! parser IDENT))
        #f
        (let ((name (make-identifier (parser-cur-token parser)
                                     (token-literal (parser-cur-token parser)))))
          (if (not (expect-peek! parser ASSIGN))
              #f
              (begin
                (next-token! parser)
                (let ((value (parse-expression parser LOWEST)))
                  (when (peek-token-is? parser SEMICOLON)
                    (next-token! parser))
                  (make-let-statement stmt-token name value))))))))

(define (parse-return-statement parser)
  "Parse return statement: return <expression>"
  (let ((stmt-token (parser-cur-token parser)))
    (next-token! parser)
    (let ((value (parse-expression parser LOWEST)))
      (when (peek-token-is? parser SEMICOLON)
        (next-token! parser))
      (make-return-statement stmt-token value))))

(define (parse-expression-statement parser)
  "Parse expression statement"
  (let* ((stmt-token (parser-cur-token parser))
         (expr (parse-expression parser LOWEST)))
    (when (peek-token-is? parser SEMICOLON)
      (next-token! parser))
    (make-expression-statement stmt-token expr)))

(define (parse-block-statement parser)
  "Parse block statement: { statements }"
  (let ((block-token (parser-cur-token parser)))
    (next-token! parser)
    (let loop ((statements '()))
      (cond
       ((or (cur-token-is? parser RBRACE)
            (cur-token-is? parser EOF))
        (make-block-statement block-token (reverse statements)))
       (else
        (let ((stmt (parse-statement parser)))
          (when stmt
            (set! statements (cons stmt statements)))
          (next-token! parser)
          (loop statements)))))))

(define (parse-break-statement parser)
  "Parse break statement"
  (let ((stmt (make-break-statement (parser-cur-token parser))))
    (when (peek-token-is? parser SEMICOLON)
      (next-token! parser))
    stmt))

(define (parse-continue-statement parser)
  "Parse continue statement"
  (let ((stmt (make-continue-statement (parser-cur-token parser))))
    (when (peek-token-is? parser SEMICOLON)
      (next-token! parser))
    stmt))
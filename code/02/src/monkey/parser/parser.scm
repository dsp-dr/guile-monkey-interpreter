;;; Chapter 02 - Parser with Pratt Parsing Algorithm
;;; Implements a recursive descent parser with operator precedence

(define-module (monkey parser parser)
  #:use-module (monkey token token)
  #:use-module (monkey lexer lexer)
  #:use-module (monkey ast ast)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (make-parser
            parse-program
            parser-errors
            parser?))

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

(define (make-parser lexer)
  "Create a new parser from a lexer"
  (let ((p (make-parser* lexer #f #f '() 
                        (make-hash-table) 
                        (make-hash-table))))
    ;; Register prefix parse functions
    (register-prefix p IDENT parse-identifier)
    (register-prefix p INT parse-integer-literal)
    (register-prefix p STRING parse-string-literal)
    (register-prefix p TRUE parse-boolean)
    (register-prefix p FALSE parse-boolean)
    (register-prefix p BANG parse-prefix-expression)
    (register-prefix p MINUS parse-prefix-expression)
    (register-prefix p LPAREN parse-grouped-expression)
    (register-prefix p IF parse-if-expression)
    (register-prefix p WHILE parse-while-expression)
    (register-prefix p FUNCTION parse-function-literal)
    (register-prefix p LBRACKET parse-array-literal)
    (register-prefix p LBRACE parse-hash-literal)
    
    ;; Register infix parse functions
    (register-infix p PLUS parse-infix-expression)
    (register-infix p MINUS parse-infix-expression)
    (register-infix p SLASH parse-infix-expression)
    (register-infix p ASTERISK parse-infix-expression)
    (register-infix p EQ parse-infix-expression)
    (register-infix p NOT-EQ parse-infix-expression)
    (register-infix p LT parse-infix-expression)
    (register-infix p GT parse-infix-expression)
    (register-infix p LPAREN parse-call-expression)
    (register-infix p LBRACKET parse-index-expression)
    
    ;; Read two tokens to set cur-token and peek-token
    (next-token! p)
    (next-token! p)
    p))

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

(define (register-prefix parser type fn)
  "Register a prefix parse function"
  (hash-table-set! (parser-prefix-parse-fns parser) type fn))

(define (register-infix parser type fn)
  "Register an infix parse function"
  (hash-table-set! (parser-infix-parse-fns parser) type fn))

;;; ============================================================================
;;; Main Parsing Functions
;;; ============================================================================

(define (parse-program parser)
  "Parse the entire program"
  (let loop ((statements '()))
    (if (cur-token-is? parser EOF)
        (make-program (reverse statements))
        (let ((stmt (parse-statement parser)))
          (when stmt
            (set! statements (cons stmt statements)))
          (next-token! parser)
          (loop statements)))))

(define (parse-statement parser)
  "Parse a statement"
  (cond
   ((cur-token-is? parser LET) (parse-let-statement parser))
   ((cur-token-is? parser RETURN) (parse-return-statement parser))
   (else (parse-expression-statement parser))))

(define (parse-let-statement parser)
  "Parse let statement: let <identifier> = <expression>"
  (let ((stmt-token (parser-cur-token parser)))
    (unless (expect-peek! parser IDENT)
      (return #f))
    
    (let ((name (make-identifier (parser-cur-token parser)
                                 (token-literal (parser-cur-token parser)))))
      (unless (expect-peek! parser ASSIGN)
        (return #f))
      
      (next-token! parser)
      
      (let ((value (parse-expression parser LOWEST)))
        (when (peek-token-is? parser SEMICOLON)
          (next-token! parser))
        
        (make-let-statement stmt-token name value)))))

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

(define (parse-expression parser precedence)
  "Parse expression with precedence"
  (let* ((type (token-type (parser-cur-token parser)))
         (prefix-fn (hash-table-ref/default 
                     (parser-prefix-parse-fns parser) 
                     type 
                     #f)))
    (unless prefix-fn
      (no-prefix-parse-fn-error! parser type)
      (return #f))
    
    (let ((left-exp (prefix-fn parser)))
      (let loop ((left left-exp))
        (if (and (not (peek-token-is? parser SEMICOLON))
                 (< precedence (peek-precedence parser)))
            (let* ((infix-type (token-type (parser-peek-token parser)))
                   (infix-fn (hash-table-ref/default
                             (parser-infix-parse-fns parser)
                             infix-type
                             #f)))
              (if infix-fn
                  (begin
                    (next-token! parser)
                    (loop (infix-fn parser left)))
                  left))
            left)))))

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

;;; ============================================================================
;;; Expression Parsing Functions
;;; ============================================================================

(define (parse-identifier parser)
  "Parse identifier"
  (make-identifier (parser-cur-token parser)
                   (token-literal (parser-cur-token parser))))

(define (parse-integer-literal parser)
  "Parse integer literal"
  (let ((value (string->number (token-literal (parser-cur-token parser)))))
    (if value
        (make-integer-literal (parser-cur-token parser) value)
        (begin
          (let ((msg (format #f "could not parse ~a as integer"
                            (token-literal (parser-cur-token parser)))))
            (set-parser-errors! parser (cons msg (parser-errors parser))))
          #f))))

(define (parse-string-literal parser)
  "Parse string literal"
  (make-string-literal (parser-cur-token parser)
                       (token-literal (parser-cur-token parser))))

(define (parse-boolean parser)
  "Parse boolean literal"
  (make-boolean (parser-cur-token parser)
                (cur-token-is? parser TRUE)))

(define (parse-prefix-expression parser)
  "Parse prefix expression: !expr or -expr"
  (let ((expr-token (parser-cur-token parser))
        (operator (token-literal (parser-cur-token parser))))
    (next-token! parser)
    (let ((right (parse-expression parser PREFIX)))
      (make-prefix-expression expr-token operator right))))

(define (parse-infix-expression parser left)
  "Parse infix expression: left op right"
  (let ((expr-token (parser-cur-token parser))
        (operator (token-literal (parser-cur-token parser)))
        (precedence (cur-precedence parser)))
    (next-token! parser)
    (let ((right (parse-expression parser precedence)))
      (make-infix-expression expr-token left operator right))))

(define (parse-grouped-expression parser)
  "Parse grouped expression: (expr)"
  (next-token! parser)
  (let ((exp (parse-expression parser LOWEST)))
    (unless (expect-peek! parser RPAREN)
      #f)
    exp))

(define (parse-if-expression parser)
  "Parse if expression"
  (let ((expr-token (parser-cur-token parser)))
    (unless (expect-peek! parser LPAREN)
      (return #f))
    
    (next-token! parser)
    (let ((condition (parse-expression parser LOWEST)))
      (unless (expect-peek! parser RPAREN)
        (return #f))
      
      (unless (expect-peek! parser LBRACE)
        (return #f))
      
      (let ((consequence (parse-block-statement parser)))
        (let ((alternative (if (peek-token-is? parser ELSE)
                              (begin
                                (next-token! parser)
                                (unless (expect-peek! parser LBRACE)
                                  (return #f))
                                (parse-block-statement parser))
                              #f)))
          (make-if-expression expr-token condition consequence alternative))))))

(define (parse-while-expression parser)
  "Parse while expression"
  (let ((expr-token (parser-cur-token parser)))
    (unless (expect-peek! parser LPAREN)
      (return #f))
    
    (next-token! parser)
    (let ((condition (parse-expression parser LOWEST)))
      (unless (expect-peek! parser RPAREN)
        (return #f))
      
      (unless (expect-peek! parser LBRACE)
        (return #f))
      
      (let ((body (parse-block-statement parser)))
        (make-while-expression expr-token condition body)))))

(define (parse-function-literal parser)
  "Parse function literal"
  (let ((fn-token (parser-cur-token parser)))
    (unless (expect-peek! parser LPAREN)
      (return #f))
    
    (let ((parameters (parse-function-parameters parser)))
      (unless (expect-peek! parser LBRACE)
        (return #f))
      
      (let ((body (parse-block-statement parser)))
        (make-function-literal fn-token parameters body)))))

(define (parse-function-parameters parser)
  "Parse function parameters"
  (let loop ((params '()))
    (cond
     ((peek-token-is? parser RPAREN)
      (next-token! parser)
      (reverse params))
     
     ((null? params)
      (next-token! parser)
      (if (cur-token-is? parser RPAREN)
          '()
          (let ((param (make-identifier (parser-cur-token parser)
                                        (token-literal (parser-cur-token parser)))))
            (if (peek-token-is? parser COMMA)
                (begin
                  (next-token! parser)
                  (loop (cons param params)))
                (begin
                  (unless (expect-peek! parser RPAREN)
                    (return #f))
                  (reverse (cons param params)))))))
     
     (else
      (next-token! parser)
      (let ((param (make-identifier (parser-cur-token parser)
                                    (token-literal (parser-cur-token parser)))))
        (if (peek-token-is? parser COMMA)
            (begin
              (next-token! parser)
              (loop (cons param params)))
            (begin
              (unless (expect-peek! parser RPAREN)
                (return #f))
              (reverse (cons param params)))))))))

(define (parse-call-expression parser function)
  "Parse call expression"
  (let ((expr-token (parser-cur-token parser))
        (arguments (parse-expression-list parser RPAREN)))
    (make-call-expression expr-token function arguments)))

(define (parse-array-literal parser)
  "Parse array literal"
  (let ((array-token (parser-cur-token parser))
        (elements (parse-expression-list parser RBRACKET)))
    (make-array-literal array-token elements)))

(define (parse-index-expression parser left)
  "Parse index expression"
  (let ((expr-token (parser-cur-token parser)))
    (next-token! parser)
    (let ((index (parse-expression parser LOWEST)))
      (unless (expect-peek! parser RBRACKET)
        (return #f))
      (make-index-expression expr-token left index))))

(define (parse-hash-literal parser)
  "Parse hash literal"
  (let ((hash-token (parser-cur-token parser)))
    (let loop ((pairs '()))
      (cond
       ((peek-token-is? parser RBRACE)
        (next-token! parser)
        (make-hash-literal hash-token (reverse pairs)))
       
       ((null? pairs)
        (next-token! parser)
        (if (cur-token-is? parser RBRACE)
            (make-hash-literal hash-token '())
            (let ((key (parse-expression parser LOWEST)))
              (unless (expect-peek! parser COLON)
                (return #f))
              (next-token! parser)
              (let ((value (parse-expression parser LOWEST)))
                (if (peek-token-is? parser COMMA)
                    (begin
                      (next-token! parser)
                      (loop (cons (cons key value) pairs)))
                    (begin
                      (unless (expect-peek! parser RBRACE)
                        (return #f))
                      (make-hash-literal hash-token 
                                        (reverse (cons (cons key value) pairs)))))))))
       
       (else
        (next-token! parser)
        (let ((key (parse-expression parser LOWEST)))
          (unless (expect-peek! parser COLON)
            (return #f))
          (next-token! parser)
          (let ((value (parse-expression parser LOWEST)))
            (if (peek-token-is? parser COMMA)
                (begin
                  (next-token! parser)
                  (loop (cons (cons key value) pairs)))
                (begin
                  (unless (expect-peek! parser RBRACE)
                    (return #f))
                  (make-hash-literal hash-token 
                                    (reverse (cons (cons key value) pairs))))))))))))

(define (parse-expression-list parser end-token)
  "Parse list of expressions until end-token"
  (let loop ((expressions '()))
    (cond
     ((peek-token-is? parser end-token)
      (next-token! parser)
      (reverse expressions))
     
     ((null? expressions)
      (next-token! parser)
      (if (cur-token-is? parser end-token)
          '()
          (let ((expr (parse-expression parser LOWEST)))
            (if (peek-token-is? parser COMMA)
                (begin
                  (next-token! parser)
                  (loop (cons expr expressions)))
                (begin
                  (unless (expect-peek! parser end-token)
                    (return #f))
                  (reverse (cons expr expressions)))))))
     
     (else
      (next-token! parser)
      (let ((expr (parse-expression parser LOWEST)))
        (if (peek-token-is? parser COMMA)
            (begin
              (next-token! parser)
              (loop (cons expr expressions)))
            (begin
              (unless (expect-peek! parser end-token)
                (return #f))
              (reverse (cons expr expressions)))))))))
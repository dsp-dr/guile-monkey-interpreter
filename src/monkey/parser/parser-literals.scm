;;; parser-literals.scm - Literal parsing functions
;;; Handles identifiers, integers, strings, booleans, arrays, and hashes

(define-module (monkey parser parser-literals)
  #:use-module (monkey token token)
  #:use-module (monkey ast ast)
  #:use-module (monkey parser parser-base)
  #:export (parse-identifier
            parse-integer-literal
            parse-string-literal
            parse-boolean
            parse-array-literal
            parse-hash-literal
            parse-expression-list
            set-parse-expression!))

;; Forward declaration
(define parse-expression #f)

(define (set-parse-expression! fn)
  "Set the parse-expression function"
  (set! parse-expression fn))

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

(define (parse-array-literal parser)
  "Parse array literal"
  (let ((array-token (parser-cur-token parser))
        (elements (parse-expression-list parser RBRACKET)))
    (make-array-literal array-token elements)))

(define (parse-hash-literal parser)
  "Parse hash literal"
  (let ((hash-token (parser-cur-token parser)))
    (let loop ((pairs '()))
      (cond
       ;; Empty hash or end of hash
       ((peek-token-is? parser RBRACE)
        (next-token! parser)
        (make-hash-literal hash-token (reverse pairs)))
       
       ;; First pair or after comma
       ((null? pairs)
        (next-token! parser)
        (if (cur-token-is? parser RBRACE)
            (make-hash-literal hash-token '())
            (parse-hash-pair parser pairs loop)))
       
       ;; Continue with more pairs
       (else
        (next-token! parser)
        (parse-hash-pair parser pairs loop))))))

(define (parse-hash-pair parser pairs loop)
  "Parse a key-value pair in a hash literal"
  (let ((key (parse-expression parser LOWEST)))
    (if (not (expect-peek! parser COLON))
        #f
        (begin
          (next-token! parser)
          (let ((value (parse-expression parser LOWEST)))
            (if (peek-token-is? parser COMMA)
                (begin
                  (next-token! parser)
                  (loop (cons (cons key value) pairs)))
                (if (not (expect-peek! parser RBRACE))
                    #f
                    (make-hash-literal (parser-cur-token parser)
                                      (reverse (cons (cons key value) pairs))))))))))

(define (parse-expression-list parser end-token)
  "Parse list of expressions until end-token"
  (let loop ((expressions '()))
    (cond
     ;; End token found
     ((peek-token-is? parser end-token)
      (next-token! parser)
      (reverse expressions))
     
     ;; First expression
     ((null? expressions)
      (next-token! parser)
      (if (cur-token-is? parser end-token)
          '()
          (parse-expression-list-item parser expressions loop end-token)))
     
     ;; Continue with more expressions
     (else
      (next-token! parser)
      (parse-expression-list-item parser expressions loop end-token)))))

(define (parse-expression-list-item parser expressions loop end-token)
  "Parse a single item in an expression list"
  (let ((expr (parse-expression parser LOWEST)))
    (if (peek-token-is? parser COMMA)
        (begin
          (next-token! parser)
          (loop (cons expr expressions)))
        (if (not (expect-peek! parser end-token))
            #f
            (reverse (cons expr expressions))))))
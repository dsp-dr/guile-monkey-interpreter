;;; parser-expressions.scm - Expression parsing functions
;;; Handles prefix, infix, grouped, if, while, for, function expressions

(define-module (monkey parser parser-expressions)
  #:use-module (monkey token token)
  #:use-module (monkey ast ast)
  #:use-module (monkey parser parser-base)
  #:use-module (monkey parser parser-statements)
  #:use-module (monkey parser parser-literals)
  #:use-module (srfi srfi-69)
  #:export (parse-expression
            parse-prefix-expression
            parse-infix-expression
            parse-grouped-expression
            parse-if-expression
            parse-while-expression
            parse-for-expression
            parse-function-literal
            parse-call-expression
            parse-index-expression
            parse-lambda-shorthand))

(define (parse-expression parser precedence)
  "Parse expression with precedence"
  (let* ((type (token-type (parser-cur-token parser)))
         (prefix-fn (hash-table-ref/default 
                     (parser-prefix-parse-fns parser) 
                     type 
                     #f)))
    (if (not prefix-fn)
        (begin
          (no-prefix-parse-fn-error! parser type)
          #f)
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
                left))))))

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
    (if (not (expect-peek! parser LPAREN))
        #f
        (begin
          (next-token! parser)
          (let ((condition (parse-expression parser LOWEST)))
            (if (not (expect-peek! parser RPAREN))
                #f
                (if (not (expect-peek! parser LBRACE))
                    #f
                    (let ((consequence (parse-block-statement parser)))
                      (let ((alternative (if (peek-token-is? parser ELSE)
                                            (begin
                                              (next-token! parser)
                                              (if (not (expect-peek! parser LBRACE))
                                                  #f
                                                  (parse-block-statement parser)))
                                            #f)))
                        (make-if-expression expr-token condition consequence alternative))))))))))

(define (parse-while-expression parser)
  "Parse while expression"
  (let ((expr-token (parser-cur-token parser)))
    (if (not (expect-peek! parser LPAREN))
        #f
        (begin
          (next-token! parser)
          (let ((condition (parse-expression parser LOWEST)))
            (if (not (expect-peek! parser RPAREN))
                #f
                (if (not (expect-peek! parser LBRACE))
                    #f
                    (let ((body (parse-block-statement parser)))
                      (make-while-expression expr-token condition body)))))))))

(define (parse-for-expression parser)
  "Parse for expression: for (init; condition; update) { body }"
  (let ((expr-token (parser-cur-token parser)))
    (if (not (expect-peek! parser LPAREN))
        #f
        (begin
          ;; Parse init
          (next-token! parser)
          (let ((init (cond
                       ((cur-token-is? parser LET)
                        (parse-let-statement parser))
                       ((cur-token-is? parser SEMICOLON)
                        #f)
                       (else
                        (parse-expression-statement parser)))))
            
            ;; Expect semicolon after init
            (unless (cur-token-is? parser SEMICOLON)
              (expect-peek! parser SEMICOLON))
            
            ;; Parse condition
            (next-token! parser)
            (let ((condition (if (cur-token-is? parser SEMICOLON)
                                #f
                                (parse-expression parser LOWEST))))
              
              ;; Expect semicolon after condition
              (unless (cur-token-is? parser SEMICOLON)
                (expect-peek! parser SEMICOLON))
              
              ;; Parse update
              (next-token! parser)
              (let ((update (if (cur-token-is? parser RPAREN)
                               #f
                               (parse-expression parser LOWEST))))
                
                ;; Expect closing paren
                (unless (cur-token-is? parser RPAREN)
                  (expect-peek! parser RPAREN))
                
                ;; Parse body
                (if (not (expect-peek! parser LBRACE))
                    #f
                    (let ((body (parse-block-statement parser)))
                      (make-for-expression expr-token init condition update body))))))))))

(define (parse-function-literal parser)
  "Parse function literal"
  (let ((fn-token (parser-cur-token parser)))
    (if (not (expect-peek! parser LPAREN))
        #f
        (let ((parameters (parse-function-parameters parser)))
          (if (not (expect-peek! parser LBRACE))
              #f
              (let ((body (parse-block-statement parser)))
                (make-function-literal fn-token parameters body)))))))

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
          (parse-parameter parser params loop)))
     
     (else
      (next-token! parser)
      (parse-parameter parser params loop)))))

(define (parse-parameter parser params loop)
  "Parse a single parameter"
  (let ((param (make-identifier (parser-cur-token parser)
                                (token-literal (parser-cur-token parser)))))
    (if (peek-token-is? parser COMMA)
        (begin
          (next-token! parser)
          (loop (cons param params)))
        (if (not (expect-peek! parser RPAREN))
            #f
            (reverse (cons param params))))))

(define (parse-call-expression parser function)
  "Parse call expression"
  (let ((expr-token (parser-cur-token parser))
        (arguments (parse-expression-list parser RPAREN)))
    (make-call-expression expr-token function arguments)))

(define (parse-index-expression parser left)
  "Parse index expression"
  (let ((expr-token (parser-cur-token parser)))
    (next-token! parser)
    (let ((index (parse-expression parser LOWEST)))
      (if (not (expect-peek! parser RBRACKET))
          #f
          (make-index-expression expr-token left index)))))

(define (parse-lambda-shorthand parser)
  "Parse lambda shorthand: |x| x + 1"
  (let ((pipe-token (parser-cur-token parser)))
    (next-token! parser)
    (if (not (cur-token-is? parser IDENT))
        (begin
          (parser-error! parser (format #f "expected identifier after |, got ~a"
                                       (token-type (parser-cur-token parser))))
          #f)
        (let ((param (make-identifier (parser-cur-token parser)
                                     (token-literal (parser-cur-token parser)))))
          (if (not (expect-peek! parser PIPE))
              (begin
                (parser-error! parser (format #f "expected |, got ~a"
                                            (token-type (parser-peek-token parser))))
                #f)
              (begin
                (next-token! parser)
                (let ((body-expr (parse-expression parser LOWEST)))
                  (let ((body (make-block-statement
                              pipe-token
                              (list (make-return-statement pipe-token body-expr)))))
                    (make-function-literal pipe-token (list param) body)))))))))
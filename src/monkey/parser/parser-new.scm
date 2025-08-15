;;; parser-new.scm - Main parser module (modular version)
;;; Ties together all parser components

(define-module (monkey parser parser-new)
  #:use-module (monkey token token)
  #:use-module (monkey lexer lexer)
  #:use-module (monkey ast ast)
  #:use-module (monkey parser parser-base)
  #:use-module (monkey parser parser-statements)
  #:use-module (monkey parser parser-literals)
  #:use-module (monkey parser parser-expressions)
  #:use-module (srfi srfi-69)
  #:export (make-parser
            parse-program)
  #:re-export (parser-errors parser?))

(define (make-parser lexer)
  "Create a new parser from a lexer"
  (let ((p (make-parser* lexer #f #f '() 
                        (make-hash-table) 
                        (make-hash-table))))
    
    ;; Wire up circular dependencies
    ((@ (monkey parser parser-statements) set-parse-expression!) 
     parse-expression)
    ((@ (monkey parser parser-literals) set-parse-expression!) 
     parse-expression)
    
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
    (register-prefix p FOR parse-for-expression)
    (register-prefix p FUNCTION parse-function-literal)
    (register-prefix p PIPE parse-lambda-shorthand)
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
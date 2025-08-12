;;; Lexer Test Suite
;;; This goes in tests/lexer-test.scm

(use-modules (srfi srfi-64)
             (monkey token token)
             (monkey lexer lexer))

(test-begin "lexer")

;;; Helper to compare token lists
(define (tokens-equal? input expected)
  "Check if tokenizing input produces expected tokens"
  (let ((lexer (make-lexer input))
        (results '()))
    (let loop ()
      (let ((tok (next-token lexer)))
        (set! results (cons (list (token-type tok) 
                                 (token-literal tok)) 
                           results))
        (unless (eq? (token-type tok) EOF)
          (loop))))
    (equal? (reverse results) expected)))

(test-group "basic tokens"
  (test-assert "single character operators"
    (tokens-equal? "=+(){},;"
                   `((,ASSIGN "=")
                     (,PLUS "+")
                     (,LPAREN "(")
                     (,RPAREN ")")
                     (,LBRACE "{")
                     (,RBRACE "}")
                     (,COMMA ",")
                     (,SEMICOLON ";")
                     (,EOF ""))))
  
  (test-assert "operators"
    (tokens-equal? "+-*/<>!"
                   `((,PLUS "+")
                     (,MINUS "-")
                     (,ASTERISK "*")
                     (,SLASH "/")
                     (,LT "<")
                     (,GT ">")
                     (,BANG "!")
                     (,EOF "")))))

(test-group "two-character operators"
  (test-assert "equality operators"
    (tokens-equal? "== !="
                   `((,EQ "==")
                     (,NOT-EQ "!=")
                     (,EOF ""))))
  
  (test-assert "mixed operators"
    (tokens-equal? "= == != ="
                   `((,ASSIGN "=")
                     (,EQ "==")
                     (,NOT-EQ "!=")
                     (,ASSIGN "=")
                     (,EOF "")))))

(test-group "keywords"
  (test-assert "all keywords"
    (tokens-equal? "fn let true false if else return while"
                   `((,FUNCTION "fn")
                     (,LET "let")
                     (,TRUE "true")
                     (,FALSE "false")
                     (,IF "if")
                     (,ELSE "else")
                     (,RETURN "return")
                     (,WHILE "while")
                     (,EOF ""))))
  
  (test-assert "keywords vs identifiers"
    (tokens-equal? "let letter if iffy"
                   `((,LET "let")
                     (,IDENT "letter")
                     (,IF "if")
                     (,IDENT "iffy")
                     (,EOF "")))))

(test-group "identifiers"
  (test-assert "simple identifiers"
    (tokens-equal? "x y foo bar"
                   `((,IDENT "x")
                     (,IDENT "y")
                     (,IDENT "foo")
                     (,IDENT "bar")
                     (,EOF ""))))
  
  (test-assert "identifiers with underscores"
    (tokens-equal? "foo_bar _private __dunder__"
                   `((,IDENT "foo_bar")
                     (,IDENT "_private")
                     (,IDENT "__dunder__")
                     (,EOF ""))))
  
  (test-assert "alphanumeric identifiers"
    (tokens-equal? "foo123 test2 v1"
                   `((,IDENT "foo123")
                     (,IDENT "test2")
                     (,IDENT "v1")
                     (,EOF "")))))

(test-group "numbers"
  (test-assert "single digit"
    (tokens-equal? "5"
                   `((,INT "5")
                     (,EOF ""))))
  
  (test-assert "multi-digit"
    (tokens-equal? "123 456 789"
                   `((,INT "123")
                     (,INT "456")
                     (,INT "789")
                     (,EOF ""))))
  
  (test-assert "zero"
    (tokens-equal? "0 00 000"
                   `((,INT "0")
                     (,INT "00")
                     (,INT "000")
                     (,EOF "")))))

(test-group "strings"
  (test-assert "empty string"
    (tokens-equal? "\"\""
                   `((,STRING "")
                     (,EOF ""))))
  
  (test-assert "simple string"
    (tokens-equal? "\"hello world\""
                   `((,STRING "hello world")
                     (,EOF ""))))
  
  (test-assert "string with special chars"
    (tokens-equal? "\"hello, world!\""
                   `((,STRING "hello, world!")
                     (,EOF ""))))
  
  (test-assert "multiple strings"
    (tokens-equal? "\"foo\" \"bar\""
                   `((,STRING "foo")
                     (,STRING "bar")
                     (,EOF "")))))

(test-group "whitespace handling"
  (test-assert "spaces"
    (tokens-equal? "  let   x   =   5  "
                   `((,LET "let")
                     (,IDENT "x")
                     (,ASSIGN "=")
                     (,INT "5")
                     (,EOF ""))))
  
  (test-assert "tabs and newlines"
    (tokens-equal? "let\tx\n=\r\n5"
                   `((,LET "let")
                     (,IDENT "x")
                     (,ASSIGN "=")
                     (,INT "5")
                     (,EOF "")))))

(test-group "complex expressions"
  (test-assert "let statement"
    (tokens-equal? "let five = 5;"
                   `((,LET "let")
                     (,IDENT "five")
                     (,ASSIGN "=")
                     (,INT "5")
                     (,SEMICOLON ";")
                     (,EOF ""))))
  
  (test-assert "arithmetic expression"
    (tokens-equal? "5 + 10 * 2"
                   `((,INT "5")
                     (,PLUS "+")
                     (,INT "10")
                     (,ASTERISK "*")
                     (,INT "2")
                     (,EOF ""))))
  
  (test-assert "function definition"
    (tokens-equal? "let add = fn(x, y) { x + y };"
                   `((,LET "let")
                     (,IDENT "add")
                     (,ASSIGN "=")
                     (,FUNCTION "fn")
                     (,LPAREN "(")
                     (,IDENT "x")
                     (,COMMA ",")
                     (,IDENT "y")
                     (,RPAREN ")")
                     (,LBRACE "{")
                     (,IDENT "x")
                     (,PLUS "+")
                     (,IDENT "y")
                     (,RBRACE "}")
                     (,SEMICOLON ";")
                     (,EOF ""))))
  
  (test-assert "if expression"
    (tokens-equal? "if (x > 5) { true } else { false }"
                   `((,IF "if")
                     (,LPAREN "(")
                     (,IDENT "x")
                     (,GT ">")
                     (,INT "5")
                     (,RPAREN ")")
                     (,LBRACE "{")
                     (,TRUE "true")
                     (,RBRACE "}")
                     (,ELSE "else")
                     (,LBRACE "{")
                     (,FALSE "false")
                     (,RBRACE "}")
                     (,EOF ""))))
  
  (test-assert "while loop"
    (tokens-equal? "while (x < 10) { let x = x + 1; }"
                   `((,WHILE "while")
                     (,LPAREN "(")
                     (,IDENT "x")
                     (,LT "<")
                     (,INT "10")
                     (,RPAREN ")")
                     (,LBRACE "{")
                     (,LET "let")
                     (,IDENT "x")
                     (,ASSIGN "=")
                     (,IDENT "x")
                     (,PLUS "+")
                     (,INT "1")
                     (,SEMICOLON ";")
                     (,RBRACE "}")
                     (,EOF ""))))
  
  (test-assert "array literal"
    (tokens-equal? "[1, 2, 3]"
                   `((,LBRACKET "[")
                     (,INT "1")
                     (,COMMA ",")
                     (,INT "2")
                     (,COMMA ",")
                     (,INT "3")
                     (,RBRACKET "]")
                     (,EOF ""))))
  
  (test-assert "hash literal"
    (tokens-equal? "{\"key\": \"value\"}"
                   `((,LBRACE "{")
                     (,STRING "key")
                     (,COLON ":")
                     (,STRING "value")
                     (,RBRACE "}")
                     (,EOF "")))))

(test-group "illegal tokens"
  (test-assert "illegal character"
    (tokens-equal? "@"
                   `((,ILLEGAL "@")
                     (,EOF ""))))
  
  (test-assert "mixed legal and illegal"
    (tokens-equal? "let @ x"
                   `((,LET "let")
                     (,ILLEGAL "@")
                     (,IDENT "x")
                     (,EOF "")))))

(test-group "real monkey code"
  (test-assert "fibonacci function"
    (tokens-equal? 
     "let fibonacci = fn(n) {
        if (n < 2) {
          return n;
        }
        return fibonacci(n - 1) + fibonacci(n - 2);
      };"
     `((,LET "let")
       (,IDENT "fibonacci")
       (,ASSIGN "=")
       (,FUNCTION "fn")
       (,LPAREN "(")
       (,IDENT "n")
       (,RPAREN ")")
       (,LBRACE "{")
       (,IF "if")
       (,LPAREN "(")
       (,IDENT "n")
       (,LT "<")
       (,INT "2")
       (,RPAREN ")")
       (,LBRACE "{")
       (,RETURN "return")
       (,IDENT "n")
       (,SEMICOLON ";")
       (,RBRACE "}")
       (,RETURN "return")
       (,IDENT "fibonacci")
       (,LPAREN "(")
       (,IDENT "n")
       (,MINUS "-")
       (,INT "1")
       (,RPAREN ")")
       (,PLUS "+")
       (,IDENT "fibonacci")
       (,LPAREN "(")
       (,IDENT "n")
       (,MINUS "-")
       (,INT "2")
       (,RPAREN ")")
       (,SEMICOLON ";")
       (,RBRACE "}")
       (,SEMICOLON ";")
       (,EOF "")))))

(test-end "lexer")

;; Print summary
(let ((runner (test-runner-current)))
  (format #t "\nTest Summary:\n")
  (format #t "  Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "  Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "  Total:  ~a\n" (+ (test-runner-pass-count runner)
                                  (test-runner-fail-count runner))))
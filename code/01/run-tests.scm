#!/usr/bin/env guile
!#
;;; Chapter 01 - Lexer Test Runner
;;; Run all lexer tests

(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (srfi srfi-64)
             (monkey token token)
             (monkey lexer lexer)
             (ice-9 format))

(test-runner-factory
 (lambda ()
   (let ((runner (test-runner-simple)))
     (test-runner-on-test-end! runner
       (lambda (runner)
         (let ((name (test-runner-test-name runner))
               (result (test-result-kind runner)))
           (case result
             ((pass) (format #t "✓ ~a~%" name))
             ((fail) (format #t "✗ ~a~%" name))
             ((xpass) (format #t "⚠ ~a (unexpected pass)~%" name))
             ((xfail) (format #t "⚠ ~a (expected fail)~%" name))
             ((skip) (format #t "○ ~a (skipped)~%" name))))))
     runner)))

(test-begin "Chapter 01 - Lexer Tests")

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(define (test-token-sequence input expected-tokens)
  "Test that input produces expected token sequence"
  (let ((lexer (make-lexer input)))
    (for-each
     (lambda (expected)
       (let ((tok (next-token lexer)))
         (test-equal (format #f "Token type for '~a'" (cdr expected))
                     (car expected) (token-type tok))
         (test-equal (format #f "Token literal for '~a'" (cdr expected))
                     (cdr expected) (token-literal tok))))
     expected-tokens)))

;;; ============================================================================
;;; Basic Token Tests
;;; ============================================================================

(test-group "Single character tokens"
  (test-token-sequence "=+(){},;"
    `((,ASSIGN . "=")
      (,PLUS . "+")
      (,LPAREN . "(")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,RBRACE . "}")
      (,COMMA . ",")
      (,SEMICOLON . ";")
      (,EOF . ""))))

(test-group "Operators"
  (test-token-sequence "+-*/!<>"
    `((,PLUS . "+")
      (,MINUS . "-")
      (,ASTERISK . "*")
      (,SLASH . "/")
      (,BANG . "!")
      (,LT . "<")
      (,GT . ">")
      (,EOF . ""))))

(test-group "Delimiters"
  (test-token-sequence "[]{}(),:;"
    `((,LBRACKET . "[")
      (,RBRACKET . "]")
      (,LBRACE . "{")
      (,RBRACE . "}")
      (,LPAREN . "(")
      (,RPAREN . ")")
      (,COMMA . ",")
      (,COLON . ":")
      (,SEMICOLON . ";")
      (,EOF . ""))))

;;; ============================================================================
;;; Two-character Operator Tests
;;; ============================================================================

(test-group "Two-character operators"
  (test-token-sequence "== != = !"
    `((,EQ . "==")
      (,NOT-EQ . "!=")
      (,ASSIGN . "=")
      (,BANG . "!")
      (,EOF . "")))
  
  (test-token-sequence "===!=="
    `((,EQ . "==")
      (,ASSIGN . "=")
      (,NOT-EQ . "!=")
      (,ASSIGN . "=")
      (,EOF . ""))))

;;; ============================================================================
;;; Keyword Tests
;;; ============================================================================

(test-group "Keywords"
  (test-token-sequence "let fn true false if else return while"
    `((,LET . "let")
      (,FUNCTION . "fn")
      (,TRUE . "true")
      (,FALSE . "false")
      (,IF . "if")
      (,ELSE . "else")
      (,RETURN . "return")
      (,WHILE . "while")
      (,EOF . "")))
  
  (test-token-sequence "letter iffy returning"
    `((,IDENT . "letter")
      (,IDENT . "iffy")
      (,IDENT . "returning")
      (,EOF . ""))))

;;; ============================================================================
;;; Identifier Tests
;;; ============================================================================

(test-group "Identifiers"
  (test-token-sequence "x y foo bar foobar"
    `((,IDENT . "x")
      (,IDENT . "y")
      (,IDENT . "foo")
      (,IDENT . "bar")
      (,IDENT . "foobar")
      (,EOF . "")))
  
  (test-token-sequence "_foo foo_bar __private__ foo123 test2"
    `((,IDENT . "_foo")
      (,IDENT . "foo_bar")
      (,IDENT . "__private__")
      (,IDENT . "foo123")
      (,IDENT . "test2")
      (,EOF . ""))))

;;; ============================================================================
;;; Number Tests
;;; ============================================================================

(test-group "Numbers"
  (test-token-sequence "5 10 123 0 00 999999"
    `((,INT . "5")
      (,INT . "10")
      (,INT . "123")
      (,INT . "0")
      (,INT . "00")
      (,INT . "999999")
      (,EOF . "")))
  
  (test-token-sequence "5+10"
    `((,INT . "5")
      (,PLUS . "+")
      (,INT . "10")
      (,EOF . ""))))

;;; ============================================================================
;;; String Tests
;;; ============================================================================

(test-group "Strings"
  (test-token-sequence "\"\" \"hello\" \"hello world\""
    `((,STRING . "")
      (,STRING . "hello")
      (,STRING . "hello world")
      (,EOF . "")))
  
  (test-token-sequence "\"foo\" + \"bar\""
    `((,STRING . "foo")
      (,PLUS . "+")
      (,STRING . "bar")
      (,EOF . "")))
  
  (test-token-sequence "\"Hello, World!\""
    `((,STRING . "Hello, World!")
      (,EOF . ""))))

;;; ============================================================================
;;; Whitespace Handling Tests
;;; ============================================================================

(test-group "Whitespace handling"
  (test-token-sequence "  let   x   =   5  "
    `((,LET . "let")
      (,IDENT . "x")
      (,ASSIGN . "=")
      (,INT . "5")
      (,EOF . "")))
  
  (test-token-sequence "let\tx\n=\r\n5"
    `((,LET . "let")
      (,IDENT . "x")
      (,ASSIGN . "=")
      (,INT . "5")
      (,EOF . "")))
  
  (test-token-sequence "\n\n\nlet\n\n\nx\n\n\n"
    `((,LET . "let")
      (,IDENT . "x")
      (,EOF . ""))))

;;; ============================================================================
;;; Complex Expression Tests
;;; ============================================================================

(test-group "Complex expressions"
  (test-token-sequence "let five = 5;"
    `((,LET . "let")
      (,IDENT . "five")
      (,ASSIGN . "=")
      (,INT . "5")
      (,SEMICOLON . ";")
      (,EOF . "")))
  
  (test-token-sequence "let add = fn(x, y) { x + y };"
    `((,LET . "let")
      (,IDENT . "add")
      (,ASSIGN . "=")
      (,FUNCTION . "fn")
      (,LPAREN . "(")
      (,IDENT . "x")
      (,COMMA . ",")
      (,IDENT . "y")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,IDENT . "x")
      (,PLUS . "+")
      (,IDENT . "y")
      (,RBRACE . "}")
      (,SEMICOLON . ";")
      (,EOF . "")))
  
  (test-token-sequence "if (5 < 10) { return true; } else { return false; }"
    `((,IF . "if")
      (,LPAREN . "(")
      (,INT . "5")
      (,LT . "<")
      (,INT . "10")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,RETURN . "return")
      (,TRUE . "true")
      (,SEMICOLON . ";")
      (,RBRACE . "}")
      (,ELSE . "else")
      (,LBRACE . "{")
      (,RETURN . "return")
      (,FALSE . "false")
      (,SEMICOLON . ";")
      (,RBRACE . "}")
      (,EOF . ""))))

;;; ============================================================================
;;; Array and Hash Tests
;;; ============================================================================

(test-group "Arrays and hashes"
  (test-token-sequence "[1, 2, 3]"
    `((,LBRACKET . "[")
      (,INT . "1")
      (,COMMA . ",")
      (,INT . "2")
      (,COMMA . ",")
      (,INT . "3")
      (,RBRACKET . "]")
      (,EOF . "")))
  
  (test-token-sequence "{\"key\": \"value\"}"
    `((,LBRACE . "{")
      (,STRING . "key")
      (,COLON . ":")
      (,STRING . "value")
      (,RBRACE . "}")
      (,EOF . "")))
  
  (test-token-sequence "myArray[0]"
    `((,IDENT . "myArray")
      (,LBRACKET . "[")
      (,INT . "0")
      (,RBRACKET . "]")
      (,EOF . ""))))

;;; ============================================================================
;;; While Loop Tests
;;; ============================================================================

(test-group "While loops"
  (test-token-sequence "while (x < 10) { let x = x + 1; }"
    `((,WHILE . "while")
      (,LPAREN . "(")
      (,IDENT . "x")
      (,LT . "<")
      (,INT . "10")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,LET . "let")
      (,IDENT . "x")
      (,ASSIGN . "=")
      (,IDENT . "x")
      (,PLUS . "+")
      (,INT . "1")
      (,SEMICOLON . ";")
      (,RBRACE . "}")
      (,EOF . ""))))

;;; ============================================================================
;;; Edge Cases and Error Tests
;;; ============================================================================

(test-group "Edge cases"
  (test-token-sequence ""
    `((,EOF . "")))
  
  (test-token-sequence "!!!"
    `((,BANG . "!")
      (,BANG . "!")
      (,BANG . "!")
      (,EOF . "")))
  
  (test-token-sequence "5555"
    `((,INT . "5555")
      (,EOF . "")))
  
  (test-token-sequence "@#$"
    `((,ILLEGAL . "@")
      (,ILLEGAL . "#")
      (,ILLEGAL . "$")
      (,EOF . ""))))

;;; ============================================================================
;;; Real Monkey Code Tests
;;; ============================================================================

(test-group "Real Monkey programs"
  (test-token-sequence 
   "let fibonacci = fn(n) {
      if (n < 2) {
        return n;
      }
      return fibonacci(n - 1) + fibonacci(n - 2);
    };"
    `((,LET . "let")
      (,IDENT . "fibonacci")
      (,ASSIGN . "=")
      (,FUNCTION . "fn")
      (,LPAREN . "(")
      (,IDENT . "n")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,IF . "if")
      (,LPAREN . "(")
      (,IDENT . "n")
      (,LT . "<")
      (,INT . "2")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,RETURN . "return")
      (,IDENT . "n")
      (,SEMICOLON . ";")
      (,RBRACE . "}")
      (,RETURN . "return")
      (,IDENT . "fibonacci")
      (,LPAREN . "(")
      (,IDENT . "n")
      (,MINUS . "-")
      (,INT . "1")
      (,RPAREN . ")")
      (,PLUS . "+")
      (,IDENT . "fibonacci")
      (,LPAREN . "(")
      (,IDENT . "n")
      (,MINUS . "-")
      (,INT . "2")
      (,RPAREN . ")")
      (,SEMICOLON . ";")
      (,RBRACE . "}")
      (,SEMICOLON . ";")
      (,EOF . "")))
  
  (test-token-sequence
   "let map = fn(arr, f) {
      let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
          accumulated
        } else {
          iter(rest(arr), push(accumulated, f(first(arr))))
        }
      };
      iter(arr, [])
    };"
    `((,LET . "let")
      (,IDENT . "map")
      (,ASSIGN . "=")
      (,FUNCTION . "fn")
      (,LPAREN . "(")
      (,IDENT . "arr")
      (,COMMA . ",")
      (,IDENT . "f")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,LET . "let")
      (,IDENT . "iter")
      (,ASSIGN . "=")
      (,FUNCTION . "fn")
      (,LPAREN . "(")
      (,IDENT . "arr")
      (,COMMA . ",")
      (,IDENT . "accumulated")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,IF . "if")
      (,LPAREN . "(")
      (,IDENT . "len")
      (,LPAREN . "(")
      (,IDENT . "arr")
      (,RPAREN . ")")
      (,EQ . "==")
      (,INT . "0")
      (,RPAREN . ")")
      (,LBRACE . "{")
      (,IDENT . "accumulated")
      (,RBRACE . "}")
      (,ELSE . "else")
      (,LBRACE . "{")
      (,IDENT . "iter")
      (,LPAREN . "(")
      (,IDENT . "rest")
      (,LPAREN . "(")
      (,IDENT . "arr")
      (,RPAREN . ")")
      (,COMMA . ",")
      (,IDENT . "push")
      (,LPAREN . "(")
      (,IDENT . "accumulated")
      (,COMMA . ",")
      (,IDENT . "f")
      (,LPAREN . "(")
      (,IDENT . "first")
      (,LPAREN . "(")
      (,IDENT . "arr")
      (,RPAREN . ")")
      (,RPAREN . ")")
      (,RPAREN . ")")
      (,RPAREN . ")")
      (,RBRACE . "}")
      (,RBRACE . "}")
      (,SEMICOLON . ";")
      (,IDENT . "iter")
      (,LPAREN . "(")
      (,IDENT . "arr")
      (,COMMA . ",")
      (,LBRACKET . "[")
      (,RBRACKET . "]")
      (,RPAREN . ")")
      (,RBRACE . "}")
      (,SEMICOLON . ";")
      (,EOF . ""))))

(test-end "Chapter 01 - Lexer Tests")

;; Print test summary
(let ((runner (test-runner-current)))
  (format #t "\n")
  (format #t "========================================\n")
  (format #t "Chapter 01 - Lexer Test Results\n")
  (format #t "========================================\n")
  (format #t "Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "Total:  ~a\n" (+ (test-runner-pass-count runner)
                              (test-runner-fail-count runner)))
  (format #t "========================================\n")
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))
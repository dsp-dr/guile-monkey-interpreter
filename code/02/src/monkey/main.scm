;;; Chapter 02 - Parser REPL
;;; Interactive REPL that shows the AST for Monkey code

(define-module (monkey main)
  #:use-module (monkey token token)
  #:use-module (monkey lexer lexer)
  #:use-module (monkey parser parser)
  #:use-module (monkey ast ast)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:export (start-repl
            main))

(define PROMPT ">> ")

(define (start-repl)
  "Start the parser REPL"
  (display "Welcome to the Monkey Programming Language!\n")
  (display "Chapter 02: Parser REPL\n")
  (display "Type Monkey code to see the AST (Ctrl-D to exit)\n\n")
  
  ;; Enable readline if available
  (catch #t
    (lambda () (activate-readline))
    (lambda args #t))
  
  (let loop ()
    (display PROMPT)
    (force-output)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        (newline)
        (display "Goodbye!\n"))
       ((string-null? line)
        (loop))
       (else
        (catch #t
          (lambda ()
            (display-ast line))
          (lambda (key . args)
            (format #t "Error: ~a~%" args)))
        (loop))))))

(define (display-ast input)
  "Parse input and display the resulting AST"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (errors (parser-errors parser)))
    
    (if (not (null? errors))
        (begin
          (format #t "\nParser errors:~%")
          (for-each (lambda (err)
                      (format #t "  - ~a~%" err))
                    errors))
        (begin
          (format #t "\nInput: ~s~%" input)
          (format #t "~50,'-a~%" "")
          (format #t "AST:~%")
          (pretty-print-ast program 0)
          (format #t "~50,'-a~%" "")
          (format #t "String representation:~%")
          (format #t "~a~%" (program->string program))
          (format #t "~50,'-a~%~%" "")))))

(define (pretty-print-ast node indent)
  "Pretty print an AST node with indentation"
  (let ((spaces (make-string (* 2 indent) #\space)))
    (cond
     ;; Program
     ((program? node)
      (format #t "~aProgram~%" spaces)
      (for-each (lambda (stmt)
                  (pretty-print-ast stmt (+ indent 1)))
                (program-statements node)))
     
     ;; Statements
     ((let-statement? node)
      (format #t "~aLetStatement~%" spaces)
      (format #t "~a  Name: ~a~%" spaces 
              (identifier-value (let-statement-name node)))
      (when (let-statement-value node)
        (format #t "~a  Value:~%" spaces)
        (pretty-print-ast (let-statement-value node) (+ indent 2))))
     
     ((return-statement? node)
      (format #t "~aReturnStatement~%" spaces)
      (when (return-statement-value node)
        (format #t "~a  Value:~%" spaces)
        (pretty-print-ast (return-statement-value node) (+ indent 2))))
     
     ((expression-statement? node)
      (format #t "~aExpressionStatement~%" spaces)
      (when (expression-statement-expression node)
        (pretty-print-ast (expression-statement-expression node) (+ indent 1))))
     
     ((block-statement? node)
      (format #t "~aBlockStatement~%" spaces)
      (for-each (lambda (stmt)
                  (pretty-print-ast stmt (+ indent 1)))
                (block-statement-statements node)))
     
     ;; Expressions
     ((identifier? node)
      (format #t "~aIdentifier: ~a~%" spaces (identifier-value node)))
     
     ((integer-literal? node)
      (format #t "~aIntegerLiteral: ~a~%" spaces (integer-literal-value node)))
     
     ((string-literal? node)
      (format #t "~aStringLiteral: ~s~%" spaces (string-literal-value node)))
     
     ((boolean? node)
      (format #t "~aBoolean: ~a~%" spaces (boolean-value node)))
     
     ((prefix-expression? node)
      (format #t "~aPrefixExpression~%" spaces)
      (format #t "~a  Operator: ~a~%" spaces (prefix-expression-operator node))
      (format #t "~a  Right:~%" spaces)
      (pretty-print-ast (prefix-expression-right node) (+ indent 2)))
     
     ((infix-expression? node)
      (format #t "~aInfixExpression~%" spaces)
      (format #t "~a  Left:~%" spaces)
      (pretty-print-ast (infix-expression-left node) (+ indent 2))
      (format #t "~a  Operator: ~a~%" spaces (infix-expression-operator node))
      (format #t "~a  Right:~%" spaces)
      (pretty-print-ast (infix-expression-right node) (+ indent 2)))
     
     ((if-expression? node)
      (format #t "~aIfExpression~%" spaces)
      (format #t "~a  Condition:~%" spaces)
      (pretty-print-ast (if-expression-condition node) (+ indent 2))
      (format #t "~a  Consequence:~%" spaces)
      (pretty-print-ast (if-expression-consequence node) (+ indent 2))
      (when (if-expression-alternative node)
        (format #t "~a  Alternative:~%" spaces)
        (pretty-print-ast (if-expression-alternative node) (+ indent 2))))
     
     ((while-expression? node)
      (format #t "~aWhileExpression~%" spaces)
      (format #t "~a  Condition:~%" spaces)
      (pretty-print-ast (while-expression-condition node) (+ indent 2))
      (format #t "~a  Body:~%" spaces)
      (pretty-print-ast (while-expression-body node) (+ indent 2)))
     
     ((function-literal? node)
      (format #t "~aFunctionLiteral~%" spaces)
      (format #t "~a  Parameters: ~a~%" spaces
              (string-join (map identifier-value 
                              (function-literal-parameters node))
                          ", "))
      (format #t "~a  Body:~%" spaces)
      (pretty-print-ast (function-literal-body node) (+ indent 2)))
     
     ((call-expression? node)
      (format #t "~aCallExpression~%" spaces)
      (format #t "~a  Function:~%" spaces)
      (pretty-print-ast (call-expression-function node) (+ indent 2))
      (format #t "~a  Arguments:~%" spaces)
      (for-each (lambda (arg)
                  (pretty-print-ast arg (+ indent 2)))
                (call-expression-arguments node)))
     
     ((array-literal? node)
      (format #t "~aArrayLiteral~%" spaces)
      (format #t "~a  Elements:~%" spaces)
      (for-each (lambda (elem)
                  (pretty-print-ast elem (+ indent 2)))
                (array-literal-elements node)))
     
     ((index-expression? node)
      (format #t "~aIndexExpression~%" spaces)
      (format #t "~a  Left:~%" spaces)
      (pretty-print-ast (index-expression-left node) (+ indent 2))
      (format #t "~a  Index:~%" spaces)
      (pretty-print-ast (index-expression-index node) (+ indent 2)))
     
     ((hash-literal? node)
      (format #t "~aHashLiteral~%" spaces)
      (format #t "~a  Pairs:~%" spaces)
      (for-each (lambda (pair)
                  (format #t "~a    Key:~%" spaces)
                  (pretty-print-ast (car pair) (+ indent 3))
                  (format #t "~a    Value:~%" spaces)
                  (pretty-print-ast (cdr pair) (+ indent 3)))
                (hash-literal-pairs node)))
     
     (else
      (format #t "~aUnknown node type~%" spaces)))))

(define (main args)
  "Main entry point"
  (start-repl))

;; If run as a script
(when (batch-mode?)
  (main (command-line)))

;;; ============================================================================
;;; Example usage in REPL:
;;; >> let x = 5 + 5;
;;; 
;;; Input: "let x = 5 + 5;"
;;; --------------------------------------------------
;;; AST:
;;; Program
;;;   LetStatement
;;;     Name: x
;;;     Value:
;;;       InfixExpression
;;;         Left:
;;;           IntegerLiteral: 5
;;;         Operator: +
;;;         Right:
;;;           IntegerLiteral: 5
;;; --------------------------------------------------
;;; String representation:
;;; let x = (5 + 5);
;;; --------------------------------------------------
;;; ============================================================================
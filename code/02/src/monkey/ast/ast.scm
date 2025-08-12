;;; Chapter 02 - AST (Abstract Syntax Tree) Node Definitions
;;; This file defines all AST node types for the Monkey language

(define-module (monkey ast ast)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (;; Program
            make-program
            program?
            program-statements
            program->string
            
            ;; Statements
            make-let-statement
            let-statement?
            let-statement-token
            let-statement-name
            let-statement-value
            set-let-statement-value!
            
            make-return-statement
            return-statement?
            return-statement-token
            return-statement-value
            
            make-expression-statement
            expression-statement?
            expression-statement-token
            expression-statement-expression
            
            make-block-statement
            block-statement?
            block-statement-token
            block-statement-statements
            
            ;; Expressions
            make-identifier
            identifier?
            identifier-token
            identifier-value
            
            make-integer-literal
            integer-literal?
            integer-literal-token
            integer-literal-value
            
            make-string-literal
            string-literal?
            string-literal-token
            string-literal-value
            
            make-boolean
            boolean?
            boolean-token
            boolean-value
            
            make-prefix-expression
            prefix-expression?
            prefix-expression-token
            prefix-expression-operator
            prefix-expression-right
            set-prefix-expression-right!
            
            make-infix-expression
            infix-expression?
            infix-expression-token
            infix-expression-left
            infix-expression-operator
            infix-expression-right
            set-infix-expression-left!
            set-infix-expression-right!
            
            make-if-expression
            if-expression?
            if-expression-token
            if-expression-condition
            if-expression-consequence
            if-expression-alternative
            set-if-expression-condition!
            set-if-expression-consequence!
            set-if-expression-alternative!
            
            make-while-expression
            while-expression?
            while-expression-token
            while-expression-condition
            while-expression-body
            set-while-expression-condition!
            set-while-expression-body!
            
            make-function-literal
            function-literal?
            function-literal-token
            function-literal-parameters
            function-literal-body
            set-function-literal-body!
            
            make-call-expression
            call-expression?
            call-expression-token
            call-expression-function
            call-expression-arguments
            set-call-expression-function!
            
            make-array-literal
            array-literal?
            array-literal-token
            array-literal-elements
            
            make-index-expression
            index-expression?
            index-expression-token
            index-expression-left
            index-expression-index
            set-index-expression-left!
            set-index-expression-index!
            
            make-hash-literal
            hash-literal?
            hash-literal-token
            hash-literal-pairs
            
            ;; String conversion
            node->string
            statement->string
            expression->string))

;;; ============================================================================
;;; Program - Root node of every AST
;;; ============================================================================

(define-record-type <program>
  (make-program statements)
  program?
  (statements program-statements))

(define (program->string prog)
  (string-join (map node->string (program-statements prog)) ""))

;;; ============================================================================
;;; Statement Nodes
;;; ============================================================================

;; Let Statement: let <name> = <value>;
(define-record-type <let-statement>
  (make-let-statement token name value)
  let-statement?
  (token let-statement-token)
  (name let-statement-name)
  (value let-statement-value set-let-statement-value!))

;; Return Statement: return <value>;
(define-record-type <return-statement>
  (make-return-statement token value)
  return-statement?
  (token return-statement-token)
  (value return-statement-value))

;; Expression Statement: <expression>;
(define-record-type <expression-statement>
  (make-expression-statement token expression)
  expression-statement?
  (token expression-statement-token)
  (expression expression-statement-expression))

;; Block Statement: { <statements> }
(define-record-type <block-statement>
  (make-block-statement token statements)
  block-statement?
  (token block-statement-token)
  (statements block-statement-statements))

;;; ============================================================================
;;; Expression Nodes
;;; ============================================================================

;; Identifier: foo, bar, x, y
(define-record-type <identifier>
  (make-identifier token value)
  identifier?
  (token identifier-token)
  (value identifier-value))

;; Integer Literal: 5, 10, 999
(define-record-type <integer-literal>
  (make-integer-literal token value)
  integer-literal?
  (token integer-literal-token)
  (value integer-literal-value))

;; String Literal: "hello world"
(define-record-type <string-literal>
  (make-string-literal token value)
  string-literal?
  (token string-literal-token)
  (value string-literal-value))

;; Boolean: true, false
(define-record-type <boolean>
  (make-boolean token value)
  boolean?
  (token boolean-token)
  (value boolean-value))

;; Prefix Expression: !true, -5
(define-record-type <prefix-expression>
  (make-prefix-expression token operator right)
  prefix-expression?
  (token prefix-expression-token)
  (operator prefix-expression-operator)
  (right prefix-expression-right set-prefix-expression-right!))

;; Infix Expression: 5 + 5, x > y
(define-record-type <infix-expression>
  (make-infix-expression token left operator right)
  infix-expression?
  (token infix-expression-token)
  (left infix-expression-left set-infix-expression-left!)
  (operator infix-expression-operator)
  (right infix-expression-right set-infix-expression-right!))

;; If Expression: if (condition) { consequence } else { alternative }
(define-record-type <if-expression>
  (make-if-expression token condition consequence alternative)
  if-expression?
  (token if-expression-token)
  (condition if-expression-condition set-if-expression-condition!)
  (consequence if-expression-consequence set-if-expression-consequence!)
  (alternative if-expression-alternative set-if-expression-alternative!))

;; While Expression: while (condition) { body }
(define-record-type <while-expression>
  (make-while-expression token condition body)
  while-expression?
  (token while-expression-token)
  (condition while-expression-condition set-while-expression-condition!)
  (body while-expression-body set-while-expression-body!))

;; Function Literal: fn(x, y) { x + y }
(define-record-type <function-literal>
  (make-function-literal token parameters body)
  function-literal?
  (token function-literal-token)
  (parameters function-literal-parameters)
  (body function-literal-body set-function-literal-body!))

;; Call Expression: add(2, 3)
(define-record-type <call-expression>
  (make-call-expression token function arguments)
  call-expression?
  (token call-expression-token)
  (function call-expression-function set-call-expression-function!)
  (arguments call-expression-arguments))

;; Array Literal: [1, 2, 3]
(define-record-type <array-literal>
  (make-array-literal token elements)
  array-literal?
  (token array-literal-token)
  (elements array-literal-elements))

;; Index Expression: array[0], hash["key"]
(define-record-type <index-expression>
  (make-index-expression token left index)
  index-expression?
  (token index-expression-token)
  (left index-expression-left set-index-expression-left!)
  (index index-expression-index set-index-expression-index!))

;; Hash Literal: {"key": "value"}
(define-record-type <hash-literal>
  (make-hash-literal token pairs)
  hash-literal?
  (token hash-literal-token)
  (pairs hash-literal-pairs))

;;; ============================================================================
;;; String Conversion Functions
;;; ============================================================================

(define (node->string node)
  (cond
   ((program? node) (program->string node))
   ((let-statement? node) (statement->string node))
   ((return-statement? node) (statement->string node))
   ((expression-statement? node) (statement->string node))
   ((block-statement? node) (statement->string node))
   (else (expression->string node))))

(define (statement->string stmt)
  (cond
   ((let-statement? stmt)
    (format #f "let ~a = ~a;"
            (node->string (let-statement-name stmt))
            (if (let-statement-value stmt)
                (node->string (let-statement-value stmt))
                "")))
   
   ((return-statement? stmt)
    (format #f "return ~a;"
            (if (return-statement-value stmt)
                (node->string (return-statement-value stmt))
                "")))
   
   ((expression-statement? stmt)
    (if (expression-statement-expression stmt)
        (node->string (expression-statement-expression stmt))
        ""))
   
   ((block-statement? stmt)
    (string-append "{"
                   (string-join (map node->string 
                                   (block-statement-statements stmt))
                              "")
                   "}"))
   
   (else "")))

(define (expression->string expr)
  (cond
   ((identifier? expr)
    (identifier-value expr))
   
   ((integer-literal? expr)
    (number->string (integer-literal-value expr)))
   
   ((string-literal? expr)
    (format #f "~s" (string-literal-value expr)))
   
   ((boolean? expr)
    (if (boolean-value expr) "true" "false"))
   
   ((prefix-expression? expr)
    (format #f "(~a~a)"
            (prefix-expression-operator expr)
            (node->string (prefix-expression-right expr))))
   
   ((infix-expression? expr)
    (format #f "(~a ~a ~a)"
            (node->string (infix-expression-left expr))
            (infix-expression-operator expr)
            (node->string (infix-expression-right expr))))
   
   ((if-expression? expr)
    (format #f "if ~a ~a~a"
            (node->string (if-expression-condition expr))
            (node->string (if-expression-consequence expr))
            (if (if-expression-alternative expr)
                (format #f " else ~a" 
                        (node->string (if-expression-alternative expr)))
                "")))
   
   ((while-expression? expr)
    (format #f "while ~a ~a"
            (node->string (while-expression-condition expr))
            (node->string (while-expression-body expr))))
   
   ((function-literal? expr)
    (format #f "fn(~a) ~a"
            (string-join (map node->string 
                            (function-literal-parameters expr))
                        ", ")
            (node->string (function-literal-body expr))))
   
   ((call-expression? expr)
    (format #f "~a(~a)"
            (node->string (call-expression-function expr))
            (string-join (map node->string 
                            (call-expression-arguments expr))
                        ", ")))
   
   ((array-literal? expr)
    (format #f "[~a]"
            (string-join (map node->string 
                            (array-literal-elements expr))
                        ", ")))
   
   ((index-expression? expr)
    (format #f "(~a[~a])"
            (node->string (index-expression-left expr))
            (node->string (index-expression-index expr))))
   
   ((hash-literal? expr)
    (format #f "{~a}"
            (string-join 
             (map (lambda (pair)
                    (format #f "~a:~a"
                            (node->string (car pair))
                            (node->string (cdr pair))))
                  (hash-literal-pairs expr))
             ", ")))
   
   (else "")))
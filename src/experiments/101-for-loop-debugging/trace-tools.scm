#!/usr/bin/env guile
!#
;;; Tracing and debugging tools for for loop issues

(add-to-load-path "../..")
(use-modules (monkey lexer lexer)
             (monkey parser parser)
             (monkey ast ast)
             (monkey evaluator evaluator)
             (monkey object object)
             (monkey object environment)
             (monkey token token)
             (ice-9 format)
             (ice-9 pretty-print))

;;; ============================================================================
;;; AST Dumper
;;; ============================================================================

(define (dump-ast node indent)
  "Recursively dump AST structure with indentation"
  (let ((spaces (make-string (* 2 indent) #\space)))
    (cond
     ((program? node)
      (format #t "~aProgram:~%" spaces)
      (for-each (lambda (stmt) (dump-ast stmt (+ indent 1)))
                (program-statements node)))
     
     ((let-statement? node)
      (format #t "~aLetStatement:~%" spaces)
      (format #t "~a  name: ~a~%" spaces (identifier-value (let-statement-name node)))
      (format #t "~a  value:~%" spaces)
      (dump-ast (let-statement-value node) (+ indent 2)))
     
     ((expression-statement? node)
      (format #t "~aExpressionStatement:~%" spaces)
      (dump-ast (expression-statement-expression node) (+ indent 1)))
     
     ((for-expression? node)
      (format #t "~aForExpression:~%" spaces)
      (format #t "~a  init:~%" spaces)
      (if (for-expression-init node)
          (dump-ast (for-expression-init node) (+ indent 2))
          (format #t "~a    <empty>~%" spaces))
      (format #t "~a  condition:~%" spaces)
      (if (for-expression-condition node)
          (dump-ast (for-expression-condition node) (+ indent 2))
          (format #t "~a    <empty>~%" spaces))
      (format #t "~a  update:~%" spaces)
      (if (for-expression-update node)
          (dump-ast (for-expression-update node) (+ indent 2))
          (format #t "~a    <empty>~%" spaces))
      (format #t "~a  body:~%" spaces)
      (dump-ast (for-expression-body node) (+ indent 2)))
     
     ((while-expression? node)
      (format #t "~aWhileExpression:~%" spaces)
      (format #t "~a  condition:~%" spaces)
      (dump-ast (while-expression-condition node) (+ indent 2))
      (format #t "~a  body:~%" spaces)
      (dump-ast (while-expression-body node) (+ indent 2)))
     
     ((block-statement? node)
      (format #t "~aBlockStatement:~%" spaces)
      (for-each (lambda (stmt) (dump-ast stmt (+ indent 1)))
                (block-statement-statements node)))
     
     ((identifier? node)
      (format #t "~aIdentifier: ~a~%" spaces (identifier-value node)))
     
     ((integer-literal? node)
      (format #t "~aInteger: ~a~%" spaces (integer-literal-value node)))
     
     ((infix-expression? node)
      (format #t "~aInfixExpression: ~a~%" spaces (infix-expression-operator node))
      (format #t "~a  left:~%" spaces)
      (dump-ast (infix-expression-left node) (+ indent 2))
      (format #t "~a  right:~%" spaces)
      (dump-ast (infix-expression-right node) (+ indent 2)))
     
     ((break-statement? node)
      (format #t "~aBreakStatement~%" spaces))
     
     ((continue-statement? node)
      (format #t "~aContinueStatement~%" spaces))
     
     (else
      (format #t "~aUnknown node: ~a~%" spaces node)))))

;;; ============================================================================
;;; Token Stream Viewer
;;; ============================================================================

(define (view-tokens input)
  "Show token stream for input"
  (format #t "=== Token Stream for: ~a ===~%" input)
  (let ((l (make-lexer input)))
    (let loop ((tok (next-token l))
               (count 0))
      (unless (eq? (token-type tok) 'EOF)
        (format #t "~3d: ~12a '~a'~%" 
                count 
                (token-type tok)
                (token-literal tok))
        (loop (next-token l) (+ count 1))))
    (format #t "~3d: EOF~%" count)))

;;; ============================================================================
;;; Parser Trace
;;; ============================================================================

(define *trace-enabled* #f)
(define *trace-depth* 0)

(define (trace-enter func-name . args)
  "Trace function entry"
  (when *trace-enabled*
    (format #t "~a-> ~a~%" 
            (make-string (* 2 *trace-depth*) #\space)
            func-name)
    (set! *trace-depth* (+ *trace-depth* 1))))

(define (trace-exit func-name result)
  "Trace function exit"
  (when *trace-enabled*
    (set! *trace-depth* (- *trace-depth* 1))
    (format #t "~a<- ~a: ~a~%" 
            (make-string (* 2 *trace-depth*) #\space)
            func-name
            (if result "success" "failed"))))

;;; ============================================================================
;;; Evaluation Tracer
;;; ============================================================================

(define (trace-eval input)
  "Trace evaluation step by step"
  (format #t "~%=== Evaluation Trace: ~a ===~%" input)
  
  ;; Parse
  (format #t "~%Step 1: Lexing...~%")
  (let ((l (make-lexer input)))
    
    (format #t "Step 2: Parsing...~%")
    (let ((p (make-parser l)))
      (let ((prog (parse-program p)))
        
        (if (not (null? (parser-errors p)))
            (begin
              (format #t "PARSE ERRORS:~%")
              (for-each (lambda (err) (format #t "  - ~a~%" err))
                        (parser-errors p)))
            (begin
              (format #t "Parse successful!~%")
              (format #t "~%Step 3: AST Structure:~%")
              (dump-ast prog 0)
              
              (format #t "~%Step 4: Evaluating...~%")
              (let ((env (make-environment)))
                (let ((result (eval-program prog env)))
                  (format #t "~%Step 5: Result:~%")
                  (format #t "  Type: ~a~%" (object-type result))
                  (format #t "  Value: ~a~%" (object->string result))
                  result))))))))

;;; ============================================================================
;;; Test Individual Cases
;;; ============================================================================

(define (test-case description input expected)
  "Test a single case with detailed output"
  (format #t "~%~%=== Test: ~a ===~%" description)
  (format #t "Input: ~a~%" input)
  (format #t "Expected: ~a~%" expected)
  
  ;; First check tokens
  (view-tokens input)
  
  ;; Then trace evaluation
  (let ((result (trace-eval input)))
    (if result
        (let ((actual (object->string result)))
          (format #t "~%Actual: ~a~%" actual)
          (if (equal? actual expected)
              (format #t "✓ PASS~%")
              (format #t "✗ FAIL: Expected ~a but got ~a~%" expected actual)))
        (format #t "✗ FAIL: Evaluation failed~%"))))

;;; ============================================================================
;;; Debug Specific Issue
;;; ============================================================================

(define (debug-for-loop)
  "Debug why for loops aren't working"
  (format #t "~%=== Debugging For Loop Issues ===~%~%")
  
  ;; Check if FOR token is recognized
  (format #t "1. Checking if FOR token is recognized:~%")
  (let ((l (make-lexer "for")))
    (let ((tok (next-token l)))
      (format #t "   Token type: ~a~%" (token-type tok))
      (format #t "   Is FOR? ~a~%" (eq? (token-type tok) FOR))))
  
  ;; Check if parser has FOR registered
  (format #t "~%2. Checking parser registration:~%")
  (let ((p (make-parser (make-lexer "for (;;) {}"))))
    (format #t "   Parser created successfully~%")
    
    ;; Try to parse
    (format #t "~%3. Attempting to parse 'for (;;) {}':~%")
    (let ((prog (parse-program p)))
      (if (null? (parser-errors p))
          (begin
            (format #t "   ✓ Parse successful!~%")
            (format #t "   Statements: ~a~%" (length (program-statements prog)))
            (dump-ast prog 1))
          (begin
            (format #t "   ✗ Parse failed with errors:~%")
            (for-each (lambda (err) (format #t "     - ~a~%" err))
                      (parser-errors p))))))
  
  ;; Check evaluator
  (format #t "~%4. Checking evaluator:~%")
  (let* ((l (make-lexer "for (;;) { break; }"))
         (p (make-parser l))
         (prog (parse-program p)))
    (if (null? (parser-errors p))
        (let* ((env (make-environment))
               (result (eval-program prog env)))
          (format #t "   Evaluation result: ~a~%" (object->string result)))
        (format #t "   Cannot test evaluator - parse failed~%"))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(format #t "=== For Loop Debugging Tools ===~%")

;; First, debug the basic issue
(debug-for-loop)

;; Test simplest cases
(format #t "~%~%=== Testing Simple Cases ===~%")

(test-case "Empty for loop"
           "for (;;) { break; }"
           "null")

(test-case "Simple counter"
           "let c = 0; for (let i = 0; i < 3; i = i + 1) { let c = c + 1; } c"
           "3")

(test-case "For with break"
           "let i = 0; for (;;) { if (i == 3) { break; } let i = i + 1; } i"
           "3")

;; Test while loop for comparison
(format #t "~%~%=== Comparing with While Loop (Working) ===~%")

(test-case "While loop counter"
           "let i = 0; let c = 0; while (i < 3) { let c = c + 1; let i = i + 1; } c"
           "3")
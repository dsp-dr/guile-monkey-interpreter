;;; Chapter 03 - Evaluator
;;; Tree-walking interpreter for the Monkey language

(define-module (monkey evaluator evaluator)
  #:use-module (monkey ast ast)
  #:use-module (monkey object object)
  #:use-module (monkey object environment)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (eval
            eval-program
            new-error
            native-bool-to-boolean))

;;; ============================================================================
;;; Main Evaluation Function
;;; ============================================================================

(define (eval node env)
  "Evaluate an AST node in the given environment"
  (cond
   ;; Statements
   ((program? node) (eval-program node env))
   ((expression-statement? node) 
    (eval (expression-statement-expression node) env))
   ((block-statement? node) 
    (eval-block-statement node env))
   ((return-statement? node) 
    (eval-return-statement node env))
   ((let-statement? node) 
    (eval-let-statement node env))
   
   ;; Literals
   ((integer-literal? node) 
    (eval-integer-literal node env))
   ((boolean? node) 
    (eval-boolean node env))
   ((string-literal? node)
    (eval-string-literal node env))
   
   ;; Expressions
   ((prefix-expression? node) 
    (eval-prefix-expression node env))
   ((infix-expression? node) 
    (eval-infix-expression node env))
   ((if-expression? node) 
    (eval-if-expression node env))
   ((identifier? node) 
    (eval-identifier node env))
   ((function-literal? node) 
    (eval-function-literal node env))
   ((call-expression? node) 
    (eval-call-expression node env))
   ((array-literal? node)
    (eval-array-literal node env))
   ((hash-literal? node)
    (eval-hash-literal node env))
   ((index-expression? node)
    (eval-index-expression node env))
   ((while-expression? node)
    (eval-while-expression node env))
   
   (else 
    (new-error "Unknown node type: ~a" node))))

;;; ============================================================================
;;; Program and Statement Evaluation
;;; ============================================================================

(define (eval-program program env)
  "Evaluate a program (list of statements)"
  (let loop ((stmts (program-statements program))
             (result *null*))
    (if (null? stmts)
        result
        (let ((result (eval (car stmts) env)))
          (cond
           ((return-value? result) (return-value-value result))
           ((error-object? result) result)
           (else (loop (cdr stmts) result)))))))

(define (eval-block-statement block env)
  "Evaluate a block statement"
  (let loop ((stmts (block-statement-statements block))
             (result *null*))
    (if (null? stmts)
        result
        (let ((result (eval (car stmts) env)))
          (if (or (return-value? result)
                  (error-object? result))
              result
              (loop (cdr stmts) result))))))

(define (eval-return-statement stmt env)
  "Evaluate a return statement"
  (let ((val (eval (return-statement-value stmt) env)))
    (if (error-object? val)
        val
        (make-return-value val))))

(define (eval-let-statement stmt env)
  "Evaluate a let statement"
  (let ((val (eval (let-statement-value stmt) env)))
    (if (error-object? val)
        val
        (env-set! env 
                  (identifier-value (let-statement-name stmt))
                  val))))

;;; ============================================================================
;;; Literal Evaluation
;;; ============================================================================

(define (eval-integer-literal node env)
  "Evaluate an integer literal"
  (make-integer-object (integer-literal-value node)))

(define (eval-boolean node env)
  "Evaluate a boolean literal"
  (native-bool-to-boolean (boolean-value node)))

(define (eval-string-literal node env)
  "Evaluate a string literal"
  (make-string-object (string-literal-value node)))

;;; ============================================================================
;;; Expression Evaluation
;;; ============================================================================

(define (eval-prefix-expression node env)
  "Evaluate a prefix expression"
  (let ((right (eval (prefix-expression-right node) env)))
    (if (error-object? right)
        right
        (eval-prefix-operator (prefix-expression-operator node) right))))

(define (eval-prefix-operator operator right)
  "Apply a prefix operator"
  (match operator
    ("!" (eval-bang-operator right))
    ("-" (eval-minus-prefix-operator right))
    (_ (new-error "unknown operator: ~a~a" 
                  operator 
                  (object-type right)))))

(define (eval-bang-operator right)
  "Evaluate the ! (not) operator"
  (cond
   ((eq? right *true*) *false*)
   ((eq? right *false*) *true*)
   ((eq? right *null*) *true*)
   (else *false*)))

(define (eval-minus-prefix-operator right)
  "Evaluate the unary minus operator"
  (if (not (integer-object? right))
      (new-error "unknown operator: -~a" (object-type right))
      (make-integer-object (- (integer-object-value right)))))

(define (eval-infix-expression node env)
  "Evaluate an infix expression"
  (let ((left (eval (infix-expression-left node) env)))
    (if (error-object? left)
        left
        (let ((right (eval (infix-expression-right node) env)))
          (if (error-object? right)
              right
              (eval-infix-operator (infix-expression-operator node) 
                                   left right))))))

(define (eval-infix-operator operator left right)
  "Apply an infix operator"
  (cond
   ((and (integer-object? left) (integer-object? right))
    (eval-integer-infix-operator operator left right))
   ((and (string-object? left) (string-object? right))
    (eval-string-infix-operator operator left right))
   ((string=? operator "==")
    (native-bool-to-boolean (eq? left right)))
   ((string=? operator "!=")
    (native-bool-to-boolean (not (eq? left right))))
   ((not (eq? (object-type left) (object-type right)))
    (new-error "type mismatch: ~a ~a ~a"
               (object-type left) operator (object-type right)))
   (else
    (new-error "unknown operator: ~a ~a ~a"
               (object-type left) operator (object-type right)))))

(define (eval-integer-infix-operator operator left right)
  "Evaluate integer infix operations"
  (let ((left-val (integer-object-value left))
        (right-val (integer-object-value right)))
    (match operator
      ("+" (make-integer-object (+ left-val right-val)))
      ("-" (make-integer-object (- left-val right-val)))
      ("*" (make-integer-object (* left-val right-val)))
      ("/" (if (zero? right-val)
               (new-error "division by zero")
               (make-integer-object (quotient left-val right-val))))
      ("<" (native-bool-to-boolean (< left-val right-val)))
      (">" (native-bool-to-boolean (> left-val right-val)))
      ("==" (native-bool-to-boolean (= left-val right-val)))
      ("!=" (native-bool-to-boolean (not (= left-val right-val))))
      (_ (new-error "unknown operator: ~a ~a ~a"
                    (object-type left) operator (object-type right))))))

(define (eval-string-infix-operator operator left right)
  "Evaluate string infix operations"
  (let ((left-val (string-object-value left))
        (right-val (string-object-value right)))
    (match operator
      ("+" (make-string-object (string-append left-val right-val)))
      ("==" (native-bool-to-boolean (string=? left-val right-val)))
      ("!=" (native-bool-to-boolean (not (string=? left-val right-val))))
      (_ (new-error "unknown operator: ~a ~a ~a"
                    (object-type left) operator (object-type right))))))

(define (eval-if-expression node env)
  "Evaluate an if expression"
  (let ((condition (eval (if-expression-condition node) env)))
    (if (error-object? condition)
        condition
        (if (is-truthy? condition)
            (eval (if-expression-consequence node) env)
            (if (if-expression-alternative node)
                (eval (if-expression-alternative node) env)
                *null*)))))

(define (eval-while-expression node env)
  "Evaluate a while expression"
  (let loop ((result *null*))
    (let ((condition (eval (while-expression-condition node) env)))
      (cond
       ((error-object? condition) condition)
       ((is-truthy? condition)
        (let ((body-result (eval (while-expression-body node) env)))
          (cond
           ((error-object? body-result) body-result)
           ((return-value? body-result) body-result)
           (else (loop body-result)))))
       (else result)))))

(define (eval-identifier node env)
  "Evaluate an identifier (variable reference)"
  (let ((val (env-get env (identifier-value node))))
    (if val
        val
        (let ((builtin (get-builtin (identifier-value node))))
          (if builtin
              builtin
              (new-error "identifier not found: ~a" 
                         (identifier-value node)))))))

(define (eval-function-literal node env)
  "Evaluate a function literal"
  (make-function-object (function-literal-parameters node)
                        (function-literal-body node)
                        env))

(define (eval-call-expression node env)
  "Evaluate a function call"
  (let ((function (eval (call-expression-function node) env)))
    (if (error-object? function)
        function
        (let ((args (eval-expressions (call-expression-arguments node) env)))
          (if (and (pair? args) (error-object? (car args)))
              (car args)
              (apply-function function args))))))

(define (eval-expressions exprs env)
  "Evaluate a list of expressions"
  (let loop ((exprs exprs) (results '()))
    (if (null? exprs)
        (reverse results)
        (let ((result (eval (car exprs) env)))
          (if (error-object? result)
              (list result)
              (loop (cdr exprs) (cons result results)))))))

(define (apply-function fn args)
  "Apply a function to arguments"
  (cond
   ((function-object? fn)
    (let ((extended-env (extend-function-env fn args)))
      (if (error-object? extended-env)
          extended-env
          (let ((evaluated (eval (function-object-body fn) extended-env)))
            (unwrap-return-value evaluated)))))
   ((builtin-object? fn)
    ((builtin-object-fn fn) args))
   (else
    (new-error "not a function: ~a" (object-type fn)))))

(define (extend-function-env fn args)
  "Create environment for function execution"
  (let ((params (function-object-parameters fn)))
    (if (not (= (length params) (length args)))
        (new-error "wrong number of arguments: expected ~a, got ~a"
                   (length params) (length args))
        (let ((env (make-enclosed-environment (function-object-env fn))))
          (for-each (lambda (param arg)
                      (env-set! env (identifier-value param) arg))
                    params args)
          env))))

(define (unwrap-return-value obj)
  "Unwrap return value objects"
  (if (return-value? obj)
      (return-value-value obj)
      obj))

;;; ============================================================================
;;; Array and Hash Evaluation
;;; ============================================================================

(define (eval-array-literal node env)
  "Evaluate an array literal"
  (let ((elements (eval-expressions (array-literal-elements node) env)))
    (if (and (pair? elements) (error-object? (car elements)))
        (car elements)
        (make-array-object elements))))

(define (eval-hash-literal node env)
  "Evaluate a hash literal"
  (let loop ((pairs (hash-literal-pairs node))
             (result '()))
    (if (null? pairs)
        (make-hash-object (reverse result))
        (let* ((pair (car pairs))
               (key (eval (car pair) env)))
          (if (error-object? key)
              key
              (if (not (hashable? key))
                  (new-error "unusable as hash key: ~a" (object-type key))
                  (let ((value (eval (cdr pair) env)))
                    (if (error-object? value)
                        value
                        (loop (cdr pairs) 
                              (cons (cons key value) result))))))))))

(define (eval-index-expression node env)
  "Evaluate an index expression"
  (let ((left (eval (index-expression-left node) env)))
    (if (error-object? left)
        left
        (let ((index (eval (index-expression-index node) env)))
          (if (error-object? index)
              index
              (eval-index-operator left index))))))

(define (eval-index-operator left index)
  "Apply index operator"
  (cond
   ((and (array-object? left) (integer-object? index))
    (eval-array-index-expression left index))
   ((hash-object? left)
    (eval-hash-index-expression left index))
   (else
    (new-error "index operator not supported: ~a" (object-type left)))))

(define (eval-array-index-expression array index)
  "Index into an array"
  (let ((idx (integer-object-value index))
        (elements (array-object-elements array)))
    (if (or (< idx 0) (>= idx (length elements)))
        *null*
        (list-ref elements idx))))

(define (eval-hash-index-expression hash key)
  "Index into a hash"
  (if (not (hashable? key))
      (new-error "unusable as hash key: ~a" (object-type key))
      (let ((pair (assoc key (hash-object-pairs hash) 
                         (lambda (a b)
                           (and (eq? (object-type a) (object-type b))
                                (equal? (object->string a) 
                                       (object->string b)))))))
        (if pair (cdr pair) *null*))))

;;; ============================================================================
;;; Built-in Functions
;;; ============================================================================

(define (get-builtin name)
  "Get a built-in function by name"
  (match name
    ("len" (make-builtin-object builtin-len))
    ("first" (make-builtin-object builtin-first))
    ("last" (make-builtin-object builtin-last))
    ("rest" (make-builtin-object builtin-rest))
    ("push" (make-builtin-object builtin-push))
    ("puts" (make-builtin-object builtin-puts))
    (_ #f)))

(define (builtin-len args)
  "Built-in len function"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((arg (car args)))
        (cond
         ((string-object? arg)
          (make-integer-object (string-length (string-object-value arg))))
         ((array-object? arg)
          (make-integer-object (length (array-object-elements arg))))
         (else
          (new-error "argument to 'len' not supported, got ~a" 
                     (object-type arg)))))))

(define (builtin-first args)
  "Built-in first function"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((arg (car args)))
        (if (not (array-object? arg))
            (new-error "argument to 'first' must be ARRAY, got ~a" 
                       (object-type arg))
            (let ((elements (array-object-elements arg)))
              (if (null? elements)
                  *null*
                  (car elements)))))))

(define (builtin-last args)
  "Built-in last function"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((arg (car args)))
        (if (not (array-object? arg))
            (new-error "argument to 'last' must be ARRAY, got ~a" 
                       (object-type arg))
            (let ((elements (array-object-elements arg)))
              (if (null? elements)
                  *null*
                  (last elements)))))))

(define (builtin-rest args)
  "Built-in rest function"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((arg (car args)))
        (if (not (array-object? arg))
            (new-error "argument to 'rest' must be ARRAY, got ~a" 
                       (object-type arg))
            (let ((elements (array-object-elements arg)))
              (if (null? elements)
                  *null*
                  (make-array-object (cdr elements))))))))

(define (builtin-push args)
  "Built-in push function"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((arr (car args))
            (elem (cadr args)))
        (if (not (array-object? arr))
            (new-error "argument to 'push' must be ARRAY, got ~a" 
                       (object-type arr))
            (make-array-object 
             (append (array-object-elements arr) (list elem)))))))

(define (builtin-puts args)
  "Built-in puts function"
  (for-each (lambda (arg)
              (display (object->string arg))
              (newline))
            args)
  *null*)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (new-error fmt . args)
  "Create a new error object"
  (make-error-object (apply format #f fmt args)))

(define (native-bool-to-boolean bool)
  "Convert native boolean to Monkey boolean object"
  (if bool *true* *false*))
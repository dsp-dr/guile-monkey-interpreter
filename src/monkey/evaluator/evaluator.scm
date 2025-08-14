;;; Chapter 03 - Evaluator
;;; Tree-walking interpreter for the Monkey language

(define-module (monkey evaluator evaluator)
  #:use-module (monkey ast ast)
  #:use-module (monkey object object)
  #:use-module (monkey object environment)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (eval
            eval-program
            new-error
            native-bool-to-boolean))

;; Import needed symbols from object module
(define truthy? is-truthy?)

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
    ;; Chapter 4: Additional built-ins
    ("type" (make-builtin-object builtin-type))
    ("str" (make-builtin-object builtin-str))
    ("int" (make-builtin-object builtin-int))
    ("split" (make-builtin-object builtin-split))
    ("join" (make-builtin-object builtin-join))
    ("contains" (make-builtin-object builtin-contains))
    ("keys" (make-builtin-object builtin-keys))
    ("values" (make-builtin-object builtin-values))
    ("delete" (make-builtin-object builtin-delete))
    ;; Quick Win Extensions
    ("map" (make-builtin-object builtin-map))
    ("filter" (make-builtin-object builtin-filter))
    ("reduce" (make-builtin-object builtin-reduce))
    ("sort" (make-builtin-object builtin-sort))
    ("abs" (make-builtin-object builtin-abs))
    ("min" (make-builtin-object builtin-min))
    ("max" (make-builtin-object builtin-max))
    ("trim" (make-builtin-object builtin-trim))
    ("replace" (make-builtin-object builtin-replace))
    ("substring" (make-builtin-object builtin-substring))
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
;;; Chapter 4: Additional Built-in Functions
;;; ============================================================================

(define (builtin-type args)
  "Return the type of an object as a string"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (make-string-object (object-type (car args)))))

(define (builtin-str args)
  "Convert an object to a string"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((obj (car args)))
        (cond
         ((string-object? obj) obj)
         ((integer-object? obj)
          (make-string-object (number->string (integer-object-value obj))))
         ((boolean-object? obj)
          (make-string-object (if (boolean-object-value obj) "true" "false")))
         (else
          (make-string-object (object->string obj)))))))

(define (builtin-int args)
  "Convert a string to an integer"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((obj (car args)))
        (cond
         ((integer-object? obj) obj)
         ((string-object? obj)
          (let ((val (string->number (string-object-value obj))))
            (if val
                (make-integer-object val)
                (new-error "could not parse '~a' as integer" 
                          (string-object-value obj)))))
         (else
          (new-error "argument to 'int' must be STRING or INTEGER, got ~a"
                     (object-type obj)))))))

(define (builtin-split args)
  "Split a string by a delimiter"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((str (car args))
            (delim (cadr args)))
        (if (and (string-object? str) (string-object? delim))
            (let* ((str-val (string-object-value str))
                   (delim-val (string-object-value delim))
                   (parts (string-split str-val (string-ref delim-val 0))))
              (make-array-object 
               (map make-string-object parts)))
            (new-error "arguments to 'split' must be STRINGs")))))

(define (builtin-join args)
  "Join array elements with a delimiter"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((arr (car args))
            (delim (cadr args)))
        (if (and (array-object? arr) (string-object? delim))
            (let ((elements (array-object-elements arr))
                  (delim-val (string-object-value delim)))
              (make-string-object
               (string-join
                (map (lambda (obj)
                       (if (string-object? obj)
                           (string-object-value obj)
                           (object->string obj)))
                     elements)
                delim-val)))
            (new-error "arguments to 'join' must be ARRAY and STRING")))))

(define (builtin-contains args)
  "Check if a string contains a substring or array contains element"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((container (car args))
            (item (cadr args)))
        (cond
         ((string-object? container)
          (if (string-object? item)
              (native-bool-to-boolean
               (string-contains (string-object-value container)
                               (string-object-value item)))
              (new-error "second argument to 'contains' must be STRING when first is STRING")))
         ((array-object? container)
          (native-bool-to-boolean
           (member item (array-object-elements container) object-equal?)))
         (else
          (new-error "first argument to 'contains' must be STRING or ARRAY"))))))

(define (builtin-keys args)
  "Get keys from a hash"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((obj (car args)))
        (if (hash-object? obj)
            (make-array-object
             (map car (hash-object-pairs obj)))
            (new-error "argument to 'keys' must be HASH, got ~a"
                       (object-type obj))))))

(define (builtin-values args)
  "Get values from a hash"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((obj (car args)))
        (if (hash-object? obj)
            (make-array-object
             (map cdr (hash-object-pairs obj)))
            (new-error "argument to 'values' must be HASH, got ~a"
                       (object-type obj))))))

(define (builtin-delete args)
  "Delete a key from a hash"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((hash-obj (car args))
            (key (cadr args)))
        (if (hash-object? hash-obj)
            (let* ((pairs (hash-object-pairs hash-obj))
                   (new-pairs (filter (lambda (pair)
                                       (not (object-equal? (car pair) key)))
                                     pairs)))
              (make-hash-object new-pairs))
            (new-error "first argument to 'delete' must be HASH, got ~a"
                       (object-type hash-obj))))))

;;; ============================================================================
;;; Quick Win Extensions - Array Operations
;;; ============================================================================

(define (builtin-map args)
  "Map function over array elements"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((arr (car args))
            (fn (cadr args)))
        (cond
         ((not (array-object? arr))
          (new-error "first argument to 'map' must be ARRAY, got ~a" 
                     (object-type arr)))
         ((not (function-object? fn))
          (new-error "second argument to 'map' must be FUNCTION, got ~a"
                     (object-type fn)))
         (else
          (let* ((elements (array-object-elements arr))
                 (fn-obj (if (builtin-object? fn)
                            fn
                            fn))
                 (new-elements 
                  (map (lambda (elem)
                         (apply-function fn-obj (list elem)))
                       elements)))
            (if (any error-object? new-elements)
                (find error-object? new-elements)
                (make-array-object new-elements))))))))

(define (builtin-filter args)
  "Filter array elements by predicate"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((arr (car args))
            (fn (cadr args)))
        (cond
         ((not (array-object? arr))
          (new-error "first argument to 'filter' must be ARRAY, got ~a"
                     (object-type arr)))
         ((not (function-object? fn))
          (new-error "second argument to 'filter' must be FUNCTION, got ~a"
                     (object-type fn)))
         (else
          (let* ((elements (array-object-elements arr))
                 (filtered 
                  (filter (lambda (elem)
                            (let ((result (apply-function fn (list elem))))
                              (if (error-object? result)
                                  #f
                                  (truthy? result))))
                          elements)))
            (make-array-object filtered)))))))

(define (builtin-reduce args)
  "Reduce array to single value"
  (if (not (= (length args) 3))
      (new-error "wrong number of arguments. got=~a, want=3" (length args))
      (let ((arr (car args))
            (fn (cadr args))
            (init (caddr args)))
        (cond
         ((not (array-object? arr))
          (new-error "first argument to 'reduce' must be ARRAY, got ~a"
                     (object-type arr)))
         ((not (function-object? fn))
          (new-error "second argument to 'reduce' must be FUNCTION, got ~a"
                     (object-type fn)))
         (else
          (let ((elements (array-object-elements arr)))
            (fold (lambda (elem acc)
                    (if (error-object? acc)
                        acc
                        (apply-function fn (list acc elem))))
                  init
                  elements)))))))

(define (builtin-sort args)
  "Sort array elements"
  (if (or (< (length args) 1) (> (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=1 or 2" (length args))
      (let ((arr (car args))
            (cmp-fn (if (= (length args) 2) (cadr args) #f)))
        (cond
         ((not (array-object? arr))
          (new-error "first argument to 'sort' must be ARRAY, got ~a"
                     (object-type arr)))
         ((and cmp-fn (not (function-object? cmp-fn)))
          (new-error "second argument to 'sort' must be FUNCTION, got ~a"
                     (object-type cmp-fn)))
         (else
          (let* ((elements (array-object-elements arr))
                 (sorted 
                  (if cmp-fn
                      ;; Custom comparator
                      (sort elements
                            (lambda (a b)
                              (let ((result (apply-function cmp-fn (list a b))))
                                (if (error-object? result)
                                    #f
                                    (truthy? result)))))
                      ;; Default sort
                      (sort elements
                            (lambda (a b)
                              (cond
                               ((and (integer-object? a) (integer-object? b))
                                (< (integer-object-value a) (integer-object-value b)))
                               ((and (string-object? a) (string-object? b))
                                (string<? (string-object-value a) (string-object-value b)))
                               (else #f)))))))
            (make-array-object sorted)))))))

;;; ============================================================================
;;; Quick Win Extensions - String Functions
;;; ============================================================================

(define (builtin-trim args)
  "Trim whitespace from string"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((arg (car args)))
        (if (string-object? arg)
            (make-string-object (string-trim-both (string-object-value arg)))
            (new-error "argument to 'trim' must be STRING, got ~a"
                       (object-type arg))))))

(define (builtin-replace args)
  "Replace substring in string"
  (if (not (= (length args) 3))
      (new-error "wrong number of arguments. got=~a, want=3" (length args))
      (let ((str-obj (car args))
            (old-obj (cadr args))
            (new-obj (caddr args)))
        (if (and (string-object? str-obj)
                 (string-object? old-obj)
                 (string-object? new-obj))
            (let* ((str (string-object-value str-obj))
                   (old (string-object-value old-obj))
                   (new (string-object-value new-obj))
                   ;; Simple replace - replace all occurrences
                   (result (let loop ((s str))
                            (let ((pos (string-contains s old)))
                              (if pos
                                  (loop (string-append 
                                         (substring s 0 pos)
                                         new
                                         (substring s (+ pos (string-length old)))))
                                  s)))))
              (make-string-object result))
            (new-error "all arguments to 'replace' must be STRING")))))

(define (builtin-substring args)
  "Get substring from string"
  (if (not (= (length args) 3))
      (new-error "wrong number of arguments. got=~a, want=3" (length args))
      (let ((str-obj (car args))
            (start-obj (cadr args))
            (end-obj (caddr args)))
        (cond
         ((not (string-object? str-obj))
          (new-error "first argument to 'substring' must be STRING, got ~a"
                     (object-type str-obj)))
         ((not (integer-object? start-obj))
          (new-error "second argument to 'substring' must be INTEGER, got ~a"
                     (object-type start-obj)))
         ((not (integer-object? end-obj))
          (new-error "third argument to 'substring' must be INTEGER, got ~a"
                     (object-type end-obj)))
         (else
          (let* ((str (string-object-value str-obj))
                 (start (integer-object-value start-obj))
                 (end (integer-object-value end-obj))
                 (len (string-length str)))
            (if (or (< start 0) (> end len) (> start end))
                (new-error "invalid substring range: [~a, ~a] for string of length ~a"
                           start end len)
                (make-string-object (substring str start end)))))))))

;;; ============================================================================
;;; Quick Win Extensions - Math Functions
;;; ============================================================================

(define (builtin-abs args)
  "Absolute value"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((arg (car args)))
        (if (integer-object? arg)
            (make-integer-object (abs (integer-object-value arg)))
            (new-error "argument to 'abs' must be INTEGER, got ~a"
                       (object-type arg))))))

(define (builtin-min args)
  "Minimum value"
  (if (< (length args) 1)
      (new-error "wrong number of arguments. got=0, want=1+")
      (let ((nums (filter integer-object? args)))
        (if (not (= (length nums) (length args)))
            (new-error "all arguments to 'min' must be INTEGER")
            (make-integer-object 
             (apply min (map integer-object-value nums)))))))

(define (builtin-max args)
  "Maximum value"
  (if (< (length args) 1)
      (new-error "wrong number of arguments. got=0, want=1+")
      (let ((nums (filter integer-object? args)))
        (if (not (= (length nums) (length args)))
            (new-error "all arguments to 'max' must be INTEGER")
            (make-integer-object 
             (apply max (map integer-object-value nums)))))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(define (new-error fmt . args)
  "Create a new error object"
  (make-error-object (apply format #f fmt args)))

(define (native-bool-to-boolean bool)
  "Convert native boolean to Monkey boolean object"
  (if bool *true* *false*))

(define (object-equal? obj1 obj2)
  "Check if two objects are equal"
  (cond
   ((and (integer-object? obj1) (integer-object? obj2))
    (= (integer-object-value obj1) (integer-object-value obj2)))
   ((and (boolean-object? obj1) (boolean-object? obj2))
    (eq? (boolean-object-value obj1) (boolean-object-value obj2)))
   ((and (string-object? obj1) (string-object? obj2))
    (string=? (string-object-value obj1) (string-object-value obj2)))
   ((and (null-object? obj1) (null-object? obj2)) #t)
   (else #f)))
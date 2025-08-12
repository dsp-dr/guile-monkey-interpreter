;;; Chapter 03 - Object System
;;; Runtime representation of values in the Monkey language

(define-module (monkey object object)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (;; Object types
            INTEGER-OBJ
            BOOLEAN-OBJ
            NULL-OBJ
            RETURN-VALUE-OBJ
            ERROR-OBJ
            FUNCTION-OBJ
            STRING-OBJ
            BUILTIN-OBJ
            ARRAY-OBJ
            HASH-OBJ
            
            ;; Integer object
            make-integer-object
            integer-object?
            integer-object-value
            
            ;; Boolean object
            make-boolean-object
            boolean-object?
            boolean-object-value
            
            ;; Null object
            make-null-object
            null-object?
            *null*
            
            ;; Return value object
            make-return-value
            return-value?
            return-value-value
            
            ;; Error object
            make-error-object
            error-object?
            error-object-message
            
            ;; Function object
            make-function-object
            function-object?
            function-object-parameters
            function-object-body
            function-object-env
            
            ;; String object
            make-string-object
            string-object?
            string-object-value
            
            ;; Built-in function object
            make-builtin-object
            builtin-object?
            builtin-object-fn
            
            ;; Array object
            make-array-object
            array-object?
            array-object-elements
            
            ;; Hash object
            make-hash-object
            hash-object?
            hash-object-pairs
            
            ;; Utility functions
            object-type
            object->string
            object-inspect
            is-truthy?
            is-error?
            
            ;; Hash key functions
            make-hash-key
            hash-key?
            hash-key-type
            hash-key-value
            hashable?))

;;; ============================================================================
;;; Object Type Constants
;;; ============================================================================

(define INTEGER-OBJ 'INTEGER)
(define BOOLEAN-OBJ 'BOOLEAN)
(define NULL-OBJ 'NULL)
(define RETURN-VALUE-OBJ 'RETURN_VALUE)
(define ERROR-OBJ 'ERROR)
(define FUNCTION-OBJ 'FUNCTION)
(define STRING-OBJ 'STRING)
(define BUILTIN-OBJ 'BUILTIN)
(define ARRAY-OBJ 'ARRAY)
(define HASH-OBJ 'HASH)

;;; ============================================================================
;;; Integer Object
;;; ============================================================================

(define-record-type <integer-object>
  (make-integer-object value)
  integer-object?
  (value integer-object-value))

;;; ============================================================================
;;; Boolean Object
;;; ============================================================================

(define-record-type <boolean-object>
  (make-boolean-object value)
  boolean-object?
  (value boolean-object-value))

;; Singleton boolean objects for efficiency
(define *true* (make-boolean-object #t))
(define *false* (make-boolean-object #f))

;;; ============================================================================
;;; Null Object
;;; ============================================================================

(define-record-type <null-object>
  (make-null-object*)
  null-object?)

;; Singleton null object
(define *null* (make-null-object*))

(define (make-null-object) *null*)

;;; ============================================================================
;;; Return Value Object
;;; ============================================================================

(define-record-type <return-value>
  (make-return-value value)
  return-value?
  (value return-value-value))

;;; ============================================================================
;;; Error Object
;;; ============================================================================

(define-record-type <error-object>
  (make-error-object message)
  error-object?
  (message error-object-message))

;;; ============================================================================
;;; Function Object
;;; ============================================================================

(define-record-type <function-object>
  (make-function-object parameters body env)
  function-object?
  (parameters function-object-parameters)
  (body function-object-body)
  (env function-object-env))

;;; ============================================================================
;;; String Object
;;; ============================================================================

(define-record-type <string-object>
  (make-string-object value)
  string-object?
  (value string-object-value))

;;; ============================================================================
;;; Built-in Function Object
;;; ============================================================================

(define-record-type <builtin-object>
  (make-builtin-object fn)
  builtin-object?
  (fn builtin-object-fn))

;;; ============================================================================
;;; Array Object
;;; ============================================================================

(define-record-type <array-object>
  (make-array-object elements)
  array-object?
  (elements array-object-elements))

;;; ============================================================================
;;; Hash Object
;;; ============================================================================

(define-record-type <hash-object>
  (make-hash-object pairs)
  hash-object?
  (pairs hash-object-pairs))

;;; ============================================================================
;;; Hash Key (for hashable values)
;;; ============================================================================

(define-record-type <hash-key>
  (make-hash-key type value)
  hash-key?
  (type hash-key-type)
  (value hash-key-value))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (object-type obj)
  "Get the type of an object"
  (cond
   ((integer-object? obj) INTEGER-OBJ)
   ((boolean-object? obj) BOOLEAN-OBJ)
   ((null-object? obj) NULL-OBJ)
   ((return-value? obj) RETURN-VALUE-OBJ)
   ((error-object? obj) ERROR-OBJ)
   ((function-object? obj) FUNCTION-OBJ)
   ((string-object? obj) STRING-OBJ)
   ((builtin-object? obj) BUILTIN-OBJ)
   ((array-object? obj) ARRAY-OBJ)
   ((hash-object? obj) HASH-OBJ)
   (else 'UNKNOWN)))

(define (object->string obj)
  "Convert an object to its string representation"
  (cond
   ((integer-object? obj)
    (number->string (integer-object-value obj)))
   
   ((boolean-object? obj)
    (if (boolean-object-value obj) "true" "false"))
   
   ((null-object? obj) "null")
   
   ((return-value? obj)
    (object->string (return-value-value obj)))
   
   ((error-object? obj)
    (format #f "ERROR: ~a" (error-object-message obj)))
   
   ((function-object? obj)
    (format #f "fn(~a) { ... }"
            (string-join 
             (map (lambda (p) (if (string? p) p "?"))
                  (function-object-parameters obj))
             ", ")))
   
   ((string-object? obj)
    (string-object-value obj))
   
   ((builtin-object? obj)
    "builtin function")
   
   ((array-object? obj)
    (format #f "[~a]"
            (string-join
             (map object->string (array-object-elements obj))
             ", ")))
   
   ((hash-object? obj)
    (let ((pairs (hash-object-pairs obj)))
      (format #f "{~a}"
              (string-join
               (map (lambda (pair)
                      (format #f "~a: ~a"
                              (object->string (car pair))
                              (object->string (cdr pair))))
                    pairs)
               ", "))))
   
   (else "unknown object")))

(define (object-inspect obj)
  "Get detailed inspection string for an object"
  (cond
   ((integer-object? obj)
    (format #f "Integer(~a)" (integer-object-value obj)))
   
   ((boolean-object? obj)
    (format #f "Boolean(~a)" (boolean-object-value obj)))
   
   ((null-object? obj) "Null")
   
   ((return-value? obj)
    (format #f "ReturnValue(~a)" 
            (object-inspect (return-value-value obj))))
   
   ((error-object? obj)
    (format #f "Error(~s)" (error-object-message obj)))
   
   ((function-object? obj)
    (format #f "Function(params: ~a, env: ~a)"
            (length (function-object-parameters obj))
            (if (function-object-env obj) "yes" "no")))
   
   ((string-object? obj)
    (format #f "String(~s)" (string-object-value obj)))
   
   ((builtin-object? obj)
    "Builtin(function)")
   
   ((array-object? obj)
    (format #f "Array(length: ~a)" 
            (length (array-object-elements obj))))
   
   ((hash-object? obj)
    (format #f "Hash(pairs: ~a)"
            (length (hash-object-pairs obj))))
   
   (else "Unknown")))

(define (is-truthy? obj)
  "Determine if an object is truthy"
  (cond
   ((null-object? obj) #f)
   ((boolean-object? obj) (boolean-object-value obj))
   (else #t)))

(define (is-error? obj)
  "Check if an object is an error"
  (and obj (error-object? obj)))

(define (hashable? obj)
  "Check if an object can be used as a hash key"
  (or (integer-object? obj)
      (boolean-object? obj)
      (string-object? obj)))

;;; ============================================================================
;;; Helper for creating commonly used objects
;;; ============================================================================

(define (native-bool-to-boolean-object bool)
  "Convert native Scheme boolean to Monkey boolean object"
  (if bool *true* *false*))
;;; Chapter 03 - Environment
;;; Variable storage and scoping for the evaluator

(define-module (monkey object environment)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)  ; Hash tables
  #:export (make-environment
            make-enclosed-environment
            environment?
            env-get
            env-set!
            environment-store
            environment-outer))

;;; ============================================================================
;;; Environment Type
;;; ============================================================================

(define-record-type <environment>
  (make-environment* store outer)
  environment?
  (store environment-store)
  (outer environment-outer))

;;; ============================================================================
;;; Constructor Functions
;;; ============================================================================

(define (make-environment)
  "Create a new empty environment"
  (make-environment* (make-hash-table) #f))

(define (make-enclosed-environment outer)
  "Create a new environment enclosed by an outer environment"
  (make-environment* (make-hash-table) outer))

;;; ============================================================================
;;; Environment Operations
;;; ============================================================================

(define (env-get env name)
  "Get a value from the environment by name"
  (let ((value (hash-table-ref/default (environment-store env) name #f)))
    (if value
        value
        (if (environment-outer env)
            (env-get (environment-outer env) name)
            #f))))

(define (env-set! env name value)
  "Set a value in the environment"
  (hash-table-set! (environment-store env) name value)
  value)

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(define (env-keys env)
  "Get all keys defined in this environment (not including outer)"
  (hash-table-keys (environment-store env)))

(define (env-all-keys env)
  "Get all keys defined in this environment and all outer environments"
  (let loop ((e env) (keys '()))
    (if e
        (loop (environment-outer e)
              (append (hash-table-keys (environment-store e)) keys))
        (delete-duplicates keys))))

(define (env-copy env)
  "Create a shallow copy of an environment"
  (let ((new-store (make-hash-table)))
    (hash-table-walk (environment-store env)
                     (lambda (key value)
                       (hash-table-set! new-store key value)))
    (make-environment* new-store (environment-outer env))))

(define (env-extend env bindings)
  "Create a new environment with additional bindings"
  (let ((new-env (make-enclosed-environment env)))
    (for-each (lambda (binding)
                (env-set! new-env (car binding) (cdr binding)))
              bindings)
    new-env))
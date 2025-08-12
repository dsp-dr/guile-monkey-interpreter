#!/usr/bin/env guile
!#
;;; Test suite for AST validator

(add-to-load-path (dirname (dirname (dirname (current-filename)))))

(use-modules (monkey ir ast-spec)
             (monkey ir ast-validator)
             (srfi srfi-64)
             (ice-9 format))

(test-begin "AST Validator Tests")

;;; ============================================================================
;;; Valid AST Tests
;;; ============================================================================

(test-group "Valid AST Examples"
  
  (test-assert "Empty program"
    (let-values (((valid? errors) 
                  (validate-ast '(program ((statements . ()))))))
      valid?))
  
  (test-assert "Simple integer literal"
    (let-values (((valid? errors)
                  (validate-ast 
                   '(program
                     ((statements . 
                       ((expression-statement
                         ((expression . 
                           (integer-literal ((value . 42)))))))))))))
      valid?))
  
  (test-assert "Simple arithmetic"
    (let-values (((valid? errors)
                  (validate-ast
                   '(program
                     ((statements . 
                       ((expression-statement
                         ((expression . 
                           (infix-expression
                            ((operator . "+")
                             (left . (integer-literal ((value . 5))))
                             (right . (integer-literal ((value . 10)))))))))))))))))
      valid?))
  
  (test-assert "Let statement"
    (let-values (((valid? errors)
                  (validate-ast
                   '(program
                     ((statements . 
                       ((let-statement
                         ((name . (identifier ((value . "x"))))
                          (value . (integer-literal ((value . 42)))))))))))))
      valid?))
  
  (test-assert "Function literal"
    (let-values (((valid? errors)
                  (validate-ast
                   '(function-literal
                     ((parameters . ((identifier ((value . "x")))))
                      (body . (block-statement ((statements . ())))))))))
      valid?))
  
  (test-assert "If expression"
    (let-values (((valid? errors)
                  (validate-ast
                   '(if-expression
                     ((condition . (boolean-literal ((value . #t))))
                      (consequence . (block-statement ((statements . ()))))
                      (alternative . (block-statement ((statements . ())))))))))
      valid?)))

;;; ============================================================================
;;; Invalid AST Tests
;;; ============================================================================

(test-group "Invalid AST Examples"
  
  (test-assert "Unknown node type"
    (let-values (((valid? errors)
                  (validate-ast '(unknown-node ((value . 42))))))
      (and (not valid?)
           (not (null? errors)))))
  
  (test-assert "Missing required field"
    (let-values (((valid? errors)
                  (validate-ast
                   '(let-statement
                     ((value . (integer-literal ((value . 42))))))))) ; missing 'name'
      (and (not valid?)
           (any (lambda (err) (string-contains err "Missing required field"))
                errors))))
  
  (test-assert "Type mismatch - string instead of integer"
    (let-values (((valid? errors)
                  (validate-ast
                   '(integer-literal ((value . "not-an-integer"))))))
      (and (not valid?)
           (any (lambda (err) (string-contains err "Type mismatch"))
                errors))))
  
  (test-assert "Invalid operator"
    (let-values (((valid? errors)
                  (validate-ast
                   '(infix-expression
                     ((operator . 42)  ; should be string
                      (left . (integer-literal ((value . 5))))
                      (right . (integer-literal ((value . 10)))))))))
      (and (not valid?)
           (any (lambda (err) (string-contains err "Type mismatch"))
                errors)))))

;;; ============================================================================
;;; File Validation Tests
;;; ============================================================================

(test-group "File Validation"
  
  (test-assert "Validate simple.sexp"
    (let ((file "examples/simple.sexp"))
      (if (file-exists? file)
          (let-values (((valid? errors) (validate-ast-file file)))
            valid?)
          (begin
            (format #t "Skipping: ~a not found~%" file)
            #t))))
  
  (test-assert "Validate function.sexp"
    (let ((file "examples/function.sexp"))
      (if (file-exists? file)
          (let-values (((valid? errors) (validate-ast-file file)))
            valid?)
          (begin
            (format #t "Skipping: ~a not found~%" file)
            #t))))
  
  (test-assert "Validate fibonacci.sexp"
    (let ((file "examples/fibonacci.sexp"))
      (if (file-exists? file)
          (let-values (((valid? errors) (validate-ast-file file)))
            valid?)
          (begin
            (format #t "Skipping: ~a not found~%" file)
            #t)))))

;;; ============================================================================
;;; Complex Validation Tests
;;; ============================================================================

(test-group "Complex Structures"
  
  (test-assert "Nested function calls"
    (let-values (((valid? errors)
                  (validate-ast
                   '(call-expression
                     ((function . (identifier ((value . "add"))))
                      (arguments . 
                       ((call-expression
                         ((function . (identifier ((value . "mul"))))
                          (arguments . 
                           ((integer-literal ((value . 2)))
                            (integer-literal ((value . 3))))))
                        (integer-literal ((value . 4))))))))))
      valid?))
  
  (test-assert "Array with mixed types"
    (let-values (((valid? errors)
                  (validate-ast
                   '(array-literal
                     ((elements . 
                       ((integer-literal ((value . 1)))
                        (string-literal ((value . "hello")))
                        (boolean-literal ((value . #t)))
                        (array-literal ((elements . ()))))))))))
      valid?))
  
  (test-assert "Hash literal"
    (let-values (((valid? errors)
                  (validate-ast
                   '(hash-literal
                     ((pairs . 
                       ((((key . (string-literal ((value . "name"))))
                          (value . (string-literal ((value . "Alice")))))
                         ((key . (string-literal ((value . "age"))))
                          (value . (integer-literal ((value . 30))))))))))))
      valid?)))

(test-end "AST Validator Tests")

;; Print summary
(let ((runner (test-runner-current)))
  (format #t "\n========================================\n")
  (format #t "AST Validator Test Results\n")
  (format #t "========================================\n")
  (format #t "Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "Total:  ~a\n" (+ (test-runner-pass-count runner)
                              (test-runner-fail-count runner)))
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))
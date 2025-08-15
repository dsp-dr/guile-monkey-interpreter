#!/usr/bin/env guile
!#
;;; Test cases for continuation interference

(define-syntax with-return
  (syntax-rules ()
    ((with-return body ...)
     (call/cc (lambda (return)
                body ...)))))

;;; Test 1: Basic nested with-return
(define (test-basic-nested)
  (format #t "\n=== Test 1: Basic Nested with-return ===\n")
  
  (letrec ((outer (lambda ()
                    (with-return
                      (format #t "Outer: start\n")
                      (let ((result (inner)))
                        (format #t "Outer: got result from inner: ~a\n" result)
                        (if result
                            (return 'outer-success)
                            (return 'outer-failure))))))
           (inner (lambda ()
                    (with-return
                      (format #t "Inner: start\n")
                      (return 'inner-result)))))
    (format #t "Result: ~a\n" (outer))))

;;; Test 2: Problem case - inner returns normally
(define (test-inner-normal-return)
  (format #t "\n=== Test 2: Inner Normal Return ===\n")
  
  (letrec ((outer (lambda ()
                    (with-return
                      (format #t "Outer: calling inner\n")
                      (let ((result (inner)))
                        (format #t "Outer: inner returned ~a\n" result)
                        (unless result
                          (format #t "Outer: about to return #f\n")
                          (return #f))
                        (format #t "Outer: returning success\n")
                        'success))))
           (inner (lambda ()
                    (with-return
                      (format #t "Inner: returning normally (not via return)\n")
                      #f))))  ; Returns #f without using return
    
    (catch #t
      (lambda ()
        (format #t "Result: ~a\n" (outer)))
      (lambda (key . args)
        (format #t "ERROR: ~a ~a\n" key args)))))

;;; Test 3: The actual parser pattern
(define (test-parser-pattern)
  (format #t "\n=== Test 3: Parser Pattern ===\n")
  
  (letrec ((parse-for-expression
            (lambda ()
              (with-return
                (format #t "parse-for: start\n")
                (let ((init (parse-let-statement)))
                  (format #t "parse-for: init = ~a\n" init)
                  (unless init
                    (format #t "parse-for: init failed, returning #f\n")
                    (return #f))
                  (format #t "parse-for: continuing with init\n")
                  `(for-expr ,init)))))
           (parse-let-statement
            (lambda ()
              (with-return
                (format #t "parse-let: checking condition\n")
                (if #f  ; Simulate failure condition
                    (begin
                      (format #t "parse-let: returning via return\n")
                      (return #f))
                    (begin
                      (format #t "parse-let: returning normally\n")
                      'let-stmt))))))
    
    (catch #t
      (lambda ()
        (format #t "Result: ~a\n" (parse-for-expression)))
      (lambda (key . args)
        (format #t "ERROR: ~a ~a\n" key args)))))

;;; Test 4: Using let* with nested with-return
(define (test-let-star-pattern)
  (format #t "\n=== Test 4: let* Pattern ===\n")
  
  (letrec ((helper (lambda ()
                     (with-return
                       (format #t "Helper: returning value\n")
                       'helper-value)))
           (outer (lambda ()
                    (with-return
                      (let* ((a (begin
                                  (format #t "Binding a\n")
                                  1))
                             (b (begin
                                  (format #t "Binding b, calling helper\n")
                                  (helper)))
                             (c (begin
                                  (format #t "Binding c\n")
                                  3)))
                        (format #t "In let* body, about to use return\n")
                        (return (list a b c)))))))
    
    (catch #t
      (lambda ()
        (format #t "Result: ~a\n" (outer)))
      (lambda (key . args)
        (format #t "ERROR: ~a ~a\n" key args)))))

;;; Test 5: Solution - Remove inner with-return
(define (test-solution-no-inner-return)
  (format #t "\n=== Test 5: Solution - No Inner with-return ===\n")
  
  (letrec ((parse-for-expression-fixed
            (lambda ()
              (with-return
                (format #t "parse-for: start\n")
                (let ((init (parse-let-statement-fixed)))
                  (format #t "parse-for: init = ~a\n" init)
                  (unless init
                    (format #t "parse-for: init failed, returning #f\n")
                    (return #f))
                  (format #t "parse-for: continuing with init\n")
                  `(for-expr ,init)))))
           (parse-let-statement-fixed
            (lambda ()
              ;; No with-return here!
              (format #t "parse-let: checking condition\n")
              (if #f  ; Simulate failure condition
                  (begin
                    (format #t "parse-let: returning #f\n")
                    #f)
                  (begin
                    (format #t "parse-let: returning let-stmt\n")
                    'let-stmt)))))
    
    (catch #t
      (lambda ()
        (format #t "Result: ~a\n" (parse-for-expression-fixed)))
      (lambda (key . args)
        (format #t "ERROR: ~a ~a\n" key args)))))

;;; Run all tests
(test-basic-nested)
(test-inner-normal-return)
(test-parser-pattern)
(test-let-star-pattern)
(test-solution-no-inner-return)

(format #t "\n=== All Tests Complete ===\n")
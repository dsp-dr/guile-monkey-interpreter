#!/usr/bin/env guile
!#
;;; Testing different macro implementations

(use-modules (ice-9 format))

;;; Original macro - has issues with compile-time binding
(define-syntax with-return-old
  (syntax-rules ()
    ((with-return-old body ...)
     (call/cc (lambda (return)
                body ...)))))

;;; Fixed macro - using let to establish binding
(define-syntax with-return
  (syntax-rules ()
    ((with-return body ...)
     (call/cc (lambda (k)
                (let ((return k))
                  body ...))))))

;;; Test the fixed macro
(format #t "Test 1: Single with-return\n")
(define result1
  (with-return
    (format #t "  About to return\n")
    (return 42)
    (format #t "  This won't print\n")
    99))
(format #t "  Result: ~a\n\n" result1)

;;; Test nested case
(format #t "Test 2: Nested with-return\n")
(define (inner)
  (with-return
    (format #t "  Inner: returning 10\n")
    (return 10)))

(define (outer)
  (with-return
    (format #t "  Outer: calling inner\n")
    (let ((val (inner)))
      (format #t "  Outer: got ~a from inner\n" val)
      (return (+ val 5)))))

(format #t "  Result: ~a\n\n" (outer))

;;; Test the problematic pattern
(format #t "Test 3: The problem pattern\n")

(define (parse-let)
  (with-return
    (format #t "  parse-let: returning #f\n")
    (return #f)))

(define (parse-for)
  (with-return
    (format #t "  parse-for: starting\n")
    (let ((init (parse-let)))
      (format #t "  parse-for: got init = ~a\n" init)
      (unless init
        (format #t "  parse-for: about to return 'failed\n")
        (return 'failed))
      'success)))

(catch #t
  (lambda ()
    (format #t "  Result: ~a\n" (parse-for)))
  (lambda (key . args)
    (format #t "  ERROR: ~a ~a\n" key args)))

(format #t "\nAll tests completed!\n")
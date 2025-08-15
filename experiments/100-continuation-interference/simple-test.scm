#!/usr/bin/env guile
!#
;;; Simplified test to understand the issue

(use-modules (ice-9 format))

(define-syntax with-return
  (syntax-rules ()
    ((with-return body ...)
     (call/cc (lambda (return)
                body ...)))))

;;; This works - single with-return
(format #t "Test 1: Single with-return\n")
(define result1
  (with-return
    (format #t "  About to return\n")
    (return 42)
    (format #t "  This won't print\n")
    99))
(format #t "  Result: ~a\n\n" result1)

;;; This also works - nested but separate
(format #t "Test 2: Nested but separate\n")
(define (inner2)
  (with-return
    (format #t "  Inner: returning 10\n")
    (return 10)))

(define result2
  (with-return
    (format #t "  Outer: calling inner\n")
    (let ((val (inner2)))
      (format #t "  Outer: got ~a\n" val)
      (return (+ val 5)))))
(format #t "  Result: ~a\n\n" result2)

;;; This is the problem pattern - calling a with-return function from within a let binding
(format #t "Test 3: The problem pattern\n")

(define (parse-let-stmt)
  (with-return
    (format #t "  parse-let: checking...\n")
    ;; Simulate returning #f
    (return #f)))

(define (parse-for-expr)
  (with-return
    (format #t "  parse-for: starting\n")
    (let ((init (parse-let-stmt)))
      (format #t "  parse-for: got init = ~a\n" init)
      (unless init
        (format #t "  parse-for: about to return\n")
        (return 'failed))  ; <-- This is where it fails!
      'success)))

(catch #t
  (lambda ()
    (format #t "  Result: ~a\n" (parse-for-expr)))
  (lambda (key . args)
    (format #t "  ERROR: ~a ~a\n" key args)))

(format #t "\nTest 4: Macro expansion check\n")
;; Let's see what the macro expands to
(format #t "  with-return expands to: call/cc with lambda (return) ...\n")
(format #t "  Each with-return creates its own 'return' binding\n")
(format #t "  The problem: 'return' in parse-for-expr refers to its own binding,\n")
(format #t "  but after parse-let-stmt returns, we're back in parse-for-expr's scope\n")
(format #t "  and 'return' should still be bound... but it's not!\n")
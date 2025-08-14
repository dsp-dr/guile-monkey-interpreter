#!/usr/bin/env guile
!#
;;; Simple test to check if libraries load

(add-to-load-path (dirname (current-filename)))
(load "ffi-bridge.scm")

;; Import all functions from the FFI module
(use-modules ((monkey extensions ffi) 
              #:select (read-file write-file file-exists? delete-file)))

;; Try a simple test
(display "Testing FFI functions...\n")

;; Test write
(display "Writing file... ")
(if (write-file "test.txt" "Hello FFI!")
    (display "OK\n")
    (display "FAILED\n"))

;; Test read
(display "Reading file... ")
(let ((content (read-file "test.txt")))
  (if (equal? content "Hello FFI!")
      (display "OK\n")
      (display "FAILED\n")))

;; Cleanup
(delete-file "test.txt")
(display "Cleanup done.\n")
#!/usr/bin/env guile
!#
;;; Test FFI Bridge

(add-to-load-path (dirname (current-filename)))
(load "ffi-bridge.scm")
(use-modules (srfi srfi-64)
             ((monkey extensions ffi) 
              #:select (read-file write-file file-exists? 
                       file-size is-directory? get-cwd
                       http-status http-get))
             (ice-9 receive))

;; Rename delete-file to avoid conflict with core
(define ffi-delete-file (@@ (monkey extensions ffi) delete-file))

(test-begin "ffi-bridge")

;;; Filesystem tests
(test-group "filesystem"
  ;; Test write and read
  (test-assert "write-file"
    (write-file "test-ffi.txt" "Hello from Scheme FFI!"))
  
  (test-equal "read-file"
    "Hello from Scheme FFI!"
    (read-file "test-ffi.txt"))
  
  (test-assert "file-exists?"
    (file-exists? "test-ffi.txt"))
  
  (test-equal "file-size"
    22
    (file-size "test-ffi.txt"))
  
  (test-assert "not is-directory?"
    (not (is-directory? "test-ffi.txt")))
  
  (test-assert "is-directory? ."
    (is-directory? "."))
  
  (test-assert "get-cwd"
    (string? (get-cwd)))
  
  (test-assert "delete-file"
    (ffi-delete-file "test-ffi.txt"))
  
  (test-assert "file deleted"
    (not (file-exists? "test-ffi.txt"))))

;;; HTTP tests (require network)
(test-group "http"
  (test-skip "http-get")  ; Skip if no network
  (test-skip "http-status")
  
  (test-equal "http-status 200"
    200
    (http-status "http://httpbin.org/status/200"))
  
  (test-assert "http-get"
    (let ((response (http-get "http://httpbin.org/get")))
      (and response (string? response) (> (string-length response) 0)))))

(test-end "ffi-bridge")

;; Display results
(define runner (test-runner-current))
(format #t "\n")
(format #t "FFI Bridge Test Results\n")
(format #t "=======================\n")
(format #t "Passed: ~a\n" (test-runner-pass-count runner))
(format #t "Failed: ~a\n" (test-runner-fail-count runner))
(format #t "Skipped: ~a\n" (test-runner-skip-count runner))
(format #t "=======================\n")

(exit (if (zero? (test-runner-fail-count runner)) 0 1))
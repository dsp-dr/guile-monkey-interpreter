#!/usr/bin/env guile
!#
;;; Tool to help balance parentheses in a Scheme file

(use-modules (ice-9 rdelim)
             (ice-9 format))

(define (analyze-parens filename)
  "Analyze parenthesis balance in a file"
  (with-input-from-file filename
    (lambda ()
      (let loop ((line-num 1)
                 (char-num 0) 
                 (depth 0)
                 (stack '()))
        (let ((c (read-char)))
          (cond
           ((eof-object? c)
            (format #t "~%Final depth: ~a~%" depth)
            (unless (zero? depth)
              (format #t "ERROR: Unbalanced parentheses!~%")
              (format #t "Unclosed parentheses at positions:~%")
              (for-each (lambda (pos)
                         (format #t "  Line ~a, char ~a~%" (car pos) (cdr pos)))
                       stack)))
           
           ((char=? c #\newline)
            (loop (+ line-num 1) 0 depth stack))
           
           ((char=? c #\()
            (loop line-num (+ char-num 1) (+ depth 1)
                  (cons (cons line-num char-num) stack)))
           
           ((char=? c #\))
            (if (null? stack)
                (begin
                  (format #t "ERROR: Extra closing paren at line ~a, char ~a~%" 
                          line-num char-num)
                  (loop line-num (+ char-num 1) depth stack))
                (loop line-num (+ char-num 1) (- depth 1) (cdr stack))))
           
           (else
            (loop line-num (+ char-num 1) depth stack))))))))

(define (find-function-boundaries filename function-name)
  "Find the start and end of a function definition"
  (with-input-from-file filename
    (lambda ()
      (let loop ((line-num 1)
                 (in-function #f)
                 (depth 0)
                 (func-start #f))
        (let ((line (read-line)))
          (cond
           ((eof-object? line)
            (when func-start
              (format #t "Function ~a starts at line ~a but doesn't end properly~%"
                      function-name func-start)))
           
           ((and (not in-function)
                 (string-contains line (format #f "(define (~a " function-name)))
            (format #t "Found ~a at line ~a~%" function-name line-num)
            (loop (+ line-num 1) #t 0 line-num))
           
           (in-function
            (let ((open-count (string-count line #\())
                  (close-count (string-count line #\))))
              (let ((new-depth (+ depth (- open-count close-count))))
                (when (and (= depth 0) (< new-depth 0))
                  (format #t "Function ~a ends at line ~a~%" function-name line-num)
                  (loop (+ line-num 1) #f 0 #f))
                (loop (+ line-num 1) in-function new-depth func-start))))
           
           (else
            (loop (+ line-num 1) in-function depth func-start))))))))

(define (string-count str char)
  "Count occurrences of char in string"
  (let loop ((i 0) (count 0))
    (if (>= i (string-length str))
        count
        (loop (+ i 1) 
              (if (char=? (string-ref str i) char)
                  (+ count 1)
                  count)))))

;; Main
(format #t "=== Analyzing parser.scm ===~%~%")
(analyze-parens "monkey/parser/parser.scm")

(format #t "~%=== Finding parse-hash-literal boundaries ===~%")
(find-function-boundaries "monkey/parser/parser.scm" "parse-hash-literal")

(format #t "~%=== Finding parse-expression-list boundaries ===~%")
(find-function-boundaries "monkey/parser/parser.scm" "parse-expression-list")
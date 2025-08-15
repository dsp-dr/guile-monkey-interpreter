#!/usr/bin/env guile
!#
;;; Final fix for parser.scm structure

(use-modules (ice-9 rdelim)
             (ice-9 format))

;; Check where parse-hash-literal actually ends
(define (find-function-end filename func-name)
  "Find where a function actually ends by tracking parentheses"
  (with-input-from-file filename
    (lambda ()
      (let loop ((line-num 1)
                 (in-func #f)
                 (depth 0)
                 (found-start #f))
        (let ((line (read-line)))
          (cond
           ((eof-object? line)
            (format #t "EOF reached. Final depth: ~a~%" depth))
           
           ((and (not in-func)
                 (string-contains line (format #f "(define (~a " func-name)))
            (format #t "~a starts at line ~a~%" func-name line-num)
            (let ((depth-change (count-parens line)))
              (loop (+ line-num 1) #t depth-change #t)))
           
           (in-func
            (let ((depth-new (+ depth (count-parens line))))
              (format #t "Line ~3d [depth ~2d->~2d]: ~a~%"
                      line-num depth depth-new
                      (if (> (string-length line) 60)
                          (string-append (substring line 0 60) "...")
                          line))
              (if (<= depth-new 0)
                  (begin
                    (format #t "~a ENDS at line ~a~%~%" func-name line-num)
                    (loop (+ line-num 1) #f 0 #f))
                  (loop (+ line-num 1) #t depth-new found-start))))
           
           (else
            (loop (+ line-num 1) #f 0 #f))))))))

(define (count-parens line)
  "Count net parentheses in a line"
  (let ((open 0) (close 0))
    (string-for-each
     (lambda (c)
       (cond
        ((char=? c #\() (set! open (+ open 1)))
        ((char=? c #\)) (set! close (+ close 1)))))
     line)
    (- open close)))

(format #t "=== Analyzing parse-hash-literal ===~%")
(find-function-end "monkey/parser/parser.scm" "parse-hash-literal")

(format #t "~%=== Analyzing parse-expression-list ===~%")
(find-function-end "monkey/parser/parser.scm" "parse-expression-list")

(format #t "~%=== Analyzing parse-for-expression ===~%")
(find-function-end "monkey/parser/parser.scm" "parse-for-expression")
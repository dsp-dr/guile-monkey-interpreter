#!/usr/bin/env guile
!#
;;; Fix parser structure by analyzing depth at each line

(use-modules (ice-9 rdelim)
             (ice-9 format))

(define (analyze-depth filename start-line end-line)
  "Show depth changes for each line"
  (with-input-from-file filename
    (lambda ()
      (let loop ((line-num 1) (depth 0))
        (let ((line (read-line)))
          (unless (eof-object? line)
            (when (and (>= line-num start-line) (<= line-num end-line))
              (let ((open-count 0)
                    (close-count 0))
                ;; Count parens in line
                (string-for-each
                 (lambda (c)
                   (cond
                    ((char=? c #\() (set! open-count (+ open-count 1)))
                    ((char=? c #\)) (set! close-count (+ close-count 1)))))
                 line)
                
                (let ((new-depth (+ depth (- open-count close-count))))
                  (format #t "~3d [~2d->~2d] (~2d,~2d): ~a~%"
                          line-num depth new-depth open-count close-count
                          (if (> (string-length line) 60)
                              (string-append (substring line 0 60) "...")
                              line))
                  (loop (+ line-num 1) new-depth))))
            (unless (>= line-num end-line)
              (let ((open-count 0)
                    (close-count 0))
                (string-for-each
                 (lambda (c)
                   (cond
                    ((char=? c #\() (set! open-count (+ open-count 1)))
                    ((char=? c #\)) (set! close-count (+ close-count 1)))))
                 line)
                (loop (+ line-num 1) (+ depth (- open-count close-count)))))))))))

(format #t "=== Analyzing parse-hash-literal area (lines 428-475) ===~%")
(format #t "Line [Depth->New] (Open,Close): Content~%")
(format #t "----------------------------------------~%")
(analyze-depth "monkey/parser/parser.scm" 428 475)
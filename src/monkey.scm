#!/usr/bin/env guile
!#
;;; Monkey Language Interpreter
;;; Complete implementation with all features

(add-to-load-path (or (and (current-filename)
                           (dirname (current-filename)))
                      (getcwd)))

;; Import all modules
(use-modules (monkey main))

;; Start the interpreter
(main (command-line))
#!/usr/bin/env guile
!#
;;; Chapter 04 - Extended Built-ins Test
;;; Test the additional built-in functions

(add-to-load-path (or (and (current-filename)
                           (dirname (current-filename)))
                      (getcwd)))
(add-to-load-path (string-append (or (and (current-filename)
                                          (dirname (current-filename)))
                                     (getcwd))
                                 "/monkey"))

(use-modules (srfi srfi-64)
             (monkey token token)
             (monkey lexer lexer)
             (monkey parser parser)
             (monkey ast ast)
             (monkey object object)
             (monkey object environment)
             (monkey evaluator evaluator)
             (ice-9 format))

(test-begin "Chapter 04 - Extended Built-ins")

;;; Test helper
(define (test-eval input)
  "Helper to evaluate input and return result"
  (let* ((lexer (make-lexer input))
         (parser (make-parser lexer))
         (program (parse-program parser))
         (env (make-environment)))
    (eval program env)))

;;; Type function tests
(test-group "type() function"
  (test-equal "type of integer" "INTEGER"
              (string-object-value (test-eval "type(5)")))
  (test-equal "type of string" "STRING"
              (string-object-value (test-eval "type(\"hello\")")))
  (test-equal "type of boolean" "BOOLEAN"
              (string-object-value (test-eval "type(true)")))
  (test-equal "type of array" "ARRAY"
              (string-object-value (test-eval "type([1, 2, 3])")))
  (test-equal "type of hash" "HASH"
              (string-object-value (test-eval "type({\"a\": 1})"))))

;;; String conversion tests
(test-group "str() function"
  (test-equal "str of integer" "42"
              (string-object-value (test-eval "str(42)")))
  (test-equal "str of boolean true" "true"
              (string-object-value (test-eval "str(true)")))
  (test-equal "str of boolean false" "false"
              (string-object-value (test-eval "str(false)")))
  (test-equal "str of string" "hello"
              (string-object-value (test-eval "str(\"hello\")"))))

;;; Integer conversion tests
(test-group "int() function"
  (test-equal "int of string" 42
              (integer-object-value (test-eval "int(\"42\")")))
  (test-equal "int of integer" 100
              (integer-object-value (test-eval "int(100)")))
  (test-assert "int of invalid string is error"
               (error-object? (test-eval "int(\"abc\")"))))

;;; String split tests
(test-group "split() function"
  (let ((result (test-eval "split(\"a,b,c\", \",\")")))
    (test-assert "split returns array" (array-object? result))
    (test-equal "split array length" 3
                (length (array-object-elements result)))
    (test-equal "split first element" "a"
                (string-object-value (car (array-object-elements result))))))

;;; Array join tests
(test-group "join() function"
  (test-equal "join array with comma" "a,b,c"
              (string-object-value 
               (test-eval "join([\"a\", \"b\", \"c\"], \",\")")))
  (test-equal "join with space" "hello world"
              (string-object-value
               (test-eval "join([\"hello\", \"world\"], \" \")"))))

;;; Contains tests
(test-group "contains() function"
  (test-equal "string contains substring" #t
              (boolean-object-value (test-eval "contains(\"hello world\", \"world\")")))
  (test-equal "string doesn't contain" #f
              (boolean-object-value (test-eval "contains(\"hello\", \"world\")")))
  (test-equal "array contains element" #t
              (boolean-object-value (test-eval "contains([1, 2, 3], 2)")))
  (test-equal "array doesn't contain" #f
              (boolean-object-value (test-eval "contains([1, 2, 3], 4)"))))

;;; Hash keys/values tests
(test-group "keys() and values() functions"
  (let ((keys-result (test-eval "keys({\"a\": 1, \"b\": 2})"))
        (vals-result (test-eval "values({\"a\": 1, \"b\": 2})")))
    (test-assert "keys returns array" (array-object? keys-result))
    (test-assert "values returns array" (array-object? vals-result))
    (test-equal "keys count" 2 (length (array-object-elements keys-result)))
    (test-equal "values count" 2 (length (array-object-elements vals-result)))))

;;; Delete from hash tests
(test-group "delete() function"
  (let ((result (test-eval "delete({\"a\": 1, \"b\": 2}, \"a\")")))
    (test-assert "delete returns hash" (hash-object? result))
    (test-equal "hash size after delete" 1
                (length (hash-object-pairs result)))))

;;; Integration test
(test-group "Chapter 4 integration"
  (let ((program "
let data = \"name:John,age:30,city:NYC\";
let parts = split(data, \",\");
let result = [];
let i = 0;
while (i < len(parts)) {
  let pair = split(parts[i], \":\");
  let result = push(result, pair);
  let i = i + 1;
}
result
"))
    (let ((result (test-eval program)))
      (test-assert "complex program works" (array-object? result))
      (test-equal "result length" 3 (length (array-object-elements result))))))

(test-end "Chapter 04 - Extended Built-ins")

;; Print summary
(let ((runner (test-runner-current)))
  (format #t "\n========================================\n")
  (format #t "Chapter 04 Test Results\n")
  (format #t "========================================\n")
  (format #t "Passed: ~a\n" (test-runner-pass-count runner))
  (format #t "Failed: ~a\n" (test-runner-fail-count runner))
  (format #t "Total:  ~a\n" (+ (test-runner-pass-count runner)
                              (test-runner-fail-count runner)))
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))
;; Fibonacci function
;; let fib = fn(n) {
;;   if (n < 2) {
;;     n
;;   } else {
;;     fib(n - 1) + fib(n - 2)
;;   }
;; };

(program
  ((statements . 
    ((let-statement
      ((name . (identifier ((value . "fib"))))
       (value . 
        (function-literal
         ((parameters . ((identifier ((value . "n")))))
          (body . 
           (block-statement
            ((statements . 
              ((expression-statement
                ((expression . 
                  (if-expression
                   ((condition . 
                     (infix-expression
                      ((operator . "<")
                       (left . (identifier ((value . "n"))))
                       (right . (integer-literal ((value . 2)))))))
                    (consequence . 
                     (block-statement
                      ((statements . 
                        ((expression-statement
                          ((expression . (identifier ((value . "n")))))))))))
                    (alternative . 
                     (block-statement
                      ((statements . 
                        ((expression-statement
                          ((expression . 
                            (infix-expression
                             ((operator . "+")
                              (left . 
                               (call-expression
                                ((function . (identifier ((value . "fib"))))
                                 (arguments . 
                                  ((infix-expression
                                    ((operator . "-")
                                     (left . (identifier ((value . "n"))))
                                     (right . (integer-literal ((value . 1)))))))))))
                              (right . 
                               (call-expression
                                ((function . (identifier ((value . "fib"))))
                                 (arguments . 
                                  ((infix-expression
                                    ((operator . "-")
                                     (left . (identifier ((value . "n"))))
                                     (right . (integer-literal ((value . 2)))))))))))))))))))))))))))))))))))))
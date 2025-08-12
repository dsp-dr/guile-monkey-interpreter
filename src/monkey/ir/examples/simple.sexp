;; Simple arithmetic expression: 5 + 10
(program
  ((statements . 
    ((expression-statement
      ((expression . 
        (infix-expression
         ((operator . "+")
          (left . (integer-literal ((value . 5))))
          (right . (integer-literal ((value . 10)))))))))))))
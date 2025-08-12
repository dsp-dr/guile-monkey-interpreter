;; Function definition and call
;; let add = fn(x, y) { x + y };
;; add(5, 10);

(program
  ((statements . 
    ((let-statement
      ((name . (identifier ((value . "add"))))
       (value . 
        (function-literal
         ((parameters . 
           ((identifier ((value . "x")))
            (identifier ((value . "y")))))
          (body . 
           (block-statement
            ((statements . 
              ((expression-statement
                ((expression . 
                  (infix-expression
                   ((operator . "+")
                    (left . (identifier ((value . "x"))))
                    (right . (identifier ((value . "y")))))))))))))))))
     
     (expression-statement
      ((expression . 
        (call-expression
         ((function . (identifier ((value . "add"))))
          (arguments . 
           ((integer-literal ((value . 5)))
            (integer-literal ((value . 10)))))))))))))))
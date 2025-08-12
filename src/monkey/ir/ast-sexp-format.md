# Monkey AST S-Expression Format Specification

## Overview

This document specifies the canonical S-expression format for representing Monkey language Abstract Syntax Trees (AST).

## Format Convention

Each AST node is represented as:
```scheme
(node-type attributes)
```

Where `attributes` is an association list of field names to values.

## Node Types

### Program (Root)
```scheme
(program
  ((statements . (list-of-statements))))
```

### Statements

#### Let Statement
```scheme
(let-statement
  ((name . (identifier ((value . "variable-name"))))
   (value . expression)))
```

#### Return Statement
```scheme
(return-statement
  ((value . expression)))
```

#### Expression Statement
```scheme
(expression-statement
  ((expression . expression)))
```

#### Block Statement
```scheme
(block-statement
  ((statements . (list-of-statements))))
```

#### While Statement
```scheme
(while-statement
  ((condition . expression)
   (body . block-statement)))
```

### Expressions

#### Literals

##### Integer Literal
```scheme
(integer-literal ((value . 42)))
```

##### Boolean Literal
```scheme
(boolean-literal ((value . #t)))  ; or #f
```

##### String Literal
```scheme
(string-literal ((value . "hello world")))
```

##### Identifier
```scheme
(identifier ((value . "variable_name")))
```

#### Operators

##### Prefix Expression
```scheme
(prefix-expression
  ((operator . "!")
   (right . expression)))
```

##### Infix Expression
```scheme
(infix-expression
  ((operator . "+")
   (left . expression)
   (right . expression)))
```

#### Control Flow

##### If Expression
```scheme
(if-expression
  ((condition . expression)
   (consequence . block-statement)
   (alternative . block-statement)))  ; optional
```

#### Functions

##### Function Literal
```scheme
(function-literal
  ((parameters . ((identifier ((value . "x")))
                  (identifier ((value . "y")))))
   (body . block-statement)
   (name . "optional-name")))  ; optional for named functions
```

##### Call Expression
```scheme
(call-expression
  ((function . expression)
   (arguments . (expression1 expression2 ...))))
```

#### Data Structures

##### Array Literal
```scheme
(array-literal
  ((elements . (expression1 expression2 ...))))
```

##### Hash Literal
```scheme
(hash-literal
  ((pairs . (((key . expression1) (value . expression2))
             ((key . expression3) (value . expression4))))))
```

##### Index Expression
```scheme
(index-expression
  ((left . expression)
   (index . expression)))
```

## Complete Examples

### Example 1: Simple Assignment
Monkey code:
```monkey
let x = 5 + 10;
```

S-expression:
```scheme
(program
  ((statements . 
    ((let-statement
      ((name . (identifier ((value . "x"))))
       (value . (infix-expression
                 ((operator . "+")
                  (left . (integer-literal ((value . 5))))
                  (right . (integer-literal ((value . 10)))))))))))))
```

### Example 2: Function Definition and Call
Monkey code:
```monkey
let add = fn(a, b) { a + b };
add(5, 10);
```

S-expression:
```scheme
(program
  ((statements . 
    ((let-statement
      ((name . (identifier ((value . "add"))))
       (value . (function-literal
                 ((parameters . ((identifier ((value . "a")))
                                (identifier ((value . "b")))))
                  (body . (block-statement
                           ((statements . 
                             ((expression-statement
                               ((expression . 
                                 (infix-expression
                                  ((operator . "+")
                                   (left . (identifier ((value . "a"))))
                                   (right . (identifier ((value . "b")))))))))))))))))
     (expression-statement
      ((expression . 
        (call-expression
         ((function . (identifier ((value . "add"))))
          (arguments . ((integer-literal ((value . 5)))
                       (integer-literal ((value . 10))))))))))))))
```

### Example 3: Conditional
Monkey code:
```monkey
if (x > 5) { 
  return true; 
} else { 
  return false; 
}
```

S-expression:
```scheme
(if-expression
  ((condition . (infix-expression
                 ((operator . ">")
                  (left . (identifier ((value . "x"))))
                  (right . (integer-literal ((value . 5)))))))
   (consequence . (block-statement
                   ((statements . 
                     ((return-statement
                       ((value . (boolean-literal ((value . #t)))))))))))
   (alternative . (block-statement
                   ((statements . 
                     ((return-statement
                       ((value . (boolean-literal ((value . #f)))))))))))))
```

### Example 4: Array and Hash
Monkey code:
```monkey
let arr = [1, 2, 3];
let hash = {"key": "value"};
```

S-expression:
```scheme
(program
  ((statements . 
    ((let-statement
      ((name . (identifier ((value . "arr"))))
       (value . (array-literal
                 ((elements . ((integer-literal ((value . 1)))
                              (integer-literal ((value . 2)))
                              (integer-literal ((value . 3))))))))))
     (let-statement
      ((name . (identifier ((value . "hash"))))
       (value . (hash-literal
                 ((pairs . ((((key . (string-literal ((value . "key"))))
                             (value . (string-literal ((value . "value"))))))))))))))))
```

## Validation Rules

1. **Node Structure**: Every node must be a list with the node type as the first element
2. **Required Fields**: All required fields must be present in the attributes
3. **Type Checking**: Field values must match their expected types
4. **Child Nodes**: Child nodes must be valid AST nodes themselves
5. **Operator Values**: Operators must be valid Monkey operators
6. **Identifier Names**: Identifiers must follow Monkey naming rules

## Type Definitions

- `string`: Scheme string
- `integer`: Scheme exact integer
- `boolean`: Scheme boolean (#t or #f)
- `identifier`: An identifier node
- `expression`: Any valid expression node
- `block-statement`: A block statement node
- `list`: A Scheme list of nodes

## Usage with Validator

```scheme
(use-modules (monkey ir ast-spec))

;; Validate an S-expression
(let-values (((valid? errors) (validate-ast my-sexp)))
  (if valid?
      (display "Valid AST!")
      (format #t "Validation errors: ~a~%" errors)))
```
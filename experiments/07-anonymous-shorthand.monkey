// Experiment: Anonymous Function Shorthand
// Goal: Add |x| x + 1 syntax for lambdas

// Current: Verbose fn syntax
puts("=== Current: fn syntax ===");
let numbers = [1, 2, 3, 4, 5];
let double = fn(x) { x * 2 };
let add = fn(x, y) { x + y };

// Map implementation for testing
let map = fn(arr, f) {
    let result = [];
    let i = 0;
    while (i < len(arr)) {
        result = push(result, f(arr[i]));
        i = i + 1;
    }
    result
};

puts(str(map(numbers, double)));

// Proposed shorthand syntax:
// let double = |x| x * 2;
// let add = |x, y| x + y;
// map(numbers, |x| x * 2)

// Even shorter for single parameter:
// map(numbers, \x -> x * 2)  // Haskell-style
// map(numbers, x => x * 2)   // JavaScript-style
// map(numbers, ^(* 2 %))      // Clojure-style

// Implementation approach:
// 1. Lexer: Recognize | or => tokens
// 2. Parser: Parse |params| expr as sugar for fn
// 3. Transform at parse time:
//    |x| x + 1  -->  fn(x) { x + 1 }
//    |x, y| x + y  -->  fn(x, y) { x + y }

// Implicit return for single expressions
// No braces needed for single expression body

// Could also support placeholder syntax:
// map(numbers, _ * 2)  // _ is implicit parameter

puts("");
puts("Level of Effort: LOW-MEDIUM");
puts("- Lexer: 1 hour (pipe or arrow tokens)");
puts("- Parser: 3-4 hours (shorthand syntax)");
puts("- AST: 0 hours (reuse FunctionLiteral)");
puts("- Testing: 2 hours");
puts("Total: ~6 hours");
puts("Note: Pure syntactic sugar, easy win for ergonomics");
// MAL (Make-a-Lisp) Step 0: REPL
// A simple Read-Eval-Print-Loop implementation in Monkey

let READ = fn(input) {
    // For now, just return the input as-is
    input
};

let EVAL = fn(ast) {
    // For now, just return the AST unchanged
    ast
};

let PRINT = fn(exp) {
    // Convert to string for printing
    str(exp)
};

let rep = fn(input) {
    // Read-Eval-Print pipeline
    PRINT(EVAL(READ(input)))
};

// Main REPL loop simulation
let inputs = [
    "hello world",
    "(+ 1 2)",
    "nil",
    "123",
    "\"a string\""
];

puts("MAL Step 0 - Basic REPL");
puts("========================");

let i = 0;
while (i < len(inputs)) {
    let input = inputs[i];
    puts("user> " + input);
    puts(rep(input));
    i = i + 1;
}
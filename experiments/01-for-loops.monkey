// Experiment: For Loops with C-style syntax
// Goal: Explore for(init; condition; increment) syntax
// Current: We only have while loops

// Current way to do a for loop with while
puts("=== Current: While loop simulation ===");
let i = 0;
while (i < 5) {
    puts("i = " + str(i));
    i = i + 1;
}

// Proposed for loop syntax
// for (let j = 0; j < 5; j = j + 1) {
//     puts("j = " + str(j));
// }

// Implementation approach:
// 1. Lexer: Add FOR keyword token
// 2. Parser: Parse for(init; cond; inc) { body }
//    - init: parse-let-statement or parse-expression-statement
//    - cond: parse-expression
//    - inc: parse-expression
// 3. AST: Create ForStatement node with init, condition, increment, body
// 4. Evaluator: 
//    - Eval init once
//    - While condition is true:
//      - Eval body
//      - Eval increment

// Desugaring approach (simpler):
// Transform: for(init; cond; inc) { body }
// Into: { init; while(cond) { body; inc } }

// Example of nested for loops (proposed)
// for (let i = 0; i < 3; i = i + 1) {
//     for (let j = 0; j < 3; j = j + 1) {
//         puts("(" + str(i) + "," + str(j) + ")");
//     }
// }

puts("");
puts("Level of Effort: MEDIUM");
puts("- Lexer: 1 hour (add FOR token)");
puts("- Parser: 3-4 hours (parse complex syntax)");
puts("- AST: 1 hour (new node type)");
puts("- Evaluator: 2-3 hours (implement semantics)");
puts("Total: ~8 hours");
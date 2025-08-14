// Experiment: Break and Continue statements
// Goal: Add loop control flow

// Current: No way to exit early or skip iterations
puts("=== Current: No break/continue ===");
let i = 0;
while (i < 10) {
    if (i == 5) {
        // Want to break here, but can't
        i = 100; // Hack to exit
    } else {
        puts("i = " + str(i));
        i = i + 1;
    }
}

// Proposed break/continue
// let j = 0;
// while (j < 10) {
//     j = j + 1;
//     if (j == 3) { continue; }  // Skip 3
//     if (j == 7) { break; }      // Stop at 7
//     puts("j = " + str(j));
// }

// Implementation challenges:
// 1. Need to track loop context in evaluator
// 2. Break/continue need special handling (like return)
// 3. Must work with nested loops

// Approach using exceptions/conditions:
// - BreakException and ContinueException objects
// - Catch in loop evaluation
// - Similar to how 'return' works with call/cc

puts("");
puts("Level of Effort: MEDIUM-HIGH");
puts("- Lexer: 30 min (BREAK, CONTINUE tokens)");
puts("- Parser: 1 hour (simple statements)");
puts("- Evaluator: 4-5 hours (control flow complexity)");
puts("- Testing: 2 hours (edge cases, nested loops)");
puts("Total: ~8 hours");
puts("Depends on: Good exception/continuation support");
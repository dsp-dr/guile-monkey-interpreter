// Experiment: Tail Call Optimization
// Goal: Optimize recursive functions to avoid stack overflow

// Current: Recursive calls consume stack
puts("=== Current: No TCO ===");
let factorial = fn(n, acc) {
    if (n <= 1) {
        acc
    } else {
        factorial(n - 1, n * acc)  // Tail call
    }
};
puts("factorial(5) = " + str(factorial(5, 1)));

// This would overflow with large numbers:
// factorial(10000, 1)  // Stack overflow!

// TCO would convert tail recursion to iteration
// The recursive call is the last operation (tail position)

// Example: Fibonacci with accumulator
let fib = fn(n, a, b) {
    if (n == 0) {
        a
    } else {
        fib(n - 1, b, a + b)  // Tail call
    }
};
puts("fib(10) = " + str(fib(10, 0, 1)));

// Non-tail recursive (cannot optimize):
let badFib = fn(n) {
    if (n <= 1) {
        n
    } else {
        badFib(n-1) + badFib(n-2)  // Not in tail position
    }
};

// Implementation approach:
// 1. Identify tail calls during evaluation
// 2. Instead of creating new stack frame, reuse current
// 3. Transform to loop internally:
//    while (true) {
//        if (base case) return result;
//        update parameters;
//    }

// In Scheme: Already has TCO built-in
// Can leverage Scheme's TCO for Monkey

puts("");
puts("Level of Effort: MEDIUM-HIGH");
puts("- Tail call detection: 3-4 hours");
puts("- Evaluator modification: 6-8 hours");
puts("- Testing: 3 hours (ensure correctness)");
puts("Total: ~13 hours");
puts("Alternative: Leverage Scheme's TCO (4 hours)");
puts("Note: Critical for functional programming style");
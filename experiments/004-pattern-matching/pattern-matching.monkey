// Experiment: Pattern Matching
// Goal: Add Scheme-like match expressions

// Current: Manual if/else checking
puts("=== Current: Manual pattern checking ===");
let checkValue = fn(val) {
    if (type(val) == "INTEGER") {
        if (val == 0) {
            "zero"
        } else if (val > 0) {
            "positive"
        } else {
            "negative"
        }
    } else if (type(val) == "STRING") {
        "string: " + val
    } else if (type(val) == "ARRAY") {
        "array of length " + str(len(val))
    } else {
        "unknown"
    }
};

puts(checkValue(42));
puts(checkValue("hello"));
puts(checkValue([1, 2, 3]));

// Proposed pattern matching syntax
// match (val) {
//     0 -> "zero"
//     n if n > 0 -> "positive"
//     n if n < 0 -> "negative"
//     "hello" -> "greeting"
//     [x, y, z] -> "triple: " + str(x)
//     [head, ...tail] -> "list with head: " + str(head)
//     {name: n, age: a} -> n + " is " + str(a)
//     _ -> "default"
// }

// Implementation approach:
// 1. Lexer: MATCH, ARROW (->) tokens
// 2. Parser: Parse match expression with patterns
// 3. Pattern compiler: Transform patterns to predicates
// 4. Evaluator: Test each pattern, bind variables

// Pattern types to support:
// - Literal patterns: 42, "hello", true
// - Variable patterns: x (binds x)
// - Array patterns: [a, b, c], [head, ...rest]
// - Hash patterns: {key: pattern}
// - Guard patterns: n if n > 0
// - Wildcard: _

puts("");
puts("Level of Effort: HIGH");
puts("- Lexer: 1 hour (new tokens)");
puts("- Parser: 6-8 hours (complex pattern syntax)");
puts("- Pattern compiler: 8-10 hours (pattern matching logic)");
puts("- Evaluator: 4-5 hours (variable binding, guards)");
puts("- Testing: 4 hours (many edge cases)");
puts("Total: ~25 hours");
puts("Note: This is a major feature requiring significant design");
// Experiment: String Interpolation
// Goal: Support "Hello, ${name}!" syntax

// Current way
puts("=== Current: String concatenation ===");
let name = "Alice";
let age = 30;
puts("Hello, " + name + "! You are " + str(age) + " years old.");

// Proposed interpolation syntax
// puts("Hello, ${name}! You are ${age} years old.");
// puts("Math: 2 + 2 = ${2 + 2}");
// puts("Nested: ${fn(x) { x * 2 }(21)}");

// Implementation approach:
// 1. Lexer: Recognize ${...} inside strings
// 2. Parser: Parse interpolated string as template
// 3. Transform to concatenation at parse time:
//    "Hello, ${name}!" -> "Hello, " + str(name) + "!"

// Alternative: Template literal with backticks
// `Hello, ${name}! You are ${age} years old.`

// Edge cases to consider:
// - Escaping: "Price: \${100}"
// - Nested braces: "${obj["key"]}"
// - Format specifiers: "${pi:.2f}" for formatting

puts("");
puts("Level of Effort: MEDIUM");
puts("- Lexer: 3-4 hours (complex string parsing)");
puts("- Parser: 2-3 hours (expression extraction)");
puts("- AST: 1 hour (InterpolatedString node)");
puts("- Evaluator: 2 hours (evaluate and concatenate)");
puts("Total: ~9 hours");
puts("Alternative: Implement as built-in format() function (2 hours)");
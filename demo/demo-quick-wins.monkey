// Quick Win Extensions Demo
// New array, string, and math functions

puts("=== Array Operations ===");
puts("");

// Map - transform each element
let numbers = [1, 2, 3, 4, 5];
puts("numbers = " + str(numbers));
let doubled = map(numbers, fn(x) { x * 2 });
puts("map(numbers, fn(x) { x * 2 }) = " + str(doubled));
puts("");

// Filter - keep elements matching predicate
let evens = filter(numbers, fn(x) { x - (x / 2) * 2 == 0 });
puts("filter(numbers, isEven) = " + str(evens));
puts("");

// Reduce - fold to single value
let sum = reduce(numbers, fn(a, b) { a + b }, 0);
puts("reduce(numbers, +, 0) = " + str(sum));
puts("");

// Sort - order elements
let mixed = [3, 1, 4, 1, 5, 9, 2, 6];
puts("mixed = " + str(mixed));
puts("sort(mixed) = " + str(sort(mixed)));
puts("");

puts("=== String Functions ===");
puts("");

// Trim whitespace
puts("trim('  hello  ') = '" + trim("  hello  ") + "'");

// Replace substring
puts("replace('hello world', 'world', 'monkey') = " + replace("hello world", "world", "monkey"));

// Substring extraction
puts("substring('hello world', 0, 5) = " + substring("hello world", 0, 5));
puts("");

puts("=== Math Functions ===");
puts("");

puts("abs(-42) = " + str(abs(-42)));
puts("min(5, 2, 8, 1) = " + str(min(5, 2, 8, 1)));
puts("max(5, 2, 8, 1) = " + str(max(5, 2, 8, 1)));
puts("");

puts("🎉 Quick wins implemented successfully!");
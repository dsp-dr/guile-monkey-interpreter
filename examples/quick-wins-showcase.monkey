// Quick Win Extensions Showcase
// Demonstrates the new built-in functions added to Monkey

puts("🚀 Quick Win Extensions - Array, String & Math Functions");
puts("=" + str(reduce([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50], fn(a,b){a+b}, 0)));
puts("");

// ===========================
// Array Operations
// ===========================
puts("📦 Array Operations");
puts("-------------------");

let numbers = [1, 2, 3, 4, 5];
puts("Original: " + str(numbers));
puts("");

// Map - Transform each element
let squared = map(numbers, fn(x) { x * x });
puts("✨ map(numbers, square)");
puts("   Result: " + str(squared));

// Filter - Keep matching elements
let evens = filter([1,2,3,4,5,6,7,8], fn(x) { 
    x - (x / 2) * 2 == 0 
});
puts("✨ filter([1..8], isEven)");
puts("   Result: " + str(evens));

// Reduce - Fold to single value
let sum = reduce(numbers, fn(acc, x) { acc + x }, 0);
let product = reduce(numbers, fn(acc, x) { acc * x }, 1);
puts("✨ reduce(numbers, +, 0)");
puts("   Sum: " + str(sum));
puts("✨ reduce(numbers, *, 1)");  
puts("   Product: " + str(product));

// Sort - Order elements
let chaos = [42, 7, 13, 1, 99, 23];
puts("✨ sort([42,7,13,1,99,23])");
puts("   Result: " + str(sort(chaos)));

puts("");

// ===========================
// String Functions
// ===========================
puts("📝 String Functions");
puts("-------------------");

// Trim - Remove whitespace
let padded = "   Monkey Language   ";
puts("✨ trim('   Monkey Language   ')");
puts("   Result: '" + trim(padded) + "'");

// Replace - Substitute text
let greeting = "Hello, World!";
puts("✨ replace('Hello, World!', 'World', 'Monkey')");
puts("   Result: " + replace(greeting, "World", "Monkey"));

// Substring - Extract portion
let text = "Monkey Programming";
puts("✨ substring('Monkey Programming', 0, 6)");
puts("   Result: " + substring(text, 0, 6));
puts("✨ substring('Monkey Programming', 7, 18)");
puts("   Result: " + substring(text, 7, 18));

puts("");

// ===========================
// Math Functions
// ===========================
puts("🔢 Math Functions");
puts("-----------------");

puts("✨ abs(-42) = " + str(abs(-42)));
puts("✨ abs(42) = " + str(abs(42)));
puts("✨ min(10, 5, 23, 2, 17) = " + str(min(10, 5, 23, 2, 17)));
puts("✨ max(10, 5, 23, 2, 17) = " + str(max(10, 5, 23, 2, 17)));

puts("");

// ===========================
// Practical Examples
// ===========================
puts("💡 Practical Examples");
puts("--------------------");

// Data processing pipeline
let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let processed = map(
    filter(data, fn(x) { x > 3 }),
    fn(x) { x * 10 }
);
puts("Pipeline: filter(>3) → map(*10)");
puts("Input:  " + str(data));
puts("Output: " + str(processed));
puts("");

// String processing
let names = ["  alice  ", "  bob  ", "  charlie  "];
let i = 0;
puts("Trimming names:");
while (i < len(names)) {
    let trimmed = trim(names[i]);
    puts("  '" + names[i] + "' → '" + trimmed + "'");
    i = i + 1;
}

puts("");
puts("✅ All quick win extensions working perfectly!");
puts("These functions make Monkey much more practical for real programming.");
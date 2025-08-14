// Simple test of new functions
let arr = [1, 2, 3, 4, 5];
puts("Original: " + str(arr));

// Test map
let doubled = map(arr, fn(x) { x * 2 });
puts("Doubled: " + str(doubled));

// Test filter
let big = filter(arr, fn(x) { x > 2 });
puts("Greater than 2: " + str(big));

// Test reduce
let sum = reduce(arr, fn(a, b) { a + b }, 0);
puts("Sum: " + str(sum));

// Test sort
let mixed = [3, 1, 4, 1, 5];
puts("Sorted: " + str(sort(mixed)));

// Test math
puts("abs(-5) = " + str(abs(-5)));
puts("min(3,1,4) = " + str(min(3, 1, 4)));
puts("max(3,1,4) = " + str(max(3, 1, 4)));

// Test strings
puts("trim('  hi  ') = '" + trim("  hi  ") + "'");
puts("replace('hello', 'l', 'L') = " + replace("hello", "l", "L"));
puts("substring('hello', 1, 4) = " + substring("hello", 1, 4));
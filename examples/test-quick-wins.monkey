// Test Quick Win Extensions

puts("=== Testing Array Operations ===");

// Test map
let arr = [1, 2, 3, 4, 5];
let doubled = map(arr, fn(x) { x * 2 });
puts("map([1,2,3,4,5], double) = " + str(doubled));

// Test filter
let evens = filter(arr, fn(x) { x - (x / 2) * 2 == 0 });
puts("filter([1,2,3,4,5], even) = " + str(evens));

// Test reduce
let sum = reduce(arr, fn(a, b) { a + b }, 0);
puts("reduce([1,2,3,4,5], +, 0) = " + str(sum));

// Test sort
let unsorted = [3, 1, 4, 1, 5, 9, 2, 6];
let sorted = sort(unsorted);
puts("sort([3,1,4,1,5,9,2,6]) = " + str(sorted));

puts("");
puts("=== Testing Math Functions ===");

// Test abs
puts("abs(-42) = " + str(abs(-42)));
puts("abs(42) = " + str(abs(42)));

// Test min/max
puts("min(5, 2, 8, 1, 9) = " + str(min(5, 2, 8, 1, 9)));
puts("max(5, 2, 8, 1, 9) = " + str(max(5, 2, 8, 1, 9)));

puts("");
puts("=== Testing String Functions ===");

// Test trim
let padded = "  hello world  ";
puts("trim('  hello world  ') = '" + trim(padded) + "'");

// Test replace
let text = "hello world";
puts("replace('hello world', 'world', 'monkey') = " + replace(text, "world", "monkey"));

// Test substring
puts("substring('hello world', 0, 5) = " + substring("hello world", 0, 5));
puts("substring('hello world', 6, 11) = " + substring("hello world", 6, 11));

puts("");
puts("=== All Quick Win Tests Complete! ===");
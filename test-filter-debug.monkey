// Test filter with lambda shorthand
let nums = [1, 2, 3, 4];
puts("Original: " + str(nums));

// Test with regular function
let result1 = filter(nums, fn(x) { x > 1 });
puts("Regular fn: " + str(result1));

// Test with lambda shorthand  
let result2 = filter(nums, |x| x > 1);
puts("Lambda shorthand: " + str(result2));
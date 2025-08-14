// Experiment: Array Operations (map, filter, reduce, sort)
// Goal: Add functional array operations

// Current: Manual iteration
puts("=== Current: Manual array processing ===");
let numbers = [1, 2, 3, 4, 5];

// Manual map
let doubled = [];
let i = 0;
while (i < len(numbers)) {
    doubled = push(doubled, numbers[i] * 2);
    i = i + 1;
}
puts("Doubled: " + str(doubled));

// Proposed built-in functions:
// map(arr, fn)     - Transform each element
// filter(arr, fn)  - Keep elements where fn returns true
// reduce(arr, fn, init) - Fold array to single value
// sort(arr, fn?)   - Sort array (optional comparator)
// find(arr, fn)    - First element matching predicate
// every(arr, fn)   - All elements match predicate
// some(arr, fn)    - Any element matches predicate

// Examples:
// let nums = [1, 2, 3, 4, 5];
// map(nums, |x| x * 2)           // [2, 4, 6, 8, 10]
// filter(nums, |x| x > 2)        // [3, 4, 5]
// reduce(nums, |a, b| a + b, 0)  // 15
// sort([3, 1, 4, 1, 5])          // [1, 1, 3, 4, 5]

// Implementation in Monkey (not built-in):
let map = fn(arr, f) {
    let iter = fn(arr, acc) {
        if (len(arr) == 0) {
            acc
        } else {
            iter(rest(arr), push(acc, f(first(arr))))
        }
    };
    iter(arr, [])
};

let filter = fn(arr, pred) {
    let iter = fn(arr, acc) {
        if (len(arr) == 0) {
            acc
        } else {
            let el = first(arr);
            if (pred(el)) {
                iter(rest(arr), push(acc, el))
            } else {
                iter(rest(arr), acc)
            }
        }
    };
    iter(arr, [])
};

puts("");
puts("Level of Effort: LOW");
puts("- Implementation: 4-5 hours (all functions)");
puts("- Testing: 2 hours");
puts("Total: ~6 hours");
puts("Note: Can implement in Scheme as built-ins");
puts("Alternative: Provide as standard library (2 hours)");
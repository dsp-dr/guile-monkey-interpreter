// Monkey Language Feature Showcase
// Demonstrates all implemented features from Chapters 1-4

// ============================================
// Chapter 1-3: Core Language Features
// ============================================

// Arithmetic with correct precedence
puts("=== Arithmetic ===");
puts("5 + 5 * 2 = " + str(5 + 5 * 2));
puts("(5 + 5) * 2 = " + str((5 + 5) * 2));
puts("");

// Variables and functions
puts("=== Functions ===");
let add = fn(a, b) { a + b };
let multiply = fn(a, b) { a * b };
puts("add(3, 4) = " + str(add(3, 4)));
puts("multiply(3, 4) = " + str(multiply(3, 4)));
puts("");

// Closures
puts("=== Closures ===");
let makeAdder = fn(x) {
    fn(y) { x + y }
};
let addFive = makeAdder(5);
puts("addFive(10) = " + str(addFive(10)));
puts("");

// Arrays
puts("=== Arrays ===");
let arr = [1, 2, 3, 4, 5];
puts("Array: " + str(arr));
puts("First element: " + str(first(arr)));
puts("Last element: " + str(last(arr)));
puts("Rest: " + str(rest(arr)));
puts("Push 6: " + str(push(arr, 6)));
puts("");

// Hashes
puts("=== Hashes ===");
let person = {"name": "Alice", "age": 30, "city": "NYC"};
puts("Person: " + str(person));
puts("Name: " + person["name"]);
puts("Age: " + str(person["age"]));
puts("");

// Conditionals
puts("=== Conditionals ===");
let checkAge = fn(age) {
    if (age >= 18) {
        "Adult"
    } else {
        "Minor"
    }
};
puts("checkAge(25) = " + checkAge(25));
puts("checkAge(15) = " + checkAge(15));
puts("");

// Recursion
puts("=== Recursion ===");
let factorial = fn(n) {
    if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    }
};
puts("factorial(5) = " + str(factorial(5)));
puts("");

// ============================================
// Chapter 4: Extended Built-in Functions
// ============================================

puts("=== Chapter 4 Extensions ===");

// type() - Get object type
puts("type(42) = " + type(42));
puts("type(\"hello\") = " + type("hello"));
puts("type([1,2,3]) = " + type([1,2,3]));
puts("type({\"a\": 1}) = " + type({"a": 1}));
puts("");

// str() and int() - Type conversion
puts("str(123) = \"" + str(123) + "\"");
let numStr = "456";
puts("int(\"456\") = " + str(int(numStr)));
puts("");

// split() and join() - String operations
let words = split("hello,world,monkey", ",");
puts("split(\"hello,world,monkey\", \",\") = " + str(words));
puts("join(words, \" \") = " + join(words, " "));
puts("");

// contains() - Check containment
puts("contains(\"hello world\", \"world\") = " + str(contains("hello world", "world")));
puts("contains([1,2,3], 2) = " + str(contains([1,2,3], 2)));
puts("");

// keys() and values() - Hash operations
let data = {"x": 10, "y": 20, "z": 30};
puts("keys({x:10, y:20, z:30}) = " + str(keys(data)));
puts("values({x:10, y:20, z:30}) = " + str(values(data)));
puts("");

// delete() - Remove from hash
let updated = delete(data, "y");
puts("delete(data, \"y\") = " + str(updated));
puts("");

// ============================================
// Higher-Order Functions
// ============================================

puts("=== Higher-Order Functions ===");

// Map implementation
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))))
        }
    };
    iter(arr, [])
};

let double = fn(x) { x * 2 };
let numbers = [1, 2, 3, 4, 5];
puts("map([1,2,3,4,5], double) = " + str(map(numbers, double)));

// Filter implementation
let filter = fn(arr, predicate) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            let el = first(arr);
            if (predicate(el)) {
                iter(rest(arr), push(accumulated, el))
            } else {
                iter(rest(arr), accumulated)
            }
        }
    };
    iter(arr, [])
};

let isEven = fn(x) { x - (x / 2) * 2 == 0 };
puts("filter([1,2,3,4,5], isEven) = " + str(filter(numbers, isEven)));
puts("");

puts("=== Demo Complete ===");
puts("Monkey interpreter successfully implements all features!");
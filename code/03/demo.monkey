// Basic arithmetic
5 + 5 * 2

// Variables
let x = 10;
let y = 20;
x + y

// Functions
let add = fn(a, b) { a + b };
add(5, 10)

// Closures
let newAdder = fn(x) { fn(y) { x + y } };
let addTwo = newAdder(2);
addTwo(5)

// Arrays
let arr = [1, 2, 3, 4, 5];
arr[2]
len(arr)

// Recursion - Fibonacci
let fib = fn(n) { if (n < 2) { n } else { fib(n-1) + fib(n-2) } };
fib(10)

# Your First Monkey Program

Welcome to Monkey! This tutorial will guide you through writing and running your first Monkey programs.

## What is Monkey?

Monkey is a programming language with:
- C-like syntax
- Dynamic typing
- First-class functions
- Closures
- Arrays and hash maps
- Built-in functions

## Starting the REPL

The REPL (Read-Eval-Print-Loop) is an interactive environment where you can type Monkey code and see immediate results.

```bash
# Start the REPL
gmake repl

# Or directly
guile -L src src/monkey/main.scm
```

## Your First Expression

In the REPL, type:

```monkey
>> 5 + 5
10
```

Congratulations! You've evaluated your first Monkey expression.

## Variables

Variables are created with the `let` keyword:

```monkey
>> let name = "Monkey"
>> name
"Monkey"

>> let age = 5
>> age
5

>> let greeting = "Hello, " + name + "!"
>> greeting
"Hello, Monkey!"
```

## Basic Data Types

### Numbers (Integers)

```monkey
>> 42
42

>> -17
-17

>> 1 + 2 * 3
7
```

### Strings

```monkey
>> "Hello, World!"
"Hello, World!"

>> "Hello" + " " + "World"
"Hello World"

>> len("Monkey")
6
```

### Booleans

```monkey
>> true
true

>> false
false

>> 5 > 3
true

>> 10 == 10
true

>> 5 != 3
true
```

### Arrays

```monkey
>> [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]

>> let fruits = ["apple", "banana", "orange"]
>> fruits[0]
"apple"

>> fruits[1]
"banana"

>> len(fruits)
3
```

### Hash Maps

```monkey
>> {"name": "Alice", "age": 30}
{name: "Alice", age: 30}

>> let person = {"name": "Bob", "age": 25}
>> person["name"]
"Bob"

>> person["age"]
25
```

## Functions

Functions are first-class values in Monkey:

```monkey
>> let add = fn(a, b) { a + b }
>> add(5, 3)
8

>> let greet = fn(name) { "Hello, " + name + "!" }
>> greet("Alice")
"Hello, Alice!"
```

### Functions with Return Statements

```monkey
>> let max = fn(a, b) {
     if (a > b) {
       return a;
     } else {
       return b;
     }
   }
>> max(10, 5)
10
```

### Higher-Order Functions

Functions can take other functions as arguments:

```monkey
>> let twice = fn(f, x) { f(f(x)) }
>> let addTwo = fn(x) { x + 2 }
>> twice(addTwo, 5)
9
```

## Control Flow

### If Expressions

```monkey
>> if (5 > 3) { "yes" } else { "no" }
"yes"

>> let x = 10
>> if (x > 5) {
     puts("x is greater than 5");
     x * 2
   } else {
     puts("x is 5 or less");
     x
   }
x is greater than 5
20
```

### While Loops

```monkey
>> let counter = 0
>> while (counter < 3) {
     puts(counter);
     let counter = counter + 1;
   }
0
1
2
```

## Built-in Functions

Monkey comes with several built-in functions:

```monkey
>> len("hello")          // String length
5

>> len([1, 2, 3])       // Array length
3

>> first([1, 2, 3])     // First element
1

>> last([1, 2, 3])      // Last element
3

>> rest([1, 2, 3])      // All but first
[2, 3]

>> push([1, 2], 3)      // Append to array
[1, 2, 3]

>> puts("Hello!")       // Print to stdout
Hello!
null
```

## Your First Complete Program

Create a file called `hello.monkey`:

```monkey
// hello.monkey - Your first Monkey program

let name = "World";
let greeting = "Hello, " + name + "!";

puts(greeting);
puts("Welcome to Monkey programming!");

// Define a function
let factorial = fn(n) {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
};

// Use the function
puts("5! = " + str(factorial(5)));

// Work with arrays
let numbers = [1, 2, 3, 4, 5];
let doubled = map(numbers, fn(x) { x * 2 });
puts("Original: " + str(numbers));
puts("Doubled: " + str(doubled));

// Create and use an object-like hash
let person = {
    "name": "Alice",
    "age": 30,
    "greet": fn() { "Hi, I'm " + person["name"] }
};

puts(person["greet"]());
```

Run it:

```bash
# If you saved it as hello.monkey
guile -L src -c '(use-modules (monkey main)) (run-file "hello.monkey")'
```

## Common Patterns

### Counter Pattern

```monkey
let count = fn(n) {
    let i = 0;
    while (i < n) {
        puts(i);
        let i = i + 1;
    }
}

count(5);  // Prints 0, 1, 2, 3, 4
```

### Recursive Functions

```monkey
let fibonacci = fn(n) {
    if (n <= 1) {
        return n;
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}

puts(fibonacci(10));  // 55
```

### List Processing

```monkey
let sum = fn(arr) {
    reduce(arr, 0, fn(acc, x) { acc + x })
}

let numbers = [1, 2, 3, 4, 5];
puts(sum(numbers));  // 15
```

## Practice Exercises

1. **Calculator Function**: Write a function that takes an operator (+, -, *, /) and two numbers, returning the result.

2. **Array Reversal**: Write a function that reverses an array without using built-in functions.

3. **Find Maximum**: Write a function that finds the maximum value in an array.

4. **String Manipulation**: Write a function that counts the occurrences of a character in a string.

5. **Nested Functions**: Create a counter that returns a function that increments and returns its value each time it's called.

## Tips for Success

1. **Experiment in the REPL**: Try out expressions interactively
2. **Read error messages**: They'll help you understand what went wrong
3. **Start simple**: Build complex programs from simple pieces
4. **Use `puts()` for debugging**: Print values to understand program flow
5. **Check types with `type()`**: Useful for debugging type issues

## Common Mistakes to Avoid

1. **Forgetting semicolons**: Statements should end with `;`
2. **Missing return**: Functions need explicit `return` statements
3. **Variable scoping**: Variables created with `let` are scoped
4. **Array indexing**: Arrays are 0-indexed
5. **String concatenation**: Use `+` to join strings

## Next Steps

Now that you've written your first Monkey programs:

1. **Explore the REPL** → [Using the REPL](03-using-repl.md)
2. **Run script files** → [Running Scripts](04-running-scripts.md)
3. **Learn more features** → [Language Features](../02-language-features/)
4. **Try examples** → [Example Programs](../../examples/)

## Summary

You've learned:
- Basic Monkey syntax and data types
- How to define variables and functions
- Control flow with if expressions and while loops
- Using built-in functions
- Writing complete programs

Keep experimenting and have fun with Monkey!
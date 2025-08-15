puts("===========================================");
puts("    New Monkey Language Features Demo");
puts("===========================================");
puts("");

puts("1. ARRAY OPERATIONS");
puts("-------------------");

let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
puts("Original array: " + str(numbers));

let squares = map(numbers, fn(x) { x * x });
puts("Squares: " + str(squares));

let evens = filter(numbers, fn(x) { x - (x / 2) * 2 == 0 });
puts("Even numbers: " + str(evens));

let sum = reduce(numbers, fn(acc, x) { acc + x }, 0);
puts("Sum: " + str(sum));

let product = reduce([1, 2, 3, 4], fn(acc, x) { acc * x }, 1);
puts("Product of [1,2,3,4]: " + str(product));

let unsorted = [42, 7, 13, 1, 99, 23, 5];
puts("Unsorted: " + str(unsorted));
puts("Sorted: " + str(sort(unsorted)));

puts("");

puts("2. LAMBDA SHORTHAND");
puts("-------------------");

let add_traditional = fn(x, y) { x + y };
puts("Traditional: fn(x, y) { x + y }");
puts("Result: " + str(add_traditional(3, 4)));

let add_lambda = |x, y| x + y;
puts("Lambda: |x, y| x + y");
puts("Result: " + str(add_lambda(3, 4)));

puts("");
puts("Using lambdas with array operations:");

let nums = [1, 2, 3, 4, 5];
let doubled = map(nums, |x| x * 2);
puts("map([1,2,3,4,5], |x| x * 2) = " + str(doubled));

let large = filter(nums, |x| x > 3);
puts("filter([1,2,3,4,5], |x| x > 3) = " + str(large));

let total = reduce(nums, |a, b| a + b, 0);
puts("reduce([1,2,3,4,5], |a, b| a + b, 0) = " + str(total));

let make_adder = |n| |x| x + n;
let add5 = make_adder(5);
puts("Nested lambda make_adder(5)(10) = " + str(add5(10)));

puts("");

puts("3. STRING INTERPOLATION");
puts("-----------------------");

let name = "Alice";
let age = 30;
let city = "New York";

puts(format("Hello, {0}!", name));
puts(format("{0} is {1} years old", name, age));
puts(format("{0} lives in {1}", name, city));

let x = 10;
let y = 20;
puts(format("The sum of {0} and {1} is {2}", x, y, x + y));
puts(format("The product is {0}", x * y));

let data = [1, 2, 3, 4, 5];
let avg = reduce(data, |a, b| a + b, 0) / len(data);
puts(format("Data: {0}, Average: {1}", data, avg));

puts("");

puts("4. COMBINING ALL FEATURES");
puts("-------------------------");

let scores = [85, 92, 78, 95, 88, 73, 91];
let passing = filter(scores, |score| score >= 80);
let adjusted = map(passing, |score| score + 5);
let average = reduce(adjusted, |sum, score| sum + score, 0) / len(adjusted);

puts(format("Original scores: {0}", scores));
puts(format("Passing scores (>= 80): {0}", passing));
puts(format("After 5-point curve: {0}", adjusted));
puts(format("Class average: {0}", average));

let report = format(
    "Report: {0} students passed out of {1} ({2}%)",
    len(passing),
    len(scores),
    (len(passing) * 100) / len(scores)
);
puts(report);

puts("");
puts("===========================================");
puts("    Demo Complete - All Features Working!");
puts("=========================================");
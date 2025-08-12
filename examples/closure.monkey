// Closures and higher-order functions

let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
let addFive = newAdder(5);

puts(addTwo(3));   // 5
puts(addFive(3));  // 8

// Counter using closure
let makeCounter = fn() {
    let count = 0;
    fn() {
        let count = count + 1;
        count;
    };
};

let counter = makeCounter();
puts(counter()); // 1
puts(counter()); // 2
puts(counter()); // 3

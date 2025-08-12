// Fibonacci sequence calculator

let fibonacci = fn(n) {
    if (n < 2) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
};

puts("Fibonacci sequence:");
let i = 0;
while (i < 10) {
    puts(fibonacci(i));
    let i = i + 1;
}

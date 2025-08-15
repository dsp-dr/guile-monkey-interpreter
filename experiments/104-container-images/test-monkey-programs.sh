#!/bin/bash
# Test core Monkey programs to verify evaluator functionality

set -e

echo "=== Testing Core Monkey Programs ==="
echo

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Track results
PASSED=0
FAILED=0

# Create temporary directory for test programs
TESTDIR=$(mktemp -d)
trap "rm -rf $TESTDIR" EXIT

# Function to create and test a Monkey program
test_monkey() {
    local name=$1
    local program=$2
    local expected=$3
    
    echo -n "Testing $name: "
    
    # Write program to file
    echo "$program" > "$TESTDIR/$name.monkey"
    
    # Run the program
    if output=$(guile -L src -c "
        (use-modules (monkey main))
        (use-modules (ice-9 textual-ports))
        (with-input-from-file \"$TESTDIR/$name.monkey\"
          (lambda ()
            (let* ((input (get-string-all (current-input-port))))
              (run-string input))))
    " 2>&1); then
        
        # Check if output contains expected result
        if echo "$output" | grep -q "$expected"; then
            echo -e "${GREEN}✓ PASS${NC}"
            ((PASSED++))
        else
            echo -e "${RED}✗ FAIL${NC}"
            echo "  Expected to find: '$expected'"
            echo "  Got: $output"
            ((FAILED++))
        fi
    else
        echo -e "${RED}✗ ERROR${NC}"
        echo "  Error: $output"
        ((FAILED++))
    fi
}

# Test 1: Variables and arithmetic
test_monkey "arithmetic" \
'let x = 5;
let y = 10;
let result = x + y;
puts(result);' \
"15"

# Test 2: Functions
test_monkey "functions" \
'let add = fn(a, b) { a + b };
let result = add(3, 4);
puts(result);' \
"7"

# Test 3: Recursion
test_monkey "recursion" \
'let factorial = fn(n) {
    if (n == 0) {
        1
    } else {
        n * factorial(n - 1)
    }
};
puts(factorial(5));' \
"120"

# Test 4: Arrays
test_monkey "arrays" \
'let arr = [1, 2, 3, 4, 5];
puts(len(arr));
puts(first(arr));
puts(last(arr));' \
"5.*1.*5"

# Test 5: Hash maps
test_monkey "hashes" \
'let person = {"name": "Bob", "age": 25};
puts(person["name"]);
puts(person["age"]);' \
"Bob.*25"

# Test 6: Higher-order functions
test_monkey "higher_order" \
'let twice = fn(f, x) { f(f(x)) };
let addTwo = fn(x) { x + 2 };
puts(twice(addTwo, 5));' \
"9"

# Test 7: Closures
test_monkey "closures" \
'let makeAdder = fn(x) {
    fn(y) { x + y }
};
let add5 = makeAdder(5);
puts(add5(10));' \
"15"

# Test 8: If expressions
test_monkey "conditionals" \
'let max = fn(a, b) {
    if (a > b) {
        a
    } else {
        b
    }
};
puts(max(10, 5));
puts(max(3, 7));' \
"10.*7"

# Test 9: String operations
test_monkey "strings" \
'let hello = "Hello";
let world = "World";
let greeting = hello + ", " + world + "!";
puts(greeting);
puts(len(greeting));' \
"Hello, World!.*13"

# Test 10: Boolean operations
test_monkey "booleans" \
'let a = true;
let b = false;
puts(a);
puts(b);
puts(5 > 3);
puts(10 == 10);
puts(5 != 3);' \
"true.*false.*true.*true.*true"

echo
echo "=== Test Summary ==="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed${NC}"
    exit 1
fi
#!/bin/bash
# Test runner for Monkey interpreter

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
TEST_DIR="$PROJECT_DIR/tests"

cd "$PROJECT_DIR"

run_test() {
    local test_name=$1
    local test_file="$TEST_DIR/${test_name}-test.scm"
    
    if [ -f "$test_file" ]; then
        echo "Running $test_name tests..."
        guile -L src/monkey --no-auto-compile -l "$test_file"
    else
        echo "Test file not found: $test_file"
        return 1
    fi
}

if [ "$1" = "all" ] || [ -z "$1" ]; then
    echo "Running all tests..."
    for test_file in "$TEST_DIR"/*-test.scm; do
        if [ -f "$test_file" ]; then
            echo "Running $(basename "$test_file")..."
            guile -L src/monkey --no-auto-compile -l "$test_file"
        fi
    done
else
    run_test "$1"
fi

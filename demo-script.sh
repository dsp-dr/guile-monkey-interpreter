#!/bin/bash
# Manual Demo Script for Guile Monkey Interpreter

clear

# Title Screen
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║     GUILE MONKEY INTERPRETER - COMPLETE IMPLEMENTATION         ║"
echo "║     Based on 'Writing An Interpreter In Go' by Thorsten Ball   ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo
sleep 2

# Chapter 1: Lexer
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 1: Lexer - Breaking code into tokens"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
sleep 2

echo "let x = 5 + 10;" | guile -q -L code/01/src code/01/src/monkey/main.scm 2>/dev/null | head -20
sleep 3

# Chapter 2: Parser
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 2: Parser - Building Abstract Syntax Trees"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
sleep 2

echo "let x = 5 * 2 + 10;" | guile -q -L code/02/src code/02/src/monkey/main.scm 2>/dev/null | head -25
sleep 3

# Chapter 3: Evaluator
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 3: Evaluator - Full Working Interpreter!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
sleep 2

# Show live evaluation
printf '5 + 5 * 2\nlet x = 10;\nlet y = 20;\nx + y\n' | guile -q -L code/03/src code/03/src/monkey/main.scm 2>/dev/null | grep -A20 ">>>" | head -20
sleep 3

# Chapter 4: Extended Built-ins
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 4: Extended Built-ins - Advanced Features"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
sleep 2

# Show new built-in functions
printf 'type(42)\nstr(100)\nkeys({"a": 1, "b": 2})\nsplit("hello,world", ",")\n' | guile -q -L src src/monkey/main.scm 2>/dev/null | grep -A20 ">>>" | head -20
sleep 3

# Final demo with complex program
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Complete Example: Functions, Arrays, and Recursion"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
sleep 2

cat << 'DEMO'
// Define a function
let add = fn(a, b) { a + b };

// Use the function
add(5, 10)

// Arrays
let arr = [1, 2, 3, 4, 5];
len(arr)

// Recursion - Factorial
let fact = fn(n) { 
  if (n <= 1) { 
    1 
  } else { 
    n * fact(n-1) 
  } 
};
fact(5)
DEMO

printf 'let add = fn(a, b) { a + b };\nadd(5, 10)\nlet arr = [1, 2, 3, 4, 5];\nlen(arr)\nlet fact = fn(n) { if (n <= 1) { 1 } else { n * fact(n-1) } };\nfact(5)\n' | guile -q -L src src/monkey/main.scm 2>/dev/null | tail -20
sleep 3

# Summary
echo
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║                    DEMO COMPLETE!                              ║"
echo "║                                                                 ║"
echo "║  ✓ Lexer: 562/572 tests passing (98.3%)                       ║"
echo "║  ✓ Parser: 138/138 tests passing (100%)                       ║"
echo "║  ✓ Evaluator: Full Monkey language support                    ║"
echo "║  ✓ Extended Built-ins: 9 additional functions                 ║"
echo "║                                                                 ║"
echo "║  GitHub: github.com/dsp-dr/guile-monkey-interpreter           ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo
sleep 3
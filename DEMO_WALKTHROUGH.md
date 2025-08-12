# Manual Demo Walkthrough

This is a step-by-step guide for manually recording the Monkey interpreter demo with asciinema.

**IMPORTANT**: This must be done manually in a terminal. Claude cannot type into asciinema recordings.
You need to open a terminal and follow these steps yourself.

## Prerequisites
- Working directory: `/home/dsp-dr/ghq/github.com/dsp-dr/guile-monkey-interpreter`
- Guile installed and working
- asciinema installed

## Recording Steps

### 1. Start asciinema recording
```bash
asciinema rec --title "Guile Monkey Interpreter" --idle-time-limit 2.0 monkey-demo.cast
```

### 2. Clear screen and start REPL
```bash
clear
gmake repl
```

Wait for the monkey face ASCII art and welcome message.

### 3. Basic Arithmetic Demo
Type these commands one by one, pressing Enter after each:

```monkey
// Arithmetic with operator precedence
5 + 5 * 2
```
Expected output: `15`

### 4. Variables
```monkey
let x = 10;
let y = 20;
x + y
```
Expected outputs: `10`, `20`, `30`

### 5. Functions
```monkey
let add = fn(a, b) { a + b };
add(15, 27)
```
Expected outputs: `fn(?, ?) { ... }`, `42`

### 6. Arrays
```monkey
let arr = [1, 2, 3, 4, 5];
len(arr)
arr[2]
```
Expected outputs: `[1, 2, 3, 4, 5]`, `5`, `3`

### 7. Chapter 4 Extensions - Type System
```monkey
type(42)
type("hello")
type(true)
```
Expected outputs: `INTEGER`, `STRING`, `BOOLEAN`

### 8. Chapter 4 Extensions - String Operations
```monkey
str(100)
int("42")
split("hello,world", ",")
join(["hello", "world"], " ")
```
Expected outputs: `100`, `42`, `[hello, world]`, `hello world`

### 9. Chapter 4 Extensions - Hash Operations
```monkey
let person = {"name": "Alice", "age": 30};
keys(person)
values(person)
```
Expected outputs: `{name: Alice, age: 30}`, `[name, age]`, `[Alice, 30]`

### 10. Exit
Press `Ctrl-C` to exit the REPL (will show `gmake: *** [Makefile:51: repl] Interrupt`)

Then press `Ctrl-D` or type `exit` to stop recording.

### 11. Upload
```bash
asciinema upload monkey-demo.cast
```

## Troubleshooting

### If REPL doesn't start:
- Check you're in the correct directory
- Try running directly: `guile -L src src/monkey/main.scm`

### If functions don't work:
- Note: Recursive functions may have issues with undefined `return` variable
- Stick to simple function examples

### If warnings appear:
- Warnings about overriding core bindings are normal
- They appear after the first expression is evaluated

## Confirmed Working Examples

All the following have been tested and work correctly:
- ✅ Arithmetic: `5 + 5 * 2` → `15`
- ✅ Variables: `let x = 10;` → `10`
- ✅ Functions: `let add = fn(a, b) { a + b };` → `fn(?, ?) { ... }`
- ✅ Function calls: `add(15, 27)` → `42`
- ✅ Type checking: `type(42)` → `INTEGER`
- ✅ Hashes: `{"name": "Alice", "age": 30}` → `{name: Alice, age: 30}`
- ✅ Hash keys: `keys(person)` → `[name, age]`
- ✅ String split: `split("hello,world", ",")` → `[hello, world]`

## Notes
- DO NOT use scripts for the demo - type everything manually
- Take your time between commands for readability
- The idle timeout is 2 seconds, so don't pause too long
- Comments with `//` are ignored by the interpreter
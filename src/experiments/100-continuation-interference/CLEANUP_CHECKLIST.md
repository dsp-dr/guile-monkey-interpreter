# Continuation Cleanup Checklist

## Problem Statement
The `with-return` macro using `call/cc` causes "unbound variable: return" errors when functions using it are called from within other `with-return` contexts. This is due to lexical scoping issues with nested continuations.

## Phase 1: Analysis and Documentation

### 1.1 Identify All with-return Usage
- [ ] List all functions using with-return
- [ ] Identify call hierarchy (which functions call which)
- [ ] Mark functions that are called from within other with-return contexts

### 1.2 Analyze Scheme IR
- [ ] Generate intermediate representation for problematic functions
- [ ] Identify where continuation bindings are lost
- [ ] Document the scope chain issues

### 1.3 Debug Trace Analysis
- [ ] Add debug output to track continuation flow
- [ ] Identify exact points of failure
- [ ] Document call stack at error points

## Phase 2: Systematic Cleanup

### 2.1 Functions to Clean (Priority Order)
- [ ] parse-let-statement ✓ (already done)
- [ ] parse-expression-statement
- [ ] parse-block-statement  
- [ ] parse-if-expression
- [ ] parse-while-expression
- [ ] parse-function-literal
- [ ] parse-call-expression
- [ ] parse-prefix-expression
- [ ] parse-infix-expression
- [ ] parse-grouped-expression
- [ ] parse-array-literal
- [ ] parse-expression-list

### 2.2 Cleanup Strategy for Each Function
1. Remove `with-return` wrapper
2. Replace `(return #f)` with `#f`
3. Replace `(return value)` with `value`
4. Use `if` instead of `unless` when returning early
5. Test function in isolation
6. Test function when called from parent

### 2.3 Testing Protocol
- [ ] Unit test each cleaned function
- [ ] Integration test with parent functions
- [ ] Full test suite after each batch

## Phase 3: Verification

### 3.1 Test Coverage
- [ ] For loops with all variations
- [ ] While loops with break/continue
- [ ] Nested control structures
- [ ] Complex expressions

### 3.2 Performance Check
- [ ] Measure parsing speed before/after
- [ ] Check memory usage
- [ ] Verify no regression in other features

## Phase 4: Documentation

### 4.1 Document Changes
- [ ] List all modified functions
- [ ] Explain why each change was necessary
- [ ] Document any behavioral differences

### 4.2 Update Comments
- [ ] Add warnings about continuation usage
- [ ] Document the pattern to avoid
- [ ] Provide examples of correct usage

## Current Status

### Functions Using with-return (from grep):
1. Line 233: parse-expression-statement
2. Line 333: parse-if-expression
3. Line 358: parse-while-expression
4. Line 435: parse-function-literal
5. Line 449: parse-lambda-shorthand
6. Line 478: parse-call-expression
7. Line 506: parse-prefix-expression
8. Line 555: parse-infix-expression
9. Line 565: parse-grouped-expression
10. Line 611: parse-expression-list

### Call Hierarchy (Functions that call others):
- parse-program → parse-statement → parse-expression-statement → parse-expression
- parse-expression calls many sub-parsers
- parse-for-expression → parse-let-statement, parse-expression-statement, parse-expression
- parse-if-expression → parse-expression, parse-block-statement
- parse-while-expression → parse-expression, parse-block-statement

### Already Fixed:
- ✓ parse-let-statement (removed with-return)
- ✓ parse-for-expression (removed with-return)

## Next Steps
1. Start with parse-expression-statement as it's called frequently
2. Move to parse-block-statement as it's used by control structures
3. Fix remaining expression parsers in dependency order
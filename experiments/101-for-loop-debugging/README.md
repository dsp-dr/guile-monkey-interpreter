# Experiment 101: For Loop Debugging

## Problem Statement
For loops parse correctly but fail during evaluation with all 13 tests failing. Need to identify and fix the evaluation issues.

## Current Symptoms
- Parser compiles successfully
- All for loop tests fail with "ERROR: Parse error" or evaluation errors
- While loops with break/continue work correctly
- Issue appears to be in the evaluation phase, not parsing

## Debugging Strategy

### Phase 1: Trace Analysis
1. Add detailed tracing to parser to see what AST is generated
2. Add tracing to evaluator to see execution flow
3. Compare for loop AST with working while loop AST
4. Identify where evaluation diverges

### Phase 2: Root Cause Analysis
1. Check if FOR token is properly registered
2. Verify parse-for-expression is being called
3. Ensure AST nodes are properly created
4. Validate evaluator handles for-expression correctly

### Phase 3: Fix Implementation
1. Fix identified issues
2. Test incrementally with simplest case first
3. Gradually test more complex scenarios
4. Ensure all 13 tests pass

## Tools Needed

### 1. AST Dumper
- Pretty-print AST structure
- Compare expected vs actual AST
- Highlight differences

### 2. Evaluation Tracer
- Step-by-step evaluation trace
- Show environment changes
- Track control flow

### 3. Parser Debugger
- Token stream viewer
- Parse tree visualizer
- Error location pinpointer

### 4. Test Harness
- Run individual test cases
- Compare output with expected
- Generate minimal reproduction cases

## Test Cases (Simplest to Complex)

1. Empty for loop: `for (;;) { }`
2. Simple counter: `for (let i = 0; i < 3; i = i + 1) { }`
3. With body: `for (let i = 0; i < 3; i = i + 1) { puts(i); }`
4. With break: `for (;;) { break; }`
5. With continue: `for (let i = 0; i < 5; i = i + 1) { if (i == 2) { continue; } }`
6. Nested loops: `for (let i = 0; i < 2; i = i + 1) { for (let j = 0; j < 2; j = j + 1) { } }`

## Expected vs Actual Behavior

### Expected (from while loop that works):
```
while (i < 3) { 
  // body
  i = i + 1; 
}
```

### For loop equivalent (failing):
```
for (let i = 0; i < 3; i = i + 1) { 
  // body
}
```

## Hypothesis
The issue might be:
1. FOR token not properly registered as a prefix operator
2. parse-for-expression not being exported/accessible
3. Evaluator missing for-expression case
4. Scoping issue with let in init clause
5. Update expression not being evaluated correctly
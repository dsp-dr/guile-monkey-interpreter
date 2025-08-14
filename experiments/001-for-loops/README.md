# Experiment 001: For Loops

## Overview
Implementation of C-style for loops in Monkey.

## Current Workaround
```monkey
// Simulating: for (i = 0; i < 10; i++)
let i = 0;
while (i < 10) {
    puts(i);
    let i = i + 1;
}
```

## Proposed Syntax
```monkey
for (let i = 0; i < 10; i = i + 1) {
    puts(i);
}
```

## Implementation Effort
- **Estimated Time**: 8 hours
- **Complexity**: MEDIUM
- **Priority**: MEDIUM

## Files
- `for-loops.monkey` - Example implementation
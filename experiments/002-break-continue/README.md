# Experiment 002: Break/Continue Statements

## Overview
Loop control flow statements for early exit and iteration skipping.

## Current Limitation
No break or continue statements - must use flags or restructure logic.

## Proposed Syntax
```monkey
while (true) {
    if (condition) {
        break;  // Exit loop
    }
    if (other_condition) {
        continue;  // Skip to next iteration
    }
}
```

## Implementation Effort
- **Estimated Time**: 8 hours
- **Complexity**: MEDIUM
- **Priority**: HIGH

## Files
- `break-continue.monkey` - Example implementation
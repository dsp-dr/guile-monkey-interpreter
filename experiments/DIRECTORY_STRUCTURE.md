# Experiments Directory Structure

All experiments follow the XXX-{name} numbering scheme for consistent organization.

## Directory Layout

```
experiments/
├── 001-for-loops/              # C-style for loops
│   ├── README.md
│   └── for-loops.monkey
├── 002-break-continue/         # Loop control flow
│   ├── README.md
│   └── break-continue.monkey
├── 003-string-interpolation/   # Template strings
│   ├── README.md
│   └── string-interpolation.monkey
├── 004-pattern-matching/       # Scheme-like match
│   ├── README.md
│   └── pattern-matching.monkey
├── 005-exception-handling/     # Try/catch/finally
│   ├── README.md
│   └── exception-handling.monkey
├── 006-tail-call-optimization/ # Recursive optimization
│   ├── README.md
│   └── tail-call-optimization.monkey
├── 007-anonymous-shorthand/    # Lambda syntax sugar
│   ├── README.md
│   └── anonymous-shorthand.monkey
├── 008-array-operations/       # Functional array methods
│   ├── README.md
│   └── array-operations.monkey
├── 009-module-system/          # Import/export system
│   ├── README.md
│   └── module-system.monkey
├── 010-ffi-extensions/         # FFI filesystem/HTTP [IMPLEMENTED]
│   ├── README.md
│   ├── IMPLEMENTATION_COMPLETE.md
│   ├── monkey_fs.c
│   ├── monkey_http.c
│   ├── ffi-bridge.scm
│   ├── Makefile
│   ├── test_fs.c
│   ├── test_http.c
│   ├── test-ffi.scm
│   ├── test-simple.scm
│   ├── libmonkey_fs.so*
│   └── libmonkey_http.so*
├── README.md                   # Main experiments overview
├── IMPLEMENTATION_PLAN.md      # Detailed analysis
└── DIRECTORY_STRUCTURE.md      # This file
```

## Numbering Convention

- **001-099**: Core language features
- **100-199**: Built-in extensions  
- **200-299**: System integrations
- **300-399**: Development tools
- **400-499**: Performance optimizations
- **500+**: Experimental/Research

## Status

- ✅ **010-ffi-extensions**: Fully implemented with tests
- 🔧 **001-009**: Proof of concept / analysis phase

## Running Experiments

```bash
# Run example Monkey programs
./monkey experiments/001-for-loops/for-loops.monkey

# Build and test FFI extensions
cd experiments/010-ffi-extensions
gmake all
guile test-ffi.scm

# Or use main Makefile
gmake ffi-test
```
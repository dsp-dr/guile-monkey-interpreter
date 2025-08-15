# Advanced GDB Debugging with Guile

## Overview

This guide provides advanced techniques for debugging Guile Scheme programs using GDB, based on community wisdom from Ramin Honary and practical experience debugging the Monkey interpreter.

## Setup

### 1. Install Development Packages

```bash
# Debian/Ubuntu
sudo apt-get install guile-3.0-dev gdb

# Fedora/RHEL
sudo dnf install guile30-devel gdb

# FreeBSD
sudo pkg install guile3 gdb
```

### 2. Compile with Debug Symbols

If you have C extensions or want maximum debugging capability:

```bash
export CFLAGS="-g -O0"
export LDFLAGS="-g"
guile-snarf -o myext.x myext.c
gcc -g -O0 -shared -fPIC -o myext.so myext.c `pkg-config --cflags --libs guile-3.0`
```

## Using the GDB Script

### Basic Usage

```bash
./gdb-guile.sh my-program.scm arg1 arg2
```

### What the Script Does

1. **Handles signals gracefully**:
   - `SIGXCPU` - CPU time limit exceeded (ignored)
   - `SIGPWR` - Power failure signal (ignored)

2. **Auto-runs your program** - No need to type `run` manually

## Common Debugging Scenarios

### 1. Segmentation Fault

```gdb
# When your program crashes
(gdb) bt                           # Show backtrace
(gdb) frame 3                      # Go to frame 3
(gdb) info locals                  # Show local variables
(gdb) print some_variable          # Print specific variable
```

### 2. Infinite Loop

```gdb
# Start program, then Ctrl+C when it hangs
(gdb) bt
(gdb) info threads                 # See all threads
(gdb) thread 2                     # Switch to thread 2
(gdb) bt
```

### 3. Memory Issues

```gdb
# Track memory allocations
(gdb) break scm_gc_malloc
(gdb) commands
> silent
> printf "Allocating %d bytes\n", size
> continue
> end
```

### 4. Exception Tracking

```gdb
# Break on Scheme errors
(gdb) break scm_error
(gdb) break scm_throw
(gdb) break scm_wrong_type_arg
```

## Advanced GDB Commands for Guile

### Printing Scheme Values

```gdb
# Print a Scheme value (SCM type)
(gdb) print my_scheme_var
$1 = 0x7ffff7a12340

# Display it in readable form
(gdb) call scm_display($1, scm_current_output_port())
"Hello, World!"

# Write to string for inspection
(gdb) call scm_object_to_string($1, scm_undefined)
$2 = 0x7ffff7a12380
(gdb) call scm_display($2, scm_current_output_port())
"#<procedure foo (x y)>"
```

### Examining Procedures

```gdb
# Check if something is a procedure
(gdb) call scm_procedure_p($1)
$3 = 0x404  # SCM_BOOL_T

# Get procedure name
(gdb) call scm_procedure_name($1)
$4 = 0x7ffff7a12400
(gdb) call scm_display($4, scm_current_output_port())
parse-expression
```

### Working with Lists

```gdb
# Check list length
(gdb) call scm_length($1)
$5 = 0x17  # 23 in decimal

# Get car and cdr
(gdb) call scm_car($1)
$6 = 0x7ffff7a12500
(gdb) call scm_cdr($1)
$7 = 0x7ffff7a12600
```

## Debugging Our Parser Issue

Here's how we could have used GDB to debug the parser parenthesis issue:

```gdb
# Start GDB session
./gdb-guile.sh test-parser.scm

# Set breakpoint at parser loading
(gdb) break scm_primitive_load
(gdb) continue

# When it breaks, check what's being loaded
(gdb) call scm_display($1, scm_current_output_port())
"parser.scm"

# Set breakpoint on read errors
(gdb) break scm_i_lreadparen
(gdb) continue

# When parenthesis mismatch occurs
(gdb) bt
#0  scm_i_lreadparen at read.c:123
#1  scm_read at read.c:456
#2  scm_primitive_load at load.c:78

# Check the file position
(gdb) print port->line_number
$8 = 471
(gdb) print port->column_number  
$9 = 82
```

## Creating Custom GDB Commands

Add to `~/.gdbinit`:

```gdb
# Command to display Scheme object
define scm-display
  call scm_display($arg0, scm_current_output_port())
  printf "\n"
end

# Command to show object type
define scm-type
  if scm_is_pair($arg0)
    printf "pair\n"
  else
    if scm_is_string($arg0)
      printf "string\n"
    else
      if scm_is_number($arg0)
        printf "number\n"
      else
        printf "other\n"
      end
    end
  end
end

# Pretty-print Scheme backtrace
define scm-backtrace
  call scm_backtrace()
end
```

## Debugging Macros and Continuations

### Tracing Macro Expansion

```gdb
# Break on macro expansion
(gdb) break scm_macro_expand
(gdb) commands
> silent
> printf "Expanding macro:\n"
> call scm_display($1, scm_current_output_port())
> printf "\n"
> continue
> end
```

### Debugging call/cc Issues

```gdb
# Track continuation captures
(gdb) break scm_call_with_current_continuation
(gdb) commands
> silent
> printf "Capturing continuation\n"
> bt 5
> continue
> end
```

## Performance Analysis with GDB

### Finding Hotspots

```gdb
# Sample where program spends time
(gdb) handle SIGALRM nostop noprint
(gdb) run &
(gdb) while 1
> where
> continue
> shell sleep 0.1
> end
```

### Memory Profiling

```gdb
# Track GC runs
(gdb) break scm_gc
(gdb) commands
> silent
> printf "GC run %d\n", gc_count++
> continue
> end
```

## Integration with Guile's Debug Facilities

### Combining GDB with Guile Debugging

```scheme
;; In your Scheme code
(use-modules (system vm trace))

(define (debug-with-gdb proc)
  ;; Trigger GDB breakpoint
  (system* "kill" "-SIGTRAP" (number->string (getpid)))
  ;; Continue with traced execution
  (trace-calls-to-procedure proc))
```

### Using from REPL

```scheme
;; Start with GDB attached
;; $ gdb guile
;; (gdb) run

;; In Guile REPL
,use (system foreign)
,use (system vm debug)

;; Trigger debugger
(raise SIGTRAP)

;; GDB will catch this
```

## Tips and Tricks

### 1. Conditional Breakpoints

```gdb
# Break only when parsing specific function
(gdb) break parse_expression if strcmp(parser->cur_token->literal, "for") == 0
```

### 2. Watchpoints

```gdb
# Watch for variable changes
(gdb) watch parser->errors
(gdb) watch -l *(int*)0x7ffff7a12340
```

### 3. Reverse Debugging

```gdb
# Record execution for replay
(gdb) target record-full
(gdb) continue
# When error occurs
(gdb) reverse-continue  # Go backwards!
```

### 4. Python Scripts in GDB

```python
# Save as guile-helpers.py
import gdb

class SCMPrinter(gdb.Command):
    def __init__(self):
        super(SCMPrinter, self).__init__("scm-print", gdb.COMMAND_USER)
    
    def invoke(self, arg, from_tty):
        val = gdb.parse_and_eval(arg)
        gdb.execute(f"call scm_display({val}, scm_current_output_port())")

SCMPrinter()
```

Load in GDB:
```gdb
(gdb) source guile-helpers.py
(gdb) scm-print my_var
```

## Troubleshooting Common Issues

### "No debugging symbols found"

```bash
# Reinstall with debug symbols
sudo apt-get install guile-3.0-dbg

# Or compile from source
./configure CFLAGS="-g -O0"
make
```

### "Cannot find bounds of current function"

```gdb
# Use different stack unwinder
(gdb) set unwind-on-terminating-exception on
(gdb) set unwindonsignal on
```

### GDB Crashes on Complex Scheme Objects

```gdb
# Limit recursion depth
(gdb) set print max-depth 3
(gdb) set print elements 10
```

## Conclusion

GDB provides powerful low-level debugging for Guile programs. While the Scheme-level debugger is usually sufficient, GDB becomes invaluable for:

- Segmentation faults
- Infinite loops
- Memory leaks
- C extension debugging
- Performance analysis
- Understanding Guile internals

Combined with Guile's built-in debugging facilities, GDB gives you complete visibility into your program's execution at both the Scheme and C levels.

Remember: Start with Scheme-level debugging (`,bt`, `,trace`, etc.) and resort to GDB when you need to dig deeper into the runtime system itself.
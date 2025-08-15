# tmux Debugging Session Guide

## Overview

The `tmux-guile.sh` script creates a persistent debugging environment with 8 specialized windows for different aspects of Monkey interpreter development.

## Session Name

The session is always named: **`monkey-debug`**

## Connecting to the Session

```bash
# Start or attach to the session
./scripts/tmux-guile.sh

# Manually attach if already running
tmux attach -t monkey-debug

# List all tmux sessions
tmux list-sessions | grep monkey
```

## Window Layout

| Window | Name | Purpose | Pre-loaded |
|--------|------|---------|------------|
| 0 | REPL | Main interpreter REPL | Monkey modules |
| 1 | Tests | Test runner (left) + file watcher (right) | Test commands |
| 2 | Parser | Parser-focused debugging | Lexer + Parser modules |
| 3 | Evaluator | Evaluator debugging | Evaluator + Object modules |
| 4 | Editor | Code editing + file browser | - |
| 5 | Trace | Profiling and tracing | trace + statprof modules |
| 6 | Git | Version control | git status |
| 7 | Logs | Debug output monitoring | - |

## Usage Example: Parser Debugging

```bash
# 1. Start the session
./scripts/tmux-guile.sh

# 2. Switch to Parser window
# Press: Ctrl+b 2

# 3. The window has Guile REPL with modules loaded
# You'll see:
scheme@(guile-user)> 

# 4. Test parsing
(define input "let x = 5 + 10;")
(define p (make-parser (make-lexer input)))
(parse-program p)

# 5. Debug specific functions
,break parse-expression
,trace parse-infix-expression
```

## Sending Commands Programmatically

```bash
# Send commands to specific windows
tmux send-keys -t monkey-debug:Parser "(define x 5)" C-m

# Capture output from a window
tmux capture-pane -t monkey-debug:Parser -p

# Run commands in REPL window
tmux send-keys -t monkey-debug:REPL "(start-repl)" C-m
```

## Navigation

| Key Combination | Action |
|----------------|--------|
| `Ctrl+b` then `0-7` | Switch to window 0-7 |
| `Ctrl+b` then `d` | Detach from session |
| `Ctrl+b` then `w` | List all windows |
| `Ctrl+b` then `arrow` | Navigate panes |
| `Ctrl+b` then `%` | Split vertically |
| `Ctrl+b` then `"` | Split horizontally |
| `Ctrl+b` then `x` | Kill current pane |

## Debugging Commands in REPL

```scheme
,help debug    ; Show all debug commands
,bt            ; Backtrace
,up / ,down    ; Navigate call stack
,frame N       ; Jump to frame N
,locals        ; Show local variables
,break PROC    ; Set breakpoint
,trace PROC    ; Trace procedure calls
,profile EXPR  ; Profile expression
```

## Session Persistence

The tmux session persists even after disconnection:

```bash
# Detach from session (keeps it running)
Ctrl+b d

# Reattach later
tmux attach -t monkey-debug

# Kill the session when done
tmux kill-session -t monkey-debug
```

## Troubleshooting

### Session Already Exists
If you see "Session 'monkey-debug' already exists", either:
- Attach to it: `tmux attach -t monkey-debug`
- Kill and recreate: `tmux kill-session -t monkey-debug && ./scripts/tmux-guile.sh`

### No tmux Installed
```bash
# FreeBSD
pkg install tmux

# Debian/Ubuntu
apt-get install tmux

# macOS
brew install tmux
```

### Guile Not Loading
Each window starts with bash. Start Guile manually:
```bash
guile --no-auto-compile -L .
```

## Example Workflow

1. **Start Session**: `./scripts/tmux-guile.sh`
2. **Parser Work**: `Ctrl+b 2` (Parser window)
3. **Test Changes**: `Ctrl+b 1` (Tests window)
4. **Check Git**: `Ctrl+b 6` (Git window)
5. **Debug Issues**: `Ctrl+b 0` (REPL with `,bt` etc.)
6. **Detach**: `Ctrl+b d`
7. **Resume Later**: `tmux attach -t monkey-debug`

## Integration with Other Tools

The tmux session works well with:
- **GDB**: Run `scripts/gdb-guile.sh` in any window
- **File Watchers**: Set up in Tests window pane
- **Continuous Testing**: Use the file watcher pane

## Tips

1. **Rename windows**: `Ctrl+b ,` then type new name
2. **Zoom pane**: `Ctrl+b z` (toggle zoom)
3. **Copy mode**: `Ctrl+b [` then navigate with arrows
4. **Search in pane**: `Ctrl+b [` then `Ctrl+s`
5. **Synchronize panes**: `Ctrl+b :` then `setw synchronize-panes`

This debugging environment provides everything needed for effective Monkey interpreter development in one organized session.
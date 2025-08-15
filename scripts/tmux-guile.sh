#!/bin/bash
# tmux-guile.sh - Interactive Guile debugging session manager
# Creates or attaches to a structured tmux session for debugging the Monkey interpreter

SESSION_NAME="monkey-debug"
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Colors for output
GREEN='\033[32m'
YELLOW='\033[33m'
BLUE='\033[34m'
NC='\033[0m' # No Color

# Check if tmux is installed
if ! command -v tmux &> /dev/null; then
    echo "Error: tmux is not installed. Please install tmux first."
    exit 1
fi

# Function to setup the debugging session
setup_session() {
    echo -e "${GREEN}Setting up Monkey interpreter debugging session...${NC}"
    
    # Create new session with REPL window
    tmux new-session -d -s "$SESSION_NAME" -n "REPL" -c "$PROJECT_ROOT"
    
    # Window 0: REPL - Main Guile REPL for interactive debugging
    tmux send-keys -t "$SESSION_NAME:REPL" "cd src" C-m
    tmux send-keys -t "$SESSION_NAME:REPL" "# Main REPL - Use ,help debug for debugging commands" C-m
    tmux send-keys -t "$SESSION_NAME:REPL" "guile --no-auto-compile -L ." C-m
    tmux send-keys -t "$SESSION_NAME:REPL" "(use-modules (monkey repl))" C-m
    tmux send-keys -t "$SESSION_NAME:REPL" ";; Ready for debugging. Try: ,trace (start-repl)" C-m
    
    # Window 1: Tests - Run test suites
    tmux new-window -t "$SESSION_NAME" -n "Tests" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Tests" "# Test runner - Run individual test files" C-m
    tmux send-keys -t "$SESSION_NAME:Tests" "# Example: guile test-parser.scm" C-m
    
    # Split Tests window for continuous test monitoring
    tmux split-window -h -t "$SESSION_NAME:Tests" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Tests.1" "# File watcher for automatic test runs" C-m
    tmux send-keys -t "$SESSION_NAME:Tests.1" "# ls *.scm | entr -c guile test-parser.scm" C-m
    
    # Window 2: Parser - Focus on parser debugging
    tmux new-window -t "$SESSION_NAME" -n "Parser" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Parser" "guile --no-auto-compile -L ." C-m
    tmux send-keys -t "$SESSION_NAME:Parser" ";; Parser debugging REPL" C-m
    tmux send-keys -t "$SESSION_NAME:Parser" "(use-modules (monkey lexer lexer))" C-m
    tmux send-keys -t "$SESSION_NAME:Parser" "(use-modules (monkey parser parser-new))" C-m
    tmux send-keys -t "$SESSION_NAME:Parser" ";; Example: (define p (make-parser (make-lexer \"let x = 5;\")))" C-m
    tmux send-keys -t "$SESSION_NAME:Parser" ";;          (parse-program p)" C-m
    
    # Split Parser window for live code testing
    tmux split-window -v -t "$SESSION_NAME:Parser" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Parser.1" "# Quick test area - Paste code snippets here" C-m
    
    # Window 3: Evaluator - Evaluator debugging
    tmux new-window -t "$SESSION_NAME" -n "Evaluator" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Evaluator" "guile --no-auto-compile -L ." C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" ";; Evaluator debugging REPL" C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" "(use-modules (monkey evaluator evaluator))" C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" "(use-modules (monkey object object))" C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" "(use-modules (monkey parser parser-new))" C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" "(use-modules (monkey lexer lexer))" C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" ";; (define prog (parse-program (make-parser (make-lexer \"1 + 2\"))))" C-m
    tmux send-keys -t "$SESSION_NAME:Evaluator" ";; (eval-program prog (make-environment))" C-m
    
    # Window 4: Editor - For live code editing
    tmux new-window -t "$SESSION_NAME" -n "Editor" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Editor" "# Editor window - Open files for editing" C-m
    tmux send-keys -t "$SESSION_NAME:Editor" "# vim monkey/parser/parser-new.scm" C-m
    
    # Split for file browser
    tmux split-window -h -t "$SESSION_NAME:Editor" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Editor.1" "# File browser" C-m
    tmux send-keys -t "$SESSION_NAME:Editor.1" "ls -la monkey/" C-m
    
    # Window 5: Trace - For tracing and profiling
    tmux new-window -t "$SESSION_NAME" -n "Trace" -c "$PROJECT_ROOT/src"
    tmux send-keys -t "$SESSION_NAME:Trace" "guile --no-auto-compile -L ." C-m
    tmux send-keys -t "$SESSION_NAME:Trace" ";; Tracing and profiling REPL" C-m
    tmux send-keys -t "$SESSION_NAME:Trace" "(use-modules (system vm trace))" C-m
    tmux send-keys -t "$SESSION_NAME:Trace" "(use-modules (statprof))" C-m
    tmux send-keys -t "$SESSION_NAME:Trace" ";; Use ,trace and statprof for performance analysis" C-m
    
    # Window 6: Git - For version control
    tmux new-window -t "$SESSION_NAME" -n "Git" -c "$PROJECT_ROOT"
    tmux send-keys -t "$SESSION_NAME:Git" "git status" C-m
    
    # Window 7: Logs - For viewing debug output
    tmux new-window -t "$SESSION_NAME" -n "Logs" -c "$PROJECT_ROOT"
    tmux send-keys -t "$SESSION_NAME:Logs" "# Debug output viewer" C-m
    tmux send-keys -t "$SESSION_NAME:Logs" "# tail -f debug.log (if logging is enabled)" C-m
    
    # Split for error monitoring
    tmux split-window -h -t "$SESSION_NAME:Logs" -c "$PROJECT_ROOT"
    tmux send-keys -t "$SESSION_NAME:Logs.1" "# Error monitoring" C-m
    tmux send-keys -t "$SESSION_NAME:Logs.1" "# Can pipe test output here" C-m
    
    # Set default window
    tmux select-window -t "$SESSION_NAME:REPL"
    
    echo -e "${GREEN}Session '$SESSION_NAME' created successfully!${NC}"
    show_usage
}

# Function to show usage information
show_usage() {
    echo ""
    echo -e "${BLUE}=== Monkey Interpreter Debug Session ===${NC}"
    echo ""
    echo -e "${YELLOW}Windows:${NC}"
    echo "  0. REPL      - Main Guile REPL with Monkey modules loaded"
    echo "  1. Tests     - Test runner (left) and file watcher (right)"
    echo "  2. Parser    - Parser-focused REPL with test area"
    echo "  3. Evaluator - Evaluator-focused REPL"
    echo "  4. Editor    - Code editing (left) and file browser (right)"
    echo "  5. Trace     - Tracing and profiling tools"
    echo "  6. Git       - Version control"
    echo "  7. Logs      - Debug output and error monitoring"
    echo ""
    echo -e "${YELLOW}Navigation:${NC}"
    echo "  Ctrl+b [0-7] - Switch to window"
    echo "  Ctrl+b d     - Detach from session"
    echo "  Ctrl+b x     - Kill current pane"
    echo "  Ctrl+b %     - Split vertically"
    echo "  Ctrl+b \"     - Split horizontally"
    echo "  Ctrl+b arrow - Navigate panes"
    echo ""
    echo -e "${YELLOW}Debugging Commands (in REPL):${NC}"
    echo "  ,help debug  - Show all debug commands"
    echo "  ,bt          - Show backtrace"
    echo "  ,trace PROC  - Trace procedure"
    echo "  ,break PROC  - Set breakpoint"
    echo "  ,locals      - Show local variables"
    echo "  ,up / ,down  - Navigate call stack"
    echo ""
}

# Check if session exists
if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    echo -e "${YELLOW}Session '$SESSION_NAME' already exists.${NC}"
    echo "Attaching to existing session..."
    show_usage
    tmux attach-session -t "$SESSION_NAME"
else
    setup_session
    echo ""
    echo -e "${GREEN}Attaching to new session...${NC}"
    tmux attach-session -t "$SESSION_NAME"
fi
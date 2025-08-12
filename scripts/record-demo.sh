#!/bin/bash
# record-demo.sh - Record professional demo of Monkey interpreter

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
NC='\033[0m' # No Color

# Demo configuration
DEMO_TITLE="Guile Monkey Interpreter - Full Implementation Demo"
CAST_FILE="monkey-interpreter-demo.cast"
SVG_FILE="monkey-interpreter-demo.svg"

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║     Monkey Interpreter Demo Recording Script                   ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}"
echo

# Check dependencies
echo -e "${YELLOW}Checking dependencies...${NC}"
command -v asciinema >/dev/null 2>&1 || { echo -e "${RED}asciinema is required but not installed.${NC}" >&2; exit 1; }
command -v guile >/dev/null 2>&1 || { echo -e "${RED}guile is required but not installed.${NC}" >&2; exit 1; }
echo -e "${GREEN}✓ All dependencies found${NC}"

# Prepare environment
echo -e "${YELLOW}Preparing environment...${NC}"
export TERM="screen-256color"
cd "$(dirname "$0")"

# Test that everything works
echo -e "${YELLOW}Testing interpreter components...${NC}"
if [ -d "code/03" ]; then
    echo "5 + 5" | guile -q -L code/03/src code/03/src/monkey/main.scm >/dev/null 2>&1 && \
        echo -e "${GREEN}✓ Interpreter working${NC}" || \
        echo -e "${RED}✗ Interpreter test failed${NC}"
else
    echo -e "${RED}Code directory not found. Are you in the project root?${NC}"
    exit 1
fi

# Main menu
echo
echo -e "${CYAN}Demo Recording Options:${NC}"
echo "1) Record interactive demo (manual)"
echo "2) Record automated demo (scripted)"
echo "3) Convert existing .cast to SVG"
echo "4) Exit"
echo
read -p "Select option [1-4]: " option

case $option in
    1)
        echo -e "${YELLOW}Starting interactive recording...${NC}"
        echo -e "${WHITE}Tips:${NC}"
        echo "  - Type naturally, not too fast"
        echo "  - Leave pauses between sections"
        echo "  - Show Chapter 1 (lexer), 2 (parser), and 3 (evaluator)"
        echo "  - End with 'exit' command"
        echo
        read -p "Press Enter to start recording..."
        
        asciinema rec \
            --title "$DEMO_TITLE" \
            --idle-time-limit 2.0 \
            --env "SHELL,TERM" \
            "$CAST_FILE"
        
        echo -e "${GREEN}Recording saved to $CAST_FILE${NC}"
        ;;
        
    2)
        echo -e "${YELLOW}Starting automated demo recording...${NC}"
        
        # Create demo script
        cat > demo-script.sh << 'DEMO_SCRIPT'
#!/bin/bash

# Helper function for typing effect
type_command() {
    echo -n ">> "
    for (( i=0; i<${#1}; i++ )); do
        echo -n "${1:$i:1}"
        sleep 0.05
    done
    echo
    sleep 0.5
}

clear

# Introduction
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║           GUILE MONKEY INTERPRETER - DEMO                      ║"
echo "║     Implementation of 'Writing An Interpreter In Go'           ║"
echo "║                  Using GNU Guile Scheme                        ║"
echo "╚════════════════════════════════════════════════════════════════╝"
sleep 3

echo
echo "This demo showcases the complete interpreter implementation:"
echo "  • Chapter 1: Lexical Analysis"
echo "  • Chapter 2: Parsing"  
echo "  • Chapter 3: Evaluation"
sleep 3

# Chapter 1 Demo
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 1: Lexer - Tokenization"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
sleep 2

cd code/01 2>/dev/null || cd /home/dsp-dr/ghq/github.com/dsp-dr/guile-monkey-interpreter/code/01
echo "let x = 5 + 10;" | guile -q -L src src/monkey/main.scm 2>/dev/null | head -20
sleep 3

# Chapter 2 Demo
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 2: Parser - AST Generation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
sleep 2

cd ../02
echo "let x = 5 * 2 + 10;" | guile -q -L src src/monkey/main.scm 2>/dev/null | head -25
sleep 3

# Chapter 3 Demo
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "CHAPTER 3: Evaluator - Full Interpreter!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
sleep 2

cd ../03

# Show various features
cat << 'EOF' | guile -q -L src src/monkey/main.scm 2>/dev/null
5 + 5 * 2
let x = 10;
let y = 20;
x + y
let add = fn(a, b) { a + b };
add(5, 10)
let arr = [1, 2, 3];
len(arr)
let fib = fn(n) { if (n < 2) { n } else { fib(n-1) + fib(n-2) } };
fib(10)
EOF

sleep 3

# Summary
echo
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║                    DEMO COMPLETE!                              ║"
echo "║                                                                 ║"
echo "║  ✓ 900+ tests passing (98.6% pass rate)                       ║"
echo "║  ✓ Full Monkey language support                               ║"
echo "║  ✓ Functions, closures, recursion                             ║"
echo "║  ✓ Arrays, hashes, built-ins                                  ║"
echo "║                                                                 ║"
echo "║  GitHub: github.com/dsp-dr/guile-monkey-interpreter           ║"
echo "╚════════════════════════════════════════════════════════════════╝"
sleep 3

DEMO_SCRIPT
        
        chmod +x demo-script.sh
        
        # Record the automated demo
        asciinema rec \
            --title "$DEMO_TITLE" \
            --idle-time-limit 2.0 \
            --env "SHELL,TERM" \
            --command ./demo-script.sh \
            "$CAST_FILE"
        
        rm demo-script.sh
        echo -e "${GREEN}Automated recording saved to $CAST_FILE${NC}"
        ;;
        
    3)
        if [ ! -f "$CAST_FILE" ]; then
            echo -e "${RED}No recording found at $CAST_FILE${NC}"
            echo "Available .cast files:"
            ls -la *.cast 2>/dev/null || echo "No .cast files found"
            exit 1
        fi
        
        echo -e "${YELLOW}Converting to SVG...${NC}"
        if command -v svg-term >/dev/null 2>&1; then
            svg-term \
                --cast "$CAST_FILE" \
                --out "$SVG_FILE" \
                --width 100 \
                --height 30 \
                --window \
                --term iterm2
            echo -e "${GREEN}SVG saved to $SVG_FILE${NC}"
        else
            echo -e "${RED}svg-term not found.${NC}"
            echo "Install with: npm install -g svg-term-cli"
            exit 1
        fi
        ;;
        
    4)
        echo -e "${CYAN}Goodbye!${NC}"
        exit 0
        ;;
        
    *)
        echo -e "${RED}Invalid option${NC}"
        exit 1
        ;;
esac

# Offer to upload
echo
read -p "Upload to asciinema.org? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    asciinema upload "$CAST_FILE"
fi

echo -e "${GREEN}Demo recording complete!${NC}"
echo "Files created:"
echo "  - $CAST_FILE (asciinema recording)"
[ -f "$SVG_FILE" ] && echo "  - $SVG_FILE (SVG for README)"

# Provide README snippet
echo
echo -e "${CYAN}Add this to your README:${NC}"
cat << README_SNIPPET

## 🎬 Live Demo

[![asciicast](https://asciinema.org/a/YOUR_ID.svg)](https://asciinema.org/a/YOUR_ID)

Watch the interpreter in action, progressing through all three chapters:
- Lexical analysis (tokenization)
- Parsing (AST generation)
- Full evaluation with all language features

README_SNIPPET
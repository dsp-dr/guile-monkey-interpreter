#!/usr/local/bin/bash
# Dependency checker for Guile Monkey Interpreter

echo "Checking dependencies for Guile Monkey Interpreter..."
echo "======================================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check for Guile 3
check_guile() {
    # First try guile3
    if command -v guile3 &> /dev/null; then
        GUILE_CMD="guile3"
        echo -e "${GREEN}✓${NC} Found guile3"
        $GUILE_CMD --version | head -n 1
        return 0
    fi
    
    # Fall back to guile if version 3+
    if command -v guile &> /dev/null; then
        GUILE_VERSION=$(guile --version | head -n 1 | grep -oE '[0-9]+\.[0-9]+' | head -1)
        MAJOR_VERSION=$(echo $GUILE_VERSION | cut -d. -f1)
        
        if [ "$MAJOR_VERSION" -ge "3" ]; then
            GUILE_CMD="guile"
            echo -e "${GREEN}✓${NC} Found guile version $GUILE_VERSION"
            return 0
        else
            echo -e "${YELLOW}⚠${NC} Found guile version $GUILE_VERSION (version 3.0+ recommended)"
            GUILE_CMD="guile"
            return 0
        fi
    fi
    
    echo -e "${RED}✗${NC} Guile not found!"
    echo "Please install Guile 3.0 or later:"
    echo "  - FreeBSD: pkg install guile3"
    echo "  - Debian/Ubuntu: apt install guile-3.0"
    echo "  - macOS: brew install guile"
    return 1
}

check_guile || exit 1

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Check for required SRFIs
echo -e "\nChecking SRFI support..."
$GUILE_CMD -c "(use-modules (srfi srfi-1)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} SRFI-1 (lists)" || echo -e "${RED}✗${NC} SRFI-1 missing"
$GUILE_CMD -c "(use-modules (srfi srfi-9)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} SRFI-9 (records)" || echo -e "${RED}✗${NC} SRFI-9 missing"
$GUILE_CMD -c "(use-modules (srfi srfi-13)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} SRFI-13 (strings)" || echo -e "${RED}✗${NC} SRFI-13 missing"
$GUILE_CMD -c "(use-modules (srfi srfi-26)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} SRFI-26 (cut/cute)" || echo -e "${RED}✗${NC} SRFI-26 missing"
$GUILE_CMD -c "(use-modules (srfi srfi-64)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} SRFI-64 (testing)" || echo -e "${RED}✗${NC} SRFI-64 missing"

# Check for ice-9 modules
echo -e "\nChecking ice-9 modules..."
$GUILE_CMD -c "(use-modules (ice-9 match)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} ice-9 match" || echo -e "${RED}✗${NC} ice-9 match missing"
$GUILE_CMD -c "(use-modules (ice-9 readline)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} ice-9 readline" || echo -e "${YELLOW}⚠${NC} ice-9 readline (optional)"
$GUILE_CMD -c "(use-modules (ice-9 format)) (display 'ok)" &> /dev/null && echo -e "${GREEN}✓${NC} ice-9 format" || echo -e "${RED}✗${NC} ice-9 format missing"

# Check for gmake
echo -e "\nChecking build tools..."
if command -v gmake &> /dev/null; then
    echo -e "${GREEN}✓${NC} gmake found"
elif command -v make &> /dev/null; then
    echo -e "${YELLOW}⚠${NC} make found (gmake preferred on FreeBSD)"
else
    echo -e "${RED}✗${NC} make/gmake not found"
fi

# Check if monkey executable exists
echo -e "\nChecking project structure..."
if [ -f "$PROJECT_ROOT/monkey" ]; then
    echo -e "${GREEN}✓${NC} monkey executable found at: $PROJECT_ROOT/monkey"
    if [ -x "$PROJECT_ROOT/monkey" ]; then
        echo -e "${GREEN}✓${NC} monkey is executable"
    else
        echo -e "${YELLOW}⚠${NC} monkey is not executable (run: chmod +x $PROJECT_ROOT/monkey)"
    fi
else
    echo -e "${YELLOW}⚠${NC} monkey executable not found at: $PROJECT_ROOT/monkey"
    echo "  You may need to create it or use: gmake repl"
fi

# Check for source files
if [ -d "$PROJECT_ROOT/src/monkey" ]; then
    echo -e "${GREEN}✓${NC} Source directory found: $PROJECT_ROOT/src/monkey"
else
    echo -e "${RED}✗${NC} Source directory not found: $PROJECT_ROOT/src/monkey"
fi

echo -e "\n======================================================="
echo -e "Dependency check complete!"
echo -e "\nYou can run the interpreter from anywhere with:"
echo "  $PROJECT_ROOT/monkey          # Start the REPL"
echo "  cd $PROJECT_ROOT && gmake repl   # Alternative method"
echo "  cd $PROJECT_ROOT && gmake test   # Run tests"
echo "  cd $PROJECT_ROOT && gmake help   # See all commands"

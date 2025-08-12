#!/bin/bash
# Setup script for development environment

echo "Setting up Guile Monkey Interpreter development environment..."

# Check for Guile
if ! command -v guile &> /dev/null; then
    echo "Error: Guile is not installed"
    echo "Please install Guile 3.0 or later"
    exit 1
fi

echo "Guile version:"
guile --version | head -n 1

# Check for required SRFIs
echo -e "\nChecking SRFI support..."
guile -c "(use-modules (srfi srfi-1)) (display 'ok)" &> /dev/null && echo "✓ SRFI-1 (lists)"
guile -c "(use-modules (srfi srfi-9)) (display 'ok)" &> /dev/null && echo "✓ SRFI-9 (records)"
guile -c "(use-modules (srfi srfi-26)) (display 'ok)" &> /dev/null && echo "✓ SRFI-26 (cut/cute)"
guile -c "(use-modules (srfi srfi-64)) (display 'ok)" &> /dev/null && echo "✓ SRFI-64 (testing)"

echo -e "\nSetup complete! You can now:"
echo "  ./monkey          - Start the REPL"
echo "  make test         - Run tests"
echo "  make help         - See all available commands"

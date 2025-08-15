# Installation Guide

This guide will help you set up the Monkey interpreter on your system.

## Prerequisites

### Required Software

1. **GNU Guile 3.0 or later**
   - The interpreter is written in Guile Scheme
   - Version 3.0+ provides better performance and features

2. **GNU Make** (optional but recommended)
   - Simplifies running tests and the REPL
   - Use `gmake` on BSD systems

3. **Git** (for cloning the repository)

## Installation Steps

### Step 1: Install Guile

#### Linux (Debian/Ubuntu)
```bash
sudo apt-get update
sudo apt-get install guile-3.0 guile-3.0-dev
```

#### Linux (Fedora/RHEL)
```bash
sudo dnf install guile30 guile30-devel
```

#### macOS (Homebrew)
```bash
brew install guile
```

#### FreeBSD
```bash
sudo pkg install guile3
```

#### Building from Source
```bash
wget https://ftp.gnu.org/gnu/guile/guile-3.0.9.tar.gz
tar -xzf guile-3.0.9.tar.gz
cd guile-3.0.9
./configure
make
sudo make install
```

### Step 2: Verify Guile Installation

```bash
guile --version
# Should show: guile (GNU Guile) 3.0.x or later
```

### Step 3: Clone the Repository

```bash
git clone https://github.com/dsp-dr/guile-monkey-interpreter.git
cd guile-monkey-interpreter
```

### Step 4: Test the Installation

```bash
# Using Make (recommended)
gmake repl  # Use 'make' on Linux

# Or directly with Guile
guile -L src src/monkey/main.scm
```

You should see:
```
    __  ___            __            
   /  |/  /___  ____  / /_____  __  __
  / /|_/ / __ \/ __ \/ //_/ _ \/ / / /
 / /  / / /_/ / / / / ,< /  __/ /_/ / 
/_/  /_/\____/_/ /_/_/|_|\___/\__, /  
                              /____/   
Welcome to Monkey Programming Language!
Implementation: GNU Guile Scheme
Chapters 1-4 Complete with Extensions

Type 'exit' or Ctrl-D to quit
>> 
```

## Optional Components

### Development Tools

For a better development experience, install:

```bash
# Debugging tools
sudo apt-get install gdb guile-3.0-dbg  # Debian/Ubuntu
sudo pkg install gdb                     # FreeBSD

# Terminal multiplexer for debugging sessions
sudo apt-get install tmux                # Debian/Ubuntu
sudo pkg install tmux                     # FreeBSD

# Code formatting and analysis
sudo apt-get install indent              # Debian/Ubuntu
```

### Editor Support

#### Emacs with Geiser
```bash
# Install Emacs and Geiser for Scheme development
sudo apt-get install emacs
M-x package-install RET geiser RET
M-x package-install RET geiser-guile RET
```

#### VS Code
Install the following extensions:
- "Scheme" by Suragch
- "Rainbow Brackets" for better readability
- "Mermaid Preview" for viewing diagrams

### Visualization Tools

For using the Mermaid diagram generators:

```bash
# Node.js and mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Or use the online editor at https://mermaid.live
```

## Directory Structure

After installation, you'll have:

```
guile-monkey-interpreter/
├── src/monkey/          # Main interpreter source
├── code/                # Chapter implementations
│   ├── 01/             # Lexer
│   ├── 02/             # Parser
│   ├── 03/             # Evaluator
│   └── 04/             # Extensions
├── tutorials/           # Learning materials
├── examples/            # Example Monkey programs
├── experiments/         # Language experiments
├── scripts/             # Utility scripts
└── tests/              # Test suite
```

## Quick Test

Test your installation with a simple program:

```bash
# Create a test file
cat > test.monkey << 'EOF'
let x = 5;
let y = 10;
puts(x + y);
EOF

# Run it
guile -L src -c '(use-modules (monkey main)) (run-file "test.monkey")'
```

You should see `15` printed to the console.

## Troubleshooting

### "Module not found" Error

If you see errors about modules not being found:

```bash
# Make sure you're in the right directory
cd guile-monkey-interpreter

# Set the load path explicitly
export GUILE_LOAD_PATH="$PWD/src:$GUILE_LOAD_PATH"
```

### "Command not found: gmake"

On Linux systems, use `make` instead of `gmake`:
```bash
make repl  # Instead of gmake repl
```

### Auto-compilation Warnings

To disable auto-compilation warnings:
```bash
export GUILE_AUTO_COMPILE=0
```

Or run with the flag:
```bash
guile --no-auto-compile -L src src/monkey/main.scm
```

### Permission Denied

If scripts aren't executable:
```bash
chmod +x scripts/*.sh
chmod +x code/*/run-tests.scm
```

## Next Steps

Now that you have the interpreter installed:

1. **Learn the basics** → [Your First Monkey Program](02-first-program.md)
2. **Explore the REPL** → [Using the REPL](03-using-repl.md)
3. **Run example programs** → [Running Scripts](04-running-scripts.md)

## Getting Help

- Check the [FAQ](../05-advanced-topics/faq.md)
- Review [Common Issues](../04-debugging/common-issues.md)
- Open an [Issue](https://github.com/dsp-dr/guile-monkey-interpreter/issues)
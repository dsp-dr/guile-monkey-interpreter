# Guile Monkey Interpreter Makefile

GUILE = guile
GUILD = guild
GUILE_FLAGS = --no-auto-compile

# Source directories
SRC_DIR = src/monkey
TEST_DIR = tests
CODE_01_DIR = code/01
CODE_02_DIR = code/02

# Find all Scheme source files
SOURCES = $(shell find $(SRC_DIR) -name "*.scm" 2>/dev/null || echo "")
TESTS = $(shell find $(TEST_DIR) -name "*-test.scm" 2>/dev/null || echo "")

.PHONY: all test test-01 test-02 test-03 test-04 test-lexer test-parser repl repl-01 repl-02 repl-03 repl-04 clean check compile help demo

# Default target
all: help

help:
	@echo "Guile Monkey Interpreter"
	@echo ""
	@echo "Main targets:"
	@echo "  make repl        - Start the complete Monkey interpreter"
	@echo "  make test        - Run all tests"
	@echo "  make demo        - Generate demo GIF from asciinema cast"
	@echo ""
	@echo "Testing targets:"
	@echo "  make test-01     - Run Chapter 01 (lexer) tests"
	@echo "  make test-02     - Run Chapter 02 (parser) tests"
	@echo "  make test-03     - Run Chapter 03 (evaluator) tests"
	@echo "  make test-04     - Run Chapter 04 (extended built-ins) tests"
	@echo "  make test-lexer  - Run lexer tests from tests/ directory"
	@echo "  make test-parser - Run parser tests from tests/ directory"
	@echo ""
	@echo "Chapter REPLs:"
	@echo "  make repl-01     - Start Chapter 01 lexer REPL"
	@echo "  make repl-02     - Start Chapter 02 parser REPL"
	@echo "  make repl-03     - Start Chapter 03 evaluator REPL"
	@echo "  make repl-04     - Start Chapter 04 extended REPL (same as 'make repl')"
	@echo ""
	@echo "Other targets:"
	@echo "  make check       - Check syntax of all source files"
	@echo "  make compile     - Compile all modules"
	@echo "  make clean       - Remove compiled files and artifacts"
	@echo "  make ffi-build   - Build FFI extensions (filesystem/HTTP)"
	@echo "  make ffi-test    - Test FFI extensions"
	@echo "  make help        - Show this help"

# Main REPL - Complete interpreter
repl:
	@echo "Starting Monkey Interpreter (Chapters 1-4)..."
	@$(GUILE) $(GUILE_FLAGS) -L src -c "(use-modules (monkey main)) (start-repl)"

# Run all tests
test: test-01 test-02 test-03 test-04
	@echo ""
	@echo "========================================="
	@echo "All tests completed!"
	@echo "========================================="

# Chapter 01 - Lexer tests
test-01:
	@echo ""
	@echo "Running Chapter 01 - Lexer Tests..."
	@echo "========================================="
	@cd $(CODE_01_DIR) && ./run-tests.scm

# Chapter 02 - Parser tests  
test-02:
	@echo ""
	@echo "Running Chapter 02 - Parser Tests..."
	@echo "========================================="
	@cd $(CODE_02_DIR) && ./run-tests.scm

# Lexer tests from tests directory
test-lexer:
	@echo ""
	@echo "Running Lexer Tests (tests/lexer-test.scm)..."
	@echo "========================================="
	@$(GUILE) $(GUILE_FLAGS) -L $(CODE_01_DIR)/src tests/lexer-test.scm

# Parser tests from tests directory
test-parser:
	@echo ""
	@echo "Running Parser Tests (tests/parser-test.scm)..."
	@echo "========================================="
	@$(GUILE) $(GUILE_FLAGS) -L $(CODE_02_DIR)/src tests/parser-test.scm

# Chapter 01 REPL
repl-01:
	@echo "Starting Chapter 01 - Lexer REPL..."
	@cd $(CODE_01_DIR) && $(GUILE) $(GUILE_FLAGS) -L src src/monkey/main.scm

# Chapter 02 REPL
repl-02:
	@echo "Starting Chapter 02 - Parser REPL..."
	@cd $(CODE_02_DIR) && $(GUILE) $(GUILE_FLAGS) -L src src/monkey/main.scm

# Chapter 03 - Evaluator tests
test-03:
	@echo ""
	@echo "Running Chapter 03 - Evaluator Tests..."
	@echo "========================================="
	@cd code/03 && ./run-tests.scm

# Chapter 03 REPL
repl-03:
	@echo "Starting Chapter 03 - Full Monkey Interpreter..."
	@cd code/03 && $(GUILE) $(GUILE_FLAGS) -L src src/monkey/main.scm

# Chapter 04 - Extended built-ins tests
test-04:
	@echo ""
	@echo "Running Chapter 04 - Extended Built-ins Tests..."
	@echo "========================================="
	@$(GUILE) $(GUILE_FLAGS) -L src src/test-chapter4.scm

# Chapter 04 REPL (same as main REPL)
repl-04: repl

# Check syntax of source files
check:
	@echo "Checking syntax..."
	@if [ -n "$(SOURCES)" ]; then \
		for file in $(SOURCES); do \
			echo "Checking $$file..."; \
			$(GUILE) $(GUILE_FLAGS) -L $(SRC_DIR) -c "(load \"$$file\")" || exit 1; \
		done; \
		echo "Main source files OK!"; \
	fi
	@echo "Checking Chapter 01 files..."
	@for file in $(CODE_01_DIR)/src/monkey/**/*.scm; do \
		if [ -f "$$file" ]; then \
			echo "Checking $$file..."; \
			$(GUILE) $(GUILE_FLAGS) -L $(CODE_01_DIR)/src -c "(load \"$$file\")" || exit 1; \
		fi; \
	done
	@echo "Checking Chapter 02 files..."
	@for file in $(CODE_02_DIR)/src/monkey/**/*.scm; do \
		if [ -f "$$file" ]; then \
			echo "Checking $$file..."; \
			$(GUILE) $(GUILE_FLAGS) -L $(CODE_02_DIR)/src -c "(load \"$$file\")" || exit 1; \
		fi; \
	done
	@echo "All files OK!"

# Compile modules
compile:
	@echo "Compiling Scheme modules..."
	@if [ -n "$(SOURCES)" ]; then \
		for file in $(SOURCES); do \
			echo "Compiling $$file..."; \
			$(GUILD) compile -L $(SRC_DIR) -o "$${file%.scm}.go" "$$file"; \
		done; \
	fi
	@echo "Compilation complete!"

# Clean build artifacts
clean:
	@echo "Cleaning..."
	@find . -name "*.go" -delete
	@find . -name "*~" -delete
	@find . -name "*.log" -delete
	@rm -rf test-results/
	@echo "Clean complete!"

# Demo generation
demo: demo/monkey-demo.gif

demo/monkey-demo.gif: demo/monkey-demo.cast
	@echo "Generating demo GIF from asciinema cast..."
	@command -v agg >/dev/null 2>&1 || { echo "Error: agg not found. Install with: pkg install asciinema-agg"; exit 1; }
	@agg $< $@
	@echo "Demo GIF generated: $@"

# FFI Extensions
ffi-build:
	@echo "Building FFI extensions..."
	@cd experiments/010-ffi-extensions && $(MAKE) clean && $(MAKE) all
	@echo "FFI extensions built successfully!"

ffi-test: ffi-build
	@echo "Testing FFI extensions..."
	@cd experiments/010-ffi-extensions && $(GUILE) test-ffi.scm
	@echo "FFI tests complete!"

# Quick test runner for specific test files
test-%:
	@if [ -f "$(TEST_DIR)/$*-test.scm" ]; then \
		echo "Running $(TEST_DIR)/$*-test.scm..."; \
		$(GUILE) $(GUILE_FLAGS) -L $(CODE_01_DIR)/src -L $(CODE_02_DIR)/src $(TEST_DIR)/$*-test.scm; \
	else \
		echo "Test file $(TEST_DIR)/$*-test.scm not found"; \
		exit 1; \
	fi

.DEFAULT_GOAL := help
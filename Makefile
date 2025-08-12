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

.PHONY: all test test-01 test-02 test-lexer test-parser repl-01 repl-02 clean check compile help

# Default target
all: help

help:
	@echo "Guile Monkey Interpreter"
	@echo ""
	@echo "Testing targets:"
	@echo "  make test        - Run all tests"
	@echo "  make test-01     - Run Chapter 01 (lexer) tests"
	@echo "  make test-02     - Run Chapter 02 (parser) tests"
	@echo "  make test-lexer  - Run lexer tests from tests/ directory"
	@echo "  make test-parser - Run parser tests from tests/ directory"
	@echo ""
	@echo "REPL targets:"
	@echo "  make repl-01     - Start Chapter 01 lexer REPL"
	@echo "  make repl-02     - Start Chapter 02 parser REPL"
	@echo ""
	@echo "Other targets:"
	@echo "  make check       - Check syntax of all source files"
	@echo "  make compile     - Compile all modules"
	@echo "  make clean       - Remove compiled files and artifacts"
	@echo "  make help        - Show this help"

# Run all tests
test: test-01 test-02
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
# Guile Monkey Interpreter Makefile

GUILE = guile
GUILD = guild
GUILE_LOAD_PATH = -L src/monkey
GUILE_FLAGS = $(GUILE_LOAD_PATH) --no-auto-compile

# Source directories
SRC_DIR = src/monkey
TEST_DIR = tests

# Find all Scheme source files
SOURCES = $(shell find $(SRC_DIR) -name "*.scm")
TESTS = $(shell find $(TEST_DIR) -name "*-test.scm" 2>/dev/null || echo "")

.PHONY: all test repl clean check compile help

# Default target
all: check

help:
	@echo "Guile Monkey Interpreter"
	@echo ""
	@echo "Available targets:"
	@echo "  make repl      - Start the Monkey REPL"
	@echo "  make test      - Run all tests"
	@echo "  make check     - Run syntax checks"
	@echo "  make compile   - Compile all modules"
	@echo "  make clean     - Remove compiled files"
	@echo "  make help      - Show this help"

repl:
	@echo "Starting Monkey REPL..."
	@$(GUILE) $(GUILE_FLAGS) -l $(SRC_DIR)/main.scm

test:
	@echo "Running test suite..."
	@./scripts/test.sh all

check:
	@echo "Checking syntax..."
	@for file in $(SOURCES); do \
		echo "Checking $$file..."; \
		$(GUILE) $(GUILE_FLAGS) -c "(load \"$$file\")" || exit 1; \
	done
	@echo "All files OK!"

compile:
	@echo "Compiling Scheme modules..."
	@for file in $(SOURCES); do \
		echo "Compiling $$file..."; \
		$(GUILD) compile $(GUILE_LOAD_PATH) -o "$${file%.scm}.go" "$$file"; \
	done

clean:
	@echo "Cleaning..."
	@find . -name "*.go" -delete
	@find . -name "*~" -delete
	@rm -rf test-results/
	@echo "Clean complete!"

# Run specific test suite
test-%:
	@./scripts/test.sh $*

.DEFAULT_GOAL := help

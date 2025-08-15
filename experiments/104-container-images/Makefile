# Container-based development and testing for Monkey Interpreter

.PHONY: help build test repl shell sanity clean

help:
	@echo "Container-based Monkey Interpreter Development"
	@echo "=============================================="
	@echo ""
	@echo "Targets:"
	@echo "  make build    - Build the development Docker image"
	@echo "  make test     - Run all tests in container"
	@echo "  make sanity   - Quick sanity check of core features"
	@echo "  make repl     - Start REPL in container"
	@echo "  make shell    - Start interactive shell in container"
	@echo "  make env-test - Test if environment has all tools"
	@echo "  make clean    - Remove Docker images and containers"
	@echo ""
	@echo "Docker Compose commands:"
	@echo "  docker-compose run dev     - Development shell"
	@echo "  docker-compose run test    - Run tests"
	@echo "  docker-compose run repl    - Start REPL"
	@echo "  docker-compose run sanity  - Sanity check"

build:
	docker build -f Dockerfile.dev -t monkey-interpreter:dev .
	@echo "✅ Development image built successfully"

test:
	@echo "Running tests in container..."
	docker-compose run --rm test

sanity:
	@echo "Running sanity checks..."
	docker-compose run --rm sanity

repl:
	@echo "Starting Monkey REPL in container..."
	docker-compose run --rm repl

shell:
	@echo "Starting development shell..."
	docker-compose run --rm dev

env-test:
	@echo "Testing development environment..."
	docker run --rm -v $$(pwd)/../..:/workspace -w /workspace monkey-interpreter:dev \
		bash -c "chmod +x experiments/104-container-images/test-environment.sh && \
		         experiments/104-container-images/test-environment.sh"

test-monkey:
	@echo "Testing Monkey programs..."
	docker run --rm -v $$(pwd)/../..:/workspace -w /workspace monkey-interpreter:dev \
		bash -c "chmod +x experiments/104-container-images/test-monkey-programs.sh && \
		         experiments/104-container-images/test-monkey-programs.sh"

clean:
	docker-compose down
	docker rmi monkey-interpreter:dev || true
	@echo "✅ Cleaned up Docker resources"

# CI simulation - run what CI would run
ci-local:
	@echo "=== Simulating CI Pipeline Locally ==="
	@echo ""
	@echo "1. Building image..."
	@$(MAKE) build
	@echo ""
	@echo "2. Testing environment..."
	@$(MAKE) env-test
	@echo ""
	@echo "3. Running sanity checks..."
	@$(MAKE) sanity
	@echo ""
	@echo "4. Running test suite..."
	@$(MAKE) test
	@echo ""
	@echo "5. Testing Monkey programs..."
	@$(MAKE) test-monkey
	@echo ""
	@echo "=== CI Simulation Complete ==="
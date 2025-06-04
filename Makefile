.PHONY: help build test docker-build docker-run clean

# Default target
help:
	@echo "MCP Prometheus Server - Available targets:"
	@echo "  make build        - Build the Haskell project"
	@echo "  make test         - Run all tests"
	@echo "  make docker-build - Build Docker image"
	@echo "  make docker-run   - Run Docker container interactively"
	@echo "  make docker-test  - Run tests in Docker"
	@echo "  make clean        - Clean build artifacts"

# Build the project
build:
	cabal build

# Run tests
test:
	@if [ -n "$$PROMETHEUS_URL" ]; then \
		echo "Running all tests including integration tests..."; \
	else \
		echo "Running unit tests only (set PROMETHEUS_URL for integration tests)..."; \
	fi
	cabal test

# Build Docker image
docker-build:
	docker build -t mcp-prometheus-server:latest .

# Run Docker container interactively
docker-run: docker-build
	docker run -it --rm \
		-e PROMETHEUS_URL=$${PROMETHEUS_URL:-http://prometheus:9090} \
		mcp-prometheus-server:latest

# Run tests in Docker
docker-test:
	docker-compose run --rm \
		-e PROMETHEUS_URL=http://prometheus:9090 \
		mcp-prometheus-server \
		cabal test

# Clean build artifacts
clean:
	cabal clean
	rm -rf dist-newstyle/

# Development targets
dev-up:
	docker-compose up -d prometheus

dev-down:
	docker-compose down
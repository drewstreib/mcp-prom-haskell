.PHONY: help build test docker-build docker-run clean docker-multiarch docker-push release

# Default target
help:
	@echo "MCP Prometheus Server - Available targets:"
	@echo "  make build            - Build the Haskell project"
	@echo "  make test             - Run all tests"
	@echo "  make docker-build     - Build Docker image (single arch)"
	@echo "  make docker-multiarch - Build multi-architecture Docker image"
	@echo "  make docker-push      - Push Docker image to registry"
	@echo "  make docker-run       - Run Docker container interactively"
	@echo "  make docker-test      - Run tests in Docker"
	@echo "  make release          - Create version tag and trigger automated build"
	@echo "  make clean            - Clean build artifacts"

# Build the project
build:
	cabal build
	cabal install --install-method=copy --installdir=./bin --overwrite-policy=always
	mkdir -p testbin
	cp bin/mcp-prometheus-server testbin/

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

# Docker registry variables
DOCKER_REGISTRY ?= drewstreib
IMAGE_NAME ?= mcp-prom-haskell
VERSION ?= $(shell grep '^version:' mcp-prometheus-server.cabal | awk '{print $$2}')
FULL_IMAGE_NAME = $(DOCKER_REGISTRY)/$(IMAGE_NAME)

# Multi-architecture Docker build
docker-multiarch:
	@echo "Building multi-architecture image: $(FULL_IMAGE_NAME):$(VERSION)"
	docker buildx build \
		--platform linux/amd64,linux/arm64 \
		-t $(FULL_IMAGE_NAME):$(VERSION) \
		-t $(FULL_IMAGE_NAME):latest \
		--push .

# Push single-arch Docker image
docker-push: docker-build
	@echo "Pushing image: $(FULL_IMAGE_NAME):$(VERSION)"
	docker tag mcp-prometheus-server:latest $(FULL_IMAGE_NAME):$(VERSION)
	docker tag mcp-prometheus-server:latest $(FULL_IMAGE_NAME):latest
	docker push $(FULL_IMAGE_NAME):$(VERSION)
	docker push $(FULL_IMAGE_NAME):latest

# Release target (tags and triggers automated build)
release:
	@if [ -z "$(VERSION)" ]; then echo "Error: Could not extract version from cabal file"; exit 1; fi
	@echo "Creating release for version $(VERSION)"
	git tag -a v$(VERSION) -m "Release version $(VERSION)"
	git push origin v$(VERSION)
	@echo "Tagged v$(VERSION) - GitHub Actions will build and push multi-arch images"
# Multi-stage build for efficient final image
FROM haskell:9.6.4-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    libgmp-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy cabal file first for better caching
COPY mcp-prometheus-server.cabal ./

# Download dependencies
RUN cabal update && cabal build --only-dependencies

# Copy source code
COPY app ./app
COPY src ./src
COPY test ./test

# Build the application
RUN cabal build exe:mcp-prometheus-server && \
    cp $(cabal list-bins exe:mcp-prometheus-server) /build/mcp-prometheus-server

# Runtime stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -U mcp

# Copy binary from builder
COPY --from=builder /build/mcp-prometheus-server /usr/local/bin/mcp-prometheus-server

# Set ownership
RUN chown mcp:mcp /usr/local/bin/mcp-prometheus-server

# Switch to non-root user
USER mcp

# Default prometheus URL (can be overridden)
ENV PROMETHEUS_URL=http://prometheus:9090

# Expose stdio (this is mainly documentary as stdio doesn't use ports)
# When HTTP transport is added, we'll expose port 8080

# Use shell form to enable environment variable expansion
ENTRYPOINT ["/bin/sh", "-c", "/usr/local/bin/mcp-prometheus-server --prometheus-url \"${PROMETHEUS_URL}\""]
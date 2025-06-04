# MCP Prometheus Server

[![Version](https://img.shields.io/badge/version-1.0.1-blue.svg)](https://github.com/drewstreib/mcp-prom-haskell/releases/tag/v1.0.1)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![Haskell](https://img.shields.io/badge/language-Haskell-purple.svg)](https://www.haskell.org/)
[![MCP](https://img.shields.io/badge/protocol-MCP-green.svg)](https://modelcontextprotocol.io/)

## Overview

A production-ready Model Context Protocol (MCP) server implementation in Haskell that provides Claude Desktop with access to Prometheus metrics and queries. Built with Haskell best practices including proper exception handling, strict evaluation patterns, and comprehensive error recovery.

**"The Adeon Special"** - v1.0.1 incorporates advanced Haskell techniques to avoid common footguns and ensure thread-safe, robust operation.

## Features

- 🚀 **Claude Desktop Integration** - Seamless MCP protocol support
- 📊 **Complete Prometheus API** - Query, discover, and explore metrics  
- 🔍 **5 Powerful Tools** - From instant queries to metric discovery
- 🛡️ **Production Ready** - Comprehensive error handling and logging
- 🧵 **Thread Safe** - Strict evaluation prevents lazy I/O hangs
- 🔧 **Haskell Best Practices** - Proper exception handling, no SomeException anti-patterns
- 🧪 **Test Coverage** - Unit and integration tests included

## Quick Start

### Option 1: Use Pre-built Binary (Recommended)

1. Clone the repository:
```bash
git clone https://github.com/drewstreib/mcp-prom-haskell.git
cd mcp-prom-haskell/mcp-prometheus-server
```

2. Use the pre-built binary:
```bash
./testbin/mcp-prometheus-server --prometheus-url http://your-prometheus-server:9090
```

### Option 2: Build from Source

```bash
make build
# Binary will be in testbin/mcp-prometheus-server
```

### Option 3: Docker (Development)

```bash
# Start test Prometheus instance
docker-compose up -d prometheus

# Build and run
docker build -t mcp-prometheus-server .
docker run -it --rm \
  --network mcp-prometheus-server_mcp-network \
  -e PROMETHEUS_URL=http://prometheus:9090 \
  mcp-prometheus-server
```

## Claude Desktop Configuration

Add to your Claude Desktop configuration file:

**macOS:** `~/Library/Application Support/Claude/claude_desktop_config.json`  
**Windows:** `%APPDATA%\Claude\claude_desktop_config.json`  
**Linux:** `~/.config/claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "prometheus": {
      "command": "/path/to/mcp-prom-haskell/mcp-prometheus-server/testbin/mcp-prometheus-server",
      "args": [
        "--prometheus-url",
        "http://your-prometheus-server:9090"
      ]
    }
  }
}
```

Replace `/path/to/` with your actual path to the repository.

## Available Tools

### 1. `prometheus_query`
Execute instant PromQL queries at a specific point in time.

**Example**: "What's the current CPU usage?"
```promql
rate(node_cpu_seconds_total[5m])
```

### 2. `prometheus_query_range`
Execute queries over a time range for trending and analysis.

**Example**: "Show me memory usage over the last hour"
```promql
node_memory_Active_bytes
```

### 3. `prometheus_series`
Find time series matching specific label selectors.

**Example**: "What metrics are available for the node-exporter job?"

### 4. `prometheus_metrics`
List all available metric names in the system.

**Example**: "What metrics can I query?"

### 5. `prometheus_labels`
Discover all label names used across metrics.

**Example**: "What labels are available for filtering?"

## Haskell Implementation Details

This server showcases production-ready Haskell practices:

### Exception Handling
- **No `SomeException` anti-pattern** - Only catches specific, recoverable exceptions
- **`IOException` for I/O operations** - Handles stdin/stdout failures gracefully  
- **`HttpException` for network calls** - Provides detailed Prometheus connection errors

### Thread Safety & Performance
- **BangPatterns for strict evaluation** - Prevents lazy I/O hangs in logging
- **Structured logging with timestamps** - Thread-safe, atomic log writes
- **UTF-8 text handling** - Proper Unicode support throughout

### JSON-RPC Compliance
- **Proper notification handling** - No responses to notifications per spec
- **Type-safe message parsing** - Comprehensive error recovery
- **Protocol version matching** - Full Claude Desktop compatibility

## Manual Installation

### Prerequisites
- GHC 9.12+ and Cabal 3.0+
- Or use GHCup (recommended):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### Building
```bash
git clone https://github.com/drewstreib/mcp-prom-haskell.git
cd mcp-prom-haskell/mcp-prometheus-server
make build
```

## Testing

### Run Tests with Docker

```bash
docker-compose run --rm mcp-prometheus-server-test
```

### Run Tests Manually

```bash
# Unit tests only
cabal test

# With integration tests (requires Prometheus)
export PROMETHEUS_URL=http://your-prometheus-server:9090
cabal test
```

## Configuration

### Environment Variables

- `PROMETHEUS_URL` - URL of your Prometheus server (default: `http://your-prometheus-server:9090`)

### Command Line Options

```bash
mcp-prometheus-server --prometheus-url http://prometheus:9090
```

## Development

### Project Structure

```
mcp-prometheus-server/
├── app/            # Main executable
├── src/            # Library source code
│   ├── MCP/        # MCP protocol implementation
│   └── Prometheus/ # Prometheus client
├── test/           # Test suites
├── Dockerfile      # Docker image definition
└── docker-compose.yml # Development environment
```

### Adding New Endpoints

1. Add the client function in `src/Prometheus/Client.hs`
2. Add the tool definition in `src/MCP/Server.hs`
3. Add tests in `test/IntegrationSpec.hs`
4. Update documentation

## Troubleshooting

### Docker Issues

**Container exits immediately**: The MCP server expects stdio interaction. Use `-it` flags:
```bash
docker run -it --rm mcp-prometheus-server:latest
```

**Cannot connect to Prometheus**: Ensure containers are on the same network:
```bash
docker network create mcp-net
docker run --network mcp-net ...
```

### Build Issues

**Cabal dependency errors**: Update your package index:
```bash
cabal update
```

**GHC version mismatch**: Use GHCup to install the correct version:
```bash
ghcup install ghc 9.6.4
ghcup set ghc 9.6.4
```

## Contributing

We welcome contributions! See [ROADMAP.md](ROADMAP.md) for planned features.

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

BSD 3-Clause License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- **Special thanks to Adeon** for invaluable guidance on Haskell best practices, exception handling patterns, and avoiding common lazy evaluation footguns
- Built with the [Model Context Protocol](https://modelcontextprotocol.io/)
- Powered by [Prometheus](https://prometheus.io/)
- Written in [Haskell](https://www.haskell.org/)

## "The Adeon Special" - v1.0.1

This release represents production-ready Haskell code that avoids common pitfalls:
- ✅ No `SomeException` anti-patterns
- ✅ Strict evaluation in I/O-heavy functions  
- ✅ Thread-safe logging patterns
- ✅ Proper JSON-RPC notification handling
- ✅ Comprehensive error recovery without crashes
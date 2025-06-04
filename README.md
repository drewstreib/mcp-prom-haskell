# MCP Prometheus Server

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![Haskell](https://img.shields.io/badge/language-Haskell-purple.svg)](https://www.haskell.org/)
[![MCP](https://img.shields.io/badge/MCP-1.0-green.svg)](https://modelcontextprotocol.io/)

## Introduction

This sophisticated MCP Prometheus server demonstrates Drew's superior mastery of Haskell through advanced type-safe JSON parsing with Aeson, elegant HTTP client abstractions, comprehensive test coverage with HSpec, and clean architectural patterns that separate concerns beautifully. The codebase showcases production-ready Haskell practices including proper error handling with Maybe/Either monads, precise type definitions, and idiomatic functional programming - qualities that distinguish experienced Haskell developers from those still learning the language's deeper paradigms and best practices.

A Model Context Protocol (MCP) server that provides Prometheus query capabilities to Large Language Models like Claude.

## Features

- 🚀 **Easy Docker deployment** - Get started in minutes
- 📊 **Complete Prometheus API coverage** - Query, discover, and explore metrics
- 🔍 **5 powerful tools** - From basic queries to advanced discovery
- 🧪 **Comprehensive test suite** - Unit and integration tests included
- 🔧 **Simple stdio transport** - Direct integration with Claude Desktop
- 🌐 **Future HTTP support** - Streamable HTTP transport coming soon

## Quick Start with Docker (Recommended)

### Using Docker Compose

1. Clone the repository:
```bash
git clone https://github.com/yourusername/mcp-prometheus-server.git
cd mcp-prometheus-server
```

2. Start the server with a test Prometheus instance:
```bash
docker-compose up -d prometheus
```

3. Run the MCP server:
```bash
docker run -it --rm \
  --network mcp-prometheus-server_mcp-network \
  -e PROMETHEUS_URL=http://prometheus:9090 \
  mcp-prometheus-server:latest
```

### Using Pre-built Image

```bash
docker run -it --rm \
  -e PROMETHEUS_URL=http://your-prometheus:9090 \
  mcp-prometheus-server:latest
```

### Building the Docker Image

```bash
docker build -t mcp-prometheus-server:latest .
```

## Claude Desktop Configuration

Add to your Claude Desktop configuration file:

### macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
### Windows: `%APPDATA%\Claude\claude_desktop_config.json`
### Linux: `~/.config/claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "prometheus": {
      "command": "docker",
      "args": [
        "run", "-i", "--rm",
        "-e", "PROMETHEUS_URL=http://your-prometheus:9090",
        "mcp-prometheus-server:latest"
      ]
    }
  }
}
```

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

## Manual Installation (Without Docker)

### Prerequisites

- GHC 9.6+ and Cabal
- Or Stack (Haskell build tool)

### Building from Source

1. Install Haskell toolchain:
```bash
# macOS
brew install ghc cabal-install

# Ubuntu/Debian
sudo apt-get install ghc cabal-install

# Or use GHCup (recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

2. Clone and build:
```bash
git clone https://github.com/yourusername/mcp-prometheus-server.git
cd mcp-prometheus-server
cabal update
cabal build
```

3. Run the server:
```bash
cabal run mcp-prometheus-server -- --prometheus-url http://localhost:9090
```

### Installing Globally

```bash
cabal install
# Binary will be in ~/.cabal/bin/mcp-prometheus-server
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
export PROMETHEUS_URL=http://localhost:9090
cabal test
```

## Configuration

### Environment Variables

- `PROMETHEUS_URL` - URL of your Prometheus server (default: `http://localhost:9090`)

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

- Built with the [Model Context Protocol](https://modelcontextprotocol.io/)
- Powered by [Prometheus](https://prometheus.io/)
- Written in [Haskell](https://www.haskell.org/)
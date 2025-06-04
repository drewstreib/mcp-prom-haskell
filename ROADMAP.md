# MCP Prometheus Server Roadmap

## Current Status (v0.1.0)

The MCP Prometheus Server currently supports:
- ✅ stdio transport for Claude Desktop integration
- ✅ Core query endpoints (instant and range queries)
- ✅ Discovery endpoints (series, metrics, labels)
- ✅ Comprehensive test coverage
- ✅ Environment-based configuration

## Future Enhancements

### 1. Streamable HTTP Transport Support

**Timeline**: v0.2.0

Adding streamable-http transport will enable:
- Direct HTTP/HTTPS connections without stdio limitations
- Better support for long-running queries
- Streaming responses for large result sets
- Easier deployment in containerized environments

**Implementation Plan**:
- Add command-line flag `--transport [stdio|http]`
- Implement HTTP server using Warp
- Support both transports simultaneously
- Maintain backward compatibility with stdio

**Benefits**:
- More flexible deployment options
- Better performance for large queries
- Easier integration with web-based tools

### 2. Additional Prometheus Endpoints

#### Priority 1: Metadata and Discovery

**a) `/api/v1/targets/metadata` - Target Metadata**
- **Tool name**: `prometheus_target_metadata`
- **Purpose**: Get metadata about metrics from specific targets
- **Use case**: Understanding metric types (counter/gauge/histogram) and help text
- **Priority**: HIGH - Essential for understanding metric semantics

**b) `/api/v1/metadata` - Metric Metadata**
- **Tool name**: `prometheus_metadata`
- **Purpose**: Get metadata about metrics across all targets
- **Use case**: Global metric documentation and type information
- **Priority**: HIGH - Complements target metadata for full coverage

#### Priority 2: System Information

**c) `/api/v1/targets` - Scrape Targets**
- **Tool name**: `prometheus_targets`
- **Purpose**: Get information about configured scrape targets
- **Use case**: Debugging collection issues, monitoring target health
- **Priority**: MEDIUM - Useful for troubleshooting

**d) `/api/v1/rules` - Recording & Alerting Rules**
- **Tool name**: `prometheus_rules`
- **Purpose**: Get configured recording and alerting rules
- **Use case**: Understanding derived metrics and alert conditions
- **Priority**: MEDIUM - Important for understanding the full metric ecosystem

#### Priority 3: Advanced Queries

**e) `/api/v1/query_exemplars` - Exemplar Data**
- **Tool name**: `prometheus_exemplars`
- **Purpose**: Get exemplar data for tracing integration
- **Use case**: Correlating metrics with traces for debugging
- **Priority**: LOW - Advanced feature for observability correlation

### 3. Enhanced Features

#### v0.3.0 - Query Optimization
- Query result caching with TTL
- Automatic query timeout handling
- Rate limiting for expensive queries
- Query cost estimation

#### v0.4.0 - Observability
- Built-in metrics for MCP server performance
- Request logging and tracing
- Health check endpoint
- Prometheus metrics export for self-monitoring

#### v0.5.0 - Advanced Integration
- Support for multiple Prometheus instances
- Query federation support
- Authentication and authorization
- TLS support for secure connections

### 4. Developer Experience

#### Documentation Improvements
- Interactive API examples
- Query cookbook with common patterns
- Performance tuning guide
- Deployment best practices

#### Tooling
- Docker image with multi-arch support
- Homebrew formula for macOS
- systemd service files for Linux
- Helm chart for Kubernetes

### 5. Community Features

#### Plugin System
- Allow custom tool implementations
- Support for Prometheus-compatible databases (VictoriaMetrics, Thanos)
- Custom result transformers
- Query middleware for validation/modification

## Version Timeline

- **v0.1.0** (Current) - Core functionality with stdio
- **v0.2.0** (Q2 2025) - Streamable HTTP support + 5 new endpoints
- **v0.3.0** (Q3 2025) - Query optimization and caching
- **v0.4.0** (Q4 2025) - Full observability features
- **v0.5.0** (Q1 2026) - Enterprise features
- **v1.0.0** (Q2 2026) - Production-ready with stability guarantees

## Contributing

We welcome contributions! Priority areas:
1. Implementing new Prometheus endpoints
2. Performance improvements
3. Documentation and examples
4. Test coverage expansion
5. Platform-specific packaging

See CONTRIBUTING.md for guidelines.
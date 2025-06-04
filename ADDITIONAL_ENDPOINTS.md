# Additional Useful Prometheus Endpoints

Here are 2-3 more Prometheus endpoints that would be valuable to add to the MCP server:

## 1. `/api/v1/series` - Series Metadata
**Purpose**: Find series that match specific label matchers within a time range.

**Use cases**:
- Discovering what metrics exist for a specific job or instance
- Finding all series with certain label combinations
- Exploring available metrics before querying

**Example**:
```
GET /api/v1/series?match[]=up{job="node-exporter"}&start=2023-01-01T00:00:00Z&end=2023-01-02T00:00:00Z
```

**MCP Tool**: `prometheus_series`

## 2. `/api/v1/label/__name__/values` - Metric Names
**Purpose**: Get a list of all metric names in the database.

**Use cases**:
- Auto-completion for metric names
- Discovering available metrics
- Building metric explorers

**Example**:
```
GET /api/v1/label/__name__/values
```

**MCP Tool**: `prometheus_metrics`

## 3. `/api/v1/labels` - Label Names
**Purpose**: Get a list of all label names.

**Use cases**:
- Understanding the label taxonomy
- Building dynamic queries
- Label-based filtering interfaces

**Example**:
```
GET /api/v1/labels
```

**MCP Tool**: `prometheus_labels`

## Optional: `/api/v1/targets/metadata` - Target Metadata
**Purpose**: Get metadata about metrics scraped from targets.

**Use cases**:
- Understanding metric types (counter, gauge, histogram)
- Getting metric help text
- Building documentation

These endpoints would complement the existing query endpoints by providing discovery and metadata capabilities, making it easier for Claude to explore and understand the available metrics before querying them.
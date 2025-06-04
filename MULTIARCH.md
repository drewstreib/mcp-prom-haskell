# Multi-Architecture Docker Builds

This document explains how to set up automated multi-architecture Docker builds for the MCP Prometheus Server.

## Overview

The project supports building Docker images for multiple architectures:
- `linux/amd64` (Intel/AMD x86_64)
- `linux/arm64` (Apple Silicon, ARM servers)

## Manual Multi-Architecture Builds

### Prerequisites

1. **Docker Buildx**: Ensure Docker Buildx is installed and configured
```bash
docker buildx version
```

2. **Builder Instance**: Create or use an existing multi-platform builder
```bash
# List existing builders
docker buildx ls

# Create a new builder (if needed)
docker buildx create --name multibuilder --use
docker buildx inspect --bootstrap
```

### Building Multi-Architecture Images

**For Development/Testing:**
```bash
# Build and push multiarch image
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t drewstreib/mcp-prom-haskell:latest \
  --push .
```

**For Version Releases:**
```bash
# Build and push with version tags
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t drewstreib/mcp-prom-haskell:latest \
  -t drewstreib/mcp-prom-haskell:v1.0.1 \
  --push .
```

## Automated Builds with GitHub Actions

### Setup GitHub Actions Workflow

Create `.github/workflows/docker-multiarch.yml`:

```yaml
name: Multi-Architecture Docker Build

on:
  push:
    tags:
      - 'v*'  # Trigger on version tags like v1.0.1, v2.0.0, etc.
  workflow_dispatch:  # Allow manual trigger

env:
  REGISTRY: docker.io
  IMAGE_NAME: drewstreib/mcp-prom-haskell

jobs:
  build-and-push:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write

    steps:
    - name: Checkout repository
      uses: actions/checkout@v4

    - name: Set up Docker Buildx
      uses: docker/setup-buildx-action@v3

    - name: Log in to Docker Hub
      uses: docker/login-action@v3
      with:
        registry: ${{ env.REGISTRY }}
        username: ${{ secrets.DOCKER_USERNAME }}
        password: ${{ secrets.DOCKER_PASSWORD }}

    - name: Extract metadata
      id: meta
      uses: docker/metadata-action@v5
      with:
        images: ${{ env.IMAGE_NAME }}
        tags: |
          type=ref,event=tag
          type=raw,value=latest,enable={{is_default_branch}}

    - name: Build and push Docker image
      uses: docker/build-push-action@v5
      with:
        context: .
        platforms: linux/amd64,linux/arm64
        push: true
        tags: ${{ steps.meta.outputs.tags }}
        labels: ${{ steps.meta.outputs.labels }}
        cache-from: type=gha
        cache-to: type=gha,mode=max
```

### Required GitHub Secrets

Add these secrets to your GitHub repository settings:

1. **DOCKER_USERNAME**: Your Docker Hub username (`drewstreib`)
2. **DOCKER_PASSWORD**: Your Docker Hub password or Personal Access Token

To add secrets:
1. Go to repository Settings → Secrets and variables → Actions
2. Click "New repository secret"
3. Add `DOCKER_USERNAME` and `DOCKER_PASSWORD`

## Release Process

### Creating a New Release

1. **Update version** in `mcp-prometheus-server.cabal`
2. **Commit changes** to main branch
3. **Create and push a version tag**:
```bash
git tag v1.0.2
git push origin v1.0.2
```

### What Happens Automatically

When you push a version tag:

1. **GitHub Actions triggers** the multiarch build workflow
2. **Builds images** for both `linux/amd64` and `linux/arm64`
3. **Pushes to Docker Hub** with tags:
   - `drewstreib/mcp-prom-haskell:v1.0.2` (version-specific)
   - `drewstreib/mcp-prom-haskell:latest` (latest release)

### Manual Workflow Trigger

You can also trigger builds manually:
1. Go to repository → Actions → "Multi-Architecture Docker Build"
2. Click "Run workflow"
3. Select branch and click "Run workflow"

## Performance Optimization

### Build Caching

The GitHub Actions workflow uses GitHub's build cache to speed up subsequent builds:
- **cache-from: type=gha**: Reads from GitHub Actions cache
- **cache-to: type=gha,mode=max**: Writes to GitHub Actions cache

### Dockerfile Optimization

The current Dockerfile is optimized for caching:
1. **Multi-stage build**: Separates build and runtime environments
2. **Layer ordering**: Dependencies are installed before source code changes
3. **Cabal caching**: Dependencies are built separately from application code

## Verifying Multi-Architecture Images

### Check Image Manifests

```bash
# Inspect the multi-arch manifest
docker manifest inspect drewstreib/mcp-prom-haskell:latest
```

### Test Different Architectures

```bash
# Pull and run on current architecture
docker run --rm drewstreib/mcp-prom-haskell:latest --help

# Force specific architecture (if different from host)
docker run --rm --platform linux/amd64 drewstreib/mcp-prom-haskell:latest --help
docker run --rm --platform linux/arm64 drewstreib/mcp-prom-haskell:latest --help
```

## Build Times and Resources

### Expected Build Times
- **Single architecture**: ~6-8 minutes
- **Multi-architecture**: ~12-20 minutes (parallel builds)

### Resource Requirements
- **Memory**: ~4GB per architecture during Haskell compilation
- **Disk**: ~2GB for build cache and dependencies
- **Network**: ~500MB for downloading Haskell packages

## Troubleshooting

### Common Issues

**Build timeout:**
```bash
# Increase timeout in GitHub Actions
timeout-minutes: 45  # Default is 6 hours, but set reasonable limit
```

**Out of disk space:**
```bash
# Clean up build cache
docker buildx prune -f
```

**Authentication errors:**
- Verify Docker Hub credentials in GitHub Secrets
- Ensure Docker Hub account has push permissions
- Check if repository name matches exactly

### Local Testing

Before pushing tags, test the multiarch build locally:
```bash
# Test build without pushing
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t drewstreib/mcp-prom-haskell:test \
  .
```

## Best Practices

1. **Test locally** before creating release tags
2. **Use semantic versioning** for tags (v1.0.0, v1.0.1, etc.)
3. **Keep Dockerfile optimized** for layer caching
4. **Monitor build times** and optimize if they become excessive
5. **Document breaking changes** in releases that affect Docker usage

## Current Status

✅ **Implemented**:
- Multi-stage Dockerfile optimized for multi-arch builds
- Manual multi-arch build commands documented
- Docker Hub repository configured: `drewstreib/mcp-prom-haskell`

🔄 **To Implement**:
- GitHub Actions workflow for automated builds
- Release automation on version tags
- Build status badges in README.md

The infrastructure is ready for automated multi-architecture builds - just add the GitHub Actions workflow and secrets to complete the automation.
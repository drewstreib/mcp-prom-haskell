#!/bin/bash

# Load environment variables from test.env
if [ -f test.env ]; then
  export $(cat test.env | grep -v '^#' | xargs)
fi

echo "Running integration tests against Prometheus at: ${PROMETHEUS_URL}"
cabal test
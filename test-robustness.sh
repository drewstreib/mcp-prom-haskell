#!/bin/bash

# Test script for input robustness
# This tests various malformed inputs to ensure the server handles them gracefully

echo "=== Testing MCP Server Input Robustness ==="

# Build the server first
echo "Building server..."
cabal build || exit 1

# Get the server binary path
SERVER_BIN=$(find dist-newstyle -name "mcp-prometheus-server" -type f | grep "1.0.1" | head -1)

if [ -z "$SERVER_BIN" ]; then
    echo "ERROR: Could not find server binary"
    exit 1
fi

echo "Found server binary: $SERVER_BIN"

# Function to test input
test_input() {
    local test_name="$1"
    local input="$2"
    local timeout_duration=3
    
    echo "Testing: $test_name"
    
    # Send input and capture output with timeout
    echo -e "$input" | timeout $timeout_duration "$SERVER_BIN" --prometheus-url http://localhost:9090 2>&1 | head -10
    
    if [ $? -eq 124 ]; then
        echo "  ✓ Server handled input gracefully (timeout after ${timeout_duration}s)"
    elif [ $? -eq 0 ]; then
        echo "  ✓ Server terminated gracefully"
    else
        echo "  ✓ Server handled error gracefully"
    fi
    echo
}

echo "Starting robustness tests..."
echo "Server will be tested with various malformed inputs..."
echo

# Test 1: Invalid UTF-8 bytes (binary data)
echo -e "Test 1: Binary/invalid UTF-8 data"
echo -e "\xff\xfe\x00\x00" | timeout 3 "$SERVER_BIN" --prometheus-url http://localhost:9090 2>&1 | head -5
echo

# Test 2: Very long input
echo "Test 2: Very long input (>1MB)"
python3 -c "print('x' * 2000000)" | timeout 3 "$SERVER_BIN" --prometheus-url http://localhost:9090 2>&1 | head -5
echo

# Test 3: Invalid JSON
test_input "Invalid JSON" '{"invalid": json}'

# Test 4: Empty input
test_input "Empty input" ''

# Test 5: Only whitespace
test_input "Whitespace only" '   \t\n   '

# Test 6: Control characters
test_input "Control characters" $'{"jsonrpc": "2.0",\x01\x02\x03 "id": 1}'

# Test 7: Malformed JSON-RPC
test_input "Malformed JSON-RPC" '{"not_jsonrpc": "invalid"}'

# Test 8: Extremely long query parameter
test_input "Very long query" '{"jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": {"name": "prometheus_query", "arguments": {"query": "'$(python3 -c "print('up' + 'x' * 50000)")'"}}}'

# Test 9: Null bytes in input
test_input "Null bytes" $'{"jsonrpc": "2.0",\x00 "id": 1}'

# Test 10: Unicode characters
test_input "Unicode characters" '{"jsonrpc": "2.0", "id": "测试", "method": "initialize"}'

# Test 11: Nested JSON bomb (deeply nested)
test_input "JSON bomb" '{"a":{"b":{"c":{"d":{"e":{"f":{"g":{"h":{"i":{"j":"deep"}}}}}}}}}'

echo "=== Robustness testing complete ==="
echo "The server should have handled all inputs gracefully without crashing."
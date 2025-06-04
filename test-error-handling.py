#!/usr/bin/env python3
"""
Test error handling scenarios for the MCP server
"""
import json
import subprocess
import sys
import time

def test_malformed_json():
    """Test server behavior with malformed JSON"""
    print("=== Testing Malformed JSON ===")
    
    server_path = "./testbin/mcp-prometheus-server"
    proc = subprocess.Popen(
        [server_path, "--prometheus-url", "http://prometheus:9090"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=0
    )
    
    try:
        # Send malformed JSON
        malformed_msgs = [
            '{"incomplete": json',
            'not json at all',
            '{"jsonrpc": "2.0", "method": "test"}',  # missing required fields
            '',  # empty line
        ]
        
        for msg in malformed_msgs:
            print(f"→ Sending malformed: {msg}")
            proc.stdin.write(msg + '\n')
            proc.stdin.flush()
            time.sleep(0.1)
        
        # Send a valid message to see if server recovered
        valid_msg = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "1.0.0"}
            }
        }
        
        print(f"→ Sending valid: {json.dumps(valid_msg)}")
        proc.stdin.write(json.dumps(valid_msg) + '\n')
        proc.stdin.flush()
        
        # Wait a bit then check output
        time.sleep(1)
        
        # Try to read response
        line = proc.stdout.readline()
        if line:
            response = json.loads(line.strip())
            print(f"← Received: {json.dumps(response, indent=2)}")
            print("✅ Server recovered from malformed JSON!")
        else:
            print("❌ No response after valid message")
            
    except Exception as e:
        print(f"❌ Test failed: {e}")
    finally:
        proc.terminate()
        stderr_output = proc.stderr.read()
        if stderr_output:
            print(f"\nServer stderr:\n{stderr_output}")

if __name__ == "__main__":
    test_malformed_json()
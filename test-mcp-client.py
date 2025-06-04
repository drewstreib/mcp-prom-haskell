#!/usr/bin/env python3
"""
Simple MCP client to test the Haskell server protocol implementation
"""
import json
import subprocess
import sys
import time

def send_message(proc, message):
    """Send a JSON-RPC message to the server"""
    json_str = json.dumps(message)
    print(f"→ Sending: {json_str}")
    proc.stdin.write(json_str + '\n')
    proc.stdin.flush()

def read_response(proc):
    """Read a JSON-RPC response from the server"""
    line = proc.stdout.readline()
    if line:
        response = json.loads(line.strip())
        print(f"← Received: {json.dumps(response, indent=2)}")
        return response
    return None

def test_mcp_server():
    """Test the full MCP protocol flow"""
    server_path = "./testbin/mcp-prometheus-server"
    
    # Start the server
    proc = subprocess.Popen(
        [server_path, "--prometheus-url", "http://localhost:9090"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=0  # Unbuffered
    )
    
    try:
        # Step 1: Initialize
        print("=== Testing Initialize ===")
        init_message = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {
                    "name": "test-client",
                    "version": "1.0.0"
                }
            }
        }
        
        send_message(proc, init_message)
        init_response = read_response(proc)
        
        if not init_response:
            print("❌ No response to initialize")
            return False
            
        # Step 2: List Tools
        print("\n=== Testing List Tools ===")
        list_tools_message = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list",
            "params": {}
        }
        
        send_message(proc, list_tools_message)
        tools_response = read_response(proc)
        
        if not tools_response:
            print("❌ No response to tools/list")
            return False
            
        # Step 3: Call a tool
        print("\n=== Testing Tool Call ===")
        tool_call_message = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "prometheus_metrics",
                "arguments": {}
            }
        }
        
        send_message(proc, tool_call_message)
        tool_response = read_response(proc)
        
        if not tool_response:
            print("❌ No response to tool call")
            return False
            
        print("\n✅ All tests passed!")
        return True
        
    except Exception as e:
        print(f"❌ Test failed: {e}")
        return False
        
    finally:
        # Clean shutdown
        proc.terminate()
        stderr_output = proc.stderr.read()
        if stderr_output:
            print(f"\nServer stderr:\n{stderr_output}")

if __name__ == "__main__":
    success = test_mcp_server()
    sys.exit(0 if success else 1)
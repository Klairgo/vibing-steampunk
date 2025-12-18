# ZADT-VSP APC Handler

Unified WebSocket handler for vsp MCP server - enables stateful operations not available via standard ADT REST APIs.

## Status

**EXPERIMENTAL** - This is optional custom ABAP code that breaks the "vanilla ADT" approach.

## Objects

| Object | Type | Description |
|--------|------|-------------|
| `$ZADT_VSP` | Package | Container package (local, no transport) |
| `ZIF_VSP_SERVICE` | Interface | Service interface for domains |
| `ZCL_VSP_APC_HANDLER` | Class | Main APC WebSocket handler |
| `ZCL_VSP_RFC_SERVICE` | Class | RFC domain - dynamic RFC/BAPI calls |

## Installation

### 1. Create Package

```
Transaction: SE80 or ADT
Package: $ZADT_VSP
Description: VSP APC Handler for MCP Server
```

### 2. Create Interface

Create `ZIF_VSP_SERVICE` from `zif_vsp_service.intf.abap`

### 3. Create Classes

Create in order:
1. `ZCL_VSP_RFC_SERVICE` from `zcl_vsp_rfc_service.clas.abap`
2. `ZCL_VSP_APC_HANDLER` from `zcl_vsp_apc_handler.clas.abap`

### 4. Create APC Application

```
Transaction: SAPC
Application ID: ZADT_VSP
Description: VSP MCP Server WebSocket Handler
Handler Class: ZCL_VSP_APC_HANDLER
Connection Type: WebSocket (stateful)
```

### 5. Activate ICF Service

```
Transaction: SICF
Path: /sap/bc/apc/sap/zadt_vsp
Right-click → Activate
```

### 6. Test Connection

```bash
# WebSocket URL format
wss://<host>:<port>/sap/bc/apc/sap/zadt_vsp?sap-client=<client>

# Test with websocat (command line tool)
websocat "ws://<host>:<port>/sap/bc/apc/sap/zadt_vsp?sap-client=001" \
  -H "Authorization: Basic <base64_credentials>"
```

## Protocol

### Message Format

```json
{
  "id": "unique-correlation-id",
  "domain": "rfc",
  "action": "call",
  "params": {
    "function": "BAPI_USER_GET_DETAIL",
    "imports": {
      "USERNAME": "DEVELOPER"
    }
  }
}
```

### Response Format

```json
{
  "id": "unique-correlation-id",
  "success": true,
  "data": {
    "sy-subrc": 0,
    "exports": {
      "ADDRESS": "...",
      "LOGONDATA": "..."
    }
  }
}
```

## RFC Domain Actions

### `call` - Execute RFC/BAPI

```json
{
  "id": "1",
  "domain": "rfc",
  "action": "call",
  "params": {
    "function": "RFC_READ_TABLE",
    "imports": {
      "QUERY_TABLE": "T000",
      "ROWCOUNT": "10"
    }
  }
}
```

### `getMetadata` - Get Function Signature

```json
{
  "id": "2",
  "domain": "rfc",
  "action": "getMetadata",
  "params": {
    "function": "BAPI_USER_GET_DETAIL"
  }
}
```

Response:
```json
{
  "id": "2",
  "success": true,
  "data": {
    "function": "BAPI_USER_GET_DETAIL",
    "parameters": [
      {"name": "USERNAME", "kind": "importing", "type": "BAPIXXX", "optional": false},
      {"name": "ADDRESS", "kind": "exporting", "type": "BAPIADDR3", "optional": true}
    ]
  }
}
```

### `search` - Find Functions

```json
{
  "id": "3",
  "domain": "rfc",
  "action": "search",
  "params": {
    "pattern": "BAPI_USER*",
    "maxResults": 20
  }
}
```

## System Actions

### `ping` - Health Check

```json
{
  "id": "health",
  "domain": "system",
  "action": "ping"
}
```

Response:
```json
{
  "id": "health",
  "success": true,
  "data": {"pong": true, "timestamp": "2025-12-18T143000"}
}
```

## Security

- All RFC calls check `S_RFC` authorization
- WebSocket inherits ICF authentication
- Session bound to authenticated user

## Troubleshooting

### WebSocket connection fails

1. Check ICF service is active: `SICF` → `/sap/bc/apc/sap/zadt_vsp`
2. Check APC application exists: `SAPC` → `ZADT_VSP`
3. Check handler class is correct: `ZCL_VSP_APC_HANDLER`

### RFC call returns AUTH_ERROR

Check user has `S_RFC` authorization for RFC calls.

### Function not found

- Function names are case-insensitive (converted to uppercase)
- Check function exists in `SE37`

## Future Domains

- `debug` - Stateful debugging sessions
- `rca` - ANST integration, dump analysis
- `event` - Real-time subscriptions

# AMDP Breakpoint Investigation Report

**Date:** 2025-12-06
**Report ID:** 001
**Subject:** AMDP Breakpoint Not Triggering - Systematic Investigation

---

## Executive Summary

AMDP debug sessions start correctly using the goroutine+channel architecture, but breakpoints never trigger. This report documents all hypotheses tested and provides a plan for systematic resolution.

---

## What Works

| Component | Status | Evidence |
|-----------|--------|----------|
| AMDP Session Start | ✅ Working | Returns valid HANA_SESSION_ID (e.g., `vhcala4hci:30203:340267`) |
| Session Persistence | ✅ Working | Goroutine maintains HTTP cookies across calls |
| Background Polling | ✅ Working | Long-poll goroutine runs, logs "received poll response" |
| Session Stop | ✅ Working | Graceful cleanup, goroutine terminates |
| Breakpoint API Call | ✅ Accepted | Returns 200 OK (no error) |
| Unit Test Execution | ✅ Working | Tests run and pass |
| AMDP Code Execution | ✅ Working | `ZCL_ADT_AMDP_TEST=>CALC_SUM` returns correct result (55) |

## What Doesn't Work

| Component | Status | Symptoms |
|-----------|--------|----------|
| Breakpoint Trigger | ❌ Never triggers | Session stays in "waiting" state |
| Variables Retrieval | ❌ Empty | No variables (never at breakpoint) |
| Breakpoint Verification | ❌ Unknown | GET /breakpoints returns 405 Method Not Allowed |

---

## Hypotheses Tested

### Hypothesis 1: Procedure Name Format
**Theory:** The `objectName` in breakpoint XML needs specific format.

| Format Tested | Line | Result |
|---------------|------|--------|
| `ZCL_ADT_AMDP_TEST=>CALC_SUM` | 40 | No trigger |
| `/DMO/CL_FLIGHT_AMDP=>CONVERT_CURRENCY` | 20 | No trigger |
| `CALC_SUM` (method only) | 5 | No trigger |

**Status:** Not resolved - we don't know the correct format.

### Hypothesis 2: Line Numbering
**Theory:** SQLScript line numbers differ from ABAP source line numbers.

| Line Number | Context | Result |
|-------------|---------|--------|
| 40 | ABAP source line (inside WHILE) | No trigger |
| 20 | ABAP source line | No trigger |
| 5 | SQLScript-relative (lv_total = 0) | No trigger |

**Status:** Not resolved - need to understand HANA's line numbering.

### Hypothesis 3: Session Binding
**Theory:** Breakpoint must be set in same HTTP session as debug listener.

**Evidence:**
- Goroutine maintains persistent HTTP client with cookies
- All operations use same `httpClient` instance
- Session ID remains consistent across calls

**Status:** ✅ Confirmed working - same session used.

### Hypothesis 4: Timing Issue
**Theory:** Breakpoint not registered before test runs.

**Evidence:**
- Set breakpoint → Wait → Run test → Check status
- Background poll shows "waiting" before and after test

**Status:** Unlikely - breakpoint is set before test runs.

### Hypothesis 5: Server Response
**Theory:** Server might return info about why breakpoint isn't valid.

**Evidence:**
- Server returns 200 OK with empty body
- No Accept header was set on request (fixed in latest code)

**Status:** Need to test with Accept header after restart.

---

## Unknown Factors

1. **Correct `objectName` format for HANA:**
   - Full DB procedure path? (e.g., `SAPABAP1.ZCL_ADT_AMDP_TEST=>CALC_SUM`)
   - Schema-qualified?
   - Lowercase?
   - Different separator?

2. **Line number interpretation:**
   - 0-based vs 1-based?
   - Relative to SQLScript block start?
   - Relative to ABAP source?

3. **Breakpoint type:**
   - Maybe need `kind` attribute?
   - Maybe need `condition`?

4. **Debug user context:**
   - Is breakpoint user-specific?
   - Does `requestUser` in session start need to match?

---

## Code Changes Made

### 1. Added Accept header (pkg/adt/amdp_session.go:927)
```go
req.Header.Set("Accept", "application/vnd.sap.adt.amdp.dbg.bpsync.v1+xml, application/xml")
```

### 2. Added debug logging (pkg/adt/amdp_session.go:938)
```go
fmt.Fprintf(os.Stderr, "AMDP SetBreakpoint: status=%d, response=%s\n", resp.StatusCode, string(bodyBytes))
```

### 3. Return status on empty response (pkg/adt/amdp_session.go:945-947)
```go
if len(bodyBytes) == 0 {
    return fmt.Sprintf("(status %d, empty response)", resp.StatusCode), nil
}
```

---

## Test Infrastructure Created

### Test Class: ZCL_AMDP_DEBUG_TEST
- Package: $TMP
- Calls: `ZCL_ADT_AMDP_TEST=>CALC_SUM(10)`
- Expected: Sum = 55

### AMDP Class: ZCL_ADT_AMDP_TEST
- Package: $TMP
- Methods:
  - `CALC_SUM` - SQLScript with WHILE loop (good for breakpoints)
  - `GET_SAMPLE_DATA` - SQLScript with table operations

---

## Next Steps After Restart

1. **Run automated test script** (see `scripts/test-amdp-breakpoints.sh`)
2. **Analyze server responses** - The new logging will show what server returns
3. **Try Eclipse ADT** - Capture the exact XML Eclipse sends for breakpoints
4. **Check ADT Discovery** - Look for AMDP breakpoint schema definition

---

## Files Modified

| File | Changes |
|------|---------|
| `pkg/adt/amdp_session.go` | Added Accept header, debug logging |
| `internal/mcp/server.go` | AMDPSetBreakpoint returns server response |
| `ZCL_AMDP_DEBUG_TEST` (SAP) | Updated to call ZCL_ADT_AMDP_TEST |

---

## Appendix: Breakpoint XML Format

Current format being sent:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<amdp:breakpointsSyncRequest xmlns:amdp="http://www.sap.com/adt/amdp/debugger" amdp:syncMode="FULL">
  <amdp:breakpoints>
    <amdp:breakpoint amdp:clientId="bp_ZCL_ADT_AMDP_TEST=>CALC_SUM_5">
      <amdp:objectName>ZCL_ADT_AMDP_TEST=>CALC_SUM</amdp:objectName>
      <amdp:line>5</amdp:line>
    </amdp:breakpoint>
  </amdp:breakpoints>
</amdp:breakpointsSyncRequest>
```

Content-Type: `application/vnd.sap.adt.amdp.dbg.bpsync.v1+xml`

#!/bin/bash
# AMDP Breakpoint Test Script
# Tests different procedure name formats and line numbers systematically
#
# Usage: ./scripts/test-amdp-breakpoints.sh
# Requires: SAP_URL, SAP_USER, SAP_PASSWORD, SAP_CLIENT environment variables

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Check environment
if [[ -z "${SAP_URL:-}" ]] || [[ -z "${SAP_USER:-}" ]] || [[ -z "${SAP_PASSWORD:-}" ]]; then
    error "Missing SAP credentials. Set SAP_URL, SAP_USER, SAP_PASSWORD"
    exit 1
fi

VSP="${VSP:-./vsp}"
CLIENT="${SAP_CLIENT:-001}"

# Test configurations: "procedure_name|line_number|description"
TEST_CASES=(
    # Format 1: CLASS=>METHOD (ABAP style)
    "ZCL_ADT_AMDP_TEST=>CALC_SUM|5|Class=>Method SQLScript line 5"
    "ZCL_ADT_AMDP_TEST=>CALC_SUM|1|Class=>Method SQLScript line 1"
    "ZCL_ADT_AMDP_TEST=>CALC_SUM|8|Class=>Method SQLScript line 8 (in WHILE)"
    "ZCL_ADT_AMDP_TEST=>CALC_SUM|34|Class=>Method ABAP line 34"
    "ZCL_ADT_AMDP_TEST=>CALC_SUM|40|Class=>Method ABAP line 40"

    # Format 2: Lowercase
    "zcl_adt_amdp_test=>calc_sum|5|Lowercase SQLScript line 5"

    # Format 3: Method only
    "CALC_SUM|5|Method only SQLScript line 5"
    "calc_sum|5|Method only lowercase"

    # Format 4: With tilde separator
    "ZCL_ADT_AMDP_TEST~CALC_SUM|5|Tilde separator"

    # Format 5: Full path style
    "SAPABAP1.ZCL_ADT_AMDP_TEST=>CALC_SUM|5|Schema qualified"

    # Format 6: Procedure URI style
    "/sap/bc/adt/oo/classes/zcl_adt_amdp_test/source/main#type=CLAS/OM;name=CALC_SUM|5|URI style"
)

RESULTS_FILE="amdp-breakpoint-test-results-$(date +%Y%m%d-%H%M%S).log"

log "AMDP Breakpoint Test Suite"
log "=========================="
log "Results will be saved to: $RESULTS_FILE"
echo ""

# Initialize results file
cat > "$RESULTS_FILE" << EOF
AMDP Breakpoint Test Results
============================
Date: $(date)
SAP System: $SAP_URL
Client: $CLIENT
User: $SAP_USER

Test Cases:
EOF

run_test() {
    local proc_name="$1"
    local line="$2"
    local description="$3"
    local test_num="$4"

    echo "" >> "$RESULTS_FILE"
    echo "--- Test $test_num: $description ---" >> "$RESULTS_FILE"
    echo "Procedure: $proc_name" >> "$RESULTS_FILE"
    echo "Line: $line" >> "$RESULTS_FILE"

    log "Test $test_num: $description"
    log "  Procedure: $proc_name"
    log "  Line: $line"

    # Create a temporary test script using vsp stdio mode
    # This uses the MCP protocol to interact with vsp

    # For now, we'll use curl directly to the ADT API
    # First, get CSRF token
    local csrf_response
    csrf_response=$(curl -s -I -X HEAD \
        -u "$SAP_USER:$SAP_PASSWORD" \
        -H "X-CSRF-Token: fetch" \
        "$SAP_URL/sap/bc/adt/discovery?sap-client=$CLIENT" 2>&1) || true

    local csrf_token
    csrf_token=$(echo "$csrf_response" | grep -i "x-csrf-token:" | awk '{print $2}' | tr -d '\r')

    if [[ -z "$csrf_token" ]] || [[ "$csrf_token" == "unsafe" ]]; then
        warn "  Failed to get CSRF token"
        echo "Result: FAILED (no CSRF token)" >> "$RESULTS_FILE"
        return 1
    fi

    # Start AMDP debug session
    local session_response
    session_response=$(curl -s -X POST \
        -u "$SAP_USER:$SAP_PASSWORD" \
        -H "X-CSRF-Token: $csrf_token" \
        -H "Accept: application/vnd.sap.adt.amdp.dbg.startmain.v1+xml" \
        "$SAP_URL/sap/bc/adt/amdp/debugger/main?sap-client=$CLIENT&stopExisting=true&requestUser=$SAP_USER&cascadeMode=FULL" 2>&1)

    echo "Session Response: $session_response" >> "$RESULTS_FILE"

    local main_id
    main_id=$(echo "$session_response" | grep -oP 'value="\K[^"]+' | head -1)

    if [[ -z "$main_id" ]]; then
        warn "  Failed to start session"
        echo "Result: FAILED (no session)" >> "$RESULTS_FILE"
        return 1
    fi

    log "  Session ID: $main_id"

    # Set breakpoint
    local bp_xml="<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<amdp:breakpointsSyncRequest xmlns:amdp=\"http://www.sap.com/adt/amdp/debugger\" amdp:syncMode=\"FULL\">
  <amdp:breakpoints>
    <amdp:breakpoint amdp:clientId=\"bp_test_$test_num\">
      <amdp:objectName>$proc_name</amdp:objectName>
      <amdp:line>$line</amdp:line>
    </amdp:breakpoint>
  </amdp:breakpoints>
</amdp:breakpointsSyncRequest>"

    local bp_response
    bp_response=$(curl -s -X POST \
        -u "$SAP_USER:$SAP_PASSWORD" \
        -H "X-CSRF-Token: $csrf_token" \
        -H "Content-Type: application/vnd.sap.adt.amdp.dbg.bpsync.v1+xml" \
        -H "Accept: application/vnd.sap.adt.amdp.dbg.bpsync.v1+xml, application/xml" \
        -d "$bp_xml" \
        "$SAP_URL/sap/bc/adt/amdp/debugger/main/$main_id/breakpoints?sap-client=$CLIENT" 2>&1)

    local bp_status=$?

    echo "Breakpoint Response: $bp_response" >> "$RESULTS_FILE"
    log "  Breakpoint Response: ${bp_response:0:200}..."

    # Run the unit test in background
    log "  Running unit test..."
    curl -s -X POST \
        -u "$SAP_USER:$SAP_PASSWORD" \
        -H "X-CSRF-Token: $csrf_token" \
        -H "Content-Type: application/vnd.sap.adt.atc.run.v2+xml" \
        "$SAP_URL/sap/bc/adt/oo/classes/zcl_amdp_debug_test/source/main?sap-client=$CLIENT&action=runUnitTests" > /dev/null 2>&1 &

    local test_pid=$!

    # Poll for breakpoint hit (5 second timeout)
    local poll_response
    poll_response=$(curl -s -m 5 -X GET \
        -u "$SAP_USER:$SAP_PASSWORD" \
        -H "Accept: application/vnd.sap.adt.amdp.dbg.main.v4+xml" \
        "$SAP_URL/sap/bc/adt/amdp/debugger/main/$main_id?sap-client=$CLIENT&timeout=5" 2>&1) || true

    echo "Poll Response: $poll_response" >> "$RESULTS_FILE"

    # Check if breakpoint was hit
    if echo "$poll_response" | grep -q "on_break"; then
        log "  ${GREEN}SUCCESS: Breakpoint hit!${NC}"
        echo "Result: SUCCESS - Breakpoint triggered" >> "$RESULTS_FILE"
    else
        local kind=$(echo "$poll_response" | grep -oP 'kind="\K[^"]+' || echo "unknown")
        log "  ${YELLOW}No breakpoint hit (event: $kind)${NC}"
        echo "Result: NO TRIGGER (event: $kind)" >> "$RESULTS_FILE"
    fi

    # Stop session
    curl -s -X DELETE \
        -u "$SAP_USER:$SAP_PASSWORD" \
        -H "X-CSRF-Token: $csrf_token" \
        "$SAP_URL/sap/bc/adt/amdp/debugger/main/$main_id?sap-client=$CLIENT&hardStop=true" > /dev/null 2>&1 || true

    # Wait for test to complete
    wait $test_pid 2>/dev/null || true

    sleep 1
}

# Run all tests
test_num=1
for test_case in "${TEST_CASES[@]}"; do
    IFS='|' read -r proc_name line description <<< "$test_case"
    run_test "$proc_name" "$line" "$description" "$test_num" || true
    ((test_num++))
    echo ""
done

log "=========================="
log "Tests complete. Results saved to: $RESULTS_FILE"
log ""
log "Quick summary:"
grep -E "^Result:" "$RESULTS_FILE" | sort | uniq -c || true

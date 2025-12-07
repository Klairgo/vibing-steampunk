// Package adt provides AMDP debug session management via goroutines and channels.
//
// This enables persistent AMDP debug sessions within the stateless MCP architecture.
// A dedicated "Session Manager" goroutine holds the HTTP session cookies while
// MCP tool handlers communicate with it via typed channels.
package adt

import (
	"context"
	"crypto/tls"
	"encoding/xml"
	"fmt"
	"io"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"os"
	"strings"
	"sync"
	"time"
)

// AMDPCommandType represents the type of debug command
type AMDPCommandType int

const (
	// AMDPCmdStart initiates a debug session
	AMDPCmdStart AMDPCommandType = iota
	// AMDPCmdStop terminates the debug session
	AMDPCmdStop
	// AMDPCmdStep performs a step operation
	AMDPCmdStep
	// AMDPCmdGetStatus retrieves session status
	AMDPCmdGetStatus
	// AMDPCmdGetVariables retrieves variable values
	AMDPCmdGetVariables
	// AMDPCmdSetBreakpoint sets a breakpoint
	AMDPCmdSetBreakpoint
	// AMDPCmdGetBreakpoints lists breakpoints
	AMDPCmdGetBreakpoints
)

// AMDPCommand represents a debug command sent to the session manager
type AMDPCommand struct {
	Type     AMDPCommandType
	Args     map[string]interface{}
	Response chan AMDPResponse
}

// AMDPResponse is the result of a debug command
type AMDPResponse struct {
	Success bool
	Data    interface{}
	Error   error
}

// AMDPSessionState represents the state of an AMDP debug session
type AMDPSessionState struct {
	SessionID   string `json:"sessionId"`
	MainID      string `json:"mainId"`
	ObjectURI   string `json:"objectUri"`
	Status      string `json:"status"` // "running", "stopped", "breakpoint", "waiting"
	CurrentLine int    `json:"currentLine,omitempty"`
	CurrentProc string `json:"currentProc,omitempty"`
	// Breakpoint event data (populated when breakpoint hits)
	DebuggeeID    string                   `json:"debuggeeId,omitempty"`
	BreakPosition *AMDPBreakPosition       `json:"breakPosition,omitempty"`
	CallStack     []AMDPBreakStackFrame    `json:"callStack,omitempty"`
	Variables     []AMDPBreakVariable      `json:"variables,omitempty"`
	LastEventKind string                   `json:"lastEventKind,omitempty"`
	LastEventTime time.Time                `json:"lastEventTime,omitempty"`
}

// AMDPBreakPosition represents position when stopped at breakpoint
type AMDPBreakPosition struct {
	ObjectName string `json:"objectName"`
	Line       int    `json:"line"`
	Column     int    `json:"column,omitempty"`
}

// AMDPBreakStackFrame represents a call stack frame at breakpoint
type AMDPBreakStackFrame struct {
	Name       string `json:"name"`
	ObjectName string `json:"objectName"`
	Line       int    `json:"line"`
	Level      int    `json:"level"`
}

// AMDPBreakVariable represents a variable at breakpoint
type AMDPBreakVariable struct {
	Name  string `json:"name"`
	Type  string `json:"type"`
	Value string `json:"value,omitempty"`
	ID    string `json:"id,omitempty"`
	Rows  int    `json:"rows,omitempty"`
}

// AMDPSessionManager manages a persistent AMDP debug session
type AMDPSessionManager struct {
	mu          sync.RWMutex
	running     bool
	state       AMDPSessionState
	cmdChannel  chan AMDPCommand
	httpClient  *http.Client
	baseURL     string
	client      string // SAP client
	csrfToken   string
	cancel      context.CancelFunc
	user        string // For background polling auth
	password    string // For background polling auth
	stopPolling chan struct{}
}

// NewAMDPSessionManager creates a new AMDP session manager
func NewAMDPSessionManager(baseURL, client string, insecure bool) *AMDPSessionManager {
	// Create HTTP client with cookie jar
	jar, _ := cookiejar.New(nil)

	transport := &http.Transport{
		TLSClientConfig: &tls.Config{
			InsecureSkipVerify: insecure,
		},
	}

	return &AMDPSessionManager{
		baseURL: baseURL,
		client:  client,
		httpClient: &http.Client{
			Jar:       jar,
			Transport: transport,
			Timeout:   60 * time.Second,
		},
	}
}

// IsRunning returns whether a debug session is active
func (m *AMDPSessionManager) IsRunning() bool {
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.running
}

// State returns the current session state
func (m *AMDPSessionManager) State() AMDPSessionState {
	m.mu.RLock()
	defer m.mu.RUnlock()
	return m.state
}

// Start initializes and starts a new AMDP debug session
func (m *AMDPSessionManager) Start(ctx context.Context, objectURI, user, password string) error {
	m.mu.Lock()
	if m.running {
		m.mu.Unlock()
		return fmt.Errorf("AMDP session already active")
	}
	m.running = true
	m.cmdChannel = make(chan AMDPCommand, 10)
	m.stopPolling = make(chan struct{})
	m.user = user
	m.password = password
	m.state = AMDPSessionState{
		ObjectURI: objectURI,
		Status:    "starting",
	}
	m.mu.Unlock()

	// Use request context for initial API calls (CSRF, session init)
	// but create a separate background context for persistent goroutines

	// Fetch CSRF token first (uses request context)
	if err := m.fetchCSRFToken(ctx, user, password); err != nil {
		m.mu.Lock()
		m.running = false
		m.mu.Unlock()
		return fmt.Errorf("failed to fetch CSRF token: %w", err)
	}

	// Start the debug session via ADT API (uses request context)
	if err := m.initiateSession(ctx, objectURI, user, password); err != nil {
		m.mu.Lock()
		m.running = false
		m.mu.Unlock()
		return fmt.Errorf("failed to start AMDP session: %w", err)
	}

	// Create background context for persistent goroutines
	// This context lives until Stop() is called
	sessionCtx, cancel := context.WithCancel(context.Background())
	m.cancel = cancel

	// Start command processor goroutine (background context)
	go m.processCommands(sessionCtx)

	// Start background polling goroutine for breakpoint events (background context)
	go m.backgroundPoll(sessionCtx)

	return nil
}

// Stop terminates the debug session
func (m *AMDPSessionManager) Stop() error {
	m.mu.RLock()
	if !m.running {
		m.mu.RUnlock()
		return nil
	}
	stopChan := m.stopPolling
	m.mu.RUnlock()

	// Signal background polling to stop
	if stopChan != nil {
		close(stopChan)
	}

	if m.cancel != nil {
		m.cancel()
	}
	return nil
}

// SendCommand sends a command to the session manager and waits for response
func (m *AMDPSessionManager) SendCommand(cmdType AMDPCommandType, args map[string]interface{}) (AMDPResponse, error) {
	m.mu.RLock()
	if !m.running {
		m.mu.RUnlock()
		return AMDPResponse{}, fmt.Errorf("AMDP session not running")
	}
	cmdChan := m.cmdChannel
	m.mu.RUnlock()

	// Create response channel for this specific command
	respChan := make(chan AMDPResponse, 1)

	// Send command
	select {
	case cmdChan <- AMDPCommand{
		Type:     cmdType,
		Args:     args,
		Response: respChan,
	}:
		// Command sent successfully
	case <-time.After(5 * time.Second):
		return AMDPResponse{}, fmt.Errorf("command channel timeout")
	}

	// Wait for response with timeout
	select {
	case resp := <-respChan:
		return resp, nil
	case <-time.After(60 * time.Second):
		return AMDPResponse{}, fmt.Errorf("command response timeout")
	}
}

// processCommands is the main goroutine loop that handles debug commands
func (m *AMDPSessionManager) processCommands(ctx context.Context) {
	defer func() {
		if r := recover(); r != nil {
			// Log panic but don't crash
			fmt.Printf("AMDP session panic: %v\n", r)
		}
		m.cleanup(ctx)
	}()

	// Keepalive ticker to prevent session timeout (every 30 seconds)
	keepalive := time.NewTicker(30 * time.Second)
	defer keepalive.Stop()

	for {
		select {
		case <-ctx.Done():
			// Context cancelled, clean up
			return

		case <-keepalive.C:
			// Send keepalive to prevent session timeout
			m.sendKeepalive(ctx)

		case cmd, ok := <-m.cmdChannel:
			if !ok {
				return
			}

			// Process command using the persistent session
			resp := m.handleCommand(ctx, cmd)

			// Send response
			select {
			case cmd.Response <- resp:
			default:
				// Response channel full or closed, skip
			}

			// If stop command, exit the goroutine
			if cmd.Type == AMDPCmdStop {
				return
			}
		}
	}
}

// handleCommand processes a single debug command
func (m *AMDPSessionManager) handleCommand(ctx context.Context, cmd AMDPCommand) AMDPResponse {
	switch cmd.Type {
	case AMDPCmdStep:
		stepType, _ := cmd.Args["step_type"].(string)
		result, err := m.step(ctx, stepType)
		return AMDPResponse{Success: err == nil, Data: result, Error: err}

	case AMDPCmdGetStatus:
		status, err := m.getStatus(ctx)
		return AMDPResponse{Success: err == nil, Data: status, Error: err}

	case AMDPCmdGetVariables:
		vars, err := m.getVariables(ctx)
		return AMDPResponse{Success: err == nil, Data: vars, Error: err}

	case AMDPCmdGetBreakpoints:
		bps, err := m.getBreakpoints(ctx)
		return AMDPResponse{Success: err == nil, Data: bps, Error: err}

	case AMDPCmdSetBreakpoint:
		procName, _ := cmd.Args["proc_name"].(string)
		line, _ := cmd.Args["line"].(int)
		respBody, err := m.setBreakpoint(ctx, procName, line)
		return AMDPResponse{Success: err == nil, Data: respBody, Error: err}

	case AMDPCmdStop:
		err := m.stopSession(ctx)
		return AMDPResponse{Success: err == nil, Error: err}

	default:
		return AMDPResponse{Error: fmt.Errorf("unknown command: %d", cmd.Type)}
	}
}

// cleanup releases resources when the session ends
func (m *AMDPSessionManager) cleanup(ctx context.Context) {
	m.mu.Lock()
	defer m.mu.Unlock()

	// Try to stop session on server
	if m.state.MainID != "" {
		m.hardStop(ctx)
	}

	m.running = false
	m.state = AMDPSessionState{}
	if m.cmdChannel != nil {
		close(m.cmdChannel)
		m.cmdChannel = nil
	}
}

// backgroundPoll continuously long-polls for debug events (breakpoint hits)
// This runs in a separate goroutine and updates session state when events occur
func (m *AMDPSessionManager) backgroundPoll(ctx context.Context) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintf(os.Stderr, "AMDP background poll panic: %v\n", r)
		}
		fmt.Fprintf(os.Stderr, "AMDP: background poll goroutine exiting\n")
	}()

	fmt.Fprintf(os.Stderr, "AMDP: background poll goroutine started\n")

	m.mu.Lock()
	m.state.Status = "waiting"
	m.mu.Unlock()

	for {
		select {
		case <-ctx.Done():
			return
		case <-m.stopPolling:
			return
		default:
			// Continue polling
		}

		// Get current mainID
		m.mu.RLock()
		mainID := m.state.MainID
		user := m.user
		password := m.password
		m.mu.RUnlock()

		if mainID == "" {
			time.Sleep(100 * time.Millisecond)
			continue
		}

		// Long-poll for events (30 second timeout to allow responsive shutdown)
		u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s?sap-client=%s&timeout=30",
			m.baseURL, url.PathEscape(mainID), m.client)

		pollCtx, cancel := context.WithTimeout(ctx, 35*time.Second)
		req, err := http.NewRequestWithContext(pollCtx, "GET", u, nil)
		if err != nil {
			cancel()
			time.Sleep(time.Second)
			continue
		}

		req.SetBasicAuth(user, password)
		req.Header.Set("Accept", "application/vnd.sap.adt.amdp.dbg.main.v4+xml")

		resp, err := m.httpClient.Do(req)
		cancel()

		if err != nil {
			// Timeout or network error - continue polling
			if ctx.Err() != nil {
				return // Context cancelled
			}
			time.Sleep(time.Second)
			continue
		}

		bodyBytes, _ := io.ReadAll(resp.Body)
		resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			time.Sleep(time.Second)
			continue
		}

		// Parse the response for debug events
		fmt.Fprintf(os.Stderr, "AMDP: received poll response (%d bytes)\n", len(bodyBytes))
		m.parseDebugEvent(bodyBytes)
	}
}

// amdpMainResponse is the XML structure for debug events
type amdpMainResponse struct {
	XMLName    xml.Name `xml:"main"`
	Kind       string   `xml:"kind,attr"`
	DebuggeeID string   `xml:"debuggeeId,attr"`
	// Position when at breakpoint
	Position *struct {
		ObjectName string `xml:"objectName,attr"`
		Line       int    `xml:"line,attr"`
		Column     int    `xml:"column,attr"`
	} `xml:"position"`
	// Call stack
	CallStack *struct {
		Frames []struct {
			Name       string `xml:"name,attr"`
			ObjectName string `xml:"objectName,attr"`
			Line       int    `xml:"line,attr"`
			Level      int    `xml:"level,attr"`
		} `xml:"stackFrame"`
	} `xml:"callStack"`
	// Variables
	Variables *struct {
		Vars []struct {
			Name  string `xml:"name,attr"`
			Type  string `xml:"type,attr"`
			Value string `xml:"value,attr"`
			ID    string `xml:"id,attr"`
			Rows  int    `xml:"rows,attr"`
		} `xml:"variable"`
	} `xml:"variables"`
}

// parseDebugEvent parses the long-poll response and updates session state
func (m *AMDPSessionManager) parseDebugEvent(body []byte) {
	var resp amdpMainResponse
	if err := xml.Unmarshal(body, &resp); err != nil {
		// Try alternate root element names - log raw response for debugging
		fmt.Fprintf(os.Stderr, "AMDP: failed to parse response: %v\nRaw: %s\n", err, string(body))
		return
	}

	fmt.Fprintf(os.Stderr, "AMDP: parsed event kind=%s debuggeeId=%s\n", resp.Kind, resp.DebuggeeID)

	m.mu.Lock()
	defer m.mu.Unlock()

	m.state.LastEventKind = resp.Kind
	m.state.LastEventTime = time.Now()

	switch resp.Kind {
	case "on_break":
		// Breakpoint hit!
		m.state.Status = "breakpoint"
		m.state.DebuggeeID = resp.DebuggeeID

		if resp.Position != nil {
			m.state.BreakPosition = &AMDPBreakPosition{
				ObjectName: resp.Position.ObjectName,
				Line:       resp.Position.Line,
				Column:     resp.Position.Column,
			}
			m.state.CurrentLine = resp.Position.Line
			m.state.CurrentProc = resp.Position.ObjectName
		}

		if resp.CallStack != nil {
			m.state.CallStack = make([]AMDPBreakStackFrame, len(resp.CallStack.Frames))
			for i, f := range resp.CallStack.Frames {
				m.state.CallStack[i] = AMDPBreakStackFrame{
					Name:       f.Name,
					ObjectName: f.ObjectName,
					Line:       f.Line,
					Level:      f.Level,
				}
			}
		}

		if resp.Variables != nil {
			m.state.Variables = make([]AMDPBreakVariable, len(resp.Variables.Vars))
			for i, v := range resp.Variables.Vars {
				m.state.Variables[i] = AMDPBreakVariable{
					Name:  v.Name,
					Type:  v.Type,
					Value: v.Value,
					ID:    v.ID,
					Rows:  v.Rows,
				}
			}
		}

	case "on_execution_end":
		// Execution finished without hitting breakpoint
		m.state.Status = "waiting"
		m.state.DebuggeeID = ""
		m.state.BreakPosition = nil
		m.state.CallStack = nil
		m.state.Variables = nil

	case "on_timeout":
		// Long-poll timeout, continue waiting
		if m.state.Status != "breakpoint" {
			m.state.Status = "waiting"
		}

	default:
		// Unknown event, log for debugging
		fmt.Printf("AMDP unknown event kind: %s\n", resp.Kind)
	}
}

// fetchCSRFToken retrieves a CSRF token for subsequent requests
func (m *AMDPSessionManager) fetchCSRFToken(ctx context.Context, user, password string) error {
	u := fmt.Sprintf("%s/sap/bc/adt/discovery?sap-client=%s", m.baseURL, m.client)

	req, err := http.NewRequestWithContext(ctx, "HEAD", u, nil)
	if err != nil {
		return err
	}

	req.SetBasicAuth(user, password)
	req.Header.Set("X-CSRF-Token", "fetch")

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	m.csrfToken = resp.Header.Get("X-CSRF-Token")
	if m.csrfToken == "" || m.csrfToken == "unsafe" {
		return fmt.Errorf("failed to get CSRF token")
	}

	return nil
}

// initiateSession starts an AMDP debug session on the server
// Uses correct ADT endpoint: /sap/bc/adt/amdp/debugger/main
func (m *AMDPSessionManager) initiateSession(ctx context.Context, objectURI, user, password string) error {
	// Correct endpoint from ADT discovery
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main?sap-client=%s&stopExisting=true&requestUser=%s&cascadeMode=FULL",
		m.baseURL, m.client, strings.ToUpper(user))

	req, err := http.NewRequestWithContext(ctx, "POST", u, nil)
	if err != nil {
		return err
	}

	req.SetBasicAuth(user, password)
	req.Header.Set("X-CSRF-Token", m.csrfToken)
	req.Header.Set("Accept", "application/vnd.sap.adt.amdp.dbg.startmain.v1+xml")

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		bodyBytes, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("failed to start session: %d - %s", resp.StatusCode, string(bodyBytes))
	}

	// Parse response - returns <startParameters> with HANA_SESSION_ID
	bodyBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}

	// Parse the startParameters response
	var startParams struct {
		XMLName    xml.Name `xml:"startParameters"`
		Parameters []struct {
			Key   string `xml:"key,attr"`
			Value string `xml:"value,attr"`
		} `xml:"parameter"`
	}
	if err := xml.Unmarshal(bodyBytes, &startParams); err != nil {
		return fmt.Errorf("session started but failed to parse response: %s", string(bodyBytes))
	}

	// Extract HANA_SESSION_ID as the mainId
	var mainID string
	for _, p := range startParams.Parameters {
		if p.Key == "HANA_SESSION_ID" {
			mainID = p.Value
			break
		}
	}
	if mainID == "" {
		return fmt.Errorf("no HANA_SESSION_ID in response: %s", string(bodyBytes))
	}

	m.mu.Lock()
	m.state.SessionID = mainID
	m.state.MainID = mainID
	m.state.Status = "running"
	m.mu.Unlock()

	return nil
}

// step performs a step operation in the debug session
// If we have a debuggeeId (from breakpoint hit), performs actual step operation
// Otherwise returns current status from background polling
func (m *AMDPSessionManager) step(ctx context.Context, stepType string) (map[string]interface{}, error) {
	m.mu.RLock()
	mainID := m.state.MainID
	debuggeeID := m.state.DebuggeeID
	status := m.state.Status
	user := m.user
	password := m.password
	m.mu.RUnlock()

	if mainID == "" {
		return nil, fmt.Errorf("no active session")
	}

	// If we have a debuggeeId, perform actual step operation
	if debuggeeID != "" && status == "breakpoint" {
		return m.performStep(ctx, mainID, debuggeeID, stepType, user, password)
	}

	// No debuggee yet - return current status (background polling handles events)
	m.mu.RLock()
	state := m.state
	m.mu.RUnlock()

	return map[string]interface{}{
		"status":        state.Status,
		"message":       "waiting for breakpoint (background polling active)",
		"lastEventKind": state.LastEventKind,
		"mainId":        mainID,
	}, nil
}

// performStep executes an actual step operation using debuggeeId
func (m *AMDPSessionManager) performStep(ctx context.Context, mainID, debuggeeID, stepType, user, password string) (map[string]interface{}, error) {
	// Endpoint: POST /sap/bc/adt/amdp/debugger/main/{mainId}/debuggees/{debuggeeId}?action=stepOver
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s/debuggees/%s?sap-client=%s&action=%s",
		m.baseURL, url.PathEscape(mainID), url.PathEscape(debuggeeID), m.client, stepType)

	req, err := http.NewRequestWithContext(ctx, "POST", u, nil)
	if err != nil {
		return nil, err
	}

	req.SetBasicAuth(user, password)
	req.Header.Set("X-CSRF-Token", m.csrfToken)
	req.Header.Set("Accept", "application/vnd.sap.adt.amdp.dbg.main.v4+xml")

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	bodyBytes, _ := io.ReadAll(resp.Body)

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("step failed: %d - %s", resp.StatusCode, string(bodyBytes))
	}

	// Parse the step response and update state
	m.parseDebugEvent(bodyBytes)

	// Return updated state
	m.mu.RLock()
	state := m.state
	m.mu.RUnlock()

	result := map[string]interface{}{
		"stepType":   stepType,
		"status":     state.Status,
		"debuggeeId": state.DebuggeeID,
	}

	if state.BreakPosition != nil {
		result["position"] = map[string]interface{}{
			"objectName": state.BreakPosition.ObjectName,
			"line":       state.BreakPosition.Line,
		}
	}

	return result, nil
}

// getStatus retrieves the current debug session status
func (m *AMDPSessionManager) getStatus(ctx context.Context) (*AMDPSessionState, error) {
	m.mu.RLock()
	mainID := m.state.MainID
	state := m.state
	m.mu.RUnlock()

	if mainID == "" {
		return nil, fmt.Errorf("no active session")
	}

	// Return current local state (we don't poll the server here to avoid blocking)
	return &state, nil
}

// getVariables retrieves variable values from the debug session
// Returns cached variables from the last breakpoint hit, or fetches fresh if debuggeeId available
func (m *AMDPSessionManager) getVariables(ctx context.Context) ([]map[string]interface{}, error) {
	m.mu.RLock()
	mainID := m.state.MainID
	debuggeeID := m.state.DebuggeeID
	status := m.state.Status
	storedVars := m.state.Variables
	m.mu.RUnlock()

	if mainID == "" {
		return nil, fmt.Errorf("no active session")
	}

	// If we have stored variables from a breakpoint hit, return them
	if len(storedVars) > 0 {
		result := make([]map[string]interface{}, len(storedVars))
		for i, v := range storedVars {
			result[i] = map[string]interface{}{
				"name":  v.Name,
				"type":  v.Type,
				"value": v.Value,
				"id":    v.ID,
				"rows":  v.Rows,
			}
		}
		return result, nil
	}

	// If at breakpoint with debuggeeId, try to fetch variables
	if status == "breakpoint" && debuggeeID != "" {
		return m.fetchVariables(ctx, mainID, debuggeeID)
	}

	// Not at breakpoint yet
	return []map[string]interface{}{
		{"status": status, "message": "waiting for breakpoint", "mainId": mainID},
	}, nil
}

// fetchVariables retrieves variables from the server using debuggeeId
func (m *AMDPSessionManager) fetchVariables(ctx context.Context, mainID, debuggeeID string) ([]map[string]interface{}, error) {
	// Endpoint: /sap/bc/adt/amdp/debugger/main/{mainId}/debuggees/{debuggeeId}/variables
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s/debuggees/%s/variables?sap-client=%s",
		m.baseURL, url.PathEscape(mainID), url.PathEscape(debuggeeID), m.client)

	m.mu.RLock()
	user := m.user
	password := m.password
	m.mu.RUnlock()

	req, err := http.NewRequestWithContext(ctx, "GET", u, nil)
	if err != nil {
		return nil, err
	}

	req.SetBasicAuth(user, password)
	req.Header.Set("Accept", "application/xml")

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	bodyBytes, _ := io.ReadAll(resp.Body)

	if resp.StatusCode != http.StatusOK {
		return []map[string]interface{}{
			{"error": fmt.Sprintf("fetch failed: %d", resp.StatusCode), "response": string(bodyBytes)},
		}, nil
	}

	// Parse variable XML response
	var varsResp struct {
		XMLName   xml.Name `xml:"variables"`
		Variables []struct {
			Name  string `xml:"name,attr"`
			Type  string `xml:"type,attr"`
			Value string `xml:"value,attr"`
			ID    string `xml:"id,attr"`
			Rows  int    `xml:"rows,attr"`
		} `xml:"variable"`
	}
	if err := xml.Unmarshal(bodyBytes, &varsResp); err != nil {
		return []map[string]interface{}{
			{"raw": string(bodyBytes)},
		}, nil
	}

	result := make([]map[string]interface{}, len(varsResp.Variables))
	for i, v := range varsResp.Variables {
		result[i] = map[string]interface{}{
			"name":  v.Name,
			"type":  v.Type,
			"value": v.Value,
			"id":    v.ID,
			"rows":  v.Rows,
		}
	}
	return result, nil
}

// getBreakpoints retrieves the list of breakpoints
func (m *AMDPSessionManager) getBreakpoints(ctx context.Context) ([]map[string]interface{}, error) {
	m.mu.RLock()
	mainID := m.state.MainID
	m.mu.RUnlock()

	if mainID == "" {
		return nil, fmt.Errorf("no active session")
	}

	// Correct endpoint: /sap/bc/adt/amdp/debugger/main/{mainId}/breakpoints
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s/breakpoints?sap-client=%s",
		m.baseURL, url.PathEscape(mainID), m.client)

	req, err := http.NewRequestWithContext(ctx, "GET", u, nil)
	if err != nil {
		return nil, err
	}

	m.mu.RLock()
	user := m.user
	password := m.password
	m.mu.RUnlock()

	req.SetBasicAuth(user, password)
	req.Header.Set("Accept", "application/xml")

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	bodyBytes, _ := io.ReadAll(resp.Body)

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("get breakpoints failed: %d - %s", resp.StatusCode, string(bodyBytes))
	}

	return []map[string]interface{}{
		{"response": string(bodyBytes)},
	}, nil
}

// setBreakpoint sets a breakpoint at the specified location
// procName can be either:
//   - Class=>Method format (e.g., "ZCL_TEST=>METHOD_NAME") - will be converted to ADT URI
//   - ADT URI format (e.g., "/sap/bc/adt/oo/classes/zcl_test/source/main")
// line is the ABAP source line number
// Returns the response body for debugging
func (m *AMDPSessionManager) setBreakpoint(ctx context.Context, procName string, line int) (string, error) {
	m.mu.RLock()
	mainID := m.state.MainID
	m.mu.RUnlock()

	if mainID == "" {
		return "", fmt.Errorf("no active session")
	}

	// Correct endpoint: /sap/bc/adt/amdp/debugger/main/{mainId}/breakpoints
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s/breakpoints?sap-client=%s",
		m.baseURL, url.PathEscape(mainID), m.client)

	// Convert Class=>Method format to ADT URI if needed
	// The AMDP debugger expects breakpoints specified as ADT object references with URI
	// Format: /sap/bc/adt/oo/classes/{classname}/source/main#start={line}
	var adtURI string
	if strings.HasPrefix(procName, "/sap/bc/adt/") {
		// Already an ADT URI, just add line fragment
		adtURI = fmt.Sprintf("%s#start=%d", strings.Split(procName, "#")[0], line)
	} else if strings.Contains(procName, "=>") {
		// Class=>Method format - extract class name and build URI
		parts := strings.Split(procName, "=>")
		className := strings.ToLower(parts[0])
		adtURI = fmt.Sprintf("/sap/bc/adt/oo/classes/%s/source/main#start=%d", className, line)
	} else {
		// Assume it's just a class name
		className := strings.ToLower(procName)
		adtURI = fmt.Sprintf("/sap/bc/adt/oo/classes/%s/source/main#start=%d", className, line)
	}

	// Build breakpoint XML with ADT object reference format
	// Based on IF_AMDP_DBG_ADT_CH_TYPES=>ty_dbg_breakpoint_req:
	//   - abap_position: sadt_object_reference with uri field
	//   - bp_kind: "line" (default)
	// The server uses uri_mapper->map_objref_to_include() to parse the URI
	clientID := fmt.Sprintf("bp_%s_%d", strings.ReplaceAll(procName, "=>", "_"), line)
	body := fmt.Sprintf(`<?xml version="1.0" encoding="UTF-8"?>
<amdp:breakpointsSyncRequest xmlns:amdp="http://www.sap.com/adt/amdp/debugger"
                              xmlns:adtcore="http://www.sap.com/adt/core"
                              amdp:syncMode="FULL">
  <amdp:breakpoints>
    <amdp:breakpoint amdp:clientId="%s" amdp:bpKind="line">
      <amdp:abapPosition>
        <adtcore:objectReference adtcore:uri="%s"/>
      </amdp:abapPosition>
    </amdp:breakpoint>
  </amdp:breakpoints>
</amdp:breakpointsSyncRequest>`, clientID, adtURI)

	req, err := http.NewRequestWithContext(ctx, "POST", u, strings.NewReader(body))
	if err != nil {
		return "", err
	}

	m.mu.RLock()
	user := m.user
	password := m.password
	m.mu.RUnlock()

	req.SetBasicAuth(user, password)
	req.Header.Set("X-CSRF-Token", m.csrfToken)
	req.Header.Set("Content-Type", "application/vnd.sap.adt.amdp.dbg.bpsync.v1+xml")
	req.Header.Set("Accept", "application/vnd.sap.adt.amdp.dbg.bpsync.v1+xml, application/xml")

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	bodyBytes, _ := io.ReadAll(resp.Body)

	// Debug: log the response to stderr
	fmt.Fprintf(os.Stderr, "AMDP SetBreakpoint: status=%d, response=%s\n", resp.StatusCode, string(bodyBytes))

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		return "", fmt.Errorf("set breakpoint failed: %d - %s", resp.StatusCode, string(bodyBytes))
	}

	// Include status in response for debugging
	if len(bodyBytes) == 0 {
		return fmt.Sprintf("(status %d, empty response)", resp.StatusCode), nil
	}
	return string(bodyBytes), nil
}

// stopSession gracefully stops the debug session
func (m *AMDPSessionManager) stopSession(ctx context.Context) error {
	m.mu.RLock()
	mainID := m.state.MainID
	m.mu.RUnlock()

	if mainID == "" {
		return nil // Already stopped
	}

	// Correct endpoint: DELETE /sap/bc/adt/amdp/debugger/main/{mainId}?hardStop=true
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s?sap-client=%s&hardStop=true",
		m.baseURL, url.PathEscape(mainID), m.client)

	req, err := http.NewRequestWithContext(ctx, "DELETE", u, nil)
	if err != nil {
		return err
	}

	req.Header.Set("X-CSRF-Token", m.csrfToken)

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	// Accept 200, 204, or 404 (already stopped)
	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusNotFound {
		bodyBytes, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("stop session failed: %d - %s", resp.StatusCode, string(bodyBytes))
	}

	return nil
}

// hardStop forcefully terminates the session
func (m *AMDPSessionManager) hardStop(ctx context.Context) {
	mainID := m.state.MainID
	if mainID == "" {
		return
	}

	// Correct endpoint: DELETE /sap/bc/adt/amdp/debugger/main/{mainId}?hardStop=true
	u := fmt.Sprintf("%s/sap/bc/adt/amdp/debugger/main/%s?sap-client=%s&hardStop=true",
		m.baseURL, url.PathEscape(mainID), m.client)

	req, _ := http.NewRequestWithContext(ctx, "DELETE", u, nil)
	if req != nil {
		req.Header.Set("X-CSRF-Token", m.csrfToken)
		m.httpClient.Do(req)
	}
}

// sendKeepalive sends a keepalive request to prevent session timeout
func (m *AMDPSessionManager) sendKeepalive(ctx context.Context) {
	m.mu.RLock()
	mainID := m.state.MainID
	m.mu.RUnlock()

	if mainID == "" {
		return
	}

	// Simple GET to discovery endpoint to keep session alive
	// Using discovery instead of main/{id} to avoid blocking long-poll
	u := fmt.Sprintf("%s/sap/bc/adt/discovery?sap-client=%s", m.baseURL, m.client)

	req, err := http.NewRequestWithContext(ctx, "HEAD", u, nil)
	if err != nil {
		return
	}

	resp, err := m.httpClient.Do(req)
	if err != nil {
		return
	}
	resp.Body.Close()
}

package adt

import (
	"context"
	"encoding/xml"
	"fmt"
	"net/http"
	"net/url"
	"strings"
)

// AMDP Debugger API endpoints
const (
	amdpDebuggerBase = "/sap/bc/adt/amdp/debugger/main"
)

// AMDPDebugSession represents an AMDP debug session.
type AMDPDebugSession struct {
	MainID       string `json:"mainId"`
	HANASession  string `json:"hanaSession,omitempty"`
	User         string `json:"user,omitempty"`
	CascadeMode  string `json:"cascadeMode,omitempty"` // "NONE" or "FULL"
}

// AMDPDebugResponse represents the response from AMDP debugger operations.
type AMDPDebugResponse struct {
	Kind        string            `json:"kind"` // on_break, on_toggle_breakpoints, on_execution_end, etc.
	Position    *AMDPPosition     `json:"position,omitempty"`
	CallStack   []AMDPStackFrame  `json:"callStack,omitempty"`
	Variables   []AMDPVariable    `json:"variables,omitempty"`
	Breakpoints []AMDPBreakpoint  `json:"breakpoints,omitempty"`
	Message     string            `json:"message,omitempty"`
}

// AMDPPosition represents a position in AMDP source code.
type AMDPPosition struct {
	ObjectName string `json:"objectName"`
	Line       int    `json:"line"`
	Column     int    `json:"column,omitempty"`
}

// AMDPStackFrame represents a frame in the AMDP call stack.
type AMDPStackFrame struct {
	Name       string       `json:"name"`
	Position   AMDPPosition `json:"position"`
	Level      int          `json:"level"`
}

// AMDPVariable represents a variable in AMDP debugging.
type AMDPVariable struct {
	Name     string `json:"name"`
	Type     string `json:"type"` // scalar, table, array
	Value    string `json:"value,omitempty"`
	ID       string `json:"id,omitempty"`
	Rows     int    `json:"rows,omitempty"` // for table types
}

// AMDPBreakpoint represents an AMDP breakpoint.
type AMDPBreakpoint struct {
	ID         string `json:"id"`
	ObjectName string `json:"objectName"`
	Line       int    `json:"line"`
	Enabled    bool   `json:"enabled"`
}

// XML types for AMDP debugger responses

// amdpStartParameters is returned by AMDPDebuggerStart
type amdpStartParameters struct {
	XMLName    xml.Name              `xml:"startParameters"`
	Parameters []amdpStartParameter  `xml:"parameter"`
}

type amdpStartParameter struct {
	Key   string `xml:"key,attr"`
	Value string `xml:"value,attr"`
}

type amdpDebuggerResponse struct {
	XMLName    xml.Name `xml:"debuggerResponse"`
	MainID     string   `xml:"mainId,attr,omitempty"`
	Kind       string   `xml:"kind,attr,omitempty"`
	Message    string   `xml:"message,attr,omitempty"`
	Position   *amdpPositionXML `xml:"position,omitempty"`
	CallStack  *amdpCallStackXML `xml:"callStack,omitempty"`
	Variables  *amdpVariablesXML `xml:"variables,omitempty"`
}

type amdpPositionXML struct {
	ObjectName string `xml:"objectName,attr"`
	Line       int    `xml:"line,attr"`
	Column     int    `xml:"column,attr,omitempty"`
}

type amdpCallStackXML struct {
	Frames []amdpStackFrameXML `xml:"stackFrame"`
}

type amdpStackFrameXML struct {
	Name       string `xml:"name,attr"`
	ObjectName string `xml:"objectName,attr"`
	Line       int    `xml:"line,attr"`
	Level      int    `xml:"level,attr"`
}

type amdpVariablesXML struct {
	Variables []amdpVariableXML `xml:"variable"`
}

type amdpVariableXML struct {
	Name  string `xml:"name,attr"`
	Type  string `xml:"type,attr"`
	Value string `xml:"value,attr,omitempty"`
	ID    string `xml:"id,attr,omitempty"`
	Rows  int    `xml:"rows,attr,omitempty"`
}

// AMDPDebuggerStart starts an AMDP debug session.
// cascadeMode can be "NONE" (debug only called procedure) or "FULL" (debug all nested procedures).
func (c *Client) AMDPDebuggerStart(ctx context.Context, user, cascadeMode string) (*AMDPDebugSession, error) {
	if user == "" {
		user = c.config.Username
	}
	if cascadeMode == "" {
		cascadeMode = "FULL"
	}

	params := url.Values{}
	params.Set("user", strings.ToUpper(user))
	params.Set("cascadeMode", cascadeMode)

	resp, err := c.transport.Request(ctx, amdpDebuggerBase, &RequestOptions{
		Method: http.MethodPost,
		Query:  params,
		Accept: "application/vnd.sap.adt.amdp.dbg.startmain.v1+xml",
	})
	if err != nil {
		return nil, fmt.Errorf("starting AMDP debugger: %w", err)
	}

	// Response is <startParameters> with HANA_SESSION_ID parameter
	var startParams amdpStartParameters
	if err := xml.Unmarshal(resp.Body, &startParams); err != nil {
		return nil, fmt.Errorf("parsing AMDP debugger response: %w", err)
	}

	// Extract HANA_SESSION_ID as the mainId
	var mainID string
	for _, p := range startParams.Parameters {
		if p.Key == "HANA_SESSION_ID" {
			mainID = p.Value
			break
		}
	}

	return &AMDPDebugSession{
		MainID:      mainID,
		HANASession: mainID,
		User:        user,
		CascadeMode: cascadeMode,
	}, nil
}

// AMDPDebuggerResume resumes an AMDP debug session and waits for events.
// This is a blocking long-poll operation that returns when:
// - A breakpoint is hit (on_break)
// - Execution ends (on_execution_end)
// - Timeout occurs
func (c *Client) AMDPDebuggerResume(ctx context.Context, mainID string, timeout int) (*AMDPDebugResponse, error) {
	if mainID == "" {
		return nil, fmt.Errorf("mainID is required")
	}
	if timeout <= 0 {
		timeout = 60
	}

	path := fmt.Sprintf("%s/%s", amdpDebuggerBase, url.PathEscape(mainID))
	params := url.Values{}
	params.Set("timeout", fmt.Sprintf("%d", timeout))

	resp, err := c.transport.Request(ctx, path, &RequestOptions{
		Method: http.MethodGet,
		Query:  params,
		Accept: "application/xml",
	})
	if err != nil {
		return nil, fmt.Errorf("resuming AMDP debugger: %w", err)
	}

	var debugResp amdpDebuggerResponse
	if err := xml.Unmarshal(resp.Body, &debugResp); err != nil {
		return nil, fmt.Errorf("parsing AMDP debugger response: %w", err)
	}

	return convertAMDPResponse(&debugResp), nil
}

// AMDPDebuggerStop stops an AMDP debug session.
// If hardStop is true, terminates the debuggee immediately.
func (c *Client) AMDPDebuggerStop(ctx context.Context, mainID string, hardStop bool) error {
	if mainID == "" {
		return fmt.Errorf("mainID is required")
	}

	path := fmt.Sprintf("%s/%s", amdpDebuggerBase, url.PathEscape(mainID))
	params := url.Values{}
	if hardStop {
		params.Set("hardStop", "true")
	}

	_, err := c.transport.Request(ctx, path, &RequestOptions{
		Method: http.MethodDelete,
		Query:  params,
	})
	if err != nil {
		return fmt.Errorf("stopping AMDP debugger: %w", err)
	}

	return nil
}

// AMDPDebuggerStep performs a step operation in the AMDP debugger.
// stepType can be: "stepInto", "stepOver", "stepReturn", "stepContinue"
func (c *Client) AMDPDebuggerStep(ctx context.Context, mainID, stepType string) (*AMDPDebugResponse, error) {
	if mainID == "" {
		return nil, fmt.Errorf("mainID is required")
	}
	if stepType == "" {
		stepType = "stepOver"
	}

	path := fmt.Sprintf("%s/%s", amdpDebuggerBase, url.PathEscape(mainID))
	params := url.Values{}
	params.Set("action", stepType)

	resp, err := c.transport.Request(ctx, path, &RequestOptions{
		Method: http.MethodPost,
		Query:  params,
		Accept: "application/xml",
	})
	if err != nil {
		return nil, fmt.Errorf("stepping AMDP debugger: %w", err)
	}

	var debugResp amdpDebuggerResponse
	if err := xml.Unmarshal(resp.Body, &debugResp); err != nil {
		return nil, fmt.Errorf("parsing AMDP debugger response: %w", err)
	}

	return convertAMDPResponse(&debugResp), nil
}

// AMDPGetScalarValues retrieves scalar variable values during AMDP debugging.
func (c *Client) AMDPGetScalarValues(ctx context.Context, mainID string, variableIDs []string) ([]AMDPVariable, error) {
	if mainID == "" {
		return nil, fmt.Errorf("mainID is required")
	}

	path := fmt.Sprintf("%s/%s/variables", amdpDebuggerBase, url.PathEscape(mainID))
	params := url.Values{}
	for _, id := range variableIDs {
		params.Add("id", id)
	}

	resp, err := c.transport.Request(ctx, path, &RequestOptions{
		Method: http.MethodGet,
		Query:  params,
		Accept: "application/xml",
	})
	if err != nil {
		return nil, fmt.Errorf("getting AMDP variables: %w", err)
	}

	var varsXML amdpVariablesXML
	if err := xml.Unmarshal(resp.Body, &varsXML); err != nil {
		return nil, fmt.Errorf("parsing AMDP variables: %w", err)
	}

	vars := make([]AMDPVariable, len(varsXML.Variables))
	for i, v := range varsXML.Variables {
		vars[i] = AMDPVariable{
			Name:  v.Name,
			Type:  v.Type,
			Value: v.Value,
			ID:    v.ID,
			Rows:  v.Rows,
		}
	}

	return vars, nil
}

// convertAMDPResponse converts XML response to the API response type.
func convertAMDPResponse(xmlResp *amdpDebuggerResponse) *AMDPDebugResponse {
	resp := &AMDPDebugResponse{
		Kind:    xmlResp.Kind,
		Message: xmlResp.Message,
	}

	if xmlResp.Position != nil {
		resp.Position = &AMDPPosition{
			ObjectName: xmlResp.Position.ObjectName,
			Line:       xmlResp.Position.Line,
			Column:     xmlResp.Position.Column,
		}
	}

	if xmlResp.CallStack != nil {
		for _, frame := range xmlResp.CallStack.Frames {
			resp.CallStack = append(resp.CallStack, AMDPStackFrame{
				Name: frame.Name,
				Position: AMDPPosition{
					ObjectName: frame.ObjectName,
					Line:       frame.Line,
				},
				Level: frame.Level,
			})
		}
	}

	if xmlResp.Variables != nil {
		for _, v := range xmlResp.Variables.Variables {
			resp.Variables = append(resp.Variables, AMDPVariable{
				Name:  v.Name,
				Type:  v.Type,
				Value: v.Value,
				ID:    v.ID,
				Rows:  v.Rows,
			})
		}
	}

	return resp
}

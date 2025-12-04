// Package dsl provides a fluent API and YAML-based workflow engine for SAP ADT automation.
// It includes test orchestration, batch operations, and pipeline composition.
package dsl

import (
	"context"
	"time"

	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// ObjectRef represents a reference to an ABAP object.
type ObjectRef struct {
	Type    string `json:"type" yaml:"type"`       // CLAS, PROG, FUNC, etc.
	Name    string `json:"name" yaml:"name"`       // Object name
	Package string `json:"package" yaml:"package"` // Package (DEVCLASS)
	URL     string `json:"url" yaml:"url"`         // ADT URL
}

// ObjectType constants for ABAP object types.
const (
	TypeClass     = "CLAS"
	TypeProgram   = "PROG"
	TypeFunction  = "FUNC"
	TypeFuncGroup = "FUGR"
	TypeInterface = "INTF"
	TypePackage   = "DEVC"
	TypeTable     = "TABL"
	TypeDDLS      = "DDLS"
)

// SearchCriteria defines search parameters.
type SearchCriteria struct {
	Query      string   `json:"query" yaml:"query"`
	Types      []string `json:"types,omitempty" yaml:"types,omitempty"`
	Packages   []string `json:"packages,omitempty" yaml:"packages,omitempty"`
	MaxResults int      `json:"maxResults,omitempty" yaml:"maxResults,omitempty"`
}

// FilterCriteria defines filtering options.
type FilterCriteria struct {
	NamePattern    string   `json:"namePattern,omitempty" yaml:"namePattern,omitempty"`
	HasMethod      string   `json:"hasMethod,omitempty" yaml:"hasMethod,omitempty"`
	ImplementsIntf string   `json:"implementsIntf,omitempty" yaml:"implementsIntf,omitempty"`
	InPackages     []string `json:"inPackages,omitempty" yaml:"inPackages,omitempty"`
	ExcludePattern string   `json:"excludePattern,omitempty" yaml:"excludePattern,omitempty"`
}

// TestConfig defines unit test execution configuration.
type TestConfig struct {
	// Risk levels
	Harmless  bool `json:"harmless" yaml:"harmless"`
	Dangerous bool `json:"dangerous" yaml:"dangerous"`
	Critical  bool `json:"critical" yaml:"critical"`

	// Duration
	Short  bool `json:"short" yaml:"short"`
	Medium bool `json:"medium" yaml:"medium"`
	Long   bool `json:"long" yaml:"long"`

	// Behavior
	StopOnFirstFailure bool `json:"stopOnFirstFailure" yaml:"stopOnFirstFailure"`
	Parallel           int  `json:"parallel" yaml:"parallel"` // Number of parallel executions
	Timeout            time.Duration `json:"timeout" yaml:"timeout"`
}

// DefaultTestConfig returns sensible defaults for test execution.
func DefaultTestConfig() TestConfig {
	return TestConfig{
		Harmless:           true,
		Dangerous:          false,
		Critical:           false,
		Short:              true,
		Medium:             true,
		Long:               false,
		StopOnFirstFailure: false,
		Parallel:           1,
		Timeout:            5 * time.Minute,
	}
}

// TestResult represents the result of a test run.
type TestResult struct {
	Object        ObjectRef        `json:"object"`
	Success       bool             `json:"success"`
	TotalTests    int              `json:"totalTests"`
	PassedTests   int              `json:"passedTests"`
	FailedTests   int              `json:"failedTests"`
	SkippedTests  int              `json:"skippedTests"`
	ExecutionTime time.Duration    `json:"executionTime"`
	Classes       []TestClassResult `json:"classes,omitempty"`
	Error         string           `json:"error,omitempty"`
}

// TestClassResult represents results for a test class.
type TestClassResult struct {
	Name          string             `json:"name"`
	Methods       []TestMethodResult `json:"methods"`
	ExecutionTime time.Duration      `json:"executionTime"`
}

// TestMethodResult represents results for a test method.
type TestMethodResult struct {
	Name          string        `json:"name"`
	Success       bool          `json:"success"`
	ExecutionTime time.Duration `json:"executionTime"`
	Message       string        `json:"message,omitempty"`
}

// TestSummary provides aggregate statistics for test runs.
type TestSummary struct {
	TotalObjects   int           `json:"totalObjects"`
	TestedObjects  int           `json:"testedObjects"`
	PassedObjects  int           `json:"passedObjects"`
	FailedObjects  int           `json:"failedObjects"`
	TotalTests     int           `json:"totalTests"`
	PassedTests    int           `json:"passedTests"`
	FailedTests    int           `json:"failedTests"`
	SkippedTests   int           `json:"skippedTests"`
	TotalTime      time.Duration `json:"totalTime"`
	Results        []TestResult  `json:"results"`
}

// BatchOperation represents a batch modification operation.
type BatchOperation struct {
	Type      string                 `json:"type" yaml:"type"` // transform, update, delete
	Transport string                 `json:"transport,omitempty" yaml:"transport,omitempty"`
	DryRun    bool                   `json:"dryRun,omitempty" yaml:"dryRun,omitempty"`
	Options   map[string]interface{} `json:"options,omitempty" yaml:"options,omitempty"`
}

// BatchResult represents the result of a batch operation.
type BatchResult struct {
	TotalObjects     int            `json:"totalObjects"`
	ProcessedObjects int            `json:"processedObjects"`
	SuccessCount     int            `json:"successCount"`
	FailureCount     int            `json:"failureCount"`
	SkippedCount     int            `json:"skippedCount"`
	Results          []ObjectResult `json:"results"`
}

// ObjectResult represents the result for a single object in a batch.
type ObjectResult struct {
	Object  ObjectRef `json:"object"`
	Success bool      `json:"success"`
	Action  string    `json:"action"` // created, updated, skipped, failed
	Message string    `json:"message,omitempty"`
}

// Pipeline represents a sequence of stages.
type Pipeline struct {
	Name        string            `json:"name" yaml:"name"`
	Description string            `json:"description,omitempty" yaml:"description,omitempty"`
	Stages      []Stage           `json:"stages" yaml:"stages"`
	Variables   map[string]string `json:"variables,omitempty" yaml:"variables,omitempty"`
}

// Stage represents a pipeline stage.
type Stage struct {
	Name       string   `json:"name" yaml:"name"`
	DependsOn  []string `json:"dependsOn,omitempty" yaml:"dependsOn,omitempty"`
	Steps      []Step   `json:"steps" yaml:"steps"`
	FailFast   bool     `json:"failFast,omitempty" yaml:"failFast,omitempty"`
	Condition  string   `json:"condition,omitempty" yaml:"condition,omitempty"`
}

// Step represents a single action in a stage.
type Step struct {
	Action     string                 `json:"action" yaml:"action"` // search, test, transform, etc.
	Name       string                 `json:"name,omitempty" yaml:"name,omitempty"`
	Parameters map[string]interface{} `json:"parameters,omitempty" yaml:"parameters,omitempty"`
	SaveAs     string                 `json:"saveAs,omitempty" yaml:"saveAs,omitempty"`
	Condition  string                 `json:"condition,omitempty" yaml:"condition,omitempty"`
}

// ExecutionContext holds state during workflow execution.
type ExecutionContext struct {
	ctx       context.Context
	client    *adt.Client
	variables map[string]interface{}
	results   map[string]interface{}
	dryRun    bool
	verbose   bool
}

// NewExecutionContext creates a new execution context.
func NewExecutionContext(ctx context.Context, client *adt.Client) *ExecutionContext {
	return &ExecutionContext{
		ctx:       ctx,
		client:    client,
		variables: make(map[string]interface{}),
		results:   make(map[string]interface{}),
	}
}

// Set stores a value in the context.
func (ec *ExecutionContext) Set(key string, value interface{}) {
	ec.results[key] = value
}

// Get retrieves a value from the context.
func (ec *ExecutionContext) Get(key string) (interface{}, bool) {
	v, ok := ec.results[key]
	return v, ok
}

// SetVariable sets a variable.
func (ec *ExecutionContext) SetVariable(key, value string) {
	ec.variables[key] = value
}

// GetVariable gets a variable.
func (ec *ExecutionContext) GetVariable(key string) string {
	if v, ok := ec.variables[key]; ok {
		return v.(string)
	}
	return ""
}

// Context returns the underlying context.
func (ec *ExecutionContext) Context() context.Context {
	return ec.ctx
}

// Client returns the ADT client.
func (ec *ExecutionContext) Client() *adt.Client {
	return ec.client
}

// SetDryRun enables dry-run mode.
func (ec *ExecutionContext) SetDryRun(dryRun bool) {
	ec.dryRun = dryRun
}

// IsDryRun returns whether dry-run mode is enabled.
func (ec *ExecutionContext) IsDryRun() bool {
	return ec.dryRun
}

// SetVerbose enables verbose output.
func (ec *ExecutionContext) SetVerbose(verbose bool) {
	ec.verbose = verbose
}

// IsVerbose returns whether verbose mode is enabled.
func (ec *ExecutionContext) IsVerbose() bool {
	return ec.verbose
}

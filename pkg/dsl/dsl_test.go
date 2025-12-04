package dsl

import (
	"context"
	"testing"
)

func TestSearchBuilder(t *testing.T) {
	t.Run("Query", func(t *testing.T) {
		s := Search(nil).Query("ZCL_*")
		if s.query != "ZCL_*" {
			t.Errorf("expected query 'ZCL_*', got '%s'", s.query)
		}
	})

	t.Run("Types", func(t *testing.T) {
		s := Search(nil).Types("CLAS", "PROG")
		if len(s.types) != 2 {
			t.Errorf("expected 2 types, got %d", len(s.types))
		}
	})

	t.Run("Classes", func(t *testing.T) {
		s := Search(nil).Classes()
		if len(s.types) != 1 || s.types[0] != TypeClass {
			t.Errorf("expected types [CLAS], got %v", s.types)
		}
	})

	t.Run("InPackages", func(t *testing.T) {
		s := Search(nil).InPackages("$TMP", "$ZRAY*")
		if len(s.packages) != 2 {
			t.Errorf("expected 2 packages, got %d", len(s.packages))
		}
	})

	t.Run("MaxResults", func(t *testing.T) {
		s := Search(nil).MaxResults(50)
		if s.maxResults != 50 {
			t.Errorf("expected maxResults 50, got %d", s.maxResults)
		}
	})

	t.Run("ChainedCalls", func(t *testing.T) {
		s := Search(nil).
			Query("Z*").
			Classes().
			InPackage("$TMP").
			MaxResults(25)

		if s.query != "Z*" {
			t.Errorf("expected query 'Z*', got '%s'", s.query)
		}
		if len(s.types) != 1 {
			t.Errorf("expected 1 type, got %d", len(s.types))
		}
		if len(s.packages) != 1 {
			t.Errorf("expected 1 package, got %d", len(s.packages))
		}
		if s.maxResults != 25 {
			t.Errorf("expected maxResults 25, got %d", s.maxResults)
		}
	})
}

func TestMatchWildcard(t *testing.T) {
	tests := []struct {
		pattern string
		s       string
		want    bool
	}{
		{"*", "anything", true},
		{"Z*", "ZCL_TEST", true},
		{"Z*", "YCL_TEST", false},
		{"$TMP", "$TMP", true},
		{"$TMP", "$ZRAY", false},
		{"$ZRAY*", "$ZRAY_TEST", true},
		{"$ZRAY*", "$ZRAY", true},
		{"$ZRAY*", "$TMP", false},
		{"ZCL_?EST", "ZCL_TEST", true},
		{"ZCL_?EST", "ZCL_BEST", true},
		{"ZCL_?EST", "ZCL_AEST", true},
		{"ZCL_?EST", "ZCL_TEST1", false},
	}

	for _, tt := range tests {
		t.Run(tt.pattern+"_"+tt.s, func(t *testing.T) {
			got := matchWildcard(tt.pattern, tt.s)
			if got != tt.want {
				t.Errorf("matchWildcard(%q, %q) = %v, want %v", tt.pattern, tt.s, got, tt.want)
			}
		})
	}
}

func TestContainsType(t *testing.T) {
	tests := []struct {
		types   []string
		objType string
		want    bool
	}{
		{[]string{"CLAS"}, "CLAS", true},
		{[]string{"CLAS"}, "CLAS/OC", true},
		{[]string{"CLAS", "PROG"}, "PROG", true},
		{[]string{"CLAS"}, "PROG", false},
		{[]string{"CLAS"}, "clas", true}, // case insensitive
	}

	for _, tt := range tests {
		t.Run(tt.objType, func(t *testing.T) {
			got := containsType(tt.types, tt.objType)
			if got != tt.want {
				t.Errorf("containsType(%v, %q) = %v, want %v", tt.types, tt.objType, got, tt.want)
			}
		})
	}
}

func TestFilterFunctions(t *testing.T) {
	t.Run("NameMatches", func(t *testing.T) {
		s := Search(nil).NameMatches("ZCL_.*_TEST")
		obj1 := ObjectRef{Name: "ZCL_MY_TEST"}
		obj2 := ObjectRef{Name: "ZCL_TEST"}

		if !s.applyFilters(obj1) {
			t.Error("expected ZCL_MY_TEST to match")
		}
		if s.applyFilters(obj2) {
			t.Error("expected ZCL_TEST to not match")
		}
	})

	t.Run("NameContains", func(t *testing.T) {
		s := Search(nil).NameContains("RAY")
		obj1 := ObjectRef{Name: "ZCL_RAY_TEST"}
		obj2 := ObjectRef{Name: "ZCL_TEST"}

		if !s.applyFilters(obj1) {
			t.Error("expected ZCL_RAY_TEST to match")
		}
		if s.applyFilters(obj2) {
			t.Error("expected ZCL_TEST to not match")
		}
	})

	t.Run("NamePrefix", func(t *testing.T) {
		s := Search(nil).NamePrefix("ZCL_RAY")
		obj1 := ObjectRef{Name: "ZCL_RAY_10"}
		obj2 := ObjectRef{Name: "ZCL_TEST"}

		if !s.applyFilters(obj1) {
			t.Error("expected ZCL_RAY_10 to match")
		}
		if s.applyFilters(obj2) {
			t.Error("expected ZCL_TEST to not match")
		}
	})

	t.Run("Exclude", func(t *testing.T) {
		s := Search(nil).Exclude(".*_TEST$")
		obj1 := ObjectRef{Name: "ZCL_RAY_10"}
		obj2 := ObjectRef{Name: "ZCL_RAY_TEST"}

		if !s.applyFilters(obj1) {
			t.Error("expected ZCL_RAY_10 to pass")
		}
		if s.applyFilters(obj2) {
			t.Error("expected ZCL_RAY_TEST to be excluded")
		}
	})
}

func TestTestRunner(t *testing.T) {
	t.Run("DefaultConfig", func(t *testing.T) {
		config := DefaultTestConfig()
		if !config.Harmless {
			t.Error("expected Harmless to be true")
		}
		if config.Dangerous {
			t.Error("expected Dangerous to be false")
		}
		if config.Long {
			t.Error("expected Long to be false")
		}
	})

	t.Run("Builder", func(t *testing.T) {
		runner := Test(nil).
			Class("ZCL_TEST").
			IncludeDangerous().
			IncludeLong().
			StopOnFirstFailure().
			Parallel(4)

		if len(runner.objects) != 1 {
			t.Errorf("expected 1 object, got %d", len(runner.objects))
		}
		if !runner.config.Dangerous {
			t.Error("expected Dangerous to be true")
		}
		if !runner.config.Long {
			t.Error("expected Long to be true")
		}
		if !runner.config.StopOnFirstFailure {
			t.Error("expected StopOnFirstFailure to be true")
		}
		if runner.config.Parallel != 4 {
			t.Errorf("expected Parallel 4, got %d", runner.config.Parallel)
		}
	})

	t.Run("BuildObjectURL", func(t *testing.T) {
		runner := Test(nil)

		tests := []struct {
			obj  ObjectRef
			want string
		}{
			{ObjectRef{Type: "CLAS", Name: "ZCL_TEST"}, "/sap/bc/adt/oo/classes/ZCL_TEST"},
			{ObjectRef{Type: "CLAS/OC", Name: "ZCL_TEST"}, "/sap/bc/adt/oo/classes/ZCL_TEST"},
			{ObjectRef{Type: "PROG", Name: "ZTEST"}, "/sap/bc/adt/programs/programs/ZTEST"},
			{ObjectRef{Type: "PROG/P", Name: "ZTEST"}, "/sap/bc/adt/programs/programs/ZTEST"},
			{ObjectRef{Type: "INTF", Name: "ZIF_TEST"}, "/sap/bc/adt/oo/interfaces/ZIF_TEST"},
			{ObjectRef{Type: "UNKNOWN", Name: "TEST"}, ""},
		}

		for _, tt := range tests {
			t.Run(tt.obj.Type+"_"+tt.obj.Name, func(t *testing.T) {
				got := runner.buildObjectURL(tt.obj)
				if got != tt.want {
					t.Errorf("buildObjectURL(%v) = %q, want %q", tt.obj, got, tt.want)
				}
			})
		}
	})
}

func TestWorkflowParsing(t *testing.T) {
	yamlData := `
name: test-workflow
description: Test workflow
variables:
  PACKAGE: "$ZRAY*"
steps:
  - name: find-classes
    action: search
    parameters:
      query: "ZCL_*"
      package: "${PACKAGE}"
    saveAs: classes
  - name: run-tests
    action: test
    parameters:
      objects: classes
    saveAs: testResults
  - name: check-results
    action: fail_if
    parameters:
      condition: "tests_failed:testResults"
`

	engine := NewWorkflowEngine(nil)
	workflow, err := engine.ParseWorkflow([]byte(yamlData))
	if err != nil {
		t.Fatalf("ParseWorkflow failed: %v", err)
	}

	if workflow.Name != "test-workflow" {
		t.Errorf("expected name 'test-workflow', got '%s'", workflow.Name)
	}
	if len(workflow.Steps) != 3 {
		t.Errorf("expected 3 steps, got %d", len(workflow.Steps))
	}
	if workflow.Variables["PACKAGE"] != "$ZRAY*" {
		t.Errorf("expected PACKAGE variable '$ZRAY*', got '%s'", workflow.Variables["PACKAGE"])
	}

	// Check first step
	step := workflow.Steps[0]
	if step.Action != "search" {
		t.Errorf("expected action 'search', got '%s'", step.Action)
	}
	if step.SaveAs != "classes" {
		t.Errorf("expected saveAs 'classes', got '%s'", step.SaveAs)
	}
}

func TestExecutionContext(t *testing.T) {
	ctx := context.Background()
	execCtx := NewExecutionContext(ctx, nil)

	t.Run("SetGet", func(t *testing.T) {
		execCtx.Set("test", "value")
		val, ok := execCtx.Get("test")
		if !ok {
			t.Error("expected value to exist")
		}
		if val != "value" {
			t.Errorf("expected 'value', got '%v'", val)
		}
	})

	t.Run("Variables", func(t *testing.T) {
		execCtx.SetVariable("VAR1", "hello")
		val := execCtx.GetVariable("VAR1")
		if val != "hello" {
			t.Errorf("expected 'hello', got '%s'", val)
		}
	})

	t.Run("DryRun", func(t *testing.T) {
		if execCtx.IsDryRun() {
			t.Error("expected dry run to be false initially")
		}
		execCtx.SetDryRun(true)
		if !execCtx.IsDryRun() {
			t.Error("expected dry run to be true")
		}
	})
}

func TestConditionEvaluation(t *testing.T) {
	ctx := context.Background()
	execCtx := NewExecutionContext(ctx, nil)
	engine := NewWorkflowEngine(nil)

	t.Run("exists", func(t *testing.T) {
		execCtx.Set("myVar", "value")
		if !engine.evaluateCondition(execCtx, "exists:myVar") {
			t.Error("expected exists:myVar to be true")
		}
		if engine.evaluateCondition(execCtx, "exists:nonExistent") {
			t.Error("expected exists:nonExistent to be false")
		}
	})

	t.Run("empty", func(t *testing.T) {
		execCtx.Set("emptyList", []ObjectRef{})
		execCtx.Set("fullList", []ObjectRef{{Name: "test"}})

		if !engine.evaluateCondition(execCtx, "empty:emptyList") {
			t.Error("expected empty:emptyList to be true")
		}
		if engine.evaluateCondition(execCtx, "empty:fullList") {
			t.Error("expected empty:fullList to be false")
		}
	})

	t.Run("not_empty", func(t *testing.T) {
		execCtx.Set("emptyList", []ObjectRef{})
		execCtx.Set("fullList", []ObjectRef{{Name: "test"}})

		if engine.evaluateCondition(execCtx, "not_empty:emptyList") {
			t.Error("expected not_empty:emptyList to be false")
		}
		if !engine.evaluateCondition(execCtx, "not_empty:fullList") {
			t.Error("expected not_empty:fullList to be true")
		}
	})
}

func TestPipelineBuilder(t *testing.T) {
	pipeline := NewPipeline(nil, "test-pipeline").
		Stage("discover").
			Search("ZCL_*", "classes").
			Then().
		Stage("test").
			DependsOn("discover").
			Test("classes", "results").
			FailIfTestsFailed("results").
			Then().
		Build()

	if pipeline.Name != "test-pipeline" {
		t.Errorf("expected name 'test-pipeline', got '%s'", pipeline.Name)
	}
	if len(pipeline.Stages) != 2 {
		t.Errorf("expected 2 stages, got %d", len(pipeline.Stages))
	}

	// Check discover stage
	discover := pipeline.Stages[0]
	if discover.Name != "discover" {
		t.Errorf("expected stage name 'discover', got '%s'", discover.Name)
	}
	if len(discover.Steps) != 1 {
		t.Errorf("expected 1 step in discover, got %d", len(discover.Steps))
	}

	// Check test stage
	test := pipeline.Stages[1]
	if test.Name != "test" {
		t.Errorf("expected stage name 'test', got '%s'", test.Name)
	}
	if len(test.DependsOn) != 1 || test.DependsOn[0] != "discover" {
		t.Errorf("expected dependsOn ['discover'], got %v", test.DependsOn)
	}
	if len(test.Steps) != 2 {
		t.Errorf("expected 2 steps in test, got %d", len(test.Steps))
	}
}

func TestBatchBuilder(t *testing.T) {
	t.Run("Objects", func(t *testing.T) {
		batch := Batch(nil).Objects(
			ObjectRef{Name: "ZCL_A"},
			ObjectRef{Name: "ZCL_B"},
		)
		if len(batch.objects) != 2 {
			t.Errorf("expected 2 objects, got %d", len(batch.objects))
		}
	})

	t.Run("Transport", func(t *testing.T) {
		batch := Batch(nil).Transport("DEVK900123")
		if batch.transport != "DEVK900123" {
			t.Errorf("expected transport 'DEVK900123', got '%s'", batch.transport)
		}
	})

	t.Run("DryRun", func(t *testing.T) {
		batch := Batch(nil).DryRun()
		if !batch.dryRun {
			t.Error("expected dryRun to be true")
		}
	})
}

func TestTestSummary(t *testing.T) {
	summary := &TestSummary{
		TotalObjects:  5,
		TestedObjects: 5,
		PassedObjects: 3,
		FailedObjects: 2,
		TotalTests:    20,
		PassedTests:   15,
		FailedTests:   5,
	}

	if summary.TotalObjects != 5 {
		t.Errorf("expected TotalObjects 5, got %d", summary.TotalObjects)
	}
	if summary.PassedTests+summary.FailedTests != summary.TotalTests {
		t.Error("passed + failed should equal total")
	}
}

func TestObjectRef(t *testing.T) {
	obj := ObjectRef{
		Type:    TypeClass,
		Name:    "ZCL_TEST",
		Package: "$TMP",
	}

	if obj.Type != "CLAS" {
		t.Errorf("expected type 'CLAS', got '%s'", obj.Type)
	}
	if obj.Name != "ZCL_TEST" {
		t.Errorf("expected name 'ZCL_TEST', got '%s'", obj.Name)
	}
}

# Phase 5 Implementation Plan: TAS-Style Debugging

**Target:** Q1 2026 (v2.14 - v2.16)
**Status:** Planning

---

## Overview

Phase 5 introduces scriptable debugging inspired by Tool-Assisted Speedruns (TAS):
- Lua scripting engine
- Variable history recording
- Checkpoint/restore system
- Watchpoint scripting

---

## Milestone 1: Lua Scripting (v2.14)

**Target:** January 2026
**Duration:** 2 weeks

### Tasks

#### 1.1 Lua Engine Integration
```
pkg/scripting/
├── lua.go           # Lua VM wrapper
├── bindings.go      # MCP tool bindings
├── repl.go          # Interactive REPL
└── scripting_test.go
```

**Implementation:**
```go
// pkg/scripting/lua.go
package scripting

import (
    lua "github.com/yuin/gopher-lua"
)

type LuaEngine struct {
    L      *lua.LState
    client *adt.Client
}

func NewLuaEngine(client *adt.Client) *LuaEngine {
    L := lua.NewState()
    engine := &LuaEngine{L: L, client: client}
    engine.registerBindings()
    return engine
}

func (e *LuaEngine) Execute(script string) error {
    return e.L.DoString(script)
}

func (e *LuaEngine) ExecuteFile(path string) error {
    return e.L.DoFile(path)
}
```

#### 1.2 Tool Bindings
Expose all MCP tools to Lua:

```go
// pkg/scripting/bindings.go
func (e *LuaEngine) registerBindings() {
    // Search
    e.L.SetGlobal("searchObject", e.L.NewFunction(e.luaSearchObject))
    e.L.SetGlobal("grepObjects", e.L.NewFunction(e.luaGrepObjects))

    // Source
    e.L.SetGlobal("getSource", e.L.NewFunction(e.luaGetSource))
    e.L.SetGlobal("writeSource", e.L.NewFunction(e.luaWriteSource))
    e.L.SetGlobal("editSource", e.L.NewFunction(e.luaEditSource))

    // Debug
    e.L.SetGlobal("setBreakpoint", e.L.NewFunction(e.luaSetBreakpoint))
    e.L.SetGlobal("waitForBreakpoint", e.L.NewFunction(e.luaWaitForBreakpoint))
    e.L.SetGlobal("stepOver", e.L.NewFunction(e.luaStepOver))
    e.L.SetGlobal("stepInto", e.L.NewFunction(e.luaStepInto))
    e.L.SetGlobal("getVariables", e.L.NewFunction(e.luaGetVariables))
    e.L.SetGlobal("getStack", e.L.NewFunction(e.luaGetStack))

    // Utility
    e.L.SetGlobal("print", e.L.NewFunction(e.luaPrint))
    e.L.SetGlobal("sleep", e.L.NewFunction(e.luaSleep))
    e.L.SetGlobal("json", e.L.NewFunction(e.luaJSON))
}

func (e *LuaEngine) luaGetSource(L *lua.LState) int {
    objType := L.ToString(1)
    name := L.ToString(2)

    source, err := e.client.GetSource(context.Background(), objType, name, "", "")
    if err != nil {
        L.Push(lua.LNil)
        L.Push(lua.LString(err.Error()))
        return 2
    }

    L.Push(lua.LString(source))
    return 1
}
```

#### 1.3 Interactive REPL
```go
// pkg/scripting/repl.go
func (e *LuaEngine) REPL() {
    reader := bufio.NewReader(os.Stdin)
    fmt.Println("vsp Lua REPL. Type 'exit' to quit.")

    for {
        fmt.Print("lua> ")
        line, _ := reader.ReadString('\n')
        line = strings.TrimSpace(line)

        if line == "exit" {
            break
        }

        if err := e.Execute(line); err != nil {
            fmt.Printf("Error: %v\n", err)
        }
    }
}
```

#### 1.4 CLI Integration
```go
// cmd/vsp/main.go
case "lua":
    if len(os.Args) > 2 {
        // Execute script file
        engine.ExecuteFile(os.Args[2])
    } else {
        // Interactive REPL
        engine.REPL()
    }
```

### Deliverables
- [ ] `pkg/scripting/lua.go` - Lua VM wrapper
- [ ] `pkg/scripting/bindings.go` - All tool bindings
- [ ] `pkg/scripting/repl.go` - Interactive REPL
- [ ] `cmd/vsp/lua.go` - CLI integration
- [ ] `examples/scripts/` - Example Lua scripts
- [ ] Tests and documentation

### Example Scripts

```lua
-- examples/scripts/find_unused_methods.lua
-- Find methods that are never called

local class_name = arg[1] or "ZCL_MY_CLASS"

-- Get all methods
local source = getSource("CLAS", class_name)
local methods = {}
for method in source:gmatch("METHODS%s+(%w+)") do
    methods[method] = { defined = true, called = false }
end

-- Check call graph
local callers = getCallersOf("/sap/bc/adt/oo/classes/" .. class_name:lower(), 3)
for _, edge in ipairs(flattenCallGraph(callers)) do
    local method = edge.callee_name:match("=>(%w+)")
    if method and methods[method] then
        methods[method].called = true
    end
end

-- Report unused
print("Unused methods in " .. class_name .. ":")
for method, info in pairs(methods) do
    if not info.called then
        print("  - " .. method)
    end
end
```

```lua
-- examples/scripts/debug_until_crash.lua
-- Keep stepping until we hit an exception

local program = arg[1] or "ZTEST_PROGRAM"
local max_steps = tonumber(arg[2]) or 1000

-- Set exception breakpoint
setBreakpoint({ kind = "exception", exception = "*" })

-- Start execution
print("Waiting for debuggee...")
local event = waitForBreakpoint(60)

if not event then
    print("No debuggee caught")
    return
end

print("Attached to: " .. event.debuggee_id)
attach(event.debuggee_id)

-- Step and record
local history = {}
for step = 1, max_steps do
    local vars = getVariables("local")
    local stack = getStack()

    history[step] = {
        location = stack[1].program .. ":" .. stack[1].line,
        variables = vars
    }

    local result = stepOver()
    if result.status == "stopped" then
        print("Execution stopped at step " .. step)
        break
    end
end

-- Save history
local file = io.open("debug_history.json", "w")
file:write(json.encode(history))
file:close()

print("History saved to debug_history.json")
```

---

## Milestone 2: Variable History Recording (v2.15)

**Target:** February 2026
**Duration:** 2 weeks

### Tasks

#### 2.1 Execution Frame Structure
```go
// pkg/adt/recorder.go
package adt

type ExecutionRecorder struct {
    frames      []ExecutionFrame
    startTime   time.Time
    maxFrames   int
    deltaMode   bool
    lastFrame   *ExecutionFrame
}

type ExecutionFrame struct {
    StepNumber  int                    `json:"step"`
    Timestamp   time.Time              `json:"timestamp"`
    Location    CodeLocation           `json:"location"`
    Variables   map[string]Variable    `json:"variables"`
    Changed     []string               `json:"changed,omitempty"`
    DBOps       []DBOperation          `json:"db_ops,omitempty"`
    RFCCalls    []RFCCall              `json:"rfc_calls,omitempty"`
}

type Variable struct {
    Name  string      `json:"name"`
    Type  string      `json:"type"`
    Value interface{} `json:"value"`
}

type CodeLocation struct {
    Program string `json:"program"`
    Include string `json:"include,omitempty"`
    Line    int    `json:"line"`
    Column  int    `json:"column,omitempty"`
}
```

#### 2.2 Recording Integration
```go
// Integration with debug session
func (r *ExecutionRecorder) RecordStep(session *DebugSession) error {
    // Get current location
    stack, err := session.GetStack()
    if err != nil {
        return err
    }

    // Get variables
    vars, err := session.GetVariables("local")
    if err != nil {
        return err
    }

    frame := ExecutionFrame{
        StepNumber: len(r.frames),
        Timestamp:  time.Now(),
        Location: CodeLocation{
            Program: stack[0].Program,
            Line:    stack[0].Line,
        },
        Variables: r.convertVariables(vars),
    }

    // Compute delta if enabled
    if r.deltaMode && r.lastFrame != nil {
        frame.Changed = r.computeChanges(r.lastFrame.Variables, frame.Variables)
    }

    r.frames = append(r.frames, frame)
    r.lastFrame = &frame

    return nil
}
```

#### 2.3 Storage Format
```go
// pkg/adt/recording.go
type ExecutionRecording struct {
    ID          string           `json:"id"`
    Object      string           `json:"object"`
    Method      string           `json:"method,omitempty"`
    StartTime   time.Time        `json:"start_time"`
    EndTime     time.Time        `json:"end_time"`
    FrameCount  int              `json:"frame_count"`
    Frames      []ExecutionFrame `json:"frames"`
    Metadata    RecordingMeta    `json:"metadata"`
}

type RecordingMeta struct {
    User        string   `json:"user"`
    System      string   `json:"system"`
    Client      string   `json:"client"`
    Tags        []string `json:"tags,omitempty"`
    Description string   `json:"description,omitempty"`
}

func (r *ExecutionRecording) Save(path string) error {
    data, err := json.MarshalIndent(r, "", "  ")
    if err != nil {
        return err
    }
    return os.WriteFile(path, data, 0644)
}

func LoadRecording(path string) (*ExecutionRecording, error) {
    data, err := os.ReadFile(path)
    if err != nil {
        return nil, err
    }
    var rec ExecutionRecording
    err = json.Unmarshal(data, &rec)
    return &rec, err
}
```

#### 2.4 MCP Tool: RecordExecution
```go
// New tool in server.go
case "RecordExecution":
    objectURI, _ := getString(args, "object_uri")
    maxFrames, _ := getInt(args, "max_frames", 1000)
    captureDB, _ := getBool(args, "capture_db", true)

    recording, err := s.client.RecordExecution(ctx, &RecordExecutionOptions{
        ObjectURI:  objectURI,
        MaxFrames:  maxFrames,
        CaptureDB:  captureDB,
        CaptureRFC: true,
    })
    // ...
```

### Deliverables
- [ ] `pkg/adt/recorder.go` - ExecutionRecorder
- [ ] `pkg/adt/recording.go` - Recording storage/load
- [ ] MCP tool: `RecordExecution`
- [ ] MCP tool: `LoadRecording`
- [ ] MCP tool: `ListRecordings`
- [ ] Lua bindings for recording
- [ ] Tests and documentation

---

## Milestone 3: Checkpoint System (v2.16)

**Target:** March 2026
**Duration:** 1 week

### Tasks

#### 3.1 Checkpoint Structure
```go
// pkg/adt/checkpoint.go
type Checkpoint struct {
    ID          string                 `json:"id"`
    StepNumber  int                    `json:"step"`
    Timestamp   time.Time              `json:"timestamp"`
    Location    CodeLocation           `json:"location"`
    Variables   map[string]Variable    `json:"variables"`
    CallStack   []StackFrame           `json:"call_stack"`
}

type CheckpointStore struct {
    checkpoints map[string]*Checkpoint
    recording   *ExecutionRecording
}

func (s *CheckpointStore) Save(name string, frame *ExecutionFrame, stack []StackFrame) {
    s.checkpoints[name] = &Checkpoint{
        ID:         name,
        StepNumber: frame.StepNumber,
        Timestamp:  frame.Timestamp,
        Location:   frame.Location,
        Variables:  frame.Variables,
        CallStack:  stack,
    }
}

func (s *CheckpointStore) Get(name string) *Checkpoint {
    return s.checkpoints[name]
}

func (s *CheckpointStore) Compare(name1, name2 string) *CheckpointDiff {
    cp1 := s.checkpoints[name1]
    cp2 := s.checkpoints[name2]
    return computeDiff(cp1, cp2)
}
```

#### 3.2 Lua Integration
```lua
-- Checkpoint API
saveCheckpoint("before_calculation")

-- ... do stuff ...

saveCheckpoint("after_calculation")

-- Compare
local diff = compareCheckpoints("before_calculation", "after_calculation")
for _, change in ipairs(diff.changes) do
    print(change.variable .. ": " .. change.old_value .. " -> " .. change.new_value)
end

-- Navigate
local cp = getCheckpoint("before_calculation")
print("At step " .. cp.step .. ", location: " .. cp.location)
for name, var in pairs(cp.variables) do
    print("  " .. name .. " = " .. tostring(var.value))
end
```

### Deliverables
- [ ] `pkg/adt/checkpoint.go` - Checkpoint store
- [ ] MCP tool: `SaveCheckpoint`
- [ ] MCP tool: `GetCheckpoint`
- [ ] MCP tool: `CompareCheckpoints`
- [ ] MCP tool: `ListCheckpoints`
- [ ] Lua bindings
- [ ] Tests and documentation

---

## Milestone 4: Watchpoint Scripting (v2.16)

**Target:** March 2026
**Duration:** 1 week

### Tasks

#### 4.1 Scriptable Watchpoints
```go
// pkg/adt/watchpoint.go
type ScriptableWatchpoint struct {
    Variable  string
    Condition string  // Lua expression
    Callback  string  // Lua function name
}

func (w *ScriptableWatchpoint) ShouldTrigger(engine *LuaEngine, oldVal, newVal interface{}) bool {
    if w.Condition == "" {
        return true  // Trigger on any change
    }

    engine.L.SetGlobal("old_value", toLuaValue(oldVal))
    engine.L.SetGlobal("new_value", toLuaValue(newVal))

    err := engine.Execute("_wp_result = " + w.Condition)
    if err != nil {
        return false
    }

    result := engine.L.GetGlobal("_wp_result")
    return lua.LVAsBool(result)
}
```

#### 4.2 Lua API
```lua
-- Watch for specific condition
setWatchpoint("LV_AMOUNT", {
    condition = "new_value < 0",
    callback = function(old_val, new_val, location)
        print("LV_AMOUNT went negative at " .. location)
        print("  Was: " .. old_val .. ", Now: " .. new_val)
        saveCheckpoint("negative_amount")
    end
})

-- Watch for any change
setWatchpoint("GV_DEBUG_FLAG", {
    callback = function(old_val, new_val, location)
        if new_val == true then
            print("Debug mode enabled!")
        end
    end
})

-- Remove watchpoint
clearWatchpoint("LV_AMOUNT")
```

### Deliverables
- [ ] `pkg/adt/watchpoint.go` - Scriptable watchpoints
- [ ] Integration with recorder
- [ ] Lua API
- [ ] Tests and documentation

---

## Testing Strategy

### Unit Tests
- Lua engine initialization and shutdown
- Each binding function individually
- Recording serialization/deserialization
- Checkpoint comparison logic

### Integration Tests
- End-to-end script execution
- Recording during debug session
- Checkpoint save/restore workflow

### Example Test
```go
func TestLuaGetSource(t *testing.T) {
    client := getTestClient(t)
    engine := scripting.NewLuaEngine(client)
    defer engine.Close()

    err := engine.Execute(`
        source = getSource("PROG", "ZTEST_PROGRAM")
        assert(source ~= nil, "Source should not be nil")
        assert(#source > 0, "Source should not be empty")
    `)

    require.NoError(t, err)
}
```

---

## Documentation

### New Documentation Files
- `docs/scripting.md` - Lua scripting guide
- `docs/recording.md` - Variable history usage
- `docs/checkpoints.md` - Checkpoint system
- `examples/scripts/README.md` - Example scripts

### README Updates
- New section on scripting
- Link to documentation
- Example script snippets

---

## Dependencies

### Go Packages
```go
require (
    github.com/yuin/gopher-lua v1.1.0
    github.com/layeh/gopher-luar v1.0.0  // Easier struct binding
)
```

### File Structure
```
pkg/
├── scripting/
│   ├── lua.go
│   ├── bindings.go
│   ├── repl.go
│   └── scripting_test.go
├── adt/
│   ├── recorder.go      (new)
│   ├── recording.go     (new)
│   ├── checkpoint.go    (new)
│   └── watchpoint.go    (new)
examples/
└── scripts/
    ├── README.md
    ├── find_unused_methods.lua
    ├── debug_until_crash.lua
    ├── trace_variable.lua
    └── automated_rca.lua
docs/
├── scripting.md
├── recording.md
└── checkpoints.md
```

---

## Success Criteria

### Phase 5 Complete When:
1. ✅ Lua REPL works interactively
2. ✅ All MCP tools accessible from Lua
3. ✅ Recording captures 1000+ frames without performance issues
4. ✅ Checkpoints can be saved and compared
5. ✅ Watchpoints trigger correctly
6. ✅ Example scripts demonstrate all features
7. ✅ Documentation complete

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Lua performance | Medium | Use LuaJIT if needed |
| Recording size | High | Delta compression, configurable depth |
| Debug session timeout | High | Implement keepalive |
| SAP version differences | Medium | Feature detection |

---

*Phase 5 sets the foundation for Phase 6 (Test Extraction) and Phase 7 (Playground).*

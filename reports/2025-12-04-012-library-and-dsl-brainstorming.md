# Library API & DSL Options Brainstorming

**Date:** 2025-12-04
**Report ID:** 012
**Subject:** Exposing ADT functions as library API and potential DSL approaches
**Related Documents:** Report 009 (Library Architecture & Caching Strategy)

---

## Executive Summary

This report explores two complementary approaches for making vsp's ADT functionality more accessible for automation:

1. **Library API**: Expose core functions as importable Go packages (already partially done in `pkg/adt/`)
2. **Domain-Specific Language (DSL)**: Create a higher-level abstraction for SAP automation workflows

**Key Recommendations:**
- **Phase 1**: Polish the existing Go library API with fluent builder patterns
- **Phase 2**: Create a YAML/JSON declarative config for common workflows
- **Phase 3**: Consider Ruby internal DSL for maximum expressiveness (if demand exists)

---

## Part 1: Library API Analysis

### Current State

The `pkg/adt/` package already provides a clean Go API:

```go
// Current API pattern
client := adt.NewClient(baseURL, username, password, opts...)
results, err := client.SearchObject(ctx, "Z*", 100)
source, err := client.GetProgram(ctx, "ZTEST")
classMap, err := client.GetClass(ctx, "ZCL_TEST")
```

### What Makes a Good Library API?

| Principle | Current State | Improvement Opportunity |
|-----------|---------------|------------------------|
| **Discoverability** | Good - methods on Client | Add code completion hints |
| **Consistency** | Good - similar patterns | Standardize return types |
| **Error handling** | Good - Go idioms | Add structured errors |
| **Testability** | Good - interfaces | Already mockable |
| **Fluent API** | Partial | Add builder patterns |
| **Documentation** | Basic | Add godoc examples |

### Proposed Library Improvements

#### 1. Fluent Builder Pattern

```go
// Instead of:
results, err := client.SearchObject(ctx, "Z*", 100)

// Provide fluent API:
results, err := client.Search().
    Query("Z*").
    Types("CLAS", "PROG", "FUNC").
    InPackages("$TMP", "$ZRAY*").
    MaxResults(100).
    Execute(ctx)
```

#### 2. Workflow Builders

```go
// Mass update workflow
workflow := adt.NewWorkflow(client).
    ForPackage("$ZRAY*").
    SelectClasses().
    WithPattern("ZCL_RAY_*").
    Do(func(class *adt.Class) error {
        // Transform each class
        source := strings.Replace(class.Source, "old", "new", -1)
        return class.Update(source)
    }).
    WithTransport("DEVK900123").
    Execute(ctx)
```

#### 3. Pipeline Pattern (inspired by Unix pipes)

```go
// Composable operations
result := adt.Pipe(client).
    FindClasses("ZCL_*").
    Filter(adt.HasMethod("EXECUTE")).
    Map(adt.ExtractDependencies).
    Reduce(adt.BuildDependencyGraph).
    Execute(ctx)
```

### Library API: Pros & Cons

| Pros | Cons |
|------|------|
| Type-safe, compile-time checks | Go-only (requires Go knowledge) |
| Full IDE support | More verbose than DSL |
| Easy testing | Can't modify without recompile |
| No runtime overhead | Steeper learning curve |
| Importable by other Go projects | - |

---

## Part 2: DSL Options Analysis

### What is a DSL?

> "A Domain-Specific Language is a programming language of limited expressiveness focused on a particular domain." - [Martin Fowler](https://martinfowler.com/books/dsl.html)

DSLs make domain tasks easier by removing extraneous code and making intent clear.

### DSL Types

| Type | Description | Examples | Effort |
|------|-------------|----------|--------|
| **Internal DSL** | Uses host language syntax | RSpec (Ruby), Goa (Go) | Medium |
| **External DSL** | Custom syntax + parser | SQL, Cucumber | High |
| **Declarative Config** | YAML/JSON/TOML | Ansible, K8s manifests | Low |

---

## Option A: Go Internal DSL (Fluent API)

Use Go's functional programming features to create expressive APIs.

### Example: ADT Operations DSL

```go
package main

import "github.com/oisee/vibing-steampunk/pkg/dsl"

func main() {
    sap := dsl.Connect("http://host:50000").
        WithUser("developer").
        WithPassword("secret").
        Client("001")

    // Read operations
    classes := sap.Classes().
        InPackage("$ZRAY*").
        NameLike("ZCL_RAY_*").
        WithMethod("EXECUTE").
        List()

    // Batch update
    sap.Batch().
        Select(classes).
        Transform(func(c *Class) string {
            return addCopyrightHeader(c.Source)
        }).
        Transport("DEVK900123").
        Activate().
        Run()

    // Graph traversal
    deps := sap.Graph().
        From("ZCL_RAY_10_SPIDER").
        TraverseDown(3).
        FilterStandard().
        AsDOT()
}
```

### Go DSL Implementation Approaches

#### 1. Functional Options Pattern (Current)

```go
client := adt.NewClient(url, user, pass,
    adt.WithClient("001"),
    adt.WithLanguage("EN"),
    adt.WithInsecure(true),
)
```

#### 2. Method Chaining (Builder)

```go
client := adt.NewClientBuilder().
    URL("http://host:50000").
    User("developer").
    Password("secret").
    Client("001").
    Build()
```

#### 3. Using [Goa Framework](https://github.com/goadesign/goa) Style

```go
var _ = Service("adt", func() {
    Method("search", func() {
        Payload(func() {
            Field(1, "query", String, "Search pattern")
            Field(2, "max_results", Int, "Max results", func() {
                Default(100)
            })
        })
        Result(ArrayOf(SearchResult))
    })
})
```

### Go DSL: Pros & Cons

| Pros | Cons |
|------|------|
| Type-safe | Still verbose |
| IDE autocomplete | Limited expressiveness |
| Compile-time validation | Chaining can be awkward |
| No external dependencies | - |

**Verdict**: Good for programmatic use, but limited for scripting scenarios.

---

## Option B: Ruby Internal DSL

Ruby is [famous for DSLs](https://thoughtbot.com/blog/writing-a-domain-specific-language-in-ruby) (Rake, Chef, Puppet, RSpec, Rails routes). Its metaprogramming makes DSLs natural.

### Example: ABAP Automation DSL in Ruby

```ruby
# abap_tasks.rb
require 'vsp'

# Connect to SAP
sap "development" do
  url "http://host:50000"
  user "developer"
  password ENV['SAP_PASSWORD']
  client "001"
end

# Define reusable tasks
task :update_copyright do
  classes in_package: "$ZRAY*", matching: "ZCL_RAY_*" do |cls|
    unless cls.source.include?("(c) 2025")
      cls.prepend_header <<~ABAP
        *----------------------------------------------------------------------*
        * (c) 2025 My Company
        *----------------------------------------------------------------------*
      ABAP
      cls.save transport: "DEVK900123"
    end
  end
end

task :run_tests do
  packages "$ZRAY*" do |pkg|
    pkg.classes.each do |cls|
      result = cls.run_unit_tests
      fail "Tests failed for #{cls.name}" unless result.success?
    end
  end
end

task :dependency_report do
  graph from: "ZCL_RAY_10_SPIDER", depth: 5 do |g|
    g.filter_standard_apis
    g.export_dot "spider_deps.dot"
    g.export_html "spider_deps.html"
  end
end

# CI/CD pipeline
pipeline :deploy do
  stage :validate do
    run_syntax_check
    run_tests
  end

  stage :deploy do
    transport "DEVK900123" do
      release
      import to: "QAS"
    end
  end
end
```

### Ruby DSL Implementation

Would require a Ruby gem that wraps the Go binary or calls ADT APIs directly:

```ruby
# lib/vsp.rb
module VSP
  class DSL
    def self.sap(name, &block)
      connection = Connection.new(name)
      connection.instance_eval(&block)
      @connections[name] = connection
    end

    def self.task(name, &block)
      Task.new(name, &block).tap { |t| @tasks[name] = t }
    end

    def self.classes(in_package:, matching: nil, &block)
      # Call Go binary or HTTP API
      results = execute("search", package: in_package, pattern: matching)
      results.each { |cls| block.call(ClassProxy.new(cls)) }
    end
  end
end
```

### Ruby DSL: Pros & Cons

| Pros | Cons |
|------|------|
| Extremely expressive | Requires Ruby runtime |
| Natural syntax | Two language ecosystems |
| Rich metaprogramming | Harder to debug |
| Familiar to DevOps (Chef/Puppet) | Type safety lost |
| Easy to extend | Performance overhead |

**Verdict**: Best expressiveness, but adds complexity. Consider if user demand exists.

---

## Option C: Python DSL / SDK

Python is popular in DevOps and has decent DSL capabilities.

### Example: Python SDK

```python
# abap_automation.py
from vsp import SAP, Pipeline

# Connect
sap = SAP.connect(
    url="http://host:50000",
    user="developer",
    password=os.environ["SAP_PASSWORD"],
    client="001"
)

# Search and transform
for cls in sap.classes(package="$ZRAY*", pattern="ZCL_RAY_*"):
    if "EXECUTE" in cls.methods:
        cls.source = add_copyright(cls.source)
        cls.save(transport="DEVK900123")

# Pipeline definition
pipeline = Pipeline("deploy") \
    .stage("validate") \
        .syntax_check() \
        .unit_tests() \
    .stage("deploy") \
        .transport("DEVK900123") \
        .release() \
        .import_to("QAS")

pipeline.run()
```

### Python DSL: Pros & Cons

| Pros | Cons |
|------|------|
| Huge ecosystem | Less DSL-friendly than Ruby |
| Popular in DevOps | Indentation sensitivity |
| Easy learning curve | Performance concerns |
| Type hints available | - |

**Verdict**: Good alternative if Ruby feels too niche. More mainstream adoption.

---

## Option D: Lua Embedded DSL

Lua is designed for embedding. Used in games (WoW), networking (nginx), and config.

### Example: Lua Automation Script

```lua
-- abap_deploy.lua
local sap = connect {
    url = "http://host:50000",
    user = "developer",
    password = os.getenv("SAP_PASSWORD"),
    client = "001"
}

-- Find and update classes
for _, cls in sap:classes { package = "$ZRAY*", pattern = "ZCL_RAY_*" } do
    if cls:has_method("EXECUTE") then
        cls:prepend_header([[
*----------------------------------------------------------------------*
* (c) 2025 My Company
*----------------------------------------------------------------------*
]])
        cls:save { transport = "DEVK900123" }
    end
end

-- Define pipeline
pipeline "deploy" {
    stage "validate" {
        syntax_check(),
        unit_tests(),
    },
    stage "deploy" {
        transport("DEVK900123"):release():import_to("QAS")
    }
}
```

### Lua DSL: Pros & Cons

| Pros | Cons |
|------|------|
| Tiny footprint (~200KB) | Less familiar to most devs |
| Fast (LuaJIT) | Smaller ecosystem |
| Easy to embed in Go | Limited stdlib |
| Sandboxable (safe execution) | 1-indexed arrays |
| Popular in gaming/embedded | - |

**Verdict**: Interesting for embedded scripting within vsp itself. Good sandboxing.

---

## Option E: YAML/JSON Declarative Config

Simplest approach - declarative configuration files.

### Example: YAML Workflow Definition

```yaml
# .vsp/workflows/deploy.yaml
name: deploy
description: Deploy ZRAY components to QAS

connection:
  url: ${SAP_URL}
  user: ${SAP_USER}
  password: ${SAP_PASSWORD}
  client: "001"

workflows:
  validate:
    - action: search
      type: class
      package: "$ZRAY*"
      pattern: "ZCL_RAY_*"
      save_as: classes

    - action: syntax_check
      objects: ${classes}
      fail_on_error: true

    - action: unit_test
      objects: ${classes}
      fail_on_error: true

  update_headers:
    - action: search
      type: class
      package: "$ZRAY*"
      save_as: classes

    - action: transform
      objects: ${classes}
      prepend: |
        *----------------------------------------------------------------------*
        * (c) 2025 My Company
        *----------------------------------------------------------------------*
      condition: not_contains("(c) 2025")
      transport: DEVK900123

  deploy:
    depends_on: [validate]
    - action: transport
      request: DEVK900123
      operations:
        - release
        - import:
            target: QAS

  graph_report:
    - action: graph
      from: ZCL_RAY_10_SPIDER
      depth: 5
      filter: standard_only
      output:
        - format: dot
          path: reports/spider.dot
        - format: html
          path: reports/spider.html
```

### CLI Usage

```bash
# Run workflow
vsp workflow run validate

# Run with variables
vsp workflow run deploy --var transport=DEVK900234

# Dry run
vsp workflow run update_headers --dry-run
```

### YAML Config: Pros & Cons

| Pros | Cons |
|------|------|
| No programming required | Limited logic/conditionals |
| Easy to version control | Can get verbose |
| CI/CD friendly | No loops/functions |
| Declarative = predictable | Error messages unclear |
| Familiar (K8s, Ansible, GitHub Actions) | - |

**Verdict**: Best for common workflows. Combine with library for complex cases.

---

## Option F: External DSL (Custom Syntax)

Build a completely custom language with its own parser.

### Example: Custom ABAP Automation Language

```
# deploy.adl (ABAP Domain Language)

connect to "development" {
  url: "http://host:50000"
  user: "developer"
  client: 001
}

workflow validate {
  for each class in package "$ZRAY*" matching "ZCL_RAY_*" {
    check syntax
    run unit tests
    fail if errors
  }
}

workflow update_headers {
  for each class in package "$ZRAY*" {
    if not contains "(c) 2025" {
      prepend """
        *----------------------------------------------------------------------*
        * (c) 2025 My Company
        *----------------------------------------------------------------------*
      """
      save with transport "DEVK900123"
    }
  }
}

graph spider_deps {
  from "ZCL_RAY_10_SPIDER"
  traverse down 5 levels
  filter standard APIs only
  export as DOT to "spider.dot"
  export as HTML to "spider.html"
}
```

### External DSL: Pros & Cons

| Pros | Cons |
|------|------|
| Perfect syntax for domain | Significant development effort |
| Most readable | Must build parser |
| No host language constraints | Tooling (syntax highlight, etc.) |
| Can add IDE support | Learning curve for users |

**Verdict**: High effort, only justified with significant user base. Park for future.

---

## Part 3: Target Audience Analysis

### Who Would Use This?

| Persona | Needs | Best Option |
|---------|-------|-------------|
| **ABAP Developer** | Quick scripts, one-off tasks | Go Library, CLI |
| **DevOps Engineer** | CI/CD pipelines, automation | YAML Config, Python |
| **SAP Basis Admin** | Mass changes, reporting | YAML Config, CLI |
| **Enterprise Architect** | Dependency analysis | Go Library, Graph tools |
| **AI/LLM Agent** | Programmatic access | MCP (current), Go Library |

### Use Case Matrix

| Use Case | CLI | Go Lib | YAML | Ruby DSL | Python |
|----------|-----|--------|------|----------|--------|
| Ad-hoc queries | +++ | ++ | + | ++ | ++ |
| CI/CD pipelines | ++ | + | +++ | ++ | ++ |
| Mass updates | + | ++ | +++ | +++ | ++ |
| Custom logic | + | +++ | + | +++ | +++ |
| LLM integration | + | +++ | + | + | ++ |
| Learning curve | +++ | ++ | +++ | ++ | +++ |

---

## Part 4: Comparison with Existing Tools

### SAP Ecosystem Tools

| Tool | Type | Strengths | Weaknesses |
|------|------|-----------|------------|
| **[abapGit](https://github.com/abapGit/abapGit)** | Git client | Version control | Requires ABAP system |
| **[SAP CLI](https://sap.github.io/fundamental-tools/)** | Python CLI | Official SAP tool | Limited ADT coverage |
| **[Project Piper](https://github.com/SAP-samples/abap-platform-ci-cd-samples)** | CI/CD framework | Enterprise ready | Complex setup |
| **[gCTS](https://help.sap.com/docs/ABAP_PLATFORM_NEW/4a368c163b08418890a406d413933ba7/26c9c6c5a89244cb9506c253d36c3fda.html)** | Git CTS | SAP supported | Cloud focus |
| **vsp** | MCP Server | AI-native, 45 tools | Young project |

### How vsp Library/DSL Would Differentiate

1. **AI-First**: Designed for LLM agents (MCP protocol)
2. **Single Binary**: No Python/Ruby runtime needed
3. **Focused Mode**: Curated 19-tool subset for safety
4. **Safety Controls**: Operation filtering, package restrictions
5. **Graph Analysis**: Built-in dependency traversal

---

## Part 5: Implementation Recommendations

### Recommended Approach: Layered Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    User-Facing Interfaces                        │
├─────────────────────────────────────────────────────────────────┤
│  CLI      │  MCP Server  │  YAML Workflows  │  Ruby/Python SDK  │
│  (vsp)    │  (current)   │  (new)           │  (future)         │
└─────┬─────┴──────┬───────┴────────┬─────────┴─────────┬─────────┘
      │            │                │                   │
      v            v                v                   v
┌─────────────────────────────────────────────────────────────────┐
│                    Go Library Layer                              │
│                    (pkg/adt + pkg/dsl)                          │
├─────────────────────────────────────────────────────────────────┤
│  • Fluent Builders    • Workflow Engine    • Graph Traversal    │
│  • Pipeline Composer  • Batch Operations   • Cache Layer        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              v
┌─────────────────────────────────────────────────────────────────┐
│                    SAP ADT REST API                              │
└─────────────────────────────────────────────────────────────────┘
```

### Phased Implementation

#### Phase 1: Polish Go Library (Low Effort, High Value)
- Add fluent builder patterns to existing API
- Create `pkg/dsl` with workflow primitives
- Add more code examples and godoc
- **Effort**: 1-2 weeks

#### Phase 2: YAML Workflow Engine (Medium Effort, High Value)
- Implement YAML config parser
- Create workflow runner
- Add to CLI: `vsp workflow run <name>`
- **Effort**: 2-3 weeks

#### Phase 3: Evaluate Scripting Language (Medium-High Effort)
- Survey users for language preference
- If Ruby: Create gem wrapping vsp binary
- If Python: Create PyPI package
- If Lua: Embed in vsp binary
- **Effort**: 3-4 weeks

#### Phase 4: External DSL (Future, If Warranted)
- Only if significant demand
- Consider using [Lingo framework](https://about.gitlab.com/blog/a-go-micro-language-framework-for-building-dsls/) for Go
- **Effort**: 6+ weeks

---

## Part 6: Quick Wins

### Immediate Value (Can Do Now)

1. **Export pkg/adt as public API**
   - Already usable, just needs documentation
   - Add `examples/` directory with use cases

2. **Add Workflow Helpers**
   ```go
   // pkg/adt/helpers.go
   func BatchUpdate(client *Client, objects []string, transform func(string) string) error
   func ExportGraph(client *Client, root string, depth int, format string) (string, error)
   ```

3. **Simple YAML Config**
   ```yaml
   # .vsp.yaml - connection config
   connections:
     dev:
       url: http://host:50000
       user: ${SAP_USER}
       client: "001"
   ```

4. **CLI Improvements**
   ```bash
   # Add convenience commands
   vsp search "ZCL_*" --type class --package "$TMP"
   vsp graph ZCL_RAY_10_SPIDER --depth 3 --format dot
   vsp batch-update --package "$ZRAY*" --transform prepend-header
   ```

---

## Conclusion

### Recommendations

| Priority | Action | Why |
|----------|--------|-----|
| **P0** | Polish Go library API | Foundation for everything else |
| **P1** | Add YAML workflow config | Low effort, high CI/CD value |
| **P2** | Survey users for scripting preference | Data-driven decision |
| **P3** | Consider Ruby or Python SDK | Based on survey results |
| **Park** | External DSL | Only if major adoption |

### Decision Framework

```
                           Low ◄─── Complexity ───► High
                           │
        High               │
          ▲                │    ┌─────────┐
          │                │    │ Ruby    │
          │   ┌───────┐    │    │ DSL     │
   Value  │   │ YAML  │    │    └─────────┘
          │   │Config │    │         ┌─────────┐
          │   └───────┘    │         │External │
          │        ┌───────┴───┐     │  DSL    │
          │        │Go Library │     └─────────┘
          │        │+ Builders │
          │        └───────────┘
         Low
```

**Start with Go Library + YAML Config. They provide 80% of value with 20% of effort.**

---

## References & Sources

### Go DSL Resources
- [Goa Framework - Design-first Go DSL](https://github.com/goadesign/goa)
- [GitLab Lingo - Go micro language framework](https://about.gitlab.com/blog/a-go-micro-language-framework-for-building-dsls/)
- [Abusing Go Syntax for DSLs](https://blog.gopheracademy.com/advent-2016/go-syntax-for-dsls/)
- [Building DSLs in Go with gRule](https://medium.com/@ykameshrao/building-dsls-in-golang-using-grule-and-protobuf-b9cd26b4ecfb)

### Ruby DSL Resources
- [Writing a DSL in Ruby - Thoughtbot](https://thoughtbot.com/blog/writing-a-domain-specific-language-in-ruby)
- [Ruby DSL Metaprogramming Guide - Toptal](https://www.toptal.com/ruby/ruby-dsl-metaprogramming-guide)
- [Martin Fowler - Internal DSL Style](https://martinfowler.com/bliki/InternalDslStyle.html)
- [Martin Fowler - Domain Specific Languages](https://martinfowler.com/books/dsl.html)

### SAP Automation Resources
- [CI/CD Tools for SAP BTP ABAP - SAP Community](https://community.sap.com/t5/technology-blog-posts-by-sap/ci-cd-tools-for-sap-btp-abap-environment/ba-p/13491022)
- [abapGit and CI/CD - Basis Technologies](https://www.basistechnologies.com/blog/using-abapgit-and-containers-to-extend-cicd-workflows-in-sap-landscapes)
- [SAP abap-platform-ci-cd-samples](https://github.com/SAP-samples/abap-platform-ci-cd-samples)
- [On-Premise ABAP CI/CD with GitHub Actions](https://community.sap.com/t5/application-development-and-automation-discussions/making-an-onpremise-abap-ci-cd-with-github-actions/m-p/13884171)

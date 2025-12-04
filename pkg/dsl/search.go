package dsl

import (
	"context"
	"regexp"
	"strings"

	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// SearchBuilder provides a fluent interface for searching ABAP objects.
type SearchBuilder struct {
	client     *adt.Client
	query      string
	types      []string
	packages   []string
	maxResults int
	filters    []FilterFunc
}

// FilterFunc is a function that filters objects.
type FilterFunc func(ObjectRef) bool

// Search creates a new search builder.
func Search(client *adt.Client) *SearchBuilder {
	return &SearchBuilder{
		client:     client,
		maxResults: 100,
		filters:    []FilterFunc{},
	}
}

// Query sets the search query pattern (supports * wildcard).
func (s *SearchBuilder) Query(query string) *SearchBuilder {
	s.query = query
	return s
}

// Types filters by object types.
func (s *SearchBuilder) Types(types ...string) *SearchBuilder {
	s.types = append(s.types, types...)
	return s
}

// Classes filters to only classes.
func (s *SearchBuilder) Classes() *SearchBuilder {
	return s.Types(TypeClass)
}

// Programs filters to only programs.
func (s *SearchBuilder) Programs() *SearchBuilder {
	return s.Types(TypeProgram)
}

// Functions filters to only function modules.
func (s *SearchBuilder) Functions() *SearchBuilder {
	return s.Types(TypeFunction)
}

// Interfaces filters to only interfaces.
func (s *SearchBuilder) Interfaces() *SearchBuilder {
	return s.Types(TypeInterface)
}

// InPackages filters to objects in specified packages.
func (s *SearchBuilder) InPackages(packages ...string) *SearchBuilder {
	s.packages = append(s.packages, packages...)
	return s
}

// InPackage is a convenience method for single package.
func (s *SearchBuilder) InPackage(pkg string) *SearchBuilder {
	return s.InPackages(pkg)
}

// MaxResults sets the maximum number of results.
func (s *SearchBuilder) MaxResults(max int) *SearchBuilder {
	s.maxResults = max
	return s
}

// Where adds a custom filter function.
func (s *SearchBuilder) Where(filter FilterFunc) *SearchBuilder {
	s.filters = append(s.filters, filter)
	return s
}

// NameMatches filters objects whose name matches a regex pattern.
func (s *SearchBuilder) NameMatches(pattern string) *SearchBuilder {
	re := regexp.MustCompile(pattern)
	return s.Where(func(obj ObjectRef) bool {
		return re.MatchString(obj.Name)
	})
}

// NameContains filters objects whose name contains a substring.
func (s *SearchBuilder) NameContains(substr string) *SearchBuilder {
	return s.Where(func(obj ObjectRef) bool {
		return strings.Contains(strings.ToUpper(obj.Name), strings.ToUpper(substr))
	})
}

// NamePrefix filters objects whose name starts with a prefix.
func (s *SearchBuilder) NamePrefix(prefix string) *SearchBuilder {
	return s.Where(func(obj ObjectRef) bool {
		return strings.HasPrefix(strings.ToUpper(obj.Name), strings.ToUpper(prefix))
	})
}

// Exclude filters out objects matching a pattern.
func (s *SearchBuilder) Exclude(pattern string) *SearchBuilder {
	re := regexp.MustCompile(pattern)
	return s.Where(func(obj ObjectRef) bool {
		return !re.MatchString(obj.Name)
	})
}

// Execute runs the search and returns matching objects.
func (s *SearchBuilder) Execute(ctx context.Context) ([]ObjectRef, error) {
	results, err := s.client.SearchObject(ctx, s.query, s.maxResults)
	if err != nil {
		return nil, err
	}

	var objects []ObjectRef
	for _, r := range results {
		obj := ObjectRef{
			Type:    r.Type,
			Name:    r.Name,
			Package: r.PackageName,
			URL:     r.URI,
		}

		// Apply type filter
		if len(s.types) > 0 && !containsType(s.types, obj.Type) {
			continue
		}

		// Apply package filter
		if len(s.packages) > 0 && !matchesPackage(s.packages, obj.Package) {
			continue
		}

		// Apply custom filters
		if !s.applyFilters(obj) {
			continue
		}

		objects = append(objects, obj)
	}

	return objects, nil
}

// ExecuteOne runs the search and returns the first matching object.
func (s *SearchBuilder) ExecuteOne(ctx context.Context) (*ObjectRef, error) {
	s.maxResults = 1
	results, err := s.Execute(ctx)
	if err != nil {
		return nil, err
	}
	if len(results) == 0 {
		return nil, nil
	}
	return &results[0], nil
}

// Count runs the search and returns the count of matching objects.
func (s *SearchBuilder) Count(ctx context.Context) (int, error) {
	results, err := s.Execute(ctx)
	if err != nil {
		return 0, err
	}
	return len(results), nil
}

// applyFilters applies all filter functions.
func (s *SearchBuilder) applyFilters(obj ObjectRef) bool {
	for _, filter := range s.filters {
		if !filter(obj) {
			return false
		}
	}
	return true
}

// containsType checks if a type is in the list.
func containsType(types []string, objType string) bool {
	for _, t := range types {
		if strings.EqualFold(t, objType) {
			return true
		}
		// Handle type codes (e.g., CLAS/OC -> CLAS)
		if strings.HasPrefix(objType, t+"/") || strings.HasPrefix(t, objType+"/") {
			return true
		}
	}
	return false
}

// matchesPackage checks if a package matches any pattern.
func matchesPackage(patterns []string, pkg string) bool {
	for _, pattern := range patterns {
		if matchWildcard(pattern, pkg) {
			return true
		}
	}
	return false
}

// matchWildcard performs simple wildcard matching (* and ?).
func matchWildcard(pattern, s string) bool {
	pattern = strings.ToUpper(pattern)
	s = strings.ToUpper(s)

	// Convert wildcard to regex
	regexPattern := "^" + regexp.QuoteMeta(pattern) + "$"
	regexPattern = strings.ReplaceAll(regexPattern, `\*`, ".*")
	regexPattern = strings.ReplaceAll(regexPattern, `\?`, ".")

	re := regexp.MustCompile(regexPattern)
	return re.MatchString(s)
}

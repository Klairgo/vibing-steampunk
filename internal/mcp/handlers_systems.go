// Package mcp provides the MCP server implementation for ABAP ADT tools.
// handlers_systems.go contains handlers for multi-system management (ListSystems, SwitchSystem).
package mcp

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"strings"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// --- Multi-System Helpers ---

// getClientForSystem returns the ADT client for a named system, creating it lazily if needed.
// If systemName is empty, returns the current active client.
// Caller must NOT hold systemsMu lock (this method acquires it internally).
func (s *Server) getClientForSystem(systemName string) (*adt.Client, string, error) {
	// Empty name = use active client
	if systemName == "" {
		return s.adtClient, s.activeSystem, nil
	}

	systemName = strings.ToLower(systemName)

	s.systemsMu.Lock()
	defer s.systemsMu.Unlock()

	if len(s.systems) == 0 {
		return nil, "", fmt.Errorf("no multi-system config found; create .vsp.json with system definitions")
	}

	entry, ok := s.systems[systemName]
	if !ok {
		available := make([]string, 0, len(s.systems))
		for k := range s.systems {
			available = append(available, k)
		}
		sort.Strings(available)
		return nil, "", fmt.Errorf("system '%s' not found. Available: %s", systemName, strings.Join(available, ", "))
	}

	// If this is the active system, return the main client (already initialized)
	if systemName == s.activeSystem && s.adtClient != nil {
		return s.adtClient, systemName, nil
	}

	// Validate credentials
	if entry.Password == "" {
		envKey := fmt.Sprintf("VSP_%s_PASSWORD", strings.ToUpper(systemName))
		return nil, "", fmt.Errorf("no password configured for system '%s'. Set %s environment variable or add password to .vsp.json", systemName, envKey)
	}

	// Lazily create ADT client if not yet created
	if entry.ADTClient == nil {
		client := entry.Client
		if client == "" {
			client = "001"
		}
		language := entry.Language
		if language == "" {
			language = "EN"
		}

		opts := []adt.Option{
			adt.WithClient(client),
			adt.WithLanguage(language),
		}
		if entry.Insecure {
			opts = append(opts, adt.WithInsecureSkipVerify())
		}
		if s.config.Verbose {
			opts = append(opts, adt.WithVerbose())
		}

		// Carry over safety settings from current config
		safety := adt.UnrestrictedSafetyConfig()
		if s.config.ReadOnly {
			safety.ReadOnly = true
		}
		if s.config.BlockFreeSQL {
			safety.BlockFreeSQL = true
		}
		if s.config.AllowedOps != "" {
			safety.AllowedOps = s.config.AllowedOps
		}
		if s.config.DisallowedOps != "" {
			safety.DisallowedOps = s.config.DisallowedOps
		}
		if len(s.config.AllowedPackages) > 0 {
			safety.AllowedPackages = s.config.AllowedPackages
		}
		if s.config.EnableTransports {
			safety.EnableTransports = true
		}
		if s.config.AllowTransportableEdits {
			safety.AllowTransportableEdits = true
		}
		opts = append(opts, adt.WithSafety(safety))

		entry.ADTClient = adt.NewClient(entry.URL, entry.User, entry.Password, opts...)
	}

	return entry.ADTClient, systemName, nil
}

// --- Multi-System Handlers ---

// handleListSystems returns a list of configured SAP systems and the active system.
func (s *Server) handleListSystems(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	s.systemsMu.RLock()
	defer s.systemsMu.RUnlock()

	if len(s.systems) == 0 {
		return mcp.NewToolResultText("No multi-system config found. Create .vsp.json with system definitions.\nSee: https://github.com/oisee/vibing-steampunk#system-profiles-vspjson"), nil
	}

	type systemInfo struct {
		Name     string `json:"name"`
		URL      string `json:"url"`
		User     string `json:"user"`
		Client   string `json:"client"`
		Active   bool   `json:"active"`
		HasAuth  bool   `json:"has_auth"`
	}

	systemList := make([]systemInfo, 0, len(s.systems))
	for name, entry := range s.systems {
		systemList = append(systemList, systemInfo{
			Name:    name,
			URL:     entry.URL,
			User:    entry.User,
			Client:  entry.Client,
			Active:  name == s.activeSystem,
			HasAuth: entry.Password != "",
		})
	}

	// Sort by name for consistent output
	sort.Slice(systemList, func(i, j int) bool {
		return systemList[i].Name < systemList[j].Name
	})

	result := map[string]interface{}{
		"active_system": s.activeSystem,
		"system_count":  len(s.systems),
		"systems":       systemList,
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

// handleSwitchSystem switches the active SAP system and reconnects the ADT client.
func (s *Server) handleSwitchSystem(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	systemName, ok := request.Params.Arguments["system"].(string)
	if !ok || systemName == "" {
		return newToolResultError("system parameter is required"), nil
	}
	systemName = strings.ToLower(systemName)

	s.systemsMu.Lock()
	defer s.systemsMu.Unlock()

	if len(s.systems) == 0 {
		return newToolResultError("No multi-system config found. Create .vsp.json with system definitions."), nil
	}

	entry, ok := s.systems[systemName]
	if !ok {
		// List available systems in error message
		available := make([]string, 0, len(s.systems))
		for k := range s.systems {
			available = append(available, k)
		}
		sort.Strings(available)
		return newToolResultError(fmt.Sprintf("System '%s' not found. Available systems: %s", systemName, strings.Join(available, ", "))), nil
	}

	// Check if already on this system
	if systemName == s.activeSystem {
		return mcp.NewToolResultText(fmt.Sprintf("Already on system '%s' (%s@%s client %s)",
			systemName, entry.User, entry.URL, entry.Client)), nil
	}

	// Validate that the system has credentials
	if entry.Password == "" {
		envKey := fmt.Sprintf("VSP_%s_PASSWORD", strings.ToUpper(systemName))
		return newToolResultError(fmt.Sprintf("No password configured for system '%s'. Set %s environment variable or add password to .vsp.json", systemName, envKey)), nil
	}

	// Create ADT client lazily if not yet created
	if entry.ADTClient == nil {
		client := entry.Client
		if client == "" {
			client = "001"
		}
		language := entry.Language
		if language == "" {
			language = "EN"
		}

		opts := []adt.Option{
			adt.WithClient(client),
			adt.WithLanguage(language),
		}
		if entry.Insecure {
			opts = append(opts, adt.WithInsecureSkipVerify())
		}
		if s.config.Verbose {
			opts = append(opts, adt.WithVerbose())
		}

		// Carry over safety settings from current config
		safety := adt.UnrestrictedSafetyConfig()
		if s.config.ReadOnly {
			safety.ReadOnly = true
		}
		if s.config.BlockFreeSQL {
			safety.BlockFreeSQL = true
		}
		if s.config.AllowedOps != "" {
			safety.AllowedOps = s.config.AllowedOps
		}
		if s.config.DisallowedOps != "" {
			safety.DisallowedOps = s.config.DisallowedOps
		}
		if len(s.config.AllowedPackages) > 0 {
			safety.AllowedPackages = s.config.AllowedPackages
		}
		if s.config.EnableTransports {
			safety.EnableTransports = true
		}
		if s.config.AllowTransportableEdits {
			safety.AllowTransportableEdits = true
		}
		opts = append(opts, adt.WithSafety(safety))

		entry.ADTClient = adt.NewClient(entry.URL, entry.User, entry.Password, opts...)
	}

	// Store previous system name for the status message
	previousSystem := s.activeSystem

	// Switch the active client
	s.adtClient = entry.ADTClient
	s.activeSystem = systemName

	// Update config to reflect the new active system (for GetConnectionInfo, etc.)
	s.config.BaseURL = entry.URL
	s.config.Username = entry.User
	s.config.Password = entry.Password
	if entry.Client != "" {
		s.config.Client = entry.Client
	}
	if entry.Language != "" {
		s.config.Language = entry.Language
	}
	s.config.InsecureSkipVerify = entry.Insecure

	// Reset WebSocket clients (they're system-specific)
	s.amdpWSClient = nil
	s.debugWSClient = nil

	// Re-create feature prober for the new system
	featureConfig := adt.FeatureConfig{
		HANA:      parseFeatureMode(s.config.FeatureHANA),
		AbapGit:   parseFeatureMode(s.config.FeatureAbapGit),
		RAP:       parseFeatureMode(s.config.FeatureRAP),
		AMDP:      parseFeatureMode(s.config.FeatureAMDP),
		UI5:       parseFeatureMode(s.config.FeatureUI5),
		Transport: parseFeatureMode(s.config.FeatureTransport),
	}
	s.featureProber = adt.NewFeatureProber(entry.ADTClient, featureConfig, s.config.Verbose)
	s.featureConfig = featureConfig

	// Update terminal ID for debugger
	adt.SetTerminalIDUser(entry.User)

	result := map[string]interface{}{
		"status":          "switched",
		"previous_system": previousSystem,
		"active_system":   systemName,
		"url":             entry.URL,
		"user":            entry.User,
		"client":          entry.Client,
	}

	output, _ := json.MarshalIndent(result, "", "  ")
	return mcp.NewToolResultText(string(output)), nil
}

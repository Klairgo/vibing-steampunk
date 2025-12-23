# WebSocket-based abapGit Integration

**Date:** 2025-12-22
**Report ID:** 003
**Subject:** Git domain for ZADT_VSP WebSocket handler
**Status:** Design

---

## Executive Summary

This design extends the ZADT_VSP WebSocket handler with a new `git` domain that leverages abapGit's native serialization/deserialization APIs. This enables importing/exporting **any object type** that abapGit supports (150+ types) via a simple ZIP-based protocol.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                          vsp (Go)                               │
├─────────────────────────────────────────────────────────────────┤
│  GitImport       │ Import ZIP/folder to SAP                     │
│  GitExport       │ Export package(s)/objects to ZIP             │
│  GitTypes        │ List 150+ supported object types             │
│  GitValidate     │ Dry-run validation                           │
└────────────┬────────────────────────────────────────────────────┘
             │ WebSocket (JSON + base64 ZIP data)
             ▼
┌─────────────────────────────────────────────────────────────────┐
│           ZADT_VSP WebSocket Handler                            │
├─────────────────────────────────────────────────────────────────┤
│  ZCL_VSP_GIT_SERVICE (implements ZIF_VSP_SERVICE)               │
│  ├─ handle_get_types    → ZCL_ABAPGIT_OBJECTS=>supported_list   │
│  ├─ handle_export       → ZCL_ABAPGIT_OBJECTS=>serialize        │
│  ├─ handle_import       → ZCL_ABAPGIT_OBJECTS=>deserialize      │
│  └─ handle_validate     → ZCL_ABAPGIT_OBJECTS=>deserialize_checks│
└─────────────────────────────────────────────────────────────────┘
```

## WebSocket Protocol

### Domain: `git`

### Actions

#### 1. `getTypes` - List supported object types

**Request:**
```json
{
  "id": "1",
  "domain": "git",
  "action": "getTypes"
}
```

**Response:**
```json
{
  "id": "1",
  "status": "ok",
  "result": {
    "count": 156,
    "types": ["CLAS", "INTF", "PROG", "FUGR", "FUNC", "TABL", "DTEL", "DOMA", ...]
  }
}
```

#### 2. `export` - Export package(s) or objects to ZIP

**Request:**
```json
{
  "id": "2",
  "domain": "git",
  "action": "export",
  "params": {
    "packages": ["$ZADT", "$ZADT_VSP"],
    "includeSubpackages": true,
    "format": "abapgit"
  }
}
```

Or export specific objects:
```json
{
  "id": "3",
  "domain": "git",
  "action": "export",
  "params": {
    "objects": [
      {"type": "CLAS", "name": "ZCL_MY_CLASS"},
      {"type": "INTF", "name": "ZIF_MY_INTERFACE"},
      {"type": "PROG", "name": "ZTEST_PROGRAM"}
    ],
    "format": "abapgit"
  }
}
```

**Response:**
```json
{
  "id": "2",
  "status": "ok",
  "result": {
    "objectCount": 15,
    "zipBase64": "UEsDBBQAAAAIAGBvSlkA...",
    "files": [
      {"path": "src/zcl_my_class.clas.abap", "size": 4523},
      {"path": "src/zcl_my_class.clas.xml", "size": 892},
      ...
    ]
  }
}
```

#### 3. `import` - Import ZIP to SAP

**Request:**
```json
{
  "id": "4",
  "domain": "git",
  "action": "import",
  "params": {
    "zipBase64": "UEsDBBQAAAAIAGBvSlkA...",
    "package": "$ZADT_VSP",
    "transport": "A4HK900123",
    "overwrite": true,
    "activate": true
  }
}
```

**Response:**
```json
{
  "id": "4",
  "status": "ok",
  "result": {
    "objectsProcessed": 15,
    "objectsCreated": 10,
    "objectsUpdated": 5,
    "objectsSkipped": 0,
    "activationErrors": 0,
    "log": [
      {"level": "info", "message": ">>> Deserializing 15 objects"},
      {"level": "success", "object": "CLAS/ZCL_MY_CLASS", "message": "Object imported"},
      {"level": "warning", "object": "DTEL/ZTEST", "message": "Already exists, overwritten"}
    ]
  }
}
```

#### 4. `validate` - Validate ZIP without importing

**Request:**
```json
{
  "id": "5",
  "domain": "git",
  "action": "validate",
  "params": {
    "zipBase64": "UEsDBBQAAAAIAGBvSlkA...",
    "package": "$ZADT_VSP"
  }
}
```

**Response:**
```json
{
  "id": "5",
  "status": "ok",
  "result": {
    "valid": true,
    "objectsFound": 15,
    "supported": 15,
    "unsupported": [],
    "conflicts": [
      {"type": "CLAS", "name": "ZCL_EXISTS", "reason": "Object exists in package $OTHER"}
    ],
    "warnings": [
      "Package $ZADT_VSP does not exist, will be created"
    ]
  }
}
```

## ABAP Implementation

### ZCL_VSP_GIT_SERVICE

```abap
CLASS zcl_vsp_git_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_vsp_service.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_object_ref,
        type TYPE trobjtype,
        name TYPE sobj_name,
      END OF ty_object_ref,
      ty_object_refs TYPE STANDARD TABLE OF ty_object_ref WITH DEFAULT KEY.

    METHODS:
      handle_get_types
        RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response,

      handle_export
        IMPORTING is_message TYPE zif_vsp_service=>ty_message
        RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response,

      handle_import
        IMPORTING is_message TYPE zif_vsp_service=>ty_message
        RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response,

      handle_validate
        IMPORTING is_message TYPE zif_vsp_service=>ty_message
        RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response,

      get_package_objects
        IMPORTING iv_package TYPE devclass
                  iv_include_subpackages TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt,

      serialize_objects
        IMPORTING it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
        RETURNING VALUE(rv_zip_base64) TYPE string
        RAISING zcx_abapgit_exception,

      deserialize_zip
        IMPORTING iv_zip_base64 TYPE string
                  iv_package TYPE devclass
                  iv_transport TYPE trkorr
                  iv_overwrite TYPE abap_bool
                  iv_activate TYPE abap_bool
        RETURNING VALUE(rs_result) TYPE ty_import_result
        RAISING zcx_abapgit_exception,

      base64_to_xstring
        IMPORTING iv_base64 TYPE string
        RETURNING VALUE(rv_xstring) TYPE xstring,

      xstring_to_base64
        IMPORTING iv_xstring TYPE xstring
        RETURNING VALUE(rv_base64) TYPE string.
ENDCLASS.

CLASS zcl_vsp_git_service IMPLEMENTATION.

  METHOD zif_vsp_service~handle_message.
    CASE is_message-action.
      WHEN 'getTypes' OR 'get_types'.
        rs_response = handle_get_types( ).
      WHEN 'export'.
        rs_response = handle_export( is_message ).
      WHEN 'import'.
        rs_response = handle_import( is_message ).
      WHEN 'validate'.
        rs_response = handle_validate( is_message ).
      WHEN OTHERS.
        rs_response-status = 'error'.
        rs_response-error = |Unknown action: { is_message-action }|.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_get_types.
    DATA: lt_types TYPE zif_abapgit_objects=>ty_types_tt,
          lo_json  TYPE REF TO zcl_abapgit_ajson.

    lt_types = zcl_abapgit_objects=>supported_list( ).

    TRY.
        lo_json = zcl_abapgit_ajson=>create_empty( ).
        lo_json->set_integer( iv_path = '/count' iv_val = lines( lt_types ) ).
        lo_json->set( iv_path = '/types' iv_val = lt_types ).

        rs_response-status = 'ok'.
        rs_response-result = lo_json->stringify( ).
      CATCH zcx_abapgit_ajson_error.
        rs_response-status = 'error'.
        rs_response-error = 'JSON serialization failed'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_export.
    DATA: lt_packages TYPE STANDARD TABLE OF devclass,
          lt_objects  TYPE ty_object_refs,
          lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_zip_base64 TYPE string,
          lo_json     TYPE REF TO zcl_abapgit_ajson,
          lv_include_sub TYPE abap_bool.

    " Parse params
    TRY.
        lo_json = zcl_abapgit_ajson=>parse( is_message-params ).

        " Option 1: Export packages
        IF lo_json->exists( '/packages' ).
          lo_json->to_abap( EXPORTING iv_path = '/packages' IMPORTING ev_container = lt_packages ).
          lv_include_sub = lo_json->get_boolean( '/includeSubpackages' ).

          LOOP AT lt_packages INTO DATA(lv_package).
            APPEND LINES OF get_package_objects( iv_package = lv_package
                                                  iv_include_subpackages = lv_include_sub )
                   TO lt_tadir.
          ENDLOOP.
        ENDIF.

        " Option 2: Export specific objects
        IF lo_json->exists( '/objects' ).
          lo_json->to_abap( EXPORTING iv_path = '/objects' IMPORTING ev_container = lt_objects ).

          LOOP AT lt_objects INTO DATA(ls_obj).
            " Get TADIR entry for each object
            DATA(ls_tadir) = zcl_abapgit_factory=>get_tadir( )->get_single(
              iv_object   = ls_obj-type
              iv_obj_name = ls_obj-name ).
            IF ls_tadir IS NOT INITIAL.
              APPEND ls_tadir TO lt_tadir.
            ENDIF.
          ENDLOOP.
        ENDIF.

        " Serialize to ZIP
        lv_zip_base64 = serialize_objects( lt_tadir ).

        " Build response
        lo_json = zcl_abapgit_ajson=>create_empty( ).
        lo_json->set_integer( iv_path = '/objectCount' iv_val = lines( lt_tadir ) ).
        lo_json->set_string( iv_path = '/zipBase64' iv_val = lv_zip_base64 ).

        rs_response-status = 'ok'.
        rs_response-result = lo_json->stringify( ).

      CATCH zcx_abapgit_exception zcx_abapgit_ajson_error INTO DATA(lx_error).
        rs_response-status = 'error'.
        rs_response-error = lx_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_import.
    " TODO: Implement using ZCL_ABAPGIT_OBJECTS=>deserialize
    " Key challenge: Create a virtual repo that provides files from ZIP
    rs_response-status = 'error'.
    rs_response-error = 'Import not yet implemented'.
  ENDMETHOD.

  METHOD handle_validate.
    " TODO: Implement using ZCL_ABAPGIT_OBJECTS=>deserialize_checks
    rs_response-status = 'error'.
    rs_response-error = 'Validate not yet implemented'.
  ENDMETHOD.

  METHOD get_package_objects.
    " Get all objects from TADIR for a package
    SELECT * FROM tadir
      INTO TABLE @rt_tadir
      WHERE devclass = @iv_package
        AND delflag = @abap_false.

    IF iv_include_subpackages = abap_true.
      " Get subpackages recursively
      DATA(lt_subpackages) = zcl_abapgit_factory=>get_sap_package( iv_package )->list_subpackages( ).
      LOOP AT lt_subpackages INTO DATA(lv_subpkg).
        SELECT * FROM tadir
          APPENDING TABLE @rt_tadir
          WHERE devclass = @lv_subpkg
            AND delflag = @abap_false.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD serialize_objects.
    DATA: lo_zip    TYPE REF TO cl_abap_zip,
          lt_files  TYPE zif_abapgit_git_definitions=>ty_files_tt,
          lo_i18n   TYPE REF TO zcl_abapgit_i18n_params.

    CREATE OBJECT lo_zip.
    lo_i18n = zcl_abapgit_i18n_params=>new( ).

    LOOP AT it_tadir INTO DATA(ls_tadir).
      DATA(ls_item) = VALUE zif_abapgit_definitions=>ty_item(
        obj_type = ls_tadir-object
        obj_name = ls_tadir-obj_name
        devclass = ls_tadir-devclass ).

      TRY.
          DATA(ls_serialized) = zcl_abapgit_objects=>serialize(
            is_item        = ls_item
            io_i18n_params = lo_i18n ).

          LOOP AT ls_serialized-files INTO DATA(ls_file).
            lo_zip->add(
              name    = |src/{ ls_file-filename }|
              content = ls_file-data ).
          ENDLOOP.
        CATCH zcx_abapgit_exception INTO DATA(lx_error).
          " Log error but continue with other objects
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    DATA(lv_zip_xstring) = lo_zip->save( ).
    rv_zip_base64 = xstring_to_base64( lv_zip_xstring ).
  ENDMETHOD.

  METHOD base64_to_xstring.
    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = rv_xstring
      EXCEPTIONS
        OTHERS  = 1.
  ENDMETHOD.

  METHOD xstring_to_base64.
    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata = iv_xstring
      IMPORTING
        b64data = rv_base64
      EXCEPTIONS
        OTHERS  = 1.
  ENDMETHOD.

ENDCLASS.
```

### Register in ZCL_VSP_APC_HANDLER

```abap
METHOD get_service.
  CASE iv_domain.
    WHEN 'rfc'.
      ri_service = NEW zcl_vsp_rfc_service( ).
    WHEN 'debug'.
      ri_service = NEW zcl_vsp_debug_service( ).
    WHEN 'amdp'.
      ri_service = NEW zcl_vsp_amdp_service( ).
    WHEN 'git'.
      ri_service = NEW zcl_vsp_git_service( ).  " NEW!
    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_apc_error
        EXPORTING
          textid = cx_apc_error=>unknown_domain
          msgv1  = iv_domain.
  ENDCASE.
ENDMETHOD.
```

## Go Implementation

### pkg/adt/git_websocket.go

```go
package adt

import (
	"archive/zip"
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// GitExportResult represents export response
type GitExportResult struct {
	ObjectCount int      `json:"objectCount"`
	ZipBase64   string   `json:"zipBase64"`
	Files       []string `json:"files,omitempty"`
}

// GitImportResult represents import response
type GitImportResult struct {
	ObjectsProcessed  int          `json:"objectsProcessed"`
	ObjectsCreated    int          `json:"objectsCreated"`
	ObjectsUpdated    int          `json:"objectsUpdated"`
	ObjectsSkipped    int          `json:"objectsSkipped"`
	ActivationErrors  int          `json:"activationErrors"`
	Log              []GitLogEntry `json:"log"`
}

type GitLogEntry struct {
	Level   string `json:"level"`
	Object  string `json:"object,omitempty"`
	Message string `json:"message"`
}

// GitExport exports packages or objects to ZIP
func (c *AMDPWebSocketClient) GitExport(ctx context.Context, packages []string, includeSubpackages bool) (*GitExportResult, error) {
	params := map[string]interface{}{
		"packages":           packages,
		"includeSubpackages": includeSubpackages,
		"format":            "abapgit",
	}

	resp, err := c.sendGitRequest(ctx, "export", params)
	if err != nil {
		return nil, err
	}

	var result GitExportResult
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, err
	}

	return &result, nil
}

// GitExportObjects exports specific objects to ZIP
func (c *AMDPWebSocketClient) GitExportObjects(ctx context.Context, objects []ObjectRef) (*GitExportResult, error) {
	params := map[string]interface{}{
		"objects": objects,
		"format":  "abapgit",
	}

	resp, err := c.sendGitRequest(ctx, "export", params)
	if err != nil {
		return nil, err
	}

	var result GitExportResult
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, err
	}

	return &result, nil
}

// GitImportZip imports base64 ZIP to SAP
func (c *AMDPWebSocketClient) GitImportZip(ctx context.Context, zipBase64, pkg, transport string, overwrite, activate bool) (*GitImportResult, error) {
	params := map[string]interface{}{
		"zipBase64": zipBase64,
		"package":   pkg,
		"transport": transport,
		"overwrite": overwrite,
		"activate":  activate,
	}

	resp, err := c.sendGitRequest(ctx, "import", params)
	if err != nil {
		return nil, err
	}

	var result GitImportResult
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, err
	}

	return &result, nil
}

// GitImportDirectory imports abapGit-format directory to SAP
func (c *AMDPWebSocketClient) GitImportDirectory(ctx context.Context, dirPath, pkg, transport string, overwrite, activate bool) (*GitImportResult, error) {
	// Create ZIP from directory
	zipBase64, err := directoryToZipBase64(dirPath)
	if err != nil {
		return nil, err
	}

	return c.GitImportZip(ctx, zipBase64, pkg, transport, overwrite, activate)
}

// GitGetTypes returns list of supported object types
func (c *AMDPWebSocketClient) GitGetTypes(ctx context.Context) ([]string, error) {
	resp, err := c.sendGitRequest(ctx, "getTypes", nil)
	if err != nil {
		return nil, err
	}

	var result struct {
		Count int      `json:"count"`
		Types []string `json:"types"`
	}
	if err := json.Unmarshal(resp.Result, &result); err != nil {
		return nil, err
	}

	return result.Types, nil
}

// SaveZipToFile saves the base64 ZIP to a file
func (r *GitExportResult) SaveZipToFile(path string) error {
	zipData, err := base64.StdEncoding.DecodeString(r.ZipBase64)
	if err != nil {
		return err
	}
	return os.WriteFile(path, zipData, 0644)
}

// ExtractToDirectory extracts the ZIP to a directory
func (r *GitExportResult) ExtractToDirectory(dirPath string) error {
	zipData, err := base64.StdEncoding.DecodeString(r.ZipBase64)
	if err != nil {
		return err
	}

	reader, err := zip.NewReader(bytes.NewReader(zipData), int64(len(zipData)))
	if err != nil {
		return err
	}

	for _, file := range reader.File {
		fpath := filepath.Join(dirPath, file.Name)

		if file.FileInfo().IsDir() {
			os.MkdirAll(fpath, os.ModePerm)
			continue
		}

		os.MkdirAll(filepath.Dir(fpath), os.ModePerm)

		outFile, err := os.Create(fpath)
		if err != nil {
			return err
		}

		rc, err := file.Open()
		if err != nil {
			outFile.Close()
			return err
		}

		_, err = io.Copy(outFile, rc)
		outFile.Close()
		rc.Close()
		if err != nil {
			return err
		}
	}

	return nil
}

// Helper: convert directory to base64 ZIP
func directoryToZipBase64(dirPath string) (string, error) {
	buf := new(bytes.Buffer)
	w := zip.NewWriter(buf)

	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}

		relPath, err := filepath.Rel(dirPath, path)
		if err != nil {
			return err
		}

		// Normalize path separators
		relPath = strings.ReplaceAll(relPath, string(os.PathSeparator), "/")

		f, err := w.Create(relPath)
		if err != nil {
			return err
		}

		content, err := os.ReadFile(path)
		if err != nil {
			return err
		}

		_, err = f.Write(content)
		return err
	})

	if err != nil {
		return "", err
	}

	if err := w.Close(); err != nil {
		return "", err
	}

	return base64.StdEncoding.EncodeToString(buf.Bytes()), nil
}

func (c *AMDPWebSocketClient) sendGitRequest(ctx context.Context, action string, params interface{}) (*WSResponse, error) {
	// Use the existing sendRequest but with domain "git"
	return c.sendRequestWithDomain(ctx, "git", action, params)
}
```

## MCP Tools

### GitExport

```
Tool: GitExport
Description: Export ABAP packages or objects to abapGit-compatible ZIP format

Parameters:
  - packages: Array of package names (e.g., ["$ZADT", "$ZADT_VSP"])
  - objects: Array of {type, name} (alternative to packages)
  - includeSubpackages: Include subpackages (default: true)
  - outputPath: Local path to save ZIP file

Returns: Path to exported ZIP file + object count
```

### GitImport

```
Tool: GitImport
Description: Import abapGit-compatible ZIP or folder to SAP

Parameters:
  - path: Path to ZIP file or folder (tool detects which)
  - package: Target package name
  - transport: Transport request (optional for $TMP)
  - overwrite: Overwrite existing objects (default: false)
  - activate: Activate after import (default: true)

Returns: Import log with created/updated/failed counts
```

### GitTypes

```
Tool: GitTypes
Description: List all 150+ object types supported by abapGit

Returns: Array of type codes with count
```

## Usage Examples

### CLI

```bash
# Export package to ZIP
vsp git export --packages '$ZADT,$ZADT_VSP' --output backup.zip

# Export specific objects
vsp git export --objects 'CLAS:ZCL_MY_CLASS,INTF:ZIF_MY_INTERFACE' --output export.zip

# Import ZIP
vsp git import --path backup.zip --package '$ZADT' --transport A4HK900123

# Import folder (abapGit format)
vsp git import --path ./src/ --package '$ZADT'

# List supported types
vsp git types
```

### Lua

```lua
-- Export package
local result = gitExport({"$ZADT", "$ZADT_VSP"}, true)
print("Exported " .. result.objectCount .. " objects")
result:saveToFile("backup.zip")

-- Import folder
local importResult = gitImport("./src/", "$ZADT", "A4HK900123", true, true)
print("Created: " .. importResult.objectsCreated)
print("Updated: " .. importResult.objectsUpdated)

-- List types
local types = gitTypes()
print("Supported types: " .. #types)
```

## Implementation Plan

### Phase 1: Export (Priority)
1. Create ZCL_VSP_GIT_SERVICE skeleton
2. Implement `getTypes` action
3. Implement `export` action for packages
4. Add Go client methods
5. Add MCP tools

### Phase 2: Import
1. Research ZIF_ABAPGIT_REPO virtual implementation
2. Implement `import` action
3. Handle transport assignment
4. Handle activation

### Phase 3: Validation & Polish
1. Implement `validate` action
2. Add file filtering
3. Add conflict resolution
4. Add progress reporting for large operations

## Dependencies

- abapGit must be installed on SAP system (`$ZGIT_DEV_*` packages)
- ZADT_VSP WebSocket handler must be deployed
- SAP user needs appropriate authorizations

## Related Documents

- [abapGit Integration Design](./2025-12-08-001-abapgit-integration-design.md)
- [WebSocket RFC Handler](./2025-12-18-002-websocket-rfc-handler.md)
- [ZCL_ABAPGIT_OBJECTS source](SAP: ZCL_ABAPGIT_OBJECTS)

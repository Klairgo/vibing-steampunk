"! <p class="shorttext synchronized">VSP Debug Domain Service</p>
"! Provides debugging capabilities via WebSocket.
"! Manages external breakpoints and debug session state.
CLASS zcl_vsp_debug_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_vsp_service.

    TYPES:
      BEGIN OF ty_breakpoint_state,
        id         TYPE string,
        kind       TYPE string,
        uri        TYPE string,
        line       TYPE i,
        enabled    TYPE abap_bool,
        condition  TYPE string,
        exception  TYPE string,
        statement  TYPE string,
      END OF ty_breakpoint_state,
      tt_breakpoints TYPE STANDARD TABLE OF ty_breakpoint_state WITH KEY id.

  PRIVATE SECTION.
    " Per-session state
    DATA mv_session_id TYPE string.
    DATA mv_debug_user TYPE sy-uname.
    DATA mt_breakpoints TYPE tt_breakpoints.
    DATA mv_attached_debuggee TYPE string.

    " Class-level: active sessions
    CLASS-DATA gt_sessions TYPE STANDARD TABLE OF REF TO zcl_vsp_debug_service WITH KEY table_line.

    METHODS handle_set_breakpoint
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_get_breakpoints
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_delete_breakpoint
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_get_status
      IMPORTING is_message         TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS escape_json
      IMPORTING iv_string         TYPE string
      RETURNING VALUE(rv_escaped) TYPE string.

    METHODS error_response
      IMPORTING iv_id              TYPE string
                iv_code            TYPE string
                iv_message         TYPE string
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS extract_param
      IMPORTING iv_params       TYPE string
                iv_name         TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    METHODS extract_param_int
      IMPORTING iv_params       TYPE string
                iv_name         TYPE string
      RETURNING VALUE(rv_value) TYPE i.

ENDCLASS.


CLASS zcl_vsp_debug_service IMPLEMENTATION.

  METHOD zif_vsp_service~get_domain.
    rv_domain = 'debug'.
  ENDMETHOD.


  METHOD zif_vsp_service~handle_message.
    mv_session_id = iv_session_id.
    IF mv_debug_user IS INITIAL.
      mv_debug_user = sy-uname.
    ENDIF.

    CASE is_message-action.
      WHEN 'setBreakpoint'.
        rs_response = handle_set_breakpoint( is_message ).
      WHEN 'getBreakpoints'.
        rs_response = handle_get_breakpoints( is_message ).
      WHEN 'deleteBreakpoint'.
        rs_response = handle_delete_breakpoint( is_message ).
      WHEN 'getStatus'.
        rs_response = handle_get_status( is_message ).
      WHEN OTHERS.
        rs_response = error_response(
          iv_id      = is_message-id
          iv_code    = 'UNKNOWN_ACTION'
          iv_message = |Action '{ is_message-action }' not supported. Available: setBreakpoint, getBreakpoints, deleteBreakpoint, getStatus|
        ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_vsp_service~on_disconnect.
    " Clean up session state
    CLEAR: mv_attached_debuggee, mt_breakpoints.
  ENDMETHOD.


  METHOD handle_set_breakpoint.
    DATA lv_kind TYPE string.
    DATA lv_uri TYPE string.
    DATA lv_line TYPE i.
    DATA lv_exception TYPE string.
    DATA lv_statement TYPE string.
    DATA lv_condition TYPE string.

    " Extract parameters
    lv_kind = extract_param( iv_params = is_message-params iv_name = 'kind' ).
    lv_uri = extract_param( iv_params = is_message-params iv_name = 'uri' ).
    lv_line = extract_param_int( iv_params = is_message-params iv_name = 'line' ).
    lv_exception = extract_param( iv_params = is_message-params iv_name = 'exception' ).
    lv_statement = extract_param( iv_params = is_message-params iv_name = 'statement' ).
    lv_condition = extract_param( iv_params = is_message-params iv_name = 'condition' ).

    IF lv_kind IS INITIAL.
      lv_kind = 'line'.
    ENDIF.

    " Validate based on kind
    CASE lv_kind.
      WHEN 'line'.
        IF lv_uri IS INITIAL OR lv_line <= 0.
          rs_response = error_response(
            iv_id = is_message-id
            iv_code = 'INVALID_PARAMS'
            iv_message = 'Line breakpoint requires uri and line parameters'
          ).
          RETURN.
        ENDIF.
      WHEN 'exception'.
        IF lv_exception IS INITIAL.
          rs_response = error_response(
            iv_id = is_message-id
            iv_code = 'INVALID_PARAMS'
            iv_message = 'Exception breakpoint requires exception parameter (class name)'
          ).
          RETURN.
        ENDIF.
      WHEN 'statement'.
        IF lv_statement IS INITIAL.
          rs_response = error_response(
            iv_id = is_message-id
            iv_code = 'INVALID_PARAMS'
            iv_message = 'Statement breakpoint requires statement parameter'
          ).
          RETURN.
        ENDIF.
      WHEN OTHERS.
        rs_response = error_response(
          iv_id = is_message-id
          iv_code = 'INVALID_KIND'
          iv_message = |Invalid breakpoint kind '{ lv_kind }'. Use: line, exception, statement|
        ).
        RETURN.
    ENDCASE.

    " For now, store locally only (internal HTTP calls to ADT may require auth)
    DATA lv_bp_id TYPE string.
    DATA lv_uuid TYPE sysuuid_c32.
    TRY.
        lv_uuid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        lv_uuid = |BP{ sy-uzeit }{ sy-datum }|.
    ENDTRY.
    lv_bp_id = lv_uuid.

    " Store in local state
    APPEND VALUE #(
      id        = lv_bp_id
      kind      = lv_kind
      uri       = lv_uri
      line      = lv_line
      enabled   = abap_true
      condition = lv_condition
      exception = lv_exception
      statement = lv_statement
    ) TO mt_breakpoints.

    " Build success response
    DATA(lv_brace_open) = '{'.
    DATA(lv_brace_close) = '}'.
    rs_response = VALUE #(
      id      = is_message-id
      success = abap_true
      data    = |{ lv_brace_open }"breakpointId":"{ lv_bp_id }","kind":"{ lv_kind }"| &&
                COND #( WHEN lv_uri IS NOT INITIAL THEN |,"uri":"{ escape_json( lv_uri ) }","line":{ lv_line }| ELSE '' ) &&
                COND #( WHEN lv_exception IS NOT INITIAL THEN |,"exception":"{ escape_json( lv_exception ) }"| ELSE '' ) &&
                COND #( WHEN lv_statement IS NOT INITIAL THEN |,"statement":"{ escape_json( lv_statement ) }"| ELSE '' ) &&
                |,"note":"Stored in session. Use vsp SetExternalBreakpoint for persistent breakpoints."{ lv_brace_close }|
    ).
  ENDMETHOD.


  METHOD handle_get_breakpoints.
    DATA(lv_brace_open) = '{'.
    DATA(lv_brace_close) = '}'.
    DATA(lv_bracket_open) = '['.
    DATA(lv_bracket_close) = ']'.

    " Return locally stored breakpoints
    DATA lv_json TYPE string.
    DATA lv_first TYPE abap_bool VALUE abap_true.

    lv_json = lv_bracket_open.

    LOOP AT mt_breakpoints INTO DATA(ls_bp).
      IF lv_first = abap_false.
        lv_json = |{ lv_json },|.
      ENDIF.
      lv_first = abap_false.

      lv_json = |{ lv_json }{ lv_brace_open }"id":"{ ls_bp-id }","kind":"{ ls_bp-kind }"|.
      IF ls_bp-uri IS NOT INITIAL.
        lv_json = |{ lv_json },"uri":"{ escape_json( ls_bp-uri ) }","line":{ ls_bp-line }|.
      ENDIF.
      IF ls_bp-exception IS NOT INITIAL.
        lv_json = |{ lv_json },"exception":"{ escape_json( ls_bp-exception ) }"|.
      ENDIF.
      IF ls_bp-statement IS NOT INITIAL.
        lv_json = |{ lv_json },"statement":"{ escape_json( ls_bp-statement ) }"|.
      ENDIF.
      lv_json = |{ lv_json }{ lv_brace_close }|.
    ENDLOOP.

    lv_json = |{ lv_json }{ lv_bracket_close }|.

    rs_response = VALUE #(
      id      = is_message-id
      success = abap_true
      data    = |{ lv_brace_open }"breakpoints":{ lv_json },"note":"Session breakpoints only. Use vsp GetExternalBreakpoints for persistent breakpoints."{ lv_brace_close }|
    ).
  ENDMETHOD.


  METHOD handle_delete_breakpoint.
    DATA lv_bp_id TYPE string.
    lv_bp_id = extract_param( iv_params = is_message-params iv_name = 'breakpointId' ).

    IF lv_bp_id IS INITIAL.
      rs_response = error_response(
        iv_id = is_message-id
        iv_code = 'INVALID_PARAMS'
        iv_message = 'breakpointId parameter required'
      ).
      RETURN.
    ENDIF.

    " Check if exists
    READ TABLE mt_breakpoints WITH KEY id = lv_bp_id TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      rs_response = error_response(
        iv_id = is_message-id
        iv_code = 'NOT_FOUND'
        iv_message = |Breakpoint { lv_bp_id } not found in session|
      ).
      RETURN.
    ENDIF.

    " Remove from local state
    DELETE mt_breakpoints WHERE id = lv_bp_id.

    DATA(lv_brace_open) = '{'.
    DATA(lv_brace_close) = '}'.
    rs_response = VALUE #(
      id      = is_message-id
      success = abap_true
      data    = |{ lv_brace_open }"deleted":"{ lv_bp_id }"{ lv_brace_close }|
    ).
  ENDMETHOD.


  METHOD handle_get_status.
    DATA(lv_brace_open) = '{'.
    DATA(lv_brace_close) = '}'.

    DATA lv_bp_count TYPE i.
    lv_bp_count = lines( mt_breakpoints ).

    rs_response = VALUE #(
      id      = is_message-id
      success = abap_true
      data    = |{ lv_brace_open }"sessionId":"{ mv_session_id }","user":"{ mv_debug_user }",| &&
                |"breakpointCount":{ lv_bp_count },"attachedDebuggee":"{ mv_attached_debuggee }"{ lv_brace_close }|
    ).
  ENDMETHOD.


  METHOD extract_param.
    " Extract string parameter from JSON params
    DATA lv_pattern TYPE string.
    lv_pattern = |"{ iv_name }"\\s*:\\s*"([^"]*)"|.
    FIND REGEX lv_pattern IN iv_params SUBMATCHES rv_value.
  ENDMETHOD.


  METHOD extract_param_int.
    " Extract integer parameter from JSON params
    DATA lv_pattern TYPE string.
    DATA lv_str TYPE string.
    lv_pattern = |"{ iv_name }"\\s*:\\s*(\\d+)|.
    FIND REGEX lv_pattern IN iv_params SUBMATCHES lv_str.
    IF sy-subrc = 0.
      rv_value = lv_str.
    ENDIF.
  ENDMETHOD.


  METHOD escape_json.
    rv_escaped = iv_string.
    REPLACE ALL OCCURRENCES OF '\' IN rv_escaped WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_escaped WITH '\"'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_escaped WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_escaped WITH '\n'.
  ENDMETHOD.


  METHOD error_response.
    DATA(lv_brace_open) = '{'.
    DATA(lv_brace_close) = '}'.
    rs_response = VALUE #(
      id      = iv_id
      success = abap_false
      error   = |{ lv_brace_open }"code":"{ iv_code }","message":"{ escape_json( iv_message ) }"{ lv_brace_close }|
    ).
  ENDMETHOD.

ENDCLASS.
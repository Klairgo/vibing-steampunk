"! <p class="shorttext synchronized">VSP RFC Service</p>
"! Enables dynamic RFC/BAPI calls via WebSocket.
"! Actions: call, getMetadata, search
CLASS zcl_vsp_rfc_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_vsp_service.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_param,
        name  TYPE string,
        value TYPE string,
      END OF ty_param,
      tt_params TYPE STANDARD TABLE OF ty_param WITH KEY name.

    METHODS handle_call
      IMPORTING is_message        TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_get_metadata
      IMPORTING is_message        TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS handle_search
      IMPORTING is_message        TYPE zif_vsp_service=>ty_message
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS extract_param
      IMPORTING iv_params       TYPE string
                iv_name         TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    METHODS extract_json_object
      IMPORTING iv_params       TYPE string
                iv_name         TYPE string
      RETURNING VALUE(rv_json)  TYPE string.

    METHODS build_error_response
      IMPORTING iv_id            TYPE string
                iv_code          TYPE string
                iv_message       TYPE string
      RETURNING VALUE(rs_response) TYPE zif_vsp_service=>ty_response.

    METHODS escape_json_string
      IMPORTING iv_string       TYPE string
      RETURNING VALUE(rv_escaped) TYPE string.

ENDCLASS.


CLASS zcl_vsp_rfc_service IMPLEMENTATION.

  METHOD zif_vsp_service~get_domain.
    rv_domain = 'rfc'.
  ENDMETHOD.


  METHOD zif_vsp_service~handle_message.
    CASE is_message-action.
      WHEN 'call'.
        rs_response = handle_call( is_message ).
      WHEN 'getMetadata'.
        rs_response = handle_get_metadata( is_message ).
      WHEN 'search'.
        rs_response = handle_search( is_message ).
      OTHERS.
        rs_response = build_error_response(
          iv_id      = is_message-id
          iv_code    = 'UNKNOWN_ACTION'
          iv_message = |Action '{ is_message-action }' not supported|
        ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_vsp_service~on_disconnect.
    " RFC service is stateless - nothing to clean up
  ENDMETHOD.


  METHOD handle_call.
    " Extract function name from params
    DATA(lv_function) = extract_param( iv_params = is_message-params iv_name = 'function' ).

    IF lv_function IS INITIAL.
      rs_response = build_error_response(
        iv_id      = is_message-id
        iv_code    = 'MISSING_PARAM'
        iv_message = 'Parameter "function" is required'
      ).
      RETURN.
    ENDIF.

    " Convert to uppercase
    TRANSLATE lv_function TO UPPER CASE.

    " Check authorization
    AUTHORITY-CHECK OBJECT 'S_RFC'
      ID 'RFC_TYPE' FIELD 'FUGR'
      ID 'RFC_NAME' DUMMY
      ID 'ACTVT' FIELD '16'.

    IF sy-subrc <> 0.
      rs_response = build_error_response(
        iv_id      = is_message-id
        iv_code    = 'AUTH_ERROR'
        iv_message = 'Not authorized for RFC calls (S_RFC)'
      ).
      RETURN.
    ENDIF.

    " Get function metadata
    DATA: lt_params  TYPE abap_func_parmbind_tab,
          lt_excps   TYPE abap_func_excpbind_tab,
          lo_result  TYPE REF TO data.

    TRY.
        " Get function interface
        DATA(lo_func) = cl_abap_typedescr=>describe_by_name( lv_function ).
        IF lo_func IS NOT BOUND OR lo_func->kind <> cl_abap_typedescr=>kind_func.
          rs_response = build_error_response(
            iv_id      = is_message-id
            iv_code    = 'FUNC_NOT_FOUND'
            iv_message = |Function '{ lv_function }' not found|
          ).
          RETURN.
        ENDIF.

        DATA(lo_func_descr) = CAST cl_abap_funcdescr( lo_func ).

        " Extract import parameters from request
        DATA(lv_imports_json) = extract_json_object( iv_params = is_message-params iv_name = 'imports' ).

        " Build parameter bindings
        LOOP AT lo_func_descr->parameters INTO DATA(ls_param).
          DATA lo_data TYPE REF TO data.

          " Create data reference for parameter
          CREATE DATA lo_data TYPE HANDLE ls_param-type.

          " For import parameters, try to get value from request
          IF ls_param-parm_kind = cl_abap_funcdescr=>importing OR
             ls_param-parm_kind = cl_abap_funcdescr=>changing.
            " Simple string parameter extraction
            DATA(lv_param_value) = extract_param( iv_params = lv_imports_json iv_name = ls_param-name ).
            IF lv_param_value IS NOT INITIAL.
              ASSIGN lo_data->* TO FIELD-SYMBOL(<fs_value>).
              IF sy-subrc = 0.
                TRY.
                    <fs_value> = lv_param_value.
                  CATCH cx_root.
                    " Type mismatch - skip
                ENDTRY.
              ENDIF.
            ENDIF.
          ENDIF.

          " Add to parameter table
          INSERT VALUE #(
            name = ls_param-name
            kind = ls_param-parm_kind
            value = lo_data
          ) INTO TABLE lt_params.
        ENDLOOP.

        " Set up exception handling
        INSERT VALUE #( name = 'OTHERS' value = 10 ) INTO TABLE lt_excps.

        " Call the function
        CALL FUNCTION lv_function
          PARAMETER-TABLE lt_params
          EXCEPTION-TABLE lt_excps.

        DATA(lv_subrc) = sy-subrc.

        " Build response JSON with export/changing/tables
        DATA lv_result_json TYPE string.
        lv_result_json = '{'.

        DATA lv_first TYPE abap_bool VALUE abap_true.

        " Add return code
        lv_result_json = |{ lv_result_json }"sy-subrc":{ lv_subrc }|.
        lv_first = abap_false.

        " Add export parameters
        DATA lv_exports_json TYPE string.
        lv_exports_json = '{'.
        DATA lv_exp_first TYPE abap_bool VALUE abap_true.

        LOOP AT lt_params INTO DATA(ls_result_param)
          WHERE kind = cl_abap_funcdescr=>exporting
             OR kind = cl_abap_funcdescr=>changing.

          ASSIGN ls_result_param-value->* TO FIELD-SYMBOL(<fs_export>).
          IF sy-subrc = 0.
            IF lv_exp_first = abap_false.
              lv_exports_json = |{ lv_exports_json },|.
            ENDIF.

            " Convert to JSON (simple string conversion)
            DATA lv_val_str TYPE string.
            TRY.
                lv_val_str = <fs_export>.
                lv_val_str = escape_json_string( lv_val_str ).
              CATCH cx_root.
                lv_val_str = ''.
            ENDTRY.

            lv_exports_json = |{ lv_exports_json }"{ ls_result_param-name }":"{ lv_val_str }"|.
            lv_exp_first = abap_false.
          ENDIF.
        ENDLOOP.

        lv_exports_json = |{ lv_exports_json }\}|.

        lv_result_json = |{ lv_result_json },"exports":{ lv_exports_json }|.
        lv_result_json = |{ lv_result_json }\}|.

        rs_response = VALUE #(
          id      = is_message-id
          success = abap_true
          data    = lv_result_json
        ).

      CATCH cx_sy_dyn_call_error INTO DATA(lx_call).
        rs_response = build_error_response(
          iv_id      = is_message-id
          iv_code    = 'CALL_ERROR'
          iv_message = lx_call->get_text( )
        ).

      CATCH cx_root INTO DATA(lx_root).
        rs_response = build_error_response(
          iv_id      = is_message-id
          iv_code    = 'ERROR'
          iv_message = lx_root->get_text( )
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_get_metadata.
    " Extract function name
    DATA(lv_function) = extract_param( iv_params = is_message-params iv_name = 'function' ).

    IF lv_function IS INITIAL.
      rs_response = build_error_response(
        iv_id      = is_message-id
        iv_code    = 'MISSING_PARAM'
        iv_message = 'Parameter "function" is required'
      ).
      RETURN.
    ENDIF.

    TRANSLATE lv_function TO UPPER CASE.

    TRY.
        DATA(lo_func) = cl_abap_typedescr=>describe_by_name( lv_function ).
        IF lo_func IS NOT BOUND OR lo_func->kind <> cl_abap_typedescr=>kind_func.
          rs_response = build_error_response(
            iv_id      = is_message-id
            iv_code    = 'FUNC_NOT_FOUND'
            iv_message = |Function '{ lv_function }' not found|
          ).
          RETURN.
        ENDIF.

        DATA(lo_func_descr) = CAST cl_abap_funcdescr( lo_func ).

        " Build metadata JSON
        DATA lv_json TYPE string.
        lv_json = |\{"function":"{ lv_function }","parameters":[|.

        DATA lv_first TYPE abap_bool VALUE abap_true.
        LOOP AT lo_func_descr->parameters INTO DATA(ls_param).
          IF lv_first = abap_false.
            lv_json = |{ lv_json },|.
          ENDIF.

          DATA lv_kind TYPE string.
          CASE ls_param-parm_kind.
            WHEN cl_abap_funcdescr=>importing.
              lv_kind = 'importing'.
            WHEN cl_abap_funcdescr=>exporting.
              lv_kind = 'exporting'.
            WHEN cl_abap_funcdescr=>changing.
              lv_kind = 'changing'.
            WHEN cl_abap_funcdescr=>tables.
              lv_kind = 'tables'.
            WHEN OTHERS.
              lv_kind = 'unknown'.
          ENDCASE.

          DATA lv_type_name TYPE string.
          IF ls_param-type IS BOUND.
            lv_type_name = ls_param-type->get_relative_name( ).
          ENDIF.

          lv_json = |{ lv_json }\{"name":"{ ls_param-name }","kind":"{ lv_kind }","type":"{ lv_type_name }","optional":{ COND #( WHEN ls_param-is_optional = abap_true THEN 'true' ELSE 'false' ) }\}|.
          lv_first = abap_false.
        ENDLOOP.

        lv_json = |{ lv_json }]\}|.

        rs_response = VALUE #(
          id      = is_message-id
          success = abap_true
          data    = lv_json
        ).

      CATCH cx_root INTO DATA(lx_error).
        rs_response = build_error_response(
          iv_id      = is_message-id
          iv_code    = 'ERROR'
          iv_message = lx_error->get_text( )
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_search.
    " Extract search pattern
    DATA(lv_pattern) = extract_param( iv_params = is_message-params iv_name = 'pattern' ).

    IF lv_pattern IS INITIAL.
      lv_pattern = '*'.
    ENDIF.

    TRANSLATE lv_pattern TO UPPER CASE.
    REPLACE ALL OCCURRENCES OF '*' IN lv_pattern WITH '%'.

    " Extract max results
    DATA(lv_max_str) = extract_param( iv_params = is_message-params iv_name = 'maxResults' ).
    DATA lv_max TYPE i VALUE 100.
    IF lv_max_str IS NOT INITIAL.
      TRY.
          lv_max = lv_max_str.
        CATCH cx_root.
          lv_max = 100.
      ENDTRY.
    ENDIF.

    " Search function modules
    SELECT funcname, short_text
      FROM tfdir
      INNER JOIN tftit ON tftit~funcname = tfdir~funcname
                      AND tftit~spras = @sy-langu
      WHERE tfdir~funcname LIKE @lv_pattern
      ORDER BY funcname
      INTO TABLE @DATA(lt_funcs)
      UP TO @lv_max ROWS.

    " Build JSON array
    DATA lv_json TYPE string.
    lv_json = '['.

    DATA lv_first TYPE abap_bool VALUE abap_true.
    LOOP AT lt_funcs INTO DATA(ls_func).
      IF lv_first = abap_false.
        lv_json = |{ lv_json },|.
      ENDIF.

      DATA(lv_text) = escape_json_string( CONV string( ls_func-short_text ) ).
      lv_json = |{ lv_json }\{"name":"{ ls_func-funcname }","description":"{ lv_text }"\}|.
      lv_first = abap_false.
    ENDLOOP.

    lv_json = |{ lv_json }]|.

    rs_response = VALUE #(
      id      = is_message-id
      success = abap_true
      data    = |{"functions":{ lv_json },"count":{ lines( lt_funcs ) }}|
    ).
  ENDMETHOD.


  METHOD extract_param.
    " Extract a string parameter from JSON
    " Simple regex-based extraction

    DATA lv_pattern TYPE string.
    lv_pattern = |"{ iv_name }"\s*:\s*"([^"]*)"|.

    FIND REGEX lv_pattern IN iv_params SUBMATCHES rv_value.
  ENDMETHOD.


  METHOD extract_json_object.
    " Extract a JSON object by key
    " Returns the {...} portion

    DATA lv_pattern TYPE string.
    lv_pattern = |"{ iv_name }"\s*:\s*(\\\{[^}]*\\\})|.

    FIND REGEX lv_pattern IN iv_params SUBMATCHES rv_json.

    IF rv_json IS INITIAL.
      rv_json = '{}'.
    ENDIF.
  ENDMETHOD.


  METHOD build_error_response.
    rs_response = VALUE #(
      id      = iv_id
      success = abap_false
      error   = |\{"code":"{ iv_code }","message":"{ escape_json_string( iv_message ) }"\}|
    ).
  ENDMETHOD.


  METHOD escape_json_string.
    rv_escaped = iv_string.
    REPLACE ALL OCCURRENCES OF '\' IN rv_escaped WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_escaped WITH '\"'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_escaped WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_escaped WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_escaped WITH '\t'.
  ENDMETHOD.

ENDCLASS.

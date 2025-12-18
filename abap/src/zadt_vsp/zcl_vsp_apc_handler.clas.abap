"! <p class="shorttext synchronized">VSP APC WebSocket Handler</p>
"! Unified WebSocket handler for vsp MCP server.
"! Provides stateful operations not available via standard ADT REST.
CLASS zcl_vsp_apc_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apc_wsp_ext_stateful.

    TYPES:
      BEGIN OF ty_message,
        id      TYPE string,
        domain  TYPE string,
        action  TYPE string,
        params  TYPE string,  " JSON string
        timeout TYPE i,
      END OF ty_message,

      BEGIN OF ty_response,
        id      TYPE string,
        success TYPE abap_bool,
        data    TYPE string,  " JSON string
        error   TYPE string,  " JSON string
      END OF ty_response,

      BEGIN OF ty_error,
        code    TYPE string,
        message TYPE string,
        details TYPE string,
      END OF ty_error.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    DATA mo_context    TYPE REF TO if_apc_wsp_context.
    DATA mo_message_manager TYPE REF TO if_apc_wsp_message_manager.
    DATA mv_session_id TYPE string.

    " Service registry
    CLASS-DATA gt_services TYPE STANDARD TABLE OF REF TO zif_vsp_service WITH KEY table_line.

    METHODS parse_message
      IMPORTING iv_text          TYPE string
      RETURNING VALUE(rs_message) TYPE ty_message.

    METHODS send_response
      IMPORTING is_response TYPE ty_response.

    METHODS send_error
      IMPORTING iv_id      TYPE string
                iv_code    TYPE string
                iv_message TYPE string.

    METHODS route_message
      IMPORTING is_message        TYPE ty_message
      RETURNING VALUE(rs_response) TYPE ty_response.

    METHODS handle_ping
      IMPORTING is_message        TYPE ty_message
      RETURNING VALUE(rs_response) TYPE ty_response.

ENDCLASS.


CLASS zcl_vsp_apc_handler IMPLEMENTATION.

  METHOD class_constructor.
    " Register available services
    APPEND NEW zcl_vsp_rfc_service( ) TO gt_services.
    " Future: debug, rca, event services
  ENDMETHOD.


  METHOD if_apc_wsp_ext_stateful~on_start.
    mo_context = i_context.
    mo_message_manager = i_message_manager.

    " Generate session ID
    DATA lv_uuid TYPE sysuuid_c32.
    TRY.
        lv_uuid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        lv_uuid = |VSP{ sy-uzeit }{ sy-datum }|.
    ENDTRY.
    mv_session_id = lv_uuid.

    " Send welcome message
    DATA(ls_response) = VALUE ty_response(
      id      = 'welcome'
      success = abap_true
      data    = |\{"session":"{ mv_session_id }","version":"1.0.0","domains":["rfc"]\}|
    ).
    send_response( ls_response ).
  ENDMETHOD.


  METHOD if_apc_wsp_ext_stateful~on_message.
    DATA(lv_text) = i_message->get_text( ).

    " Parse incoming message
    DATA(ls_message) = parse_message( lv_text ).

    IF ls_message-id IS INITIAL.
      send_error( iv_id = 'unknown' iv_code = 'PARSE_ERROR' iv_message = 'Invalid message format' ).
      RETURN.
    ENDIF.

    " Route and handle
    DATA(ls_response) = route_message( ls_message ).
    send_response( ls_response ).
  ENDMETHOD.


  METHOD if_apc_wsp_ext_stateful~on_close.
    " Cleanup - notify services of disconnect
    LOOP AT gt_services INTO DATA(lo_service).
      lo_service->on_disconnect( mv_session_id ).
    ENDLOOP.
  ENDMETHOD.


  METHOD if_apc_wsp_ext_stateful~on_error.
    " Log error - could extend with more sophisticated handling
  ENDMETHOD.


  METHOD parse_message.
    " Parse JSON message into structure
    " Expected format: {"id":"xxx","domain":"rfc","action":"call","params":{...}}

    TRY.
        " Use simple JSON parsing
        DATA(lo_json) = cl_abap_codepage=>convert_to( lv_text = iv_text codepage = 'UTF-8' ).

        " Extract fields using regex (simple approach)
        " In production, use /ui2/cl_json or similar

        FIND REGEX '"id"\s*:\s*"([^"]*)"' IN iv_text SUBMATCHES rs_message-id.
        FIND REGEX '"domain"\s*:\s*"([^"]*)"' IN iv_text SUBMATCHES rs_message-domain.
        FIND REGEX '"action"\s*:\s*"([^"]*)"' IN iv_text SUBMATCHES rs_message-action.

        " Extract params as raw JSON substring
        DATA lv_params_start TYPE i.
        DATA lv_params_end TYPE i.
        FIND '"params"' IN iv_text MATCH OFFSET lv_params_start.
        IF sy-subrc = 0.
          " Find the colon and opening brace
          FIND REGEX '"params"\s*:\s*(\{.*\})' IN iv_text SUBMATCHES rs_message-params.
        ENDIF.

        " Extract timeout if present
        DATA lv_timeout TYPE string.
        FIND REGEX '"timeout"\s*:\s*(\d+)' IN iv_text SUBMATCHES lv_timeout.
        IF sy-subrc = 0.
          rs_message-timeout = lv_timeout.
        ELSE.
          rs_message-timeout = 30000. " Default 30s
        ENDIF.

      CATCH cx_root.
        CLEAR rs_message.
    ENDTRY.
  ENDMETHOD.


  METHOD send_response.
    TRY.
        DATA(lo_message) = mo_message_manager->create_message( ).

        " Build JSON response
        DATA(lv_json) = |\{"id":"{ is_response-id }","success":{ COND #( WHEN is_response-success = abap_true THEN 'true' ELSE 'false' ) }|.

        IF is_response-data IS NOT INITIAL.
          lv_json = |{ lv_json },"data":{ is_response-data }|.
        ENDIF.

        IF is_response-error IS NOT INITIAL.
          lv_json = |{ lv_json },"error":{ is_response-error }|.
        ENDIF.

        lv_json = |{ lv_json }\}|.

        lo_message->set_text( lv_json ).
        mo_message_manager->send( lo_message ).

      CATCH cx_apc_error INTO DATA(lx_error).
        " Connection may be closed - ignore
    ENDTRY.
  ENDMETHOD.


  METHOD send_error.
    DATA(ls_response) = VALUE ty_response(
      id      = iv_id
      success = abap_false
      error   = |\{"code":"{ iv_code }","message":"{ iv_message }"\}|
    ).
    send_response( ls_response ).
  ENDMETHOD.


  METHOD route_message.
    " Handle built-in actions
    IF is_message-domain = 'system'.
      CASE is_message-action.
        WHEN 'ping'.
          rs_response = handle_ping( is_message ).
          RETURN.
      ENDCASE.
    ENDIF.

    " Route to domain service
    LOOP AT gt_services INTO DATA(lo_service).
      IF lo_service->get_domain( ) = is_message-domain.
        rs_response = lo_service->handle_message(
          iv_session_id = mv_session_id
          is_message    = is_message
        ).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Unknown domain
    rs_response = VALUE #(
      id      = is_message-id
      success = abap_false
      error   = |\{"code":"UNKNOWN_DOMAIN","message":"Domain '{ is_message-domain }' not found"\}|
    ).
  ENDMETHOD.


  METHOD handle_ping.
    rs_response = VALUE #(
      id      = is_message-id
      success = abap_true
      data    = |\{"pong":true,"timestamp":"{ sy-datum }T{ sy-uzeit }"\}|
    ).
  ENDMETHOD.

ENDCLASS.

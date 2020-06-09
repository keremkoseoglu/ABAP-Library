CLASS zcl_bc_jira_wlog_entry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF t_state,
             key TYPE zbcs_jira_isstim_key,
             fld TYPE zbcs_jira_isstim_fld,
           END OF t_state,

           BEGIN OF t_wlog_obj,
             key TYPE zbcs_jira_isstim_key,
             obj TYPE REF TO zcl_bc_jira_wlog_entry,
           END OF t_wlog_obj,

           tt_wlog_obj TYPE STANDARD TABLE OF t_wlog_obj WITH DEFAULT KEY.

    INTERFACES:
      zif_bc_jira_data_actuality_dec,
      zif_bc_jira_issue_persistence.

    CONSTANTS c_data_actuality_key TYPE char10 VALUE 'TEMPO'.

    CLASS-METHODS create_instance
      IMPORTING
        !is_state     TYPE t_state
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_jira_wlog_entry.

    CLASS-METHODS delete_entries_in_date_range
      IMPORTING
        !iv_begda TYPE begda
        !iv_endda TYPE endda.

    CLASS-METHODS get_instance
      IMPORTING
        !is_key       TYPE zbcs_jira_isstim_key
      RETURNING
        VALUE(ro_obj) TYPE REF TO zcl_bc_jira_wlog_entry
      RAISING
        zcx_bc_table_content.

    CLASS-METHODS post_to_jira
      IMPORTING
        !iv_author_name    TYPE zbcd_jira_tim_author_name
        !iv_issue_key      TYPE zbcd_jira_issh_key
        !iv_remain_est_sec TYPE i DEFAULT 0
        !iv_comment        TYPE zbcd_jira_tim_comment
        !iv_date_start     TYPE zbcd_jira_tim_date_start
        !iv_time_spent_sec TYPE zbcd_jira_tim_spent_sec
      RAISING
        zcx_bc_jira_wlog_post.

    METHODS get_fields
      RETURNING
        VALUE(rs_fld) TYPE zbcs_jira_isstim_fld.

    METHODS get_key
      RETURNING
        VALUE(rs_key) TYPE zbcs_jira_isstim_key.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_multiton,
             key   TYPE zbcs_jira_isstim_key,
             obj   TYPE REF TO zcl_bc_jira_wlog_entry,
             cx_tc TYPE REF TO zcx_bc_table_content,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton WITH UNIQUE KEY primary_key COMPONENTS key.

    CONSTANTS:
      c_dest_post_time TYPE char30  VALUE 'ZZTUGJIRA_SAAT_KAYIT',
      c_http_ok        type char3   value '200',
      c_tabname_head   TYPE tabname VALUE 'ZBCT_JIRA_ISSTIM'.

    CLASS-DATA gt_multiton TYPE tt_multiton.

    DATA: gs_state            TYPE t_state,
          gv_instance_from_db TYPE abap_bool ##NEEDED.

    CLASS-METHODS load_multiton
      IMPORTING
        !is_key     TYPE zbcs_jira_isstim_key
      EXPORTING
        er_multiton TYPE REF TO t_multiton
        ev_fresh    TYPE abap_bool.

ENDCLASS.



CLASS zcl_bc_jira_wlog_entry IMPLEMENTATION.


  METHOD create_instance.

    load_multiton( EXPORTING is_key      = is_state-key
                   IMPORTING er_multiton = DATA(lr_multiton) ).

    lr_multiton->obj->gs_state = is_state.
    lr_multiton->obj->gv_instance_from_db = abap_false.
    ro_obj = lr_multiton->obj.

  ENDMETHOD.


  METHOD delete_entries_in_date_range.

    DELETE FROM zbct_jira_isstim
      WHERE tim_date_start GE iv_begda AND
            tim_date_start LE iv_endda.

  ENDMETHOD.


  METHOD get_fields.
    rs_fld = gs_state-fld.
  ENDMETHOD.


  METHOD get_instance.

    load_multiton( EXPORTING is_key      = is_key
                   IMPORTING er_multiton = DATA(lr_multiton)
                             ev_fresh    = DATA(lv_fresh) ).

    IF lr_multiton->cx_tc IS NOT INITIAL.
      RAISE EXCEPTION lr_multiton->cx_tc.
    ENDIF.

    IF lv_fresh EQ abap_true.

      SELECT SINGLE * INTO CORRESPONDING FIELDS OF @lr_multiton->obj->gs_state-fld
             FROM zbct_jira_isstim
             WHERE tim_id EQ @lr_multiton->key-tim_id.

      IF sy-subrc NE 0.
        lr_multiton->cx_tc = NEW #( objectid = CONV #( lr_multiton->key-tim_id )
                                    tabname  = c_tabname_head
                                    textid   = zcx_bc_table_content=>entry_missing ).

        RAISE EXCEPTION lr_multiton->cx_tc.
      ENDIF.

      lr_multiton->obj->gv_instance_from_db = abap_true.

    ENDIF.

    ro_obj = lr_multiton->obj.

  ENDMETHOD.


  METHOD get_key.
    rs_key = gs_state-key.
  ENDMETHOD.


  METHOD load_multiton.

    CLEAR: er_multiton,
           ev_fresh.

    TRY.

        er_multiton = REF #( gt_multiton[ KEY primary_key
                                          COMPONENTS key-tim_id = is_key-tim_id ] ).

      CATCH cx_sy_itab_line_not_found.

        DATA(ls_multiton) = VALUE t_multiton( key = is_key ).
        ls_multiton-obj = NEW #( ).
        ls_multiton-obj->gs_state-key = is_key.

        INSERT ls_multiton INTO TABLE gt_multiton REFERENCE INTO er_multiton.
        ev_fresh = abap_true.

    ENDTRY.

  ENDMETHOD.

  METHOD post_to_jira.

    TRY.

        "  Code below is inspired by https://blogs.sap.com/2014/11/09/calling-an-external-restful-service-from-abap-http-method-post/

        cl_http_client=>create_by_destination(
          EXPORTING
            destination              = c_dest_post_time
          IMPORTING
            client                   = DATA(lo_http_client)
          EXCEPTIONS
            argument_not_found       = 1
            destination_not_found    = 2
            destination_no_authority = 3
            plugin_not_active        = 4
            internal_error           = 5
            OTHERS                   = 6
        ).

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_bc_jira_wlog_post.
        ENDIF.

        data(lo_rest_client) = new cl_rest_http_client( lo_http_client ).

        lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

        datA(lv_timestamp) = zcl_bc_json_toolkit=>sap_datetime_to_json_timestamp( iv_date_start ).

        data(lv_body) =
          |\{| &&
          |"author": \{ "name": "{ iv_author_name }" \}, | &&
          |"issue": \{ "remainingEstimateSeconds": { iv_remain_est_sec }, "key": "{ iv_issue_key }" \}, | &&
          |"comment": "{ iv_comment }", | &&
          |"dateStarted": "{ lv_timestamp }", | &&
          |"timeSpentSeconds": { iv_time_spent_sec }| &&
          |\}|.

        data(lo_request) = lo_rest_client->if_rest_client~create_request_entity( ).
        lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
        lo_request->set_string_data( lv_body ).

        lo_rest_client->if_rest_resource~post( lo_request ).

        data(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).
        data(lv_http_code) = lo_response->get_header_field( '~status_code' ).

        if lv_http_code ne c_http_ok.
          raise exception type zcx_bc_jira_wlog_post.
        endif.

      CATCH zcx_bc_jira_wlog_post INTO DATA(lo_post_error).
        RAISE EXCEPTION lo_post_error.
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_jira_wlog_post
          EXPORTING
            previous = lo_diaper.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_bc_jira_data_actuality_dec~get_data_actuality.

    ev_key  = c_data_actuality_key.
    ev_desc = TEXT-043.

    SELECT MAX( tim_date_start ) INTO @ev_date FROM zbct_jira_isstim.
    CHECK sy-subrc EQ 0.

    SELECT MAX( tim_time_start )
      INTO @ev_time
      FROM zbct_jira_isstim
      WHERE tim_date_start EQ @ev_date.

  ENDMETHOD.


  METHOD zif_bc_jira_issue_persistence~purge_db.
    DELETE FROM zbct_jira_isstim CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
  ENDMETHOD.


  METHOD zif_bc_jira_issue_persistence~write_multiton_to_db.

    LOOP AT gt_multiton ASSIGNING FIELD-SYMBOL(<ls_multiton>) WHERE obj IS NOT INITIAL.
      <ls_multiton>-obj->zif_bc_jira_issue_persistence~write_to_db( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_bc_jira_issue_persistence~write_to_db.

    DATA ls_db TYPE zbct_jira_isstim.

    DELETE FROM zbct_jira_isstim WHERE tim_id EQ gs_state-key-tim_id.

    MOVE-CORRESPONDING: gs_state-key TO ls_db,
                        gs_state-fld TO ls_db.

    INSERT zbct_jira_isstim FROM ls_db.

  ENDMETHOD.
ENDCLASS.
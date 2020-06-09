CLASS zcl_bc_http_spider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: gv_html           TYPE string,
          gv_http_dest      TYPE rfcdest READ-ONLY,
          gv_pos_item_start TYPE i,
          gv_pos_item_end   TYPE i.

    METHODS check_sysubrc
      RAISING
        zcx_bc_symsg.

    METHODS constructor
      IMPORTING
        !iv_http_dest TYPE rfcdest.

    METHODS crop_start
      IMPORTING
        !iv_start_txt   TYPE clike
        !iv_end_txt     TYPE clike
      RETURNING
        VALUE(rv_found) TYPE abap_bool.

    METHODS crop_end.

    METHODS get_first_value_after_tag
      IMPORTING
        !iv_tag         TYPE clike
      RETURNING
        VALUE(rv_value) TYPE string.

    METHODS get_tag_value
      IMPORTING
        !iv_tag         TYPE clike
      RETURNING
        VALUE(rv_value) TYPE string.

    METHODS get_tag_value_after
      IMPORTING
        !iv_start       TYPE clike
        !iv_tag         TYPE clike
      RETURNING
        VALUE(rv_value) TYPE string.

    METHODS http_read
      RAISING
        zcx_bc_int_data_read
        zcx_bc_symsg.

    METHODS replace_sym
      CHANGING
        !cv_val TYPE clike.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF t_bin,
             line TYPE x LENGTH 255,
           END OF t_bin,

           tt_bin TYPE STANDARD TABLE OF t_bin.

    CONSTANTS: c_gt(1) VALUE '>',
               c_lt(1) VALUE '<',
               c_rc_ok TYPE i       VALUE 200.

    DATA: gv_xml_xstring    TYPE xstring.

ENDCLASS.



CLASS ZCL_BC_HTTP_SPIDER IMPLEMENTATION.


  METHOD check_sysubrc.

    CHECK sy-subrc NE 0.

    DATA(lo_cx) = zcx_bc_symsg=>get_instance( ).
    RAISE EXCEPTION lo_cx.

  ENDMETHOD.


  METHOD constructor.
    gv_http_dest = iv_http_dest.
  ENDMETHOD.


  METHOD crop_end.
    SHIFT gv_html LEFT BY gv_pos_item_end PLACES.
  ENDMETHOD.


  METHOD crop_start.

    FIND FIRST OCCURRENCE OF iv_start_txt IN gv_html MATCH OFFSET gv_pos_item_start.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    FIND FIRST OCCURRENCE OF iv_end_txt IN SECTION OFFSET gv_pos_item_start OF gv_html MATCH OFFSET gv_pos_item_end.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    rv_found = abap_true.

  ENDMETHOD.


  METHOD get_first_value_after_tag.

    DATA lv_tag_open TYPE abap_bool.

    DATA(lv_html_len) = strlen( gv_html ).

    FIND FIRST OCCURRENCE OF |<{ iv_tag }| IN SECTION OFFSET gv_pos_item_start OF gv_html MATCH OFFSET DATA(lv_tag_start).
    CHECK sy-subrc EQ 0 AND lv_tag_start LT gv_pos_item_end.

    DATA(lv_pos) = lv_tag_start.

    DO.
      DATA(lv_char) = gv_html+lv_pos(1).

      CASE lv_char.
        WHEN c_lt.
          lv_tag_open  = abap_true.

          IF rv_value is not initial.
            RETURN.
          ENDIF.

        WHEN c_gt.
          lv_tag_open  = abap_false.


        WHEN OTHERS.
          IF lv_tag_open EQ abap_false and lv_char CA '-1234567890QWERTYUIOPĞÜASDFGHJKLŞİZXCVBNMÖÇqwertyuıopğüasdfghjklşizxcvbnmöç'.
            rv_value = |{ rv_value }{ lv_char }|.
            CONDENSE rv_value.
          ENDIF.
      ENDCASE.

      ADD 1 TO lv_pos.
      IF lv_pos GE gv_pos_item_end.
        RETURN.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD get_tag_value.

    FIND FIRST OCCURRENCE OF |<{ iv_tag }| IN gv_html MATCH OFFSET DATA(lv_tag_start).
    CHECK sy-subrc EQ 0 AND lv_tag_start LT gv_pos_item_end.

    FIND FIRST OCCURRENCE OF '>' IN SECTION OFFSET lv_tag_start OF gv_html MATCH OFFSET DATA(lv_tag_end).
    CHECK sy-subrc EQ 0 AND lv_tag_end LT gv_pos_item_end.

    FIND FIRST OCCURRENCE OF '<' IN SECTION OFFSET lv_tag_end OF gv_html MATCH OFFSET DATA(lv_next_tag_start).
    CHECK sy-subrc EQ 0 AND lv_next_tag_start LT gv_pos_item_end.

    ADD 1 TO lv_tag_end.

    DATA(lv_len) = lv_next_tag_start - lv_tag_end.

    rv_value = gv_html+lv_tag_end(lv_len).
    replace_sym( CHANGING cv_val = rv_value ).

  ENDMETHOD.


  METHOD get_tag_value_after.

    FIND FIRST OCCURRENCE OF iv_start IN gv_html MATCH OFFSET DATA(lv_fld_start).
    CHECK sy-subrc EQ 0 AND lv_fld_start LT gv_pos_item_end.

    FIND FIRST OCCURRENCE OF |<{ iv_tag }>| IN SECTION OFFSET lv_fld_start OF gv_html MATCH OFFSET DATA(lv_tag_start).
    CHECK sy-subrc EQ 0 AND lv_tag_start LT gv_pos_item_end.

    FIND FIRST OCCURRENCE OF |</{ iv_tag }>| IN SECTION OFFSET lv_tag_start OF gv_html MATCH OFFSET DATA(lv_tag_end).
    CHECK sy-subrc EQ 0 AND lv_tag_end LT gv_pos_item_end.

    lv_tag_start = lv_tag_start + strlen( iv_tag ) + 2.
    DATA(lv_len) = lv_tag_end - lv_tag_start.

    rv_value = gv_html+lv_tag_start(lv_len).
    replace_sym( CHANGING cv_val = rv_value ).

  ENDMETHOD.


  METHOD http_read.

    DATA: lt_bin TYPE tt_bin,
          lv_len TYPE i.

    cl_http_client=>create_by_destination( EXPORTING destination = gv_http_dest
                                           IMPORTING client      = DATA(lo_client) ).

    lo_client->request->set_method( if_http_request=>co_request_method_get ).

    lo_client->send( EXCEPTIONS http_communication_failure = 1
                                http_invalid_state         = 2
                                http_processing_failed     = 3
                                http_invalid_timeout       = 4
                                OTHERS                     = 5 ).
    check_sysubrc( ).

    lo_client->receive( EXCEPTIONS http_communication_failure = 1
                                   http_invalid_state         = 2
                                   http_processing_failed     = 3
                                   OTHERS                     = 4 ).
    check_sysubrc( ).

    lo_client->response->get_status( IMPORTING code = DATA(lv_rc) ).

    IF lv_rc NE c_rc_ok.
      RAISE EXCEPTION TYPE zcx_bc_int_data_read
        EXPORTING
          textid = zcx_bc_int_data_read=>cant_read.
    ENDIF.

    gv_xml_xstring = lo_client->response->get_data( ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = gv_xml_xstring
      IMPORTING
        output_length = lv_len
      TABLES
        binary_tab    = lt_bin.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = lv_len
      IMPORTING
        text_buffer  = gv_html
      TABLES
        binary_tab   = lt_bin
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

  ENDMETHOD.


  METHOD replace_sym.
    macro_replace_sym: '&#160;'  ` `,
                       '&#199;'  'Ç',
                       '&#214;'  'Ö',
                       '&#220;'  'Ü',
                       '&#226;'  'â',
                       '&#231;'  'ç',
                       '&#246;'  'ö',
                       '&#287;'  'ğ',
                       '&#252;'  'ü',
                       '&#304;'  'İ',
                       '&#305;'  'ı',
                       '&#350;'  'Ş',
                       '&#351;'  'ş',
                       '&#8217;' 'ş',
                       '&apos;'  ''''.
  ENDMETHOD.
ENDCLASS.
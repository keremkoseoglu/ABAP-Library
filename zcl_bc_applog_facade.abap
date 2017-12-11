CLASS zcl_bc_applog_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_APPLOG_FACADE'.
    CONSTANTS c_def_preservation TYPE int4 VALUE 60 ##NO_TEXT.
    CONSTANTS c_msgty_a TYPE symsgty VALUE 'A' ##NO_TEXT.
    CONSTANTS c_msgty_e TYPE symsgty VALUE 'E' ##NO_TEXT.
    CONSTANTS c_msgty_i TYPE symsgty VALUE 'I' ##NO_TEXT.
    CONSTANTS c_msgty_s TYPE symsgty VALUE 'S' ##NO_TEXT.
    CONSTANTS c_msgty_w TYPE symsgty VALUE 'W' ##NO_TEXT.
    CONSTANTS c_msgty_x TYPE symsgty VALUE 'X' ##NO_TEXT.
    CONSTANTS c_option_eq TYPE ddoption VALUE 'EQ' ##NO_TEXT.
    CONSTANTS c_sign_i TYPE ddsign VALUE 'I' ##NO_TEXT.
    CONSTANTS c_sever_all TYPE zbcd_sever VALUE '' ##NO_TEXT.
    CONSTANTS c_sever_error TYPE zbcd_sever VALUE 'E' ##NO_TEXT.
    CONSTANTS c_sever_warning TYPE zbcd_sever VALUE 'W' ##NO_TEXT.
    DATA gv_object TYPE balobj_d .
    DATA gv_subobject TYPE balsubobj .
    DATA gv_log_handle TYPE balloghndl .

    CLASS-METHODS get_crit_msgty_range
      RETURNING
        VALUE(rt_msgty_rng) TYPE rjksd_msgty_range_tab .
    METHODS add_bapi_coru_return
      IMPORTING
        !it_bapi_coru_return TYPE ty_t_bapi_coru_return .
    METHODS add_bapiret1
      IMPORTING
        !it_bapireturn1 TYPE bapiret1_tab .
    METHODS add_bapiret2
      IMPORTING
        !it_bapiret2 TYPE bapiret2_tt .
    METHODS add_bapireturn
      IMPORTING
        !it_bapireturn TYPE isi_bapireturn_tt .
    METHODS add_bapireturn1
      IMPORTING
        !it_bapireturn1 TYPE bapireturn1_tabtype .
    METHODS add_bdcmsgcoll
      IMPORTING
        !it_bdcmsgcoll TYPE tab_bdcmsgcoll .
    METHODS add_exception
      IMPORTING
        !io_cx     TYPE REF TO cx_root
        !iv_msgty  TYPE symsgty DEFAULT c_msgty_e.
    METHODS add_free_text
      IMPORTING
        !iv_text  TYPE c
        !iv_msgty TYPE symsgty DEFAULT c_msgty_s .

    METHODS add_itab_fld_as_free_text
      IMPORTING
        !ir_tab   TYPE REF TO data
        !iv_fld   TYPE fieldname
        !iv_intro TYPE clike OPTIONAL
        !iv_msgty TYPE symsgty DEFAULT c_msgty_s
      RAISING
        zcx_bc_class_method.

    methods add_String importing !iv_msg type string.

    METHODS add_swr
      IMPORTING
        !it_swr TYPE swr_msgtab .

    METHODS add_swr_messag
      IMPORTING
        !it_swr TYPE sapi_msg_lines.

    METHODS add_sy_msg
      IMPORTING
        !iv_cumulate TYPE abap_bool DEFAULT abap_false .
    METHODS add_t100_msg
      IMPORTING
        !iv_msgid    TYPE symsgid
        !iv_msgno    TYPE symsgno
        !iv_msgty    TYPE symsgty
        !iv_msgv1    TYPE data OPTIONAL
        !iv_msgv2    TYPE data OPTIONAL
        !iv_msgv3    TYPE data OPTIONAL
        !iv_msgv4    TYPE data OPTIONAL
        !iv_cumulate TYPE abap_bool DEFAULT abap_false.
    METHODS clear_log .
    METHODS constructor
      IMPORTING
        !iv_object            TYPE balobj_d
        !iv_subobject         TYPE balsubobj
        !iv_preservation_days TYPE int4 DEFAULT c_def_preservation
        !iv_extnumber         TYPE balnrext OPTIONAL
      RAISING
        zcx_bc_log .
    METHODS get_message_count
      IMPORTING
        !iv_msgty_x     TYPE flag DEFAULT abap_true
        !iv_msgty_a     TYPE flag DEFAULT abap_true
        !iv_msgty_e     TYPE flag DEFAULT abap_true
        !iv_msgty_w     TYPE flag DEFAULT abap_true
        !iv_msgty_i     TYPE flag DEFAULT abap_true
        !iv_msgty_s     TYPE flag DEFAULT abap_true
      RETURNING
        VALUE(rv_count) TYPE int4 .
    METHODS get_messages
      IMPORTING
        !iv_msgty_x        TYPE abap_bool DEFAULT abap_true
        !iv_msgty_a        TYPE abap_bool DEFAULT abap_true
        !iv_msgty_e        TYPE abap_bool DEFAULT abap_true
        !iv_msgty_w        TYPE abap_bool DEFAULT abap_true
        !iv_msgty_i        TYPE abap_bool DEFAULT abap_true
        !iv_msgty_s        TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_messages) TYPE bapiret2_t .
    METHODS get_text_of_msgid
      IMPORTING
        VALUE(iv_cl)     TYPE sy-msgid
        VALUE(iv_number) TYPE sy-msgno
      RETURNING
        VALUE(rv_msg)    TYPE bapi_msg .
    METHODS get_worst_severity
      RETURNING
        VALUE(rv_sever) TYPE zbcd_sever .
    METHODS save_to_db
      IMPORTING
        !iv_commit         TYPE abap_bool DEFAULT abap_false
      CHANGING
        VALUE(cv_save_all) TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_bc_log_save .
    METHODS get_log_handle
      RETURNING
        VALUE(rv_log_handle) TYPE balloghndl .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_meth_aifaft TYPE seocpdname VALUE 'ADD_ITAB_FLD_AS_FREE_TEXT'.

    CLASS-METHODS determine_pclass
      IMPORTING
        !iv_msgty        TYPE symsgty
      RETURNING
        VALUE(rv_result) TYPE bal_s_msg-probclass .
ENDCLASS.



CLASS ZCL_BC_APPLOG_FACADE IMPLEMENTATION.


  METHOD add_bapiret1.

    LOOP AT it_bapireturn1 ASSIGNING FIELD-SYMBOL(<ls_br1>).
      add_t100_msg( iv_msgid    = <ls_br1>-id
                    iv_msgno    = <ls_br1>-number
                    iv_msgty    = <ls_br1>-type
                    iv_msgv1    = <ls_br1>-message_v1
                    iv_msgv2    = <ls_br1>-message_v2
                    iv_msgv3    = <ls_br1>-message_v3
                    iv_msgv4    = <ls_br1>-message_v4
                    iv_cumulate = abap_false ).
    ENDLOOP.

  ENDMETHOD. "ADD_BAPIRET1


  METHOD add_bapiret2.

    LOOP AT it_bapiret2 ASSIGNING FIELD-SYMBOL(<ls_br2>).
      add_t100_msg( iv_msgid    = <ls_br2>-id
                    iv_msgno    = <ls_br2>-number
                    iv_msgty    = <ls_br2>-type
                    iv_msgv1    = <ls_br2>-message_v1
                    iv_msgv2    = <ls_br2>-message_v2
                    iv_msgv3    = <ls_br2>-message_v3
                    iv_msgv4    = <ls_br2>-message_v4
                    iv_cumulate = abap_false ).
    ENDLOOP.

  ENDMETHOD. "add_bapiret2


  METHOD add_bapireturn.

    LOOP AT it_bapireturn ASSIGNING FIELD-SYMBOL(<ls_br>).
      add_free_text( iv_text  = <ls_br>-message
                     iv_msgty = <ls_br>-type ).
    ENDLOOP.

  ENDMETHOD. "add_bapireturn


  METHOD add_bapireturn1.

    LOOP AT it_bapireturn1 ASSIGNING FIELD-SYMBOL(<ls_br1>).
      add_t100_msg( iv_msgid    = <ls_br1>-id
                    iv_msgno    = <ls_br1>-number
                    iv_msgty    = <ls_br1>-type
                    iv_msgv1    = <ls_br1>-message_v1
                    iv_msgv2    = <ls_br1>-message_v2
                    iv_msgv3    = <ls_br1>-message_v3
                    iv_msgv4    = <ls_br1>-message_v4
                    iv_cumulate = abap_false ).
    ENDLOOP.

  ENDMETHOD. "ADD_BAPIRETURN1


  METHOD add_bapi_coru_return.

    LOOP AT it_bapi_coru_return ASSIGNING FIELD-SYMBOL(<ls_bcr>).
      add_t100_msg( iv_msgid    = <ls_bcr>-id
                    iv_msgno    = <ls_bcr>-number
                    iv_msgty    = <ls_bcr>-type
                    iv_msgv1    = <ls_bcr>-message_v1
                    iv_msgv2    = <ls_bcr>-message_v2
                    iv_msgv3    = <ls_bcr>-message_v3
                    iv_msgv4    = <ls_bcr>-message_v4
                    iv_cumulate = abap_false ).
    ENDLOOP.

  ENDMETHOD. "ADD_BAPI_CORU_RETURN


  METHOD add_bdcmsgcoll.

    LOOP AT it_bdcmsgcoll ASSIGNING FIELD-SYMBOL(<ls_bmc>).
      add_t100_msg( iv_msgid    = <ls_bmc>-msgid
                    iv_msgno    = CONV #( <ls_bmc>-msgnr )
                    iv_msgty    = <ls_bmc>-msgtyp
                    iv_msgv1    = <ls_bmc>-msgv1
                    iv_msgv2    = <ls_bmc>-msgv2
                    iv_msgv3    = <ls_bmc>-msgv3
                    iv_msgv4    = <ls_bmc>-msgv4
                    iv_cumulate = abap_false ).
    ENDLOOP.

  ENDMETHOD. "ADD_BDCMSGCOLL


  METHOD add_exception.

    CHECK io_cx IS NOT INITIAL.

    add_exception(
        io_cx    = io_cx->previous
        iv_msgty = iv_msgty
    ).


    CASE io_cx->is_resumable.
      WHEN abap_true.

        add_free_text(
            iv_text  = CONV text200( io_cx->get_text( ) )
            iv_msgty = iv_msgty
        ).

      WHEN abap_false.

        MESSAGE io_cx TYPE c_msgty_s.
        sy-msgty = iv_msgty.
        add_sy_msg( ).

    ENDCASE.


  ENDMETHOD. "add_exception


  METHOD add_free_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT' ##FM_SUBRC_OK
      EXPORTING
        i_log_handle  = gv_log_handle
        i_text        = iv_text
        i_msgty       = iv_msgty
        i_probclass   = determine_pclass( iv_msgty )
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.

  ENDMETHOD. "add_free_text


  METHOD add_itab_fld_as_free_text.

    FIELD-SYMBOLS:
      <lt_itab> TYPE ANY TABLE.

    TRY.

        IF iv_intro IS NOT INITIAL.
          add_free_text(
              iv_text  = iv_intro
              iv_msgty = iv_msgty
          ).
        ENDIF.

        CHECK ir_tab IS NOT INITIAL.
        ASSIGN ir_tab->* TO <lt_itab>.

        CHECK
          <lt_itab> IS ASSIGNED AND
          <lt_itab>[] IS NOT INITIAL.

        LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).

          ASSIGN COMPONENT iv_fld
            OF STRUCTURE <ls_itab>
            TO FIELD-SYMBOL(<lv_txt>).

          add_free_text(
              iv_text  = <lv_txt>
              iv_msgty = iv_msgty
          ).

        ENDLOOP.

      CATCH cx_root INTO DATA(lo_cx_root).

        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = lo_cx_root
            class    = c_clsname_me
            method   = c_meth_aifaft.

    ENDTRY.

  ENDMETHOD.

  method add_string.

    DATA:
        lt_br2  type bapiret2_t,
        lt_ret  TYPE TABLE OF char50,
        lv_cmsg(9999).

    lv_cmsg = iv_msg.

    CALL FUNCTION 'IQAPI_WORD_WRAP'
      EXPORTING
        textline            = lv_cmsg
*       DELIMITER           = ' '
        outputlen           = 50
*   IMPORTING
*       OUT_LINE1           =
*       OUT_LINE2           =
*       OUT_LINE3           =
      TABLES
        out_lines           = lt_ret
      EXCEPTIONS
        outputlen_too_large = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_br2 ASSIGNING FIELD-SYMBOL(<ls_return>).
    LOOP AT lt_ret ASSIGNING FIELD-SYMBOL(<ls_ret>).

      <ls_return>-id = 'L1'.
      <ls_return>-number = '000'.
      <ls_return>-type = 'E'.
      IF  <ls_return>-message_v1 IS INITIAL.
        <ls_return>-message_v1 = <ls_ret>.
      ELSEIF  <ls_return>-message_v2 IS INITIAL.
        <ls_return>-message_v2 = <ls_ret>.
      ELSEIF  <ls_return>-message_v3 IS INITIAL.
        <ls_return>-message_v3 = <ls_ret>.
      ELSEIF  <ls_return>-message_v4 IS INITIAL.
        <ls_return>-message_v4 = <ls_ret>.
        APPEND INITIAL LINE TO lt_br2 ASSIGNING <ls_return>.
      ENDIF.
    ENDLOOP.

    DELETE lt_br2 WHERE message_v1 IS INITIAL.

    add_bapiret2( lt_br2 ).

  endmethod.


  METHOD add_swr.

    LOOP AT it_swr ASSIGNING FIELD-SYMBOL(<ls_swr>).
      add_t100_msg( iv_msgid    = <ls_swr>-msgid
                    iv_msgno    = <ls_swr>-msgno
                    iv_msgty    = <ls_swr>-msgty
                    iv_msgv1    = <ls_swr>-msgv1
                    iv_msgv2    = <ls_swr>-msgv2
                    iv_msgv3    = <ls_swr>-msgv3
                    iv_msgv4    = <ls_swr>-msgv4
                    iv_cumulate = abap_false ).
    ENDLOOP.

  ENDMETHOD. "ADD_SWR


  METHOD add_swr_messag.

    LOOP AT it_swr ASSIGNING FIELD-SYMBOL(<ls_swr>).
      add_free_text( iv_text  = <ls_swr>-line
                     iv_msgty = <ls_swr>-msg_type ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_sy_msg.

    add_t100_msg( iv_msgid    = sy-msgid
                  iv_msgno    = sy-msgno
                  iv_msgty    = sy-msgty
                  iv_msgv1    = sy-msgv1
                  iv_msgv2    = sy-msgv2
                  iv_msgv3    = sy-msgv3
                  iv_msgv4    = sy-msgv4
                  iv_cumulate = iv_cumulate ).

  ENDMETHOD. "add_sy_msg


  METHOD add_t100_msg.

    DATA: ls_log_msg TYPE bal_s_msg.

*   Veri hazırlığı
    ls_log_msg-msgty = iv_msgty.
    ls_log_msg-msgid = iv_msgid.
    ls_log_msg-msgno = iv_msgno.

    ls_log_msg-probclass = determine_pclass( ls_log_msg-msgty ).

*   Parametreler
    WRITE: iv_msgv1 TO ls_log_msg-msgv1 LEFT-JUSTIFIED,
           iv_msgv2 TO ls_log_msg-msgv2 LEFT-JUSTIFIED,
           iv_msgv3 TO ls_log_msg-msgv3 LEFT-JUSTIFIED,
           iv_msgv4 TO ls_log_msg-msgv4 LEFT-JUSTIFIED.

*   Ekle veya kümüle et
    IF iv_cumulate IS INITIAL.

      CALL FUNCTION 'BAL_LOG_MSG_ADD' ##fm_subrc_ok
        EXPORTING
          i_log_handle  = gv_log_handle
          i_s_msg       = ls_log_msg
        EXCEPTIONS
          log_not_found = 0
          OTHERS        = 1.

    ELSE.

      CALL FUNCTION 'BAL_LOG_MSG_CUMULATE' ##fm_subrc_ok
        EXPORTING
          i_log_handle     = gv_log_handle
          i_s_msg          = ls_log_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

    ENDIF.

  ENDMETHOD. "add_t100_msg


  METHOD clear_log.

    CHECK gv_log_handle IS NOT INITIAL.

    DATA: lv_msg_handle TYPE  balmsghndl.
    lv_msg_handle = CONV #( gv_log_handle ).

    CALL FUNCTION 'BAL_LOG_MSG_DELETE'
      EXPORTING
        i_s_msg_handle = lv_msg_handle
      EXCEPTIONS
        msg_not_found  = 1
        log_not_found  = 2
        OTHERS         = 3 ##FM_SUBRC_OK.

    CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL' ##fm_subrc_ok
      EXPORTING
        i_log_handle  = gv_log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.


  ENDMETHOD. "clear_log


  METHOD constructor.

    gv_object    = iv_object.
    gv_subobject = iv_subobject.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = VALUE bal_s_log( object     = gv_object
                                        subobject  = gv_subobject
                                        aluser     = sy-uname
                                        altcode    = sy-tcode
                                        alprog     = sy-cprog
                                        almode     = COND #( WHEN sy-batch IS INITIAL THEN 'D' ELSE 'B' )
                                        aldate_del = COND #( WHEN iv_preservation_days IS NOT INITIAL THEN sy-datum + iv_preservation_days )
                                        extnumber  = iv_extnumber )
      IMPORTING
        e_log_handle = gv_log_handle
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_log
        EXPORTING
          textid    = zcx_bc_log=>cant_create_instance
          previous  = zcx_bc_symsg=>get_instance( )
          object    = gv_object
          subobject = gv_subobject.

    ENDIF.

  ENDMETHOD. "constructor


  METHOD determine_pclass.

    rv_result = COND #( WHEN iv_msgty IN get_crit_msgty_range( ) THEN '1'
                        WHEN iv_msgty EQ c_msgty_w THEN '2'
                        WHEN iv_msgty EQ c_msgty_i OR iv_msgty EQ c_msgty_s THEN '4'
                        ELSE '2' ).

  ENDMETHOD. "determine_pclass


  METHOD get_crit_msgty_range.

    STATICS lt_msgty TYPE rjksd_msgty_range_tab.

    IF lt_msgty[] IS INITIAL.
      lt_msgty = VALUE #( option = c_option_eq sign = c_sign_i ( low = c_msgty_x )
                                                               ( low = c_msgty_a )
                                                               ( low = c_msgty_e ) ).
    ENDIF.

    rt_msgty_rng[] = lt_msgty[].

  ENDMETHOD. "get_crit_msgty_range


  METHOD get_log_handle.
    rv_log_handle = gv_log_handle.
  ENDMETHOD.


  METHOD get_messages.

    DATA: lt_log_handle TYPE bal_t_logh,
          lt_msg_handle TYPE bal_t_msgh,

          ls_msg        TYPE bal_s_msg,
          ls_msg_filter TYPE bal_s_mfil,
          ls_msg_bapi   TYPE bapiret2.

    CHECK gv_log_handle IS NOT INITIAL.
    APPEND gv_log_handle TO lt_log_handle.

    IF iv_msgty_x IS NOT INITIAL.
      APPEND VALUE #( option = c_option_eq
                      sign   = c_sign_i
                      low    = c_msgty_x ) TO ls_msg_filter-msgty.
    ENDIF.

    IF iv_msgty_a IS NOT INITIAL.
      APPEND VALUE #( option = c_option_eq
                      sign   = c_sign_i
                      low    = c_msgty_a ) TO ls_msg_filter-msgty.
    ENDIF.

    IF iv_msgty_e IS NOT INITIAL.
      APPEND VALUE #( option = c_option_eq
                      sign   = c_sign_i
                      low    = c_msgty_e ) TO ls_msg_filter-msgty.
    ENDIF.

    IF iv_msgty_w IS NOT INITIAL.
      APPEND VALUE #( option = c_option_eq
                      sign   = c_sign_i
                      low    = c_msgty_w ) TO ls_msg_filter-msgty.
    ENDIF.

    IF iv_msgty_i IS NOT INITIAL.
      APPEND VALUE #( option = c_option_eq
                      sign   = c_sign_i
                      low    = c_msgty_i ) TO ls_msg_filter-msgty.
    ENDIF.

    IF iv_msgty_s IS NOT INITIAL.
      APPEND VALUE #( option = c_option_eq
                      sign   = c_sign_i
                      low    = c_msgty_s ) TO ls_msg_filter-msgty.
    ENDIF.

    CHECK ls_msg_filter-msgty[] IS NOT INITIAL.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = lt_log_handle
        i_s_msg_filter = ls_msg_filter
      IMPORTING
        e_t_msg_handle = lt_msg_handle
      EXCEPTIONS
        msg_not_found  = 1.

    CHECK sy-subrc IS INITIAL.

    LOOP AT lt_msg_handle ASSIGNING FIELD-SYMBOL(<ls_msg_handle>).

      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <ls_msg_handle>
          i_langu        = sy-langu
        IMPORTING
          e_s_msg        = ls_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2.

      CHECK sy-subrc IS INITIAL.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = ls_msg-msgty
          cl     = ls_msg-msgid
          number = ls_msg-msgno
          par1   = ls_msg-msgv1
          par2   = ls_msg-msgv2
          par3   = ls_msg-msgv3
          par4   = ls_msg-msgv4
        IMPORTING
          return = ls_msg_bapi.

      APPEND ls_msg_bapi TO rt_messages.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_message_count.

    rv_count = lines( get_messages( iv_msgty_x = iv_msgty_x
                                    iv_msgty_a = iv_msgty_a
                                    iv_msgty_e = iv_msgty_e
                                    iv_msgty_w = iv_msgty_w
                                    iv_msgty_i = iv_msgty_i
                                    iv_msgty_s = iv_msgty_s ) ).

  ENDMETHOD. "get_message_count


  METHOD get_text_of_msgid.

**==========================================================*
** Sample Code for calling Method
**==========================================================*
*    data : lo_log       type ref to zcl_bc_applog_facade.
*    data : gv_long_msg  type bapi_msg.
*
*    lo_log = new #( iv_object = 'A' iv_subobject = 'B' ).
*
*    gv_long_msg = lo_log->get_text_of_msgid( exporting  iv_cl = v_msgid  iv_number = v_msgno ).
**==========================================================*

    DATA : ls_msg_bapi TYPE bapiret2.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = 'S'
        cl     = iv_cl
        number = iv_number
      IMPORTING
        return = ls_msg_bapi.

    rv_msg = ls_msg_bapi-message.

  ENDMETHOD.


  METHOD get_worst_severity.

    DATA(lt_msg) = get_messages( ).

    LOOP AT lt_msg TRANSPORTING NO FIELDS WHERE type IN get_crit_msgty_range(  ).
      rv_sever = c_sever_error.
      RETURN.
    ENDLOOP.

    IF line_exists( lt_msg[ type = c_msgty_w ] ).
      rv_sever = c_sever_warning.
      RETURN.
    ENDIF.

    rv_sever = c_sever_all.

  ENDMETHOD. "get_worst_severity


  METHOD save_to_db.

    DATA lt_log_handle TYPE bal_t_logh.


    APPEND gv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = abap_true
        i_save_all       = cv_save_all
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc_log_save
        EXPORTING
          textid    = zcx_bc_log_save=>cant_save
          object    = gv_object
          subobject = gv_subobject
          previous  = zcx_bc_symsg=>get_instance( ).
    ENDIF.

    IF iv_commit EQ abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD. "save_to_db
ENDCLASS.
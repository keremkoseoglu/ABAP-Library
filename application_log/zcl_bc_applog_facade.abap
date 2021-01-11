CLASS zcl_bc_applog_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_APPLOG_FACADE' ##NO_TEXT.
    CONSTANTS c_def_preservation TYPE int4 VALUE ycl_simbal=>default_preservation.
    CONSTANTS c_msgty_a TYPE symsgty VALUE ycl_simbal=>msgty-abort.
    CONSTANTS c_msgty_e TYPE symsgty VALUE ycl_simbal=>msgty-error.
    CONSTANTS c_msgty_i TYPE symsgty VALUE ycl_simbal=>msgty-info.
    CONSTANTS c_msgty_s TYPE symsgty VALUE ycl_simbal=>msgty-status.
    CONSTANTS c_msgty_w TYPE symsgty VALUE ycl_simbal=>msgty-warning.
    CONSTANTS c_msgty_x TYPE symsgty VALUE ycl_simbal=>msgty-exit.
    CONSTANTS c_sever_all TYPE zbcd_sever VALUE ycl_simbal=>severity-all.
    CONSTANTS c_sever_error TYPE zbcd_sever VALUE ycl_simbal=>severity-error.
    CONSTANTS c_sever_lock TYPE zbcd_sever VALUE ycl_simbal=>severity-lock.
    CONSTANTS c_sever_warning TYPE zbcd_sever VALUE ycl_simbal=>severity-warning.
    DATA gv_object TYPE balobj_d READ-ONLY.
    DATA gv_subobject TYPE balsubobj READ-ONLY.
    DATA gv_log_handle TYPE balloghndl READ-ONLY.

    CLASS-METHODS get_crit_msgty_range
      RETURNING
        VALUE(rt_msgty_rng) TYPE rjksd_msgty_range_tab .

    CLASS-METHODS enqueue_slg2 EXPORTING VALUE(success) TYPE abap_bool.

    METHODS add_bapi_coru_return
      IMPORTING
        !it_bapi_coru_return TYPE ty_t_bapi_coru_return .
    METHODS add_bapiret1
      IMPORTING
        !it_bapireturn1 TYPE bapiret1_tab .
    METHODS add_bapiret2
      IMPORTING
        !it_bapiret2 TYPE bapiret2_tt
        !iv_cumulate TYPE abap_bool DEFAULT abap_false .
    METHODS add_bapireturn
      IMPORTING
        !it_bapireturn TYPE isi_bapireturn_tt .
    METHODS add_bapireturn1
      IMPORTING
        !it_bapireturn1 TYPE bapireturn1_tabtype .
    METHODS add_bdcmsgcoll
      IMPORTING
        !it_bdcmsgcoll TYPE tab_bdcmsgcoll .

    METHODS add_bcsy_text
      IMPORTING
        !it_bcsy_text TYPE bcsy_text
        !iv_msgty     TYPE symsgty DEFAULT c_msgty_s .

    METHODS add_deepest_exception
      IMPORTING
        !io_cx    TYPE REF TO cx_root
        !iv_msgty TYPE symsgty DEFAULT c_msgty_e .

    METHODS add_exception
      IMPORTING
        !io_cx    TYPE REF TO cx_root
        !iv_msgty TYPE symsgty DEFAULT c_msgty_e .

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
        zcx_bc_class_method .
    METHODS add_merrdat_f_tab
      IMPORTING
        !it_merrdat TYPE merrdat_f_tab .
    METHODS add_string
      IMPORTING
        !iv_msg TYPE string .
    METHODS add_swr
      IMPORTING
        !it_swr TYPE swr_msgtab .
    METHODS add_swr_messag
      IMPORTING
        !it_swr TYPE sapi_msg_lines .
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
        !iv_cumulate TYPE abap_bool DEFAULT abap_false .
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

    METHODS get_messages_as_json
      IMPORTING
        !iv_msgty_x    TYPE abap_bool DEFAULT abap_true
        !iv_msgty_a    TYPE abap_bool DEFAULT abap_true
        !iv_msgty_e    TYPE abap_bool DEFAULT abap_true
        !iv_msgty_w    TYPE abap_bool DEFAULT abap_true
        !iv_msgty_i    TYPE abap_bool DEFAULT abap_true
        !iv_msgty_s    TYPE abap_bool DEFAULT abap_true
        !iv_short      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS get_text_of_msgid
      IMPORTING
        VALUE(iv_cl)     TYPE sy-msgid
        VALUE(iv_number) TYPE sy-msgno
      RETURNING
        VALUE(rv_msg)    TYPE bapi_msg .
    METHODS get_worst_severity
      RETURNING
        VALUE(rv_sever) TYPE zbcd_sever .
    METHODS save_to_db_2th_connection
      CHANGING
        VALUE(cv_save_all) TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_bc_log_save .
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

    METHODS add_mesg IMPORTING !it_mesg TYPE mesg_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_meth_aifaft TYPE seocpdname VALUE 'ADD_ITAB_FLD_AS_FREE_TEXT'.

    CONSTANTS: BEGIN OF tcode,
                 slg2 TYPE sytcode VALUE 'SLG2',
               END OF tcode.

    DATA simbal TYPE REF TO ycl_simbal.
ENDCLASS.



CLASS zcl_bc_applog_facade IMPLEMENTATION.
  METHOD get_crit_msgty_range.
    rt_msgty_rng = ycl_simbal=>get_crit_msgty_range( ).
  ENDMETHOD. "get_crit_msgty_range


  METHOD enqueue_slg2.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " SLG2 işlem kodunda mükerrer silme işlemi başlamasın diye
    " kilit koyuyoruz.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'ENQUEUE_EZBC_TCODE'
      EXPORTING
        tcode          = zcl_bc_applog_facade=>tcode-slg2
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    success = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD add_bapiret1.
    me->simbal->add_bapiret1( it_bapireturn1 ).
  ENDMETHOD.


  METHOD add_bapiret2.
    me->simbal->add_bapiret2( it_bapiret2 ).
  ENDMETHOD.


  METHOD add_bapireturn.
    me->simbal->add_bapireturn( CORRESPONDING #( it_bapireturn ) ).
  ENDMETHOD.


  METHOD add_bapireturn1.
    me->simbal->add_bapireturn1( CORRESPONDING #( it_bapireturn1 ) ).
  ENDMETHOD.


  METHOD add_bapi_coru_return.
    me->simbal->add_bapi_coru_return( CORRESPONDING #( it_bapi_coru_return ) ).
  ENDMETHOD.


  METHOD add_bcsy_text.
    me->simbal->add_bcsy_text( bcsy_text = it_bcsy_text
                               msgty     = iv_msgty ).
  ENDMETHOD.


  METHOD add_bdcmsgcoll.
    me->simbal->add_bdcmsgcoll( it_bdcmsgcoll ).
  ENDMETHOD.


  METHOD add_deepest_exception.
    me->simbal->add_deepest_exception( cx    = io_cx
                                       msgty = iv_msgty ).
  ENDMETHOD.


  METHOD add_exception.
    me->simbal->add_exception( cx    = io_cx
                               msgty = iv_msgty ).
  ENDMETHOD.


  METHOD add_free_text.
    me->simbal->add_free_text(
        text  = iv_text
        msgty = iv_msgty ).
  ENDMETHOD.


  METHOD add_itab_fld_as_free_text.
    TRY.
        me->simbal->add_itab_fld_as_free_text(
            tab   = ir_tab
            fld   = iv_fld
            intro = iv_intro
            msgty = iv_msgty ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_class_method
          EXPORTING
            textid   = zcx_bc_class_method=>unexpected_error
            previous = diaper
            class    = c_clsname_me
            method   = c_meth_aifaft.
    ENDTRY.
  ENDMETHOD.


  METHOD add_merrdat_f_tab.
    LOOP AT it_merrdat ASSIGNING FIELD-SYMBOL(<ls_merrdat>).
      me->simbal->add_t100_msg(
        msgid = <ls_merrdat>-msgid
        msgno = <ls_merrdat>-msgno
        msgty = <ls_merrdat>-msgty
        msgv1 = <ls_merrdat>-msgv1
        msgv2 = <ls_merrdat>-msgv2
        msgv3 = <ls_merrdat>-msgv3
        msgv4 = <ls_merrdat>-msgv4 ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_string.
    me->simbal->add_string( iv_msg ).
  ENDMETHOD.


  METHOD add_swr.
    me->simbal->add_swr( it_swr ).
  ENDMETHOD.


  METHOD add_swr_messag.
    me->simbal->add_swr_messag( it_swr ).
  ENDMETHOD.


  METHOD add_sy_msg.
    me->simbal->add_sy_msg( cumulate = iv_cumulate ).
  ENDMETHOD.


  METHOD add_t100_msg.
    me->simbal->add_t100_msg(
        msgid    = iv_msgid
        msgno    = iv_msgno
        msgty    = iv_msgty
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4
        cumulate = iv_cumulate ).
  ENDMETHOD.


  METHOD clear_log.
    me->simbal->clear_log( ).
  ENDMETHOD.


  METHOD constructor.
    TRY.
        me->simbal = NEW #( object            = iv_object
                            subobject         = iv_subobject
                            preservation_days = iv_preservation_days
                            extnumber         = iv_extnumber ).

        me->gv_object    = me->simbal->object.
        me->gv_subobject = me->simbal->subobject.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_log
          EXPORTING
            textid    = zcx_bc_log=>cant_create_instance
            previous  = diaper
            object    = gv_object
            subobject = gv_subobject.

    ENDTRY.
  ENDMETHOD.


  METHOD get_log_handle.
    rv_log_handle = me->simbal->log_handle.
  ENDMETHOD.


  METHOD get_messages.
    rt_messages = me->simbal->get_messages(
        msgty_x = iv_msgty_x
        msgty_a = iv_msgty_a
        msgty_e = iv_msgty_e
        msgty_w = iv_msgty_w
        msgty_i = iv_msgty_i
        msgty_s = iv_msgty_s ).
  ENDMETHOD.


  METHOD get_messages_as_json.
    rv_json = me->simbal->get_messages_as_json(
        msgty_x = iv_msgty_x
        msgty_a = iv_msgty_a
        msgty_e = iv_msgty_e
        msgty_w = iv_msgty_w
        msgty_i = iv_msgty_i
        msgty_s = iv_msgty_s
        short   = iv_short ).
  ENDMETHOD.


  METHOD get_message_count.
    rv_count = me->simbal->get_message_count(
        msgty_x = iv_msgty_x
        msgty_a = iv_msgty_a
        msgty_e = iv_msgty_e
        msgty_w = iv_msgty_w
        msgty_i = iv_msgty_i
        msgty_s = iv_msgty_s ).
  ENDMETHOD.


  METHOD get_text_of_msgid.
    rv_msg = me->simbal->get_text_of_msgid(
        cl     = iv_cl
        number = iv_number ).
  ENDMETHOD.


  METHOD get_worst_severity.
    rv_sever = me->simbal->get_worst_severity( ).
  ENDMETHOD.


  METHOD save_to_db.
    TRY.
        me->simbal->save_to_db(
          EXPORTING commit   = iv_commit
          CHANGING  save_all = cv_save_all ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_log_save
          EXPORTING
            textid    = zcx_bc_log_save=>cant_save
            object    = gv_object
            subobject = gv_subobject
            previous  = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD save_to_db_2th_connection.
    TRY.
        me->simbal->save_to_db_2nd_connection(
          CHANGING save_all = cv_save_all ).

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE zcx_bc_log_save
          EXPORTING
            textid    = zcx_bc_log_save=>cant_save
            object    = gv_object
            subobject = gv_subobject
            previous  = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD add_mesg.
    me->simbal->add_mesg( CORRESPONDING #( it_mesg ) ).
  ENDMETHOD.
ENDCLASS.
CLASS zcl_bc_sap_user DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_bname,
        bname TYPE xubname,
      END OF t_bname,

      tt_bname TYPE STANDARD TABLE OF t_bname WITH DEFAULT KEY,

      BEGIN OF t_bname_text,
        bname TYPE xubname,
        text  TYPE string,
      END OF t_bname_text,

      tt_bname_text TYPE STANDARD TABLE OF t_bname_text WITH DEFAULT KEY,

      BEGIN OF t_bulk_pwdchgdate,
        bname      TYPE xubname,
        pwdchgdate TYPE usr02-pwdchgdate,
      END OF t_bulk_pwdchgdate,

      tt_bulk_pwdchgdate TYPE STANDARD TABLE OF t_bulk_pwdchgdate WITH DEFAULT KEY.

    constants:
      c_actvt_change  type ACTIV_AUTH value '02',
      c_actvt_display type ACTIV_AUTH value '03'.

    DATA:
      gv_bname TYPE xubname READ-ONLY.

    CLASS-METHODS:

      check_pwdchgdate_auth
        importing !iv_actvt type activ_auth
        raising   zcx_bc_authorization,

      get_full_name_wo_error
        IMPORTING !iv_bname      TYPE xubname
        RETURNING VALUE(rv_name) TYPE full_name,

      get_instance
        IMPORTING !iv_bname     TYPE xubname
        RETURNING VALUE(ro_obj) TYPE REF TO zcl_bc_sap_user
        RAISING   zcx_bc_user_master_data,

      set_pwd_chg_date_bulk
        IMPORTING
          !it_date    TYPE tt_bulk_pwdchgdate
          !io_log     TYPE REF TO zcl_bc_applog_facade OPTIONAL
        EXPORTING
          !et_success TYPE tt_bname
          !et_failure TYPE tt_bname_text.

    METHODS:
      get_email returning value(rv_email) type AD_SMTPADR,

      get_full_name    RETURNING VALUE(rv_name) TYPE full_name,

      get_pwd_chg_date
        IMPORTING !iv_force_fresh TYPE abap_bool
        RETURNING VALUE(rv_date)  TYPE xubcdat,

      get_uname_text   RETURNING VALUE(rv_utext) TYPE ad_namtext,

      set_pwd_chg_date
        IMPORTING !iv_pwdchgdate TYPE usr02-pwdchgdate
        RAISING
          zcx_bc_authorization
          zcx_bc_lock.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_lazy_flag,
        email      type abap_bool,
        full_name  TYPE abap_bool,
        pwdchgdate TYPE abap_bool,
        uname_text TYPE abap_bool,
      END OF t_lazy_flag,

      BEGIN OF t_lazy_var,
        email      type AD_SMTPADR,
        full_name  TYPE full_name,
        pwdchgdate TYPE xubcdat,
        uname_text TYPE name_text,
      END OF t_lazy_var,

      BEGIN OF t_multiton,
        bname TYPE xubname,
        obj   TYPE REF TO zcl_bc_sap_user,
      END OF t_multiton,

      tt_multiton
        TYPE HASHED TABLE OF t_multiton
        WITH UNIQUE KEY primary_key COMPONENTS bname.

    CLASS-DATA:
      gt_multiton TYPE tt_multiton.

    DATA:
      gs_lazy_flag TYPE t_lazy_flag,
      gs_lazy_var  TYPE t_lazy_var.

    CLASS-METHODS:

      dequeue_user
        IMPORTING !iv_bname TYPE xubname,

      enqueue_user
        IMPORTING !iv_bname TYPE xubname
        RAISING   zcx_bc_lock.

ENDCLASS.



CLASS ZCL_BC_SAP_USER IMPLEMENTATION.


  method check_pwdchgdate_auth.

    AUTHORITY-CHECK OBJECT 'ZBCAOPDC' ID 'ACTVT' field iv_actvt.
    check sy-subrc ne 0.

    raise exception type zcx_bc_authorization
      EXPORTING
        textid    = zcx_Bc_Authorization=>no_auth.

  endmethod.


  METHOD dequeue_user.

    CALL FUNCTION 'DEQUEUE_E_USR04'
      EXPORTING
        bname = iv_bname.

  ENDMETHOD.


  METHOD enqueue_user.

    TRY.

        CALL FUNCTION 'ENQUEUE_E_USR04'
          EXPORTING
            bname          = iv_bname
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3
            ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'ENQUEUE_E_USR04' ).

      CATCH zcx_bc_function_subrc INTO DATA(lo_cx_fsr).

        RAISE EXCEPTION TYPE zcx_bc_lock
          EXPORTING
            textid   = zcx_bc_lock=>locked_by_user
            previous = lo_cx_fsr
            bname    = CONV #( sy-msgv1 ).

    ENDTRY.

  ENDMETHOD.

  method get_email.

    data: lt_Return type standard table of BAPIRET2,
          lt_Smtp   type standard table of BAPIADSMTP.

    if gs_lazy_flag-email is INITIAL.

      call function 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username       = gv_bname
          cache_results  = abap_False
        TABLES
          return         = lt_Return
          addsmtp        = lt_smtp.

      sort lt_Smtp by std_no descending.

      loop at lt_smtp assigning field-symbol(<ls_smtp>)
        where ( valid_from is initial )
           or ( valid_from le sy-datum and
                valid_to   ge sy-datum ).

        gs_lazy_var-email = <ls_smtp>-e_mail.
        exit.
      endloop.

      gs_lazy_flag-email = Abap_True.
    endif.

    rv_email = gs_lazy_var-email.

  endmethod.

  METHOD get_full_name.

    IF gs_lazy_flag-full_name EQ abap_false.

      SELECT SINGLE name_first && @space && name_last
        INTO @gs_lazy_var-full_name
        FROM adrp
        WHERE persnumber EQ ( SELECT persnumber FROM usr21 WHERE bname = @gv_bname )
        ##WARN_OK.

      gs_lazy_flag-full_name = abap_true.
    ENDIF.

    rv_name = gs_lazy_var-full_name.

  ENDMETHOD.


  METHOD get_full_name_wo_error.

    TRY.
        rv_name = get_instance( iv_bname )->get_full_name( ).
      CATCH cx_root ##no_handler.
    ENDTRY.

  ENDMETHOD.


  METHOD get_instance.

    ASSIGN gt_multiton[
      KEY primary_key
      COMPONENTS bname = iv_bname
    ] TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc NE 0.

      DATA(ls_multiton) = VALUE t_multiton( bname = iv_bname ).

      SELECT SINGLE gltgv, gltgb
        INTO @DATA(ls_usr02)
        FROM usr02
        WHERE bname EQ @ls_multiton-bname.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_bc_user_master_data
          EXPORTING
            textid = zcx_bc_user_master_data=>user_unknown
            uname  = ls_multiton-bname.
      ENDIF.

      IF ( ls_usr02-gltgv IS NOT INITIAL AND ls_usr02-gltgv GT sy-datum ) OR
         ( ls_usr02-gltgb IS NOT INITIAL AND ls_usr02-gltgb LT sy-datum ).

        RAISE EXCEPTION TYPE zcx_bc_user_master_data
          EXPORTING
            textid = zcx_bc_user_master_data=>user_inactive
            uname  = ls_multiton-bname.

      ENDIF.

      ls_multiton-obj = NEW #( ).
      ls_multiton-obj->gv_bname = ls_multiton-bname.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.

    ENDIF.

    ro_obj = <ls_multiton>-obj.

  ENDMETHOD.


  METHOD get_pwd_chg_date.

    gs_lazy_flag-pwdchgdate = SWITCH #( iv_force_fresh WHEN abap_true THEN abap_false ).

    IF gs_lazy_flag-pwdchgdate EQ abap_false.

      SELECT SINGLE pwdchgdate
        INTO @gs_lazy_var-pwdchgdate
        FROM usr02
        WHERE bname EQ @gv_bname.

      gs_lazy_flag-pwdchgdate = abap_true.

    ENDIF.

    rv_date = gs_lazy_var-pwdchgdate.

  ENDMETHOD.


  METHOD get_uname_text.

    IF gs_lazy_flag-uname_text EQ abap_false.

      SELECT SINGLE name_textc
        INTO @gs_lazy_var-uname_text
        FROM user_addr
        WHERE bname EQ @gv_bname
        ##WARN_OK.

      gs_lazy_flag-uname_text = abap_true.
    ENDIF.

    rv_utext = gs_lazy_var-uname_text.

  ENDMETHOD.


  METHOD set_pwd_chg_date.

    DATA:
      lo_obj TYPE REF TO object,
      lo_obs TYPE REF TO zif_bc_pwdchgdate_observer.

*   Yetki

    check_pwdchgdate_auth( c_actvt_change ).

*   Güncelleme öncesi not alınması gereken bilgiler

    DATA(lv_old) = get_pwd_chg_date( abap_true ).

*   Güncelleme

    enqueue_user( gv_bname ).

    UPDATE usr02
      SET pwdchgdate = @iv_pwdchgdate
      WHERE bname EQ @gv_bname.

    dequeue_user( gv_bname ).

*   Observer Design Pattern

    TRY.
        DATA(lt_observer) = zcl_bc_abap_class=>get_instance( zif_bc_pwdchgdate_observer=>c_clsname_me )->get_instanceable_subclasses( ).
      CATCH cx_root ##no_handler .
    ENDTRY.

    LOOP AT lt_observer ASSIGNING FIELD-SYMBOL(<ls_observer>).

      TRY.
          CREATE OBJECT lo_obj TYPE (<ls_observer>-clsname).
          lo_obs ?= lo_obj.

          lo_obs->pwdchgdate_changed_manually(
            iv_bname = gv_bname
            iv_old   = lv_old
            iv_new   = iv_pwdchgdate
          ).

        CATCH cx_root ##no_handler.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_pwd_chg_date_bulk.

    CLEAR:
      et_success,
      et_failure.

    LOOP AT it_date ASSIGNING FIELD-SYMBOL(<ls_date>).

      TRY.
          get_instance( <ls_date>-bname )->set_pwd_chg_date( <ls_date>-pwdchgdate ).
          APPEND VALUE #( bname = <ls_date>-bname ) TO et_success.
        CATCH cx_root INTO DATA(lo_cx_root).

          IF io_log IS NOT INITIAL.
            io_log->add_exception( lo_cx_root ).
          ENDIF.

          APPEND VALUE #(
            bname = <ls_date>-bname
            text  = lo_cx_root->get_text( )
          ) TO et_failure.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
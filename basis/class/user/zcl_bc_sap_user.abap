CLASS zcl_bc_sap_user DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_bname,
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

    TYPES full_name TYPE char70.

    CONSTANTS: c_actvt_change  TYPE activ_auth  VALUE '02',
               c_actvt_display TYPE activ_auth  VALUE '03',
               c_parid_sicil   TYPE usr05-parid VALUE 'ZSICIL',
               c_ustyp_dialog  TYPE usr02-ustyp VALUE 'A'.

    DATA gv_bname TYPE xubname READ-ONLY.

    CLASS-METHODS check_pwdchgdate_auth
      IMPORTING iv_actvt TYPE activ_auth
      RAISING   zcx_bc_authorization.

    CLASS-METHODS get_full_name_wo_error
      IMPORTING iv_bname       TYPE xubname
      RETURNING VALUE(rv_name) TYPE full_name.

    CLASS-METHODS get_instance
      IMPORTING iv_bname             TYPE xubname
                iv_tolerate_inactive TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ro_obj)        TYPE REF TO zcl_bc_sap_user
      RAISING   zcx_bc_user_master_data.

    CLASS-METHODS get_instance_via_email
      IMPORTING iv_email             TYPE ad_smtpadr
                iv_tolerate_inactive TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ro_obj)        TYPE REF TO zcl_bc_sap_user
      RAISING   zcx_bc_table_content
                zcx_bc_user_master_data.

    CLASS-METHODS get_instance_via_param_val
      IMPORTING iv_parid             TYPE usr05-parid
                iv_parva             TYPE usr05-parva
                iv_tolerate_inactive TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ro_obj)        TYPE REF TO zcl_bc_sap_user
      RAISING   zcx_bc_table_content
                zcx_bc_user_master_data.

    CLASS-METHODS set_pwd_chg_date_bulk
      IMPORTING it_date    TYPE tt_bulk_pwdchgdate
                io_log     TYPE REF TO zcl_bc_applog_facade OPTIONAL
      EXPORTING et_success TYPE tt_bname
                et_failure TYPE tt_bname_text.

    METHODS can_debug_change RETURNING VALUE(rv_can) TYPE abap_bool.

    METHODS can_develop      RETURNING VALUE(rv_can) TYPE abap_bool.

    METHODS disable
      IMPORTING iv_lock             TYPE abap_bool DEFAULT abap_true
                iv_restrict         TYPE abap_bool DEFAULT abap_true
                iv_restriction_date TYPE dats      DEFAULT sy-datum
                iv_commit           TYPE abap_bool DEFAULT abap_true
      EXPORTING ev_success          TYPE abap_bool
                et_return           TYPE bapiret2_tab.

    METHODS get_email     RETURNING VALUE(rv_email) TYPE ad_smtpadr.

    METHODS get_full_name RETURNING VALUE(rv_name)  TYPE full_name.

    METHODS get_language  RETURNING VALUE(rv_langu) TYPE sylangu.

    METHODS get_mobile_number
      IMPORTING iv_tolerate_missing_number TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_mobile)           TYPE ad_tlnmbr
      RAISING   zcx_bc_user_master_data.

    METHODS get_pwd_chg_date
      IMPORTING iv_force_fresh TYPE abap_bool
      RETURNING VALUE(rv_date) TYPE xubcdat.

    METHODS get_uname_text RETURNING VALUE(rv_utext)  TYPE ad_namtext.

    METHODS is_dialog_user RETURNING VALUE(rv_dialog) TYPE abap_bool.

    METHODS set_pwd_chg_date
      IMPORTING iv_pwdchgdate TYPE usr02-pwdchgdate
      RAISING   zcx_bc_authorization
                zcx_bc_lock.

    METHODS get_manager
      RETURNING VALUE(result) TYPE REF TO zcl_bc_sap_user
      RAISING   zcx_hr_manager_determination.

    METHODS get_unit_manager
      RETURNING VALUE(result) TYPE REF TO zcl_bc_sap_user
      RAISING   zcx_hr_manager_determination.

    METHODS get_work_center
      RETURNING VALUE(result) TYPE object_person_assignment
      RAISING   zcx_bc_user_work_center.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_lazy_flag,
             email       TYPE abap_bool,
             full_name   TYPE abap_bool,
             mobile      TYPE abap_bool,
             pwdchgdate  TYPE abap_bool,
             uname_text  TYPE abap_bool,
             work_center TYPE abap_bool,
             language    TYPE abap_bool,
           END OF t_lazy_flag,

           BEGIN OF t_lazy_var,
             email       TYPE ad_smtpadr,
             full_name   TYPE full_name,
             mobile      TYPE ad_tlnmbr,
             pwdchgdate  TYPE xubcdat,
             uname_text  TYPE name_text,
             work_center TYPE object_person_assignment,
             language    TYPE sylangu,
           END OF t_lazy_var.

    TYPES: BEGIN OF t_multiton,
             bname TYPE xubname,
             obj   TYPE REF TO zcl_bc_sap_user,
           END OF t_multiton,

           tt_multiton TYPE HASHED TABLE OF t_multiton
                         WITH UNIQUE KEY primary_key COMPONENTS bname.

    TYPES: BEGIN OF t_user_email,
             bname TYPE usr21-bname,
             email TYPE adr6-smtp_addr,
           END OF t_user_email,

           tt_user_email TYPE STANDARD TABLE OF t_user_email WITH EMPTY KEY.

    TYPES: BEGIN OF t_clazy_flag,
             user_email TYPE abap_bool,
           END OF t_clazy_flag,

           BEGIN OF t_clazy_var,
             user_email TYPE tt_user_email,
           END OF t_clazy_var.

    TYPES tt_rcrid TYPE STANDARD TABLE OF rcrid WITH KEY table_line.

    CONSTANTS: BEGIN OF c_table,
                 adr6 TYPE tabname VALUE 'ADR6',
               END OF c_table.

    CLASS-DATA gs_clazy_flag TYPE t_clazy_flag.
    CLASS-DATA gs_clazy_var  TYPE t_clazy_var.
    CLASS-DATA gt_multiton   TYPE tt_multiton.

    DATA gs_lazy_flag    TYPE t_lazy_flag.
    DATA gs_lazy_var     TYPE t_lazy_var.
    DATA gs_usr02        TYPE usr02.
    DATA go_manager      TYPE REF TO zcl_bc_sap_user.
    DATA go_unit_manager TYPE REF TO zcl_bc_sap_user.

    CLASS-METHODS dequeue_user
      IMPORTING iv_bname TYPE xubname.

    CLASS-METHODS enqueue_user
      IMPORTING iv_bname TYPE xubname
      RAISING   zcx_bc_lock.

    CLASS-METHODS read_all_user_emails_lazy.

    METHODS evaluate_disable_return
      IMPORTING it_return_from_bapi TYPE bapiret2_tab
                iv_commit           TYPE abap_bool
      CHANGING  ct_return_export    TYPE bapiret2_tab
                cv_success          TYPE abap_bool.
ENDCLASS.


CLASS zcl_bc_sap_user IMPLEMENTATION.
  METHOD can_debug_change.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                    FOR USER gv_bname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  FIELD 'DEBUG'
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.

    rv_can = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD can_develop.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                    FOR USER gv_bname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  DUMMY
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.

    rv_can = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD check_pwdchgdate_auth.
    AUTHORITY-CHECK OBJECT 'ZBCAOPDC' ID 'ACTVT' FIELD iv_actvt.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_bc_authorization( textid = zcx_bc_authorization=>no_auth ).
    ENDIF.
  ENDMETHOD.

  METHOD dequeue_user.
    CALL FUNCTION 'DEQUEUE_E_USR04'
      EXPORTING bname = iv_bname.
  ENDMETHOD.

  METHOD disable.
    CLEAR: ev_success,
           et_return.

    AUTHORITY-CHECK OBJECT 'S_USER_GRP' ID 'ACTVT' FIELD '05' ##AUTH_FLD_MISSING.
    IF sy-subrc <> 0.
      APPEND VALUE #( type    = zcl_bc_applog_facade=>c_msgty_e
                      message = TEXT-634 )
             TO et_return.
      RETURN.
    ENDIF.

    ev_success = abap_true.

    IF iv_restrict = abap_true.
      DATA(lt_change_return) = VALUE bapiret2_tab( ).
      DATA(ls_logon)         = VALUE bapilogond( gltgb = iv_restriction_date ).
      DATA(ls_logon_x)       = VALUE bapilogonx( gltgb = abap_true ).

      CALL FUNCTION 'BAPI_USER_CHANGE'
        EXPORTING username   = gv_bname
                  logondata  = ls_logon
                  logondatax = ls_logon_x
        TABLES    return     = lt_change_return.

      evaluate_disable_return( EXPORTING it_return_from_bapi = lt_change_return
                                         iv_commit           = iv_commit
                               CHANGING  ct_return_export    = et_return
                                         cv_success          = ev_success ).
      IF ev_success = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF iv_lock = abap_true.
      DATA(lt_lock_return) = VALUE bapiret2_tab( ).

      CALL FUNCTION 'BAPI_USER_LOCK'
        EXPORTING username = gv_bname
        TABLES    return   = lt_lock_return.

      evaluate_disable_return( EXPORTING it_return_from_bapi = lt_lock_return
                                         iv_commit           = iv_commit
                               CHANGING  ct_return_export    = et_return
                                         cv_success          = ev_success ).
      IF ev_success = abap_false.
        RETURN.
      ENDIF.

    ENDIF.

    IF iv_commit = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING wait = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD enqueue_user.
    TRY.
        CALL FUNCTION 'ENQUEUE_E_USR04'
          EXPORTING  bname          = iv_bname
          EXCEPTIONS foreign_lock   = 1
                     system_failure = 2
                     OTHERS         = 3
          ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'ENQUEUE_E_USR04' ).

      CATCH zcx_bc_function_subrc INTO DATA(lo_cx_fsr).
        RAISE EXCEPTION NEW zcx_bc_lock( textid   = zcx_bc_lock=>locked_by_user
                                         previous = lo_cx_fsr
                                         bname    = CONV #( sy-msgv1 ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD evaluate_disable_return.
    LOOP AT it_return_from_bapi TRANSPORTING NO FIELDS WHERE type IN zcl_bc_applog_facade=>get_crit_msgty_range( ).
      IF iv_commit = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      cv_success       = abap_false.
      ct_return_export = it_return_from_bapi.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_email.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2,
          lt_smtp   TYPE STANDARD TABLE OF bapiadsmtp.

    IF gs_lazy_flag-email IS INITIAL.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING username      = gv_bname
                  cache_results = abap_false
        TABLES    return        = lt_return
                  addsmtp       = lt_smtp.

      SORT lt_smtp BY std_no DESCENDING.

      LOOP AT lt_smtp ASSIGNING FIELD-SYMBOL(<ls_smtp>)
           WHERE    ( valid_from IS INITIAL )
                 OR (     valid_from <= sy-datum
                      AND valid_to   >= sy-datum ).

        gs_lazy_var-email = zcl_bc_mail_facade=>cleanse_email_address( <ls_smtp>-e_mail ).
        EXIT.
      ENDLOOP.

      gs_lazy_flag-email = abap_true.
    ENDIF.

    rv_email = gs_lazy_var-email.
  ENDMETHOD.

  METHOD get_full_name.
    IF gs_lazy_flag-full_name = abap_false.

      SELECT SINGLE name_first && @space && name_last
             INTO @gs_lazy_var-full_name
             FROM adrp
             WHERE persnumber = ( SELECT persnumber FROM usr21 WHERE bname = @gv_bname )
        ##WARN_OK.                                      "#EC CI_NOORDER

      gs_lazy_flag-full_name = abap_true.
    ENDIF.

    rv_name = gs_lazy_var-full_name.
  ENDMETHOD.

  METHOD get_language.
    IF gs_lazy_flag-language = abap_false.

      SELECT SINGLE langu INTO @gs_lazy_var-language
             FROM adrp
             WHERE persnumber = ( SELECT persnumber FROM usr21 WHERE bname = @gv_bname )
        ##WARN_OK.                                      "#EC CI_NOORDER

      gs_lazy_flag-language = abap_true.
    ENDIF.

    rv_langu = gs_lazy_var-language.
  ENDMETHOD.

  METHOD get_full_name_wo_error.
    TRY.
        rv_name = get_instance( iv_bname )->get_full_name( ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN gt_multiton[ KEY primary_key
                        COMPONENTS bname = iv_bname ]
           TO FIELD-SYMBOL(<ls_multiton>).

    IF sy-subrc <> 0.
      DATA(ls_multiton) = VALUE t_multiton( bname = iv_bname ).

      SELECT SINGLE * FROM usr02
             WHERE bname = @ls_multiton-bname
             INTO @DATA(ls_usr02).

      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_bc_user_master_data( textid = zcx_bc_user_master_data=>user_unknown
                                                     uname  = ls_multiton-bname ).
      ENDIF.

      IF     iv_tolerate_inactive = abap_false
         AND (    ls_usr02-uflag IS NOT INITIAL
               OR ( ls_usr02-gltgv IS NOT INITIAL AND ls_usr02-gltgv > sy-datum )
               OR ( ls_usr02-gltgb IS NOT INITIAL AND ls_usr02-gltgb < sy-datum ) ).

        RAISE EXCEPTION NEW zcx_bc_user_master_data( textid = zcx_bc_user_master_data=>user_inactive
                                                     uname  = ls_multiton-bname ).
      ENDIF.

      ls_multiton-obj = NEW #( ).
      ls_multiton-obj->gv_bname = ls_multiton-bname.
      ls_multiton-obj->gs_usr02 = ls_usr02.

      INSERT ls_multiton INTO TABLE gt_multiton ASSIGNING <ls_multiton>.
    ENDIF.

    ro_obj = <ls_multiton>-obj.
  ENDMETHOD.

  METHOD get_instance_via_email.
    read_all_user_emails_lazy( ).

    DATA(alt_email_addresses) = zcl_bc_mail_facade=>get_alt_domain_email_addresses( iv_email ).

    LOOP AT alt_email_addresses REFERENCE INTO DATA(alt_email_adr).
      DATA(cleansed_email) = zcl_bc_mail_facade=>cleanse_email_address( alt_email_adr->* ).
      DATA(users)          = VALUE rke_userid( ).

      LOOP AT gs_clazy_var-user_email ASSIGNING FIELD-SYMBOL(<user_email>).
        CHECK     zcl_bc_text_toolkit=>are_texts_same_ignoring_case( iv_text1 = cleansed_email
                                                                     iv_text2 = <user_email>-email )
              AND ( NOT line_exists( users[ table_line = <user_email>-bname ] ) ).

        INSERT <user_email>-bname INTO TABLE users.
      ENDLOOP.

      CHECK lines( users ) = 1.

      ro_obj = get_instance( iv_bname             = users[ 1 ]
                             iv_tolerate_inactive = iv_tolerate_inactive ).

      RETURN.
    ENDLOOP.

    RAISE EXCEPTION NEW zcx_bc_table_content( textid   = zcx_bc_table_content=>no_suitable_entry_found
                                              objectid = CONV #( iv_email )
                                              tabname  = c_table-adr6 ).
  ENDMETHOD.

  METHOD get_instance_via_param_val.
    DATA(parva_rng) = COND zbctt_xuvalue_rng(
                        WHEN iv_parva IS INITIAL
                        THEN VALUE #( ( sign   = ycl_addict_toolkit=>sign-include
                                        option = ycl_addict_toolkit=>option-eq
                                        low    = iv_parva ) )
                        ELSE VALUE #( sign = ycl_addict_toolkit=>sign-include
                                      ( option = ycl_addict_toolkit=>option-eq
                                        low    = iv_parva )
                                      ( option = ycl_addict_toolkit=>option-cp
                                        low    = |*{ iv_parva }| )
                                      ( option = ycl_addict_toolkit=>option-cp
                                        low    = |*{ iv_parva }*| )
                                      ( option = ycl_addict_toolkit=>option-cp
                                        low    = |{ iv_parva }*| ) ) ).

    SELECT FROM usr05 "#EC CI_GENBUFF
           FIELDS bname
           WHERE parid  = @iv_parid
             AND parva IN @parva_rng
           INTO TABLE @DATA(bnames).

    CASE lines( bnames ).
      WHEN 0.
        RAISE EXCEPTION NEW zcx_bc_user_master_data( textid = zcx_bc_user_master_data=>no_user_for_param_val
                                                     parid  = iv_parid
                                                     parva  = iv_parva ).

      WHEN 1.
        ro_obj = get_instance( iv_bname             = bnames[ 1 ]-bname
                               iv_tolerate_inactive = iv_tolerate_inactive ).

      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_bc_user_master_data( textid = zcx_bc_user_master_data=>multi_user_for_param_val
                                                     parid  = iv_parid
                                                     parva  = iv_parva ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_mobile_number.
    IF gs_lazy_flag-mobile = abap_false.

      SELECT SINGLE adr2~tel_number
             FROM usr21
                  INNER JOIN adr2
                    ON  adr2~addrnumber = usr21~addrnumber
                    AND adr2~persnumber = usr21~persnumber
             WHERE usr21~bname      = @gv_bname
               AND adr2~date_from  <= @sy-datum
               AND adr2~tel_number <> @space
             INTO @gs_lazy_var-mobile.
      "#EC CI_NOORDER

      gs_lazy_flag-mobile = abap_true.
    ENDIF.

    IF     gs_lazy_var-mobile         IS INITIAL
       AND iv_tolerate_missing_number  = abap_false.

      RAISE EXCEPTION NEW zcx_bc_user_master_data( textid = zcx_bc_user_master_data=>mobile_missing
                                                   uname  = gv_bname ).
    ENDIF.

    rv_mobile = gs_lazy_var-mobile.
  ENDMETHOD.

  METHOD get_pwd_chg_date.
    gs_lazy_flag-pwdchgdate = SWITCH #( iv_force_fresh
                                        WHEN abap_true
                                        THEN abap_false ).

    IF gs_lazy_flag-pwdchgdate = abap_false.

      SELECT SINGLE pwdchgdate FROM usr02
             WHERE bname = @gv_bname
             INTO @gs_lazy_var-pwdchgdate.

      gs_lazy_flag-pwdchgdate = abap_true.
    ENDIF.

    rv_date = gs_lazy_var-pwdchgdate.
  ENDMETHOD.

  METHOD get_uname_text.
    IF gs_lazy_flag-uname_text = abap_false.
      SELECT SINGLE name_textc INTO @gs_lazy_var-uname_text
             FROM user_addr
             WHERE bname = @gv_bname
        ##WARN_OK.                                      "#EC CI_NOORDER

      gs_lazy_flag-uname_text = abap_true.
    ENDIF.

    rv_utext = gs_lazy_var-uname_text.
  ENDMETHOD.

  METHOD is_dialog_user.
    rv_dialog = xsdbool( gs_usr02-ustyp = c_ustyp_dialog ).
  ENDMETHOD.

  METHOD read_all_user_emails_lazy.
    CHECK gs_clazy_flag-user_email = abap_false.

    SELECT DISTINCT usr21~bname,
                    adr6~smtp_addr AS email
           FROM adr6
                INNER JOIN usr21
                  ON  usr21~persnumber = adr6~persnumber
                  AND usr21~addrnumber = adr6~addrnumber
           INTO CORRESPONDING FIELDS OF TABLE @gs_clazy_var-user_email.

    LOOP AT gs_clazy_var-user_email ASSIGNING FIELD-SYMBOL(<ls_user_email>).
      <ls_user_email>-email = zcl_bc_mail_facade=>cleanse_email_address( <ls_user_email>-email ).
    ENDLOOP.

    gs_clazy_flag-user_email = abap_true.
  ENDMETHOD.

  METHOD set_pwd_chg_date.
    DATA: lo_obj TYPE REF TO object,
          lo_obs TYPE REF TO zif_bc_pwdchgdate_observer.

    " Yetki
    check_pwdchgdate_auth( c_actvt_change ).

    " Güncelleme öncesi not alınması gereken bilgiler
    DATA(lv_old) = get_pwd_chg_date( abap_true ).

    " Güncelleme
    enqueue_user( gv_bname ).
    UPDATE usr02 SET pwdchgdate = @iv_pwdchgdate WHERE bname = @gv_bname.
    gs_usr02-pwdchgdate = iv_pwdchgdate.
    dequeue_user( gv_bname ).

    " Observer Design Pattern

    TRY.
        DATA(lt_observer) = zcl_bc_abap_class=>get_instance( zif_bc_pwdchgdate_observer=>c_clsname_me )->get_instanceable_subclasses( ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    LOOP AT lt_observer ASSIGNING FIELD-SYMBOL(<ls_observer>).

      TRY.
          CREATE OBJECT lo_obj TYPE (<ls_observer>-clsname).
          lo_obs ?= lo_obj.

          lo_obs->pwdchgdate_changed_manually( iv_bname = gv_bname
                                               iv_old   = lv_old
                                               iv_new   = iv_pwdchgdate ).

        CATCH cx_root ##NO_HANDLER.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_manager.
    IF go_manager IS INITIAL.
      DATA(employee_email) = get_email( ).

      IF employee_email IS INITIAL.
        RAISE EXCEPTION NEW zcx_hr_manager_determination( textid = zcx_hr_manager_determination=>empty_user_email
                                                          bname  = gv_bname ).
      ENDIF.

      DATA(org_chart) = zcl_hr_org_chart=>get_default_provider( ).
      go_manager = org_chart->get_manager_via_email( employee_email ).
    ENDIF.

    result = go_manager.
  ENDMETHOD.

  METHOD get_unit_manager.
    IF go_unit_manager IS INITIAL.
      DATA(employee_email) = get_email( ).

      IF employee_email IS INITIAL.
        RAISE EXCEPTION NEW zcx_hr_manager_determination( textid = zcx_hr_manager_determination=>empty_user_email
                                                          bname  = gv_bname ).
      ENDIF.

      DATA(org_chart) = zcl_hr_org_chart=>get_default_provider( ).
      go_unit_manager = org_chart->get_unit_manager_via_email( employee_email ).
    ENDIF.

    result = go_unit_manager.
  ENDMETHOD.

  METHOD get_work_center.
    IF gs_lazy_flag-work_center IS INITIAL.
      DO 1 TIMES.
        DATA(users) = VALUE pernr_us_tab( ).

        CALL FUNCTION 'HR_GET_EMPLOYEES_FROM_USER'
          EXPORTING user              = sy-uname
                    begda             = sy-datum
                    endda             = sy-datum
                    iv_with_authority = abap_true
          TABLES    ee_tab            = users.

        CHECK users IS NOT INITIAL.

        DATA(person_assignments) = VALUE tt_rcrid( ( objty = 'P'
                                                     objid = users[ 1 ]-pernr ) ).

        DATA(workcenters_of_person) = VALUE rplm_tt_person_assignment( ).

        CALL FUNCTION 'COI2_WORKCENTER_OF_PERSON'
          EXPORTING  begda                       = sy-datum
                     endda                       = sy-datum
                     in_plvar                    = '01'
          TABLES     in_object                   = person_assignments
                     out_object                  = workcenters_of_person
          EXCEPTIONS no_in_objects               = 1
                     invalid_object              = 2
                     invalid_hr_planning_variant = 3
                     other_error                 = 4
                     evaluation_path_not_found   = 5
                     OTHERS                      = 6.

        CHECK sy-subrc = 0 AND workcenters_of_person IS NOT INITIAL.
        gs_lazy_var-work_center = workcenters_of_person[ 1 ].
      ENDDO.

      gs_lazy_flag-work_center = abap_true.
    ENDIF.

    IF gs_lazy_var-work_center IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_user_work_center( textid = zcx_bc_user_work_center=>cant_determine_work_center
                                                   bname  = sy-uname ).
    ENDIF.

    result = gs_lazy_var-work_center.
  ENDMETHOD.

  METHOD set_pwd_chg_date_bulk.
    CLEAR: et_success,
           et_failure.

    LOOP AT it_date ASSIGNING FIELD-SYMBOL(<ls_date>).

      TRY.
          get_instance( <ls_date>-bname )->set_pwd_chg_date( <ls_date>-pwdchgdate ).
          APPEND VALUE #( bname = <ls_date>-bname ) TO et_success.
        CATCH cx_root INTO DATA(lo_cx_root).

          IF io_log IS NOT INITIAL.
            io_log->add_exception( lo_cx_root ).
          ENDIF.

          APPEND VALUE #( bname = <ls_date>-bname
                          text  = lo_cx_root->get_text( ) )
                 TO et_failure.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
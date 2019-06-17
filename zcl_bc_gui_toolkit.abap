CLASS zcl_bc_gui_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_trkorr,
        trkorr TYPE e070-trkorr,
      END OF t_trkorr .
    TYPES:
      tt_trkorr TYPE STANDARD TABLE OF t_trkorr WITH DEFAULT KEY .
    TYPES:
      BEGIN OF t_dynpro_attr,
        dylang TYPE d020s-spra,
        dyname TYPE d020s-prog,
        dynumb TYPE d020s-dnum,
        head   TYPE d020s,
        pltab  TYPE dyn_flowlist,
        ftab   TYPE tdt_d021s,
      END OF t_dynpro_attr .
    TYPES:
      tt_dynpro_attr TYPE HASHED TABLE OF t_dynpro_attr
                                   WITH UNIQUE KEY primary_key COMPONENTS dylang
                                                                          dyname
                                                                          dynumb .

    CONSTANTS c_display_mode_popup TYPE zbcd_log_disp VALUE 'P' ##NO_TEXT.
    CONSTANTS c_display_mode_popup_light TYPE zbcd_log_disp VALUE 'L' ##NO_TEXT.
    CONSTANTS c_display_mode_single TYPE zbcd_log_disp VALUE 'S' ##NO_TEXT.
    CONSTANTS c_display_mode_standard TYPE zbcd_log_disp VALUE '' ##NO_TEXT.
    CONSTANTS c_icon_gray TYPE icon_d VALUE '@BZ@' ##NO_TEXT.
    CONSTANTS c_icon_green TYPE icon_d VALUE '@5B@' ##NO_TEXT.
    CONSTANTS c_icon_red TYPE icon_d VALUE '@5C@' ##NO_TEXT.
    CONSTANTS c_icon_yellow TYPE icon_d VALUE '@5D@' ##NO_TEXT.
    CLASS-DATA gt_dynpro_attr TYPE tt_dynpro_attr .

    CLASS-METHODS appl_log_display
      IMPORTING
        !iv_object                    TYPE balhdr-object
        !iv_subobject                 TYPE balhdr-subobject
        !iv_external_number           TYPE balhdr-extnumber OPTIONAL
        !iv_suppress_selection_dialog TYPE char1 DEFAULT 'X'
        !iv_date_from                 TYPE datum OPTIONAL .

    CLASS-METHODS are_you_sure
      IMPORTING
        !iv_text TYPE clike
      RAISING
        zcx_bc_user_input .

    CLASS-METHODS are_you_sure_t100
      IMPORTING
        !iv_msgid TYPE symsgid
        !iv_msgno TYPE symsgno
        !iv_msgv1 TYPE any OPTIONAL
        !iv_msgv2 TYPE any OPTIONAL
        !iv_msgv3 TYPE any OPTIONAL
        !iv_msgv4 TYPE any OPTIONAL
      RAISING
        zcx_bc_user_input.

    CLASS-METHODS call_slg1_for_intid
      IMPORTING
        !iv_intid     TYPE zbcd_intid
        !iv_base_date TYPE dats DEFAULT sy-datum
        !iv_base_time TYPE tims DEFAULT sy-uzeit
        !iv_hour_span TYPE thour DEFAULT '1.00'
        !iv_skip_sel  TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_bc_table_content
        zcx_bc_function_subrc .
    CLASS-METHODS display_applog_msg
      IMPORTING
        !io_applog             TYPE REF TO zcl_bc_applog_facade
        !iv_display_mode       TYPE zbcd_log_disp DEFAULT c_display_mode_popup_light
        !iv_grid               TYPE balusegrid DEFAULT abap_true
        !iv_tree_ontop         TYPE baltrontop OPTIONAL
        !iv_showall            TYPE balshowall DEFAULT abap_true
      RETURNING
        VALUE(rv_exit_command) TYPE bal_s_excm
      RAISING
        zcx_bc_log_disp .
    CLASS-METHODS display_cx_msg_i
      IMPORTING
        !io_cx TYPE REF TO cx_root .
    CLASS-METHODS display_cx_msg_deepest
      IMPORTING
        !io_cx TYPE REF TO cx_root .
    CLASS-METHODS display_cx_msg_matryoshka
      IMPORTING
        !io_cx TYPE REF TO cx_root .
    CLASS-METHODS display_cx_msg_popup
      IMPORTING
        !io_cx   TYPE REF TO cx_root
        !iv_text TYPE clike OPTIONAL .
    CLASS-METHODS display_transport_requests
      IMPORTING
        !it_trkorr TYPE tt_trkorr .
    CLASS-METHODS get_msgty_icon
      IMPORTING
        !iv_msgty      TYPE symsgty
      RETURNING
        VALUE(rv_icon) TYPE icon_d .
    CLASS-METHODS get_trkorr_list_from_user
      IMPORTING
        !iv_line         TYPE txw_note-line OPTIONAL
      RETURNING
        VALUE(rt_trkorr) TYPE zcl_bc_transport_request=>tt_trkorr .
*-- mehmet sertkaya
    CLASS-METHODS sapgui_mess
      IMPORTING
        !iv_top  TYPE int4
        !iv_akt  TYPE int4
        !iv_len  TYPE int4
        !iv_text TYPE text255 .
    CLASS-METHODS popup_column_list
      IMPORTING
        !iv_repid TYPE sy-repid
        !iv_tname TYPE text100 .
    CLASS-METHODS get_file
      RETURNING
        VALUE(rv_filename) TYPE string .
    CLASS-METHODS upload_excel_file
      IMPORTING
        !iv_repid       TYPE sy-repid
        !iv_tname       TYPE text100
        !iv_filename    TYPE string
        !iv_line        TYPE int4
      CHANGING
        VALUE(rt_table) TYPE REF TO data .
    CLASS-METHODS upload_excel_file_data
      IMPORTING
        !iv_repid          TYPE sy-repid OPTIONAL
        !iv_structure_name TYPE dd02l-tabname OPTIONAL
        !iv_filename       TYPE string OPTIONAL
        !iv_line           TYPE int4 OPTIONAL
      CHANGING
        !ct_table          TYPE ANY TABLE OPTIONAL
      RETURNING
        VALUE(rt_return)   TYPE bapiret2_t
      EXCEPTIONS
        error_structure_read
        conversion_error .
    CLASS-METHODS convert_any_input_to_data
      IMPORTING
        VALUE(iv_source) TYPE any
      CHANGING
        VALUE(cv_target) TYPE any
      RETURNING
        VALUE(rv_subrc)  TYPE sy-subrc .
    CLASS-METHODS perpare_number
      CHANGING
        VALUE(cv_parameter)   TYPE any
        VALUE(cv_decimal_pos) TYPE int4 .
    CLASS-METHODS popup_column_list_str
      IMPORTING
        !iv_str TYPE dd02l-tabname .
    CLASS-METHODS get_screen_attributes
      IMPORTING
        !iv_prog         TYPE d020s-prog
        !iv_dnum         TYPE d020s-dnum
        !iv_lang         TYPE d020s-spra OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE t_dynpro_attr .
    CLASS-METHODS download_excel_template
      IMPORTING
        !iv_str TYPE dd02l-tabname
        !it_col TYPE zbctt_column_excel OPTIONAL .
    CLASS-METHODS download_file
      IMPORTING
        iv_filename TYPE string OPTIONAL
        ir_filedata TYPE REF TO data .
    CLASS-METHODS download_file_with_dialog
      IMPORTING
        !ir_filedata      TYPE REF TO data
        !iv_def_extension TYPE string DEFAULT '.TXT'
        !iv_def_filename  TYPE string OPTIONAL
        !iv_title         TYPE string DEFAULT 'Backup'   ##NO_TEXT
        !iv_Separator     type abap_bool default abap_False
      RAISING
        zcx_bc_symsg
        zcx_bc_user_input .
    CLASS-METHODS write_cx_msg_matryoshka
      IMPORTING
        !io_cx TYPE REF TO cx_root .
    CLASS-METHODS convert_any_input_to_output
      IMPORTING
        !iv_source      TYPE any
        !iv_convexit    TYPE convexit OPTIONAL
        !iv_datatype    TYPE dd03p-datatype OPTIONAL
        !iv_refvalue    TYPE any OPTIONAL
      CHANGING
        !cv_target      TYPE any
      RETURNING
        VALUE(rv_subrc) TYPE sy-subrc .
    CLASS-METHODS get_definition_ddic
      IMPORTING
        !iv_structure_name TYPE dd02l-tabname
      RETURNING
        VALUE(rt_def)      TYPE tcacs_dd03p .
    CLASS-METHODS deep_cx_root
      IMPORTING
        !iv_count TYPE int4 DEFAULT 100
      CHANGING
        !cx_root  TYPE REF TO cx_root .
    CLASS-METHODS gui_message
      IMPORTING
        !iv_index   TYPE i
        !iv_level   TYPE i
        !iv_message TYPE text100 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_fsd,
        filename    TYPE string,
        path        TYPE string,
        fullpath    TYPE string,
        user_action TYPE i,
      END OF t_fsd .
    TYPES:
      tt_txw_note TYPE STANDARD TABLE OF txw_note WITH DEFAULT KEY .

    CONSTANTS:
      c_answer_yes(1) VALUE 'J' ##NO_TEXT.
    CONSTANTS c_option_eq TYPE ddoption VALUE 'EQ' ##NO_TEXT.
    CONSTANTS c_sign_i TYPE ddsign VALUE 'I' ##NO_TEXT.
ENDCLASS.



CLASS zcl_bc_gui_toolkit IMPLEMENTATION.


  METHOD appl_log_display.

    DATA : lv_date_from TYPE balhdr-aldate.

    IF iv_date_from NE '00000000'.
      lv_date_from  = iv_date_from.
    ELSE.
      lv_date_from  = sy-datum - 365.                    "#EC NUMBER_OK
    ENDIF.

    CALL FUNCTION 'APPL_LOG_DISPLAY'
      EXPORTING
        object                    = iv_object
        subobject                 = iv_subobject
        external_number           = iv_external_number
        suppress_selection_dialog = iv_suppress_selection_dialog
        date_from                 = lv_date_from
*     IMPORTING
*       NUMBER_OF_PROTOCOLS       =
      EXCEPTIONS
        no_authority              = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD are_you_sure.

    DATA lv_answer(1).

    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
        textline1 = iv_text
        titel     = TEXT-608
      IMPORTING
        answer    = lv_answer.

    IF lv_answer NE c_answer_yes.
      RAISE EXCEPTION TYPE zcx_bc_user_input
        EXPORTING
          textid = zcx_bc_user_input=>user_cancelled.
    ENDIF.

  ENDMETHOD.

  METHOD are_you_sure_t100.

    DATA lv_question TYPE string.

    MESSAGE
      ID     iv_msgid
      TYPE   zcl_bc_applog_facade=>c_msgty_s
      NUMBER iv_msgno
      WITH   iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
      INTO   lv_question.

    are_you_sure( lv_question ).

  ENDMETHOD.


  METHOD call_slg1_for_intid.

    DATA: lv_bdate TYPE p2006-begda,
          lv_btime TYPE p2001-beguz,
          lv_edate TYPE p2006-begda,
          lv_etime TYPE p2001-beguz,
          lv_spanm TYPE p2012-anzhl.

    DATA(ls_def) = zcl_bc_int_master=>get_integration_def( iv_intid ).

    lv_spanm = iv_hour_span * -1.

    CALL FUNCTION 'CATT_ADD_TO_TIME'
      EXPORTING
        idate = iv_base_date
        itime = iv_base_time
        stdaz = lv_spanm
      IMPORTING
        edate = lv_bdate
        etime = lv_btime.

    CALL FUNCTION 'CATT_ADD_TO_TIME'
      EXPORTING
        idate = iv_base_date
        itime = iv_base_time
        stdaz = iv_hour_span
      IMPORTING
        edate = lv_edate
        etime = lv_etime.

    CALL FUNCTION 'APPL_LOG_DISPLAY'
      EXPORTING
        object                    = ls_def-balobj
        subobject                 = ls_def-balsub
        date_from                 = lv_bdate
        time_from                 = lv_btime
        date_to                   = lv_edate
        time_to                   = lv_etime
        suppress_selection_dialog = iv_skip_sel
      EXCEPTIONS
        no_authority              = 1
        OTHERS                    = 2 ##FM_SUBRC_OK.

    zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'APPL_LOG_DISPLAY' ).

  ENDMETHOD.


  METHOD convert_any_input_to_data.
    DATA: lv_field_type TYPE c.
    DATA: lv_decimals TYPE i.
    DATA: lv_decimals_target TYPE i.
    FIELD-SYMBOLS: <fs_type_x> TYPE x.

    DATA lv_darlv_number(12)     TYPE c VALUE '1234567890 '.

    DESCRIBE FIELD cv_target TYPE lv_field_type.

    CLEAR rv_subrc.

    CASE lv_field_type.
      WHEN 'C'.
        cv_target = iv_source.

      WHEN 'D'.
* Bitte beachten:
* Die Funktion CONVERT_DATE_TO_INTERNAL erwartet das Datum analog den
* Einstellungen in den Festwerten des Benutzers. Ist dort das Format
* TT.MM.JJJJ eingestellt, dann muss der Routine das Datum in der
* Form 31.12.1999 oder 31121999 übergeben werden.
* Ist bei den Benutzerfestwerten das amerikanische Format JJJJ.MM.TT
* eingestellt, dann erwartet die Funktion das Datum in der Form
* 1999.12.31 oder 19991231
* Einige Excel Datumsformate haben die Eigenart, zusätzliche Leerzeichen
* und Punkte in das Datum einzufügen. Deshalb werden Punkte in der
* DO Schleife entfernt und anschliessend noch die Leerzeichen durch
* den condense entfernt.
        DO 3 TIMES.
          REPLACE '.' WITH ' ' INTO iv_source.
        ENDDO.
        CONDENSE iv_source NO-GAPS.

        IF  NOT iv_source IS INITIAL   "Space
        AND iv_source <> '00.00.0000'
        AND iv_source <> '00000000'.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = iv_source
            IMPORTING
              date_internal = cv_target
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
          IF sy-subrc <> 0.
            rv_subrc = 4.
          ENDIF.
        ENDIF.

      WHEN 'T'.
        IF NOT iv_source IS INITIAL AND iv_source <> '00:00:00'.
          CALL FUNCTION 'CONVERT_TIME_INPUT'
            EXPORTING
              input         = iv_source
            IMPORTING
              output        = cv_target
            EXCEPTIONS
              error_message = 1
              OTHERS        = 1.
          IF sy-subrc <> 0.
            rv_subrc = 4.
          ENDIF.
        ENDIF.
      WHEN 'X'.
        ASSIGN iv_source TO <fs_type_x> ##SUBRC_READ.
        IF sy-subrc <> 0.
          rv_subrc = 8.
        ENDIF.
        cv_target = <fs_type_x> .
      WHEN 'N'.
        DESCRIBE FIELD cv_target DECIMALS lv_decimals_target.
        perpare_number( CHANGING cv_parameter   = iv_source
                                 cv_decimal_pos = lv_decimals ).

        IF iv_source CN lv_darlv_number.
          rv_subrc = 8.
        ELSE.
          lv_decimals_target = lv_decimals_target - lv_decimals.
          cv_target = iv_source * ( 10 ** ( lv_decimals_target ) ) ##OPERATOR.
        ENDIF.
      WHEN 'I'.
        DESCRIBE FIELD cv_target DECIMALS lv_decimals_target.
        perpare_number( CHANGING cv_parameter   = iv_source
                                 cv_decimal_pos = lv_decimals ).
        IF iv_source CN lv_darlv_number.
          rv_subrc = 8.
        ELSE.
          lv_decimals_target = lv_decimals_target - lv_decimals.
          cv_target = iv_source * ( 10 ** ( lv_decimals_target ) ) ##OPERATOR.
        ENDIF.
      WHEN 'P'.
        DESCRIBE FIELD cv_target DECIMALS lv_decimals_target.
        DATA : lv_char(20),
               lv_float TYPE f.

        lv_char = iv_source.
        TRY.
            CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
              EXPORTING
                i_char         = lv_char
              IMPORTING
                e_float        = lv_float
*               e_packed       =
              EXCEPTIONS
                invalid_number = 1
                OTHERS         = 2 ##FM_SUBRC_OK.

            zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'OIU_ME_CHAR_TO_NUMBER' ).

            cv_target = lv_float.

          CATCH cx_root INTO DATA(lo_cx).
            RAISE EXCEPTION lo_cx.
        ENDTRY.


      WHEN 'F'.
        CALL FUNCTION 'CHAR_FLTP_CONVERSION'
          EXPORTING
            string = iv_source
          IMPORTING
            flstr  = cv_target
          EXCEPTIONS
            OTHERS = 1.
        IF sy-subrc <> 0.
          rv_subrc = 4.
        ENDIF.
      WHEN OTHERS.
        cv_target = iv_source.
    ENDCASE.

  ENDMETHOD.


  METHOD convert_any_input_to_output.

    DATA: lv_field_type TYPE c.
    DATA: lv_decimals_target TYPE i.
    DATA: lv_fmname TYPE char80.
    FIELD-SYMBOLS: <fs_type_x> TYPE x.

    IF iv_refvalue IS NOT INITIAL.
      IF iv_datatype EQ 'CURR'.
        WRITE iv_source TO cv_target CURRENCY iv_refvalue.
        CONDENSE cv_target.
        RETURN.
      ELSEIF iv_datatype EQ 'QUAN'.
        WRITE iv_source TO cv_target UNIT iv_refvalue.
        CONDENSE cv_target.
        RETURN.
      ENDIF.
    ENDIF.

    DESCRIBE FIELD iv_source TYPE lv_field_type.

    IF iv_convexit IS NOT INITIAL.
      CONCATENATE  'CONVERSION_EXIT_'
                 iv_convexit
                 '_OUTPUT' INTO lv_fmname.

      CALL FUNCTION lv_fmname
        EXPORTING
          input  = iv_source
        IMPORTING
          output = cv_target
        EXCEPTIONS
          OTHERS = 1 ##FM_SUBRC_OK.
      RETURN.
    ENDIF.
    CASE lv_field_type.
      WHEN 'C'.
        cv_target = iv_source.

      WHEN 'D'.
        WRITE iv_source TO cv_target DD/MM/YYYY.

      WHEN 'T'.
        WRITE iv_source TO cv_target ENVIRONMENT TIME FORMAT.
      WHEN 'X'.
        ASSIGN iv_source TO <fs_type_x> .
        cv_target = <fs_type_x> .
      WHEN 'N'.
        DATA : lv_int TYPE i.
        lv_int = iv_source.
        cv_target = lv_int.
        CONDENSE cv_target.
      WHEN 'I'.
        cv_target = iv_source.
        CONDENSE cv_target.
      WHEN 'P'.
        DESCRIBE FIELD iv_source DECIMALS lv_decimals_target.
        WRITE iv_source TO cv_target DECIMALS lv_decimals_target.
      WHEN 'F'.
        DATA : lv_decimalx TYPE p DECIMALS 3.

        CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
          EXPORTING
            if_float  = iv_source
          IMPORTING
            ef_packed = lv_decimalx
          EXCEPTIONS
            overflow  = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          rv_subrc = 4.
          RETURN.
        ENDIF.

        WRITE lv_decimalx TO cv_target DECIMALS 3.
        CONDENSE cv_target.

      WHEN OTHERS.
        cv_target = iv_source.
    ENDCASE.
  ENDMETHOD.


  METHOD deep_cx_root.
    TRY.
        DO iv_count TIMES.
          IF cx_root->previous IS NOT INITIAL.
            cx_root = cx_root->previous .
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD display_applog_msg.

    DATA: lt_msgf            TYPE esp1_message_tab_type,
          ls_display_profile TYPE bal_s_prof,
          lv_lineno          TYPE msgzeile.

*   Basit Pop Up mesaj
    IF iv_display_mode EQ c_display_mode_popup_light.

      LOOP AT io_applog->get_messages( ) ASSIGNING FIELD-SYMBOL(<ls_msgb>).
        ADD 1 TO lv_lineno.

        APPEND VALUE #( msgid  = <ls_msgb>-id
                        msgty  = <ls_msgb>-type
                        msgno  = <ls_msgb>-number
                        msgv1  = <ls_msgb>-message_v1
                        msgv2  = <ls_msgb>-message_v2
                        msgv3  = <ls_msgb>-message_v3
                        msgv4  = <ls_msgb>-message_v4
                        lineno = lv_lineno ) TO lt_msgf.
      ENDLOOP.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_msgf.

      IMPORT e_exit_command TO rv_exit_command FROM MEMORY ID 'E_EXIT_COMMAND'.

      RETURN.
    ENDIF.

*   Standart App.Log Popup
    CASE iv_display_mode.
      WHEN c_display_mode_single.
        CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.
      WHEN c_display_mode_popup.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.
      WHEN c_display_mode_standard.
        CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
          IMPORTING
            e_s_display_profile = ls_display_profile.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    ls_display_profile-use_grid = iv_grid.
    ls_display_profile-title = TEXT-001.
    ls_display_profile-tree_ontop = iv_tree_ontop.
    ls_display_profile-head_text = TEXT-001.
    ls_display_profile-head_size = 35.                   "#EC NUMBER_OK
    ls_display_profile-tree_size = 22.                   "#EC NUMBER_OK
    ls_display_profile-show_all = iv_showall.
    ls_display_profile-disvariant-report = sy-repid.

    ls_display_profile-lev1_fcat = VALUE #( ref_table = 'BAL_S_SHOW'

                                            ( ref_field = 'LOG_HANDLE'
                                              col_pos   = 0
                                              no_out    = abap_true )

                                            ( ref_field = 'EXTNUMBER'
                                              col_pos   = 1
                                              outputlen = 40 )

                                            ( ref_field  = 'ALDATE'
                                              col_pos    = 2
                                              colddictxt = 'R'
                                              is_treecol = abap_true )

                                            ( ref_field  = 'ALTIME'
                                              col_pos    = 3
                                              colddictxt = 'R'
                                              is_treecol = abap_true )

                                            ( ref_field  = 'ALUSER'
                                              col_pos    = 4
                                              colddictxt = 'R'
                                              is_treecol = abap_true ) ).

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_display_profile
      IMPORTING
        e_s_exit_command     = rv_exit_command
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_bc_log_disp
        EXPORTING
          textid    = zcx_bc_log_disp=>cant_disp
          previous  = zcx_bc_symsg=>get_instance( )
          object    = io_applog->gv_object
          subobject = io_applog->gv_subobject.

    ENDIF.

  ENDMETHOD. "show_messages


  METHOD display_cx_msg_deepest.

    IF io_cx->previous IS INITIAL.
      display_cx_msg_i( io_cx ).
    ELSE.
      display_cx_msg_deepest( io_cx->previous ).
    ENDIF.

  ENDMETHOD.


  METHOD display_cx_msg_i.
    CHECK io_cx IS NOT INITIAL.
    MESSAGE io_cx TYPE zcl_bc_applog_facade=>c_msgty_i.
  ENDMETHOD.


  METHOD display_cx_msg_matryoshka.

    CHECK io_cx IS NOT INITIAL.
    display_cx_msg_matryoshka( io_cx->previous ).
    display_cx_msg_i( io_cx ).

  ENDMETHOD.


  METHOD display_cx_msg_popup.

    TRY.

        DATA(lo_log) = NEW zcl_bc_applog_facade( iv_object    = 'ZBC'
                                                 iv_subobject = 'WF_SUB' ).

        IF iv_text IS SUPPLIED.
          lo_log->add_free_text( iv_text  = iv_text
                                 iv_msgty = zcl_bc_applog_facade=>c_msgty_e ).
        ENDIF.

        lo_log->add_exception( io_cx ).

        display_applog_msg( lo_log ).

      CATCH cx_root.

        display_cx_msg_matryoshka( io_cx ).

        IF iv_text IS SUPPLIED.
          MESSAGE iv_text TYPE zcl_bc_applog_facade=>c_msgty_i.
        ENDIF.

    ENDTRY.

  ENDMETHOD.


  METHOD display_transport_requests.

    DATA lt_req TYPE strhi_requests_wd.

*   Ön kontroller
    CHECK it_trkorr[] IS NOT INITIAL.

*   Request & Task'leri oku

    DATA(lt_trkorr) = CORRESPONDING tt_trkorr(
      zcl_bc_transport_request=>get_request_list(
        is_param = VALUE #(
                     s_trkorr = VALUE #( FOR ls_trkorr IN it_trkorr (
                                  option = c_option_eq
                                  sign   = c_sign_i
                                  low    = ls_trkorr-trkorr
                                ) )
                     p_srch_strkorr = abap_true
                   )
    ) ).


*   Fonksiyonu çağır
    LOOP AT lt_trkorr ASSIGNING FIELD-SYMBOL(<ls_trkorr>).

      TRY.
          DATA(lo_req) = zcl_bc_transport_request=>get_instance( <ls_trkorr>-trkorr ).
          DATA(ls_content) = lo_req->get_content( ).

          APPEND VALUE #(
            h              = CORRESPONDING #( lo_req->get_header( ) )
            objects        = CORRESPONDING #( ls_content-object )
            keys           = CORRESPONDING #( ls_content-key )
            objects_filled = abap_true
          ) TO lt_req.

        CATCH cx_root.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    CALL FUNCTION 'TRINT_DISPLAY_REQUESTS'
      EXPORTING
        it_requests        = lt_req
        iv_first_node_text = TEXT-013.

  ENDMETHOD.


  METHOD download_excel_template.

    DATA : lt_fcat     TYPE lvc_t_fcat,
           lt_fcat_t   TYPE lvc_t_fcat,
           lr_dy_table TYPE REF TO data,
           lr_dy_line  TYPE REF TO data.

    FIELD-SYMBOLS : <lt_column> TYPE STANDARD TABLE,
                    <ls_column> TYPE any.

    data(lt_col) = it_col.
    SORT lt_col BY fieldname.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       I_BUFFER_ACTIVE        =
        i_structure_name       = iv_str
*       I_CLIENT_NEVER_DISPLAY = 'X'
*       I_BYPASSING_BUFFER     =
*       I_INTERNAL_TABNAME     =
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                     DISPLAY LIKE 'E'.
    ELSE.

      LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
        APPEND INITIAL LINE TO lt_fcat_t ASSIGNING FIELD-SYMBOL(<ls_fcat_t>).
        <ls_fcat_t>-fieldname = <ls_fcat>-fieldname.
        <ls_fcat_t>-datatype  = 'CHAR'.
        <ls_fcat_t>-inttype   = 'C'  .
        <ls_fcat_t>-intlen    = 50   .
      ENDLOOP.

*     Dinamik tablo oluştur
      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog = lt_fcat_t[]
        IMPORTING
          ep_table        = lr_dy_table.
      ASSIGN lr_dy_table->* TO <lt_column>.

*     dinamik satır yapısını oluştur
      CREATE DATA lr_dy_line LIKE LINE OF <lt_column>.
      ASSIGN lr_dy_line->* TO <ls_column>.

      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

      APPEND INITIAL LINE TO <lt_column> ASSIGNING <ls_column>.
      LOOP AT lt_fcat ASSIGNING <ls_fcat>.
        ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_column> TO FIELD-SYMBOL(<lv_column>).
        READ TABLE lt_col WITH KEY fieldname = <ls_fcat>-fieldname
                          ASSIGNING FIELD-SYMBOL(<ls_col>) BINARY SEARCH.
        IF sy-subrc EQ 0.
          <lv_column> = <ls_col>-fieldtext.
        ELSE.
          <lv_column> = <ls_fcat>-scrtext_l.
        ENDIF.

      ENDLOOP.

      APPEND INITIAL LINE TO <lt_column> ASSIGNING <ls_column>.
      LOOP AT lt_fcat ASSIGNING <ls_fcat>.
        ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_column> TO <lv_column>.
        SHIFT <ls_fcat>-intlen LEFT DELETING LEADING '0'.
        <lv_column> = |{ <ls_fcat>-datatype }{ <ls_fcat>-intlen }|.
      ENDLOOP.

      zcl_bc_gui_toolkit=>download_file( REF #( <lt_column> ) ).
    ENDIF.

  ENDMETHOD.


  METHOD download_file.

    FIELD-SYMBOLS : <lt_filedata> TYPE STANDARD TABLE.

    ASSIGN ir_filedata->* TO <lt_filedata>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    data(lv_filename) = iv_Filename.

    IF lv_filename IS INITIAL.

      TRY.
          cl_gui_frontend_services=>directory_browse(
                              EXPORTING window_title = CONV #( TEXT-003 )
                              CHANGING selected_folder = lv_filename ).
        CATCH cx_root INTO DATA(lx_root).
          MESSAGE lx_root TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
      if lv_filename IS INITIAL.
        return.
      endif.

      CONCATENATE lv_filename '\template.xls' INTO lv_filename.
    ENDIF.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_filename
        write_field_separator   = 'X'
      TABLES
        data_tab                = <lt_filedata>
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22. "#EC NUMBER_OK

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                     DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE i000(l1) WITH lv_filename TEXT-004 ##MG_MISSING.
    ENDIF.


  ENDMETHOD.


  METHOD download_file_with_dialog.

    DATA ls_fsd TYPE t_fsd.

    FIELD-SYMBOLS <lt_filedata> TYPE STANDARD TABLE.

    cl_gui_frontend_services=>file_save_dialog( EXPORTING  window_title              = iv_title
                                                           default_extension         = iv_def_extension
                                                           default_file_name         = iv_def_filename
                                                CHANGING   filename                  = ls_fsd-filename
                                                           path                      = ls_fsd-path
                                                           fullpath                  = ls_fsd-fullpath
                                                           user_action               = ls_fsd-user_action
                                                EXCEPTIONS cntl_error                = 1
                                                           error_no_gui              = 2
                                                           not_supported_by_gui      = 3
                                                           invalid_default_file_name = 4
                                                   OTHERS                    = 5 ).
    IF sy-subrc <> 0.
      DATA(lo_cx_symsg) = zcx_bc_symsg=>get_instance( ).
      RAISE EXCEPTION lo_cx_symsg.
    ENDIF.

    IF NOT ( ls_fsd-user_action EQ cl_gui_frontend_services=>action_ok OR
             ls_fsd-user_action EQ cl_gui_frontend_services=>action_replace ).

      RAISE EXCEPTION TYPE zcx_bc_user_input
        EXPORTING
          textid = zcx_bc_user_input=>user_cancelled.

    ENDIF.

    ASSIGN ir_filedata->* TO <lt_filedata>.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = ls_fsd-fullpath
        WRITE_FIELD_SEPARATOR     = iv_separator
      CHANGING
        data_tab                  = <lt_filedata>
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
      ). "#EC NUMBER_OK

    IF sy-subrc <> 0.
      lo_cx_symsg = zcx_bc_symsg=>get_instance( ).
      RAISE EXCEPTION lo_cx_symsg.
    ENDIF.

  ENDMETHOD.


  METHOD get_definition_ddic.
    DATA : ls_dd02v_n    TYPE dd02v.
*--------------------------------------------------------------------*
* Tablonun detayını al
*--------------------------------------------------------------------*
    CALL FUNCTION 'DD_INT_TABL_GET'
      EXPORTING
        tabname        = iv_structure_name
      IMPORTING
        dd02v_n        = ls_dd02v_n
      TABLES
        dd03p_n        = rt_def
      EXCEPTIONS
        internal_error = 1.

    IF sy-subrc EQ 0 AND lines(  rt_def ) GT 0.

      CALL FUNCTION 'DD_TABL_EXPAND' ##FM_SUBRC_OK
        EXPORTING
          dd02v_wa          = ls_dd02v_n
          mode              = 46
          prid              = 0
        TABLES
          dd03p_tab         = rt_def
        EXCEPTIONS
          illegal_parameter = 1.

    ENDIF.

  ENDMETHOD.


  METHOD get_file.

    DATA: lv_rc       TYPE i,
          lt_filelist TYPE filetable,
          lv_desktop  TYPE string.

    CLEAR: lt_filelist[],
           lv_rc.

    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = lv_desktop
      EXCEPTIONS
        cntl_error        = 1
        ##SUBRC_OK.

    CALL METHOD cl_gui_cfw=>update_view.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
*       file_filter       = cl_gui_frontend_services=>filetype_text
*       default_filename  = '*.xls'
        initial_directory = lv_desktop
        multiselection    = space
      CHANGING
        file_table        = lt_filelist
        rc                = lv_rc.


    IF lines( lt_filelist ) IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_filelist ASSIGNING FIELD-SYMBOL(<ls_filelist>)
       WHERE filename NE space.
      rv_filename = <ls_filelist>.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_msgty_icon.

    rv_icon = SWITCH #( iv_msgty WHEN zcl_bc_applog_facade=>c_msgty_a
                                   OR zcl_bc_applog_facade=>c_msgty_e
                                   OR zcl_bc_applog_facade=>c_msgty_x THEN c_icon_red

                                 WHEN zcl_bc_applog_facade=>c_msgty_i
                                   OR zcl_bc_applog_facade=>c_msgty_s THEN c_icon_green

                                 WHEN zcl_bc_applog_facade=>c_msgty_w THEN c_icon_yellow

                                 ELSE c_icon_gray ).

  ENDMETHOD. "get_msgty_icon


  METHOD get_screen_attributes.
    DATA: lv_langu       TYPE d020s-spra,
          ls_dynpro_attr TYPE t_dynpro_attr.
    lv_langu = iv_lang.
    IF lv_langu IS INITIAL.
      lv_langu = sy-langu.
    ENDIF.
    ASSIGN gt_dynpro_attr[ KEY primary_key COMPONENTS
                           dylang = lv_langu
                           dyname = iv_prog
                           dynumb = iv_dnum ] TO FIELD-SYMBOL(<ls_dynpro_attr>).
    IF sy-subrc <> 0.
      CLEAR ls_dynpro_attr.
      ls_dynpro_attr-dylang               = lv_langu.
      ls_dynpro_attr-dyname               = iv_prog.
      ls_dynpro_attr-dynumb               = iv_dnum.
      CALL FUNCTION 'RS_IMPORT_DYNPRO'
        EXPORTING
          dylang               = lv_langu
          dyname               = iv_prog
          dynumb               = iv_dnum
        IMPORTING
          header               = ls_dynpro_attr-head
        TABLES
          ftab                 = ls_dynpro_attr-ftab
          pltab                = ls_dynpro_attr-pltab
        EXCEPTIONS
          button_error         = 1
          dylanguage_invalid   = 2
          dylanguage_not_inst  = 3
          dyname_invalid       = 4
          dynproload_not_found = 5
          dynpro_old           = 6
          dynumb_invalid       = 7
          ftab_invalid         = 8
          gen_error            = 9
          gen_ok               = 10
          header_invalid       = 11
          internal_error       = 12
          no_dynpro            = 13
          no_ftab_row          = 14
          no_memory            = 15
          no_processlogic      = 16
          pltab_invalid        = 17
          request_invalid      = 18
          OTHERS               = 19.
      IF sy-subrc <> 0.
*   Implement suitable error handling here
      ENDIF.
      INSERT ls_dynpro_attr INTO TABLE gt_dynpro_attr ASSIGNING <ls_dynpro_attr>.
    ENDIF.
    rs_return = <ls_dynpro_attr>.
  ENDMETHOD.


  METHOD get_trkorr_list_from_user.

    DATA(lv_intro) = COND text72(
        WHEN iv_line IS NOT INITIAL
        THEN iv_line
        ELSE TEXT-052
    ).

    DATA(lt_note) = VALUE tt_txw_note(
        ( line = lv_intro  )
        ( line = space )
    ).

    CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
      TABLES
        t_txwnote = lt_note.

    LOOP AT lt_note
        INTO DATA(ls_line)
        WHERE (
            line IS NOT INITIAL AND
            line NE lv_intro
        ).

      CONDENSE ls_line-line.
      APPEND VALUE #( trkorr = ls_line-line ) TO rt_trkorr.

    ENDLOOP.

  ENDMETHOD.


  METHOD gui_message.

    DATA: lv_index(8)     TYPE c,
          lv_level(2)     TYPE c,
          lv_message(132) TYPE c,
          lv_rate         TYPE i.




    lv_rate = iv_index * 100 / iv_level.

    MOVE: iv_index     TO lv_index,
          lv_level     TO lv_level,
          iv_message   TO lv_message.

    CONDENSE: lv_index, lv_message.

    CONCATENATE: '(' lv_index '/' lv_level ')' INTO lv_index,
                 lv_index lv_message INTO lv_message SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_rate
        text       = lv_message.

  ENDMETHOD.


  METHOD perpare_number.
    DATA lv_strlen TYPE i.
    DATA lv_last_decimal TYPE i.
    DATA lv_hlpvz TYPE i.

    lv_strlen = strlen( cv_parameter ).
    CLEAR: sy-subrc, cv_decimal_pos.
    WHILE sy-subrc =  0.
      IF cv_parameter CA '.'.
        IF sy-fdpos  > lv_last_decimal.
          lv_last_decimal = sy-fdpos + 1.
        ENDIF.
      ENDIF.
      REPLACE '.' WITH space INTO cv_parameter.
    ENDWHILE.
    CLEAR sy-subrc.
    WHILE sy-subrc =  0.
      IF cv_parameter CA ','.
        IF sy-fdpos  > lv_last_decimal.
          lv_last_decimal = sy-fdpos + 1.
        ENDIF.
      ENDIF.
      REPLACE ',' WITH space INTO cv_parameter.
    ENDWHILE.
    CLEAR sy-subrc.
    WHILE sy-subrc =  0.
      IF cv_parameter CA ';'.
        IF sy-fdpos  > lv_last_decimal.
          lv_last_decimal = sy-fdpos + 1.
        ENDIF.
      ENDIF.
      REPLACE ';' WITH space INTO cv_parameter.
    ENDWHILE.
    CLEAR sy-subrc.
    WHILE sy-subrc =  0.
      IF cv_parameter CA '/'.
        IF sy-fdpos  > lv_last_decimal.
          lv_last_decimal = sy-fdpos + 1.
        ENDIF.
      ENDIF.
      REPLACE '/' WITH space INTO cv_parameter.
    ENDWHILE.
    IF NOT lv_last_decimal IS INITIAL.
      lv_hlpvz = lv_strlen - 1.
      IF cv_parameter+lv_hlpvz(1) = '-'.
        cv_decimal_pos = lv_strlen - lv_last_decimal - 1.
      ELSE.
        cv_decimal_pos = lv_strlen - lv_last_decimal.
      ENDIF.
    ENDIF.
    CONDENSE cv_parameter NO-GAPS.

  ENDMETHOD.


  METHOD popup_column_list.

    TYPES : BEGIN OF ty_disp,
              compname TYPE rstrucinfo-compname,
              type     TYPE rstrucinfo-type,
              olen(3),
            END OF ty_disp.

    DATA : lt_comp TYPE TABLE OF rstrucinfo,
           lt_disp TYPE TABLE OF ty_disp.

    CALL FUNCTION 'GET_COMPONENT_LIST'
      EXPORTING
        program    = iv_repid
        fieldname  = iv_tname
      TABLES
        components = lt_comp[].

    lt_disp = CORRESPONDING #( lt_comp ).

    DATA : lt_icols TYPE TABLE OF help_value,
           ls_icols TYPE help_value.

    ls_icols-tabname = 'DD03D'.
    ls_icols-fieldname = 'FIELDNAME'.
    ls_icols-selectflag = 'X'.
    APPEND ls_icols TO  lt_icols.

    ls_icols-tabname = 'RSTRUCINFO'.
    ls_icols-fieldname = 'TYPE'.
    ls_icols-selectflag = 'X'.
    APPEND ls_icols TO lt_icols.

    ls_icols-tabname = 'DD03P'.
    ls_icols-fieldname = 'LENG'.
    ls_icols-selectflag = 'X'.
    APPEND ls_icols TO lt_icols.


* bakılacak ?
*data(lt_ICOLS) = value help_value(
*                    tabname    = 'DD03D'
*                    fieldname  = 'FIELDNAME'
*                    selectflag = 'X' ).
*
*append value #(
*                    tabname = 'RSTRUCINFO'
*                    fieldname = 'TYPE'
*                    selectflag = 'X'
*               ) to lt_ICOLS.
**
**                   (
**                    tabname = 'DD03P'
**                    fieldname = 'LENG'
**                    selectflag = 'X'
**                   )
*                   ).



    CALL FUNCTION 'MD_POPUP_SHOW_INTERNAL_TABLE' ##FM_SUBRC_OK
      EXPORTING
        title   = TEXT-001
*    importing
*       index   =
      TABLES
        values  = lt_disp[]
        columns = lt_icols[]
      EXCEPTIONS
        leave   = 1
        OTHERS  = 2.

  ENDMETHOD.


  METHOD popup_column_list_str.

    DATA : lt_fcat TYPE lvc_t_fcat,
           lt_list TYPE TABLE OF zbcs_column_list_str.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = iv_str
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                     DISPLAY LIKE 'E'.
    ENDIF.

    IF lt_fcat IS INITIAL.
      RETURN.
    ENDIF.

    lt_list = CORRESPONDING #( lt_fcat ).

    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title               = TEXT-002
        i_selection           = abap_false
        i_screen_start_line   = 3
        i_screen_end_line     = 30
        i_screen_start_column = 50
        i_screen_end_column   = 180
        i_zebra               = abap_true
        i_tabname             = 'LT_LIST'
        i_structure_name      = 'ZBCS_COLUMN_LIST_STR'
      TABLES
        t_outtab              = lt_list
      EXCEPTIONS
        program_error         = 1
        OTHERS                = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                     DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD sapgui_mess.

    DATA : lv_yuzde TYPE i,
           lv_mod   TYPE i.

    lv_mod = iv_akt MOD 10.
    IF lv_mod NE 0.
      RETURN.
    ENDIF.

    IF iv_top NE 0.
      lv_yuzde = ( iv_akt * 100 ) / iv_top.
    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' ##FM_SUBRC_OK
      EXPORTING
        percentage = lv_yuzde
        text       = iv_text
      EXCEPTIONS
        OTHERS     = 1.

  ENDMETHOD.


  METHOD upload_excel_file.

    DATA: lt_tabcde  TYPE TABLE OF alsmex_tabline,
          ls_tabcde  TYPE alsmex_tabline,
          lv_file    TYPE rlgrap-filename,
          lt_comp    TYPE TABLE OF rstrucinfo,
          ls_comp    TYPE rstrucinfo,
          lv_tind(4) TYPE n,
          lo_line    TYPE REF TO data.

    lv_file = iv_filename.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE' ##FM_SUBRC_OK
      EXPORTING
        filename                = lv_file
        i_begin_col             = 1
        i_begin_row             = iv_line
        i_end_col               = 999
        i_end_row               = 99999
      TABLES
        intern                  = lt_tabcde
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.


    CALL FUNCTION 'GET_COMPONENT_LIST'
      EXPORTING
        program    = iv_repid
        fieldname  = iv_tname
      TABLES
        components = lt_comp[].


    FIELD-SYMBOLS : <fs_value> TYPE any,
                    <fs_table> TYPE STANDARD TABLE,
                    <fs_lines> TYPE any.

    ASSIGN rt_table->* TO <fs_table>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CREATE DATA lo_line LIKE LINE OF <fs_table>.
    ASSIGN lo_line->* TO <fs_lines>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT lt_tabcde INTO ls_tabcde.
      lv_tind = ls_tabcde-col.
      READ TABLE lt_comp INTO ls_comp INDEX lv_tind.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT ls_comp-compname
               OF STRUCTURE <fs_lines> TO <fs_value>.
        IF sy-subrc EQ 0.
          <fs_value> = ls_tabcde-value.
        ENDIF.
      ENDIF.

      AT END OF row.
        APPEND <fs_lines> TO <fs_table>.
      ENDAT.
    ENDLOOP.
    rt_table = REF #( <fs_table> ).
  ENDMETHOD.


  METHOD upload_excel_file_data.

    DATA: lt_tabcde     TYPE TABLE OF alsmex_tabline,
          ls_tabcde     TYPE alsmex_tabline,
          lv_file       TYPE rlgrap-filename,
          lv_line       TYPE i,
*          lv_line_excel type i,
          lv_tind(4)    TYPE n,
          lo_line       TYPE REF TO data,
          lt_definition TYPE STANDARD TABLE OF dd03p,
          ls_dd02v_n    TYPE dd02v,
          lv_fname      TYPE rs38l_fnam.


    IF iv_filename IS NOT  INITIAL.
      lv_file = iv_filename.
    ELSE.
      lv_file = CONV rlgrap-filename( get_file( ) ) ##OPERATOR.
    ENDIF.

    IF iv_line IS INITIAL.
      lv_line = 1.
    ELSE.
      lv_line = iv_line.
    ENDIF.

*--------------------------------------------------------------------*
* Tablonun detayını al
*--------------------------------------------------------------------*
    CALL FUNCTION 'DD_INT_TABL_GET'
      EXPORTING
        tabname        = iv_structure_name
      IMPORTING
        dd02v_n        = ls_dd02v_n
      TABLES
        dd03p_n        = lt_definition
      EXCEPTIONS
        internal_error = 1.

    IF sy-subrc EQ 0 AND lines( lt_definition ) GT 0.

      CALL FUNCTION 'DD_TABL_EXPAND' ##FM_SUBRC_OK
        EXPORTING
          dd02v_wa          = ls_dd02v_n
          mode              = 46
          prid              = 0
        TABLES
          dd03p_tab         = lt_definition
        EXCEPTIONS
          illegal_parameter = 1.

    ENDIF.

    IF lines( lt_definition ) EQ 0.
      RAISE error_structure_read.
    ENDIF.

    DELETE lt_definition WHERE fieldname EQ '.INCLUDE'.


    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE' ##FM_SUBRC_OK
      EXPORTING
        filename                = lv_file
        i_begin_col             = 1
        i_begin_row             = lv_line
        i_end_col               = lines( lt_definition )
        i_end_row               = 999999
      TABLES
        intern                  = lt_tabcde
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc NE 0.
      CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE' ##FM_SUBRC_OK
        EXPORTING
          filename                = lv_file
          i_begin_col             = 1
          i_begin_row             = lv_line
          i_end_col               = lines( lt_definition )
          i_end_row               = 65000
        TABLES
          intern                  = lt_tabcde
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.
      IF sy-subrc NE 0.
        MESSAGE i001(ls) WITH lv_file TEXT-e01.
      ENDIF.

    ENDIF.


*--------------------------------------------------------------------*
* MAPPING
*--------------------------------------------------------------------*
    FIELD-SYMBOLS : <fs_value> TYPE any,
                    <fs_table> TYPE STANDARD TABLE,
                    <fs_lines> TYPE any.

    ASSIGN ct_table TO <fs_table>.
*    check sy-subrc eq 0.
*
    CREATE DATA lo_line LIKE LINE OF <fs_table>.
    ASSIGN lo_line->* TO <fs_lines>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT lt_tabcde INTO ls_tabcde.
      lv_tind = ls_tabcde-col.
      READ TABLE lt_definition ASSIGNING FIELD-SYMBOL(<fs_definition>) INDEX lv_tind.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT <fs_definition>-fieldname
               OF STRUCTURE <fs_lines> TO <fs_value>.
        IF sy-subrc EQ 0.
*--------------------------------------------------------------------*
* Dönüşüm işlemi...
*--------------------------------------------------------------------*
          TRY.
              convert_any_input_to_data( EXPORTING iv_source = ls_tabcde-value
                                         CHANGING  cv_target = <fs_value> ).
            CATCH cx_root INTO DATA(lo_cx).
              MESSAGE lo_cx->previous TYPE 'S' DISPLAY LIKE 'E'.
              RAISE conversion_error.

          ENDTRY.

          IF <fs_definition>-convexit IS NOT INITIAL AND
             <fs_definition>-inttype NE 'P'.
            lv_fname = 'CONVERSION_EXIT_' &&
                       <fs_definition>-convexit &&
                       '_INPUT'.

            CALL FUNCTION lv_fname ##FM_SUBRC_OK
              EXPORTING
                input  = <fs_value>
              IMPORTING
                output = <fs_value>
              EXCEPTIONS
                OTHERS = 1.

          ENDIF.
        ENDIF.
      ENDIF.

      AT END OF row.
        APPEND <fs_lines> TO <fs_table>.
        CLEAR <fs_lines>.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.


  METHOD write_cx_msg_matryoshka.

    CHECK io_cx IS NOT INITIAL.
    write_cx_msg_matryoshka( io_cx->previous ).

    DATA(lv_text) = io_cx->get_text( ).
    NEW-LINE.
    WRITE lv_text.

  ENDMETHOD.
ENDCLASS.
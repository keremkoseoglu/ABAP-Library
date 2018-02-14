class ZCL_BC_TOOLKIT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_address ,
        addrnumber TYPE ad_addrnum,
        adrc       TYPE adrc,
      END OF t_address .

  constants C_INFO_DECIMALS type TEXT255 value 'Sayisal alanlar ___.___.___,__ seklinde olmalidir!(Örnegin 1.240,50)' ##NO_TEXT.
  constants C_INFO_COLUMN type RSFUNC_TXT value '...Kolon Siralari...' ##NO_TEXT.
  constants C_INFO_POPER type RSFUNC_TXT value 'Dönem Bilgisi AA.YYYY seklinde olmalidir!(Örnegin 01.2015)' ##NO_TEXT.
  constants C_DOWNLOAD_EXCEL type RSFUNC_TXT value '@M8@ Örnek Dosya İndir' ##NO_TEXT.
  constants C_LOG_REPORT type RSFUNC_TXT value '@96@ Loglar' ##NO_TEXT.

  class-methods GET_SYMSG_AS_TEXT
    importing
      !IS_SYMSG type SYMSG optional
    returning
      value(RV_TEXT) type STRING .
  class-methods IS_SAAB_BREAK_POINT_ACTIVE
    importing
      !IV_NAME type AAB_ID_NAME
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  class-methods READ_TEXT
    importing
      !IV_ID type THEAD-TDID optional
      !IV_LANGUAGE type THEAD-TDSPRAS optional
      !IV_NAME type THEAD-TDNAME optional
      !IV_OBJECT type THEAD-TDOBJECT optional
    returning
      value(RT_LINES) type TTTEXT
    raising
      ZCX_BC_READ_TEXT .
  class-methods CHECK_DEBUG
    importing
      !IV_ANAME type AAB_ID_NAME .
  class-methods COMMIT
    importing
      !IV_WAIT type CHAR1 default 'X' .
  class-methods ROLLBACK .
  class-methods GET_ADRC
    importing
      !IV_ADDRNUMBER type AD_ADDRNUM
    returning
      value(RS_ADDRES) type T_ADDRESS .
  class-methods SSF_NAME
    importing
      !IM_FORMNAME type TDSFNAME
    returning
      value(RV_FNAME) type RS38L_FNAM .
  class-methods PRINT_OPTION_GET_FOR_PDF
    exporting
      value(ES_CONTROL_PARAM) type SSFCTRLOP
      value(ES_COMPOSER_PARAM) type SSFCOMPOP .
  class-methods ASSIGN_READTEXT_VALUE
    importing
      !IV_ID type THEAD-TDID optional
      !IV_LANGUAGE type THEAD-TDSPRAS optional
      !IV_NAME type THEAD-TDNAME optional
      !IV_OBJECT type THEAD-TDOBJECT optional
    changing
      !CV_TEXT type ANY .
  PROTECTED SECTION.
private section.

  types:
    tt_address TYPE HASHED TABLE OF t_address WITH UNIQUE KEY primary_key COMPONENTS addrnumber .

  class-data GT_ADDRESS type TT_ADDRESS .
ENDCLASS.



CLASS ZCL_BC_TOOLKIT IMPLEMENTATION.


  method assign_readtext_value.

   field-symbols <lv_text> type any.

   assign cv_text to <lv_text>.

     try.
          data(lt_lines) =
            zcl_bc_toolkit=>read_text(
              iv_id       = iv_id
              iv_language = sy-langu
              iv_name     = iv_name
              iv_object   = iv_object ).

      catch cx_root into data(lx_root).
      endtry.

      loop at lt_lines assigning field-symbol(<ls_lines>).
        if <lv_text> is initial .
           <lv_text> = <ls_lines>-tdline.
        else.
          <lv_text> = |{ <lv_text> } { <ls_lines>-tdline }|.
        endif.
      endloop.

  endmethod.


  METHOD check_debug.

    CHECK iv_aname IS NOT INITIAL.

    WHILE is_saab_break_point_active( iv_aname ) EQ abap_true.
      WAIT UP TO 1 SECONDS.
    ENDWHILE.

  ENDMETHOD.


  METHOD commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = iv_wait.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD get_adrc.
    DATA ls_address TYPE t_address.
    ASSIGN gt_address[ KEY primary_key COMPONENTS addrnumber = iv_addrnumber ] TO FIELD-SYMBOL(<ls_address>).
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM adrc INTO ls_address-adrc
        WHERE addrnumber = iv_addrnumber ##WARN_OK.
      ls_address-addrnumber = iv_addrnumber.
      INSERT ls_address INTO TABLE gt_address ASSIGNING <ls_address>.
    ENDIF.
    rs_addres = <ls_address>.
  ENDMETHOD.


  METHOD get_symsg_as_text.

    IF is_symsg IS SUPPLIED.
      DATA(ls_symsg) = is_symsg.
    ELSE.
      MOVE-CORRESPONDING sy TO ls_symsg.
    ENDIF.

    MESSAGE ID     ls_symsg-msgid
            TYPE   zcl_bc_applog_facade=>c_msgty_s
            NUMBER ls_symsg-msgno
            WITH   ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
            INTO   rv_text.

  ENDMETHOD.


  METHOD is_saab_break_point_active.

    DATA: lv_server_name   TYPE msxxlist-name,
          lv_timestamp_now TYPE aab_id_act-act_tstamp.

    CALL FUNCTION 'GENERAL_GET_APP_SERVER_NAME' ##fm_Subrc_ok
      IMPORTING
        server_name        = lv_server_name
      EXCEPTIONS
        error_reading_name = 1
        OTHERS             = 2.

    lv_timestamp_now = |{ sy-datum }{ sy-uzeit }|.

    SELECT SINGLE mandt INTO sy-mandt ##write_ok ##WARN_OK
           FROM aab_id_act
           WHERE name EQ iv_name
             AND ( ( username EQ sy-uname AND server EQ space          ) OR
                   ( username EQ space    AND server EQ lv_server_name ) OR
                   ( username EQ space    AND server EQ space          ) )
             AND actmode EQ 1
             AND act_tstamp LE lv_timestamp_now
             AND exp_tstamp GE lv_timestamp_now.

    rv_active = boolc( sy-subrc EQ 0 ).

  ENDMETHOD.


  METHOD print_option_get_for_pdf.

    es_control_param-getotf     = 'X'.
    es_control_param-no_dialog  = 'X'.
    es_control_param-preview    = 'X'.
    es_control_param-device     = 'PRINTER'.

    es_composer_param-tddest = 'LP01'.
    es_composer_param-tdnoprev  = ''.


  ENDMETHOD.


  METHOD read_text.

*==========================================================*
* Sample Code for calling Method
*==========================================================*
*    data : go_msg type ref to zcx_bc_read_text,
*           gv_msg type string.
*
*    try.
*        data(lt_lines) = zcl_bc_toolkit=>read_text(
*                         exporting
*                          iv_id       = 'LTQM'
*                          iv_language = 'T'
*                          iv_name     = conv #( gs_viqmel-qmnum )
*                          iv_object   = 'QMEL').
*
*      catch zcx_bc_read_text into go_msg.
*        gv_msg = go_msg->get_text( ).
*    endtry.
*==========================================================*

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = iv_id
        language                = iv_language
        name                    = iv_name
        object                  = iv_object
      TABLES
        lines                   = rt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_bc_read_text
        EXPORTING
          hata_msg = zcl_bc_toolkit=>get_symsg_as_text( )
          textid   = zcx_bc_read_text=>read_error.

    ENDIF.

  ENDMETHOD.


  METHOD rollback.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ENDMETHOD.


  METHOD ssf_name.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = im_formname
*       VARIANT            = ' '
*       DIRECT_CALL        = ' '
      IMPORTING
        fm_name            = rv_fname
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3 ##FM_SUBRC_OK.

  ENDMETHOD.
ENDCLASS.
CLASS zcl_bc_ddic_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_tabfld TYPE zcl_bc_abap_table=>tt_tabfld.

    CONSTANTS c_fieldname_high   TYPE fieldname VALUE ycl_addict_toolkit=>field-high.
    CONSTANTS c_fieldname_low    TYPE fieldname VALUE ycl_addict_toolkit=>field-low.
    CONSTANTS c_fieldname_option TYPE fieldname VALUE ycl_addict_toolkit=>field-option.
    CONSTANTS c_fieldname_sign   TYPE fieldname VALUE ycl_addict_toolkit=>field-sign.

    CONSTANTS c_option_bt        TYPE ddoption  VALUE ycl_addict_toolkit=>option-bt.
    CONSTANTS c_option_cp        TYPE ddoption  VALUE ycl_addict_toolkit=>option-cp.
    CONSTANTS c_option_eq        TYPE ddoption  VALUE ycl_addict_toolkit=>option-eq.
    CONSTANTS c_option_ge        TYPE ddoption  VALUE ycl_addict_toolkit=>option-ge.
    CONSTANTS c_option_le        TYPE ddoption  VALUE ycl_addict_toolkit=>option-le.
    CONSTANTS c_option_ne        TYPE ddoption  VALUE ycl_addict_toolkit=>option-ne.
    CONSTANTS c_option_gt        TYPE ddoption  VALUE 'GT'.
    CONSTANTS c_option_lt        TYPE ddoption  VALUE 'LT'.

    CONSTANTS c_rollname_option  TYPE rollname  VALUE ycl_addict_toolkit=>rollname-option.
    CONSTANTS c_rollname_sign    TYPE rollname  VALUE ycl_addict_toolkit=>rollname-sign.

    CONSTANTS c_sign_e           TYPE ddsign    VALUE ycl_addict_toolkit=>sign-exclude.
    CONSTANTS c_sign_i           TYPE ddsign    VALUE ycl_addict_toolkit=>sign-include.

    CLASS-METHODS alpha_input
      IMPORTING iv_input  TYPE any
      EXPORTING ev_output TYPE any.

    CLASS-METHODS alpha_input_change
      CHANGING VALUE(c_field) TYPE any.

    CLASS-METHODS alpha_output
      IMPORTING iv_input         TYPE any
      EXPORTING VALUE(ev_output) TYPE any.

    CLASS-METHODS char_to_dec2
      IMPORTING iv_char       TYPE char20
      RETURNING VALUE(rv_dec) TYPE dmbtr_13_2.

    CLASS-METHODS char_to_dec3
      IMPORTING iv_char       TYPE char20
      RETURNING VALUE(rv_dec) TYPE menge_d.

    CLASS-METHODS conv_10digitdate_to_8digitdate
      IMPORTING iv_date        TYPE any
      RETURNING VALUE(rv_date) TYPE datum.

    CLASS-METHODS cunit_input
      IMPORTING iv_meins        TYPE meins
      RETURNING VALUE(rv_meins) TYPE meins.

    CLASS-METHODS cunit_output
      IMPORTING iv_meins        TYPE meins
      RETURNING VALUE(rv_meins) TYPE meins.

    CLASS-METHODS decimal_to_char_for_vbap_field
      IMPORTING iv_decimal TYPE zsdd_vbap_menge
      CHANGING  cv_char    TYPE any.

    CLASS-METHODS ensure_all_wa_fields_full
      IMPORTING ir_wa                      TYPE REF TO data
                iv_structure               TYPE tabname
                iv_wa_variable_name        TYPE string    OPTIONAL
                iv_forgive_missing_str_fld TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_bc_field_value
                zcx_bc_method_parameter
                zcx_bc_variable.

    CLASS-METHODS float_to_char
      IMPORTING iv_float       TYPE atflv
      RETURNING VALUE(rv_char) TYPE atnam.

    CLASS-METHODS get_rollname_pairs
      IMPORTING iv_tabname1   TYPE tabname
                iv_tabname2   TYPE tabname
      RETURNING VALUE(rt_ret) TYPE zbctt_rollname_pair
      RAISING   zcx_bc_table_content.

    CLASS-METHODS get_table_fields
      IMPORTING iv_tabname    TYPE tabname
      RETURNING VALUE(rt_fld) TYPE tt_tabfld
      RAISING   zcx_bc_table_content.

    CLASS-METHODS matnr_input
      IMPORTING iv_matnr        TYPE clike
      RETURNING VALUE(rv_matnr) TYPE matnr
      RAISING   ycx_addict_function_subrc.

    CLASS-METHODS matnr_output
      IMPORTING iv_matnr        TYPE matnr
      RETURNING VALUE(rv_matnr) TYPE matnr.

    CLASS-METHODS vbeln_input
      IMPORTING iv_vbeln        TYPE clike
      RETURNING VALUE(rv_vbeln) TYPE vbeln_vl.

    CLASS-METHODS domain_value_get
      IMPORTING iv_domname       TYPE dd07v-domname
                iv_domvalue      TYPE dd07v-domvalue_l
      RETURNING VALUE(rv_ddtext) TYPE dd07v-ddtext.

  PRIVATE SECTION.
    CONSTANTS c_clsname_me TYPE seoclsname VALUE 'ZCL_BC_DDIC_TOOLKIT' ##NO_TEXT.
    CONSTANTS c_meth_eawff TYPE seocpdname VALUE 'ENSURE_ALL_WA_FIELDS_FULL' ##NO_TEXT.
ENDCLASS.


CLASS zcl_bc_ddic_toolkit IMPLEMENTATION.
  METHOD alpha_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = iv_input
      IMPORTING output = ev_output.
  ENDMETHOD.

  METHOD alpha_input_change.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = c_field
      IMPORTING output = c_field.
  ENDMETHOD.

  METHOD alpha_output.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING input  = iv_input
      IMPORTING output = ev_output.
  ENDMETHOD.

  METHOD char_to_dec2.
    DATA lv_char TYPE char20.

    TRY.
        lv_char = iv_char.
        SEARCH lv_char FOR ','.
        IF sy-subrc = 0.
          REPLACE '.' WITH '' INTO lv_char.
          CONDENSE lv_char NO-GAPS.
        ELSE.
          REPLACE '.' WITH ',' INTO lv_char.
        ENDIF.

        CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
          EXPORTING  i_char         = lv_char
          IMPORTING
*                     E_FLOAT        =
                     e_packed       = rv_dec
          EXCEPTIONS invalid_number = 1
                     OTHERS         = 2.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD char_to_dec3.
    CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
      EXPORTING  i_char         = iv_char
      IMPORTING
*                 E_FLOAT        =
                 e_packed       = rv_dec
      EXCEPTIONS invalid_number = 1
                 OTHERS         = 2.
  ENDMETHOD.

  METHOD conv_10digitdate_to_8digitdate.
    DATA : BEGIN OF ls_date,
             day   TYPE c LENGTH 2,
             month TYPE c LENGTH 2,
             year  TYPE c LENGTH 4,
           END OF ls_date.

    SPLIT iv_date AT '.' INTO ls_date-day ls_date-month ls_date-year.

    IF     ls_date-day   IS NOT INITIAL
       AND ls_date-month IS NOT INITIAL
       AND ls_date-year  IS NOT INITIAL.

      alpha_input( EXPORTING iv_input  = ls_date-month
                   IMPORTING ev_output = ls_date-month ).

      alpha_input( EXPORTING iv_input  = ls_date-day
                   IMPORTING ev_output = ls_date-day ).

      rv_date = |{ ls_date-year }{ ls_date-month }{ ls_date-day }|.

    ELSE.

      rv_date = iv_date.
    ENDIF.
  ENDMETHOD.

  METHOD cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING  input          = iv_meins
      IMPORTING  output         = rv_meins
      EXCEPTIONS unit_not_found = 1
                 OTHERS         = 2.
  ENDMETHOD.

  METHOD cunit_output.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING  input          = iv_meins
*                 LANGUAGE       = SY-LANGU
      IMPORTING
*                 LONG_TEXT      =
                 output         = rv_meins
*                 SHORT_TEXT     =
      EXCEPTIONS unit_not_found = 1
                 OTHERS         = 2.

    IF sy-subrc <> 0.
      rv_meins = iv_meins.
      " Implement suitable error handling here
    ENDIF.
  ENDMETHOD.

  METHOD decimal_to_char_for_vbap_field.
    DATA lv_char TYPE char50.

    TRY.
        lv_char = iv_decimal.
        CONDENSE lv_char.
        cv_char = lv_char.
      CATCH cx_sy_conversion_no_number ##NO_HANDLER.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD domain_value_get.
    CALL FUNCTION 'DOMAIN_VALUE_GET'
      EXPORTING  i_domname  = iv_domname
                 i_domvalue = iv_domvalue
      IMPORTING  e_ddtext   = rv_ddtext
      EXCEPTIONS not_exist  = 1
                 OTHERS     = 2 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD ensure_all_wa_fields_full.
    " ______________________________
    " Parametre kontrolleri

    IF ir_wa IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_method_parameter( textid      = zcx_bc_method_parameter=>param_value_initial
                                                   class_name  = c_clsname_me
                                                   method_name = c_meth_eawff
                                                   param_name  = 'IR_WA' ).
    ENDIF.

    IF iv_structure IS INITIAL.
      RAISE EXCEPTION NEW zcx_bc_method_parameter( textid      = zcx_bc_method_parameter=>param_value_initial
                                                   class_name  = c_clsname_me
                                                   method_name = c_meth_eawff
                                                   param_name  = 'IV_STRUCTURE' ).
    ENDIF.

    " ______________________________
    " DDIC bilgilerini al

    TRY.
        DATA(lo_tab) = zcl_bc_abap_table=>get_instance( iv_structure ).
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION NEW zcx_bc_method_parameter( textid      = zcx_bc_method_parameter=>param_value_invalid
                                                     previous    = lo_diaper
                                                     class_name  = c_clsname_me
                                                     method_name = c_meth_eawff
                                                     param_name  = 'IV_STRUCTURE' ).
    ENDTRY.

    " ______________________________
    " Alanların doluluk kontrolü

    ASSIGN ir_wa->* TO FIELD-SYMBOL(<ls_wa>).

    LOOP AT lo_tab->get_fields( ) ASSIGNING FIELD-SYMBOL(<ls_fld>).

      ASSIGN COMPONENT <ls_fld>-fieldname OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<lv_wa_val>).
      IF sy-subrc <> 0.

        IF iv_forgive_missing_str_fld = abap_true.
          CONTINUE.
        ELSE.

          RAISE EXCEPTION NEW zcx_bc_method_parameter( textid       = zcx_bc_method_parameter=>param_pair_inconsistent
                                                       previous     = NEW zcx_bc_work_area_structure(
                                                           textid    = zcx_bc_work_area_structure=>field_missing
                                                           workarea  = CONV #( iv_wa_variable_name )
                                                           fieldname = <ls_fld>-fieldname )
                                                       class_name   = c_clsname_me
                                                       method_name  = c_meth_eawff
                                                       param_name   = 'IR_WA'
                                                       param_name_2 = 'IV_STRUCTURE' ).
        ENDIF.

      ENDIF.

      CHECK <lv_wa_val> IS INITIAL.

      RAISE EXCEPTION NEW zcx_bc_field_value( textid    = zcx_bc_field_value=>missing_value
                                              fieldname = |{ <ls_fld>-fieldname }|
                                              ddtext    = zcl_bc_data_element=>get_text_safe( <ls_fld>-rollname ) ).

    ENDLOOP.
  ENDMETHOD.

  METHOD float_to_char.
    DATA lv_dec TYPE dec10_2.

    CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
      EXPORTING  if_float  = iv_float
*                 IF_SIGNIFICANT_PLACES = 15
      IMPORTING  ef_packed = lv_dec
      EXCEPTIONS overflow  = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.

    WRITE lv_dec TO rv_char.
    CONDENSE rv_char.
  ENDMETHOD.

  METHOD get_rollname_pairs.
    rt_ret = zcl_bc_abap_table=>get_rollname_pairs( iv_tabname1 = iv_tabname1
                                                    iv_tabname2 = iv_tabname2 ).
  ENDMETHOD.

  METHOD get_table_fields.
    rt_fld = CORRESPONDING #( zcl_bc_abap_table=>get_instance( iv_tabname )->get_fields( ) ).
  ENDMETHOD.

  METHOD matnr_input.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING  input        = iv_matnr
      IMPORTING  output       = rv_matnr
      EXCEPTIONS length_error = 1
                 OTHERS       = 2.

    ycx_addict_function_subrc=>raise_if_sysubrc_not_initial( 'CONVERSION_EXIT_MATN1_INPUT' ).
  ENDMETHOD.

  METHOD matnr_output.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING input  = iv_matnr
      IMPORTING output = rv_matnr.
  ENDMETHOD.

  METHOD vbeln_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = iv_vbeln
      IMPORTING output = rv_vbeln.
  ENDMETHOD.
ENDCLASS.